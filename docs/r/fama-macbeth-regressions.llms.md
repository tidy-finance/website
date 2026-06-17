# Fama-MacBeth Regressions

> **NOTE:**
>
> You are reading **Tidy Finance with R**. You can find the equivalent chapter for the sibling **Tidy Finance with Python** [here](../python/fama-macbeth-regressions.llms.md).

In this chapter, we present a simple implementation of Fama and MacBeth ([1973](#ref-Fama1973)), a regression approach commonly called Fama-MacBeth regressions. Fama-MacBeth regressions are widely used in empirical asset pricing studies. We use individual stocks as test assets to estimate the risk premium associated with the three factors included in Fama and French ([1993](#ref-Fama1993)).

Researchers use the two-stage regression approach to estimate risk premiums in various markets, but predominately in the stock market. Essentially, the two-step Fama-MacBeth regressions exploit a linear relationship between expected returns and exposure to (priced) risk factors. The basic idea of the regression approach is to project asset returns on factor exposures or characteristics that resemble exposure to a risk factor in the cross-section in each time period. Then, in the second step, the estimates are aggregated across time to test if a risk factor is priced. In principle, Fama-MacBeth regressions can be used in the same way as portfolio sorts introduced in previous chapters.

The Fama-MacBeth procedure is a simple two-step approach: The first step uses the exposures (characteristics) as explanatory variables in \\T\\ cross-sectional regressions. For example, if \\r\_{i,t+1}\\ denote the excess returns of asset \\i\\ in month \\t+1\\, then the famous Fama-French three-factor model implies the following return generating process (see also [Campbell et al. 1998](#ref-Campbell1998)): \\\begin{aligned}r\_{i,t+1} = \alpha_i + \lambda^{M}\_t \beta^M\_{i,t} + \lambda^{SMB}\_t \beta^{SMB}\_{i,t} + \lambda^{HML}\_t \beta^{HML}\_{i,t} + \epsilon\_{i,t}.\end{aligned}\\ Here, we are interested in the compensation \\\lambda^{f}\_t\\ for the exposure to each risk factor \\\beta^{f}\_{i,t}\\ at each time point, i.e., the risk premium. Note the terminology: \\\beta^{f}\_{i,t}\\ is an asset-specific characteristic, e.g., a factor exposure or an accounting variable. *If* there is a linear relationship between expected returns and the characteristic in a given month, we expect the regression coefficient to reflect the relationship, i.e., \\\lambda_t^{f}\neq0\\.

In the second step, the time-series average \\\frac{1}{T}\sum\_{t=1}^T \hat\lambda^{f}\_t\\ of the estimates \\\hat\lambda^{f}\_t\\ can then be interpreted as the risk premium for the specific risk factor \\f\\. We follow Zaffaroni and Zhou ([2022](#ref-Zaffaroni2022)) and consider the standard cross-sectional regression to predict future returns. If the characteristics are replaced with time \\t+1\\ variables, then the regression approach captures risk attributes rather than risk premiums.

Before we move to the implementation, we want to highlight that the characteristics, e.g., \\\hat\beta^{f}\_{i}\\, are often estimated in a separate step before applying the actual Fama-MacBeth methodology. You can think of this as a *step 0*. You might thus worry that the errors of \\\hat\beta^{f}\_{i}\\ impact the risk premiums’ standard errors. Measurement error in \\\hat\beta^{f}\_{i}\\ indeed affects the risk premium estimates, i.e., they lead to biased estimates. The literature provides adjustments for this bias (see, e.g., [Shanken 1992](#ref-Shanken1992); [Kim 1995](#ref-Kim1995); [Chen et al. 2015](#ref-Chen2015), among others) but also shows that the bias goes to zero as \\T \to \infty\\. We refer to Gagliardini et al. ([2016](#ref-Gagliardini2016)) for an in-depth discussion also covering the case of time-varying betas. Moreover, if you plan to use Fama-MacBeth regressions with individual stocks, Hou et al. ([2020](#ref-Hou2020)) advocates using weighted-least squares to estimate the coefficients such that they are not biased toward small firms. Without this adjustment, the high number of small firms would drive the coefficient estimates.

The current chapter relies on this set of R packages.

``` r
library(tidyverse)
```

    Warning: package 'dplyr' was built under R version 4.5.3

``` r
library(arrow)
library(sandwich)
library(broom)
```

## Data Preparation

We illustrate Fama and MacBeth ([1973](#ref-Fama1973)) with the monthly CRSP sample and use three characteristics to explain the cross-section of returns: market capitalization, the book-to-market ratio, and the CAPM beta (i.e., the covariance of the excess stock returns with the market excess returns). We collect the data from our Parquet files introduced in [Accessing and Managing Financial Data](../r/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../r/wrds-crsp-and-compustat.llms.md). As in the previous chapters, we keep only firms with positive book equity, which is a common practice when working with book-to-market ratios (see [Fama and French 1992](#ref-Fama1992) for details).

``` r
crsp_monthly <- read_parquet("data/crsp_monthly.parquet") |>
  select(permno, gvkey, date, ret_excess, mktcap) |>
  collect()

compustat_annual <- read_parquet("data/compustat_annual.parquet") |>
  select(datadate, gvkey, be) |>
  filter(be > 0)

beta <- read_parquet("data/beta.parquet") |>
  filter(return_type == "monthly") |>
  select(permno, date, beta)
```

We use the Compustat and CRSP data to compute the book-to-market ratio and the (log) market capitalization. Furthermore, we also use the CAPM betas based on monthly returns we computed in the previous chapters.

``` r
characteristics <- compustat_annual |>
  mutate(date = floor_date(ymd(datadate), "month")) |>
  left_join(crsp_monthly, join_by(gvkey, date)) |>
  left_join(beta, join_by(permno, date)) |>
  transmute(
    gvkey,
    bm = be / mktcap,
    log_mktcap = log(mktcap),
    beta = beta,
    sorting_date = date %m+% months(6)
  )

data_fama_macbeth <- crsp_monthly |>
  left_join(characteristics, join_by(gvkey, date == sorting_date)) |>
  group_by(permno) |>
  arrange(date) |>
  fill(c(beta, bm, log_mktcap), .direction = "down") |>
  ungroup() |>
  left_join(
    crsp_monthly |>
      select(permno, date, ret_excess_lead = ret_excess) |>
      mutate(date = date %m-% months(1)),
    join_by(permno, date)
  ) |>
  select(permno, date, ret_excess_lead, beta, log_mktcap, bm) |>
  drop_na()
```

## Cross-Sectional Regression

Next, we run the cross-sectional regressions with the characteristics as explanatory variables for each month. We regress the returns of the test assets at a particular time point on the characteristics of each asset. By doing so, we get an estimate of the risk premiums \\\hat\lambda^{f}\_t\\ for each point in time.

``` r
risk_premiums <- data_fama_macbeth |>
  nest(data = c(ret_excess_lead, beta, log_mktcap, bm, permno)) |>
  mutate(
    estimates = map(
      data,
      \(x) tidy(lm(ret_excess_lead ~ beta + log_mktcap + bm, data = x))
    )
  ) |>
  unnest(estimates)
```

## Time-Series Aggregation

Now that we have the risk premiums’ estimates for each period, we can average across the time-series dimension to get the expected risk premium for each characteristic. Similarly, we manually create the \\t\\-test statistics for each regressor, which we can then compare to usual critical values of 1.96 or 2.576 for two-tailed significance tests.

``` r
price_of_risk <- risk_premiums |>
  group_by(factor = term) |>
  summarize(
    risk_premium = mean(estimate),
    t_statistic = mean(estimate) / sd(estimate) * sqrt(n())
  )
```

It is common to adjust for autocorrelation when reporting standard errors of risk premiums. As in [Univariate Portfolio Sorts](../r/univariate-portfolio-sorts.llms.md), the typical procedure for this is computing Newey and West ([1987](#ref-Newey1987)) standard errors. We again recommend the data-driven approach of Newey and West ([1994](#ref-Newey1994)) using the `NeweyWest()` function, but note that you can enforce the typical 6 lag settings via `NeweyWest(., lag = 6, prewhite = FALSE)`.

``` r
regressions_for_newey_west <- risk_premiums |>
  select(date, factor = term, estimate) |>
  nest(data = c(date, estimate)) |>
  mutate(
    model = map(data, \(x) lm(estimate ~ 1, x)),
    mean = map(model, tidy)
  )

price_of_risk_newey_west <- regressions_for_newey_west |>
  mutate(newey_west_se = map_dbl(model, \(x) sqrt(NeweyWest(x)))) |>
  unnest(mean) |>
  mutate(t_statistic_newey_west = estimate / newey_west_se) |>
  select(factor, risk_premium = estimate, t_statistic_newey_west)

left_join(
  price_of_risk,
  price_of_risk_newey_west |>
    select(factor, t_statistic_newey_west),
  join_by(factor)
)
```

    # A tibble: 4 × 4
      factor      risk_premium t_statistic t_statistic_newey_west
      <chr>              <dbl>       <dbl>                  <dbl>
    1 (Intercept)    0.0115         4.62                   3.91  
    2 beta          -0.0000115     -0.0114                -0.0103
    3 bm             0.00157        3.52                   3.01  
    4 log_mktcap    -0.000934      -2.74                  -2.40  

Finally, let us interpret the results. Stocks with higher book-to-market ratios earn higher expected future returns, which is in line with the value premium. The negative value for log market capitalization reflects the size premium for smaller stocks. Consistent with results from earlier chapters, we detect no relation between beta and future stock returns.

You can also replicate the results using the `tidyfinance` package via the `estimate_fama_macbeth()` function. By default, it uses Newey-West standard errors and returns a data frame with the columns `factor`, `risk_premium`, `n` (the average number of cross-sectional observations), `standard_error`, and `t_statistic`. Note that the intercept row is labeled `intercept` (rather than `(Intercept)`):

``` r
library(tidyfinance)

estimate_fama_macbeth(
  data_fama_macbeth,
  "ret_excess_lead ~ beta + bm + log_mktcap"
)
```

You can mirror the manual Newey-West specification above by passing the corresponding arguments through `vcov_options`. Setting `detail = TRUE` additionally returns a `summary_statistics` tibble with the average `r_squared`, `adj_r_squared`, and `n_obs` across the cross-sectional regressions:

``` r
library(tidyfinance)
```

    Warning: package 'tidyfinance' was built under R version 4.5.3

``` r
estimate_fama_macbeth(
  data_fama_macbeth,
  "ret_excess_lead ~ beta + bm + log_mktcap",
  vcov_options = list(lag = 6, prewhite = FALSE),
  detail = TRUE
)
```

    $coefficients
    # A tibble: 4 × 5
      factor     risk_premium     n standard_error t_statistic
      <chr>             <dbl> <dbl>          <dbl>       <dbl>
    1 intercept     0.0115      725       0.00286       4.01  
    2 beta         -0.0000115   725       0.00109      -0.0105
    3 bm            0.00157     725       0.000512      3.08  
    4 log_mktcap   -0.000934    725       0.000365     -2.56  

    $summary_statistics
    # A tibble: 1 × 3
      r_squared adj_r_squared n_obs
          <dbl>         <dbl> <dbl>
    1    0.0317        0.0296 2798.

## Key Takeaways

- Fama-MacBeth regressions provide a two-step approach to estimate risk premiums by running time-series averages of cross-sectional regressions on asset characteristics.
- Fama-MacBeth regressions are commonly used in empirical asset pricing to test whether factors like size, value, or market beta are priced in the cross-section of stock returns.
- Measurement error in factor exposures, especially when estimated beforehand, can bias results, but corrections such as Newey-West standard errors and weighted regressions can improve accuracy.
- The `tidyfinance` R package provides a user-friendly `estimate_fama_macbeth()` function that simplifies the Fama-MacBeth estimation pipeline, with `vcov_options` to control the standard error estimation and `detail = TRUE` to report average cross-sectional fit statistics.

## Exercises

1.  Estimate stock-specific value and size risk factors similar to the CAPM-beta using rolling estimation based on Fama-French 3 Factors. Use these estimates instead of the stock characteristics in the Fama-MacBeth regression from above. How do the coefficient estimates differ?
2.  Download the 49 Industry Portfolios from Ken French data library. Use these industry portfolios instead of the stocks to estimate the three rolling risk-factors (beta, value, size). Replicate the Fama-MacBeth regression from above. Are the coefficient estimates similar?
3.  Use individual stocks with weighted-least squares based on a firm’s size as suggested by Hou et al. ([2020](#ref-Hou2020)). Then, repeat the Fama-MacBeth regressions without the weighting scheme adjustment but drop the smallest 20 percent of firms each month. Compare the results of the three approaches.

## References

Campbell, John Y., Andrew W. Lo, A. Craig MacKinlay, and Robert F. Whitelaw. 1998. “The Econometrics of Financial Markets.” *Macroeconomic Dynamics* 2 (4): 559–62. <https://doi.org/10.1017/S1365100598009092>.

Chen, Hong-Yi, Alice C Lee, and Cheng-Few Lee. 2015. “Alternative errors-in-variables models and their applications in finance research.” *The Quarterly Review of Economics and Finance* 58: 213–27. <https://doi.org/10.1016/j.qref.2014.12.002>.

Fama, Eugene F., and Kenneth R. French. 1992. “The cross-section of expected stock returns.” *The Journal of Finance* 47 (2): 427–65. <https://doi.org/2329112>.

Fama, Eugene F., and Kenneth R. French. 1993. “Common risk factors in the returns on stocks and bonds.” *Journal of Financial Economics* 33 (1): 3–56. <https://doi.org/10.1016/0304-405X(93)90023-5>.

Fama, Eugene F., and James D. MacBeth. 1973. “Risk, return, and equilibrium: Empirical tests.” *Journal of Political Economy* 81 (3): 607–36. <https://doi.org/10.1086/260061>.

Gagliardini, Patrick, Elisa Ossola, and Olivier Scaillet. 2016. “Time-varying risk premium in large cross-sectional equity data sets.” *Econometrica* 84 (3): 985–1046. <https://doi.org/10.3982/ECTA11069>.

Hou, Kewei, Chen Xue, and Lu Zhang. 2020. “Replicating anomalies.” *Review of Financial Studies* 33 (5): 2019–133. <https://doi.org/10.1093/rfs/hhy131>.

Kim, Dongcheol. 1995. “The errors in the variables problem in the cross-section of expected stock returns.” *The Journal of Finance* 50 (5): 1605–34. <https://doi.org/10.1111/j.1540-6261.1995.tb05190.x>.

Newey, Whitney .K, and Kenneth D. West. 1994. “Automatic lag selection in covariance matrix estimation.” *The Review of Economic Studies* 61 (4): 631–53. <https://www.jstor.org/stable/2297912>.

Newey, Whitney K., and Kenneth D. West. 1987. “A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance Matrix.” *Econometrica* 55 (3): 703–8. <http://www.jstor.org/stable/1913610>.

Shanken, Jay. 1992. “On the estimation of beta-pricing models.” *Review of Financial Studies* 5 (1): 1–33. <https://doi.org/10.1093/rfs/5.1.1>.

Zaffaroni, Paolo, and Guofu Zhou. 2022. “Asset pricing: Cross-section predictability.” *Working Paper*. <http://dx.doi.org/10.2139/ssrn.4111428>.
