# Fama-MacBeth regressions

In this chapter, we present a simple implementation of @Fama1973 to introduce the regression approach of @Fama1973 which is widely used in empirical asset pricing studies. We use individual stocks as test assets to estimate the risk premium associated with the three factors included in @Fama1993.

Researchers use the two-stage regression approach to estimate risk premiums in various markets, but predominately in the stock market. 
Essentially, the two-step Fama-MacBeth regressions exploit a linear relationship between expected returns and exposure to (priced) risk factors. 
The basic idea of the regression approach is to project asset returns on factor exposures or characteristics that resemble exposure to a risk factor in the cross-section in each time period. 
Then, in the second step, the estimates are then aggregated across time to test if a risk factor is priced. 
In principle, Fama-MacBeth regressions can be used in the same way as portfolio sorts introduced in previous chapters.

\index{Fama-MacBeth} The Fama-MacBeth procedure is a simple two-step approach: 
The first step uses the exposures (characteristics) as explanatory variables in $T$ cross-sectional regressions, i.e.,
$$\begin{aligned}r_{i,t+1} = \alpha_i + \lambda^{M}_t \beta^M_{i,t}  + \lambda^{SMB}_t \beta^{SMB}_{i,t} + \lambda^{HML}_t \beta^{HML}_{i,t} + \epsilon_{i,t}\text{, for each t}.\end{aligned}$$ 
Here, we are interested in the compensation $\lambda^{f}_t$ for the exposure to each risk factor $\beta^{f}_{i,t}$ at each time point, i.e., the risk premium. Note the terminology: $\beta^{f}_{i,t}$ is a asset-specific characteristic, e.g., a factor exposure or an accounting variable. *If* there is a linear relationship between expected returns and the characteristic in a given month, we expect the regression coefficient to reflect the relationship, i.e., $\lambda_t^{f}\neq0$. 

In the second step, the time-series average $\frac{1}{T}\sum\limits_{t=1}^T \hat\lambda^{f}_t$ of the estimates $\hat\lambda^{f}_t$ can then be interpreted as the risk premium for the specific risk factor $f$. We follow @Zaffaroni2022 and consider the standard cross-sectional regression to predict future returns. If the characteristics are replaced with time $t+1$ variables, the regression approach rather captures risk attributes. 

Before we move to the implementation, we want to highlight that the characteristics, e.g., $\hat\beta^{f}_{i}$, are typically estimated in a separate step before applying the actual Fama-MacBeth methodology. You can think of this as a *step 0*. You might thus worry that the errors of $\hat\beta^{f}_{i}$ impact the risk premiums' standard errors. Measurement error in $\hat\beta^{f}_{i}$ indeed affects the risk premium estimates, i.e., they lead to biased estimates. The literature provides adjustments for this bias [see, e.g., @Shanken1992, @Kim1995, @Chen2015, among others] but also shows that the bias goes to zero as $T \to \infty$. We refer to @Gagliardini2016 for an in-depth discussion also covering the case of time-varying betas. Moreover, if you plan to use Fama-MacBeth regressions with individual stocks: @Hou2020 advocates using weighed-least squares to estimate the coefficients such that they are not biased towards small firms. Without this adjustment, the high number of small firms would drive the coefficient estimates.

The current chapter relies on this set of packages. 

```r
library(tidyverse)
library(RSQLite)
library(lubridate)
library(sandwich)
library(broom)
```
Compared to previous chapters, we introduce the `broom` package [@broom] to tidy the estimation output of many estimated linear models. 

## Data preparation

We illustrate @Fama1973 with the monthly CRSP sample and use three characteristics to explain the cross-section of returns: market capitalization, the book-to-market ratio, and the CAPM beta (i.e., the covariance of the excess stock returns with the market excess returns). We collect the data from our `SQLite`-database introduced in chapters 2-3.\index{Data!CRSP}\index{Data!Compustat}\index{Beta}


```r
tidy_finance <- dbConnect(
  SQLite(), "data/tidy_finance.sqlite", extended_types = TRUE
)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
  collect()

compustat <- tbl(tidy_finance, "compustat") |>
  collect()

beta <- tbl(tidy_finance, "beta") |>
  collect()
```

We use the Compustat and CRSP data to compute the book-to-market ratio and the (log) market capitalization.\index{Book-to-market ratio}\index{Market capitalization} 
Furthermore, we also use the CAPM betas based on daily returns we computed in the previous chapters.\index{Beta}\index{CAPM}


```r
bm <- compustat |>
  mutate(month = floor_date(ymd(datadate), "month")) |>
  left_join(crsp_monthly, by = c("gvkey", "month")) |>
  left_join(beta, by = c("permno", "month")) |>
  transmute(gvkey,
            bm = be / mktcap,
            log_mktcap = log(mktcap),
            beta = beta_monthly,
            sorting_date = month %m+% months(6))

data_fama_macbeth <- crsp_monthly |>
  left_join(bm, by = c("gvkey", "month" = "sorting_date")) |>
  group_by(permno) |>
  arrange(month) |>
  fill(c(beta, bm, log_mktcap), .direction = "down") |>
  ungroup() |>
  left_join(crsp_monthly |>
              select(permno, month, ret_excess_lead = ret) |>
              mutate(month = month %m-% months(1)),
            by = c("permno", "month")) |>
  select(permno, month, ret_excess_lead, beta, log_mktcap, bm) |>
  drop_na()
```

## Cross-sectional regression

Next, we run the cross-sectional regressions with the characteristics as explanatory variables for each month.  
We regress the returns of the test assets at a particular time point on each asset's characteristics. 
By doing so, we get an estimate of the risk premiums $\hat\lambda^{F_f}_t$ for each point in time. \index{Regression!Cross-section}


```r
risk_premiums <- data_fama_macbeth |>
  nest(data = c(ret_excess_lead, beta, log_mktcap, bm, permno)) |> 
  mutate(estimates = map(
    data,
    ~tidy(lm(ret_excess_lead ~ beta + log_mktcap + bm, data = .x)))) |> 
  unnest(estimates)
```

## Time-series aggregation

Now that we have the risk premiums' estimates for each period, we can average across the time-series dimension to get the expected risk premium for each characteristic. Similarly, we manually create the $t$-test statistics for each regressor, which we can then compare to usual critical values of 1.96 or 2.576 for two-tailed significance tests. 


```r
price_of_risk <- risk_premiums |>
  group_by(factor = term) |>
  summarise(
    risk_premium = mean(estimate) * 100,
    t_statistic = mean(estimate) / sd(estimate) * sqrt(n())
  )
```

It is common to adjust for autocorrelation when reporting standard errors of risk premiums. As in chapter 5, the typical procedure for this is computing @Newey1987 standard errors. We again recommend the data-driven approach of @Newey1994 using the `NeweyWest()` function, but note that you can enforce the typical 6 lag settings via `NeweyWest(., lag = 6, prewhite = FALSE)`.\index{Standard errors!Newey-West}


```r
regressions_for_newey_west <- risk_premiums |>
  select(month, factor = term, estimate) |>
  nest(data = c(month, estimate)) |>
  mutate(
    model = map(data, ~ lm(estimate ~ 1, .)),
    mean = map(model, tidy)
  )

price_of_risk_newey_west <- regressions_for_newey_west |>
  mutate(newey_west_se = map_dbl(model, ~ sqrt(NeweyWest(.)))) |>
  unnest(mean) |>
  mutate(t_statistic_newey_west = estimate / newey_west_se) |>
  select(factor,
    risk_premium = estimate,
    t_statistic_newey_west
  )

left_join(price_of_risk,
          price_of_risk_newey_west |> 
            select(factor, t_statistic_newey_west),
          by = "factor")
```

```
# A tibble: 4 × 4
  factor      risk_premium t_statistic t_statistic_newey_west
  <chr>              <dbl>       <dbl>                  <dbl>
1 (Intercept)      1.70         6.68                   5.76  
2 beta             0.00763      0.0730                 0.0651
3 bm               0.141        3.03                   2.59  
4 log_mktcap      -0.114       -3.20                  -2.94  
```

Finally, let us interpret the results. Stocks with higher book-to-market ratios earn higher expected future returns, which is in line with the value premium. The negative value for log market capitalization reflects the size premium for smaller stocks. Consistent with results from earlier chapters, we detect no relation between beta and future stock returns.

## Exercises

1. Download a sample of test assets from Kenneth French's homepage and reevaluate the risk premiums for industry portfolios instead of individual stocks.
1. Use individual stocks with weighted-least squares based on a firm's size as suggested by @Hou2020. Then, repeat the Fama-MacBeth regressions without the weighting scheme adjustment but drop the smallest 20 percent of firms each month. Compare the results of the three approaches. 
1. Implement a rolling-window regression for the time-series estimation of the factor exposure. Skip one month after each rolling period before including the exposures in the cross-sectional regression to avoid a look-ahead bias. Then, adapt the cross-sectional regression and compute the average risk premiums.
