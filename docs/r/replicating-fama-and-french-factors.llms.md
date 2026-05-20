# Replicating Fama-French Factors

> **NOTE:**
>
> You are reading **Tidy Finance with R**. You can find the equivalent chapter for the sibling **Tidy Finance with Python** [here](../python/replicating-fama-and-french-factors.llms.md).

In this chapter, we provide a replication of the famous Fama-French factor portfolios. The Fama-French factor models are a cornerstone of empirical asset pricing Fama and French ([2015](#ref-FamaFrench2015)). On top of the market factor represented by the traditional CAPM beta, the three-factor model includes the size and value factors to explain the cross section of returns. Its successor, the five-factor model, additionally includes profitability and investment as explanatory factors.

We start with the three-factor model. We already introduced the size and value factors in [Value and Bivariate Sorts](../r/value-and-bivariate-sorts.llms.md), and their definition remains the same: size is the SMB factor (small-minus-big) that is long small firms and short large firms. The value factor is HML (high-minus-low) and is long in high book-to-market firms and short in low book-to-market counterparts.

After the replication of the three-factor model, we move to the five factors by constructing the profitability factor RMW (robust-minus-weak) as the difference between the returns of firms with high and low operating profitability and the investment factor CMA (conservative-minus-aggressive) as the difference between firms with high versus low investment rates.

The current chapter relies on this set of R packages.

``` r
library(tidyverse)
library(arrow)
```

## Data Preparation

We use CRSP and Compustat as data sources, as we need the same variables to compute the factors in the way Fama and French do it. Hence, there is nothing new below, and we only load data from our Parquet files introduced in [Accessing and Managing Financial Data](../r/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../r/wrds-crsp-and-compustat.llms.md).[^1]

``` r
crsp_monthly <- read_parquet("data-r/crsp_monthly.parquet") |>
  select(
    permno,
    gvkey,
    date,
    ret_excess,
    mktcap,
    mktcap_lag,
    exchange
  )

compustat_annual <- read_parquet("data-r/compustat_annual.parquet") |>
  select(gvkey, datadate, be, op, inv)

factors_ff3_monthly <- read_parquet("data-r/factors_ff3_monthly.parquet") |>
  select(date, smb, hml)

factors_ff5_monthly <- read_parquet("data-r/factors_ff5_monthly.parquet") |>
  select(date, smb, hml, rmw, cma)
```

Yet when we start merging our dataset for computing the premiums, there are a few differences to [Value and Bivariate Sorts](../r/value-and-bivariate-sorts.llms.md). First, Fama and French form their portfolios in June of year \\t\\, whereby the returns of July are the first monthly return for the respective portfolio. For firm size, they consequently use the market capitalization recorded for June. It is then held constant until June of year \\t+1\\.

Second, Fama and French also have a different protocol for computing the book-to-market ratio. They use market equity as of the end of year \\t - 1\\ and the book equity reported in year \\t-1\\, i.e., the `datadate` is within the last year. Hence, the book-to-market ratio can be based on accounting information that is up to 18 months old. Market equity also does not necessarily reflect the same time point as book equity. The other sorting variables are analogously to book equity taken from year \\t-1\\.

To implement all these time lags, we again employ the temporary `sorting_date`-column. Notice that when we combine the information, we want to have a single observation per year and stock since we are only interested in computing the breakpoints held constant for the entire year. We ensure this by a call of `distinct()` at the end of the chunk below.

``` r
size <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, sorting_date, size = mktcap)

market_equity <- crsp_monthly |>
  filter(month(date) == 12) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701)"))) |>
  select(permno, gvkey, sorting_date, me = mktcap)

book_to_market <- compustat_annual |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  select(gvkey, sorting_date, be) |>
  inner_join(market_equity, join_by(gvkey, sorting_date)) |>
  mutate(bm = be / me) |>
  select(permno, sorting_date, me, bm)

sorting_variables <- size |>
  inner_join(
    book_to_market,
    join_by(permno, sorting_date)
  ) |>
  drop_na() |>
  distinct(permno, sorting_date, .keep_all = TRUE)
```

## Portfolio Sorts

Next, we construct our portfolios with an adjusted `assign_portfolio()` function. Fama and French rely on NYSE-specific breakpoints to independently form two portfolios in the size dimension at the median and three portfolios in the dimension of book-to-market at the 30- and 70-percentiles. The sorts for book-to-market require an adjustment to the function in [Value and Bivariate Sorts](../r/value-and-bivariate-sorts.llms.md) because it would not produce the right breakpoints. Instead of `n_portfolios`, we now specify `percentiles`, which takes the sequence of breakpoints as an object specified in the function’s call. Specifically, we give `percentiles = c(0.3, 0.7)` to the function. Additionally, we perform an `inner_join()` with our return data to ensure that we only use traded stocks when computing the breakpoints as a first step.

``` r
assign_portfolio <- function(
  data,
  sorting_variable,
  percentiles
) {
  breakpoints <- data |>
    filter(exchange == "NYSE") |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = c(0, percentiles, 1),
      na.rm = TRUE,
      names = FALSE
    )

  assigned_portfolios <- data |>
    mutate(
      portfolio = findInterval(
        pick(everything()) |>
          pull({{ sorting_variable }}),
        breakpoints,
        all.inside = TRUE
      )
    ) |>
    pull(portfolio)

  assigned_portfolios
}

portfolios <- sorting_variables |>
  group_by(sorting_date) |>
  mutate(
    portfolio_size = assign_portfolio(
      data = pick(everything()),
      sorting_variable = "size",
      percentiles = c(0.5)
    ),
    portfolio_bm = assign_portfolio(
      data = pick(everything()),
      sorting_variable = "bm",
      percentiles = c(0.3, 0.7)
    )
  ) |>
  ungroup() |>
  select(permno, sorting_date, portfolio_size, portfolio_bm)
```

Alternatively, you can implement the same portfolio sorts using the `assign_portfolio()` function from the `tidyfinance` package, which we omit to avoid repeating almost the same code chunk as above.

Next, we merge the portfolios to the return data for the rest of the year. To implement this step, we create a new column `sorting_date` in our return data by setting the date to sort on to July of \\t-1\\ if the month is June (of year \\t\\) or earlier or to July of year \\t\\ if the month is July or later.

``` r
portfolios <- crsp_monthly |>
  mutate(
    sorting_date = case_when(
      month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
      month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
    )
  ) |>
  inner_join(portfolios, join_by(permno, sorting_date))
```

## Fama-French Three-Factor Model

Equipped with the return data and the assigned portfolios, we can now compute the value-weighted average return for each of the six portfolios. Then, we form the Fama-French factors. For the size factor (i.e., SMB), we go long in the three small portfolios and short the three large portfolios by taking an average across either group. For the value factor (i.e., HML), we go long in the two high book-to-market portfolios and short the two low book-to-market portfolios, again weighting them equally.

``` r
factors_replicated <- portfolios |>
  group_by(portfolio_size, portfolio_bm, date) |>
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  ) |>
  group_by(date) |>
  summarize(
    smb_replicated = mean(ret[portfolio_size == 1]) -
      mean(ret[portfolio_size == 2]),
    hml_replicated = mean(ret[portfolio_bm == 3]) -
      mean(ret[portfolio_bm == 1])
  )
```

## Replication Evaluation

In the previous section, we replicated the size and value premiums following the procedure outlined by Fama and French. The final question is then: how close did we get? We answer this question by looking at the two time-series estimates in a regression analysis using `lm()`. If we did a good job, then we should see a non-significant intercept (rejecting the notion of systematic error), a coefficient close to 1 (indicating a high correlation), and an adjusted R-squared close to 1 (indicating a high proportion of explained variance).

``` r
test <- factors_ff3_monthly |>
  inner_join(factors_replicated, join_by(date)) |>
  mutate(
    across(c(smb_replicated, hml_replicated), ~ round(., 4))
  )
```

To test the success of the SMB factor, we hence run the following regression:

``` r
model_smb <- lm(smb ~ smb_replicated, data = test)
summary(model_smb)
```


    Call:
    lm(formula = smb ~ smb_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.020203 -0.001374  0.000019  0.001461  0.015537 

    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    -0.000165   0.000125   -1.32     0.19    
    smb_replicated  0.979116   0.004110  238.21   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00346 on 760 degrees of freedom
    Multiple R-squared:  0.987, Adjusted R-squared:  0.987 
    F-statistic: 5.67e+04 on 1 and 760 DF,  p-value: <2e-16

The results for the SMB factor are really convincing, as all three criteria outlined above are met and the coefficient is 0.98 and the R-squared is at 99 percent.

``` r
model_hml <- lm(hml ~ hml_replicated, data = test)
summary(model_hml)
```


    Call:
    lm(formula = hml ~ hml_replicated, data = test)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.02360 -0.00247 -0.00018  0.00202  0.03114 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    0.000407   0.000209    1.95    0.052 .  
    hml_replicated 0.955236   0.006893  138.58   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00574 on 760 degrees of freedom
    Multiple R-squared:  0.962, Adjusted R-squared:  0.962 
    F-statistic: 1.92e+04 on 1 and 760 DF,  p-value: <2e-16

The replication of the HML factor is also a success, although at a slightly lower coefficient of 0.96 and an R-squared around 96 percent.

The evidence hence allows us to conclude that we did a relatively good job in replicating the original Fama-French size and value premiums, although we do not know their underlying code. From our perspective, a perfect match is only possible with additional information from the maintainers of the original data.

## Fama-French Five-Factor Model

Now, let us move to the replication of the five-factor model. We extend the `other_sorting_variables` table from above with the additional characteristics operating profitability `op` and investment `inv`. Note that the `drop_na()` statement yields different sample sizes as some firms with `be` values might not have `op` or `inv` values.

``` r
other_sorting_variables <- compustat_annual |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  select(gvkey, sorting_date, be, op, inv) |>
  inner_join(market_equity, join_by(gvkey, sorting_date)) |>
  mutate(bm = be / me) |>
  select(permno, sorting_date, me, be, bm, op, inv)

sorting_variables <- size |>
  inner_join(
    other_sorting_variables,
    join_by(permno, sorting_date)
  ) |>
  drop_na() |>
  distinct(permno, sorting_date, .keep_all = TRUE)
```

In each month, we independently sort all stocks into the two size portfolios. The value, profitability, and investment portfolios, on the other hand, are the results of dependent sorts based on the size portfolios. We then merge the portfolios to the return data for the rest of the year just as above.

``` r
portfolios <- sorting_variables |>
  group_by(sorting_date) |>
  mutate(
    portfolio_size = assign_portfolio(
      data = pick(everything()),
      sorting_variable = "size",
      percentiles = c(0.5)
    )
  ) |>
  group_by(sorting_date, portfolio_size) |>
  mutate(
    across(
      c(bm, op, inv),
      ~ assign_portfolio(
        data = pick(everything()),
        sorting_variable = .,
        percentiles = c(0.3, 0.7)
      ),
      .names = "portfolio_{.col}"
    )
  ) |>
  ungroup() |>
  select(
    permno,
    sorting_date,
    portfolio_size,
    portfolio_bm,
    portfolio_op,
    portfolio_inv
  )

portfolios <- crsp_monthly |>
  mutate(
    sorting_date = case_when(
      month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
      month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
    )
  ) |>
  inner_join(portfolios, join_by(permno, sorting_date))
```

Now, we want to construct each of the factors, but this time the size factor actually comes last because it is the result of averaging across all other factor portfolios. This dependency is the reason why we keep the table with value-weighted portfolio returns as a separate object that we reuse later. We construct the value factor, HML, as above by going long the two portfolios with high book-to-market ratios and shorting the two portfolios with low book-to-market.

``` r
portfolios_value <- portfolios |>
  group_by(portfolio_size, portfolio_bm, date) |>
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  )

factors_value <- portfolios_value |>
  group_by(date) |>
  summarize(
    hml_replicated = mean(ret[portfolio_bm == 3]) -
      mean(ret[portfolio_bm == 1])
  )
```

For the profitability factor, RMW, we take a long position in the two high profitability portfolios and a short position in the two low profitability portfolios.

``` r
portfolios_profitability <- portfolios |>
  group_by(portfolio_size, portfolio_op, date) |>
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  )

factors_profitability <- portfolios_profitability |>
  group_by(date) |>
  summarize(
    rmw_replicated = mean(ret[portfolio_op == 3]) -
      mean(ret[portfolio_op == 1])
  )
```

For the investment factor, CMA, we go long the two low investment portfolios and short the two high investment portfolios.

``` r
portfolios_investment <- portfolios |>
  group_by(portfolio_size, portfolio_inv, date) |>
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  )

factors_investment <- portfolios_investment |>
  group_by(date) |>
  summarize(
    cma_replicated = mean(ret[portfolio_inv == 1]) -
      mean(ret[portfolio_inv == 3])
  )
```

Finally, the size factor, SMB, is constructed by going long the nine small portfolios and short the nine large portfolios.

``` r
factors_size <- bind_rows(
  portfolios_value,
  portfolios_profitability,
  portfolios_investment
) |>
  group_by(date) |>
  summarize(
    smb_replicated = mean(ret[portfolio_size == 1]) -
      mean(ret[portfolio_size == 2])
  )
```

We then join all factors together into one dataframe and construct again a suitable table to run tests for evaluating our replication.

``` r
factors_replicated <- factors_size |>
  full_join(factors_value, join_by(date)) |>
  full_join(factors_profitability, join_by(date)) |>
  full_join(factors_investment, join_by(date))

test <- factors_ff5_monthly |>
  inner_join(factors_replicated, join_by(date)) |>
  mutate(
    across(
      c(smb_replicated, hml_replicated, rmw_replicated, cma_replicated),
      ~ round(., 4)
    )
  )
```

Let us start the replication evaluation again with the size factor:

``` r
model_smb <- lm(smb ~ smb_replicated, data = test)
summary(model_smb)
```


    Call:
    lm(formula = smb ~ smb_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.019394 -0.001875  0.000166  0.001942  0.013436 

    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    -0.000221   0.000129   -1.72    0.086 .  
    smb_replicated  0.959342   0.004084  234.88   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00349 on 736 degrees of freedom
    Multiple R-squared:  0.987, Adjusted R-squared:  0.987 
    F-statistic: 5.52e+04 on 1 and 736 DF,  p-value: <2e-16

The results for the SMB factor are quite convincing as all three criteria outlined above are met and the coefficient is 0.96 and the R-squared is at 99 percent.

``` r
model_hml <- lm(hml ~ hml_replicated, data = test)
summary(model_hml)
```


    Call:
    lm(formula = hml ~ hml_replicated, data = test)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.04341 -0.00409 -0.00030  0.00384  0.03453 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    0.000609   0.000287    2.12    0.034 *  
    hml_replicated 0.980389   0.009798  100.06   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00778 on 736 degrees of freedom
    Multiple R-squared:  0.932, Adjusted R-squared:  0.931 
    F-statistic: 1e+04 on 1 and 736 DF,  p-value: <2e-16

The replication of the HML factor is also a success, although at a slightly higher coefficient of 0.98 and an R-squared around 93 percent.

``` r
model_rmw <- lm(rmw ~ rmw_replicated, data = test)
summary(model_rmw)
```


    Call:
    lm(formula = rmw ~ rmw_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.018489 -0.003047 -0.000012  0.003281  0.019066 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    2.83e-05   1.99e-04    0.14     0.89    
    rmw_replicated 9.49e-01   8.71e-03  108.99   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00536 on 736 degrees of freedom
    Multiple R-squared:  0.942, Adjusted R-squared:  0.942 
    F-statistic: 1.19e+04 on 1 and 736 DF,  p-value: <2e-16

We are also able to replicate the RMW factor quite well with a coefficient of 0.95 and an R-squared around 94 percent.

``` r
model_cma <- lm(cma ~ cma_replicated, data = test)
summary(model_cma)
```


    Call:
    lm(formula = cma ~ cma_replicated, data = test)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.02876 -0.00274  0.00004  0.00260  0.02121 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    0.000523   0.000172    3.05   0.0024 ** 
    cma_replicated 0.955140   0.008140  117.33   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00464 on 736 degrees of freedom
    Multiple R-squared:  0.949, Adjusted R-squared:  0.949 
    F-statistic: 1.38e+04 on 1 and 736 DF,  p-value: <2e-16

Finally, the CMA factor also replicates well with a coefficient of 0.96 and an R-squared around 95 percent.

Overall, our approach seems to replicate the Fama-French five-factor models just as well as the three factors.

## Key Takeaways

- The three-factor model adds size (SMB) and value (HML) to the traditional CAPM, while the five-factor model extends this with profitability (RMW) and investment (CMA) factors.
- The portfolio construction follows the original Fama-French methodology, including NYSE breakpoints, specific time lags, and sorting rules based on firm characteristics.
- The quality of replication can be evaluated using regression analysis and confirms strong alignment with the original Fama-French data.

## Exercises

1.  Replicate the market factor `mkt_excess` from the `factors_ff3_monthly` data as the value-weight return of all CRSP firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ that have a CRSP share code of 10 or 11. Assess your replication effort using linear regressions as above.
2.  Fama and French ([1993](#ref-Fama1993)) claim that their sample excludes firms until they have appeared in Compustat for two years. Implement this additional filter and compare the improvements of your replication effort.
3.  On his homepage, [Kenneth French](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/variable_definitions.html) provides instructions on how to construct the most common variables used for portfolio sorts. Try to replicate the univariate portfolio sort return time series for `E/P` (earnings/price) provided on his homepage and evaluate your replication effort using regressions.

## References

Fama, Eugene F., and Kenneth R. French. 1992. “The cross-section of expected stock returns.” *The Journal of Finance* 47 (2): 427–65. <https://doi.org/2329112>.

Fama, Eugene F., and Kenneth R. French. 1993. “Common risk factors in the returns on stocks and bonds.” *Journal of Financial Economics* 33 (1): 3–56. <https://doi.org/10.1016/0304-405X(93)90023-5>.

Fama, Eugene F., and Kenneth R. French. 2015. “A Five-Factor Asset Pricing Model.” *Journal of Financial Economics* 116 (1): 1–22. <https://doi.org/10.1016/j.jfineco.2014.10.010>.

## Footnotes

[^1]: Note that Fama and French ([1992](#ref-Fama1992)) claim to exclude financial firms. To a large extent this happens through using industry format “INDL”, as we do in [WRDS, CRSP, and Compustat](../r/wrds-crsp-and-compustat.llms.md). Neither the original paper, nor Ken French’s website, or the WRDS replication contains any indication that financial companies are excluded using additional filters such as industry codes.
