# Replicating Fama-French Factors

In this chapter, we provide a replication of the famous Fama-French factor portfolios. The Fama-French factor models are a cornerstone of empirical asset pricing Fama and French ([2015](#ref-FamaFrench2015)). On top of the market factor represented by the traditional CAPM beta, the three-factor model includes the size and value factors to explain the cross section of returns. Its successor, the five-factor model, additionally includes profitability and investment as explanatory factors.

We start with the three-factor model. We already introduced the size and value factors in [Value and Bivariate Sorts](../chapters/value-and-bivariate-sorts.llms.md), and their definition remains the same: size is the SMB factor (small-minus-big) that is long small firms and short large firms. The value factor is HML (high-minus-low) and is long in high book-to-market firms and short in low book-to-market counterparts.

After the replication of the three-factor model, we move to the five factors by constructing the profitability factor RMW (robust-minus-weak) as the difference between the returns of firms with high and low operating profitability and the investment factor CMA (conservative-minus-aggressive) as the difference between firms with high versus low investment rates.

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
library(nanoparquet)
```

## Python

``` python
import polars as pl
import numpy as np
import pyfixest as pf
```

## Data Preparation

We use CRSP and Compustat as data sources, as we need the same variables to compute the factors in the way Fama and French do it. Hence, we only load data from our Parquet files introduced in [Accessing and Managing Financial Data](../chapters/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md).[^1] The only addition is that we keep only firms with positive book equity, which is a common practice when working with book-to-market ratios (see [Fama and French 1992](#ref-Fama1992) for details).

## R

``` r
crsp_monthly <- read_parquet("data/crsp_monthly.parquet") |>
  select(
    permno,
    gvkey,
    date,
    ret_excess,
    mktcap,
    mktcap_lag,
    exchange
  )

compustat_annual <- read_parquet("data/compustat_annual.parquet") |>
  select(gvkey, datadate, be, op, inv) |>
  filter(be > 0)

factors_ff3_monthly <- read_parquet("data/factors_ff3_monthly.parquet") |>
  select(date, smb, hml)

factors_ff5_monthly <- read_parquet("data/factors_ff5_monthly.parquet") |>
  select(date, smb, hml, rmw, cma)
```

## Python

``` python
crsp_monthly = (
    pl.read_parquet("data/crsp_monthly.parquet")
    .select(["permno", "gvkey", "date", "ret_excess", "mktcap",
        "mktcap_lag", "exchange"])
)

compustat_annual = (
    pl.read_parquet("data/compustat_annual.parquet")
    .select(["gvkey", "datadate", "be", "op", "inv"])
    .filter(pl.col("be") > 0)
)

factors_ff3_monthly = (
    pl.read_parquet("data/factors_ff3_monthly.parquet")
    .select(["date", "smb", "hml"])
)

factors_ff5_monthly = (
    pl.read_parquet("data/factors_ff5_monthly.parquet")
    .select(["date", "smb", "hml", "rmw", "cma"])
)
```

Yet when we start merging our dataset for computing the premiums, there are a few differences to [Value and Bivariate Sorts](../chapters/value-and-bivariate-sorts.llms.md). First, Fama and French form their portfolios in June of year \\t\\, whereby the returns of July are the first monthly return for the respective portfolio. For firm size, they consequently use the market capitalization recorded for June. It is then held constant until June of year \\t+1\\.

Second, Fama and French also have a different protocol for computing the book-to-market ratio. They use market equity as of the end of year \\t - 1\\ and the book equity reported in year \\t-1\\, i.e., the `datadate` is within the last year. Hence, the book-to-market ratio can be based on accounting information that is up to 18 months old. Market equity also does not necessarily reflect the same time point as book equity. The other sorting variables are analogously to book equity taken from year \\t-1\\.

To implement all these time lags, we again employ the temporary `sorting_date`-column. Notice that when we combine the information, we want to have a single observation per year and stock since we are only interested in computing the breakpoints held constant for the entire year.

## R

We ensure a single observation per year and stock by a call of `distinct()` at the end of the chunk below.

``` r
size <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, sorting_date, size = mktcap)

market_equity <- crsp_monthly |>
  filter(month(date) == 12) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |>
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

## Python

We ensure a single observation per year and stock by a call of `unique()` at the end of the chunk below.

``` python
size = (crsp_monthly
    .filter(pl.col("date").dt.month() == 6)
    .with_columns(sorting_date=pl.col("date").dt.offset_by("1mo").cast(pl.Date))
    .select(["permno", "exchange", "sorting_date", "mktcap"])
    .rename({"mktcap": "size"})
)

market_equity = (crsp_monthly
    .filter(pl.col("date").dt.month() == 12)
    .with_columns(sorting_date=pl.col("date").dt.offset_by("7mo").cast(pl.Date))
    .select(["permno", "gvkey", "sorting_date", "mktcap"])
    .rename({"mktcap": "me"})
)

book_to_market = (compustat_annual
    .with_columns(
        sorting_date=pl.date(pl.col("datadate").dt.year() + 1, 7, 1)
    )
    .join(market_equity, how="inner", on=["gvkey", "sorting_date"])
    .with_columns(bm=pl.col("be") / pl.col("me"))
    .select(["permno", "sorting_date", "me", "bm"])
)

sorting_variables = (size
    .join(book_to_market, how="inner", on=["permno", "sorting_date"])
    .drop_nulls()
    .unique(subset=["permno", "sorting_date"], keep="first")
 )
```

## Portfolio Sorts

Next, we construct our portfolios with an adjusted `assign_portfolio()` function. Fama and French rely on NYSE-specific breakpoints to independently form two portfolios in the size dimension at the median and three portfolios in the dimension of book-to-market at the 30- and 70-percentiles. The sorts for book-to-market require an adjustment to the function in [Value and Bivariate Sorts](../chapters/value-and-bivariate-sorts.llms.md) because it would not produce the right breakpoints. Instead of `n_portfolios`, we now specify `percentiles`, which takes the sequence of breakpoints as an object specified in the function’s call. Additionally, we perform an inner join with our return data to ensure that we only use traded stocks when computing the breakpoints as a first step.

## R

Specifically, we give `percentiles = c(0.3, 0.7)` to the function.

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

## Python

Specifically, we give `percentiles = [0.3, 0.7]` to the function.

``` python
def assign_portfolio(data, sorting_variable, percentiles):
    """Assign portfolios to a bin according to a sorting variable."""

    breakpoints = np.quantile(
      data.filter(pl.col("exchange") == "NYSE")
        .get_column(sorting_variable)
        .to_numpy(),
      percentiles, method="linear"
    )
    breakpoints = np.unique(breakpoints)

    assigned_portfolios = (data
      .select(pl.col(sorting_variable)
        .cut(
            list(breakpoints[1:-1]),
            labels=[str(i) for i in range(1, len(breakpoints))],
            left_closed=True
        )
        .cast(pl.String)
        .cast(pl.Int64)
        .alias("portfolio")
      )
      .get_column("portfolio")
    )

    return assigned_portfolios

portfolios = (sorting_variables
    .group_by("sorting_date")
    .map_groups(lambda x: x
        .with_columns(
        portfolio_size=assign_portfolio(x, "size", [0, 0.5, 1]),
        portfolio_bm=assign_portfolio(x, "bm", [0, 0.3, 0.7, 1])
        )
    )
    .select(["permno", "sorting_date", "portfolio_size", "portfolio_bm"])
)
```

Alternatively, you can implement the same portfolio sorts using the `assign_portfolio()` function from the `tidyfinance` package, which we omit to avoid repeating almost the same code chunk as above.

Next, we merge the portfolios to the return data for the rest of the year. To implement this step, we create a new column `sorting_date` in our return data by setting the date to sort on to July of \\t-1\\ if the month is June (of year \\t\\) or earlier or to July of year \\t\\ if the month is July or later.

## R

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

## Python

``` python
portfolios = (crsp_monthly
    .with_columns(
        sorting_date=pl.when(pl.col("date").dt.month() <= 6)
            .then(pl.date(pl.col("date").dt.year() - 1, 7, 1))
            .otherwise(pl.date(pl.col("date").dt.year(), 7, 1))
    )
    .join(portfolios, how="inner", on=["permno", "sorting_date"])
)
```

## Fama-French Three-Factor Model

Equipped with the return data and the assigned portfolios, we can now compute the value-weighted average return for each of the six portfolios. Then, we form the Fama-French factors. For the size factor (i.e., SMB), we go long in the three small portfolios and short the three large portfolios by taking an average across either group. For the value factor (i.e., HML), we go long in the two high book-to-market portfolios and short the two low book-to-market portfolios, again weighting them equally.

## R

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

## Python

``` python
factors_replicated = (portfolios
    .group_by(["portfolio_size", "portfolio_bm", "date"])
    .agg(ret=(pl.col("ret_excess") * pl.col("mktcap_lag")).sum()
        / pl.col("mktcap_lag").sum())
    .group_by("date")
    .agg(
        smb_replicated=(
            pl.col("ret").filter(pl.col("portfolio_size") == 1).mean()
            - pl.col("ret").filter(pl.col("portfolio_size") == 2).mean()),
        hml_replicated=(
            pl.col("ret").filter(pl.col("portfolio_bm") == 3).mean()
            - pl.col("ret").filter(pl.col("portfolio_bm") == 1).mean())
    )
)
```

## Replication Evaluation

In the previous section, we replicated the size and value premiums following the procedure outlined by Fama and French. The final question is then: how close did we get? We answer this question by looking at the two time-series estimates in a regression analysis. If we did a good job, then we should see a non-significant intercept (rejecting the notion of systematic error), a coefficient close to one (indicating a high correlation), and an adjusted R-squared close to one (indicating a high proportion of explained variance).

## R

We run the regressions using `lm()`.

``` r
test <- factors_ff3_monthly |>
  inner_join(factors_replicated, join_by(date)) |>
  mutate(
    across(c(smb_replicated, hml_replicated), ~ round(., 4))
  )
```

## Python

We run the regressions using `pf.feols()`.

``` python
test = (factors_replicated
    .join(factors_ff3_monthly, how="inner", on="date")
    .with_columns(pl.col("^.*_replicated$").round(4))
)
```

To test the success of the SMB factor, we hence run the following regression:

## R

``` r
model_smb <- lm(smb ~ smb_replicated, data = test)
summary(model_smb)
```


    Call:
    lm(formula = smb ~ smb_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.020200 -0.001371  0.000008  0.001434  0.015442 

    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    -0.000168   0.000125   -1.34     0.18    
    smb_replicated  0.979119   0.004107  238.38   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00345 on 760 degrees of freedom
    Multiple R-squared:  0.987, Adjusted R-squared:  0.987 
    F-statistic: 5.68e+04 on 1 and 760 DF,  p-value: <2e-16

The results for the SMB factor are really convincing, as all three criteria outlined above are met and the coefficient is 0.98 and the R-squared is at 99 percent.

## Python

``` python
model_smb = pf.feols(
    fml="smb ~ smb_replicated",
    data=test
)
model_smb.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: smb, Fixed effects: 0
    Inference:  iid
    Observations:  762

    | Coefficient    |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:---------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept      |     -0.000 |        0.000 |    -1.338 |      0.181 | -0.000 |   0.000 |
    | smb_replicated |      0.979 |        0.004 |   238.383 |      0.000 |  0.971 |   0.987 |
    ---
    RMSE: 0.003 R2: 0.987 

The results for the SMB factor are really convincing, as all three criteria outlined above are met and the coefficient is 0.98 and the R-squared is at 99 percent.

## R

``` r
model_hml <- lm(hml ~ hml_replicated, data = test)
summary(model_hml)
```


    Call:
    lm(formula = hml ~ hml_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.023587 -0.002528 -0.000181  0.002026  0.031125 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    0.000404   0.000210    1.93    0.055 .  
    hml_replicated 0.955124   0.006915  138.12   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00576 on 760 degrees of freedom
    Multiple R-squared:  0.962, Adjusted R-squared:  0.962 
    F-statistic: 1.91e+04 on 1 and 760 DF,  p-value: <2e-16

The replication of the HML factor is also a success, although at a slightly lower coefficient of 0.96 and an R-squared around 96 percent.

## Python

``` python
model_hml = pf.feols(
    fml="hml ~ hml_replicated",
    data=test
)
model_hml.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: hml, Fixed effects: 0
    Inference:  iid
    Observations:  762

    | Coefficient    |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:---------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept      |      0.000 |        0.000 |     1.926 |      0.055 | -0.000 |   0.001 |
    | hml_replicated |      0.955 |        0.007 |   138.117 |      0.000 |  0.942 |   0.969 |
    ---
    RMSE: 0.006 R2: 0.962 

The replication of the HML factor is also a success, although at a slightly lower coefficient of 0.96 and an R-squared around 96 percent.

The evidence hence allows us to conclude that we did a relatively good job in replicating the original Fama-French size and value premiums, although we do not know their underlying code. From our perspective, a perfect match is only possible with additional information from the maintainers of the original data.

## Fama-French Five-Factor Model

Now, let us move to the replication of the five-factor model. We extend the `other_sorting_variables` table from above with the additional characteristics operating profitability `op` and investment `inv`. Note that the missing-value statement yields different sample sizes, as some firms with `be` values might not have `op` or `inv` values.

## R

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

## Python

``` python
other_sorting_variables = (compustat_annual
    .with_columns(
        sorting_date=pl.date(pl.col("datadate").dt.year() + 1, 7, 1)
    )
    .join(market_equity, how="inner", on=["gvkey", "sorting_date"])
    .with_columns(bm=pl.col("be") / pl.col("me"))
    .select(["permno", "sorting_date", "me", "be", "bm", "op", "inv"])
)

sorting_variables = (size
    .join(other_sorting_variables, how="inner", on=["permno", "sorting_date"])
    .drop_nulls()
    .unique(subset=["permno", "sorting_date"], keep="first")
 )
```

In each month, we independently sort all stocks into the two size portfolios. The value, profitability, and investment portfolios, on the other hand, are the results of dependent sorts based on the size portfolios. We then merge the portfolios to the return data for the rest of the year just as above.

## R

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

## Python

``` python
portfolios = (sorting_variables
    .group_by("sorting_date")
    .map_groups(lambda x: x
        .with_columns(
        portfolio_size=assign_portfolio(x, "size", [0, 0.5, 1])
        )
    )
    .group_by("sorting_date", "portfolio_size")
    .map_groups(lambda x: x
        .with_columns(
        portfolio_bm=assign_portfolio(x, "bm", [0, 0.3, 0.7, 1]),
        portfolio_op=assign_portfolio(x, "op", [0, 0.3, 0.7, 1]),
        portfolio_inv=assign_portfolio(x, "inv", [0, 0.3, 0.7, 1])
        )
    )
    .select(["permno", "sorting_date",
            "portfolio_size", "portfolio_bm",
            "portfolio_op", "portfolio_inv"])
)

portfolios = (crsp_monthly
    .with_columns(
        sorting_date=pl.when(pl.col("date").dt.month() <= 6)
            .then(pl.date(pl.col("date").dt.year() - 1, 7, 1))
            .otherwise(pl.date(pl.col("date").dt.year(), 7, 1))
    )
    .join(portfolios, how="inner", on=["permno", "sorting_date"])
)
```

Now, we want to construct each of the factors, but this time the size factor actually comes last because it is the result of averaging across all other factor portfolios. This dependency is the reason why we keep the table with value-weighted portfolio returns as a separate object that we reuse later. We construct the value factor, HML, as above by going long the two portfolios with high book-to-market ratios and shorting the two portfolios with low book-to-market.

## R

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

## Python

``` python
portfolios_value = (portfolios
    .group_by(["portfolio_size", "portfolio_bm", "date"])
    .agg(ret=(pl.col("ret_excess") * pl.col("mktcap_lag")).sum()
        / pl.col("mktcap_lag").sum())
)

factors_value = (portfolios_value
    .group_by("date")
    .agg(hml_replicated=(
        pl.col("ret").filter(pl.col("portfolio_bm") == 3).mean()
        - pl.col("ret").filter(pl.col("portfolio_bm") == 1).mean()))
)
```

For the profitability factor, RMW, we take a long position in the two high profitability portfolios and a short position in the two low profitability portfolios.

## R

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

## Python

``` python
portfolios_profitability = (portfolios
    .group_by(["portfolio_size", "portfolio_op", "date"])
    .agg(ret=(pl.col("ret_excess") * pl.col("mktcap_lag")).sum()
        / pl.col("mktcap_lag").sum())
)

factors_profitability = (portfolios_profitability
    .group_by("date")
    .agg(rmw_replicated=(
        pl.col("ret").filter(pl.col("portfolio_op") == 3).mean()
        - pl.col("ret").filter(pl.col("portfolio_op") == 1).mean()))
)
```

For the investment factor, CMA, we go long the two low investment portfolios and short the two high investment portfolios.

## R

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

## Python

``` python
portfolios_investment = (portfolios
    .group_by(["portfolio_size", "portfolio_inv", "date"])
    .agg(ret=(pl.col("ret_excess") * pl.col("mktcap_lag")).sum()
        / pl.col("mktcap_lag").sum())
)

factors_investment = (portfolios_investment
    .group_by("date")
    .agg(cma_replicated=(
        pl.col("ret").filter(pl.col("portfolio_inv") == 1).mean()
        - pl.col("ret").filter(pl.col("portfolio_inv") == 3).mean()))
)
```

Finally, the size factor, SMB, is constructed by going long the nine small portfolios and short the nine large portfolios.

## R

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

## Python

``` python
factors_size = (
    pl.concat(
        [portfolios_value.select(["portfolio_size", "date", "ret"]),
         portfolios_profitability.select(["portfolio_size", "date", "ret"]),
         portfolios_investment.select(["portfolio_size", "date", "ret"])]
    )
    .group_by("date")
    .agg(smb_replicated=(
        pl.col("ret").filter(pl.col("portfolio_size") == 1).mean()
        - pl.col("ret").filter(pl.col("portfolio_size") == 2).mean()))
)
```

We then join all factors together into one data frame and construct again a suitable table to run tests for evaluating our replication.

## R

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

## Python

``` python
factors_replicated = (factors_size
    .join(factors_value, how="full", on="date", coalesce=True)
    .join(factors_profitability, how="full", on="date", coalesce=True)
    .join(factors_investment, how="full", on="date", coalesce=True)
)

test = (factors_replicated
    .join(factors_ff5_monthly, how="inner", on="date")
    .with_columns(pl.col("^.*_replicated$").round(4))
)
```

Let us start the replication evaluation again with the size factor:

## R

``` r
model_smb <- lm(smb ~ smb_replicated, data = test)
summary(model_smb)
```


    Call:
    lm(formula = smb ~ smb_replicated, data = test)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.01939 -0.00187  0.00015  0.00195  0.01345 

    Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    -0.000229   0.000129   -1.77    0.077 .  
    smb_replicated  0.959233   0.004104  233.73   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.0035 on 736 degrees of freedom
    Multiple R-squared:  0.987, Adjusted R-squared:  0.987 
    F-statistic: 5.46e+04 on 1 and 736 DF,  p-value: <2e-16

The results for the SMB factor are quite convincing as all three criteria outlined above are met and the coefficient is 0.96 and the R-squared is at 99 percent.

## Python

``` python
model_smb = pf.feols(
    fml="smb ~ smb_replicated",
    data=test
)
model_smb.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: smb, Fixed effects: 0
    Inference:  iid
    Observations:  738

    | Coefficient    |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:---------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept      |     -0.000 |        0.000 |    -1.768 |      0.077 | -0.000 |   0.000 |
    | smb_replicated |      0.959 |        0.004 |   233.729 |      0.000 |  0.951 |   0.967 |
    ---
    RMSE: 0.003 R2: 0.987 

The results for the SMB factor are quite convincing, as all three criteria outlined above are met and the coefficient is 0.96 and the R-squared is at 99 percent.

## R

``` r
model_hml <- lm(hml ~ hml_replicated, data = test)
summary(model_hml)
```


    Call:
    lm(formula = hml ~ hml_replicated, data = test)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.04314 -0.00405 -0.00034  0.00386  0.03455 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    0.000603   0.000288     2.1    0.036 *  
    hml_replicated 0.979547   0.009801    99.9   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00779 on 736 degrees of freedom
    Multiple R-squared:  0.931, Adjusted R-squared:  0.931 
    F-statistic: 9.99e+03 on 1 and 736 DF,  p-value: <2e-16

The replication of the HML factor is also a success, although at a slightly higher coefficient of 0.98 and an R-squared around 93 percent.

## Python

``` python
model_hml = pf.feols(
    fml="hml ~ hml_replicated",
    data=test
)
model_hml.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: hml, Fixed effects: 0
    Inference:  iid
    Observations:  738

    | Coefficient    |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:---------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept      |      0.001 |        0.000 |     2.095 |      0.036 |  0.000 |   0.001 |
    | hml_replicated |      0.980 |        0.010 |    99.940 |      0.000 |  0.960 |   0.999 |
    ---
    RMSE: 0.008 R2: 0.931 

The replication of the HML factor is also a success, although at a slightly higher coefficient of 0.98 and an R-squared around 93 percent.

## R

``` r
model_rmw <- lm(rmw ~ rmw_replicated, data = test)
summary(model_rmw)
```


    Call:
    lm(formula = rmw ~ rmw_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.018496 -0.003051 -0.000013  0.003297  0.018493 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    2.83e-05   1.99e-04    0.14     0.89    
    rmw_replicated 9.49e-01   8.72e-03  108.79   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00537 on 736 degrees of freedom
    Multiple R-squared:  0.941, Adjusted R-squared:  0.941 
    F-statistic: 1.18e+04 on 1 and 736 DF,  p-value: <2e-16

We are also able to replicate the RMW factor quite well with a coefficient of 0.95 and an R-squared around 94 percent.

## Python

``` python
model_rmw = pf.feols(
    fml="rmw ~ rmw_replicated",
    data=test
)
model_rmw.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: rmw, Fixed effects: 0
    Inference:  iid
    Observations:  738

    | Coefficient    |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:---------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept      |      0.000 |        0.000 |     0.142 |      0.887 | -0.000 |   0.000 |
    | rmw_replicated |      0.949 |        0.009 |   108.793 |      0.000 |  0.932 |   0.966 |
    ---
    RMSE: 0.005 R2: 0.941 

We are also able to replicate the RMW factor quite well with a coefficient of 0.95 and an R-squared around 94 percent.

## R

``` r
model_cma <- lm(cma ~ cma_replicated, data = test)
summary(model_cma)
```


    Call:
    lm(formula = cma ~ cma_replicated, data = test)

    Residuals:
          Min        1Q    Median        3Q       Max 
    -0.028737 -0.002733  0.000006  0.002622  0.021216 

    Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
    (Intercept)    0.000519   0.000172    3.01   0.0027 ** 
    cma_replicated 0.954025   0.008163  116.87   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.00466 on 736 degrees of freedom
    Multiple R-squared:  0.949, Adjusted R-squared:  0.949 
    F-statistic: 1.37e+04 on 1 and 736 DF,  p-value: <2e-16

Finally, the CMA factor also replicates well with a coefficient of 0.95 and an R-squared around 95 percent.

## Python

``` python
model_cma = pf.feols(
    fml="cma ~ cma_replicated",
    data=test
)
model_cma.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: cma, Fixed effects: 0
    Inference:  iid
    Observations:  738

    | Coefficient    |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:---------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept      |      0.001 |        0.000 |     3.013 |      0.003 |  0.000 |   0.001 |
    | cma_replicated |      0.954 |        0.008 |   116.871 |      0.000 |  0.938 |   0.970 |
    ---
    RMSE: 0.005 R2: 0.949 

Finally, the CMA factor also replicates well with a coefficient of 0.95 and an R-squared around 95 percent.

Overall, our approach seems to replicate the Fama-French five-factor model just as well as the three factors.

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

[^1]: Note that Fama and French ([1992](#ref-Fama1992)) claim to exclude financial firms. To a large extent this happens through using industry format “INDL”, as we do in [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md). Neither the original paper, nor Ken French’s website, or the WRDS replication contains any indication that financial companies are excluded using additional filters such as industry codes.
