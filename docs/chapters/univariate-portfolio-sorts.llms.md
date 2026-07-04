# Univariate Portfolio Sorts

In this chapter, we dive into portfolio sorts, one of the most widely used statistical methodologies in empirical asset pricing (e.g., [Bali et al. 2016](#ref-BaliEngleMurray2016)). The key application of portfolio sorts is to examine whether one or more variables can predict future excess returns. In general, the idea is to sort individual stocks into portfolios, where the stocks within each portfolio are similar with respect to a sorting variable, such as firm size. The different portfolios then represent well-diversified investments that differ in the level of the sorting variable. You can then attribute the differences in the return distribution to the impact of the sorting variable. We start by introducing univariate portfolio sorts (which sort based on only one characteristic) and tackle bivariate sorting in [Value and Bivariate Sorts](../chapters/value-and-bivariate-sorts.llms.md).

A univariate portfolio sort considers only one sorting variable \\x\_{t-1,i}\\. Here, \\i\\ denotes the stock and \\t-1\\ indicates that the characteristic is observable by investors at time \\t\\. The objective is to assess the cross-sectional relation between \\x\_{t-1,i}\\ and, typically, stock excess returns \\r\_{t,i}\\ at time \\t\\ as the outcome variable. To illustrate how portfolio sorts work, we use estimates for market betas from the previous chapter as our sorting variable.

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
library(nanoparquet)
library(scales)
library(lmtest)
library(broom)
library(sandwich)

theme_set(theme_minimal())
```

Compared to previous chapters, we introduce `lmtest` ([Zeileis and Hothorn 2002](#ref-lmtest)) for inference for estimated coefficients, `broom` package ([Robinson et al. 2022](#ref-broom)) to tidy the estimation output of many estimated linear models, and `sandwich` ([Zeileis 2006](#ref-sandwich)) for different covariance matrix estimators

## Python

``` python
import polars as pl
import pyfixest as pf

from plotnine import *
from mizani.formatters import percent_format

theme_set(theme_minimal())
```

## Data Preparation

We start with loading the required data from our local folder introduced in [Accessing and Managing Financial Data](../chapters/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md). In particular, we use the monthly CRSP sample as our asset universe. Once we form our portfolios, we use the Fama-French market factor returns to compute the risk-adjusted performance (i.e., alpha). `beta` is the data frame with market betas computed in the previous chapter.

## R

``` r
crsp_monthly <- read_parquet("data/crsp_monthly.parquet") |>
  select(permno, date, ret_excess, mktcap_lag)

factors_ff3_monthly <- read_parquet("data/factors_ff3_monthly.parquet") |>
  select(date, mkt_excess)

beta <- read_parquet("data/beta.parquet") |>
  filter(return_type == "monthly") |>
  select(permno, date, beta)
```

## Python

``` python
crsp_monthly = (
    pl.read_parquet("data/crsp_monthly.parquet")
    .select(["permno", "date", "ret_excess", "mktcap_lag"])
)

factors_ff3_monthly = (
    pl.read_parquet("data/factors_ff3_monthly.parquet")
    .select(["date", "mkt_excess"])
)

beta = (
    pl.read_parquet("data/beta.parquet")
    .filter(pl.col("return_type") == "monthly")
    .select(["permno", "date", "beta"])
)
```

## Sorting by Market Beta

Next, we merge our sorting variable with the return data. We use the one-month *lagged* betas as a sorting variable to ensure that the sorts rely only on information available when we create the portfolios. To lag stock beta by one month, we add one month to the current date and join the resulting information with our return data. This procedure ensures that month \\t\\ information is available in month \\t+1\\.

## R

You may be tempted to simply use a call such as `crsp_monthly |> group_by(permno) |> mutate(beta_lag = lag(beta)))` instead. This procedure, however, does not work correctly if there are non-explicit missing values in the time series.

``` r
beta_lag <- beta |>
  mutate(date = date %m+% months(1)) |>
  select(permno, date, beta_lag = beta) |>
  drop_na()

data_for_sorts <- crsp_monthly |>
  inner_join(beta_lag, join_by(permno, date))
```

## Python

You may be tempted to simply use a call such as `crsp_monthly.with_columns(beta_lag=pl.col("beta").shift(1).over("permno"))` instead. This procedure, however, does not work correctly if there are implicit missing values in the time series.

``` python
beta_lag = (beta
    .with_columns(date=pl.col("date").dt.offset_by("1mo"))
    .select(["permno", "date", "beta"])
    .rename({"beta": "beta_lag"})
    .drop_nulls()
)

data_for_sorts = (crsp_monthly
    .join(beta_lag, how="inner", on=["permno", "date"])
)
```

The first step to conduct portfolio sorts is to calculate periodic breakpoints that you can use to group the stocks into portfolios. For simplicity, we start with the median lagged market beta as the single breakpoint. We then compute the value-weighted returns for each of the two resulting portfolios, which means that the lagged market capitalization determines the weight.

## R

The lagged market capitalization determines the weight in `weighted.mean()`.

``` r
beta_portfolios <- data_for_sorts |>
  group_by(date) |>
  mutate(
    breakpoint = median(beta_lag),
    portfolio = case_when(
      beta_lag <= breakpoint ~ "low",
      beta_lag > breakpoint ~ "high"
    )
  ) |>
  group_by(date, portfolio) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop")
```

## Python

We assign portfolios within each date using `qcut()` with the median as the only breakpoint and divide the sum of capitalization-weighted excess returns by the sum of weights.

``` python
beta_portfolios = (data_for_sorts
    .with_columns(
        portfolio=pl.col("beta_lag")
            .qcut([0.5], labels=["low", "high"])
            .over("date")
    )
    .group_by(["portfolio", "date"])
    .agg(
        ret=(pl.col("ret_excess") * pl.col("mktcap_lag")).sum()
            / pl.col("mktcap_lag").sum()
    )
)
```

## Performance Evaluation

We can construct a long-short strategy based on the two portfolios: buy the high-beta portfolio and, at the same time, short the low-beta portfolio. Thereby, the overall position in the market is net-zero, i.e., you do not need to invest money to realize this strategy in the absence of frictions.

## R

``` r
beta_longshort <- beta_portfolios |>
  pivot_wider(id_cols = date, names_from = portfolio, values_from = ret) |>
  mutate(long_short = high - low)
```

## Python

``` python
beta_longshort = (beta_portfolios
    .pivot(values="ret", index="date", on="portfolio")
    .sort("date")
    .with_columns(long_short=pl.col("high")-pl.col("low"))
)
```

We compute the average return and the corresponding standard error to test whether the long-short portfolio yields on average positive or negative excess returns. In the asset pricing literature, one typically adjusts for autocorrelation by using Newey and West ([1987](#ref-Newey1987)) \\t\\-statistics to test the null hypothesis that average portfolio excess returns are equal to zero. One necessary input for Newey-West standard errors is a chosen bandwidth based on the number of lags employed for the estimation.

## R

Researchers often default to a pre-specified lag length of six months. A data-driven alternative advocated by Newey and West ([1994](#ref-Newey1994)) is available in the `sandwich` package via `NeweyWest()`, but we use the common six-lag setting here to keep the R and Python results comparable. To implement this test, we compute the average return via `lm()` and then employ the `coeftest()` function, passing the arguments `lag = 6, prewhite = FALSE` on to `NeweyWest()`.

``` r
model_fit <- lm(long_short ~ 1, data = beta_longshort)
coeftest(model_fit, vcov = NeweyWest, lag = 6, prewhite = FALSE)
```


    t test of coefficients:

                Estimate Std. Error t value Pr(>|t|)
    (Intercept) 0.000395   0.001250    0.32     0.75

## Python

Researchers often default to choosing a pre-specified lag length of six months (which is not a data-driven approach). We do so by setting `vcov` to `"NW"` (Newey-West) in `pf.feols()` and providing the maximum lag length together with the time identifier through the `vcov_kwargs` dictionary.

``` python
model_fit = pf.feols(
    "long_short ~ 1",
    data=beta_longshort,
    vcov="NW", vcov_kwargs={"lag": 6, "time_id": "date"}
)
model_fit.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: long_short
    sample: None = all
    Inference:  NW
    Observations:  731

    | Coefficient   |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:--------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept     |      0.000 |        0.001 |     0.316 |      0.752 | -0.002 |   0.003 |
    ---
    RMSE: 0.032 R2: 0.0 

The results indicate that we cannot reject the null hypothesis of average returns being equal to zero. Our portfolio strategy using the median as a breakpoint hence does not yield any abnormal returns. Is this finding surprising if you reconsider the CAPM? It certainly is. The CAPM yields that the high-beta stocks should yield higher expected returns. Our portfolio sort implicitly mimics an investment strategy that finances high-beta stocks by shorting low-beta stocks. Therefore, one should expect that the average excess returns yield a return that is above the risk-free rate.

## Functional Programming for Portfolio Sorts

Now, we take portfolio sorts to the next level. We want to be able to sort stocks into an arbitrary number of portfolios.

In some applications, the variable used for the sorting might be clustered (e.g., at a lower bound of 0). Then, multiple breakpoints may be identical, leading to empty portfolios. Similarly, some portfolios might have a very small number of stocks at the beginning of the sample. Cases where the number of portfolio constituents differs substantially due to the distribution of the characteristics require careful consideration and, depending on the application, might require customized sorting approaches.

## R

For this case, functional programming is very handy: we employ the [curly-curly](https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/#a-simpler-interpolation-pattern-with-)-operator to give us flexibility concerning which variable to use for the sorting, denoted by `sorting_variable`. We use `quantile()` to compute breakpoints for `n_portfolios`. Then, we assign portfolios to stocks using the `findInterval()` function. The output of the following function is a new column that contains the number of the portfolio to which a stock belongs.

``` r
assign_portfolio <- function(
  data,
  sorting_variable,
  n_portfolios
) {
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
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
```

## Python

For this case, functional programming is very handy: we define a function that gives us flexibility concerning which variable to use for the sorting, denoted by `sorting_variable`. We use `qcut()` to compute breakpoints for `n_portfolios` and to assign each stock to the corresponding portfolio bin in a single step. The output of the following function is an expression that yields a new column containing the number of the portfolio to which a stock belongs.

``` python
def assign_portfolio(sorting_variable, n_portfolios):
    """Assign portfolios to a bin between breakpoints."""

    assigned_portfolios = (pl.col(sorting_variable)
        .qcut(
            n_portfolios,
            labels=[str(i) for i in range(1, n_portfolios + 1)],
            allow_duplicates=True
        )
    )

    return assigned_portfolios
```

We can use the above function to sort stocks into ten portfolios each month using lagged betas and compute value-weighted returns for each portfolio. Note that we transform the portfolio column to an ordered categorical variable because it provides more convenience for the figure construction below.

## R

``` r
beta_portfolios <- data_for_sorts |>
  group_by(date) |>
  mutate(
    portfolio = assign_portfolio(
      data = pick(everything()),
      sorting_variable = beta_lag,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) |>
  group_by(portfolio, date) |>
  summarize(
    ret_excess = weighted.mean(ret_excess, mktcap_lag),
    .groups = "drop"
  ) |>
  left_join(factors_ff3_monthly, join_by(date))
```

## Python

``` python
portfolio_labels = pl.Enum([str(i) for i in range(1, 11)])

beta_portfolios = (data_for_sorts
  .with_columns(
      portfolio=assign_portfolio("beta_lag", 10).over("date")
  )
  .group_by(["portfolio", "date"])
  .agg(
      ret=(pl.col("ret_excess") * pl.col("mktcap_lag")).sum()
          / pl.col("mktcap_lag").sum()
  )
  .with_columns(portfolio=pl.col("portfolio").cast(portfolio_labels))
  .join(factors_ff3_monthly, how="left", on="date")
)
```

## More Performance Evaluation

In the next step, we compute summary statistics for each beta portfolio. Namely, we compute CAPM-adjusted alphas, the beta of each beta portfolio, and average returns.

## R

``` r
beta_portfolios_summary <- beta_portfolios |>
  nest(data = c(date, ret_excess, mkt_excess)) |>
  mutate(
    estimates = map(
      data,
      \(x) tidy(lm(ret_excess ~ 1 + mkt_excess, data = x))
    )
  ) |>
  unnest(estimates) |>
  select(portfolio, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  rename(alpha = `(Intercept)`, beta = mkt_excess) |>
  left_join(
    beta_portfolios |>
      group_by(portfolio) |>
      summarize(ret_excess = mean(ret_excess), .groups = "drop"),
    join_by(portfolio)
  )
```

## Python

``` python
beta_portfolios_summary = []
for portfolio, portfolio_data in beta_portfolios.group_by("portfolio"):
    model_fit = pf.feols(
        "ret ~ 1 + mkt_excess",
        data=portfolio_data
    )
    beta_portfolios_summary.append({
        "portfolio": portfolio[0],
        "alpha": model_fit.coef().iloc[0],
        "beta": model_fit.coef().iloc[1],
        "ret": portfolio_data["ret"].mean()
    })

beta_portfolios_summary = (pl.DataFrame(beta_portfolios_summary)
    .with_columns(portfolio=pl.col("portfolio").cast(portfolio_labels))
    .sort("portfolio")
)
```

[Figure 1](#fig-701) illustrates the CAPM alphas of beta-sorted portfolios. It shows that low-beta portfolios tend to exhibit positive alphas, while high-beta portfolios exhibit negative alphas.

## R

``` r
beta_portfolios_summary |>
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "CAPM alphas of beta-sorted portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "None")
```

[![Title: CAPM alphas of beta-sorted portfolios. The figure shows bar charts of alphas of beta-sorted portfolios with the decile portfolio on the horizontal axis and the corresponding CAPM alpha on the vertical axis. Alphas for low-beta portfolios are positive, while high-beta portfolios show negative alphas.](univariate-portfolio-sorts_files/figure-html/fig-701-1.png)](univariate-portfolio-sorts_files/figure-html/fig-701-1.png "Figure 1: The figure shows CAPM alphas of beta-sorted portfolios. Portfolios are sorted into deciles each month based on their estimated CAPM beta. The bar charts indicate the CAPM alpha of the resulting portfolio returns during the entire CRSP period.")

Figure 1: The figure shows CAPM alphas of beta-sorted portfolios. Portfolios are sorted into deciles each month based on their estimated CAPM beta. The bar charts indicate the CAPM alpha of the resulting portfolio returns during the entire CRSP period.

## Python

``` python
beta_portfolios_figure = (
  ggplot(
    beta_portfolios_summary,
    aes(x="portfolio", y="alpha", fill="portfolio")
  )
  + geom_bar(stat="identity")
  + labs(
      x="Portfolio", y="CAPM alpha", fill="Portfolio",
      title="CAPM alphas of beta-sorted portfolios"
    )
  + scale_y_continuous(labels=percent_format())
  + theme(legend_position="none")
)
beta_portfolios_figure.show()
```

[![Title: CAPM alphas of beta-sorted portfolios. The figure shows bar charts of alphas of beta-sorted portfolios with the decile portfolio on the horizontal axis and the corresponding CAPM alpha on the vertical axis. Alphas for low-beta portfolios are positive, while high-beta portfolios show negative alphas.](univariate-portfolio-sorts_files/figure-html/univariate-portfolio-sorts-fig-701-py-1.png)](univariate-portfolio-sorts_files/figure-html/univariate-portfolio-sorts-fig-701-py-1.png "The figure shows CAPM alphas of beta-sorted portfolios. Portfolios are sorted into deciles each month based on their estimated CAPM beta. The bar charts indicate the CAPM alpha of the resulting portfolio returns during the entire CRSP period.")

The figure shows CAPM alphas of beta-sorted portfolios. Portfolios are sorted into deciles each month based on their estimated CAPM beta. The bar charts indicate the CAPM alpha of the resulting portfolio returns during the entire CRSP period.

These results suggest a negative relation between beta and future stock returns, which contradicts the predictions of the CAPM. According to the CAPM, returns should increase with beta across the portfolios and risk-adjusted returns should be statistically indistinguishable from zero.

## The Security Market Line and Beta Portfolios

The CAPM predicts that our portfolios should lie on the security market line (SML). The slope of the SML is equal to the market risk premium and reflects the risk-return trade-off at any given time. [Figure 2](#fig-702) illustrates the security market line: We see that (not surprisingly) the high-beta portfolio returns have a high correlation with the market returns. However, it seems like the average excess returns for high-beta stocks are lower than what the security market line implies would be an “appropriate” compensation for the high market risk.

## R

``` r
sml_capm <- lm(
  ret_excess ~ 1 + beta,
  data = beta_portfolios_summary
)$coefficients

beta_portfolios_summary |>
  ggplot(aes(
    x = beta,
    y = ret_excess,
    color = portfolio
  )) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = mean(factors_ff3_monthly$mkt_excess),
    linetype = "solid"
  ) +
  geom_abline(
    intercept = sml_capm[1],
    slope = sml_capm[2],
    linetype = "dashed"
  ) +
  scale_y_continuous(
    labels = percent,
    limit = c(0, mean(factors_ff3_monthly$mkt_excess) * 2)
  ) +
  scale_x_continuous(limits = c(0, 2)) +
  labs(
    x = "Beta",
    y = "Excess return",
    color = "Portfolio",
    title = "Average portfolio excess returns and beta estimates"
  )
```

[![Title: Average portfolio excess returns and beta estimates. The figure shows a scatter plot of the average excess returns per beta portfolio with average beta estimates per portfolio on the horizontal axis and average excess returns on the vertical axis. An increasing solid line indicates the security market line. A dashed increasing line with lower slope than the security market line indicates that the CAPM prediction is not valid for CRSP data.](univariate-portfolio-sorts_files/figure-html/fig-702-3.png)](univariate-portfolio-sorts_files/figure-html/fig-702-3.png "Figure 2: The figure shows average portfolio excess returns and beta estimates. Excess returns are computed as CAPM alphas of the beta-sorted portfolios. The horizontal axis indicates the CAPM beta of the resulting beta-sorted portfolio return time series. The dashed line indicates the slope coefficient of a linear regression of excess returns on portfolio betas.")

Figure 2: The figure shows average portfolio excess returns and beta estimates. Excess returns are computed as CAPM alphas of the beta-sorted portfolios. The horizontal axis indicates the CAPM beta of the resulting beta-sorted portfolio return time series. The dashed line indicates the slope coefficient of a linear regression of excess returns on portfolio betas.

## Python

``` python
sml_capm = pf.feols(
    "ret ~ 1 + beta",
    data=beta_portfolios_summary
).coef()

sml_figure = (
  ggplot(
    beta_portfolios_summary,
    aes(x="beta", y="ret", color="portfolio")
  )
  + geom_point()
  + geom_abline(
      intercept=0, slope=factors_ff3_monthly["mkt_excess"].mean(), linetype="solid"
    )
  + geom_abline(
      intercept=sml_capm["Intercept"], slope=sml_capm["beta"], linetype="dashed"
    )
  + labs(
      x="Beta", y="Excess return", color="Portfolio",
      title="Average portfolio excess returns and beta estimates"
    )
  + scale_x_continuous(limits=(0, 2))
  + scale_y_continuous(
      labels=percent_format(),
      limits=(0, factors_ff3_monthly["mkt_excess"].mean()*2)
    )
)
sml_figure.show()
```

[![Title: Average portfolio excess returns and beta estimates. The figure shows a scatter plot of the average excess returns per beta portfolio with average beta estimates per portfolio on the horizontal axis and average excess returns on the vertical axis. An increasing solid line indicates the security market line. A dashed increasing line with lower slope than the security market line indicates that the CAPM prediction is not valid for CRSP data.](univariate-portfolio-sorts_files/figure-html/univariate-portfolio-sorts-fig-702-py-1.png)](univariate-portfolio-sorts_files/figure-html/univariate-portfolio-sorts-fig-702-py-1.png "The figure shows average portfolio excess returns and beta estimates. Excess returns are computed as CAPM alphas of the beta-sorted portfolios. The horizontal axis indicates the CAPM beta of the resulting beta-sorted portfolio return time series. The dashed line indicates the slope coefficient of a linear regression of excess returns on portfolio betas.")

The figure shows average portfolio excess returns and beta estimates. Excess returns are computed as CAPM alphas of the beta-sorted portfolios. The horizontal axis indicates the CAPM beta of the resulting beta-sorted portfolio return time series. The dashed line indicates the slope coefficient of a linear regression of excess returns on portfolio betas.

To provide more evidence against the CAPM predictions, we again form a long-short strategy that buys the high-beta portfolio and shorts the low-beta portfolio.

## R

``` r
beta_longshort <- beta_portfolios |>
  mutate(
    portfolio = case_when(
      portfolio == max(as.numeric(portfolio)) ~ "high",
      portfolio == min(as.numeric(portfolio)) ~ "low"
    )
  ) |>
  filter(portfolio %in% c("low", "high")) |>
  pivot_wider(
    id_cols = date,
    names_from = portfolio,
    values_from = ret_excess
  ) |>
  mutate(long_short = high - low) |>
  left_join(factors_ff3_monthly, join_by(date))
```

## Python

``` python
beta_longshort = (beta_portfolios
  .with_columns(
    portfolio=pl.when(pl.col("portfolio") == pl.col("portfolio").max())
        .then(pl.lit("high"))
        .when(pl.col("portfolio") == pl.col("portfolio").min())
        .then(pl.lit("low"))
        .otherwise(pl.col("portfolio").cast(pl.Utf8))
  )
  .filter(pl.col("portfolio").is_in(["low", "high"]))
  .pivot(values="ret", index="date", on="portfolio")
  .sort("date")
  .with_columns(long_short=pl.col("high")-pl.col("low"))
  .join(factors_ff3_monthly, how="left", on="date")
)
```

Again, the resulting long-short strategy does not exhibit statistically significant returns.

## R

``` r
coeftest(
  lm(long_short ~ 1, data = beta_longshort),
  vcov = NeweyWest, lag = 6, prewhite = FALSE
)
```


    t test of coefficients:

                Estimate Std. Error t value Pr(>|t|)
    (Intercept)  0.00251    0.00313     0.8     0.42

## Python

``` python
model_fit = pf.feols(
    "long_short ~ 1",
    data=beta_longshort,
    vcov="NW", vcov_kwargs={"lag": 6, "time_id": "date"}
)
model_fit.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: long_short
    sample: None = all
    Inference:  NW
    Observations:  731

    | Coefficient   |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:--------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept     |      0.003 |        0.003 |     0.798 |      0.425 | -0.004 |   0.009 |
    ---
    RMSE: 0.08 R2: 0.0 

However, controlling for the effect of beta, the long-short portfolio yields a statistically significant negative CAPM-adjusted alpha, although the average excess stock returns should be zero according to the CAPM. The results thus provide no evidence in support of the CAPM. The negative value has been documented as the so-called betting-against-beta factor ([Frazzini and Pedersen 2014](#ref-Frazzini2014)). Betting-against-beta corresponds to a strategy that shorts high-beta stocks and takes a (levered) long position in low-beta stocks. If borrowing constraints prevent investors from taking positions on the security market line they are instead incentivized to buy high-beta stocks, which leads to a relatively higher price (and therefore lower expected returns than implied by the CAPM) for such high-beta stocks. As a result, the betting-against-beta strategy earns from providing liquidity to capital-constrained investors with lower risk aversion.

## R

``` r
coeftest(
  lm(long_short ~ 1 + mkt_excess, data = beta_longshort),
  vcov = NeweyWest, lag = 6, prewhite = FALSE
)
```


    t test of coefficients:

                Estimate Std. Error t value Pr(>|t|)    
    (Intercept) -0.00424    0.00244   -1.74    0.083 .  
    mkt_excess   1.16353    0.08323   13.98   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Python

``` python
model_fit = pf.feols(
    "long_short ~ 1 + mkt_excess",
    data=beta_longshort,
    vcov="NW", vcov_kwargs={"lag": 6, "time_id": "date"}
)
model_fit.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: long_short
    sample: None = all
    Inference:  NW
    Observations:  731

    | Coefficient   |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:--------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept     |     -0.004 |        0.002 |    -1.739 |      0.082 | -0.009 |   0.001 |
    | mkt_excess    |      1.163 |        0.083 |    13.960 |      0.000 |  1.000 |   1.327 |
    ---
    RMSE: 0.061 R2: 0.427 

[Figure 3](#fig-703) shows the annual returns of the extreme beta portfolios we are mainly interested in. The figure illustrates no consistent striking patterns over the last years; each portfolio exhibits periods with positive and negative annual returns.

## R

``` r
beta_longshort |>
  group_by(year = year(date)) |>
  summarize(
    low = prod(1 + low) - 1,
    high = prod(1 + high) - 1,
    long_short = prod(1 + long_short) - 1
  ) |>
  pivot_longer(cols = -year) |>
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_col(position = "dodge") +
  facet_wrap(~name, ncol = 1) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Annual returns of beta portfolios",
    x = NULL,
    y = NULL
  )
```

[![Title: Annual returns of beta portfolios. The figure shows bar charts of annual returns of long, short, and long-short beta portfolios with years on the horizontal axis and returns on the vertical axis. Each portfolio is plotted in its own facet. The long-short portfolio strategy delivers very high losses during some periods.](univariate-portfolio-sorts_files/figure-html/fig-703-1.png)](univariate-portfolio-sorts_files/figure-html/fig-703-1.png "Figure 3: The figure shows annual returns of beta portfolios. We construct portfolios by sorting stocks into high and low based on their estimated CAPM beta. Long short indicates a strategy that goes long into high beta stocks and short low beta stocks.")

Figure 3: The figure shows annual returns of beta portfolios. We construct portfolios by sorting stocks into high and low based on their estimated CAPM beta. Long short indicates a strategy that goes long into high beta stocks and short low beta stocks.

## Python

``` python
beta_longshort_year = (beta_longshort
  .with_columns(year=pl.col("date").dt.year())
  .group_by("year")
  .agg(
    low=(pl.col("low") + 1).product() - 1,
    high=(pl.col("high") + 1).product() - 1,
    long_short=(pl.col("long_short") + 1).product() - 1
  )
  .sort("year")
  .unpivot(index="year", on=["low", "high", "long_short"],
           variable_name="name", value_name="value")
)

beta_longshort_figure = (
  ggplot(
    beta_longshort_year,
    aes(x="year", y="value", fill="name")
  )
  + geom_col(position="dodge")
  + facet_wrap("~name", ncol=1)
  + labs(x="", y="", title="Annual returns of beta portfolios")
  + scale_y_continuous(labels=percent_format())
  + theme(legend_position="none")
)
beta_longshort_figure.show()
```

[![Title: Annual returns of beta portfolios. The figure shows bar charts of annual returns of long, short, and long-short beta portfolios with years on the horizontal axis and returns on the vertical axis. Each portfolio is plotted in its own facet. The long-short portfolio strategy delivers very high losses during some periods.](univariate-portfolio-sorts_files/figure-html/univariate-portfolio-sorts-fig-703-py-1.png)](univariate-portfolio-sorts_files/figure-html/univariate-portfolio-sorts-fig-703-py-1.png "The figure shows annual returns of beta portfolios. We construct portfolios by sorting stocks into high and low based on their estimated CAPM beta. Long short indicates a strategy that goes long into high beta stocks and short low beta stocks.")

The figure shows annual returns of beta portfolios. We construct portfolios by sorting stocks into high and low based on their estimated CAPM beta. Long short indicates a strategy that goes long into high beta stocks and short low beta stocks.

Overall, this chapter shows how functional programming can be leveraged to form an arbitrary number of portfolios using any sorting variable and how to evaluate the performance of the resulting portfolios. In the next chapter, we dive deeper into the many degrees of freedom that arise in the context of portfolio analysis.

## Key Takeaways

- Univariate portfolio sorts assess whether a single firm characteristic, like lagged market beta, can predict future excess returns.
- Portfolios are formed each month using quantile breakpoints, with returns computed using value-weighted averages to reflect realistic investment strategies.
- A long-short strategy based on beta-sorted portfolios fails to generate significant positive excess returns, contradicting CAPM predictions that higher beta should yield higher returns.
- The analysis highlights the “betting against beta” anomaly, where low-beta portfolios deliver higher alphas than high-beta portfolios, providing evidence against the CAPM.
- The functional programming capabilities of R and Python enable scalable and flexible portfolio sorting, making it easy to analyze multiple characteristics and portfolio configurations.

## Exercises

1.  Take the two long-short beta strategies based on different numbers of portfolios and compare the returns. Is there a significant difference in returns? How do the Sharpe ratios compare between the strategies? Find one additional portfolio evaluation statistic and compute it.
2.  We plotted the alphas of the ten beta portfolios above. Write a function that tests these estimates for significance. Which portfolios have significant alphas?
3.  The analysis here is based on betas from monthly returns. However, we also computed betas from daily returns. Re-run the analysis and point out differences in the results.
4.  Given the results in this chapter, can you define a long-short strategy that yields positive abnormal returns (i.e., alphas)? Plot the cumulative excess return of your strategy and the market excess return for comparison.

## References

Bali, Turan G, Robert F Engle, and Scott Murray. 2016. *Empirical asset pricing: The cross section of stock returns*. John Wiley & Sons. <https://doi.org/10.1002/9781118445112.stat07954>.

Frazzini, Andrea, and Lasse Heje Pedersen. 2014. “Betting against beta.” *Journal of Financial Economics* 111 (1): 1–25. <https://doi.org/10.1016/j.jfineco.2013.10.005>.

Newey, Whitney .K, and Kenneth D. West. 1994. “Automatic lag selection in covariance matrix estimation.” *The Review of Economic Studies* 61 (4): 631–53. <https://www.jstor.org/stable/2297912>.

Newey, Whitney K., and Kenneth D. West. 1987. “A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance Matrix.” *Econometrica* 55 (3): 703–8. <http://www.jstor.org/stable/1913610>.

Robinson, David, Alex Hayes, and Simon Couch. 2022. *broom: Convert statistical objects into tidy tibbles*. <https://CRAN.R-project.org/package=broom>.

Zeileis, Achim. 2006. “Object-Oriented computation of sandwich estimators.” *Journal of Statistical Software* 16 (9): 1–16. <http://dx.doi.org/10.18637/jss.v016.i09>.

Zeileis, Achim, and Torsten Hothorn. 2002. “Diagnostic checking in regression relationships.” *R News* 2 (3): 7–10. <https://CRAN.R-project.org/doc/Rnews/>.
