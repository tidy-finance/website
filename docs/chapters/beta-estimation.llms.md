# Beta Estimation

In this chapter, we introduce an important concept in financial economics: the exposure of an individual stock to changes in the market portfolio. According to the Capital Asset Pricing Model (CAPM) of Sharpe ([1964](#ref-Sharpe1964)), Lintner ([1965](#ref-Lintner1965)), and Mossin ([1966](#ref-Mossin1966)), cross-sectional variation in expected asset returns should be a function of the covariance between the excess return of the asset and the excess return on the market portfolio. The regression coefficient of excess market returns on excess stock returns is usually called the market beta. We show an estimation procedure for the market betas. We do not go into details about the foundations of market beta but simply refer to any treatment of the [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model) for further information. Instead, we provide details about all the functions that we use to compute the results. In particular, we leverage a useful computational concept: vectorized rolling-window estimation that computes the rolling betas of all stocks in closed form.

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
library(nanoparquet)
library(slider)

theme_set(theme_minimal())
```

Compared to previous chapters, we introduce `slider` ([Vaughan 2021](#ref-slider)) for the rolling sums that we use in the closed-form beta estimation.

## Python

``` python
import polars as pl
import numpy as np
import pyfixest as pf
import os

from plotnine import *
from mizani.formatters import percent_format

theme_set(theme_minimal())
```

Compared to previous chapters, we introduce `pyfixest` ([Fischer 2024](#ref-pyfixest)) for regression analysis.

## Estimating Beta Using Monthly Returns

The estimation procedure is based on a rolling-window estimation, where we may use either monthly or daily returns and different window lengths. First, let us start with loading the monthly CRSP data from our Parquet files introduced in [Accessing and Managing Financial Data](../chapters/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md).

## R

``` r
crsp_monthly <- read_parquet("data/crsp_monthly.parquet") |>
  select(permno, date, industry, ret_excess)

factors_ff3_monthly <- read_parquet("data/factors_ff3_monthly.parquet") |>
  select(date, mkt_excess)

crsp_monthly <- crsp_monthly |>
  left_join(factors_ff3_monthly, join_by(date))
```

## Python

``` python
crsp_monthly = (pl.read_parquet("data/crsp_monthly.parquet")
    .select(["permno", "date", "industry", "ret_excess"])
)

factors_ff3_monthly = (pl.read_parquet("data/factors_ff3_monthly.parquet")
    .select(["date", "mkt_excess"])
)

crsp_monthly = (crsp_monthly
    .join(factors_ff3_monthly, how="left", on="date")
)
```

To estimate the CAPM regression coefficients

\\ r\_{i, t} - r\_{f, t} = \alpha_i + \beta_i(r\_{m, t}-r\_{f,t})+\varepsilon\_{i, t} \tag{1}\\

we regress stock excess returns `ret_excess` on excess returns of the market portfolio `mkt_excess`.

## R

R provides a simple solution to estimate (linear) models with the function `lm()`. `lm()` requires a formula as input that is specified in a compact symbolic form. An expression of the form `y ~ model` is interpreted as a specification that the response `y` is modeled by a linear predictor specified symbolically by `model`. Such a model consists of a series of terms separated by `+` operators. In addition to standard linear models, `lm()` provides a lot of flexibility. You should check out the documentation for more information. To start, we restrict the data only to the time series of observations in CRSP that correspond to Apple’s stock (i.e., to `permno` 14593 for Apple) and compute \\\hat\alpha_i\\ as well as \\\hat\beta_i\\.

``` r
model_fit <- lm(
  "ret_excess ~ mkt_excess",
  data = crsp_monthly |>
    filter(permno == "14593")
)
coefficients <- summary(model_fit)$coefficients
coefficients
```

                Estimate Std. Error t value Pr(>|t|)
    (Intercept)  0.00992    0.00488    2.03 4.27e-02
    mkt_excess   1.37560    0.10761   12.78 8.89e-33

`lm()` returns an object of class `lm` which contains all information we usually care about with linear models. `summary()` returns information about the estimated parameters. The output above indicates that Apple moves excessively with the market as the estimated \\\hat\beta_i\\ is above one (\\\hat\beta_i \approx 1.4\\).

## Python

Python provides a simple solution to estimate (linear) models with the function `pf.feols()`. The function requires a formula as input that is specified in a compact symbolic form. An expression of the form `y ~ model` is interpreted as a specification that the response `y` is modeled by a linear predictor specified symbolically by `model`. Such a model consists of a series of terms separated by `+` operators. In addition to standard linear models, `pf.feols()` provides a lot of flexibility. You should check out the documentation for more information. To start, we restrict the data only to the time series of observations in CRSP that correspond to Apple’s stock (i.e., to `permno` 14593 for Apple) and compute \\\hat\alpha_i\\ as well as \\\hat\beta_i\\.

``` python
model_fit = pf.feols(
    "ret_excess ~ mkt_excess",
    data=crsp_monthly.filter(pl.col("permno") == 14593)
)
coefficients = model_fit.tidy()
coefficients
```

                 Estimate  Std. Error    t value  Pr(>|t|)      2.5%     97.5%
    Coefficient                                                               
    Intercept    0.009919    0.004884   2.031102  0.042747  0.000325  0.019513
    mkt_excess   1.375598    0.107606  12.783617  0.000000  1.164207  1.586989

`pf.feols()` returns an object of class `Feols`, which contains all the information we usually care about with linear models. The `tidy()` method returns information about the estimated parameters. The output above indicates that Apple moves excessively with the market as the estimated \\\hat\beta_i\\ is above one (\\\hat\beta_i \approx 1.4\\).

## Rolling-Window Estimation

After we estimated the regression coefficients on an example, we scale the estimation of \\\beta_i\\ to the entire CRSP sample and perform rolling-window estimations for every stock. Fitting millions of individual regressions—one for each stock and month—would be prohibitively slow. Fortunately, for a single-regressor model such as the CAPM, the rolling OLS coefficients have a closed-form solution that we can compute from rolling sums. Writing \\x\\ for the market excess return and \\y\\ for the stock excess return, the slope and intercept over a window read

\\\hat\beta_i = \frac{S\_{xy}}{S\_{xx}}, \qquad \hat\alpha_i = \bar y - \hat\beta_i \bar x, \tag{2}\\

where \\S\_{xy} = \sum_t x_t y_t - \frac{1}{n}\sum_t x_t \sum_t y_t\\ and \\S\_{xx} = \sum_t x_t^2 - \frac{1}{n}\left(\sum_t x_t\right)^2\\. Because these quantities only require the rolling sums \\\sum_t x_t\\, \\\sum_t y_t\\, \\\sum_t x_t^2\\, \\\sum_t y_t^2\\, and \\\sum_t x_t y_t\\, we can compute the rolling betas—together with their \\t\\-statistics—for all stocks at once using vectorized rolling aggregations, without fitting a single regression. We refer to our blog post [Fast, Vectorized Beta Estimation](https://blog.tidy-finance.org/posts/fast-beta-estimation/) for a detailed derivation.

The following function implements this approach in three steps: (i) it computes the per-period cumulants for each stock, (ii) it rolls the sums over the requested `look_back` window, and (iii) it maps the aggregated sums into the closed-form estimates. We only keep windows with at least `min_obs` observations to avoid huge fluctuations if the time series is too short. As we demonstrate further below, we can apply the same function to daily returns data.

## R

The `slide_index_sum()` function from the `slider` package computes the rolling sums over a monthly index in a straightforward manner.

``` r
roll_capm_estimation <- function(data, look_back = 60, min_obs = 48) {
  cumulants <- data |>
    summarize(
      n = n(),
      sum_x = sum(mkt_excess),
      sum_y = sum(ret_excess),
      sum_xx = sum(mkt_excess^2),
      sum_yy = sum(ret_excess^2),
      sum_xy = sum(mkt_excess * ret_excess),
      .by = c(permno, date)
    ) |>
    arrange(permno, date) |>
    mutate(
      period = year(date) * 12 + month(date),
      .by = permno
    )

  rolling_sums <- cumulants |>
    mutate(
      across(
        c(n, sum_x, sum_y, sum_xx, sum_yy, sum_xy),
        \(col) slide_index_sum(col, period, before = look_back - 1)
      ),
      .by = permno
    ) |>
    filter(n >= min_obs)

  estimates <- rolling_sums |>
    mutate(
      s_xx = sum_xx - sum_x^2 / n,
      s_xy = sum_xy - sum_x * sum_y / n,
      s_yy = sum_yy - sum_y^2 / n,
      beta = s_xy / s_xx,
      alpha = (sum_y - beta * sum_x) / n,
      sigma2 = (s_yy - beta * s_xy) / (n - 2),
      t_alpha = alpha / sqrt(sigma2 * (1 / n + (sum_x / n)^2 / s_xx)),
      t_beta = beta / sqrt(sigma2 / s_xx)
    )

  bind_rows(
    estimates |>
      transmute(permno, date, coefficient = "alpha",
                estimate = alpha, t_statistic = t_alpha),
    estimates |>
      transmute(permno, date, coefficient = "mkt_excess",
                estimate = beta, t_statistic = t_beta)
  ) |>
    arrange(permno, date, coefficient)
}
```

## Python

The `rolling()` method from `polars` computes the rolling sums over the monthly date index in a straightforward manner.

``` python
def roll_capm_estimation(data, look_back=60, min_obs=48):
    cumulants = (data
        .group_by("permno", "date")
        .agg(
            n=pl.len().cast(pl.Float64),
            sum_x=pl.col("mkt_excess").sum(),
            sum_y=pl.col("ret_excess").sum(),
            sum_xx=(pl.col("mkt_excess") ** 2).sum(),
            sum_yy=(pl.col("ret_excess") ** 2).sum(),
            sum_xy=(pl.col("mkt_excess") * pl.col("ret_excess")).sum(),
        )
        .sort("permno", "date")
    )

    rolling_sums = (cumulants
        .rolling(
            index_column="date",
            period=f"{look_back}mo",
            group_by="permno",
            closed="right",
        )
        .agg(
            pl.col("n", "sum_x", "sum_y", "sum_xx", "sum_yy", "sum_xy").sum(),
        )
        .filter(pl.col("n") >= min_obs)
    )

    estimates = (rolling_sums
        .with_columns(
            s_xx=pl.col("sum_xx") - pl.col("sum_x") ** 2 / pl.col("n"),
            s_xy=pl.col("sum_xy") - pl.col("sum_x") * pl.col("sum_y") / pl.col("n"),
            s_yy=pl.col("sum_yy") - pl.col("sum_y") ** 2 / pl.col("n"),
        )
        .with_columns(beta=pl.col("s_xy") / pl.col("s_xx"))
        .with_columns(
            alpha=(pl.col("sum_y") - pl.col("beta") * pl.col("sum_x")) / pl.col("n"),
            sigma2=(pl.col("s_yy") - pl.col("beta") * pl.col("s_xy"))
                / (pl.col("n") - 2),
        )
        .with_columns(
            t_alpha=pl.col("alpha") / (
                pl.col("sigma2")
                * (1 / pl.col("n")
                    + (pl.col("sum_x") / pl.col("n")) ** 2 / pl.col("s_xx"))
            ).sqrt(),
            t_beta=pl.col("beta") / (pl.col("sigma2") / pl.col("s_xx")).sqrt(),
        )
    )

    return (pl.concat([
            estimates.select(
                "permno", "date",
                coefficient=pl.lit("alpha"),
                estimate=pl.col("alpha"),
                t_statistic=pl.col("t_alpha"),
            ),
            estimates.select(
                "permno", "date",
                coefficient=pl.lit("mkt_excess"),
                estimate=pl.col("beta"),
                t_statistic=pl.col("t_beta"),
            ),
        ])
        .sort("permno", "date", "coefficient")
    )
```

Before we attack the whole CRSP sample, let us focus on a couple of examples for well-known firms.

## R

``` r
examples <- tibble(
  permno = c(14593, 10107, 93436, 17778),
  company = c("Apple", "Microsoft", "Tesla", "Berkshire Hathaway")
)
```

## Python

``` python
examples = pl.DataFrame({
    "permno": [14593, 10107, 93436, 17778],
    "company": ["Apple", "Microsoft", "Tesla", "Berkshire Hathaway"]
}).with_columns(permno=pl.col("permno").cast(pl.Float64))
```

Because `roll_capm_estimation()` groups by `permno` internally, we do not have to loop over stocks or nest the data. We simply restrict the sample to the example stocks and hand the entire frame to the function, which returns a tidy data frame with a time series of beta estimates for each stock.

## R

``` r
capm_examples <- crsp_monthly |>
  filter(permno %in% examples$permno) |>
  roll_capm_estimation()
capm_examples
```

         permno       date coefficient  estimate t_statistic
    1     10107 1990-03-01       alpha  4.17e-02    2.309559
    2     10107 1990-03-01  mkt_excess  1.40e+00    4.173565
    3     10107 1990-04-01       alpha  4.27e-02    2.413745
    4     10107 1990-04-01  mkt_excess  1.39e+00    4.197367
    5     10107 1990-05-01       alpha  4.43e-02    2.533768
    6     10107 1990-05-01  mkt_excess  1.43e+00    4.466921
    7     10107 1990-06-01       alpha  4.44e-02    2.594059
    8     10107 1990-06-01  mkt_excess  1.43e+00    4.515088
    9     10107 1990-07-01       alpha  4.14e-02    2.433937
    10    10107 1990-07-01  mkt_excess  1.46e+00    4.578970
    11    10107 1990-08-01       alpha  4.19e-02    2.518123
    12    10107 1990-08-01  mkt_excess  1.44e+00    4.740405
    13    10107 1990-09-01       alpha  4.31e-02    2.637750
    14    10107 1990-09-01  mkt_excess  1.42e+00    4.750310
    15    10107 1990-10-01       alpha  4.29e-02    2.675445
    16    10107 1990-10-01  mkt_excess  1.42e+00    4.806355
    17    10107 1990-11-01       alpha  4.29e-02    2.717272
    18    10107 1990-11-01  mkt_excess  1.42e+00    4.900897
    19    10107 1990-12-01       alpha  4.21e-02    2.715787
    20    10107 1990-12-01  mkt_excess  1.41e+00    4.928269
    21    10107 1991-01-01       alpha  4.52e-02    2.898734
    22    10107 1991-01-01  mkt_excess  1.46e+00    5.057461
    23    10107 1991-02-01       alpha  4.38e-02    2.832374
    24    10107 1991-02-01  mkt_excess  1.42e+00    5.008333
    25    10107 1991-03-01       alpha  4.28e-02    2.805863
    26    10107 1991-03-01  mkt_excess  1.41e+00    5.018872
    27    10107 1991-04-01       alpha  3.84e-02    2.534832
    28    10107 1991-04-01  mkt_excess  1.43e+00    5.115538
    29    10107 1991-05-01       alpha  3.91e-02    2.583179
    30    10107 1991-05-01  mkt_excess  1.44e+00    5.143781
    31    10107 1991-06-01       alpha  4.13e-02    2.781743
    32    10107 1991-06-01  mkt_excess  1.46e+00    5.354182
    33    10107 1991-07-01       alpha  4.13e-02    2.775166
    34    10107 1991-07-01  mkt_excess  1.45e+00    5.241417
    35    10107 1991-08-01       alpha  4.46e-02    3.025108
    36    10107 1991-08-01  mkt_excess  1.50e+00    5.432691
    37    10107 1991-09-01       alpha  4.35e-02    2.951291
    38    10107 1991-09-01  mkt_excess  1.54e+00    5.456167
    39    10107 1991-10-01       alpha  3.94e-02    2.806309
    40    10107 1991-10-01  mkt_excess  1.47e+00    5.446466
    41    10107 1991-11-01       alpha  3.68e-02    2.716487
    42    10107 1991-11-01  mkt_excess  1.45e+00    5.589755
    43    10107 1991-12-01       alpha  3.65e-02    2.683912
    44    10107 1991-12-01  mkt_excess  1.41e+00    5.584929
    45    10107 1992-01-01       alpha  3.36e-02    2.688737
    46    10107 1992-01-01  mkt_excess  1.18e+00    4.866248
    47    10107 1992-02-01       alpha  3.38e-02    2.714731
    48    10107 1992-02-01  mkt_excess  1.19e+00    4.886069
    49    10107 1992-03-01       alpha  2.97e-02    2.481118
    50    10107 1992-03-01  mkt_excess  1.18e+00    5.061915
    51    10107 1992-04-01       alpha  2.67e-02    2.206346
    52    10107 1992-04-01  mkt_excess  1.19e+00    5.034563
    53    10107 1992-05-01       alpha  2.64e-02    2.193060
    54    10107 1992-05-01  mkt_excess  1.19e+00    5.047254
    55    10107 1992-06-01       alpha  2.72e-02    2.304528
    56    10107 1992-06-01  mkt_excess  1.26e+00    5.432656
    57    10107 1992-07-01       alpha  2.91e-02    2.529876
    58    10107 1992-07-01  mkt_excess  1.28e+00    5.692550
    59    10107 1992-08-01       alpha  2.65e-02    2.398528
    60    10107 1992-08-01  mkt_excess  1.24e+00    5.713956
    61    10107 1992-09-01       alpha  2.51e-02    2.303853
    62    10107 1992-09-01  mkt_excess  1.26e+00    5.897708
    63    10107 1992-10-01       alpha  2.57e-02    2.316709
    64    10107 1992-10-01  mkt_excess  1.30e+00    4.810767
    65    10107 1992-11-01       alpha  2.60e-02    2.317441
    66    10107 1992-11-01  mkt_excess  1.26e+00    4.517255
    67    10107 1992-12-01       alpha  2.28e-02    2.026123
    68    10107 1992-12-01  mkt_excess  1.18e+00    4.131255
    69    10107 1993-01-01       alpha  2.31e-02    2.060201
    70    10107 1993-01-01  mkt_excess  1.20e+00    4.176746
    71    10107 1993-02-01       alpha  2.23e-02    1.986612
    72    10107 1993-02-01  mkt_excess  1.21e+00    4.167927
    73    10107 1993-03-01       alpha  2.41e-02    2.135718
    74    10107 1993-03-01  mkt_excess  1.21e+00    4.126564
    75    10107 1993-04-01       alpha  2.40e-02    2.136255
    76    10107 1993-04-01  mkt_excess  1.24e+00    4.258583
    77    10107 1993-05-01       alpha  2.36e-02    2.101765
    78    10107 1993-05-01  mkt_excess  1.25e+00    4.306662
    79    10107 1993-06-01       alpha  2.14e-02    1.904078
    80    10107 1993-06-01  mkt_excess  1.22e+00    4.160864
    81    10107 1993-07-01       alpha  2.05e-02    1.789241
    82    10107 1993-07-01  mkt_excess  1.21e+00    4.053303
    83    10107 1993-08-01       alpha  2.28e-02    2.020985
    84    10107 1993-08-01  mkt_excess  1.12e+00    3.821760
    85    10107 1993-09-01       alpha  2.43e-02    2.156611
    86    10107 1993-09-01  mkt_excess  1.12e+00    3.782711
    87    10107 1993-10-01       alpha  2.49e-02    2.223269
    88    10107 1993-10-01  mkt_excess  1.12e+00    3.808818
    89    10107 1993-11-01       alpha  2.56e-02    2.284090
    90    10107 1993-11-01  mkt_excess  1.11e+00    3.769225
    91    10107 1993-12-01       alpha  2.37e-02    2.129549
    92    10107 1993-12-01  mkt_excess  1.10e+00    3.755641
    93    10107 1994-01-01       alpha  2.34e-02    2.107488
    94    10107 1994-01-01  mkt_excess  1.08e+00    3.649405
    95    10107 1994-02-01       alpha  2.30e-02    2.068641
    96    10107 1994-02-01  mkt_excess  1.09e+00    3.680256
    97    10107 1994-03-01       alpha  2.74e-02    2.616902
    98    10107 1994-03-01  mkt_excess  1.08e+00    3.911413
    99    10107 1994-04-01       alpha  2.77e-02    2.650361
    100   10107 1994-04-01  mkt_excess  1.06e+00    3.809729
    101   10107 1994-05-01       alpha  2.96e-02    2.779406
    102   10107 1994-05-01  mkt_excess  1.06e+00    3.706706
    103   10107 1994-06-01       alpha  3.15e-02    3.035130
    104   10107 1994-06-01  mkt_excess  1.04e+00    3.774018
    105   10107 1994-07-01       alpha  3.15e-02    3.054854
    106   10107 1994-07-01  mkt_excess  1.09e+00    3.866049
    107   10107 1994-08-01       alpha  3.20e-02    3.084017
    108   10107 1994-08-01  mkt_excess  1.11e+00    3.954583
    109   10107 1994-09-01       alpha  2.88e-02    2.848936
    110   10107 1994-09-01  mkt_excess  1.15e+00    4.191191
    111   10107 1994-10-01       alpha  2.62e-02    2.721218
    112   10107 1994-10-01  mkt_excess  1.26e+00    4.806153
    113   10107 1994-11-01       alpha  2.62e-02    2.735752
    114   10107 1994-11-01  mkt_excess  1.25e+00    4.820257
    115   10107 1994-12-01       alpha  2.58e-02    2.686499
    116   10107 1994-12-01  mkt_excess  1.25e+00    4.802009
    117   10107 1995-01-01       alpha  2.16e-02    2.273787
    118   10107 1995-01-01  mkt_excess  1.38e+00    5.183204
    119   10107 1995-02-01       alpha  2.10e-02    2.207149
    120   10107 1995-02-01  mkt_excess  1.37e+00    5.189533
    121   10107 1995-03-01       alpha  2.10e-02    2.209208
    122   10107 1995-03-01  mkt_excess  1.37e+00    5.203633
    123   10107 1995-04-01       alpha  2.11e-02    2.193474
    124   10107 1995-04-01  mkt_excess  1.43e+00    5.318564
    125   10107 1995-05-01       alpha  1.96e-02    2.084538
    126   10107 1995-05-01  mkt_excess  1.29e+00    4.726890
    127   10107 1995-06-01       alpha  1.92e-02    2.035027
    128   10107 1995-06-01  mkt_excess  1.30e+00    4.773727
    129   10107 1995-07-01       alpha  2.07e-02    2.229003
    130   10107 1995-07-01  mkt_excess  1.22e+00    4.580018
    131   10107 1995-08-01       alpha  1.98e-02    2.089041
    132   10107 1995-08-01  mkt_excess  1.27e+00    4.308068
    133   10107 1995-09-01       alpha  1.63e-02    1.683818
    134   10107 1995-09-01  mkt_excess  1.33e+00    4.339735
    135   10107 1995-10-01       alpha  1.83e-02    1.853367
    136   10107 1995-10-01  mkt_excess  1.29e+00    4.113361
    137   10107 1995-11-01       alpha  1.60e-02    1.531202
    138   10107 1995-11-01  mkt_excess  1.14e+00    3.371233
    139   10107 1995-12-01       alpha  1.57e-02    1.503963
    140   10107 1995-12-01  mkt_excess  1.15e+00    3.369071
    141   10107 1996-01-01       alpha  1.38e-02    1.443190
    142   10107 1996-01-01  mkt_excess  9.75e-01    3.096826
    143   10107 1996-02-01       alpha  1.45e-02    1.525634
    144   10107 1996-02-01  mkt_excess  1.02e+00    3.114460
    145   10107 1996-03-01       alpha  1.51e-02    1.597438
    146   10107 1996-03-01  mkt_excess  1.03e+00    3.131103
    147   10107 1996-04-01       alpha  1.75e-02    1.861968
    148   10107 1996-04-01  mkt_excess  1.02e+00    3.130028
    149   10107 1996-05-01       alpha  1.70e-02    1.819763
    150   10107 1996-05-01  mkt_excess  9.93e-01    3.033913
    151   10107 1996-06-01       alpha  1.83e-02    1.942293
    152   10107 1996-06-01  mkt_excess  9.33e-01    2.761200
    153   10107 1996-07-01       alpha  1.85e-02    2.012872
    154   10107 1996-07-01  mkt_excess  9.00e-01    2.774139
    155   10107 1996-08-01       alpha  1.68e-02    1.872007
    156   10107 1996-08-01  mkt_excess  8.57e-01    2.715302
    157   10107 1996-09-01       alpha  1.61e-02    1.782667
    158   10107 1996-09-01  mkt_excess  8.87e-01    2.854770
    159   10107 1996-10-01       alpha  1.60e-02    1.768304
    160   10107 1996-10-01  mkt_excess  8.85e-01    2.849817
    161   10107 1996-11-01       alpha  1.47e-02    1.596979
    162   10107 1996-11-01  mkt_excess  1.02e+00    3.288323
    163   10107 1996-12-01       alpha  1.60e-02    1.748630
    164   10107 1996-12-01  mkt_excess  9.47e-01    2.719949
    165   10107 1997-01-01       alpha  1.57e-02    1.636511
    166   10107 1997-01-01  mkt_excess  1.15e+00    3.228253
    167   10107 1997-02-01       alpha  1.46e-02    1.518328
    168   10107 1997-02-01  mkt_excess  1.17e+00    3.278042
    169   10107 1997-03-01       alpha  1.47e-02    1.540465
    170   10107 1997-03-01  mkt_excess  1.18e+00    3.388374
    171   10107 1997-04-01       alpha  1.89e-02    1.803914
    172   10107 1997-04-01  mkt_excess  1.37e+00    3.670639
    173   10107 1997-05-01       alpha  1.72e-02    1.625384
    174   10107 1997-05-01  mkt_excess  1.26e+00    3.493467
    175   10107 1997-06-01       alpha  1.99e-02    1.889026
    176   10107 1997-06-01  mkt_excess  1.13e+00    3.182602
    177   10107 1997-07-01       alpha  2.02e-02    1.912889
    178   10107 1997-07-01  mkt_excess  1.16e+00    3.382191
    179   10107 1997-08-01       alpha  1.82e-02    1.734063
    180   10107 1997-08-01  mkt_excess  1.23e+00    3.628541
    181   10107 1997-09-01       alpha  1.69e-02    1.585008
    182   10107 1997-09-01  mkt_excess  1.16e+00    3.455925
    183   10107 1997-10-01       alpha  1.58e-02    1.516265
    184   10107 1997-10-01  mkt_excess  1.16e+00    3.558417
    185   10107 1997-11-01       alpha  1.64e-02    1.574419
    186   10107 1997-11-01  mkt_excess  1.18e+00    3.600457
    187   10107 1997-12-01       alpha  1.63e-02    1.563546
    188   10107 1997-12-01  mkt_excess  1.18e+00    3.609673
    189   10107 1998-01-01       alpha  1.91e-02    1.786155
    190   10107 1998-01-01  mkt_excess  1.16e+00    3.449152
    191   10107 1998-02-01       alpha  2.03e-02    1.893341
    192   10107 1998-02-01  mkt_excess  1.18e+00    3.632114
    193   10107 1998-03-01       alpha  1.93e-02    1.796286
    194   10107 1998-03-01  mkt_excess  1.15e+00    3.605432
    195   10107 1998-04-01       alpha  2.05e-02    1.903237
    196   10107 1998-04-01  mkt_excess  1.10e+00    3.419709
    197   10107 1998-05-01       alpha  1.88e-02    1.768710
    198   10107 1998-05-01  mkt_excess  1.13e+00    3.554757
    199   10107 1998-06-01       alpha  2.30e-02    2.026769
    200   10107 1998-06-01  mkt_excess  1.19e+00    3.540075
    201   10107 1998-07-01       alpha  2.70e-02    2.508539
    202   10107 1998-07-01  mkt_excess  1.13e+00    3.553537
    203   10107 1998-08-01       alpha  2.86e-02    2.794959
    204   10107 1998-08-01  mkt_excess  1.11e+00    4.251572
    205   10107 1998-09-01       alpha  2.78e-02    2.717311
    206   10107 1998-09-01  mkt_excess  1.14e+00    4.467631
    207   10107 1998-10-01       alpha  2.76e-02    2.625974
    208   10107 1998-10-01  mkt_excess  1.05e+00    4.097574
    209   10107 1998-11-01       alpha  2.85e-02    2.671273
    210   10107 1998-11-01  mkt_excess  1.08e+00    4.209733
    211   10107 1998-12-01       alpha  2.95e-02    2.758492
    212   10107 1998-12-01  mkt_excess  1.10e+00    4.343242
    213   10107 1999-01-01       alpha  3.23e-02    2.872391
    214   10107 1999-01-01  mkt_excess  1.14e+00    4.309606
    215   10107 1999-02-01       alpha  2.99e-02    2.618319
    216   10107 1999-02-01  mkt_excess  1.20e+00    4.474371
    217   10107 1999-03-01       alpha  3.01e-02    2.572202
    218   10107 1999-03-01  mkt_excess  1.25e+00    4.553456
    219   10107 1999-04-01       alpha  2.70e-02    2.220539
    220   10107 1999-04-01  mkt_excess  1.21e+00    4.249771
    221   10107 1999-05-01       alpha  2.46e-02    2.066783
    222   10107 1999-05-01  mkt_excess  1.22e+00    4.414101
    223   10107 1999-06-01       alpha  2.57e-02    2.133461
    224   10107 1999-06-01  mkt_excess  1.22e+00    4.374277
    225   10107 1999-07-01       alpha  2.58e-02    2.171311
    226   10107 1999-07-01  mkt_excess  1.24e+00    4.540898
    227   10107 1999-08-01       alpha  2.65e-02    2.243760
    228   10107 1999-08-01  mkt_excess  1.21e+00    4.419812
    229   10107 1999-09-01       alpha  2.69e-02    2.281684
    230   10107 1999-09-01  mkt_excess  1.21e+00    4.411899
    231   10107 1999-10-01       alpha  2.48e-02    2.092842
    232   10107 1999-10-01  mkt_excess  1.17e+00    4.323188
    233   10107 1999-11-01       alpha  2.32e-02    1.920444
    234   10107 1999-11-01  mkt_excess  1.16e+00    4.209912
    235   10107 1999-12-01       alpha  2.55e-02    2.045682
    236   10107 1999-12-01  mkt_excess  1.25e+00    4.508305
    237   10107 2000-01-01       alpha  2.33e-02    1.875426
    238   10107 2000-01-01  mkt_excess  1.33e+00    4.838300
    239   10107 2000-02-01       alpha  2.13e-02    1.674627
    240   10107 2000-02-01  mkt_excess  1.33e+00    4.692924
    241   10107 2000-03-01       alpha  2.12e-02    1.654239
    242   10107 2000-03-01  mkt_excess  1.35e+00    4.802281
    243   10107 2000-04-01       alpha  1.20e-02    0.893415
    244   10107 2000-04-01  mkt_excess  1.54e+00    5.305159
    245   10107 2000-05-01       alpha  1.11e-02    0.837500
    246   10107 2000-05-01  mkt_excess  1.57e+00    5.471325
    247   10107 2000-06-01       alpha  1.34e-02    0.977835
    248   10107 2000-06-01  mkt_excess  1.62e+00    5.502900
    249   10107 2000-07-01       alpha  1.23e-02    0.898863
    250   10107 2000-07-01  mkt_excess  1.67e+00    5.668374
    251   10107 2000-08-01       alpha  1.10e-02    0.788503
    252   10107 2000-08-01  mkt_excess  1.61e+00    5.450199
    253   10107 2000-09-01       alpha  1.08e-02    0.785862
    254   10107 2000-09-01  mkt_excess  1.66e+00    5.733985
    255   10107 2000-10-01       alpha  1.22e-02    0.871758
    256   10107 2000-10-01  mkt_excess  1.63e+00    5.577901
    257   10107 2000-11-01       alpha  1.50e-02    1.129703
    258   10107 2000-11-01  mkt_excess  1.68e+00    6.276107
    259   10107 2000-12-01       alpha  1.08e-02    0.763398
    260   10107 2000-12-01  mkt_excess  1.68e+00    5.881187
    261   10107 2001-01-01       alpha  1.58e-02    1.037159
    262   10107 2001-01-01  mkt_excess  1.73e+00    5.621702
    263   10107 2001-02-01       alpha  1.82e-02    1.190406
    264   10107 2001-02-01  mkt_excess  1.65e+00    5.519448
    265   10107 2001-03-01       alpha  1.85e-02    1.215333
    266   10107 2001-03-01  mkt_excess  1.63e+00    5.592052
    267   10107 2001-04-01       alpha  1.89e-02    1.239543
    268   10107 2001-04-01  mkt_excess  1.67e+00    5.778573
    269   10107 2001-05-01       alpha  1.90e-02    1.241545
    270   10107 2001-05-01  mkt_excess  1.67e+00    5.779325
    271   10107 2001-06-01       alpha  2.00e-02    1.307306
    272   10107 2001-06-01  mkt_excess  1.66e+00    5.736146
    273   10107 2001-07-01       alpha  1.74e-02    1.133244
    274   10107 2001-07-01  mkt_excess  1.70e+00    5.791966
    275   10107 2001-08-01       alpha  1.69e-02    1.103861
    276   10107 2001-08-01  mkt_excess  1.72e+00    5.957372
    277   10107 2001-09-01       alpha  1.81e-02    1.186656
    278   10107 2001-09-01  mkt_excess  1.71e+00    6.048849
    279   10107 2001-10-01       alpha  1.92e-02    1.256078
    280   10107 2001-10-01  mkt_excess  1.72e+00    6.065769
    281   10107 2001-11-01       alpha  1.83e-02    1.197853
    282   10107 2001-11-01  mkt_excess  1.70e+00    6.007358
    283   10107 2001-12-01       alpha  1.71e-02    1.115442
    284   10107 2001-12-01  mkt_excess  1.70e+00    6.037210
    285   10107 2002-01-01       alpha  1.45e-02    0.960176
    286   10107 2002-01-01  mkt_excess  1.67e+00    5.962140
    287   10107 2002-02-01       alpha  1.44e-02    0.950477
    288   10107 2002-02-01  mkt_excess  1.68e+00    5.994740
    289   10107 2002-03-01       alpha  1.34e-02    0.884750
    290   10107 2002-03-01  mkt_excess  1.67e+00    5.923258
    291   10107 2002-04-01       alpha  8.54e-03    0.584451
    292   10107 2002-04-01  mkt_excess  1.64e+00    6.064336
    293   10107 2002-05-01       alpha  9.93e-03    0.685724
    294   10107 2002-05-01  mkt_excess  1.68e+00    6.191805
    295   10107 2002-06-01       alpha  1.41e-02    0.957558
    296   10107 2002-06-01  mkt_excess  1.62e+00    5.914072
    297   10107 2002-07-01       alpha  1.43e-02    0.971200
    298   10107 2002-07-01  mkt_excess  1.63e+00    5.972253
    299   10107 2002-08-01       alpha  1.46e-02    0.990145
    300   10107 2002-08-01  mkt_excess  1.62e+00    5.932623
    301   10107 2002-09-01       alpha  1.71e-02    1.164901
    302   10107 2002-09-01  mkt_excess  1.63e+00    6.125862
    303   10107 2002-10-01       alpha  1.80e-02    1.226998
    304   10107 2002-10-01  mkt_excess  1.67e+00    6.336265
    305   10107 2002-11-01       alpha  1.71e-02    1.160747
    306   10107 2002-11-01  mkt_excess  1.66e+00    6.317878
    307   10107 2002-12-01       alpha  1.88e-02    1.295113
    308   10107 2002-12-01  mkt_excess  1.67e+00    6.508473
    309   10107 2003-01-01       alpha  1.57e-02    1.090987
    310   10107 2003-01-01  mkt_excess  1.68e+00    6.594696
    311   10107 2003-02-01       alpha  1.60e-02    1.109846
    312   10107 2003-02-01  mkt_excess  1.68e+00    6.500262
    313   10107 2003-03-01       alpha  1.66e-02    1.148544
    314   10107 2003-03-01  mkt_excess  1.69e+00    6.508201
    315   10107 2003-04-01       alpha  1.52e-02    1.048100
    316   10107 2003-04-01  mkt_excess  1.65e+00    6.426863
    317   10107 2003-05-01       alpha  1.30e-02    0.884366
    318   10107 2003-05-01  mkt_excess  1.59e+00    6.180101
    319   10107 2003-06-01       alpha  9.50e-03    0.666217
    320   10107 2003-06-01  mkt_excess  1.56e+00    6.212641
    321   10107 2003-07-01       alpha  8.58e-03    0.603060
    322   10107 2003-07-01  mkt_excess  1.56e+00    6.230156
    323   10107 2003-08-01       alpha  5.93e-03    0.420083
    324   10107 2003-08-01  mkt_excess  1.66e+00    6.240186
    325   10107 2003-09-01       alpha  6.40e-03    0.452639
    326   10107 2003-09-01  mkt_excess  1.64e+00    6.097557
    327   10107 2003-10-01       alpha  6.41e-03    0.453650
    328   10107 2003-10-01  mkt_excess  1.65e+00    6.111002
    329   10107 2003-11-01       alpha  4.92e-03    0.348029
    330   10107 2003-11-01  mkt_excess  1.63e+00    5.969706
    331   10107 2003-12-01       alpha  4.25e-03    0.301142
    332   10107 2003-12-01  mkt_excess  1.62e+00    5.884733
    333   10107 2004-01-01       alpha  3.99e-04    0.029053
    334   10107 2004-01-01  mkt_excess  1.57e+00    5.859710
    335   10107 2004-02-01       alpha  6.99e-04    0.051120
    336   10107 2004-02-01  mkt_excess  1.54e+00    5.746044
    337   10107 2004-03-01       alpha -2.29e-03   -0.169281
    338   10107 2004-03-01  mkt_excess  1.51e+00    5.696723
    339   10107 2004-04-01       alpha  1.75e-03    0.131484
    340   10107 2004-04-01  mkt_excess  1.55e+00    5.909806
    341   10107 2004-05-01       alpha  1.05e-03    0.079025
    342   10107 2004-05-01  mkt_excess  1.55e+00    5.914245
    343   10107 2004-06-01       alpha  1.37e-03    0.102687
    344   10107 2004-06-01  mkt_excess  1.55e+00    5.847632
    345   10107 2004-07-01       alpha  2.29e-03    0.171681
    346   10107 2004-07-01  mkt_excess  1.53e+00    5.785277
    347   10107 2004-08-01       alpha  2.16e-05    0.001632
    348   10107 2004-08-01  mkt_excess  1.54e+00    5.841202
    349   10107 2004-09-01       alpha -4.77e-04   -0.036013
    350   10107 2004-09-01  mkt_excess  1.54e+00    5.840493
    351   10107 2004-10-01       alpha  6.56e-04    0.049718
    352   10107 2004-10-01  mkt_excess  1.57e+00    5.912760
    353   10107 2004-11-01       alpha  1.80e-03    0.137406
    354   10107 2004-11-01  mkt_excess  1.59e+00    6.018509
    355   10107 2004-12-01       alpha -2.04e-03   -0.158516
    356   10107 2004-12-01  mkt_excess  1.49e+00    5.649647
    357   10107 2005-01-01       alpha -1.51e-04   -0.011802
    358   10107 2005-01-01  mkt_excess  1.45e+00    5.534227
    359   10107 2005-02-01       alpha  8.53e-04    0.067316
    360   10107 2005-02-01  mkt_excess  1.47e+00    5.638627
    361   10107 2005-03-01       alpha -1.30e-03   -0.103767
    362   10107 2005-03-01  mkt_excess  1.43e+00    5.485173
    363   10107 2005-04-01       alpha  3.98e-03    0.336857
    364   10107 2005-04-01  mkt_excess  1.30e+00    5.242589
    365   10107 2005-05-01       alpha  4.34e-03    0.369215
    366   10107 2005-05-01  mkt_excess  1.27e+00    5.145457
    367   10107 2005-06-01       alpha -2.06e-04   -0.018324
    368   10107 2005-06-01  mkt_excess  1.19e+00    5.012602
    369   10107 2005-07-01       alpha  1.18e-03    0.106278
    370   10107 2005-07-01  mkt_excess  1.17e+00    4.994812
    371   10107 2005-08-01       alpha  4.13e-03    0.372891
    372   10107 2005-08-01  mkt_excess  1.21e+00    5.092858
    373   10107 2005-09-01       alpha  4.17e-03    0.378199
    374   10107 2005-09-01  mkt_excess  1.17e+00    4.893177
    375   10107 2005-10-01       alpha  1.74e-03    0.162805
    376   10107 2005-10-01  mkt_excess  1.21e+00    5.190662
    377   10107 2005-11-01       alpha  3.05e-03    0.285783
    378   10107 2005-11-01  mkt_excess  1.17e+00    4.855179
    379   10107 2005-12-01       alpha  6.48e-03    0.668402
    380   10107 2005-12-01  mkt_excess  1.20e+00    5.456345
    381   10107 2006-01-01       alpha  1.10e-03    0.145371
    382   10107 2006-01-01  mkt_excess  1.11e+00    6.486516
    383   10107 2006-02-01       alpha -1.01e-03   -0.135165
    384   10107 2006-02-01  mkt_excess  1.18e+00    6.667296
    385   10107 2006-03-01       alpha -1.32e-03   -0.175815
    386   10107 2006-03-01  mkt_excess  1.19e+00    6.525506
    387   10107 2006-04-01       alpha -5.43e-03   -0.740667
    388   10107 2006-04-01  mkt_excess  1.07e+00    5.801774
    389   10107 2006-05-01       alpha -6.03e-03   -0.823460
    390   10107 2006-05-01  mkt_excess  1.08e+00    5.880172
    391   10107 2006-06-01       alpha -6.81e-03   -0.942965
    392   10107 2006-06-01  mkt_excess  1.09e+00    6.037778
    393   10107 2006-07-01       alpha -4.92e-03   -0.686950
    394   10107 2006-07-01  mkt_excess  1.07e+00    5.951486
    395   10107 2006-08-01       alpha -2.79e-03   -0.390457
    396   10107 2006-08-01  mkt_excess  1.03e+00    5.631789
    397   10107 2006-09-01       alpha -1.91e-03   -0.263842
    398   10107 2006-09-01  mkt_excess  1.03e+00    5.296175
    399   10107 2006-10-01       alpha -3.39e-03   -0.485711
    400   10107 2006-10-01  mkt_excess  1.01e+00    5.386844
    401   10107 2006-11-01       alpha -3.63e-03   -0.523330
    402   10107 2006-11-01  mkt_excess  9.79e-01    5.104855
    403   10107 2006-12-01       alpha -3.78e-03   -0.546036
    404   10107 2006-12-01  mkt_excess  9.76e-01    5.093196
    405   10107 2007-01-01       alpha -3.08e-03   -0.443591
    406   10107 2007-01-01  mkt_excess  9.73e-01    5.068059
    407   10107 2007-02-01       alpha -3.17e-03   -0.455179
    408   10107 2007-02-01  mkt_excess  9.72e-01    5.046857
    409   10107 2007-03-01       alpha -3.39e-03   -0.488549
    410   10107 2007-03-01  mkt_excess  9.74e-01    5.006849
    411   10107 2007-04-01       alpha -1.10e-03   -0.159554
    412   10107 2007-04-01  mkt_excess  9.25e-01    4.756537
    413   10107 2007-05-01       alpha -9.10e-04   -0.131938
    414   10107 2007-05-01  mkt_excess  9.20e-01    4.740216
    415   10107 2007-06-01       alpha -5.02e-03   -0.777804
    416   10107 2007-06-01  mkt_excess  1.10e+00    5.841574
    417   10107 2007-07-01       alpha -3.58e-03   -0.551077
    418   10107 2007-07-01  mkt_excess  1.03e+00    5.266377
    419   10107 2007-08-01       alpha -4.18e-03   -0.643972
    420   10107 2007-08-01  mkt_excess  1.04e+00    5.273715
    421   10107 2007-09-01       alpha -4.31e-03   -0.642400
    422   10107 2007-09-01  mkt_excess  1.03e+00    4.713838
    423   10107 2007-10-01       alpha -1.09e-03   -0.147872
    424   10107 2007-10-01  mkt_excess  8.55e-01    3.344100
    425   10107 2007-11-01       alpha -2.51e-03   -0.345202
    426   10107 2007-11-01  mkt_excess  8.80e-01    3.454036
    427   10107 2007-12-01       alpha  4.44e-04    0.060068
    428   10107 2007-12-01  mkt_excess  7.68e-01    2.867590
    429   10107 2008-01-01       alpha  7.66e-04    0.105545
    430   10107 2008-01-01  mkt_excess  7.81e-01    3.076608
    431   10107 2008-02-01       alpha -2.83e-03   -0.371966
    432   10107 2008-02-01  mkt_excess  9.08e-01    3.428750
    433   10107 2008-03-01       alpha -2.02e-03   -0.264011
    434   10107 2008-03-01  mkt_excess  8.88e-01    3.338706
    435   10107 2008-04-01       alpha -2.32e-03   -0.302727
    436   10107 2008-04-01  mkt_excess  8.82e-01    3.162271
    437   10107 2008-05-01       alpha -1.89e-03   -0.253249
    438   10107 2008-05-01  mkt_excess  9.99e-01    3.538037
    439   10107 2008-06-01       alpha -9.17e-04   -0.124516
    440   10107 2008-06-01  mkt_excess  8.88e-01    3.434714
    441   10107 2008-07-01       alpha -2.10e-03   -0.283824
    442   10107 2008-07-01  mkt_excess  8.99e-01    3.437375
    443   10107 2008-08-01       alpha -1.05e-03   -0.140941
    444   10107 2008-08-01  mkt_excess  9.18e-01    3.478298
    445   10107 2008-09-01       alpha -7.46e-04   -0.100671
    446   10107 2008-09-01  mkt_excess  8.25e-01    3.418318
    447   10107 2008-10-01       alpha  7.92e-04    0.110773
    448   10107 2008-10-01  mkt_excess  9.49e-01    4.917506
    449   10107 2008-11-01       alpha  1.09e-03    0.152262
    450   10107 2008-11-01  mkt_excess  9.68e-01    5.192347
    451   10107 2008-12-01       alpha -2.90e-04   -0.040206
    452   10107 2008-12-01  mkt_excess  9.43e-01    4.972544
    453   10107 2009-01-01       alpha -6.53e-04   -0.089840
    454   10107 2009-01-01  mkt_excess  9.83e-01    5.312475
    455   10107 2009-02-01       alpha  8.54e-04    0.116935
    456   10107 2009-02-01  mkt_excess  9.45e-01    5.358679
    457   10107 2009-03-01       alpha  2.74e-03    0.378155
    458   10107 2009-03-01  mkt_excess  9.86e-01    5.844403
    459   10107 2009-04-01       alpha  1.72e-03    0.240993
    460   10107 2009-04-01  mkt_excess  9.94e-01    6.247751
    461   10107 2009-05-01       alpha  1.61e-03    0.226128
    462   10107 2009-05-01  mkt_excess  9.88e-01    6.280455
    463   10107 2009-06-01       alpha  2.67e-03    0.360867
    464   10107 2009-06-01  mkt_excess  9.84e-01    6.029171
    465   10107 2009-07-01       alpha  5.85e-04    0.078078
    466   10107 2009-07-01  mkt_excess  9.39e-01    5.771664
    467   10107 2009-08-01       alpha  1.64e-03    0.219314
    468   10107 2009-08-01  mkt_excess  9.45e-01    5.853514
    469   10107 2009-09-01       alpha  1.77e-03    0.237428
    470   10107 2009-09-01  mkt_excess  9.47e-01    5.895511
    471   10107 2009-10-01       alpha  3.52e-03    0.459410
    472   10107 2009-10-01  mkt_excess  9.28e-01    5.648086
    473   10107 2009-11-01       alpha  3.35e-03    0.438242
    474   10107 2009-11-01  mkt_excess  9.26e-01    5.660327
    475   10107 2009-12-01       alpha  4.14e-03    0.543139
    476   10107 2009-12-01  mkt_excess  9.38e-01    5.748087
    477   10107 2010-01-01       alpha  3.28e-03    0.428241
    478   10107 2010-01-01  mkt_excess  9.50e-01    5.802994
    479   10107 2010-02-01       alpha  4.10e-03    0.539810
    480   10107 2010-02-01  mkt_excess  9.56e-01    5.908826
    481   10107 2010-03-01       alpha  3.86e-03    0.507328
    482   10107 2010-03-01  mkt_excess  9.32e-01    5.823714
    483   10107 2010-04-01       alpha  3.08e-03    0.408259
    484   10107 2010-04-01  mkt_excess  9.48e-01    5.973450
    485   10107 2010-05-01       alpha  2.04e-03    0.266553
    486   10107 2010-05-01  mkt_excess  9.98e-01    6.312734
    487   10107 2010-06-01       alpha  1.94e-03    0.252650
    488   10107 2010-06-01  mkt_excess  1.02e+00    6.516705
    489   10107 2010-07-01       alpha  2.99e-03    0.388017
    490   10107 2010-07-01  mkt_excess  1.05e+00    6.730846
    491   10107 2010-08-01       alpha  1.04e-03    0.136754
    492   10107 2010-08-01  mkt_excess  1.07e+00    6.980218
    493   10107 2010-09-01       alpha  1.22e-03    0.161549
    494   10107 2010-09-01  mkt_excess  1.03e+00    6.989659
    495   10107 2010-10-01       alpha  1.73e-03    0.227282
    496   10107 2010-10-01  mkt_excess  1.05e+00    7.067043
    497   10107 2010-11-01       alpha  1.99e-04    0.025989
    498   10107 2010-11-01  mkt_excess  1.04e+00    6.947565
    499   10107 2010-12-01       alpha  1.68e-03    0.220546
    500   10107 2010-12-01  mkt_excess  1.05e+00    7.166418
    501   10107 2011-01-01       alpha  5.52e-04    0.072744
    502   10107 2011-01-01  mkt_excess  1.04e+00    7.100713
    503   10107 2011-02-01       alpha  1.05e-04    0.013771
    504   10107 2011-02-01  mkt_excess  1.02e+00    6.957267
    505   10107 2011-03-01       alpha -6.18e-04   -0.080205
    506   10107 2011-03-01  mkt_excess  1.02e+00    6.912565
    507   10107 2011-04-01       alpha  1.29e-03    0.173971
    508   10107 2011-04-01  mkt_excess  1.02e+00    7.213607
    509   10107 2011-05-01       alpha  1.48e-03    0.199881
    510   10107 2011-05-01  mkt_excess  1.02e+00    7.160430
    511   10107 2011-06-01       alpha  1.98e-03    0.265999
    512   10107 2011-06-01  mkt_excess  1.01e+00    7.082811
    513   10107 2011-07-01       alpha  2.70e-03    0.357739
    514   10107 2011-07-01  mkt_excess  1.00e+00    6.945110
    515   10107 2011-08-01       alpha  2.54e-03    0.337784
    516   10107 2011-08-01  mkt_excess  9.85e-01    6.911829
    517   10107 2011-09-01       alpha  2.01e-03    0.268565
    518   10107 2011-09-01  mkt_excess  9.77e-01    6.997378
    519   10107 2011-10-01       alpha  1.12e-03    0.149589
    520   10107 2011-10-01  mkt_excess  9.49e-01    6.996500
    521   10107 2011-11-01       alpha  5.32e-04    0.070645
    522   10107 2011-11-01  mkt_excess  9.49e-01    6.977000
    523   10107 2011-12-01       alpha  5.78e-04    0.076776
    524   10107 2011-12-01  mkt_excess  9.49e-01    6.976728
    525   10107 2012-01-01       alpha  1.77e-03    0.231224
    526   10107 2012-01-01  mkt_excess  9.72e-01    7.053467
    527   10107 2012-02-01       alpha  3.57e-03    0.469486
    528   10107 2012-02-01  mkt_excess  9.72e-01    7.145006
    529   10107 2012-03-01       alpha  3.70e-03    0.487505
    530   10107 2012-03-01  mkt_excess  9.70e-01    7.150766
    531   10107 2012-04-01       alpha  3.13e-03    0.413116
    532   10107 2012-04-01  mkt_excess  9.64e-01    7.111423
    533   10107 2012-05-01       alpha  2.86e-03    0.377075
    534   10107 2012-05-01  mkt_excess  9.75e-01    7.234235
    535   10107 2012-06-01       alpha  3.44e-03    0.454404
    536   10107 2012-06-01  mkt_excess  9.73e-01    7.254402
    537   10107 2012-07-01       alpha  2.43e-03    0.319659
    538   10107 2012-07-01  mkt_excess  9.74e-01    7.197573
    539   10107 2012-08-01       alpha  3.20e-03    0.420463
    540   10107 2012-08-01  mkt_excess  9.78e-01    7.232200
    541   10107 2012-09-01       alpha  2.35e-03    0.305984
    542   10107 2012-09-01  mkt_excess  9.71e-01    7.110069
    543   10107 2012-10-01       alpha -1.83e-03   -0.275585
    544   10107 2012-10-01  mkt_excess  9.55e-01    8.085190
    545   10107 2012-11-01       alpha -2.22e-03   -0.330509
    546   10107 2012-11-01  mkt_excess  9.42e-01    7.859068
    547   10107 2012-12-01       alpha -3.44e-03   -0.520517
    548   10107 2012-12-01  mkt_excess  9.46e-01    8.009596
    549   10107 2013-01-01       alpha -3.34e-03   -0.504513
    550   10107 2013-01-01  mkt_excess  9.32e-01    7.850658
    551   10107 2013-02-01       alpha -7.92e-04   -0.126919
    552   10107 2013-02-01  mkt_excess  9.06e-01    8.092157
    553   10107 2013-03-01       alpha -1.78e-03   -0.287296
    554   10107 2013-03-01  mkt_excess  9.09e-01    8.221413
    555   10107 2013-04-01       alpha  1.16e-03    0.174871
    556   10107 2013-04-01  mkt_excess  9.25e-01    7.783888
    557   10107 2013-05-01       alpha  2.09e-03    0.315350
    558   10107 2013-05-01  mkt_excess  9.31e-01    7.823366
    559   10107 2013-06-01       alpha  1.12e-03    0.170004
    560   10107 2013-06-01  mkt_excess  9.55e-01    7.910262
    561   10107 2013-07-01       alpha  2.63e-04    0.038107
    562   10107 2013-07-01  mkt_excess  9.13e-01    7.283397
    563   10107 2013-08-01       alpha  9.25e-04    0.132369
    564   10107 2013-08-01  mkt_excess  8.96e-01    7.078077
    565   10107 2013-09-01       alpha -1.00e-03   -0.143838
    566   10107 2013-09-01  mkt_excess  9.26e-01    7.166123
    567   10107 2013-10-01       alpha -5.01e-04   -0.070092
    568   10107 2013-10-01  mkt_excess  9.26e-01    6.419734
    569   10107 2013-11-01       alpha  8.03e-04    0.109836
    570   10107 2013-11-01  mkt_excess  9.20e-01    6.136853
    571   10107 2013-12-01       alpha  1.03e-03    0.140708
    572   10107 2013-12-01  mkt_excess  9.17e-01    6.135864
    573   10107 2014-01-01       alpha  3.39e-03    0.462104
    574   10107 2014-01-01  mkt_excess  8.62e-01    5.630046
    575   10107 2014-02-01       alpha  1.79e-03    0.237422
    576   10107 2014-02-01  mkt_excess  8.95e-01    5.530291
    577   10107 2014-03-01       alpha  2.76e-03    0.369202
    578   10107 2014-03-01  mkt_excess  8.49e-01    5.105350
    579   10107 2014-04-01       alpha  2.42e-03    0.326765
    580   10107 2014-04-01  mkt_excess  8.39e-01    4.876568
    581   10107 2014-05-01       alpha  2.52e-03    0.341951
    582   10107 2014-05-01  mkt_excess  8.43e-01    4.863392
    583   10107 2014-06-01       alpha -4.58e-06   -0.000655
    584   10107 2014-06-01  mkt_excess  8.58e-01    5.237658
    585   10107 2014-07-01       alpha  1.69e-03    0.246746
    586   10107 2014-07-01  mkt_excess  8.91e-01    5.430323
    587   10107 2014-08-01       alpha  1.62e-03    0.237136
    588   10107 2014-08-01  mkt_excess  8.93e-01    5.453548
    589   10107 2014-09-01       alpha  2.33e-03    0.341637
    590   10107 2014-09-01  mkt_excess  8.78e-01    5.353105
    591   10107 2014-10-01       alpha -6.60e-05   -0.009969
    592   10107 2014-10-01  mkt_excess  9.19e-01    5.756714
    593   10107 2014-11-01       alpha -1.85e-04   -0.028044
    594   10107 2014-11-01  mkt_excess  9.13e-01    5.668967
    595   10107 2014-12-01       alpha -8.60e-04   -0.130580
    596   10107 2014-12-01  mkt_excess  9.15e-01    5.669234
    597   10107 2015-01-01       alpha -2.16e-03   -0.318065
    598   10107 2015-01-01  mkt_excess  9.42e-01    5.660267
    599   10107 2015-02-01       alpha -1.69e-03   -0.247443
    600   10107 2015-02-01  mkt_excess  9.63e-01    5.823230
    601   10107 2015-03-01       alpha -2.52e-03   -0.369828
    602   10107 2015-03-01  mkt_excess  1.00e+00    5.939144
    603   10107 2015-04-01       alpha  4.57e-04    0.060076
    604   10107 2015-04-01  mkt_excess  9.85e-01    5.240676
    605   10107 2015-05-01       alpha  2.09e-03    0.273436
    606   10107 2015-05-01  mkt_excess  9.00e-01    4.613356
    607   10107 2015-06-01       alpha  2.85e-03    0.370521
    608   10107 2015-06-01  mkt_excess  8.63e-01    4.334278
    609   10107 2015-07-01       alpha  3.13e-03    0.411403
    610   10107 2015-07-01  mkt_excess  8.20e-01    4.057188
    611   10107 2015-08-01       alpha  4.03e-03    0.535065
    612   10107 2015-08-01  mkt_excess  7.95e-01    4.004023
    613   10107 2015-09-01       alpha  5.02e-03    0.676852
    614   10107 2015-09-01  mkt_excess  8.17e-01    3.971244
    615   10107 2015-10-01       alpha  5.14e-03    0.669824
    616   10107 2015-10-01  mkt_excess  9.05e-01    4.371895
    617   10107 2015-11-01       alpha  6.67e-03    0.874441
    618   10107 2015-11-01  mkt_excess  8.98e-01    4.369929
    619   10107 2015-12-01       alpha  7.05e-03    0.936577
    620   10107 2015-12-01  mkt_excess  8.54e-01    4.095549
    621   10107 2016-01-01       alpha  8.38e-03    1.123901
    622   10107 2016-01-01  mkt_excess  8.28e-01    4.088777
    623   10107 2016-02-01       alpha  8.00e-03    1.075992
    624   10107 2016-02-01  mkt_excess  8.63e-01    4.234851
    625   10107 2016-03-01       alpha  9.15e-03    1.233203
    626   10107 2016-03-01  mkt_excess  8.72e-01    4.421062
    627   10107 2016-04-01       alpha  7.44e-03    0.971277
    628   10107 2016-04-01  mkt_excess  8.76e-01    4.277358
    629   10107 2016-05-01       alpha  8.67e-03    1.122924
    630   10107 2016-05-01  mkt_excess  8.73e-01    4.239306
    631   10107 2016-06-01       alpha  6.97e-03    0.901720
    632   10107 2016-06-01  mkt_excess  8.95e-01    4.331692
    633   10107 2016-07-01       alpha  6.34e-03    0.815051
    634   10107 2016-07-01  mkt_excess  9.49e-01    4.590656
    635   10107 2016-08-01       alpha  5.73e-03    0.729331
    636   10107 2016-08-01  mkt_excess  9.75e-01    4.569485
    637   10107 2016-09-01       alpha  5.48e-03    0.684090
    638   10107 2016-09-01  mkt_excess  9.81e-01    4.348367
    639   10107 2016-10-01       alpha  6.56e-03    0.828698
    640   10107 2016-10-01  mkt_excess  1.04e+00    4.251771
    641   10107 2016-11-01       alpha  6.86e-03    0.859176
    642   10107 2016-11-01  mkt_excess  9.98e-01    4.132561
    643   10107 2016-12-01       alpha  6.94e-03    0.867821
    644   10107 2016-12-01  mkt_excess  9.99e-01    4.135619
    645   10107 2017-01-01       alpha  6.46e-03    0.824581
    646   10107 2017-01-01  mkt_excess  9.45e-01    3.928096
    647   10107 2017-02-01       alpha  5.60e-03    0.714573
    648   10107 2017-02-01  mkt_excess  9.05e-01    3.739820
    649   10107 2017-03-01       alpha  6.23e-03    0.798327
    650   10107 2017-03-01  mkt_excess  9.08e-01    3.739467
    651   10107 2017-04-01       alpha  6.74e-03    0.858956
    652   10107 2017-04-01  mkt_excess  9.06e-01    3.712156
    653   10107 2017-05-01       alpha  8.06e-03    1.009230
    654   10107 2017-05-01  mkt_excess  8.55e-01    3.337748
    655   10107 2017-06-01       alpha  7.50e-03    0.941319
    656   10107 2017-06-01  mkt_excess  8.54e-01    3.302204
    657   10107 2017-07-01       alpha  8.85e-03    1.114498
    658   10107 2017-07-01  mkt_excess  8.54e-01    3.318219
    659   10107 2017-08-01       alpha  8.99e-03    1.137107
    660   10107 2017-08-01  mkt_excess  8.43e-01    3.272929
    661   10107 2017-09-01       alpha  9.38e-03    1.197494
    662   10107 2017-09-01  mkt_excess  8.55e-01    3.346698
    663   10107 2017-10-01       alpha  1.15e-02    1.425866
    664   10107 2017-10-01  mkt_excess  8.52e-01    3.253021
    665   10107 2017-11-01       alpha  1.26e-02    1.584415
    666   10107 2017-11-01  mkt_excess  8.35e-01    3.250129
    667   10107 2017-12-01       alpha  1.28e-02    1.613501
    668   10107 2017-12-01  mkt_excess  8.35e-01    3.252811
    669   10107 2018-01-01       alpha  1.33e-02    1.662220
    670   10107 2018-01-01  mkt_excess  9.09e-01    3.528936
    671   10107 2018-02-01       alpha  1.36e-02    1.730154
    672   10107 2018-02-01  mkt_excess  8.99e-01    3.575604
    673   10107 2018-03-01       alpha  1.33e-02    1.718037
    674   10107 2018-03-01  mkt_excess  9.25e-01    3.693365
    675   10107 2018-04-01       alpha  1.14e-02    1.551907
    676   10107 2018-04-01  mkt_excess  9.11e-01    3.819154
    677   10107 2018-05-01       alpha  1.14e-02    1.552756
    678   10107 2018-05-01  mkt_excess  9.10e-01    3.814311
    679   10107 2018-06-01       alpha  1.13e-02    1.529334
    680   10107 2018-06-01  mkt_excess  9.08e-01    3.782418
    681   10107 2018-07-01       alpha  1.27e-02    1.831557
    682   10107 2018-07-01  mkt_excess  1.06e+00    4.619631
    683   10107 2018-08-01       alpha  1.09e-02    1.587752
    684   10107 2018-08-01  mkt_excess  1.12e+00    4.948045
    685   10107 2018-09-01       alpha  1.16e-02    1.719554
    686   10107 2018-09-01  mkt_excess  1.15e+00    5.114827
    687   10107 2018-10-01       alpha  1.18e-02    1.800517
    688   10107 2018-10-01  mkt_excess  1.14e+00    5.376971
    689   10107 2018-11-01       alpha  1.15e-02    1.759832
    690   10107 2018-11-01  mkt_excess  1.12e+00    5.313167
    691   10107 2018-12-01       alpha  1.27e-02    2.005264
    692   10107 2018-12-01  mkt_excess  1.13e+00    5.913899
    693   10107 2019-01-01       alpha  1.13e-02    1.737178
    694   10107 2019-01-01  mkt_excess  1.06e+00    5.638031
    695   10107 2019-02-01       alpha  1.21e-02    1.878581
    696   10107 2019-02-01  mkt_excess  1.09e+00    5.807765
    697   10107 2019-03-01       alpha  1.17e-02    1.817484
    698   10107 2019-03-01  mkt_excess  1.10e+00    5.876060
    699   10107 2019-04-01       alpha  1.27e-02    1.960729
    700   10107 2019-04-01  mkt_excess  1.12e+00    5.974432
    701   10107 2019-05-01       alpha  1.33e-02    2.072218
    702   10107 2019-05-01  mkt_excess  1.11e+00    6.170647
    703   10107 2019-06-01       alpha  1.35e-02    2.108442
    704   10107 2019-06-01  mkt_excess  1.10e+00    6.315859
    705   10107 2019-07-01       alpha  1.25e-02    1.947267
    706   10107 2019-07-01  mkt_excess  1.12e+00    6.418984
    707   10107 2019-08-01       alpha  1.31e-02    2.050045
    708   10107 2019-08-01  mkt_excess  1.11e+00    6.326170
    709   10107 2019-09-01       alpha  1.21e-02    1.900426
    710   10107 2019-09-01  mkt_excess  1.12e+00    6.357393
    711   10107 2019-10-01       alpha  1.25e-02    1.956933
    712   10107 2019-10-01  mkt_excess  1.12e+00    6.399189
    713   10107 2019-11-01       alpha  1.27e-02    1.999562
    714   10107 2019-11-01  mkt_excess  1.13e+00    6.458892
    715   10107 2019-12-01       alpha  1.34e-02    2.111759
    716   10107 2019-12-01  mkt_excess  1.12e+00    6.476904
    717   10107 2020-01-01       alpha  1.70e-02    2.750817
    718   10107 2020-01-01  mkt_excess  1.05e+00    6.220536
    719   10107 2020-02-01       alpha  1.74e-02    2.858762
    720   10107 2020-02-01  mkt_excess  1.02e+00    6.232143
    721   10107 2020-03-01       alpha  2.09e-02    3.484754
    722   10107 2020-03-01  mkt_excess  8.79e-01    5.993176
    723   10107 2020-04-01       alpha  1.80e-02    3.423492
    724   10107 2020-04-01  mkt_excess  8.76e-01    7.409642
    725   10107 2020-05-01       alpha  1.84e-02    3.532121
    726   10107 2020-05-01  mkt_excess  8.63e-01    7.441078
    727   10107 2020-06-01       alpha  2.07e-02    3.938619
    728   10107 2020-06-01  mkt_excess  8.59e-01    7.375625
    729   10107 2020-07-01       alpha  1.95e-02    3.637038
    730   10107 2020-07-01  mkt_excess  8.32e-01    7.090706
    731   10107 2020-08-01       alpha  2.05e-02    3.783139
    732   10107 2020-08-01  mkt_excess  8.24e-01    7.025626
    733   10107 2020-09-01       alpha  1.88e-02    3.427066
    734   10107 2020-09-01  mkt_excess  8.55e-01    7.203482
    735   10107 2020-10-01       alpha  1.69e-02    3.278891
    736   10107 2020-10-01  mkt_excess  8.04e-01    7.049720
    737   10107 2020-11-01       alpha  1.62e-02    3.081782
    738   10107 2020-11-01  mkt_excess  7.56e-01    6.880804
    739   10107 2020-12-01       alpha  1.56e-02    2.958449
    740   10107 2020-12-01  mkt_excess  7.58e-01    6.913624
    741   10107 2021-01-01       alpha  1.56e-02    2.932103
    742   10107 2021-01-01  mkt_excess  7.66e-01    6.848347
    743   10107 2021-02-01       alpha  1.67e-02    3.237086
    744   10107 2021-02-01  mkt_excess  7.53e-01    6.968592
    745   10107 2021-03-01       alpha  1.62e-02    3.133073
    746   10107 2021-03-01  mkt_excess  7.41e-01    6.772902
    747   10107 2021-04-01       alpha  1.84e-02    3.912601
    748   10107 2021-04-01  mkt_excess  7.41e-01    7.472453
    749   10107 2021-05-01       alpha  1.73e-02    3.697844
    750   10107 2021-05-01  mkt_excess  7.42e-01    7.531296
    751   10107 2021-06-01       alpha  1.90e-02    4.061021
    752   10107 2021-06-01  mkt_excess  7.41e-01    7.549291
    753   10107 2021-07-01       alpha  1.85e-02    4.069164
    754   10107 2021-07-01  mkt_excess  7.29e-01    7.573580
    755   10107 2021-08-01       alpha  1.89e-02    4.123762
    756   10107 2021-08-01  mkt_excess  7.31e-01    7.577123
    757   10107 2021-09-01       alpha  1.80e-02    3.886387
    758   10107 2021-09-01  mkt_excess  7.54e-01    7.776979
    759   10107 2021-10-01       alpha  1.84e-02    3.675889
    760   10107 2021-10-01  mkt_excess  8.07e-01    7.834422
    761   10107 2021-11-01       alpha  1.88e-02    3.840656
    762   10107 2021-11-01  mkt_excess  8.21e-01    8.069305
    763   10107 2021-12-01       alpha  1.85e-02    3.742039
    764   10107 2021-12-01  mkt_excess  8.18e-01    8.000852
    765   10107 2022-01-01       alpha  1.73e-02    3.507002
    766   10107 2022-01-01  mkt_excess  8.42e-01    8.323692
    767   10107 2022-02-01       alpha  1.74e-02    3.571245
    768   10107 2022-02-01  mkt_excess  8.60e-01    8.610231
    769   10107 2022-03-01       alpha  1.70e-02    3.487913
    770   10107 2022-03-01  mkt_excess  8.59e-01    8.607776
    771   10107 2022-04-01       alpha  1.59e-02    3.277373
    772   10107 2022-04-01  mkt_excess  8.85e-01    9.172176
    773   10107 2022-05-01       alpha  1.54e-02    3.148449
    774   10107 2022-05-01  mkt_excess  8.88e-01    9.153970
    775   10107 2022-06-01       alpha  1.61e-02    3.337221
    776   10107 2022-06-01  mkt_excess  8.86e-01    9.481450
    777   10107 2022-07-01       alpha  1.56e-02    3.242168
    778   10107 2022-07-01  mkt_excess  8.80e-01    9.666688
    779   10107 2022-08-01       alpha  1.44e-02    2.945508
    780   10107 2022-08-01  mkt_excess  8.96e-01    9.764542
    781   10107 2022-09-01       alpha  1.41e-02    2.920025
    782   10107 2022-09-01  mkt_excess  9.24e-01   10.388050
    783   10107 2022-10-01       alpha  1.16e-02    2.376937
    784   10107 2022-10-01  mkt_excess  8.82e-01    9.972429
    785   10107 2022-11-01       alpha  1.27e-02    2.565308
    786   10107 2022-11-01  mkt_excess  8.94e-01   10.045775
    787   10107 2022-12-01       alpha  1.24e-02    2.517200
    788   10107 2022-12-01  mkt_excess  9.02e-01   10.255844
    789   10107 2023-01-01       alpha  1.11e-02    2.264378
    790   10107 2023-01-01  mkt_excess  8.77e-01   10.037935
    791   10107 2023-02-01       alpha  1.12e-02    2.281501
    792   10107 2023-02-01  mkt_excess  8.76e-01   10.000638
    793   10107 2023-03-01       alpha  1.34e-02    2.529606
    794   10107 2023-03-01  mkt_excess  8.83e-01    9.322726
    795   10107 2023-04-01       alpha  1.41e-02    2.617936
    796   10107 2023-04-01  mkt_excess  8.83e-01    9.231700
    797   10107 2023-05-01       alpha  1.45e-02    2.685526
    798   10107 2023-05-01  mkt_excess  8.79e-01    9.093136
    799   10107 2023-06-01       alpha  1.44e-02    2.638565
    800   10107 2023-06-01  mkt_excess  8.67e-01    9.003413
    801   10107 2023-07-01       alpha  1.30e-02    2.347599
    802   10107 2023-07-01  mkt_excess  8.56e-01    8.781738
    803   10107 2023-08-01       alpha  1.23e-02    2.236726
    804   10107 2023-08-01  mkt_excess  8.57e-01    8.798832
    805   10107 2023-09-01       alpha  1.21e-02    2.200444
    806   10107 2023-09-01  mkt_excess  8.60e-01    8.909244
    807   10107 2023-10-01       alpha  1.39e-02    2.449417
    808   10107 2023-10-01  mkt_excess  8.36e-01    8.291256
    809   10107 2023-11-01       alpha  1.41e-02    2.465580
    810   10107 2023-11-01  mkt_excess  8.48e-01    8.524383
    811   10107 2023-12-01       alpha  1.36e-02    2.314400
    812   10107 2023-12-01  mkt_excess  8.23e-01    7.918170
    813   10107 2024-01-01       alpha  1.48e-02    2.563754
    814   10107 2024-01-01  mkt_excess  8.46e-01    8.101222
    815   10107 2024-02-01       alpha  1.40e-02    2.436956
    816   10107 2024-02-01  mkt_excess  8.37e-01    8.067938
    817   10107 2024-03-01       alpha  1.32e-02    2.289007
    818   10107 2024-03-01  mkt_excess  8.35e-01    8.058561
    819   10107 2024-04-01       alpha  1.12e-02    1.968711
    820   10107 2024-04-01  mkt_excess  8.42e-01    8.210777
    821   10107 2024-05-01       alpha  1.16e-02    2.010992
    822   10107 2024-05-01  mkt_excess  8.43e-01    8.087441
    823   10107 2024-06-01       alpha  1.20e-02    2.080568
    824   10107 2024-06-01  mkt_excess  8.43e-01    7.968531
    825   10107 2024-07-01       alpha  1.06e-02    1.776971
    826   10107 2024-07-01  mkt_excess  8.42e-01    7.689483
    827   10107 2024-08-01       alpha  9.68e-03    1.610364
    828   10107 2024-08-01  mkt_excess  8.47e-01    7.692815
    829   10107 2024-09-01       alpha  9.98e-03    1.661516
    830   10107 2024-09-01  mkt_excess  8.47e-01    7.703190
    831   10107 2024-10-01       alpha  8.84e-03    1.452926
    832   10107 2024-10-01  mkt_excess  8.54e-01    7.663636
    833   10107 2024-11-01       alpha  8.28e-03    1.357813
    834   10107 2024-11-01  mkt_excess  8.44e-01    7.612716
    835   10107 2024-12-01       alpha  8.34e-03    1.372772
    836   10107 2024-12-01  mkt_excess  8.41e-01    7.616261
    837   14593 1984-12-01       alpha  1.04e-03    0.048229
    838   14593 1984-12-01  mkt_excess  2.04e+00    4.043927
    839   14593 1985-01-01       alpha -2.21e-03   -0.102670
    840   14593 1985-01-01  mkt_excess  1.90e+00    3.877764
    841   14593 1985-02-01       alpha -5.63e-03   -0.263670
    842   14593 1985-02-01  mkt_excess  1.88e+00    3.830902
    843   14593 1985-03-01       alpha -7.44e-03   -0.354146
    844   14593 1985-03-01  mkt_excess  1.89e+00    3.879356
    845   14593 1985-04-01       alpha -7.85e-03   -0.381238
    846   14593 1985-04-01  mkt_excess  1.90e+00    3.926158
    847   14593 1985-05-01       alpha -1.27e-02   -0.607435
    848   14593 1985-05-01  mkt_excess  1.76e+00    3.604950
    849   14593 1985-06-01       alpha -1.23e-02   -0.600295
    850   14593 1985-06-01  mkt_excess  1.76e+00    3.645535
    851   14593 1985-07-01       alpha -1.41e-02   -0.700133
    852   14593 1985-07-01  mkt_excess  1.77e+00    3.688824
    853   14593 1985-08-01       alpha -1.47e-02   -0.739348
    854   14593 1985-08-01  mkt_excess  1.77e+00    3.733275
    855   14593 1985-09-01       alpha -1.21e-02   -0.617644
    856   14593 1985-09-01  mkt_excess  1.71e+00    3.641910
    857   14593 1985-10-01       alpha -1.02e-02   -0.523713
    858   14593 1985-10-01  mkt_excess  1.75e+00    3.776209
    859   14593 1985-11-01       alpha -1.06e-02   -0.554193
    860   14593 1985-11-01  mkt_excess  1.74e+00    3.842543
    861   14593 1985-12-01       alpha -1.01e-02   -0.538498
    862   14593 1985-12-01  mkt_excess  1.75e+00    3.919325
    863   14593 1986-01-01       alpha -7.80e-03   -0.414604
    864   14593 1986-01-01  mkt_excess  1.70e+00    3.773844
    865   14593 1986-02-01       alpha -7.04e-03   -0.373786
    866   14593 1986-02-01  mkt_excess  1.68e+00    3.806744
    867   14593 1986-03-01       alpha -4.24e-03   -0.226862
    868   14593 1986-03-01  mkt_excess  1.74e+00    3.988418
    869   14593 1986-04-01       alpha -6.06e-03   -0.327629
    870   14593 1986-04-01  mkt_excess  1.77e+00    4.102853
    871   14593 1986-05-01       alpha -6.73e-03   -0.363573
    872   14593 1986-05-01  mkt_excess  1.83e+00    4.278865
    873   14593 1986-06-01       alpha -4.14e-03   -0.226296
    874   14593 1986-06-01  mkt_excess  1.78e+00    4.192278
    875   14593 1986-07-01       alpha -4.13e-03   -0.226380
    876   14593 1986-07-01  mkt_excess  1.78e+00    4.295774
    877   14593 1986-08-01       alpha -1.38e-03   -0.075042
    878   14593 1986-08-01  mkt_excess  1.76e+00    4.182682
    879   14593 1986-09-01       alpha  2.83e-03    0.155151
    880   14593 1986-09-01  mkt_excess  1.63e+00    3.929266
    881   14593 1986-10-01       alpha -7.38e-04   -0.041329
    882   14593 1986-10-01  mkt_excess  1.53e+00    3.776969
    883   14593 1986-11-01       alpha  3.39e-03    0.189837
    884   14593 1986-11-01  mkt_excess  1.57e+00    3.850015
    885   14593 1986-12-01       alpha -1.93e-04   -0.011095
    886   14593 1986-12-01  mkt_excess  1.64e+00    4.126950
    887   14593 1987-01-01       alpha  1.52e-03    0.085485
    888   14593 1987-01-01  mkt_excess  1.78e+00    4.649195
    889   14593 1987-02-01       alpha  4.14e-03    0.226872
    890   14593 1987-02-01  mkt_excess  1.82e+00    4.608084
    891   14593 1987-03-01       alpha  3.36e-03    0.182660
    892   14593 1987-03-01  mkt_excess  1.80e+00    4.521517
    893   14593 1987-04-01       alpha  1.15e-02    0.621140
    894   14593 1987-04-01  mkt_excess  1.76e+00    4.387083
    895   14593 1987-05-01       alpha  1.12e-02    0.603965
    896   14593 1987-05-01  mkt_excess  1.76e+00    4.338879
    897   14593 1987-06-01       alpha  1.17e-02    0.622239
    898   14593 1987-06-01  mkt_excess  1.73e+00    4.227036
    899   14593 1987-07-01       alpha  8.66e-03    0.459059
    900   14593 1987-07-01  mkt_excess  1.75e+00    4.268182
    901   14593 1987-08-01       alpha  1.15e-02    0.603638
    902   14593 1987-08-01  mkt_excess  1.69e+00    3.868598
    903   14593 1987-09-01       alpha  1.36e-02    0.712080
    904   14593 1987-09-01  mkt_excess  1.66e+00    3.825894
    905   14593 1987-10-01       alpha  1.30e-02    0.719299
    906   14593 1987-10-01  mkt_excess  1.46e+00    4.138179
    907   14593 1987-11-01       alpha  9.50e-03    0.535988
    908   14593 1987-11-01  mkt_excess  1.44e+00    4.209913
    909   14593 1987-12-01       alpha  1.32e-02    0.738564
    910   14593 1987-12-01  mkt_excess  1.50e+00    4.403732
    911   14593 1988-01-01       alpha  7.30e-03    0.423695
    912   14593 1988-01-01  mkt_excess  1.43e+00    4.363762
    913   14593 1988-02-01       alpha  5.63e-03    0.327299
    914   14593 1988-02-01  mkt_excess  1.41e+00    4.332750
    915   14593 1988-03-01       alpha  6.76e-03    0.396587
    916   14593 1988-03-01  mkt_excess  1.44e+00    4.445146
    917   14593 1988-04-01       alpha  5.61e-03    0.330576
    918   14593 1988-04-01  mkt_excess  1.40e+00    4.312240
    919   14593 1988-05-01       alpha  3.66e-03    0.217739
    920   14593 1988-05-01  mkt_excess  1.40e+00    4.346900
    921   14593 1988-06-01       alpha  7.52e-03    0.457178
    922   14593 1988-06-01  mkt_excess  1.45e+00    4.594968
    923   14593 1988-07-01       alpha  1.14e-02    0.714530
    924   14593 1988-07-01  mkt_excess  1.38e+00    4.503717
    925   14593 1988-08-01       alpha  9.15e-03    0.574137
    926   14593 1988-08-01  mkt_excess  1.40e+00    4.587270
    927   14593 1988-09-01       alpha  1.63e-02    1.136663
    928   14593 1988-09-01  mkt_excess  1.41e+00    5.164802
    929   14593 1988-10-01       alpha  1.38e-02    0.949648
    930   14593 1988-10-01  mkt_excess  1.41e+00    5.051521
    931   14593 1988-11-01       alpha  1.61e-02    1.122989
    932   14593 1988-11-01  mkt_excess  1.42e+00    5.203766
    933   14593 1988-12-01       alpha  1.30e-02    0.933494
    934   14593 1988-12-01  mkt_excess  1.46e+00    5.464574
    935   14593 1989-01-01       alpha  1.02e-02    0.715982
    936   14593 1989-01-01  mkt_excess  1.40e+00    5.203925
    937   14593 1989-02-01       alpha  7.63e-03    0.539981
    938   14593 1989-02-01  mkt_excess  1.45e+00    5.373752
    939   14593 1989-03-01       alpha  8.11e-03    0.575427
    940   14593 1989-03-01  mkt_excess  1.44e+00    5.376493
    941   14593 1989-04-01       alpha  3.84e-03    0.286140
    942   14593 1989-04-01  mkt_excess  1.47e+00    5.791882
    943   14593 1989-05-01       alpha  6.10e-03    0.442127
    944   14593 1989-05-01  mkt_excess  1.50e+00    5.698676
    945   14593 1989-06-01       alpha  5.98e-03    0.435439
    946   14593 1989-06-01  mkt_excess  1.53e+00    5.819386
    947   14593 1989-07-01       alpha  4.25e-03    0.301620
    948   14593 1989-07-01  mkt_excess  1.46e+00    5.517711
    949   14593 1989-08-01       alpha  6.99e-03    0.502062
    950   14593 1989-08-01  mkt_excess  1.54e+00    5.694453
    951   14593 1989-09-01       alpha  7.95e-03    0.572269
    952   14593 1989-09-01  mkt_excess  1.53e+00    5.683719
    953   14593 1989-10-01       alpha  9.91e-03    0.711679
    954   14593 1989-10-01  mkt_excess  1.51e+00    5.584091
    955   14593 1989-11-01       alpha  8.52e-03    0.607759
    956   14593 1989-11-01  mkt_excess  1.51e+00    5.547433
    957   14593 1989-12-01       alpha  2.45e-03    0.170388
    958   14593 1989-12-01  mkt_excess  1.50e+00    5.370162
    959   14593 1990-01-01       alpha  5.72e-03    0.405154
    960   14593 1990-01-01  mkt_excess  1.52e+00    5.543707
    961   14593 1990-02-01       alpha  8.21e-03    0.595200
    962   14593 1990-02-01  mkt_excess  1.52e+00    5.691201
    963   14593 1990-03-01       alpha  1.24e-02    0.891387
    964   14593 1990-03-01  mkt_excess  1.52e+00    5.647777
    965   14593 1990-04-01       alpha  1.34e-02    0.963925
    966   14593 1990-04-01  mkt_excess  1.51e+00    5.653539
    967   14593 1990-05-01       alpha  1.62e-02    1.231462
    968   14593 1990-05-01  mkt_excess  1.54e+00    6.164602
    969   14593 1990-06-01       alpha  1.76e-02    1.338974
    970   14593 1990-06-01  mkt_excess  1.53e+00    6.102735
    971   14593 1990-07-01       alpha  1.89e-02    1.452567
    972   14593 1990-07-01  mkt_excess  1.53e+00    6.174951
    973   14593 1990-08-01       alpha  2.03e-02    1.570401
    974   14593 1990-08-01  mkt_excess  1.52e+00    6.353099
    975   14593 1990-09-01       alpha  1.57e-02    1.206817
    976   14593 1990-09-01  mkt_excess  1.60e+00    6.675135
    977   14593 1990-10-01       alpha  1.54e-02    1.189431
    978   14593 1990-10-01  mkt_excess  1.57e+00    6.570776
    979   14593 1990-11-01       alpha  1.72e-02    1.324805
    980   14593 1990-11-01  mkt_excess  1.61e+00    6.716869
    981   14593 1990-12-01       alpha  1.88e-02    1.436203
    982   14593 1990-12-01  mkt_excess  1.62e+00    6.683153
    983   14593 1991-01-01       alpha  2.15e-02    1.590875
    984   14593 1991-01-01  mkt_excess  1.67e+00    6.706758
    985   14593 1991-02-01       alpha  2.08e-02    1.529202
    986   14593 1991-02-01  mkt_excess  1.65e+00    6.590812
    987   14593 1991-03-01       alpha  2.24e-02    1.628043
    988   14593 1991-03-01  mkt_excess  1.66e+00    6.530790
    989   14593 1991-04-01       alpha  1.76e-02    1.245013
    990   14593 1991-04-01  mkt_excess  1.68e+00    6.395844
    991   14593 1991-05-01       alpha  1.22e-02    0.838581
    992   14593 1991-05-01  mkt_excess  1.60e+00    5.971565
    993   14593 1991-06-01       alpha  1.23e-02    0.847670
    994   14593 1991-06-01  mkt_excess  1.62e+00    6.097457
    995   14593 1991-07-01       alpha  1.35e-02    0.931193
    996   14593 1991-07-01  mkt_excess  1.61e+00    6.001469
    997   14593 1991-08-01       alpha  1.40e-02    0.962995
    998   14593 1991-08-01  mkt_excess  1.60e+00    5.891700
    999   14593 1991-09-01       alpha  1.24e-02    0.853361
    1000  14593 1991-09-01  mkt_excess  1.62e+00    5.820644
    1001  14593 1991-10-01       alpha  1.34e-02    0.921055
    1002  14593 1991-10-01  mkt_excess  1.64e+00    5.860008
    1003  14593 1991-11-01       alpha  1.21e-02    0.844245
    1004  14593 1991-11-01  mkt_excess  1.62e+00    5.893608
    1005  14593 1991-12-01       alpha  1.02e-02    0.707718
    1006  14593 1991-12-01  mkt_excess  1.58e+00    5.902452
    1007  14593 1992-01-01       alpha  1.07e-02    0.744212
    1008  14593 1992-01-01  mkt_excess  1.45e+00    5.219066
    1009  14593 1992-02-01       alpha  8.09e-03    0.578791
    1010  14593 1992-02-01  mkt_excess  1.41e+00    5.157500
    1011  14593 1992-03-01       alpha  8.03e-03    0.575064
    1012  14593 1992-03-01  mkt_excess  1.44e+00    5.286342
    1013  14593 1992-04-01       alpha  3.83e-03    0.287857
    1014  14593 1992-04-01  mkt_excess  1.48e+00    5.691085
    1015  14593 1992-05-01       alpha  3.73e-03    0.279754
    1016  14593 1992-05-01  mkt_excess  1.48e+00    5.689800
    1017  14593 1992-06-01       alpha  1.49e-03    0.109716
    1018  14593 1992-06-01  mkt_excess  1.52e+00    5.707859
    1019  14593 1992-07-01       alpha  8.18e-04    0.059977
    1020  14593 1992-07-01  mkt_excess  1.51e+00    5.648001
    1021  14593 1992-08-01       alpha -2.93e-03   -0.226905
    1022  14593 1992-08-01  mkt_excess  1.45e+00    5.725667
    1023  14593 1992-09-01       alpha -4.94e-03   -0.384120
    1024  14593 1992-09-01  mkt_excess  1.46e+00    5.793070
    1025  14593 1992-10-01       alpha -3.21e-03   -0.241724
    1026  14593 1992-10-01  mkt_excess  1.52e+00    4.721180
    1027  14593 1992-11-01       alpha -2.09e-03   -0.155283
    1028  14593 1992-11-01  mkt_excess  1.51e+00    4.532357
    1029  14593 1992-12-01       alpha -3.71e-03   -0.284486
    1030  14593 1992-12-01  mkt_excess  1.40e+00    4.221036
    1031  14593 1993-01-01       alpha -3.01e-03   -0.232674
    1032  14593 1993-01-01  mkt_excess  1.43e+00    4.292936
    1033  14593 1993-02-01       alpha -4.44e-03   -0.340952
    1034  14593 1993-02-01  mkt_excess  1.45e+00    4.279211
    1035  14593 1993-03-01       alpha -4.64e-03   -0.354302
    1036  14593 1993-03-01  mkt_excess  1.43e+00    4.184746
    1037  14593 1993-04-01       alpha -4.12e-03   -0.315214
    1038  14593 1993-04-01  mkt_excess  1.41e+00    4.163175
    1039  14593 1993-05-01       alpha -3.41e-03   -0.258815
    1040  14593 1993-05-01  mkt_excess  1.43e+00    4.211853
    1041  14593 1993-06-01       alpha -9.20e-03   -0.652493
    1042  14593 1993-06-01  mkt_excess  1.42e+00    3.861848
    1043  14593 1993-07-01       alpha -1.39e-02   -0.928670
    1044  14593 1993-07-01  mkt_excess  1.45e+00    3.722008
    1045  14593 1993-08-01       alpha -1.41e-02   -0.936968
    1046  14593 1993-08-01  mkt_excess  1.40e+00    3.565758
    1047  14593 1993-09-01       alpha -1.66e-02   -1.099508
    1048  14593 1993-09-01  mkt_excess  1.40e+00    3.526204
    1049  14593 1993-10-01       alpha -9.79e-03   -0.616195
    1050  14593 1993-10-01  mkt_excess  1.43e+00    3.426086
    1051  14593 1993-11-01       alpha -8.86e-03   -0.556428
    1052  14593 1993-11-01  mkt_excess  1.41e+00    3.378851
    1053  14593 1993-12-01       alpha -1.11e-02   -0.693899
    1054  14593 1993-12-01  mkt_excess  1.40e+00    3.337269
    1055  14593 1994-01-01       alpha -8.09e-03   -0.511862
    1056  14593 1994-01-01  mkt_excess  1.51e+00    3.593396
    1057  14593 1994-02-01       alpha -4.89e-03   -0.304806
    1058  14593 1994-02-01  mkt_excess  1.45e+00    3.390439
    1059  14593 1994-03-01       alpha -4.58e-03   -0.287344
    1060  14593 1994-03-01  mkt_excess  1.46e+00    3.490796
    1061  14593 1994-04-01       alpha -6.74e-03   -0.421657
    1062  14593 1994-04-01  mkt_excess  1.45e+00    3.405889
    1063  14593 1994-05-01       alpha -9.85e-03   -0.628378
    1064  14593 1994-05-01  mkt_excess  1.39e+00    3.310036
    1065  14593 1994-06-01       alpha -8.66e-03   -0.556686
    1066  14593 1994-06-01  mkt_excess  1.38e+00    3.336792
    1067  14593 1994-07-01       alpha -3.20e-03   -0.201971
    1068  14593 1994-07-01  mkt_excess  1.57e+00    3.624933
    1069  14593 1994-08-01       alpha -4.53e-03   -0.287094
    1070  14593 1994-08-01  mkt_excess  1.56e+00    3.658836
    1071  14593 1994-09-01       alpha -5.29e-03   -0.335336
    1072  14593 1994-09-01  mkt_excess  1.58e+00    3.699080
    1073  14593 1994-10-01       alpha -3.00e-03   -0.183742
    1074  14593 1994-10-01  mkt_excess  1.66e+00    3.726552
    1075  14593 1994-11-01       alpha -3.20e-03   -0.196731
    1076  14593 1994-11-01  mkt_excess  1.70e+00    3.869764
    1077  14593 1994-12-01       alpha  9.93e-04    0.062807
    1078  14593 1994-12-01  mkt_excess  1.72e+00    4.037244
    1079  14593 1995-01-01       alpha -1.15e-03   -0.072459
    1080  14593 1995-01-01  mkt_excess  1.83e+00    4.122065
    1081  14593 1995-02-01       alpha -2.05e-03   -0.128600
    1082  14593 1995-02-01  mkt_excess  1.79e+00    4.053089
    1083  14593 1995-03-01       alpha -6.65e-03   -0.417371
    1084  14593 1995-03-01  mkt_excess  1.74e+00    3.934108
    1085  14593 1995-04-01       alpha -6.62e-03   -0.413234
    1086  14593 1995-04-01  mkt_excess  1.77e+00    3.960871
    1087  14593 1995-05-01       alpha -5.08e-03   -0.320336
    1088  14593 1995-05-01  mkt_excess  1.89e+00    4.103732
    1089  14593 1995-06-01       alpha -6.03e-03   -0.380380
    1090  14593 1995-06-01  mkt_excess  1.94e+00    4.224903
    1091  14593 1995-07-01       alpha -6.89e-03   -0.429748
    1092  14593 1995-07-01  mkt_excess  1.89e+00    4.092890
    1093  14593 1995-08-01       alpha -1.04e-02   -0.636564
    1094  14593 1995-08-01  mkt_excess  2.04e+00    4.023096
    1095  14593 1995-09-01       alpha -9.94e-03   -0.586791
    1096  14593 1995-09-01  mkt_excess  1.83e+00    3.414489
    1097  14593 1995-10-01       alpha -1.20e-02   -0.714788
    1098  14593 1995-10-01  mkt_excess  1.89e+00    3.526489
    1099  14593 1995-11-01       alpha -1.27e-02   -0.755795
    1100  14593 1995-11-01  mkt_excess  1.79e+00    3.295382
    1101  14593 1995-12-01       alpha -1.74e-02   -1.034469
    1102  14593 1995-12-01  mkt_excess  1.75e+00    3.199786
    1103  14593 1996-01-01       alpha -2.16e-02   -1.312556
    1104  14593 1996-01-01  mkt_excess  1.54e+00    2.832805
    1105  14593 1996-02-01       alpha -2.15e-02   -1.316566
    1106  14593 1996-02-01  mkt_excess  1.63e+00    2.872502
    1107  14593 1996-03-01       alpha -2.54e-02   -1.572778
    1108  14593 1996-03-01  mkt_excess  1.57e+00    2.783610
    1109  14593 1996-04-01       alpha -2.24e-02   -1.410239
    1110  14593 1996-04-01  mkt_excess  1.52e+00    2.742347
    1111  14593 1996-05-01       alpha -1.97e-02   -1.265543
    1112  14593 1996-05-01  mkt_excess  1.65e+00    3.022121
    1113  14593 1996-06-01       alpha -2.26e-02   -1.410631
    1114  14593 1996-06-01  mkt_excess  1.70e+00    2.962425
    1115  14593 1996-07-01       alpha -1.84e-02   -1.163378
    1116  14593 1996-07-01  mkt_excess  1.40e+00    2.512661
    1117  14593 1996-08-01       alpha -1.92e-02   -1.220860
    1118  14593 1996-08-01  mkt_excess  1.39e+00    2.516348
    1119  14593 1996-09-01       alpha -1.98e-02   -1.230415
    1120  14593 1996-09-01  mkt_excess  1.25e+00    2.264998
    1121  14593 1996-10-01       alpha -1.97e-02   -1.227654
    1122  14593 1996-10-01  mkt_excess  1.25e+00    2.257852
    1123  14593 1996-11-01       alpha -2.14e-02   -1.308727
    1124  14593 1996-11-01  mkt_excess  1.30e+00    2.353161
    1125  14593 1996-12-01       alpha -2.37e-02   -1.457539
    1126  14593 1996-12-01  mkt_excess  1.40e+00    2.259040
    1127  14593 1997-01-01       alpha -2.90e-02   -1.738489
    1128  14593 1997-01-01  mkt_excess  1.20e+00    1.949728
    1129  14593 1997-02-01       alpha -2.98e-02   -1.794928
    1130  14593 1997-02-01  mkt_excess  1.20e+00    1.950106
    1131  14593 1997-03-01       alpha -2.17e-02   -1.293751
    1132  14593 1997-03-01  mkt_excess  8.26e-01    1.360674
    1133  14593 1997-04-01       alpha -2.32e-02   -1.373056
    1134  14593 1997-04-01  mkt_excess  7.62e-01    1.266361
    1135  14593 1997-05-01       alpha -2.36e-02   -1.383335
    1136  14593 1997-05-01  mkt_excess  6.95e-01    1.196291
    1137  14593 1997-06-01       alpha -2.09e-02   -1.211103
    1138  14593 1997-06-01  mkt_excess  4.72e-01    0.811734
    1139  14593 1997-07-01       alpha -2.04e-02   -1.153141
    1140  14593 1997-07-01  mkt_excess  7.54e-01    1.311774
    1141  14593 1997-08-01       alpha -1.24e-02   -0.678362
    1142  14593 1997-08-01  mkt_excess  4.59e-01    0.779143
    1143  14593 1997-09-01       alpha -1.23e-02   -0.667792
    1144  14593 1997-09-01  mkt_excess  4.43e-01    0.765270
    1145  14593 1997-10-01       alpha -2.04e-02   -1.121035
    1146  14593 1997-10-01  mkt_excess  6.24e-01    1.102231
    1147  14593 1997-11-01       alpha -2.08e-02   -1.148795
    1148  14593 1997-11-01  mkt_excess  5.88e-01    1.035554
    1149  14593 1997-12-01       alpha -2.57e-02   -1.377414
    1150  14593 1997-12-01  mkt_excess  5.76e-01    0.985236
    1151  14593 1998-01-01       alpha -1.81e-02   -0.902302
    1152  14593 1998-01-01  mkt_excess  4.99e-01    0.792721
    1153  14593 1998-02-01       alpha -1.53e-02   -0.738889
    1154  14593 1998-02-01  mkt_excess  7.56e-01    1.209504
    1155  14593 1998-03-01       alpha -1.36e-02   -0.649811
    1156  14593 1998-03-01  mkt_excess  8.46e-01    1.357299
    1157  14593 1998-04-01       alpha -1.45e-02   -0.686604
    1158  14593 1998-04-01  mkt_excess  8.72e-01    1.375955
    1159  14593 1998-05-01       alpha -1.55e-02   -0.745692
    1160  14593 1998-05-01  mkt_excess  8.37e-01    1.346159
    1161  14593 1998-06-01       alpha -9.17e-03   -0.454695
    1162  14593 1998-06-01  mkt_excess  8.07e-01    1.344034
    1163  14593 1998-07-01       alpha  2.35e-03    0.119052
    1164  14593 1998-07-01  mkt_excess  5.82e-01    0.993618
    1165  14593 1998-08-01       alpha  2.83e-03    0.150548
    1166  14593 1998-08-01  mkt_excess  6.27e-01    1.311277
    1167  14593 1998-09-01       alpha  6.92e-03    0.363733
    1168  14593 1998-09-01  mkt_excess  7.12e-01    1.501158
    1169  14593 1998-10-01       alpha  1.27e-03    0.068655
    1170  14593 1998-10-01  mkt_excess  6.50e-01    1.447393
    1171  14593 1998-11-01       alpha -1.37e-03   -0.072825
    1172  14593 1998-11-01  mkt_excess  5.69e-01    1.259428
    1173  14593 1998-12-01       alpha  2.40e-03    0.123966
    1174  14593 1998-12-01  mkt_excess  6.92e-01    1.517742
    1175  14593 1999-01-01       alpha  6.91e-04    0.035793
    1176  14593 1999-01-01  mkt_excess  6.72e-01    1.481257
    1177  14593 1999-02-01       alpha -5.37e-03   -0.279138
    1178  14593 1999-02-01  mkt_excess  7.96e-01    1.769816
    1179  14593 1999-03-01       alpha -4.02e-03   -0.206702
    1180  14593 1999-03-01  mkt_excess  7.66e-01    1.674331
    1181  14593 1999-04-01       alpha  8.25e-04    0.041402
    1182  14593 1999-04-01  mkt_excess  8.29e-01    1.782536
    1183  14593 1999-05-01       alpha  7.77e-04    0.039195
    1184  14593 1999-05-01  mkt_excess  8.37e-01    1.813153
    1185  14593 1999-06-01       alpha  2.62e-03    0.130887
    1186  14593 1999-06-01  mkt_excess  8.04e-01    1.736430
    1187  14593 1999-07-01       alpha  4.49e-03    0.227802
    1188  14593 1999-07-01  mkt_excess  6.59e-01    1.446569
    1189  14593 1999-08-01       alpha  7.49e-03    0.378278
    1190  14593 1999-08-01  mkt_excess  5.99e-01    1.301364
    1191  14593 1999-09-01       alpha  8.40e-03    0.425010
    1192  14593 1999-09-01  mkt_excess  5.84e-01    1.274555
    1193  14593 1999-10-01       alpha  6.14e-03    0.312180
    1194  14593 1999-10-01  mkt_excess  6.86e-01    1.527454
    1195  14593 1999-11-01       alpha  1.18e-02    0.585737
    1196  14593 1999-11-01  mkt_excess  6.54e-01    1.423154
    1197  14593 1999-12-01       alpha  1.12e-02    0.553838
    1198  14593 1999-12-01  mkt_excess  6.47e-01    1.432982
    1199  14593 2000-01-01       alpha  1.17e-02    0.585757
    1200  14593 2000-01-01  mkt_excess  6.32e-01    1.427810
    1201  14593 2000-02-01       alpha  1.36e-02    0.681807
    1202  14593 2000-02-01  mkt_excess  6.49e-01    1.462834
    1203  14593 2000-03-01       alpha  1.73e-02    0.865785
    1204  14593 2000-03-01  mkt_excess  7.00e-01    1.589570
    1205  14593 2000-04-01       alpha  1.48e-02    0.750989
    1206  14593 2000-04-01  mkt_excess  7.42e-01    1.731925
    1207  14593 2000-05-01       alpha  6.82e-03    0.336663
    1208  14593 2000-05-01  mkt_excess  8.89e-01    2.030302
    1209  14593 2000-06-01       alpha  8.07e-03    0.392962
    1210  14593 2000-06-01  mkt_excess  9.34e-01    2.116104
    1211  14593 2000-07-01       alpha  8.75e-03    0.430350
    1212  14593 2000-07-01  mkt_excess  9.56e-01    2.179299
    1213  14593 2000-08-01       alpha  1.10e-02    0.536973
    1214  14593 2000-08-01  mkt_excess  1.01e+00    2.315403
    1215  14593 2000-09-01       alpha  1.05e-03    0.047494
    1216  14593 2000-09-01  mkt_excess  1.32e+00    2.849644
    1217  14593 2000-10-01       alpha -3.08e-03   -0.137680
    1218  14593 2000-10-01  mkt_excess  1.39e+00    2.958591
    1219  14593 2000-11-01       alpha -3.29e-03   -0.149347
    1220  14593 2000-11-01  mkt_excess  1.40e+00    3.128794
    1221  14593 2000-12-01       alpha -2.22e-03   -0.101507
    1222  14593 2000-12-01  mkt_excess  1.40e+00    3.144892
    1223  14593 2001-01-01       alpha  6.50e-03    0.285304
    1224  14593 2001-01-01  mkt_excess  1.47e+00    3.198956
    1225  14593 2001-02-01       alpha  6.65e-03    0.294265
    1226  14593 2001-02-01  mkt_excess  1.49e+00    3.366016
    1227  14593 2001-03-01       alpha  1.50e-02    0.652357
    1228  14593 2001-03-01  mkt_excess  1.34e+00    3.021972
    1229  14593 2001-04-01       alpha  1.63e-02    0.706721
    1230  14593 2001-04-01  mkt_excess  1.35e+00    3.110690
    1231  14593 2001-05-01       alpha  1.18e-02    0.506663
    1232  14593 2001-05-01  mkt_excess  1.35e+00    3.057590
    1233  14593 2001-06-01       alpha  1.85e-02    0.792797
    1234  14593 2001-06-01  mkt_excess  1.30e+00    2.951810
    1235  14593 2001-07-01       alpha  1.30e-02    0.554908
    1236  14593 2001-07-01  mkt_excess  1.38e+00    3.074069
    1237  14593 2001-08-01       alpha  1.34e-02    0.574993
    1238  14593 2001-08-01  mkt_excess  1.35e+00    3.046873
    1239  14593 2001-09-01       alpha  1.50e-02    0.650405
    1240  14593 2001-09-01  mkt_excess  1.42e+00    3.317153
    1241  14593 2001-10-01       alpha  1.62e-02    0.701000
    1242  14593 2001-10-01  mkt_excess  1.43e+00    3.336728
    1243  14593 2001-11-01       alpha  1.84e-02    0.795294
    1244  14593 2001-11-01  mkt_excess  1.49e+00    3.479147
    1245  14593 2001-12-01       alpha  2.05e-02    0.886869
    1246  14593 2001-12-01  mkt_excess  1.47e+00    3.449904
    1247  14593 2002-01-01       alpha  2.73e-02    1.213177
    1248  14593 2002-01-01  mkt_excess  1.53e+00    3.665033
    1249  14593 2002-02-01       alpha  2.61e-02    1.156170
    1250  14593 2002-02-01  mkt_excess  1.55e+00    3.697794
    1251  14593 2002-03-01       alpha  2.29e-02    1.021312
    1252  14593 2002-03-01  mkt_excess  1.60e+00    3.843641
    1253  14593 2002-04-01       alpha  2.70e-02    1.212041
    1254  14593 2002-04-01  mkt_excess  1.61e+00    3.902970
    1255  14593 2002-05-01       alpha  2.88e-02    1.302404
    1256  14593 2002-05-01  mkt_excess  1.68e+00    4.048425
    1257  14593 2002-06-01       alpha  3.03e-02    1.388707
    1258  14593 2002-06-01  mkt_excess  1.80e+00    4.438083
    1259  14593 2002-07-01       alpha  2.88e-02    1.322624
    1260  14593 2002-07-01  mkt_excess  1.78e+00    4.418743
    1261  14593 2002-08-01       alpha  2.29e-02    1.079633
    1262  14593 2002-08-01  mkt_excess  1.84e+00    4.665365
    1263  14593 2002-09-01       alpha  2.74e-02    1.286117
    1264  14593 2002-09-01  mkt_excess  1.80e+00    4.645845
    1265  14593 2002-10-01       alpha  2.92e-02    1.387103
    1266  14593 2002-10-01  mkt_excess  1.74e+00    4.595540
    1267  14593 2002-11-01       alpha  2.71e-02    1.273812
    1268  14593 2002-11-01  mkt_excess  1.69e+00    4.465036
    1269  14593 2002-12-01       alpha  3.23e-02    1.569144
    1270  14593 2002-12-01  mkt_excess  1.72e+00    4.726550
    1271  14593 2003-01-01       alpha  2.65e-02    1.349843
    1272  14593 2003-01-01  mkt_excess  1.71e+00    4.926694
    1273  14593 2003-02-01       alpha  2.48e-02    1.267105
    1274  14593 2003-02-01  mkt_excess  1.65e+00    4.721292
    1275  14593 2003-03-01       alpha  2.20e-02    1.121954
    1276  14593 2003-03-01  mkt_excess  1.63e+00    4.607517
    1277  14593 2003-04-01       alpha  2.00e-02    1.012941
    1278  14593 2003-04-01  mkt_excess  1.56e+00    4.477847
    1279  14593 2003-05-01       alpha  2.26e-02    1.137982
    1280  14593 2003-05-01  mkt_excess  1.61e+00    4.615572
    1281  14593 2003-06-01       alpha  2.29e-02    1.151176
    1282  14593 2003-06-01  mkt_excess  1.61e+00    4.608595
    1283  14593 2003-07-01       alpha  2.00e-02    1.025923
    1284  14593 2003-07-01  mkt_excess  1.64e+00    4.781713
    1285  14593 2003-08-01       alpha  1.77e-02    0.916785
    1286  14593 2003-08-01  mkt_excess  1.78e+00    4.871654
    1287  14593 2003-09-01       alpha  1.49e-02    0.771292
    1288  14593 2003-09-01  mkt_excess  1.75e+00    4.749361
    1289  14593 2003-10-01       alpha  1.74e-02    0.909969
    1290  14593 2003-10-01  mkt_excess  1.81e+00    4.961154
    1291  14593 2003-11-01       alpha  1.98e-02    1.061273
    1292  14593 2003-11-01  mkt_excess  1.90e+00    5.278086
    1293  14593 2003-12-01       alpha  1.60e-02    0.864467
    1294  14593 2003-12-01  mkt_excess  1.83e+00    5.062868
    1295  14593 2004-01-01       alpha  1.74e-02    0.936841
    1296  14593 2004-01-01  mkt_excess  1.84e+00    5.111924
    1297  14593 2004-02-01       alpha  1.93e-02    1.046257
    1298  14593 2004-02-01  mkt_excess  1.82e+00    5.043904
    1299  14593 2004-03-01       alpha  2.24e-02    1.209482
    1300  14593 2004-03-01  mkt_excess  1.82e+00    5.001808
    1301  14593 2004-04-01       alpha  1.88e-02    1.025901
    1302  14593 2004-04-01  mkt_excess  1.77e+00    4.904485
    1303  14593 2004-05-01       alpha  2.00e-02    1.088203
    1304  14593 2004-05-01  mkt_excess  1.78e+00    4.898602
    1305  14593 2004-06-01       alpha  2.28e-02    1.236488
    1306  14593 2004-06-01  mkt_excess  1.81e+00    4.941964
    1307  14593 2004-07-01       alpha  1.96e-02    1.090255
    1308  14593 2004-07-01  mkt_excess  1.85e+00    5.184707
    1309  14593 2004-08-01       alpha  1.75e-02    0.984807
    1310  14593 2004-08-01  mkt_excess  1.86e+00    5.289990
    1311  14593 2004-09-01       alpha  1.87e-02    1.053369
    1312  14593 2004-09-01  mkt_excess  1.87e+00    5.294674
    1313  14593 2004-10-01       alpha  2.16e-02    1.176103
    1314  14593 2004-10-01  mkt_excess  1.85e+00    4.999564
    1315  14593 2004-11-01       alpha  2.23e-02    1.208159
    1316  14593 2004-11-01  mkt_excess  1.87e+00    5.050779
    1317  14593 2004-12-01       alpha  2.23e-02    1.204074
    1318  14593 2004-12-01  mkt_excess  1.91e+00    5.051785
    1319  14593 2005-01-01       alpha  2.47e-02    1.313287
    1320  14593 2005-01-01  mkt_excess  1.89e+00    4.894552
    1321  14593 2005-02-01       alpha  2.60e-02    1.375687
    1322  14593 2005-02-01  mkt_excess  1.90e+00    4.895831
    1323  14593 2005-03-01       alpha  2.40e-02    1.267137
    1324  14593 2005-03-01  mkt_excess  1.89e+00    4.808983
    1325  14593 2005-04-01       alpha  2.21e-02    1.162818
    1326  14593 2005-04-01  mkt_excess  1.91e+00    4.784853
    1327  14593 2005-05-01       alpha  2.66e-02    1.441608
    1328  14593 2005-05-01  mkt_excess  1.83e+00    4.717759
    1329  14593 2005-06-01       alpha  2.24e-02    1.215955
    1330  14593 2005-06-01  mkt_excess  1.77e+00    4.547720
    1331  14593 2005-07-01       alpha  2.37e-02    1.286200
    1332  14593 2005-07-01  mkt_excess  1.79e+00    4.603390
    1333  14593 2005-08-01       alpha  2.44e-02    1.321160
    1334  14593 2005-08-01  mkt_excess  1.76e+00    4.421784
    1335  14593 2005-09-01       alpha  3.44e-02    2.111485
    1336  14593 2005-09-01  mkt_excess  1.55e+00    4.370786
    1337  14593 2005-10-01       alpha  3.94e-02    2.492761
    1338  14593 2005-10-01  mkt_excess  1.49e+00    4.323773
    1339  14593 2005-11-01       alpha  4.15e-02    2.617089
    1340  14593 2005-11-01  mkt_excess  1.47e+00    4.097523
    1341  14593 2005-12-01       alpha  4.45e-02    2.850843
    1342  14593 2005-12-01  mkt_excess  1.49e+00    4.201432
    1343  14593 2006-01-01       alpha  3.79e-02    2.638773
    1344  14593 2006-01-01  mkt_excess  1.38e+00    4.227582
    1345  14593 2006-02-01       alpha  3.69e-02    2.538292
    1346  14593 2006-02-01  mkt_excess  1.33e+00    3.851437
    1347  14593 2006-03-01       alpha  2.94e-02    2.096303
    1348  14593 2006-03-01  mkt_excess  1.52e+00    4.443103
    1349  14593 2006-04-01       alpha  3.06e-02    2.182159
    1350  14593 2006-04-01  mkt_excess  1.52e+00    4.301820
    1351  14593 2006-05-01       alpha  3.27e-02    2.423431
    1352  14593 2006-05-01  mkt_excess  1.58e+00    4.700423
    1353  14593 2006-06-01       alpha  2.87e-02    2.164822
    1354  14593 2006-06-01  mkt_excess  1.62e+00    4.894230
    1355  14593 2006-07-01       alpha  3.48e-02    2.645132
    1356  14593 2006-07-01  mkt_excess  1.56e+00    4.729000
    1357  14593 2006-08-01       alpha  3.27e-02    2.472122
    1358  14593 2006-08-01  mkt_excess  1.58e+00    4.683251
    1359  14593 2006-09-01       alpha  3.50e-02    2.627217
    1360  14593 2006-09-01  mkt_excess  1.53e+00    4.285051
    1361  14593 2006-10-01       alpha  3.36e-02    2.526009
    1362  14593 2006-10-01  mkt_excess  1.51e+00    4.232624
    1363  14593 2006-11-01       alpha  3.39e-02    2.554380
    1364  14593 2006-11-01  mkt_excess  1.46e+00    3.975547
    1365  14593 2006-12-01       alpha  3.23e-02    2.409115
    1366  14593 2006-12-01  mkt_excess  1.46e+00    3.921119
    1367  14593 2007-01-01       alpha  2.95e-02    2.215587
    1368  14593 2007-01-01  mkt_excess  1.48e+00    4.017544
    1369  14593 2007-02-01       alpha  3.14e-02    2.385072
    1370  14593 2007-02-01  mkt_excess  1.44e+00    3.955981
    1371  14593 2007-03-01       alpha  3.23e-02    2.453386
    1372  14593 2007-03-01  mkt_excess  1.44e+00    3.919606
    1373  14593 2007-04-01       alpha  3.07e-02    2.324189
    1374  14593 2007-04-01  mkt_excess  1.49e+00    4.000330
    1375  14593 2007-05-01       alpha  3.35e-02    2.500105
    1376  14593 2007-05-01  mkt_excess  1.52e+00    4.044450
    1377  14593 2007-06-01       alpha  3.77e-02    2.867434
    1378  14593 2007-06-01  mkt_excess  1.33e+00    3.478305
    1379  14593 2007-07-01       alpha  4.17e-02    3.145196
    1380  14593 2007-07-01  mkt_excess  1.16e+00    2.901159
    1381  14593 2007-08-01       alpha  4.30e-02    3.262077
    1382  14593 2007-08-01  mkt_excess  1.16e+00    2.906446
    1383  14593 2007-09-01       alpha  4.09e-02    3.012828
    1384  14593 2007-09-01  mkt_excess  1.31e+00    2.938944
    1385  14593 2007-10-01       alpha  4.35e-02    3.149022
    1386  14593 2007-10-01  mkt_excess  1.39e+00    2.917177
    1387  14593 2007-11-01       alpha  4.42e-02    3.345232
    1388  14593 2007-11-01  mkt_excess  1.60e+00    3.471653
    1389  14593 2007-12-01       alpha  4.62e-02    3.444739
    1390  14593 2007-12-01  mkt_excess  1.54e+00    3.165147
    1391  14593 2008-01-01       alpha  3.84e-02    2.746535
    1392  14593 2008-01-01  mkt_excess  1.96e+00    4.003670
    1393  14593 2008-02-01       alpha  3.62e-02    2.593390
    1394  14593 2008-02-01  mkt_excess  2.03e+00    4.184541
    1395  14593 2008-03-01       alpha  4.06e-02    2.914661
    1396  14593 2008-03-01  mkt_excess  1.99e+00    4.110622
    1397  14593 2008-04-01       alpha  4.22e-02    3.147468
    1398  14593 2008-04-01  mkt_excess  2.41e+00    4.934687
    1399  14593 2008-05-01       alpha  4.16e-02    3.128547
    1400  14593 2008-05-01  mkt_excess  2.31e+00    4.592142
    1401  14593 2008-06-01       alpha  4.28e-02    3.267056
    1402  14593 2008-06-01  mkt_excess  2.24e+00    4.879274
    1403  14593 2008-07-01       alpha  4.12e-02    3.142458
    1404  14593 2008-07-01  mkt_excess  2.26e+00    4.876432
    1405  14593 2008-08-01       alpha  4.14e-02    3.157990
    1406  14593 2008-08-01  mkt_excess  2.26e+00    4.879372
    1407  14593 2008-09-01       alpha  3.97e-02    3.013866
    1408  14593 2008-09-01  mkt_excess  2.51e+00    5.857950
    1409  14593 2008-10-01       alpha  4.60e-02    3.317131
    1410  14593 2008-10-01  mkt_excess  1.88e+00    5.014848
    1411  14593 2008-11-01       alpha  4.82e-02    3.529791
    1412  14593 2008-11-01  mkt_excess  1.94e+00    5.451749
    1413  14593 2008-12-01       alpha  4.74e-02    3.431876
    1414  14593 2008-12-01  mkt_excess  1.96e+00    5.396939
    1415  14593 2009-01-01       alpha  5.01e-02    3.548977
    1416  14593 2009-01-01  mkt_excess  1.83e+00    5.098529
    1417  14593 2009-02-01       alpha  5.17e-02    3.602912
    1418  14593 2009-02-01  mkt_excess  1.71e+00    4.947873
    1419  14593 2009-03-01       alpha  4.94e-02    3.492578
    1420  14593 2009-03-01  mkt_excess  1.70e+00    5.150467
    1421  14593 2009-04-01       alpha  5.00e-02    3.560933
    1422  14593 2009-04-01  mkt_excess  1.67e+00    5.322506
    1423  14593 2009-05-01       alpha  4.87e-02    3.462637
    1424  14593 2009-05-01  mkt_excess  1.64e+00    5.284132
    1425  14593 2009-06-01       alpha  4.72e-02    3.372099
    1426  14593 2009-06-01  mkt_excess  1.62e+00    5.254922
    1427  14593 2009-07-01       alpha  4.66e-02    3.331280
    1428  14593 2009-07-01  mkt_excess  1.61e+00    5.312989
    1429  14593 2009-08-01       alpha  4.51e-02    3.215200
    1430  14593 2009-08-01  mkt_excess  1.59e+00    5.257694
    1431  14593 2009-09-01       alpha  4.41e-02    3.150055
    1432  14593 2009-09-01  mkt_excess  1.59e+00    5.267279
    1433  14593 2009-10-01       alpha  3.96e-02    3.013184
    1434  14593 2009-10-01  mkt_excess  1.55e+00    5.504109
    1435  14593 2009-11-01       alpha  3.56e-02    2.774587
    1436  14593 2009-11-01  mkt_excess  1.46e+00    5.334635
    1437  14593 2009-12-01       alpha  3.74e-02    2.951504
    1438  14593 2009-12-01  mkt_excess  1.49e+00    5.507054
    1439  14593 2010-01-01       alpha  3.29e-02    2.679083
    1440  14593 2010-01-01  mkt_excess  1.55e+00    5.920236
    1441  14593 2010-02-01       alpha  3.08e-02    2.537241
    1442  14593 2010-02-01  mkt_excess  1.53e+00    5.921799
    1443  14593 2010-03-01       alpha  3.24e-02    2.681036
    1444  14593 2010-03-01  mkt_excess  1.53e+00    6.023718
    1445  14593 2010-04-01       alpha  3.54e-02    2.971279
    1446  14593 2010-04-01  mkt_excess  1.51e+00    6.022938
    1447  14593 2010-05-01       alpha  3.64e-02    3.042445
    1448  14593 2010-05-01  mkt_excess  1.47e+00    5.946210
    1449  14593 2010-06-01       alpha  3.88e-02    3.293112
    1450  14593 2010-06-01  mkt_excess  1.47e+00    6.084102
    1451  14593 2010-07-01       alpha  3.58e-02    3.011855
    1452  14593 2010-07-01  mkt_excess  1.39e+00    5.797759
    1453  14593 2010-08-01       alpha  3.41e-02    2.886492
    1454  14593 2010-08-01  mkt_excess  1.41e+00    5.928459
    1455  14593 2010-09-01       alpha  3.24e-02    2.773747
    1456  14593 2010-09-01  mkt_excess  1.40e+00    6.156642
    1457  14593 2010-10-01       alpha  3.09e-02    2.649972
    1458  14593 2010-10-01  mkt_excess  1.41e+00    6.216001
    1459  14593 2010-11-01       alpha  2.93e-02    2.535358
    1460  14593 2010-11-01  mkt_excess  1.39e+00    6.158545
    1461  14593 2010-12-01       alpha  2.74e-02    2.353874
    1462  14593 2010-12-01  mkt_excess  1.35e+00    6.047702
    1463  14593 2011-01-01       alpha  2.77e-02    2.382283
    1464  14593 2011-01-01  mkt_excess  1.35e+00    6.057366
    1465  14593 2011-02-01       alpha  2.91e-02    2.543304
    1466  14593 2011-02-01  mkt_excess  1.34e+00    6.115880
    1467  14593 2011-03-01       alpha  3.06e-02    2.719761
    1468  14593 2011-03-01  mkt_excess  1.35e+00    6.272033
    1469  14593 2011-04-01       alpha  2.82e-02    2.515910
    1470  14593 2011-04-01  mkt_excess  1.34e+00    6.242283
    1471  14593 2011-05-01       alpha  3.03e-02    2.757443
    1472  14593 2011-05-01  mkt_excess  1.31e+00    6.208738
    1473  14593 2011-06-01       alpha  3.08e-02    2.812851
    1474  14593 2011-06-01  mkt_excess  1.31e+00    6.252632
    1475  14593 2011-07-01       alpha  3.08e-02    2.818214
    1476  14593 2011-07-01  mkt_excess  1.30e+00    6.184816
    1477  14593 2011-08-01       alpha  3.24e-02    2.977313
    1478  14593 2011-08-01  mkt_excess  1.29e+00    6.251836
    1479  14593 2011-09-01       alpha  3.21e-02    2.959237
    1480  14593 2011-09-01  mkt_excess  1.26e+00    6.212014
    1481  14593 2011-10-01       alpha  3.07e-02    2.789092
    1482  14593 2011-10-01  mkt_excess  1.19e+00    6.005237
    1483  14593 2011-11-01       alpha  2.81e-02    2.547052
    1484  14593 2011-11-01  mkt_excess  1.19e+00    5.969790
    1485  14593 2011-12-01       alpha  3.04e-02    2.802228
    1486  14593 2011-12-01  mkt_excess  1.19e+00    6.095982
    1487  14593 2012-01-01       alpha  3.16e-02    2.920379
    1488  14593 2012-01-01  mkt_excess  1.21e+00    6.200742
    1489  14593 2012-02-01       alpha  3.37e-02    3.074635
    1490  14593 2012-02-01  mkt_excess  1.23e+00    6.252766
    1491  14593 2012-03-01       alpha  3.34e-02    3.050751
    1492  14593 2012-03-01  mkt_excess  1.23e+00    6.297362
    1493  14593 2012-04-01       alpha  3.27e-02    2.979053
    1494  14593 2012-04-01  mkt_excess  1.24e+00    6.284397
    1495  14593 2012-05-01       alpha  3.10e-02    2.887024
    1496  14593 2012-05-01  mkt_excess  1.20e+00    6.300907
    1497  14593 2012-06-01       alpha  2.99e-02    2.776027
    1498  14593 2012-06-01  mkt_excess  1.19e+00    6.212555
    1499  14593 2012-07-01       alpha  2.85e-02    2.669240
    1500  14593 2012-07-01  mkt_excess  1.21e+00    6.351651
    1501  14593 2012-08-01       alpha  2.90e-02    2.707021
    1502  14593 2012-08-01  mkt_excess  1.21e+00    6.371215
    1503  14593 2012-09-01       alpha  2.74e-02    2.553889
    1504  14593 2012-09-01  mkt_excess  1.20e+00    6.279887
    1505  14593 2012-10-01       alpha  2.24e-02    2.152529
    1506  14593 2012-10-01  mkt_excess  1.19e+00    6.449483
    1507  14593 2012-11-01       alpha  2.18e-02    2.091292
    1508  14593 2012-11-01  mkt_excess  1.19e+00    6.371583
    1509  14593 2012-12-01       alpha  1.85e-02    1.749416
    1510  14593 2012-12-01  mkt_excess  1.19e+00    6.280768
    1511  14593 2013-01-01       alpha  1.99e-02    1.926796
    1512  14593 2013-01-01  mkt_excess  1.03e+00    5.564428
    1513  14593 2013-02-01       alpha  2.01e-02    1.947192
    1514  14593 2013-02-01  mkt_excess  1.01e+00    5.469678
    1515  14593 2013-03-01       alpha  1.69e-02    1.666765
    1516  14593 2013-03-01  mkt_excess  1.01e+00    5.610483
    1517  14593 2013-04-01       alpha  1.41e-02    1.437601
    1518  14593 2013-04-01  mkt_excess  9.81e-01    5.574030
    1519  14593 2013-05-01       alpha  1.30e-02    1.326551
    1520  14593 2013-05-01  mkt_excess  9.76e-01    5.566150
    1521  14593 2013-06-01       alpha  1.18e-02    1.182410
    1522  14593 2013-06-01  mkt_excess  9.65e-01    5.289180
    1523  14593 2013-07-01       alpha  1.39e-02    1.381499
    1524  14593 2013-07-01  mkt_excess  9.80e-01    5.387242
    1525  14593 2013-08-01       alpha  1.50e-02    1.483375
    1526  14593 2013-08-01  mkt_excess  9.60e-01    5.233358
    1527  14593 2013-09-01       alpha  1.98e-02    2.152405
    1528  14593 2013-09-01  mkt_excess  7.91e-01    4.657480
    1529  14593 2013-10-01       alpha  1.82e-02    1.948591
    1530  14593 2013-10-01  mkt_excess  8.85e-01    4.700508
    1531  14593 2013-11-01       alpha  2.10e-02    2.253464
    1532  14593 2013-11-01  mkt_excess  8.24e-01    4.303803
    1533  14593 2013-12-01       alpha  2.24e-02    2.449736
    1534  14593 2013-12-01  mkt_excess  8.22e-01    4.395944
    1535  14593 2014-01-01       alpha  1.70e-02    1.851026
    1536  14593 2014-01-01  mkt_excess  9.44e-01    4.928951
    1537  14593 2014-02-01       alpha  1.43e-02    1.534813
    1538  14593 2014-02-01  mkt_excess  1.02e+00    5.081910
    1539  14593 2014-03-01       alpha  1.40e-02    1.533551
    1540  14593 2014-03-01  mkt_excess  9.71e-01    4.771614
    1541  14593 2014-04-01       alpha  1.55e-02    1.713079
    1542  14593 2014-04-01  mkt_excess  8.80e-01    4.179716
    1543  14593 2014-05-01       alpha  1.60e-02    1.770821
    1544  14593 2014-05-01  mkt_excess  8.75e-01    4.115870
    1545  14593 2014-06-01       alpha  1.53e-02    1.690559
    1546  14593 2014-06-01  mkt_excess  8.78e-01    4.132274
    1547  14593 2014-07-01       alpha  1.56e-02    1.748166
    1548  14593 2014-07-01  mkt_excess  8.22e-01    3.842178
    1549  14593 2014-08-01       alpha  1.61e-02    1.803066
    1550  14593 2014-08-01  mkt_excess  8.33e-01    3.899016
    1551  14593 2014-09-01       alpha  1.51e-02    1.711339
    1552  14593 2014-09-01  mkt_excess  8.24e-01    3.881755
    1553  14593 2014-10-01       alpha  1.51e-02    1.699132
    1554  14593 2014-10-01  mkt_excess  8.38e-01    3.913030
    1555  14593 2014-11-01       alpha  1.61e-02    1.805404
    1556  14593 2014-11-01  mkt_excess  8.48e-01    3.886973
    1557  14593 2014-12-01       alpha  1.43e-02    1.583171
    1558  14593 2014-12-01  mkt_excess  8.59e-01    3.878884
    1559  14593 2015-01-01       alpha  1.77e-02    1.965451
    1560  14593 2015-01-01  mkt_excess  7.84e-01    3.544262
    1561  14593 2015-02-01       alpha  1.78e-02    1.963078
    1562  14593 2015-02-01  mkt_excess  7.98e-01    3.637710
    1563  14593 2015-03-01       alpha  1.62e-02    1.822022
    1564  14593 2015-03-01  mkt_excess  7.63e-01    3.476347
    1565  14593 2015-04-01       alpha  1.47e-02    1.677067
    1566  14593 2015-04-01  mkt_excess  7.57e-01    3.492340
    1567  14593 2015-05-01       alpha  1.41e-02    1.583775
    1568  14593 2015-05-01  mkt_excess  7.90e-01    3.472570
    1569  14593 2015-06-01       alpha  1.30e-02    1.439364
    1570  14593 2015-06-01  mkt_excess  8.13e-01    3.475085
    1571  14593 2015-07-01       alpha  1.23e-02    1.372855
    1572  14593 2015-07-01  mkt_excess  8.46e-01    3.535874
    1573  14593 2015-08-01       alpha  1.23e-02    1.368814
    1574  14593 2015-08-01  mkt_excess  8.51e-01    3.596285
    1575  14593 2015-09-01       alpha  1.18e-02    1.355094
    1576  14593 2015-09-01  mkt_excess  7.68e-01    3.171208
    1577  14593 2015-10-01       alpha  1.17e-02    1.333217
    1578  14593 2015-10-01  mkt_excess  7.71e-01    3.269017
    1579  14593 2015-11-01       alpha  1.10e-02    1.253864
    1580  14593 2015-11-01  mkt_excess  7.74e-01    3.281167
    1581  14593 2015-12-01       alpha  9.00e-03    1.018500
    1582  14593 2015-12-01  mkt_excess  8.42e-01    3.444995
    1583  14593 2016-01-01       alpha  7.72e-03    0.882667
    1584  14593 2016-01-01  mkt_excess  8.70e-01    3.659710
    1585  14593 2016-02-01       alpha  7.52e-03    0.863511
    1586  14593 2016-02-01  mkt_excess  8.70e-01    3.644819
    1587  14593 2016-03-01       alpha  8.52e-03    0.967265
    1588  14593 2016-03-01  mkt_excess  9.13e-01    3.896681
    1589  14593 2016-04-01       alpha  6.34e-03    0.690416
    1590  14593 2016-04-01  mkt_excess  9.20e-01    3.748455
    1591  14593 2016-05-01       alpha  7.13e-03    0.770520
    1592  14593 2016-05-01  mkt_excess  9.25e-01    3.742370
    1593  14593 2016-06-01       alpha  6.76e-03    0.726653
    1594  14593 2016-06-01  mkt_excess  9.22e-01    3.704891
    1595  14593 2016-07-01       alpha  3.53e-03    0.398983
    1596  14593 2016-07-01  mkt_excess  1.02e+00    4.324046
    1597  14593 2016-08-01       alpha  2.59e-03    0.290026
    1598  14593 2016-08-01  mkt_excess  1.06e+00    4.363846
    1599  14593 2016-09-01       alpha  1.33e-03    0.146949
    1600  14593 2016-09-01  mkt_excess  1.14e+00    4.467923
    1601  14593 2016-10-01       alpha  1.68e-03    0.189550
    1602  14593 2016-10-01  mkt_excess  1.25e+00    4.594148
    1603  14593 2016-11-01       alpha  1.98e-03    0.220281
    1604  14593 2016-11-01  mkt_excess  1.19e+00    4.355693
    1605  14593 2016-12-01       alpha  1.50e-03    0.166467
    1606  14593 2016-12-01  mkt_excess  1.19e+00    4.396208
    1607  14593 2017-01-01       alpha  1.28e-03    0.143658
    1608  14593 2017-01-01  mkt_excess  1.15e+00    4.219418
    1609  14593 2017-02-01       alpha  9.95e-04    0.114607
    1610  14593 2017-02-01  mkt_excess  1.11e+00    4.140760
    1611  14593 2017-03-01       alpha  9.56e-04    0.111375
    1612  14593 2017-03-01  mkt_excess  1.08e+00    4.029756
    1613  14593 2017-04-01       alpha  1.10e-03    0.128183
    1614  14593 2017-04-01  mkt_excess  1.07e+00    3.993348
    1615  14593 2017-05-01       alpha  1.22e-04    0.013849
    1616  14593 2017-05-01  mkt_excess  1.15e+00    4.075136
    1617  14593 2017-06-01       alpha -7.20e-04   -0.081748
    1618  14593 2017-06-01  mkt_excess  1.17e+00    4.102695
    1619  14593 2017-07-01       alpha -1.22e-03   -0.138608
    1620  14593 2017-07-01  mkt_excess  1.18e+00    4.131966
    1621  14593 2017-08-01       alpha -8.12e-05   -0.009132
    1622  14593 2017-08-01  mkt_excess  1.14e+00    3.930696
    1623  14593 2017-09-01       alpha -9.21e-04   -0.102035
    1624  14593 2017-09-01  mkt_excess  1.12e+00    3.817548
    1625  14593 2017-10-01       alpha  2.20e-03    0.243474
    1626  14593 2017-10-01  mkt_excess  1.08e+00    3.677125
    1627  14593 2017-11-01       alpha  2.42e-03    0.266491
    1628  14593 2017-11-01  mkt_excess  1.08e+00    3.661942
    1629  14593 2017-12-01       alpha  3.70e-03    0.415963
    1630  14593 2017-12-01  mkt_excess  1.07e+00    3.742251
    1631  14593 2018-01-01       alpha  4.40e-03    0.541941
    1632  14593 2018-01-01  mkt_excess  1.19e+00    4.555876
    1633  14593 2018-02-01       alpha  8.12e-03    0.993562
    1634  14593 2018-02-01  mkt_excess  1.09e+00    4.181366
    1635  14593 2018-03-01       alpha  7.63e-03    0.947843
    1636  14593 2018-03-01  mkt_excess  1.15e+00    4.426068
    1637  14593 2018-04-01       alpha  7.52e-03    0.937185
    1638  14593 2018-04-01  mkt_excess  1.16e+00    4.452687
    1639  14593 2018-05-01       alpha  9.02e-03    1.100534
    1640  14593 2018-05-01  mkt_excess  1.19e+00    4.494371
    1641  14593 2018-06-01       alpha  1.10e-02    1.379193
    1642  14593 2018-06-01  mkt_excess  1.14e+00    4.425426
    1643  14593 2018-07-01       alpha  1.03e-02    1.309547
    1644  14593 2018-07-01  mkt_excess  1.07e+00    4.124077
    1645  14593 2018-08-01       alpha  9.33e-03    1.136218
    1646  14593 2018-08-01  mkt_excess  1.23e+00    4.549631
    1647  14593 2018-09-01       alpha  9.76e-03    1.213825
    1648  14593 2018-09-01  mkt_excess  1.28e+00    4.767675
    1649  14593 2018-10-01       alpha  1.11e-02    1.414535
    1650  14593 2018-10-01  mkt_excess  1.17e+00    4.633360
    1651  14593 2018-11-01       alpha  7.53e-03    0.868858
    1652  14593 2018-11-01  mkt_excess  1.12e+00    4.013224
    1653  14593 2018-12-01       alpha  7.45e-03    0.878949
    1654  14593 2018-12-01  mkt_excess  1.16e+00    4.547794
    1655  14593 2019-01-01       alpha  8.76e-03    1.030178
    1656  14593 2019-01-01  mkt_excess  1.06e+00    4.304883
    1657  14593 2019-02-01       alpha  8.74e-03    1.029038
    1658  14593 2019-02-01  mkt_excess  1.06e+00    4.277774
    1659  14593 2019-03-01       alpha  9.85e-03    1.145225
    1660  14593 2019-03-01  mkt_excess  1.06e+00    4.242019
    1661  14593 2019-04-01       alpha  8.22e-03    0.968336
    1662  14593 2019-04-01  mkt_excess  1.08e+00    4.410965
    1663  14593 2019-05-01       alpha  6.00e-03    0.714465
    1664  14593 2019-05-01  mkt_excess  1.13e+00    4.822380
    1665  14593 2019-06-01       alpha  6.55e-03    0.774400
    1666  14593 2019-06-01  mkt_excess  1.17e+00    5.078251
    1667  14593 2019-07-01       alpha  6.50e-03    0.764253
    1668  14593 2019-07-01  mkt_excess  1.19e+00    5.129633
    1669  14593 2019-08-01       alpha  6.34e-03    0.752112
    1670  14593 2019-08-01  mkt_excess  1.18e+00    5.082048
    1671  14593 2019-09-01       alpha  7.12e-03    0.837421
    1672  14593 2019-09-01  mkt_excess  1.18e+00    5.048557
    1673  14593 2019-10-01       alpha  7.79e-03    0.907100
    1674  14593 2019-10-01  mkt_excess  1.19e+00    5.015710
    1675  14593 2019-11-01       alpha  7.09e-03    0.831752
    1676  14593 2019-11-01  mkt_excess  1.18e+00    5.052735
    1677  14593 2019-12-01       alpha  9.32e-03    1.097002
    1678  14593 2019-12-01  mkt_excess  1.18e+00    5.114663
    1679  14593 2020-01-01       alpha  8.18e-03    0.972381
    1680  14593 2020-01-01  mkt_excess  1.23e+00    5.311415
    1681  14593 2020-02-01       alpha  7.39e-03    0.891442
    1682  14593 2020-02-01  mkt_excess  1.24e+00    5.559765
    1683  14593 2020-03-01       alpha  9.90e-03    1.194457
    1684  14593 2020-03-01  mkt_excess  1.11e+00    5.499063
    1685  14593 2020-04-01       alpha  1.00e-02    1.203168
    1686  14593 2020-04-01  mkt_excess  1.11e+00    5.914083
    1687  14593 2020-05-01       alpha  9.88e-03    1.183047
    1688  14593 2020-05-01  mkt_excess  1.11e+00    5.998562
    1689  14593 2020-06-01       alpha  1.21e-02    1.417923
    1690  14593 2020-06-01  mkt_excess  1.12e+00    5.903679
    1691  14593 2020-07-01       alpha  1.43e-02    1.652773
    1692  14593 2020-07-01  mkt_excess  1.16e+00    6.131715
    1693  14593 2020-08-01       alpha  1.57e-02    1.756130
    1694  14593 2020-08-01  mkt_excess  1.21e+00    6.254321
    1695  14593 2020-09-01       alpha  1.42e-02    1.566423
    1696  14593 2020-09-01  mkt_excess  1.24e+00    6.355675
    1697  14593 2020-10-01       alpha  1.35e-02    1.503461
    1698  14593 2020-10-01  mkt_excess  1.27e+00    6.388199
    1699  14593 2020-11-01       alpha  1.35e-02    1.476930
    1700  14593 2020-11-01  mkt_excess  1.20e+00    6.320238
    1701  14593 2020-12-01       alpha  1.61e-02    1.773756
    1702  14593 2020-12-01  mkt_excess  1.19e+00    6.337536
    1703  14593 2021-01-01       alpha  1.63e-02    1.782479
    1704  14593 2021-01-01  mkt_excess  1.18e+00    6.163689
    1705  14593 2021-02-01       alpha  1.46e-02    1.551972
    1706  14593 2021-02-01  mkt_excess  1.16e+00    5.900891
    1707  14593 2021-03-01       alpha  1.36e-02    1.450726
    1708  14593 2021-03-01  mkt_excess  1.14e+00    5.721745
    1709  14593 2021-04-01       alpha  1.66e-02    1.845781
    1710  14593 2021-04-01  mkt_excess  1.14e+00    6.025023
    1711  14593 2021-05-01       alpha  1.47e-02    1.632791
    1712  14593 2021-05-01  mkt_excess  1.14e+00    6.014156
    1713  14593 2021-06-01       alpha  1.66e-02    1.835932
    1714  14593 2021-06-01  mkt_excess  1.14e+00    6.013346
    1715  14593 2021-07-01       alpha  1.67e-02    1.857891
    1716  14593 2021-07-01  mkt_excess  1.13e+00    5.962971
    1717  14593 2021-08-01       alpha  1.66e-02    1.838199
    1718  14593 2021-08-01  mkt_excess  1.13e+00    5.961262
    1719  14593 2021-09-01       alpha  1.50e-02    1.672860
    1720  14593 2021-09-01  mkt_excess  1.15e+00    6.158511
    1721  14593 2021-10-01       alpha  1.44e-02    1.589162
    1722  14593 2021-10-01  mkt_excess  1.14e+00    6.131824
    1723  14593 2021-11-01       alpha  1.77e-02    1.961823
    1724  14593 2021-11-01  mkt_excess  1.14e+00    6.089669
    1725  14593 2021-12-01       alpha  1.79e-02    1.973001
    1726  14593 2021-12-01  mkt_excess  1.15e+00    6.106804
    1727  14593 2022-01-01       alpha  1.87e-02    2.072980
    1728  14593 2022-01-01  mkt_excess  1.12e+00    6.111050
    1729  14593 2022-02-01       alpha  1.66e-02    1.871232
    1730  14593 2022-02-01  mkt_excess  1.12e+00    6.166032
    1731  14593 2022-03-01       alpha  1.62e-02    1.820899
    1732  14593 2022-03-01  mkt_excess  1.13e+00    6.199589
    1733  14593 2022-04-01       alpha  1.65e-02    1.877743
    1734  14593 2022-04-01  mkt_excess  1.13e+00    6.493091
    1735  14593 2022-05-01       alpha  1.47e-02    1.661370
    1736  14593 2022-05-01  mkt_excess  1.14e+00    6.498262
    1737  14593 2022-06-01       alpha  1.60e-02    1.850025
    1738  14593 2022-06-01  mkt_excess  1.14e+00    6.780697
    1739  14593 2022-07-01       alpha  1.68e-02    1.920363
    1740  14593 2022-07-01  mkt_excess  1.17e+00    7.092289
    1741  14593 2022-08-01       alpha  1.52e-02    1.768069
    1742  14593 2022-08-01  mkt_excess  1.18e+00    7.287185
    1743  14593 2022-09-01       alpha  1.63e-02    1.949612
    1744  14593 2022-09-01  mkt_excess  1.21e+00    7.849496
    1745  14593 2022-10-01       alpha  1.54e-02    1.850280
    1746  14593 2022-10-01  mkt_excess  1.20e+00    7.966144
    1747  14593 2022-11-01       alpha  1.43e-02    1.684702
    1748  14593 2022-11-01  mkt_excess  1.18e+00    7.699466
    1749  14593 2022-12-01       alpha  1.38e-02    1.618097
    1750  14593 2022-12-01  mkt_excess  1.21e+00    7.944008
    1751  14593 2023-01-01       alpha  1.53e-02    1.832537
    1752  14593 2023-01-01  mkt_excess  1.24e+00    8.302184
    1753  14593 2023-02-01       alpha  1.42e-02    1.725005
    1754  14593 2023-02-01  mkt_excess  1.25e+00    8.541923
    1755  14593 2023-03-01       alpha  1.61e-02    1.941822
    1756  14593 2023-03-01  mkt_excess  1.25e+00    8.463546
    1757  14593 2023-04-01       alpha  1.67e-02    2.023951
    1758  14593 2023-04-01  mkt_excess  1.25e+00    8.478389
    1759  14593 2023-05-01       alpha  1.58e-02    1.937341
    1760  14593 2023-05-01  mkt_excess  1.24e+00    8.534658
    1761  14593 2023-06-01       alpha  1.62e-02    1.995175
    1762  14593 2023-06-01  mkt_excess  1.24e+00    8.608952
    1763  14593 2023-07-01       alpha  1.60e-02    1.954359
    1764  14593 2023-07-01  mkt_excess  1.24e+00    8.566928
    1765  14593 2023-08-01       alpha  1.32e-02    1.691119
    1766  14593 2023-08-01  mkt_excess  1.22e+00    8.864394
    1767  14593 2023-09-01       alpha  1.28e-02    1.638559
    1768  14593 2023-09-01  mkt_excess  1.23e+00    9.013599
    1769  14593 2023-10-01       alpha  1.21e-02    1.558393
    1770  14593 2023-10-01  mkt_excess  1.25e+00    9.061070
    1771  14593 2023-11-01       alpha  1.55e-02    2.261078
    1772  14593 2023-11-01  mkt_excess  1.26e+00   10.525820
    1773  14593 2023-12-01       alpha  1.48e-02    2.123351
    1774  14593 2023-12-01  mkt_excess  1.23e+00    9.943140
    1775  14593 2024-01-01       alpha  1.44e-02    2.070730
    1776  14593 2024-01-01  mkt_excess  1.26e+00   10.003956
    1777  14593 2024-02-01       alpha  1.32e-02    1.840290
    1778  14593 2024-02-01  mkt_excess  1.24e+00    9.584442
    1779  14593 2024-03-01       alpha  1.05e-02    1.431643
    1780  14593 2024-03-01  mkt_excess  1.23e+00    9.363791
    1781  14593 2024-04-01       alpha  1.12e-02    1.540171
    1782  14593 2024-04-01  mkt_excess  1.22e+00    9.317853
    1783  14593 2024-05-01       alpha  1.33e-02    1.806914
    1784  14593 2024-05-01  mkt_excess  1.21e+00    9.059852
    1785  14593 2024-06-01       alpha  1.36e-02    1.845423
    1786  14593 2024-06-01  mkt_excess  1.20e+00    8.907091
    1787  14593 2024-07-01       alpha  1.32e-02    1.798701
    1788  14593 2024-07-01  mkt_excess  1.20e+00    8.946842
    1789  14593 2024-08-01       alpha  1.31e-02    1.784305
    1790  14593 2024-08-01  mkt_excess  1.20e+00    8.909577
    1791  14593 2024-09-01       alpha  1.21e-02    1.652972
    1792  14593 2024-09-01  mkt_excess  1.20e+00    8.934535
    1793  14593 2024-10-01       alpha  1.03e-02    1.429891
    1794  14593 2024-10-01  mkt_excess  1.20e+00    9.054173
    1795  14593 2024-11-01       alpha  9.52e-03    1.310309
    1796  14593 2024-11-01  mkt_excess  1.18e+00    8.962577
    1797  14593 2024-12-01       alpha  1.02e-02    1.395387
    1798  14593 2024-12-01  mkt_excess  1.16e+00    8.737073
    1799  17778 1980-10-01       alpha  3.05e-02    2.541160
    1800  17778 1980-10-01  mkt_excess  9.93e-01    3.655803
    1801  17778 1980-11-01       alpha  2.93e-02    2.452425
    1802  17778 1980-11-01  mkt_excess  9.16e-01    3.511040
    1803  17778 1980-12-01       alpha  2.88e-02    2.462220
    1804  17778 1980-12-01  mkt_excess  9.28e-01    3.639610
    1805  17778 1981-01-01       alpha  2.93e-02    2.568173
    1806  17778 1981-01-01  mkt_excess  9.14e-01    3.677389
    1807  17778 1981-02-01       alpha  3.07e-02    2.723027
    1808  17778 1981-02-01  mkt_excess  9.15e-01    3.685813
    1809  17778 1981-03-01       alpha  2.93e-02    2.620065
    1810  17778 1981-03-01  mkt_excess  8.91e-01    3.608465
    1811  17778 1981-04-01       alpha  3.04e-02    2.755714
    1812  17778 1981-04-01  mkt_excess  8.79e-01    3.590706
    1813  17778 1981-05-01       alpha  2.97e-02    2.746469
    1814  17778 1981-05-01  mkt_excess  8.80e-01    3.624883
    1815  17778 1981-06-01       alpha  2.89e-02    2.709141
    1816  17778 1981-06-01  mkt_excess  8.92e-01    3.711940
    1817  17778 1981-07-01       alpha  2.75e-02    2.605742
    1818  17778 1981-07-01  mkt_excess  9.06e-01    3.781060
    1819  17778 1981-08-01       alpha  2.79e-02    2.701608
    1820  17778 1981-08-01  mkt_excess  8.91e-01    3.842909
    1821  17778 1981-09-01       alpha  2.75e-02    2.712722
    1822  17778 1981-09-01  mkt_excess  9.05e-01    4.027891
    1823  17778 1981-10-01       alpha  2.77e-02    2.774487
    1824  17778 1981-10-01  mkt_excess  9.09e-01    4.119009
    1825  17778 1981-11-01       alpha  2.69e-02    2.727920
    1826  17778 1981-11-01  mkt_excess  9.28e-01    4.273432
    1827  17778 1981-12-01       alpha  2.51e-02    2.562604
    1828  17778 1981-12-01  mkt_excess  9.13e-01    4.197029
    1829  17778 1982-01-01       alpha  2.27e-02    2.300382
    1830  17778 1982-01-01  mkt_excess  9.56e-01    4.355770
    1831  17778 1982-02-01       alpha  2.42e-02    2.452486
    1832  17778 1982-02-01  mkt_excess  9.32e-01    4.308960
    1833  17778 1982-03-01       alpha  2.17e-02    2.193356
    1834  17778 1982-03-01  mkt_excess  9.52e-01    4.388809
    1835  17778 1982-04-01       alpha  2.23e-02    2.256152
    1836  17778 1982-04-01  mkt_excess  9.52e-01    4.411725
    1837  17778 1982-05-01       alpha  2.13e-02    2.170064
    1838  17778 1982-05-01  mkt_excess  9.60e-01    4.491005
    1839  17778 1982-06-01       alpha  2.22e-02    2.280159
    1840  17778 1982-06-01  mkt_excess  9.90e-01    4.645943
    1841  17778 1982-07-01       alpha  2.16e-02    2.212492
    1842  17778 1982-07-01  mkt_excess  1.00e+00    4.693673
    1843  17778 1982-08-01       alpha  1.96e-02    1.967100
    1844  17778 1982-08-01  mkt_excess  9.06e-01    4.363580
    1845  17778 1982-09-01       alpha  2.08e-02    2.062423
    1846  17778 1982-09-01  mkt_excess  9.14e-01    4.351638
    1847  17778 1982-10-01       alpha  1.96e-02    1.945302
    1848  17778 1982-10-01  mkt_excess  9.22e-01    4.563568
    1849  17778 1982-11-01       alpha  2.00e-02    1.975902
    1850  17778 1982-11-01  mkt_excess  9.31e-01    4.599359
    1851  17778 1982-12-01       alpha  1.94e-02    1.939676
    1852  17778 1982-12-01  mkt_excess  9.33e-01    4.657188
    1853  17778 1983-01-01       alpha  1.83e-02    1.808455
    1854  17778 1983-01-01  mkt_excess  9.18e-01    4.497370
    1855  17778 1983-02-01       alpha  1.91e-02    1.873117
    1856  17778 1983-02-01  mkt_excess  9.22e-01    4.506816
    1857  17778 1983-03-01       alpha  2.09e-02    2.018795
    1858  17778 1983-03-01  mkt_excess  9.39e-01    4.514280
    1859  17778 1983-04-01       alpha  2.03e-02    1.948102
    1860  17778 1983-04-01  mkt_excess  9.25e-01    4.392371
    1861  17778 1983-05-01       alpha  1.82e-02    1.777130
    1862  17778 1983-05-01  mkt_excess  9.17e-01    4.424896
    1863  17778 1983-06-01       alpha  1.69e-02    1.610775
    1864  17778 1983-06-01  mkt_excess  8.90e-01    4.207939
    1865  17778 1983-07-01       alpha  1.89e-02    1.813248
    1866  17778 1983-07-01  mkt_excess  8.94e-01    4.244232
    1867  17778 1983-08-01       alpha  2.00e-02    1.918824
    1868  17778 1983-08-01  mkt_excess  8.94e-01    4.212641
    1869  17778 1983-09-01       alpha  2.31e-02    2.131762
    1870  17778 1983-09-01  mkt_excess  8.96e-01    4.061865
    1871  17778 1983-10-01       alpha  2.08e-02    1.955155
    1872  17778 1983-10-01  mkt_excess  1.02e+00    4.473466
    1873  17778 1983-11-01       alpha  2.30e-02    2.226437
    1874  17778 1983-11-01  mkt_excess  1.04e+00    4.706428
    1875  17778 1983-12-01       alpha  2.42e-02    2.397784
    1876  17778 1983-12-01  mkt_excess  1.05e+00    4.851066
    1877  17778 1984-01-01       alpha  2.51e-02    2.497696
    1878  17778 1984-01-01  mkt_excess  1.06e+00    4.939526
    1879  17778 1984-02-01       alpha  2.43e-02    2.427674
    1880  17778 1984-02-01  mkt_excess  1.08e+00    5.029219
    1881  17778 1984-03-01       alpha  2.24e-02    2.232052
    1882  17778 1984-03-01  mkt_excess  1.05e+00    4.858920
    1883  17778 1984-04-01       alpha  2.20e-02    2.189559
    1884  17778 1984-04-01  mkt_excess  1.06e+00    4.866108
    1885  17778 1984-05-01       alpha  2.23e-02    2.218259
    1886  17778 1984-05-01  mkt_excess  1.04e+00    4.823191
    1887  17778 1984-06-01       alpha  2.02e-02    2.009058
    1888  17778 1984-06-01  mkt_excess  1.01e+00    4.688530
    1889  17778 1984-07-01       alpha  1.46e-02    1.731652
    1890  17778 1984-07-01  mkt_excess  1.00e+00    5.559370
    1891  17778 1984-08-01       alpha  1.26e-02    1.487106
    1892  17778 1984-08-01  mkt_excess  9.21e-01    5.212498
    1893  17778 1984-09-01       alpha  1.33e-02    1.570979
    1894  17778 1984-09-01  mkt_excess  9.18e-01    5.211111
    1895  17778 1984-10-01       alpha  1.54e-02    1.873179
    1896  17778 1984-10-01  mkt_excess  8.40e-01    4.792704
    1897  17778 1984-11-01       alpha  1.42e-02    1.738219
    1898  17778 1984-11-01  mkt_excess  8.23e-01    4.691929
    1899  17778 1984-12-01       alpha  1.36e-02    1.667960
    1900  17778 1984-12-01  mkt_excess  8.19e-01    4.655850
    1901  17778 1985-01-01       alpha  1.41e-02    1.724280
    1902  17778 1985-01-01  mkt_excess  8.33e-01    4.795527
    1903  17778 1985-02-01       alpha  1.49e-02    1.819938
    1904  17778 1985-02-01  mkt_excess  8.34e-01    4.782060
    1905  17778 1985-03-01       alpha  2.19e-02    2.558927
    1906  17778 1985-03-01  mkt_excess  6.43e-01    3.310773
    1907  17778 1985-04-01       alpha  2.14e-02    2.513513
    1908  17778 1985-04-01  mkt_excess  6.41e-01    3.286108
    1909  17778 1985-05-01       alpha  1.96e-02    2.310702
    1910  17778 1985-05-01  mkt_excess  5.88e-01    3.030524
    1911  17778 1985-06-01       alpha  2.32e-02    2.721014
    1912  17778 1985-06-01  mkt_excess  6.18e-01    3.157544
    1913  17778 1985-07-01       alpha  2.16e-02    2.550560
    1914  17778 1985-07-01  mkt_excess  5.89e-01    2.973256
    1915  17778 1985-08-01       alpha  1.96e-02    2.329809
    1916  17778 1985-08-01  mkt_excess  5.88e-01    2.976228
    1917  17778 1985-09-01       alpha  2.03e-02    2.408227
    1918  17778 1985-09-01  mkt_excess  5.85e-01    2.988454
    1919  17778 1985-10-01       alpha  2.29e-02    2.511432
    1920  17778 1985-10-01  mkt_excess  6.53e-01    3.092699
    1921  17778 1985-11-01       alpha  2.26e-02    2.463049
    1922  17778 1985-11-01  mkt_excess  6.55e-01    3.019406
    1923  17778 1985-12-01       alpha  2.14e-02    2.280759
    1924  17778 1985-12-01  mkt_excess  6.04e-01    2.718302
    1925  17778 1986-01-01       alpha  1.91e-02    1.989821
    1926  17778 1986-01-01  mkt_excess  6.11e-01    2.652543
    1927  17778 1986-02-01       alpha  1.97e-02    1.994874
    1928  17778 1986-02-01  mkt_excess  7.06e-01    3.053257
    1929  17778 1986-03-01       alpha  2.16e-02    2.187086
    1930  17778 1986-03-01  mkt_excess  7.52e-01    3.261053
    1931  17778 1986-04-01       alpha  1.91e-02    1.920401
    1932  17778 1986-04-01  mkt_excess  7.81e-01    3.364433
    1933  17778 1986-05-01       alpha  1.84e-02    1.829651
    1934  17778 1986-05-01  mkt_excess  7.53e-01    3.237519
    1935  17778 1986-06-01       alpha  1.92e-02    1.914415
    1936  17778 1986-06-01  mkt_excess  7.42e-01    3.184108
    1937  17778 1986-07-01       alpha  1.99e-02    1.999641
    1938  17778 1986-07-01  mkt_excess  7.45e-01    3.286093
    1939  17778 1986-08-01       alpha  1.89e-02    1.877803
    1940  17778 1986-08-01  mkt_excess  7.39e-01    3.203420
    1941  17778 1986-09-01       alpha  1.81e-02    1.797461
    1942  17778 1986-09-01  mkt_excess  7.68e-01    3.355726
    1943  17778 1986-10-01       alpha  1.83e-02    1.809213
    1944  17778 1986-10-01  mkt_excess  7.71e-01    3.359928
    1945  17778 1986-11-01       alpha  1.57e-02    1.558884
    1946  17778 1986-11-01  mkt_excess  7.48e-01    3.262999
    1947  17778 1986-12-01       alpha  1.71e-02    1.704647
    1948  17778 1986-12-01  mkt_excess  7.19e-01    3.137819
    1949  17778 1987-01-01       alpha  1.88e-02    1.876129
    1950  17778 1987-01-01  mkt_excess  6.78e-01    3.140518
    1951  17778 1987-02-01       alpha  1.87e-02    1.843919
    1952  17778 1987-02-01  mkt_excess  6.99e-01    3.182634
    1953  17778 1987-03-01       alpha  2.06e-02    2.043855
    1954  17778 1987-03-01  mkt_excess  6.77e-01    3.101794
    1955  17778 1987-04-01       alpha  1.95e-02    1.934850
    1956  17778 1987-04-01  mkt_excess  6.90e-01    3.160462
    1957  17778 1987-05-01       alpha  1.95e-02    1.926234
    1958  17778 1987-05-01  mkt_excess  6.86e-01    3.106426
    1959  17778 1987-06-01       alpha  1.96e-02    1.927047
    1960  17778 1987-06-01  mkt_excess  6.72e-01    3.023251
    1961  17778 1987-07-01       alpha  2.20e-02    2.125366
    1962  17778 1987-07-01  mkt_excess  6.66e-01    2.958262
    1963  17778 1987-08-01       alpha  2.17e-02    2.122184
    1964  17778 1987-08-01  mkt_excess  7.36e-01    3.165618
    1965  17778 1987-09-01       alpha  2.12e-02    2.106509
    1966  17778 1987-09-01  mkt_excess  7.18e-01    3.142140
    1967  17778 1987-10-01       alpha  1.89e-02    1.950755
    1968  17778 1987-10-01  mkt_excess  8.26e-01    4.349961
    1969  17778 1987-11-01       alpha  1.64e-02    1.687178
    1970  17778 1987-11-01  mkt_excess  8.57e-01    4.566097
    1971  17778 1987-12-01       alpha  1.40e-02    1.445518
    1972  17778 1987-12-01  mkt_excess  8.34e-01    4.538684
    1973  17778 1988-01-01       alpha  1.46e-02    1.515977
    1974  17778 1988-01-01  mkt_excess  8.40e-01    4.601504
    1975  17778 1988-02-01       alpha  1.36e-02    1.414239
    1976  17778 1988-02-01  mkt_excess  8.30e-01    4.577817
    1977  17778 1988-03-01       alpha  1.33e-02    1.401092
    1978  17778 1988-03-01  mkt_excess  7.99e-01    4.457162
    1979  17778 1988-04-01       alpha  1.56e-02    1.642161
    1980  17778 1988-04-01  mkt_excess  8.24e-01    4.534106
    1981  17778 1988-05-01       alpha  1.61e-02    1.704580
    1982  17778 1988-05-01  mkt_excess  8.23e-01    4.527279
    1983  17778 1988-06-01       alpha  1.81e-02    1.954267
    1984  17778 1988-06-01  mkt_excess  8.42e-01    4.765605
    1985  17778 1988-07-01       alpha  1.78e-02    1.924193
    1986  17778 1988-07-01  mkt_excess  8.52e-01    4.806520
    1987  17778 1988-08-01       alpha  1.68e-02    1.832481
    1988  17778 1988-08-01  mkt_excess  8.56e-01    4.874900
    1989  17778 1988-09-01       alpha  1.48e-02    1.696659
    1990  17778 1988-09-01  mkt_excess  8.62e-01    5.178616
    1991  17778 1988-10-01       alpha  1.29e-02    1.485233
    1992  17778 1988-10-01  mkt_excess  8.76e-01    5.250872
    1993  17778 1988-11-01       alpha  1.28e-02    1.473987
    1994  17778 1988-11-01  mkt_excess  8.76e-01    5.263447
    1995  17778 1988-12-01       alpha  1.29e-02    1.481083
    1996  17778 1988-12-01  mkt_excess  8.70e-01    5.215961
    1997  17778 1989-01-01       alpha  1.25e-02    1.425508
    1998  17778 1989-01-01  mkt_excess  8.63e-01    5.205931
    1999  17778 1989-02-01       alpha  1.20e-02    1.363402
    2000  17778 1989-02-01  mkt_excess  8.66e-01    5.177026
    2001  17778 1989-03-01       alpha  1.32e-02    1.506428
    2002  17778 1989-03-01  mkt_excess  8.66e-01    5.207310
    2003  17778 1989-04-01       alpha  1.64e-02    1.734368
    2004  17778 1989-04-01  mkt_excess  9.10e-01    5.079790
    2005  17778 1989-05-01       alpha  1.47e-02    1.547322
    2006  17778 1989-05-01  mkt_excess  9.33e-01    5.160355
    2007  17778 1989-06-01       alpha  1.58e-02    1.686483
    2008  17778 1989-06-01  mkt_excess  9.33e-01    5.201690
    2009  17778 1989-07-01       alpha  1.68e-02    1.768547
    2010  17778 1989-07-01  mkt_excess  9.48e-01    5.301925
    2011  17778 1989-08-01       alpha  1.81e-02    1.933400
    2012  17778 1989-08-01  mkt_excess  9.99e-01    5.500697
    2013  17778 1989-09-01       alpha  1.93e-02    2.056805
    2014  17778 1989-09-01  mkt_excess  9.90e-01    5.415910
    2015  17778 1989-10-01       alpha  1.97e-02    2.103527
    2016  17778 1989-10-01  mkt_excess  9.88e-01    5.443419
    2017  17778 1989-11-01       alpha  1.91e-02    2.015487
    2018  17778 1989-11-01  mkt_excess  9.84e-01    5.369268
    2019  17778 1989-12-01       alpha  2.01e-02    2.130633
    2020  17778 1989-12-01  mkt_excess  9.87e-01    5.401421
    2021  17778 1990-01-01       alpha  1.83e-02    1.939609
    2022  17778 1990-01-01  mkt_excess  1.04e+00    5.675358
    2023  17778 1990-02-01       alpha  1.67e-02    1.760648
    2024  17778 1990-02-01  mkt_excess  1.04e+00    5.631687
    2025  17778 1990-03-01       alpha  1.14e-02    1.258857
    2026  17778 1990-03-01  mkt_excess  1.05e+00    6.015201
    2027  17778 1990-04-01       alpha  1.16e-02    1.285824
    2028  17778 1990-04-01  mkt_excess  1.05e+00    6.021160
    2029  17778 1990-05-01       alpha  1.20e-02    1.340706
    2030  17778 1990-05-01  mkt_excess  1.05e+00    6.120321
    2031  17778 1990-06-01       alpha  9.99e-03    1.147171
    2032  17778 1990-06-01  mkt_excess  1.04e+00    6.291224
    2033  17778 1990-07-01       alpha  1.00e-02    1.151631
    2034  17778 1990-07-01  mkt_excess  1.05e+00    6.311983
    2035  17778 1990-08-01       alpha  1.07e-02    1.244578
    2036  17778 1990-08-01  mkt_excess  1.04e+00    6.513758
    2037  17778 1990-09-01       alpha  9.25e-03    1.074175
    2038  17778 1990-09-01  mkt_excess  1.07e+00    6.730170
    2039  17778 1990-10-01       alpha  6.30e-03    0.805442
    2040  17778 1990-10-01  mkt_excess  1.02e+00    7.075908
    2041  17778 1990-11-01       alpha  7.29e-03    0.944836
    2042  17778 1990-11-01  mkt_excess  1.05e+00    7.333109
    2043  17778 1990-12-01       alpha  9.28e-03    1.243811
    2044  17778 1990-12-01  mkt_excess  1.07e+00    7.732848
    2045  17778 1991-01-01       alpha  1.18e-02    1.619412
    2046  17778 1991-01-01  mkt_excess  1.08e+00    8.071705
    2047  17778 1991-02-01       alpha  9.83e-03    1.422859
    2048  17778 1991-02-01  mkt_excess  1.03e+00    8.100633
    2049  17778 1991-03-01       alpha  8.08e-03    1.177644
    2050  17778 1991-03-01  mkt_excess  1.01e+00    7.944479
    2051  17778 1991-04-01       alpha  9.28e-03    1.371613
    2052  17778 1991-04-01  mkt_excess  1.00e+00    7.998234
    2053  17778 1991-05-01       alpha  1.09e-02    1.640011
    2054  17778 1991-05-01  mkt_excess  1.02e+00    8.316638
    2055  17778 1991-06-01       alpha  1.06e-02    1.598327
    2056  17778 1991-06-01  mkt_excess  1.02e+00    8.397232
    2057  17778 1991-07-01       alpha  9.92e-03    1.482765
    2058  17778 1991-07-01  mkt_excess  1.02e+00    8.187974
    2059  17778 1991-08-01       alpha  1.05e-02    1.587056
    2060  17778 1991-08-01  mkt_excess  1.03e+00    8.311702
    2061  17778 1991-09-01       alpha  1.12e-02    1.683267
    2062  17778 1991-09-01  mkt_excess  1.02e+00    8.014468
    2063  17778 1991-10-01       alpha  9.65e-03    1.442730
    2064  17778 1991-10-01  mkt_excess  1.01e+00    7.818965
    2065  17778 1991-11-01       alpha  1.09e-02    1.660734
    2066  17778 1991-11-01  mkt_excess  1.01e+00    8.029179
    2067  17778 1991-12-01       alpha  9.55e-03    1.451865
    2068  17778 1991-12-01  mkt_excess  9.95e-01    8.160495
    2069  17778 1992-01-01       alpha  9.54e-03    1.460830
    2070  17778 1992-01-01  mkt_excess  1.03e+00    8.089673
    2071  17778 1992-02-01       alpha  8.53e-03    1.304637
    2072  17778 1992-02-01  mkt_excess  1.02e+00    7.971245
    2073  17778 1992-03-01       alpha  8.95e-03    1.364379
    2074  17778 1992-03-01  mkt_excess  1.01e+00    7.883093
    2075  17778 1992-04-01       alpha  9.45e-03    1.443968
    2076  17778 1992-04-01  mkt_excess  1.00e+00    7.854063
    2077  17778 1992-05-01       alpha  9.24e-03    1.410584
    2078  17778 1992-05-01  mkt_excess  1.00e+00    7.846494
    2079  17778 1992-06-01       alpha  1.01e-02    1.543993
    2080  17778 1992-06-01  mkt_excess  1.01e+00    7.853322
    2081  17778 1992-07-01       alpha  8.56e-03    1.335746
    2082  17778 1992-07-01  mkt_excess  9.84e-01    7.842847
    2083  17778 1992-08-01       alpha  8.70e-03    1.363404
    2084  17778 1992-08-01  mkt_excess  9.98e-01    7.963846
    2085  17778 1992-09-01       alpha  7.61e-03    1.212601
    2086  17778 1992-09-01  mkt_excess  1.01e+00    8.197985
    2087  17778 1992-10-01       alpha  7.11e-03    1.118311
    2088  17778 1992-10-01  mkt_excess  1.03e+00    6.677906
    2089  17778 1992-11-01       alpha  9.65e-03    1.488224
    2090  17778 1992-11-01  mkt_excess  1.01e+00    6.236807
    2091  17778 1992-12-01       alpha  1.15e-02    1.774551
    2092  17778 1992-12-01  mkt_excess  1.05e+00    6.420150
    2093  17778 1993-01-01       alpha  1.20e-02    1.868879
    2094  17778 1993-01-01  mkt_excess  1.07e+00    6.477144
    2095  17778 1993-02-01       alpha  1.26e-02    1.979450
    2096  17778 1993-02-01  mkt_excess  1.08e+00    6.504948
    2097  17778 1993-03-01       alpha  1.05e-02    1.698739
    2098  17778 1993-03-01  mkt_excess  1.11e+00    6.889776
    2099  17778 1993-04-01       alpha  9.43e-03    1.566392
    2100  17778 1993-04-01  mkt_excess  1.10e+00    7.099347
    2101  17778 1993-05-01       alpha  1.10e-02    1.701493
    2102  17778 1993-05-01  mkt_excess  1.14e+00    6.844050
    2103  17778 1993-06-01       alpha  1.14e-02    1.760558
    2104  17778 1993-06-01  mkt_excess  1.15e+00    6.817000
    2105  17778 1993-07-01       alpha  1.11e-02    1.715831
    2106  17778 1993-07-01  mkt_excess  1.16e+00    6.879025
    2107  17778 1993-08-01       alpha  1.13e-02    1.731223
    2108  17778 1993-08-01  mkt_excess  1.18e+00    6.941816
    2109  17778 1993-09-01       alpha  9.44e-03    1.450841
    2110  17778 1993-09-01  mkt_excess  1.17e+00    6.845099
    2111  17778 1993-10-01       alpha  1.03e-02    1.589316
    2112  17778 1993-10-01  mkt_excess  1.17e+00    6.910758
    2113  17778 1993-11-01       alpha  1.03e-02    1.599315
    2114  17778 1993-11-01  mkt_excess  1.17e+00    6.894495
    2115  17778 1993-12-01       alpha  9.52e-03    1.442036
    2116  17778 1993-12-01  mkt_excess  1.16e+00    6.713871
    2117  17778 1994-01-01       alpha  9.19e-03    1.388237
    2118  17778 1994-01-01  mkt_excess  1.17e+00    6.647496
    2119  17778 1994-02-01       alpha  9.22e-03    1.394077
    2120  17778 1994-02-01  mkt_excess  1.17e+00    6.660219
    2121  17778 1994-03-01       alpha  1.06e-02    1.575791
    2122  17778 1994-03-01  mkt_excess  1.12e+00    6.379844
    2123  17778 1994-04-01       alpha  7.53e-03    1.324438
    2124  17778 1994-04-01  mkt_excess  1.03e+00    6.821604
    2125  17778 1994-05-01       alpha  7.63e-03    1.346834
    2126  17778 1994-05-01  mkt_excess  1.04e+00    6.857165
    2127  17778 1994-06-01       alpha  7.62e-03    1.347381
    2128  17778 1994-06-01  mkt_excess  1.04e+00    6.871012
    2129  17778 1994-07-01       alpha  9.69e-03    1.527105
    2130  17778 1994-07-01  mkt_excess  1.05e+00    6.028583
    2131  17778 1994-08-01       alpha  7.99e-03    1.235073
    2132  17778 1994-08-01  mkt_excess  1.01e+00    5.734651
    2133  17778 1994-09-01       alpha  6.76e-03    1.067414
    2134  17778 1994-09-01  mkt_excess  1.02e+00    5.929039
    2135  17778 1994-10-01       alpha  7.28e-03    1.138370
    2136  17778 1994-10-01  mkt_excess  1.03e+00    5.889259
    2137  17778 1994-11-01       alpha  9.02e-03    1.421019
    2138  17778 1994-11-01  mkt_excess  1.01e+00    5.875647
    2139  17778 1994-12-01       alpha  8.16e-03    1.284701
    2140  17778 1994-12-01  mkt_excess  1.00e+00    5.848424
    2141  17778 1995-01-01       alpha  1.27e-02    1.832458
    2142  17778 1995-01-01  mkt_excess  9.46e-01    4.885592
    2143  17778 1995-02-01       alpha  1.16e-02    1.588363
    2144  17778 1995-02-01  mkt_excess  8.92e-01    4.405146
    2145  17778 1995-03-01       alpha  1.29e-02    1.824838
    2146  17778 1995-03-01  mkt_excess  9.04e-01    4.607467
    2147  17778 1995-04-01       alpha  1.15e-02    1.586536
    2148  17778 1995-04-01  mkt_excess  8.93e-01    4.418263
    2149  17778 1995-05-01       alpha  1.19e-02    1.647442
    2150  17778 1995-05-01  mkt_excess  9.25e-01    4.415251
    2151  17778 1995-06-01       alpha  1.18e-02    1.626626
    2152  17778 1995-06-01  mkt_excess  9.22e-01    4.404803
    2153  17778 1995-07-01       alpha  1.24e-02    1.708606
    2154  17778 1995-07-01  mkt_excess  9.13e-01    4.378445
    2155  17778 1995-08-01       alpha  1.30e-02    1.752581
    2156  17778 1995-08-01  mkt_excess  8.89e-01    3.873062
    2157  17778 1995-09-01       alpha  1.57e-02    2.031770
    2158  17778 1995-09-01  mkt_excess  8.70e-01    3.549942
    2159  17778 1995-10-01       alpha  1.55e-02    1.997942
    2160  17778 1995-10-01  mkt_excess  8.76e-01    3.566842
    2161  17778 1995-11-01       alpha  1.56e-02    2.018987
    2162  17778 1995-11-01  mkt_excess  8.91e-01    3.559628
    2163  17778 1995-12-01       alpha  1.54e-02    1.999975
    2164  17778 1995-12-01  mkt_excess  8.89e-01    3.543744
    2165  17778 1996-01-01       alpha  1.43e-02    1.868589
    2166  17778 1996-01-01  mkt_excess  8.42e-01    3.319996
    2167  17778 1996-02-01       alpha  1.56e-02    2.010765
    2168  17778 1996-02-01  mkt_excess  8.34e-01    3.097603
    2169  17778 1996-03-01       alpha  1.50e-02    1.926999
    2170  17778 1996-03-01  mkt_excess  8.55e-01    3.143189
    2171  17778 1996-04-01       alpha  1.45e-02    1.834731
    2172  17778 1996-04-01  mkt_excess  8.41e-01    3.075399
    2173  17778 1996-05-01       alpha  1.24e-02    1.519036
    2174  17778 1996-05-01  mkt_excess  7.82e-01    2.736383
    2175  17778 1996-06-01       alpha  1.29e-02    1.566983
    2176  17778 1996-06-01  mkt_excess  7.57e-01    2.557912
    2177  17778 1996-07-01       alpha  1.43e-02    1.771127
    2178  17778 1996-07-01  mkt_excess  7.32e-01    2.580051
    2179  17778 1996-08-01       alpha  1.40e-02    1.736908
    2180  17778 1996-08-01  mkt_excess  7.23e-01    2.548779
    2181  17778 1996-09-01       alpha  1.37e-02    1.676317
    2182  17778 1996-09-01  mkt_excess  7.01e-01    2.497224
    2183  17778 1996-10-01       alpha  1.44e-02    1.778713
    2184  17778 1996-10-01  mkt_excess  7.06e-01    2.537140
    2185  17778 1996-11-01       alpha  1.46e-02    1.768497
    2186  17778 1996-11-01  mkt_excess  6.47e-01    2.320760
    2187  17778 1996-12-01       alpha  1.50e-02    1.838129
    2188  17778 1996-12-01  mkt_excess  6.47e-01    2.078279
    2189  17778 1997-01-01       alpha  1.56e-02    1.888712
    2190  17778 1997-01-01  mkt_excess  6.00e-01    1.965161
    2191  17778 1997-02-01       alpha  1.63e-02    1.988967
    2192  17778 1997-02-01  mkt_excess  6.00e-01    1.978347
    2193  17778 1997-03-01       alpha  1.66e-02    2.043677
    2194  17778 1997-03-01  mkt_excess  5.77e-01    1.954656
    2195  17778 1997-04-01       alpha  1.68e-02    2.054090
    2196  17778 1997-04-01  mkt_excess  5.81e-01    1.992643
    2197  17778 1997-05-01       alpha  1.75e-02    2.099950
    2198  17778 1997-05-01  mkt_excess  6.66e-01    2.349636
    2199  17778 1997-06-01       alpha  1.77e-02    2.086244
    2200  17778 1997-06-01  mkt_excess  7.05e-01    2.466384
    2201  17778 1997-07-01       alpha  1.78e-02    2.065486
    2202  17778 1997-07-01  mkt_excess  6.20e-01    2.215127
    2203  17778 1997-08-01       alpha  1.55e-02    1.771355
    2204  17778 1997-08-01  mkt_excess  7.11e-01    2.518305
    2205  17778 1997-09-01       alpha  1.56e-02    1.768513
    2206  17778 1997-09-01  mkt_excess  7.28e-01    2.621265
    2207  17778 1997-10-01       alpha  1.54e-02    1.772929
    2208  17778 1997-10-01  mkt_excess  7.43e-01    2.740474
    2209  17778 1997-11-01       alpha  1.45e-02    1.697831
    2210  17778 1997-11-01  mkt_excess  6.94e-01    2.588874
    2211  17778 1997-12-01       alpha  1.32e-02    1.564238
    2212  17778 1997-12-01  mkt_excess  6.89e-01    2.595638
    2213  17778 1998-01-01       alpha  1.45e-02    1.696140
    2214  17778 1998-01-01  mkt_excess  6.76e-01    2.514982
    2215  17778 1998-02-01       alpha  1.44e-02    1.664359
    2216  17778 1998-02-01  mkt_excess  7.23e-01    2.762967
    2217  17778 1998-03-01       alpha  1.59e-02    1.749088
    2218  17778 1998-03-01  mkt_excess  8.16e-01    3.006823
    2219  17778 1998-04-01       alpha  1.59e-02    1.724459
    2220  17778 1998-04-01  mkt_excess  8.16e-01    2.955552
    2221  17778 1998-05-01       alpha  1.49e-02    1.715366
    2222  17778 1998-05-01  mkt_excess  7.48e-01    2.868150
    2223  17778 1998-06-01       alpha  1.56e-02    1.767215
    2224  17778 1998-06-01  mkt_excess  7.70e-01    2.934677
    2225  17778 1998-07-01       alpha  1.25e-02    1.395110
    2226  17778 1998-07-01  mkt_excess  8.43e-01    3.175354
    2227  17778 1998-08-01       alpha  1.12e-02    1.314305
    2228  17778 1998-08-01  mkt_excess  8.64e-01    3.988318
    2229  17778 1998-09-01       alpha  1.14e-02    1.322504
    2230  17778 1998-09-01  mkt_excess  8.08e-01    3.760181
    2231  17778 1998-10-01       alpha  1.13e-02    1.310726
    2232  17778 1998-10-01  mkt_excess  8.14e-01    3.863409
    2233  17778 1998-11-01       alpha  1.12e-02    1.282727
    2234  17778 1998-11-01  mkt_excess  8.09e-01    3.866855
    2235  17778 1998-12-01       alpha  1.22e-02    1.403379
    2236  17778 1998-12-01  mkt_excess  7.95e-01    3.889000
    2237  17778 1999-01-01       alpha  1.13e-02    1.267639
    2238  17778 1999-01-01  mkt_excess  7.77e-01    3.719294
    2239  17778 1999-02-01       alpha  1.48e-02    1.637255
    2240  17778 1999-02-01  mkt_excess  7.01e-01    3.313516
    2241  17778 1999-03-01       alpha  1.30e-02    1.426183
    2242  17778 1999-03-01  mkt_excess  7.23e-01    3.365739
    2243  17778 1999-04-01       alpha  1.35e-02    1.466761
    2244  17778 1999-04-01  mkt_excess  7.29e-01    3.402198
    2245  17778 1999-05-01       alpha  1.26e-02    1.368116
    2246  17778 1999-05-01  mkt_excess  7.49e-01    3.508280
    2247  17778 1999-06-01       alpha  1.13e-02    1.196477
    2248  17778 1999-06-01  mkt_excess  7.22e-01    3.304158
    2249  17778 1999-07-01       alpha  8.48e-03    0.970749
    2250  17778 1999-07-01  mkt_excess  7.00e-01    3.474942
    2251  17778 1999-08-01       alpha  8.17e-03    0.945411
    2252  17778 1999-08-01  mkt_excess  7.33e-01    3.651789
    2253  17778 1999-09-01       alpha  5.19e-03    0.579078
    2254  17778 1999-09-01  mkt_excess  7.89e-01    3.794635
    2255  17778 1999-10-01       alpha  5.40e-03    0.590304
    2256  17778 1999-10-01  mkt_excess  8.37e-01    4.003235
    2257  17778 1999-11-01       alpha  2.48e-03    0.258963
    2258  17778 1999-11-01  mkt_excess  8.33e-01    3.801085
    2259  17778 1999-12-01       alpha  2.23e-03    0.228427
    2260  17778 1999-12-01  mkt_excess  7.78e-01    3.564973
    2261  17778 2000-01-01       alpha -2.30e-03   -0.252836
    2262  17778 2000-01-01  mkt_excess  8.09e-01    4.014441
    2263  17778 2000-02-01       alpha -3.09e-03   -0.333041
    2264  17778 2000-02-01  mkt_excess  8.20e-01    3.978528
    2265  17778 2000-03-01       alpha -1.82e-04   -0.017517
    2266  17778 2000-03-01  mkt_excess  9.05e-01    3.961847
    2267  17778 2000-04-01       alpha  3.37e-03    0.328015
    2268  17778 2000-04-01  mkt_excess  8.46e-01    3.797313
    2269  17778 2000-05-01       alpha  3.50e-03    0.344625
    2270  17778 2000-05-01  mkt_excess  8.35e-01    3.801907
    2271  17778 2000-06-01       alpha  1.83e-03    0.175788
    2272  17778 2000-06-01  mkt_excess  7.99e-01    3.566399
    2273  17778 2000-07-01       alpha  2.34e-03    0.225593
    2274  17778 2000-07-01  mkt_excess  7.84e-01    3.504418
    2275  17778 2000-08-01       alpha  1.85e-03    0.177445
    2276  17778 2000-08-01  mkt_excess  7.78e-01    3.525566
    2277  17778 2000-09-01       alpha  3.65e-03    0.349621
    2278  17778 2000-09-01  mkt_excess  6.77e-01    3.090478
    2279  17778 2000-10-01       alpha  3.52e-03    0.337747
    2280  17778 2000-10-01  mkt_excess  6.79e-01    3.108319
    2281  17778 2000-11-01       alpha  5.61e-03    0.540187
    2282  17778 2000-11-01  mkt_excess  5.91e-01    2.811892
    2283  17778 2000-12-01       alpha  6.45e-03    0.618213
    2284  17778 2000-12-01  mkt_excess  5.92e-01    2.801819
    2285  17778 2001-01-01       alpha  5.91e-03    0.563291
    2286  17778 2001-01-01  mkt_excess  5.84e-01    2.753381
    2287  17778 2001-02-01       alpha  6.08e-03    0.587619
    2288  17778 2001-02-01  mkt_excess  5.26e-01    2.598441
    2289  17778 2001-03-01       alpha  6.26e-03    0.608789
    2290  17778 2001-03-01  mkt_excess  5.47e-01    2.764857
    2291  17778 2001-04-01       alpha  6.58e-03    0.640072
    2292  17778 2001-04-01  mkt_excess  5.43e-01    2.791395
    2293  17778 2001-05-01       alpha  8.33e-03    0.825666
    2294  17778 2001-05-01  mkt_excess  5.54e-01    2.898577
    2295  17778 2001-06-01       alpha  8.50e-03    0.842451
    2296  17778 2001-06-01  mkt_excess  5.52e-01    2.893324
    2297  17778 2001-07-01       alpha  7.92e-03    0.784095
    2298  17778 2001-07-01  mkt_excess  5.64e-01    2.923948
    2299  17778 2001-08-01       alpha  8.70e-03    0.864419
    2300  17778 2001-08-01  mkt_excess  5.54e-01    2.911121
    2301  17778 2001-09-01       alpha  9.83e-03    0.979261
    2302  17778 2001-09-01  mkt_excess  5.30e-01    2.843237
    2303  17778 2001-10-01       alpha  9.88e-03    0.983941
    2304  17778 2001-10-01  mkt_excess  5.30e-01    2.843047
    2305  17778 2001-11-01       alpha  9.22e-03    0.911865
    2306  17778 2001-11-01  mkt_excess  5.10e-01    2.731925
    2307  17778 2001-12-01       alpha  9.83e-03    0.967536
    2308  17778 2001-12-01  mkt_excess  5.16e-01    2.751724
    2309  17778 2002-01-01       alpha  9.68e-03    0.954132
    2310  17778 2002-01-01  mkt_excess  5.24e-01    2.779745
    2311  17778 2002-02-01       alpha  9.34e-03    0.920536
    2312  17778 2002-02-01  mkt_excess  5.26e-01    2.796539
    2313  17778 2002-03-01       alpha  7.83e-03    0.766956
    2314  17778 2002-03-01  mkt_excess  5.24e-01    2.763585
    2315  17778 2002-04-01       alpha  8.48e-03    0.830896
    2316  17778 2002-04-01  mkt_excess  5.04e-01    2.665404
    2317  17778 2002-05-01       alpha  7.38e-03    0.731111
    2318  17778 2002-05-01  mkt_excess  4.71e-01    2.487236
    2319  17778 2002-06-01       alpha  4.92e-03    0.487497
    2320  17778 2002-06-01  mkt_excess  4.89e-01    2.605603
    2321  17778 2002-07-01       alpha  6.60e-03    0.651892
    2322  17778 2002-07-01  mkt_excess  4.82e-01    2.576869
    2323  17778 2002-08-01       alpha  9.41e-03    0.942879
    2324  17778 2002-08-01  mkt_excess  4.59e-01    2.473382
    2325  17778 2002-09-01       alpha  9.39e-03    0.938687
    2326  17778 2002-09-01  mkt_excess  4.19e-01    2.301840
    2327  17778 2002-10-01       alpha  9.06e-03    0.906227
    2328  17778 2002-10-01  mkt_excess  3.97e-01    2.212801
    2329  17778 2002-11-01       alpha  7.85e-03    0.781574
    2330  17778 2002-11-01  mkt_excess  3.76e-01    2.101025
    2331  17778 2002-12-01       alpha  8.19e-03    0.814458
    2332  17778 2002-12-01  mkt_excess  3.71e-01    2.086309
    2333  17778 2003-01-01       alpha  5.69e-03    0.567512
    2334  17778 2003-01-01  mkt_excess  3.77e-01    2.131814
    2335  17778 2003-02-01       alpha  2.93e-03    0.290724
    2336  17778 2003-02-01  mkt_excess  3.55e-01    1.972470
    2337  17778 2003-03-01       alpha  1.12e-04    0.011692
    2338  17778 2003-03-01  mkt_excess  3.06e-01    1.772442
    2339  17778 2003-04-01       alpha  1.12e-03    0.116330
    2340  17778 2003-04-01  mkt_excess  3.35e-01    1.968946
    2341  17778 2003-05-01       alpha  4.44e-04    0.046240
    2342  17778 2003-05-01  mkt_excess  3.39e-01    2.010440
    2343  17778 2003-06-01       alpha -8.82e-04   -0.092967
    2344  17778 2003-06-01  mkt_excess  3.24e-01    1.942291
    2345  17778 2003-07-01       alpha  5.27e-04    0.056459
    2346  17778 2003-07-01  mkt_excess  3.10e-01    1.892984
    2347  17778 2003-08-01       alpha  2.90e-03    0.315526
    2348  17778 2003-08-01  mkt_excess  2.28e-01    1.314387
    2349  17778 2003-09-01       alpha  3.33e-03    0.362155
    2350  17778 2003-09-01  mkt_excess  2.42e-01    1.385921
    2351  17778 2003-10-01       alpha  2.66e-03    0.290983
    2352  17778 2003-10-01  mkt_excess  2.24e-01    1.285123
    2353  17778 2003-11-01       alpha  3.26e-03    0.354899
    2354  17778 2003-11-01  mkt_excess  2.18e-01    1.226272
    2355  17778 2003-12-01       alpha  2.96e-03    0.321679
    2356  17778 2003-12-01  mkt_excess  2.12e-01    1.184190
    2357  17778 2004-01-01       alpha  5.31e-03    0.582812
    2358  17778 2004-01-01  mkt_excess  2.39e-01    1.344707
    2359  17778 2004-02-01       alpha  4.53e-03    0.502545
    2360  17778 2004-02-01  mkt_excess  2.68e-01    1.518334
    2361  17778 2004-03-01       alpha  4.52e-03    0.501746
    2362  17778 2004-03-01  mkt_excess  2.72e-01    1.537455
    2363  17778 2004-04-01       alpha  3.66e-03    0.408131
    2364  17778 2004-04-01  mkt_excess  2.57e-01    1.451536
    2365  17778 2004-05-01       alpha  3.71e-03    0.413700
    2366  17778 2004-05-01  mkt_excess  2.44e-01    1.375107
    2367  17778 2004-06-01       alpha  4.63e-03    0.519937
    2368  17778 2004-06-01  mkt_excess  2.63e-01    1.484169
    2369  17778 2004-07-01       alpha  4.65e-03    0.521920
    2370  17778 2004-07-01  mkt_excess  2.63e-01    1.487807
    2371  17778 2004-08-01       alpha  5.46e-03    0.617109
    2372  17778 2004-08-01  mkt_excess  2.58e-01    1.469867
    2373  17778 2004-09-01       alpha  7.60e-03    0.896036
    2374  17778 2004-09-01  mkt_excess  2.31e-01    1.368853
    2375  17778 2004-10-01       alpha  4.55e-03    0.555792
    2376  17778 2004-10-01  mkt_excess  1.69e-01    1.024840
    2377  17778 2004-11-01       alpha  6.20e-03    0.781570
    2378  17778 2004-11-01  mkt_excess  1.91e-01    1.197058
    2379  17778 2004-12-01       alpha  7.66e-03    0.966766
    2380  17778 2004-12-01  mkt_excess  2.25e-01    1.394307
    2381  17778 2005-01-01       alpha  9.39e-03    1.207544
    2382  17778 2005-01-01  mkt_excess  1.94e-01    1.214961
    2383  17778 2005-02-01       alpha  1.19e-02    1.635865
    2384  17778 2005-02-01  mkt_excess  2.22e-01    1.485837
    2385  17778 2005-03-01       alpha  6.27e-03    1.116344
    2386  17778 2005-03-01  mkt_excess  1.19e-01    1.021956
    2387  17778 2005-04-01       alpha  5.18e-03    0.922652
    2388  17778 2005-04-01  mkt_excess  1.40e-01    1.189440
    2389  17778 2005-05-01       alpha  5.17e-03    0.921927
    2390  17778 2005-05-01  mkt_excess  1.31e-01    1.109329
    2391  17778 2005-06-01       alpha  6.61e-03    1.232929
    2392  17778 2005-06-01  mkt_excess  1.65e-01    1.457080
    2393  17778 2005-07-01       alpha  6.07e-03    1.133270
    2394  17778 2005-07-01  mkt_excess  1.64e-01    1.448837
    2395  17778 2005-08-01       alpha  5.44e-03    1.016878
    2396  17778 2005-08-01  mkt_excess  1.51e-01    1.311698
    2397  17778 2005-09-01       alpha  3.23e-03    0.648768
    2398  17778 2005-09-01  mkt_excess  1.97e-01    1.819840
    2399  17778 2005-10-01       alpha  4.22e-03    0.837674
    2400  17778 2005-10-01  mkt_excess  1.87e-01    1.709031
    2401  17778 2005-11-01       alpha  3.85e-03    0.773504
    2402  17778 2005-11-01  mkt_excess  2.37e-01    2.096789
    2403  17778 2005-12-01       alpha  2.51e-03    0.516828
    2404  17778 2005-12-01  mkt_excess  2.31e-01    2.095813
    2405  17778 2006-01-01       alpha  3.32e-03    0.694150
    2406  17778 2006-01-01  mkt_excess  2.45e-01    2.254358
    2407  17778 2006-02-01       alpha  1.86e-03    0.390680
    2408  17778 2006-02-01  mkt_excess  2.90e-01    2.570358
    2409  17778 2006-03-01       alpha  3.42e-03    0.727348
    2410  17778 2006-03-01  mkt_excess  2.53e-01    2.208555
    2411  17778 2006-04-01       alpha  2.85e-03    0.607240
    2412  17778 2006-04-01  mkt_excess  2.42e-01    2.048570
    2413  17778 2006-05-01       alpha  3.48e-03    0.734281
    2414  17778 2006-05-01  mkt_excess  2.27e-01    1.916892
    2415  17778 2006-06-01       alpha  3.11e-03    0.655619
    2416  17778 2006-06-01  mkt_excess  2.30e-01    1.933830
    2417  17778 2006-07-01       alpha  3.08e-03    0.648899
    2418  17778 2006-07-01  mkt_excess  2.29e-01    1.925724
    2419  17778 2006-08-01       alpha  3.45e-03    0.719641
    2420  17778 2006-08-01  mkt_excess  2.44e-01    1.992486
    2421  17778 2006-09-01       alpha  2.63e-03    0.545847
    2422  17778 2006-09-01  mkt_excess  2.72e-01    2.108368
    2423  17778 2006-10-01       alpha  3.81e-03    0.757419
    2424  17778 2006-10-01  mkt_excess  2.98e-01    2.219092
    2425  17778 2006-11-01       alpha  4.40e-03    0.888617
    2426  17778 2006-11-01  mkt_excess  3.40e-01    2.483546
    2427  17778 2006-12-01       alpha  3.56e-03    0.739735
    2428  17778 2006-12-01  mkt_excess  3.30e-01    2.480672
    2429  17778 2007-01-01       alpha  3.77e-03    0.784882
    2430  17778 2007-01-01  mkt_excess  3.23e-01    2.428382
    2431  17778 2007-02-01       alpha  3.29e-03    0.678992
    2432  17778 2007-02-01  mkt_excess  3.31e-01    2.467768
    2433  17778 2007-03-01       alpha  4.20e-03    0.879202
    2434  17778 2007-03-01  mkt_excess  3.54e-01    2.645307
    2435  17778 2007-04-01       alpha  2.95e-03    0.620016
    2436  17778 2007-04-01  mkt_excess  3.83e-01    2.849862
    2437  17778 2007-05-01       alpha  2.40e-03    0.503168
    2438  17778 2007-05-01  mkt_excess  3.82e-01    2.846053
    2439  17778 2007-06-01       alpha  4.50e-03    0.983553
    2440  17778 2007-06-01  mkt_excess  2.86e-01    2.152068
    2441  17778 2007-07-01       alpha  3.50e-03    0.768614
    2442  17778 2007-07-01  mkt_excess  3.40e-01    2.468738
    2443  17778 2007-08-01       alpha  3.51e-03    0.769064
    2444  17778 2007-08-01  mkt_excess  3.44e-01    2.493842
    2445  17778 2007-09-01       alpha  1.52e-03    0.327417
    2446  17778 2007-09-01  mkt_excess  4.37e-01    2.874946
    2447  17778 2007-10-01       alpha  3.15e-03    0.636951
    2448  17778 2007-10-01  mkt_excess  5.07e-01    2.972708
    2449  17778 2007-11-01       alpha  5.59e-03    1.142478
    2450  17778 2007-11-01  mkt_excess  4.83e-01    2.818556
    2451  17778 2007-12-01       alpha  4.89e-03    0.990258
    2452  17778 2007-12-01  mkt_excess  5.24e-01    2.934008
    2453  17778 2008-01-01       alpha  5.93e-03    1.247437
    2454  17778 2008-01-01  mkt_excess  4.98e-01    2.996263
    2455  17778 2008-02-01       alpha  8.61e-03    1.901519
    2456  17778 2008-02-01  mkt_excess  4.17e-01    2.648609
    2457  17778 2008-03-01       alpha  7.25e-03    1.578125
    2458  17778 2008-03-01  mkt_excess  4.35e-01    2.724841
    2459  17778 2008-04-01       alpha  6.77e-03    1.506139
    2460  17778 2008-04-01  mkt_excess  3.22e-01    1.961015
    2461  17778 2008-05-01       alpha  6.73e-03    1.502986
    2462  17778 2008-05-01  mkt_excess  3.33e-01    1.962822
    2463  17778 2008-06-01       alpha  4.47e-03    0.973127
    2464  17778 2008-06-01  mkt_excess  4.88e-01    3.028920
    2465  17778 2008-07-01       alpha  3.85e-03    0.827866
    2466  17778 2008-07-01  mkt_excess  5.11e-01    3.112363
    2467  17778 2008-08-01       alpha  3.41e-03    0.738769
    2468  17778 2008-08-01  mkt_excess  4.98e-01    3.049791
    2469  17778 2008-09-01       alpha  6.98e-03    1.340707
    2470  17778 2008-09-01  mkt_excess  2.18e-01    1.284745
    2471  17778 2008-10-01       alpha  5.48e-03    1.029739
    2472  17778 2008-10-01  mkt_excess  3.84e-01    2.675032
    2473  17778 2008-11-01       alpha  3.28e-03    0.612376
    2474  17778 2008-11-01  mkt_excess  4.36e-01    3.123583
    2475  17778 2008-12-01       alpha  2.16e-03    0.389972
    2476  17778 2008-12-01  mkt_excess  4.26e-01    2.923800
    2477  17778 2009-01-01       alpha  7.30e-04    0.132024
    2478  17778 2009-01-01  mkt_excess  4.45e-01    3.167196
    2479  17778 2009-02-01       alpha -9.40e-04   -0.166616
    2480  17778 2009-02-01  mkt_excess  5.09e-01    3.732962
    2481  17778 2009-03-01       alpha  3.79e-04    0.066740
    2482  17778 2009-03-01  mkt_excess  5.59e-01    4.226101
    2483  17778 2009-04-01       alpha  7.38e-04    0.130251
    2484  17778 2009-04-01  mkt_excess  5.83e-01    4.618140
    2485  17778 2009-05-01       alpha  6.69e-04    0.118171
    2486  17778 2009-05-01  mkt_excess  5.65e-01    4.519139
    2487  17778 2009-06-01       alpha  5.37e-04    0.094572
    2488  17778 2009-06-01  mkt_excess  5.66e-01    4.514434
    2489  17778 2009-07-01       alpha  1.07e-03    0.188035
    2490  17778 2009-07-01  mkt_excess  5.87e-01    4.751000
    2491  17778 2009-08-01       alpha  1.48e-03    0.259354
    2492  17778 2009-08-01  mkt_excess  5.92e-01    4.805793
    2493  17778 2009-09-01       alpha  1.34e-03    0.235413
    2494  17778 2009-09-01  mkt_excess  5.86e-01    4.779295
    2495  17778 2009-10-01       alpha  1.88e-03    0.331814
    2496  17778 2009-10-01  mkt_excess  5.92e-01    4.863748
    2497  17778 2009-11-01       alpha  2.18e-03    0.385673
    2498  17778 2009-11-01  mkt_excess  5.96e-01    4.944056
    2499  17778 2009-12-01       alpha  1.19e-03    0.210709
    2500  17778 2009-12-01  mkt_excess  5.82e-01    4.814284
    2501  17778 2010-01-01       alpha  3.47e-03    0.548010
    2502  17778 2010-01-01  mkt_excess  5.46e-01    4.035127
    2503  17778 2010-02-01       alpha  4.06e-03    0.640508
    2504  17778 2010-02-01  mkt_excess  5.54e-01    4.099347
    2505  17778 2010-03-01       alpha  4.22e-03    0.666824
    2506  17778 2010-03-01  mkt_excess  5.39e-01    4.048806
    2507  17778 2010-04-01       alpha  3.49e-03    0.543240
    2508  17778 2010-04-01  mkt_excess  5.25e-01    3.882860
    2509  17778 2010-05-01       alpha  3.23e-03    0.500790
    2510  17778 2010-05-01  mkt_excess  5.57e-01    4.182157
    2511  17778 2010-06-01       alpha  6.11e-03    0.876974
    2512  17778 2010-06-01  mkt_excess  4.96e-01    3.483620
    2513  17778 2010-07-01       alpha  5.47e-03    0.777863
    2514  17778 2010-07-01  mkt_excess  4.73e-01    3.322785
    2515  17778 2010-08-01       alpha  6.10e-03    0.864717
    2516  17778 2010-08-01  mkt_excess  4.63e-01    3.264559
    2517  17778 2010-09-01       alpha  6.50e-03    0.922692
    2518  17778 2010-09-01  mkt_excess  4.62e-01    3.368944
    2519  17778 2010-10-01       alpha  4.61e-03    0.650892
    2520  17778 2010-10-01  mkt_excess  4.53e-01    3.295933
    2521  17778 2010-11-01       alpha  4.34e-03    0.613460
    2522  17778 2010-11-01  mkt_excess  4.50e-01    3.259468
    2523  17778 2010-12-01       alpha  4.07e-03    0.573586
    2524  17778 2010-12-01  mkt_excess  4.36e-01    3.195328
    2525  17778 2011-01-01       alpha  4.31e-03    0.607400
    2526  17778 2011-01-01  mkt_excess  4.38e-01    3.208560
    2527  17778 2011-02-01       alpha  5.77e-03    0.810313
    2528  17778 2011-02-01  mkt_excess  4.47e-01    3.274130
    2529  17778 2011-03-01       alpha  4.47e-03    0.624036
    2530  17778 2011-03-01  mkt_excess  4.45e-01    3.234233
    2531  17778 2011-04-01       alpha  4.56e-03    0.635740
    2532  17778 2011-04-01  mkt_excess  4.42e-01    3.222742
    2533  17778 2011-05-01       alpha  3.00e-03    0.417891
    2534  17778 2011-05-01  mkt_excess  4.57e-01    3.319015
    2535  17778 2011-06-01       alpha  2.90e-03    0.404834
    2536  17778 2011-06-01  mkt_excess  4.59e-01    3.333813
    2537  17778 2011-07-01       alpha  2.43e-03    0.337681
    2538  17778 2011-07-01  mkt_excess  4.64e-01    3.365843
    2539  17778 2011-08-01       alpha  2.05e-03    0.285724
    2540  17778 2011-08-01  mkt_excess  4.56e-01    3.357539
    2541  17778 2011-09-01       alpha  2.43e-03    0.339940
    2542  17778 2011-09-01  mkt_excess  4.56e-01    3.411640
    2543  17778 2011-10-01       alpha  1.77e-03    0.251003
    2544  17778 2011-10-01  mkt_excess  4.67e-01    3.664138
    2545  17778 2011-11-01       alpha  1.96e-03    0.277519
    2546  17778 2011-11-01  mkt_excess  4.67e-01    3.657107
    2547  17778 2011-12-01       alpha  1.06e-03    0.149583
    2548  17778 2011-12-01  mkt_excess  4.65e-01    3.629883
    2549  17778 2012-01-01       alpha  1.30e-03    0.183213
    2550  17778 2012-01-01  mkt_excess  4.67e-01    3.666034
    2551  17778 2012-02-01       alpha  1.47e-03    0.207926
    2552  17778 2012-02-01  mkt_excess  4.58e-01    3.616995
    2553  17778 2012-03-01       alpha  1.47e-03    0.208150
    2554  17778 2012-03-01  mkt_excess  4.60e-01    3.642736
    2555  17778 2012-04-01       alpha  1.69e-03    0.238986
    2556  17778 2012-04-01  mkt_excess  4.64e-01    3.668194
    2557  17778 2012-05-01       alpha  2.17e-03    0.307439
    2558  17778 2012-05-01  mkt_excess  4.64e-01    3.696257
    2559  17778 2012-06-01       alpha  2.63e-03    0.371731
    2560  17778 2012-06-01  mkt_excess  4.70e-01    3.747017
    2561  17778 2012-07-01       alpha  2.59e-03    0.365761
    2562  17778 2012-07-01  mkt_excess  4.74e-01    3.762115
    2563  17778 2012-08-01       alpha  1.16e-03    0.165329
    2564  17778 2012-08-01  mkt_excess  4.69e-01    3.769780
    2565  17778 2012-09-01       alpha  2.02e-03    0.287953
    2566  17778 2012-09-01  mkt_excess  4.76e-01    3.817785
    2567  17778 2012-10-01       alpha -8.76e-07   -0.000129
    2568  17778 2012-10-01  mkt_excess  4.69e-01    3.888136
    2569  17778 2012-11-01       alpha -1.09e-03   -0.162942
    2570  17778 2012-11-01  mkt_excess  4.90e-01    4.111181
    2571  17778 2012-12-01       alpha -1.12e-03   -0.168326
    2572  17778 2012-12-01  mkt_excess  4.92e-01    4.122702
    2573  17778 2013-01-01       alpha -4.59e-06   -0.000679
    2574  17778 2013-01-01  mkt_excess  5.05e-01    4.165216
    2575  17778 2013-02-01       alpha -1.33e-04   -0.019591
    2576  17778 2013-02-01  mkt_excess  5.15e-01    4.239785
    2577  17778 2013-03-01       alpha  6.77e-04    0.100422
    2578  17778 2013-03-01  mkt_excess  5.12e-01    4.250420
    2579  17778 2013-04-01       alpha  1.17e-03    0.173697
    2580  17778 2013-04-01  mkt_excess  5.18e-01    4.284185
    2581  17778 2013-05-01       alpha  2.26e-03    0.331906
    2582  17778 2013-05-01  mkt_excess  5.25e-01    4.301317
    2583  17778 2013-06-01       alpha  3.35e-03    0.497057
    2584  17778 2013-06-01  mkt_excess  4.94e-01    4.005587
    2585  17778 2013-07-01       alpha  4.28e-03    0.639769
    2586  17778 2013-07-01  mkt_excess  4.89e-01    4.028294
    2587  17778 2013-08-01       alpha  3.63e-03    0.542581
    2588  17778 2013-08-01  mkt_excess  4.95e-01    4.077442
    2589  17778 2013-09-01       alpha -7.82e-05   -0.012830
    2590  17778 2013-09-01  mkt_excess  5.93e-01    5.259538
    2591  17778 2013-10-01       alpha  2.99e-04    0.047960
    2592  17778 2013-10-01  mkt_excess  5.71e-01    4.552683
    2593  17778 2013-11-01       alpha  1.74e-03    0.278942
    2594  17778 2013-11-01  mkt_excess  5.30e-01    4.150024
    2595  17778 2013-12-01       alpha  3.11e-03    0.513424
    2596  17778 2013-12-01  mkt_excess  5.31e-01    4.287276
    2597  17778 2014-01-01       alpha  3.35e-03    0.548476
    2598  17778 2014-01-01  mkt_excess  5.18e-01    4.064337
    2599  17778 2014-02-01       alpha  6.06e-03    0.991612
    2600  17778 2014-02-01  mkt_excess  4.36e-01    3.320274
    2601  17778 2014-03-01       alpha  7.14e-03    1.171346
    2602  17778 2014-03-01  mkt_excess  3.87e-01    2.856578
    2603  17778 2014-04-01       alpha  7.53e-03    1.253394
    2604  17778 2014-04-01  mkt_excess  3.49e-01    2.497668
    2605  17778 2014-05-01       alpha  7.75e-03    1.307098
    2606  17778 2014-05-01  mkt_excess  3.67e-01    2.640724
    2607  17778 2014-06-01       alpha  7.82e-03    1.315315
    2608  17778 2014-06-01  mkt_excess  3.61e-01    2.594281
    2609  17778 2014-07-01       alpha  7.31e-03    1.254177
    2610  17778 2014-07-01  mkt_excess  3.36e-01    2.398860
    2611  17778 2014-08-01       alpha  7.92e-03    1.327691
    2612  17778 2014-08-01  mkt_excess  3.54e-01    2.476549
    2613  17778 2014-09-01       alpha  8.27e-03    1.400786
    2614  17778 2014-09-01  mkt_excess  3.59e-01    2.520400
    2615  17778 2014-10-01       alpha  8.67e-03    1.459443
    2616  17778 2014-10-01  mkt_excess  3.50e-01    2.447225
    2617  17778 2014-11-01       alpha  9.46e-03    1.584180
    2618  17778 2014-11-01  mkt_excess  3.62e-01    2.486720
    2619  17778 2014-12-01       alpha  1.00e-02    1.692938
    2620  17778 2014-12-01  mkt_excess  3.67e-01    2.533994
    2621  17778 2015-01-01       alpha  5.33e-03    1.022277
    2622  17778 2015-01-01  mkt_excess  4.70e-01    3.678114
    2623  17778 2015-02-01       alpha  4.91e-03    0.941981
    2624  17778 2015-02-01  mkt_excess  4.60e-01    3.645809
    2625  17778 2015-03-01       alpha  4.75e-03    0.919646
    2626  17778 2015-03-01  mkt_excess  4.74e-01    3.718165
    2627  17778 2015-04-01       alpha  5.34e-03    1.060559
    2628  17778 2015-04-01  mkt_excess  4.81e-01    3.868541
    2629  17778 2015-05-01       alpha  6.82e-03    1.354000
    2630  17778 2015-05-01  mkt_excess  4.26e-01    3.310534
    2631  17778 2015-06-01       alpha  1.38e-03    0.322141
    2632  17778 2015-06-01  mkt_excess  5.79e-01    5.214267
    2633  17778 2015-07-01       alpha  2.38e-03    0.577625
    2634  17778 2015-07-01  mkt_excess  6.30e-01    5.753650
    2635  17778 2015-08-01       alpha  7.08e-04    0.175595
    2636  17778 2015-08-01  mkt_excess  6.81e-01    6.395638
    2637  17778 2015-09-01       alpha  4.01e-04    0.100862
    2638  17778 2015-09-01  mkt_excess  7.10e-01    6.446782
    2639  17778 2015-10-01       alpha  1.22e-03    0.322496
    2640  17778 2015-10-01  mkt_excess  7.29e-01    7.145067
    2641  17778 2015-11-01       alpha  8.13e-04    0.213967
    2642  17778 2015-11-01  mkt_excess  7.31e-01    7.128648
    2643  17778 2015-12-01       alpha  1.17e-03    0.319205
    2644  17778 2015-12-01  mkt_excess  7.71e-01    7.623560
    2645  17778 2016-01-01       alpha  1.79e-03    0.492342
    2646  17778 2016-01-01  mkt_excess  7.49e-01    7.571935
    2647  17778 2016-02-01       alpha  1.89e-03    0.524587
    2648  17778 2016-02-01  mkt_excess  7.29e-01    7.368407
    2649  17778 2016-03-01       alpha  2.78e-03    0.790986
    2650  17778 2016-03-01  mkt_excess  7.26e-01    7.762272
    2651  17778 2016-04-01       alpha  3.46e-03    0.993186
    2652  17778 2016-04-01  mkt_excess  7.34e-01    7.874682
    2653  17778 2016-05-01       alpha  3.49e-03    0.989747
    2654  17778 2016-05-01  mkt_excess  7.16e-01    7.603975
    2655  17778 2016-06-01       alpha  4.15e-03    1.169375
    2656  17778 2016-06-01  mkt_excess  7.09e-01    7.470971
    2657  17778 2016-07-01       alpha  4.27e-03    1.184534
    2658  17778 2016-07-01  mkt_excess  6.84e-01    7.139607
    2659  17778 2016-08-01       alpha  4.34e-03    1.177537
    2660  17778 2016-08-01  mkt_excess  7.00e-01    6.998237
    2661  17778 2016-09-01       alpha  2.67e-03    0.696357
    2662  17778 2016-09-01  mkt_excess  7.38e-01    6.834122
    2663  17778 2016-10-01       alpha  2.92e-03    0.771707
    2664  17778 2016-10-01  mkt_excess  7.17e-01    6.150728
    2665  17778 2016-11-01       alpha  3.23e-03    0.815529
    2666  17778 2016-11-01  mkt_excess  7.59e-01    6.340796
    2667  17778 2016-12-01       alpha  4.14e-03    1.061000
    2668  17778 2016-12-01  mkt_excess  7.57e-01    6.430426
    2669  17778 2017-01-01       alpha  4.09e-03    1.053970
    2670  17778 2017-01-01  mkt_excess  7.66e-01    6.427690
    2671  17778 2017-02-01       alpha  4.62e-03    1.210338
    2672  17778 2017-02-01  mkt_excess  7.94e-01    6.735794
    2673  17778 2017-03-01       alpha  3.93e-03    1.021467
    2674  17778 2017-03-01  mkt_excess  7.98e-01    6.667031
    2675  17778 2017-04-01       alpha  3.70e-03    0.953805
    2676  17778 2017-04-01  mkt_excess  7.96e-01    6.596741
    2677  17778 2017-05-01       alpha  2.49e-03    0.635016
    2678  17778 2017-05-01  mkt_excess  8.41e-01    6.673908
    2679  17778 2017-06-01       alpha  2.61e-03    0.667245
    2680  17778 2017-06-01  mkt_excess  8.30e-01    6.547349
    2681  17778 2017-07-01       alpha  2.61e-03    0.665474
    2682  17778 2017-07-01  mkt_excess  8.33e-01    6.567384
    2683  17778 2017-08-01       alpha  3.55e-03    0.913256
    2684  17778 2017-08-01  mkt_excess  8.36e-01    6.602153
    2685  17778 2017-09-01       alpha  3.09e-03    0.797965
    2686  17778 2017-09-01  mkt_excess  8.25e-01    6.537245
    2687  17778 2017-10-01       alpha  3.38e-03    0.865010
    2688  17778 2017-10-01  mkt_excess  8.17e-01    6.431162
    2689  17778 2017-11-01       alpha  3.33e-03    0.849683
    2690  17778 2017-11-01  mkt_excess  8.22e-01    6.488728
    2691  17778 2017-12-01       alpha  3.42e-03    0.870991
    2692  17778 2017-12-01  mkt_excess  8.21e-01    6.482772
    2693  17778 2018-01-01       alpha  3.40e-03    0.869106
    2694  17778 2018-01-01  mkt_excess  8.19e-01    6.479609
    2695  17778 2018-02-01       alpha  2.44e-03    0.639163
    2696  17778 2018-02-01  mkt_excess  8.33e-01    6.818202
    2697  17778 2018-03-01       alpha  2.09e-03    0.551615
    2698  17778 2018-03-01  mkt_excess  8.54e-01    6.993485
    2699  17778 2018-04-01       alpha  1.43e-03    0.374571
    2700  17778 2018-04-01  mkt_excess  8.59e-01    6.948747
    2701  17778 2018-05-01       alpha  2.56e-04    0.068032
    2702  17778 2018-05-01  mkt_excess  8.29e-01    6.816568
    2703  17778 2018-06-01       alpha -3.84e-05   -0.010131
    2704  17778 2018-06-01  mkt_excess  8.29e-01    6.744164
    2705  17778 2018-07-01       alpha  5.59e-04    0.145484
    2706  17778 2018-07-01  mkt_excess  8.62e-01    6.792193
    2707  17778 2018-08-01       alpha  1.16e-03    0.298213
    2708  17778 2018-08-01  mkt_excess  8.56e-01    6.686540
    2709  17778 2018-09-01       alpha  1.49e-03    0.386494
    2710  17778 2018-09-01  mkt_excess  8.61e-01    6.692990
    2711  17778 2018-10-01       alpha  2.49e-03    0.661414
    2712  17778 2018-10-01  mkt_excess  8.37e-01    6.930588
    2713  17778 2018-11-01       alpha  3.35e-03    0.880564
    2714  17778 2018-11-01  mkt_excess  8.51e-01    6.924360
    2715  17778 2018-12-01       alpha  3.88e-03    1.040886
    2716  17778 2018-12-01  mkt_excess  8.30e-01    7.372539
    2717  17778 2019-01-01       alpha  3.99e-03    1.031632
    2718  17778 2019-01-01  mkt_excess  7.52e-01    6.722566
    2719  17778 2019-02-01       alpha  3.33e-03    0.833806
    2720  17778 2019-02-01  mkt_excess  7.36e-01    6.320524
    2721  17778 2019-03-01       alpha  1.82e-03    0.479436
    2722  17778 2019-03-01  mkt_excess  7.40e-01    6.683660
    2723  17778 2019-04-01       alpha  1.83e-03    0.474420
    2724  17778 2019-04-01  mkt_excess  7.66e-01    6.882296
    2725  17778 2019-05-01       alpha  1.29e-03    0.334748
    2726  17778 2019-05-01  mkt_excess  8.09e-01    7.529914
    2727  17778 2019-06-01       alpha  1.90e-03    0.497136
    2728  17778 2019-06-01  mkt_excess  8.26e-01    7.955951
    2729  17778 2019-07-01       alpha  1.06e-03    0.272380
    2730  17778 2019-07-01  mkt_excess  8.26e-01    7.762552
    2731  17778 2019-08-01       alpha  3.05e-04    0.081533
    2732  17778 2019-08-01  mkt_excess  8.00e-01    7.789688
    2733  17778 2019-09-01       alpha  1.52e-04    0.040477
    2734  17778 2019-09-01  mkt_excess  8.08e-01    7.852081
    2735  17778 2019-10-01       alpha  3.00e-04    0.080121
    2736  17778 2019-10-01  mkt_excess  8.10e-01    7.865378
    2737  17778 2019-11-01       alpha -2.63e-04   -0.071425
    2738  17778 2019-11-01  mkt_excess  8.02e-01    7.969434
    2739  17778 2019-12-01       alpha -4.48e-04   -0.121816
    2740  17778 2019-12-01  mkt_excess  8.05e-01    8.027128
    2741  17778 2020-01-01       alpha -2.14e-04   -0.058177
    2742  17778 2020-01-01  mkt_excess  7.96e-01    7.881796
    2743  17778 2020-02-01       alpha -3.27e-04   -0.090712
    2744  17778 2020-02-01  mkt_excess  8.30e-01    8.545581
    2745  17778 2020-03-01       alpha -4.38e-04   -0.122597
    2746  17778 2020-03-01  mkt_excess  8.43e-01    9.663601
    2747  17778 2020-04-01       alpha -7.26e-04   -0.192160
    2748  17778 2020-04-01  mkt_excess  7.55e-01    8.903042
    2749  17778 2020-05-01       alpha -1.38e-03   -0.355088
    2750  17778 2020-05-01  mkt_excess  7.34e-01    8.511193
    2751  17778 2020-06-01       alpha -1.65e-03   -0.414578
    2752  17778 2020-06-01  mkt_excess  7.20e-01    8.164052
    2753  17778 2020-07-01       alpha -1.47e-03   -0.360771
    2754  17778 2020-07-01  mkt_excess  7.42e-01    8.334737
    2755  17778 2020-08-01       alpha -6.61e-04   -0.155920
    2756  17778 2020-08-01  mkt_excess  7.70e-01    8.372138
    2757  17778 2020-09-01       alpha -3.00e-04   -0.070990
    2758  17778 2020-09-01  mkt_excess  7.63e-01    8.329915
    2759  17778 2020-10-01       alpha -9.23e-04   -0.217419
    2760  17778 2020-10-01  mkt_excess  7.80e-01    8.291990
    2761  17778 2020-11-01       alpha -3.35e-04   -0.077682
    2762  17778 2020-11-01  mkt_excess  8.14e-01    9.041668
    2763  17778 2020-12-01       alpha -6.75e-04   -0.154804
    2764  17778 2020-12-01  mkt_excess  8.07e-01    8.923034
    2765  17778 2021-01-01       alpha -1.57e-03   -0.360808
    2766  17778 2021-01-01  mkt_excess  8.25e-01    9.012993
    2767  17778 2021-02-01       alpha -1.81e-03   -0.415881
    2768  17778 2021-02-01  mkt_excess  8.35e-01    9.154512
    2769  17778 2021-03-01       alpha -1.30e-03   -0.296010
    2770  17778 2021-03-01  mkt_excess  8.41e-01    9.038110
    2771  17778 2021-04-01       alpha -1.27e-03   -0.286964
    2772  17778 2021-04-01  mkt_excess  8.50e-01    9.148138
    2773  17778 2021-05-01       alpha  5.01e-04    0.113046
    2774  17778 2021-05-01  mkt_excess  8.46e-01    9.063155
    2775  17778 2021-06-01       alpha -9.14e-04   -0.200311
    2776  17778 2021-06-01  mkt_excess  8.43e-01    8.791722
    2777  17778 2021-07-01       alpha -5.67e-04   -0.125918
    2778  17778 2021-07-01  mkt_excess  8.50e-01    8.937811
    2779  17778 2021-08-01       alpha -1.28e-03   -0.286694
    2780  17778 2021-08-01  mkt_excess  8.54e-01    9.103185
    2781  17778 2021-09-01       alpha -5.97e-04   -0.136960
    2782  17778 2021-09-01  mkt_excess  8.52e-01    9.345615
    2783  17778 2021-10-01       alpha -9.51e-04   -0.216551
    2784  17778 2021-10-01  mkt_excess  8.55e-01    9.448324
    2785  17778 2021-11-01       alpha -2.15e-03   -0.504970
    2786  17778 2021-11-01  mkt_excess  8.44e-01    9.564316
    2787  17778 2021-12-01       alpha -1.60e-03   -0.366824
    2788  17778 2021-12-01  mkt_excess  8.52e-01    9.431760
    2789  17778 2022-01-01       alpha  8.39e-04    0.181672
    2790  17778 2022-01-01  mkt_excess  7.98e-01    8.441606
    2791  17778 2022-02-01       alpha  1.23e-03    0.265312
    2792  17778 2022-02-01  mkt_excess  7.87e-01    8.308142
    2793  17778 2022-03-01       alpha  3.05e-03    0.631762
    2794  17778 2022-03-01  mkt_excess  7.96e-01    8.067388
    2795  17778 2022-04-01       alpha  3.09e-03    0.647261
    2796  17778 2022-04-01  mkt_excess  8.05e-01    8.507367
    2797  17778 2022-05-01       alpha  2.86e-03    0.598821
    2798  17778 2022-05-01  mkt_excess  8.07e-01    8.513920
    2799  17778 2022-06-01       alpha  9.88e-04    0.202148
    2800  17778 2022-06-01  mkt_excess  8.51e-01    8.975273
    2801  17778 2022-07-01       alpha  1.00e-03    0.203513
    2802  17778 2022-07-01  mkt_excess  8.62e-01    9.285264
    2803  17778 2022-08-01       alpha -2.40e-04   -0.048824
    2804  17778 2022-08-01  mkt_excess  8.74e-01    9.455852
    2805  17778 2022-09-01       alpha  8.70e-04    0.176519
    2806  17778 2022-09-01  mkt_excess  8.49e-01    9.366270
    2807  17778 2022-10-01       alpha  1.21e-03    0.243053
    2808  17778 2022-10-01  mkt_excess  8.59e-01    9.568063
    2809  17778 2022-11-01       alpha  1.56e-03    0.312576
    2810  17778 2022-11-01  mkt_excess  8.65e-01    9.596891
    2811  17778 2022-12-01       alpha  1.92e-03    0.384924
    2812  17778 2022-12-01  mkt_excess  8.55e-01    9.586033
    2813  17778 2023-01-01       alpha  6.47e-04    0.128517
    2814  17778 2023-01-01  mkt_excess  8.29e-01    9.256973
    2815  17778 2023-02-01       alpha  8.23e-04    0.163487
    2816  17778 2023-02-01  mkt_excess  8.26e-01    9.216929
    2817  17778 2023-03-01       alpha  8.35e-04    0.165541
    2818  17778 2023-03-01  mkt_excess  8.21e-01    9.138689
    2819  17778 2023-04-01       alpha  2.58e-03    0.499885
    2820  17778 2023-04-01  mkt_excess  8.19e-01    8.923214
    2821  17778 2023-05-01       alpha  2.52e-03    0.489595
    2822  17778 2023-05-01  mkt_excess  8.24e-01    8.964941
    2823  17778 2023-06-01       alpha  2.98e-03    0.578791
    2824  17778 2023-06-01  mkt_excess  8.24e-01    9.071188
    2825  17778 2023-07-01       alpha  2.38e-03    0.467433
    2826  17778 2023-07-01  mkt_excess  8.19e-01    9.096708
    2827  17778 2023-08-01       alpha  2.76e-03    0.540023
    2828  17778 2023-08-01  mkt_excess  8.11e-01    8.974528
    2829  17778 2023-09-01       alpha  2.76e-03    0.540931
    2830  17778 2023-09-01  mkt_excess  8.09e-01    9.038192
    2831  17778 2023-10-01       alpha  2.23e-03    0.437378
    2832  17778 2023-10-01  mkt_excess  8.20e-01    9.034837
    2833  17778 2023-11-01       alpha  1.24e-03    0.243258
    2834  17778 2023-11-01  mkt_excess  8.08e-01    9.113226
    2835  17778 2023-12-01       alpha  2.13e-04    0.040984
    2836  17778 2023-12-01  mkt_excess  8.05e-01    8.729603
    2837  17778 2024-01-01       alpha  1.76e-03    0.340974
    2838  17778 2024-01-01  mkt_excess  8.25e-01    8.819159
    2839  17778 2024-02-01       alpha  2.97e-03    0.585532
    2840  17778 2024-02-01  mkt_excess  8.37e-01    9.161215
    2841  17778 2024-03-01       alpha  3.23e-03    0.636682
    2842  17778 2024-03-01  mkt_excess  8.37e-01    9.182489
    2843  17778 2024-04-01       alpha  2.14e-03    0.427517
    2844  17778 2024-04-01  mkt_excess  8.38e-01    9.315106
    2845  17778 2024-05-01       alpha  2.91e-03    0.580775
    2846  17778 2024-05-01  mkt_excess  8.24e-01    9.078582
    2847  17778 2024-06-01       alpha  1.95e-03    0.382755
    2848  17778 2024-06-01  mkt_excess  8.15e-01    8.758831
    2849  17778 2024-07-01       alpha  3.68e-03    0.717715
    2850  17778 2024-07-01  mkt_excess  8.16e-01    8.692096
    2851  17778 2024-08-01       alpha  4.77e-03    0.907349
    2852  17778 2024-08-01  mkt_excess  8.17e-01    8.480484
    2853  17778 2024-09-01       alpha  3.67e-03    0.687056
    2854  17778 2024-09-01  mkt_excess  8.15e-01    8.325696
    2855  17778 2024-10-01       alpha  3.30e-03    0.617235
    2856  17778 2024-10-01  mkt_excess  8.17e-01    8.342652
    2857  17778 2024-11-01       alpha  3.42e-03    0.638293
    2858  17778 2024-11-01  mkt_excess  8.20e-01    8.419313
    2859  17778 2024-12-01       alpha  2.64e-03    0.489909
    2860  17778 2024-12-01  mkt_excess  8.30e-01    8.487625
    2861  93436 2014-06-01       alpha  5.59e-02    1.778119
    2862  93436 2014-06-01  mkt_excess  5.90e-01    0.766978
    2863  93436 2014-07-01       alpha  5.27e-02    1.717255
    2864  93436 2014-07-01  mkt_excess  6.49e-01    0.857909
    2865  93436 2014-08-01       alpha  5.44e-02    1.794054
    2866  93436 2014-08-01  mkt_excess  6.97e-01    0.932588
    2867  93436 2014-09-01       alpha  5.06e-02    1.704606
    2868  93436 2014-09-01  mkt_excess  7.66e-01    1.039701
    2869  93436 2014-10-01       alpha  4.94e-02    1.687565
    2870  93436 2014-10-01  mkt_excess  7.56e-01    1.035706
    2871  93436 2014-11-01       alpha  4.84e-02    1.680693
    2872  93436 2014-11-01  mkt_excess  7.48e-01    1.034816
    2873  93436 2014-12-01       alpha  4.54e-02    1.600625
    2874  93436 2014-12-01  mkt_excess  7.79e-01    1.084382
    2875  93436 2015-01-01       alpha  4.25e-02    1.536926
    2876  93436 2015-01-01  mkt_excess  8.44e-01    1.200824
    2877  93436 2015-02-01       alpha  4.17e-02    1.520897
    2878  93436 2015-02-01  mkt_excess  7.86e-01    1.142578
    2879  93436 2015-03-01       alpha  3.93e-02    1.462313
    2880  93436 2015-03-01  mkt_excess  8.22e-01    1.207402
    2881  93436 2015-04-01       alpha  4.22e-02    1.589449
    2882  93436 2015-04-01  mkt_excess  8.04e-01    1.185751
    2883  93436 2015-05-01       alpha  4.32e-02    1.651234
    2884  93436 2015-05-01  mkt_excess  8.04e-01    1.194276
    2885  93436 2015-06-01       alpha  4.40e-02    1.719828
    2886  93436 2015-06-01  mkt_excess  7.89e-01    1.189018
    2887  93436 2015-07-01       alpha  4.47e-02    1.788351
    2888  93436 2015-07-01  mkt_excess  9.84e-01    1.479209
    2889  93436 2015-08-01       alpha  4.38e-02    1.757621
    2890  93436 2015-08-01  mkt_excess  1.02e+00    1.542449
    2891  93436 2015-09-01       alpha  4.38e-02    1.787236
    2892  93436 2015-09-01  mkt_excess  1.14e+00    1.670450
    2893  93436 2015-10-01       alpha  4.21e-02    1.677172
    2894  93436 2015-10-01  mkt_excess  8.78e-01    1.295504
    2895  93436 2015-11-01       alpha  3.33e-02    1.448820
    2896  93436 2015-11-01  mkt_excess  9.17e-01    1.479604
    2897  93436 2015-12-01       alpha  3.68e-02    1.683301
    2898  93436 2015-12-01  mkt_excess  1.18e+00    1.949168
    2899  93436 2016-01-01       alpha  3.50e-02    1.616819
    2900  93436 2016-01-01  mkt_excess  1.35e+00    2.296928
    2901  93436 2016-02-01       alpha  3.57e-02    1.659643
    2902  93436 2016-02-01  mkt_excess  1.39e+00    2.353376
    2903  93436 2016-03-01       alpha  3.43e-02    1.589328
    2904  93436 2016-03-01  mkt_excess  1.44e+00    2.516039
    2905  93436 2016-04-01       alpha  3.54e-02    1.650886
    2906  93436 2016-04-01  mkt_excess  1.46e+00    2.551328
    2907  93436 2016-05-01       alpha  3.19e-02    1.474124
    2908  93436 2016-05-01  mkt_excess  1.47e+00    2.547212
    2909  93436 2016-06-01       alpha  3.12e-02    1.438626
    2910  93436 2016-06-01  mkt_excess  1.47e+00    2.526985
    2911  93436 2016-07-01       alpha  3.21e-02    1.466580
    2912  93436 2016-07-01  mkt_excess  1.46e+00    2.510714
    2913  93436 2016-08-01       alpha  3.15e-02    1.418690
    2914  93436 2016-08-01  mkt_excess  1.41e+00    2.330773
    2915  93436 2016-09-01       alpha  2.81e-02    1.236878
    2916  93436 2016-09-01  mkt_excess  1.50e+00    2.352446
    2917  93436 2016-10-01       alpha  2.74e-02    1.225018
    2918  93436 2016-10-01  mkt_excess  1.51e+00    2.187186
    2919  93436 2016-11-01       alpha  2.43e-02    1.074602
    2920  93436 2016-11-01  mkt_excess  1.44e+00    2.102177
    2921  93436 2016-12-01       alpha  2.84e-02    1.260470
    2922  93436 2016-12-01  mkt_excess  1.43e+00    2.110211
    2923  93436 2017-01-01       alpha  3.09e-02    1.376199
    2924  93436 2017-01-01  mkt_excess  1.51e+00    2.187851
    2925  93436 2017-02-01       alpha  2.93e-02    1.304382
    2926  93436 2017-02-01  mkt_excess  1.44e+00    2.071885
    2927  93436 2017-03-01       alpha  3.03e-02    1.352911
    2928  93436 2017-03-01  mkt_excess  1.41e+00    2.021807
    2929  93436 2017-04-01       alpha  3.44e-02    1.532867
    2930  93436 2017-04-01  mkt_excess  1.36e+00    1.954052
    2931  93436 2017-05-01       alpha  3.71e-02    1.621384
    2932  93436 2017-05-01  mkt_excess  1.27e+00    1.731347
    2933  93436 2017-06-01       alpha  3.76e-02    1.651727
    2934  93436 2017-06-01  mkt_excess  1.28e+00    1.737449
    2935  93436 2017-07-01       alpha  3.81e-02    1.671451
    2936  93436 2017-07-01  mkt_excess  1.25e+00    1.687117
    2937  93436 2017-08-01       alpha  3.96e-02    1.745144
    2938  93436 2017-08-01  mkt_excess  1.24e+00    1.679457
    2939  93436 2017-09-01       alpha  3.87e-02    1.699662
    2940  93436 2017-09-01  mkt_excess  1.23e+00    1.651302
    2941  93436 2017-10-01       alpha  3.88e-02    1.681941
    2942  93436 2017-10-01  mkt_excess  1.17e+00    1.560273
    2943  93436 2017-11-01       alpha  3.43e-02    1.483397
    2944  93436 2017-11-01  mkt_excess  1.13e+00    1.514291
    2945  93436 2017-12-01       alpha  3.44e-02    1.488802
    2946  93436 2017-12-01  mkt_excess  1.13e+00    1.515783
    2947  93436 2018-01-01       alpha  3.46e-02    1.494733
    2948  93436 2018-01-01  mkt_excess  1.16e+00    1.550602
    2949  93436 2018-02-01       alpha  3.58e-02    1.579374
    2950  93436 2018-02-01  mkt_excess  1.18e+00    1.632382
    2951  93436 2018-03-01       alpha  3.02e-02    1.325579
    2952  93436 2018-03-01  mkt_excess  1.34e+00    1.823168
    2953  93436 2018-04-01       alpha  2.56e-02    1.179584
    2954  93436 2018-04-01  mkt_excess  1.29e+00    1.839936
    2955  93436 2018-05-01       alpha  1.46e-02    0.864377
    2956  93436 2018-05-01  mkt_excess  1.01e+00    1.834111
    2957  93436 2018-06-01       alpha  1.59e-02    0.920731
    2958  93436 2018-06-01  mkt_excess  1.03e+00    1.839699
    2959  93436 2018-07-01       alpha  1.25e-02    0.728731
    2960  93436 2018-07-01  mkt_excess  7.79e-01    1.374063
    2961  93436 2018-08-01       alpha  5.24e-03    0.314490
    2962  93436 2018-08-01  mkt_excess  9.81e-01    1.790426
    2963  93436 2018-09-01       alpha  1.71e-03    0.102699
    2964  93436 2018-09-01  mkt_excess  9.51e-01    1.719164
    2965  93436 2018-10-01       alpha  1.47e-02    0.880334
    2966  93436 2018-10-01  mkt_excess  5.34e-01    0.995954
    2967  93436 2018-11-01       alpha  1.80e-02    1.114040
    2968  93436 2018-11-01  mkt_excess  6.36e-01    1.220831
    2969  93436 2018-12-01       alpha  1.56e-02    1.000340
    2970  93436 2018-12-01  mkt_excess  6.02e-01    1.278241
    2971  93436 2019-01-01       alpha  9.94e-03    0.640408
    2972  93436 2019-01-01  mkt_excess  5.70e-01    1.268916
    2973  93436 2019-02-01       alpha  6.37e-03    0.439957
    2974  93436 2019-02-01  mkt_excess  3.94e-01    0.934292
    2975  93436 2019-03-01       alpha  6.81e-03    0.472015
    2976  93436 2019-03-01  mkt_excess  3.79e-01    0.902216
    2977  93436 2019-04-01       alpha  4.83e-03    0.326319
    2978  93436 2019-04-01  mkt_excess  3.00e-01    0.703988
    2979  93436 2019-05-01       alpha -1.90e-04   -0.012666
    2980  93436 2019-05-01  mkt_excess  5.23e-01    1.245003
    2981  93436 2019-06-01       alpha -6.04e-04   -0.039922
    2982  93436 2019-06-01  mkt_excess  6.25e-01    1.517184
    2983  93436 2019-07-01       alpha  1.71e-03    0.112191
    2984  93436 2019-07-01  mkt_excess  6.07e-01    1.462643
    2985  93436 2019-08-01       alpha -1.74e-03   -0.117915
    2986  93436 2019-08-01  mkt_excess  5.48e-01    1.349059
    2987  93436 2019-09-01       alpha  9.48e-04    0.064060
    2988  93436 2019-09-01  mkt_excess  5.20e-01    1.277951
    2989  93436 2019-10-01       alpha  5.72e-03    0.365537
    2990  93436 2019-10-01  mkt_excess  5.73e-01    1.330810
    2991  93436 2019-11-01       alpha  6.09e-03    0.389043
    2992  93436 2019-11-01  mkt_excess  5.82e-01    1.358030
    2993  93436 2019-12-01       alpha  1.13e-02    0.701031
    2994  93436 2019-12-01  mkt_excess  6.31e-01    1.431123
    2995  93436 2020-01-01       alpha  2.28e-02    1.217112
    2996  93436 2020-01-01  mkt_excess  5.12e-01    0.997829
    2997  93436 2020-02-01       alpha  2.45e-02    1.332865
    2998  93436 2020-02-01  mkt_excess  5.00e-01    1.009224
    2999  93436 2020-03-01       alpha  2.20e-02    1.198375
    3000  93436 2020-03-01  mkt_excess  7.30e-01    1.628870
    3001  93436 2020-04-01       alpha  2.23e-02    1.166681
    3002  93436 2020-04-01  mkt_excess  1.14e+00    2.663202
    3003  93436 2020-05-01       alpha  2.09e-02    1.092618
    3004  93436 2020-05-01  mkt_excess  1.13e+00    2.669010
    3005  93436 2020-06-01       alpha  2.35e-02    1.197766
    3006  93436 2020-06-01  mkt_excess  1.18e+00    2.715480
    3007  93436 2020-07-01       alpha  2.73e-02    1.362290
    3008  93436 2020-07-01  mkt_excess  1.28e+00    2.914698
    3009  93436 2020-08-01       alpha  3.41e-02    1.491994
    3010  93436 2020-08-01  mkt_excess  1.60e+00    3.235796
    3011  93436 2020-09-01       alpha  3.14e-02    1.369852
    3012  93436 2020-09-01  mkt_excess  1.65e+00    3.332078
    3013  93436 2020-10-01       alpha  3.32e-02    1.505892
    3014  93436 2020-10-01  mkt_excess  1.87e+00    3.826504
    3015  93436 2020-11-01       alpha  3.32e-02    1.484957
    3016  93436 2020-11-01  mkt_excess  2.04e+00    4.370083
    3017  93436 2020-12-01       alpha  3.37e-02    1.492494
    3018  93436 2020-12-01  mkt_excess  2.09e+00    4.450142
    3019  93436 2021-01-01       alpha  3.83e-02    1.686749
    3020  93436 2021-01-01  mkt_excess  2.01e+00    4.212060
    3021  93436 2021-02-01       alpha  3.52e-02    1.521205
    3022  93436 2021-02-01  mkt_excess  1.98e+00    4.076406
    3023  93436 2021-03-01       alpha  3.34e-02    1.440947
    3024  93436 2021-03-01  mkt_excess  1.95e+00    3.962638
    3025  93436 2021-04-01       alpha  3.26e-02    1.399787
    3026  93436 2021-04-01  mkt_excess  1.93e+00    3.937728
    3027  93436 2021-05-01       alpha  3.21e-02    1.376469
    3028  93436 2021-05-01  mkt_excess  1.95e+00    3.971165
    3029  93436 2021-06-01       alpha  3.36e-02    1.440015
    3030  93436 2021-06-01  mkt_excess  1.94e+00    3.958537
    3031  93436 2021-07-01       alpha  3.28e-02    1.410184
    3032  93436 2021-07-01  mkt_excess  1.94e+00    3.948168
    3033  93436 2021-08-01       alpha  3.50e-02    1.509423
    3034  93436 2021-08-01  mkt_excess  1.93e+00    3.945547
    3035  93436 2021-09-01       alpha  3.88e-02    1.680753
    3036  93436 2021-09-01  mkt_excess  1.87e+00    3.880105
    3037  93436 2021-10-01       alpha  4.24e-02    1.782823
    3038  93436 2021-10-01  mkt_excess  1.97e+00    4.029608
    3039  93436 2021-11-01       alpha  4.50e-02    1.924912
    3040  93436 2021-11-01  mkt_excess  2.02e+00    4.157115
    3041  93436 2021-12-01       alpha  4.15e-02    1.756994
    3042  93436 2021-12-01  mkt_excess  1.99e+00    4.068090
    3043  93436 2022-01-01       alpha  3.91e-02    1.677282
    3044  93436 2022-01-01  mkt_excess  2.00e+00    4.196053
    3045  93436 2022-02-01       alpha  3.96e-02    1.710498
    3046  93436 2022-02-01  mkt_excess  2.04e+00    4.298470
    3047  93436 2022-03-01       alpha  4.04e-02    1.733841
    3048  93436 2022-03-01  mkt_excess  2.06e+00    4.336433
    3049  93436 2022-04-01       alpha  3.84e-02    1.666027
    3050  93436 2022-04-01  mkt_excess  2.09e+00    4.577835
    3051  93436 2022-05-01       alpha  3.51e-02    1.515904
    3052  93436 2022-05-01  mkt_excess  2.11e+00    4.582785
    3053  93436 2022-06-01       alpha  3.56e-02    1.548532
    3054  93436 2022-06-01  mkt_excess  2.09e+00    4.678705
    3055  93436 2022-07-01       alpha  3.95e-02    1.724561
    3056  93436 2022-07-01  mkt_excess  2.15e+00    4.953152
    3057  93436 2022-08-01       alpha  3.79e-02    1.660007
    3058  93436 2022-08-01  mkt_excess  2.16e+00    5.018467
    3059  93436 2022-09-01       alpha  4.27e-02    1.883150
    3060  93436 2022-09-01  mkt_excess  2.10e+00    5.036811
    3061  93436 2022-10-01       alpha  3.99e-02    1.706118
    3062  93436 2022-10-01  mkt_excess  1.97e+00    4.654474
    3063  93436 2022-11-01       alpha  3.84e-02    1.621214
    3064  93436 2022-11-01  mkt_excess  1.93e+00    4.520201
    3065  93436 2022-12-01       alpha  3.37e-02    1.398497
    3066  93436 2022-12-01  mkt_excess  2.04e+00    4.755327
    3067  93436 2023-01-01       alpha  3.72e-02    1.522539
    3068  93436 2023-01-01  mkt_excess  2.12e+00    4.882733
    3069  93436 2023-02-01       alpha  4.07e-02    1.649799
    3070  93436 2023-02-01  mkt_excess  2.09e+00    4.745391
    3071  93436 2023-03-01       alpha  4.33e-02    1.766999
    3072  93436 2023-03-01  mkt_excess  2.04e+00    4.677644
    3073  93436 2023-04-01       alpha  3.78e-02    1.520964
    3074  93436 2023-04-01  mkt_excess  2.05e+00    4.615054
    3075  93436 2023-05-01       alpha  4.31e-02    1.724991
    3076  93436 2023-05-01  mkt_excess  2.05e+00    4.605399
    3077  93436 2023-06-01       alpha  4.20e-02    1.681654
    3078  93436 2023-06-01  mkt_excess  2.09e+00    4.734649
    3079  93436 2023-07-01       alpha  4.43e-02    1.795880
    3080  93436 2023-07-01  mkt_excess  2.11e+00    4.833084
    3081  93436 2023-08-01       alpha  4.53e-02    1.847566
    3082  93436 2023-08-01  mkt_excess  2.13e+00    4.898195
    3083  93436 2023-09-01       alpha  4.88e-02    2.008324
    3084  93436 2023-09-01  mkt_excess  2.11e+00    4.942464
    3085  93436 2023-10-01       alpha  3.75e-02    1.591810
    3086  93436 2023-10-01  mkt_excess  2.33e+00    5.546110
    3087  93436 2023-11-01       alpha  3.75e-02    1.585808
    3088  93436 2023-11-01  mkt_excess  2.31e+00    5.588609
    3089  93436 2023-12-01       alpha  3.27e-02    1.372820
    3090  93436 2023-12-01  mkt_excess  2.36e+00    5.574965
    3091  93436 2024-01-01       alpha  3.14e-02    1.327298
    3092  93436 2024-01-01  mkt_excess  2.49e+00    5.819046
    3093  93436 2024-02-01       alpha  3.14e-02    1.324531
    3094  93436 2024-02-01  mkt_excess  2.48e+00    5.813784
    3095  93436 2024-03-01       alpha  3.08e-02    1.292028
    3096  93436 2024-03-01  mkt_excess  2.46e+00    5.733291
    3097  93436 2024-04-01       alpha  3.74e-02    1.603127
    3098  93436 2024-04-01  mkt_excess  2.47e+00    5.892425
    3099  93436 2024-05-01       alpha  3.69e-02    1.560252
    3100  93436 2024-05-01  mkt_excess  2.39e+00    5.595736
    3101  93436 2024-06-01       alpha  3.69e-02    1.565370
    3102  93436 2024-06-01  mkt_excess  2.39e+00    5.545202
    3103  93436 2024-07-01       alpha  3.84e-02    1.623687
    3104  93436 2024-07-01  mkt_excess  2.39e+00    5.531564
    3105  93436 2024-08-01       alpha  3.67e-02    1.536670
    3106  93436 2024-08-01  mkt_excess  2.38e+00    5.444790
    3107  93436 2024-09-01       alpha  3.90e-02    1.626881
    3108  93436 2024-09-01  mkt_excess  2.38e+00    5.427937
    3109  93436 2024-10-01       alpha  3.44e-02    1.453088
    3110  93436 2024-10-01  mkt_excess  2.38e+00    5.488947
    3111  93436 2024-11-01       alpha  3.80e-02    1.593754
    3112  93436 2024-11-01  mkt_excess  2.45e+00    5.645404
    3113  93436 2024-12-01       alpha  3.95e-02    1.654254
    3114  93436 2024-12-01  mkt_excess  2.39e+00    5.496126

## Python

``` python
capm_examples = roll_capm_estimation(
    crsp_monthly.filter(pl.col("permno").is_in(examples["permno"]))
)
capm_examples
```

shape: (3_114, 5)

| permno  | date       | coefficient  | estimate | t_statistic |
|---------|------------|--------------|----------|-------------|
| f64     | date       | str          | f64      | f64         |
| 10107.0 | 1990-03-01 | "alpha"      | 0.041709 | 2.309559    |
| 10107.0 | 1990-03-01 | "mkt_excess" | 1.397341 | 4.173565    |
| 10107.0 | 1990-04-01 | "alpha"      | 0.042693 | 2.413745    |
| 10107.0 | 1990-04-01 | "mkt_excess" | 1.385035 | 4.197367    |
| 10107.0 | 1990-05-01 | "alpha"      | 0.044259 | 2.533768    |
| …       | …          | …            | …        | …           |
| 93436.0 | 2024-10-01 | "mkt_excess" | 2.380626 | 5.488947    |
| 93436.0 | 2024-11-01 | "alpha"      | 0.038041 | 1.593754    |
| 93436.0 | 2024-11-01 | "mkt_excess" | 2.451092 | 5.645404    |
| 93436.0 | 2024-12-01 | "alpha"      | 0.039498 | 1.654254    |
| 93436.0 | 2024-12-01 | "mkt_excess" | 2.385498 | 5.496126    |

[Figure 1](#fig-601) displays the resulting beta estimates, focusing exclusively on the coefficient of `"mkt_excess"`.

## R

``` r
beta_examples <- capm_examples |>
  left_join(examples, join_by(permno)) |>
  filter(coefficient == "mkt_excess")

beta_examples |>
  ggplot(aes(x = date, y = estimate, color = company, linetype = company)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "Monthly beta estimates for example stocks using 5 years of data"
  )
```

[![Title: Monthly beta estimates for example stocks using five years of data. The figure shows a time series of beta estimates based on five years of monthly data for Apple, Berkshire Hathaway, Microsoft, and Tesla. The estimated betas vary over time and across stocks but are always positive for each stock.](beta-estimation_files/figure-html/fig-601-1.png)](beta-estimation_files/figure-html/fig-601-1.png "Figure 1: The figure shows monthly beta estimates for example stocks using five years of data. The CAPM betas are estimated with monthly data and a rolling window of length five years based on adjusted excess returns from CRSP. We use market excess returns from Kenneth French data library.")

Figure 1: The figure shows monthly beta estimates for example stocks using five years of data. The CAPM betas are estimated with monthly data and a rolling window of length five years based on adjusted excess returns from CRSP. We use market excess returns from Kenneth French data library.

## Python

``` python
beta_examples = (capm_examples
    .join(examples, how="left", on="permno")
    .filter(pl.col("coefficient") == "mkt_excess")
)

beta_figure = (
    ggplot(
        beta_examples,
        aes(x="date", y="estimate", color="company", linetype="company"),
    )
    + geom_line()
    + labs(
        x="",
        y="",
        color="",
        linetype="",
        title="Monthly beta estimates for example stocks using 5 years of data",
    )
    + scale_x_date(date_breaks="5 year", date_labels="%Y")
)
beta_figure.show()
```

[![Title: Monthly beta estimates for example stocks using five years of data. The figure shows a time series of beta estimates based on five years of monthly data for Apple, Berkshire Hathaway, Microsoft, and Tesla. The estimated betas vary over time and across stocks but are always positive for each stock.](beta-estimation_files/figure-html/beta-estimation-fig-601-py-1.png)](beta-estimation_files/figure-html/beta-estimation-fig-601-py-1.png "The figure shows monthly beta estimates for example stocks using five years of data. The CAPM betas are estimated with monthly data and a rolling window of length five years based on adjusted excess returns from CRSP. We use market excess returns from Kenneth French data library.")

The figure shows monthly beta estimates for example stocks using five years of data. The CAPM betas are estimated with monthly data and a rolling window of length five years based on adjusted excess returns from CRSP. We use market excess returns from Kenneth French data library.

## Estimating Beta for the Whole Sample

Because `roll_capm_estimation()` estimates the betas in closed form and in a single vectorized pass, we can now apply it directly to the whole CRSP sample. The estimation for our sample of around 25k stocks finishes in a few seconds on a typical laptop—no explicit loops over stocks and no parallelization required.

> **NOTE:**
>
> We could distribute the estimation across several cores, but for the closed-form CAPM estimation the parallelization overhead is not worth it. We revisit parallelization in [Size Sorts and p-Hacking](../chapters/size-sorts-and-p-hacking.llms.md), where repeatedly sorting portfolios across many specifications makes it pay off.

## R

``` r
capm_monthly <- roll_capm_estimation(crsp_monthly)
capm_monthly
```

Instead of implementing the rolling-window estimation by hand, you can also use the `estimate_betas()` function from the `tidyfinance` package, which is built precisely for this task:

``` r
library(tidyfinance)

estimate_betas(
  crsp_monthly,
  "ret_excess ~ mkt_excess",
  lookback = months(60),
  min_obs = 48
)
```

## Python

``` python
capm_monthly = roll_capm_estimation(crsp_monthly)
capm_monthly
```

## Estimating Beta Using Daily Returns

Before we provide some descriptive statistics of our beta estimates, we implement the estimation for the daily CRSP sample as well. Depending on the application, you might either use longer horizon beta estimates based on monthly data or shorter horizon estimates based on daily returns. As loading the full daily CRSP data requires relatively large amounts of memory, we split the beta estimation into smaller chunks. The logic follows the approach that we use to download the daily CRSP data (see [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md)).

First, we load the daily Fama-French market excess returns.

## R

``` r
factors_ff3_daily <- read_parquet("data/factors_ff3_daily.parquet") |>
  select(date, mkt_excess)
```

## Python

``` python
factors_ff3_daily = (pl.read_parquet("data/factors_ff3_daily.parquet")
    .select(["date", "mkt_excess"])
)
```

We then create a connection to the daily CRSP data, but we don’t load the whole table into our memory. We only extract all distinct `permno` because we loop the beta estimation over batches of stocks with size 500. To estimate the CAPM over a consistent lookback window while accommodating different return frequencies, we adjust the minimum required number of observations accordingly. Specifically, we require at least 1,000 daily returns over a five‑year period for a valid estimation. This threshold is consistent with the monthly requirement of 48 observations out of 60 months, given that there are roughly 252 trading days in a year.

## R

``` r
permnos <- list.dirs(
  "data/crsp_daily", full.names = FALSE, recursive = FALSE
) |>
  str_remove("permno=") |>
  as.integer()

batch_size <- 500
batches <- ceiling(length(permnos) / batch_size)
min_obs <- 1000
```

## Python

``` python
permnos = [
    int(folder.split("=")[1])
    for folder in os.listdir("data/crsp_daily")
    if folder.startswith("permno=")
]

batch_size = 500
batches = np.ceil(len(permnos) / batch_size).astype(int)
min_obs = 1_000
```

We then proceed to perform the same steps as with the monthly CRSP data, just in batches: We load the daily returns, truncate the daily dates to the beginning of the month, and hand each batch to `roll_capm_estimation()`, which again estimates the betas for all stocks in the batch at once.

## R

``` r
capm_daily <- list()

for (j in 1:batches) {
  permno_batch <- permnos[
    ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
  ]

  crsp_daily_sub <- permno_batch |>
    map(\(p) {
      paste0("data/crsp_daily/permno=", p) |>
        list.files(full.names = TRUE) |>
        map(read_parquet) |>
        list_rbind() |>
        mutate(permno = p)
    }) |>
    list_rbind() |>
    select(permno, date, ret_excess)

  capm_daily[[j]] <- crsp_daily_sub |>
    inner_join(factors_ff3_daily, join_by(date)) |>
    mutate(date = floor_date(date, "month")) |>
    roll_capm_estimation(min_obs = min_obs)

  message(
    "Batch ",
    j,
    " out of ",
    batches,
    " done (",
    scales::percent(j / batches),
    ")\n"
  )
}
capm_daily <- bind_rows(capm_daily)
```

## Python

Note that we also convert the daily date to the beginning of the month so that we can still look back over 60 months and get one beta estimate per month, even though we are using daily data.

``` python
capm_daily = []

for j in range(1, batches + 1):
    permno_batch = permnos[((j - 1) * batch_size) : (min(j * batch_size, len(permnos)))]

    crsp_daily_sub = (
        pl.scan_parquet("data/crsp_daily", hive_partitioning=True)
        .filter(pl.col("permno").is_in(permno_batch))
        .select(["permno", "date", "ret_excess"])
        .collect()
        # hive partition keys are read as integers; align with crsp_monthly
        .with_columns(permno=pl.col("permno").cast(pl.Float64))
        .join(factors_ff3_daily, how="inner", on="date")
        .with_columns(date=pl.col("date").dt.truncate("1mo"))
    )

    capm_daily_sub = roll_capm_estimation(crsp_daily_sub, min_obs=min_obs)
    capm_daily.append(capm_daily_sub)

    print(f"Batch {j} out of {batches} done ({(j / batches) * 100:.2f}%)\n")

capm_daily = pl.concat(capm_daily)
```

## Comparing Beta Estimates

What is a typical value for stock betas? First, let us extract the relevant estimates from our CAPM results based on monthly returns.

## R

``` r
beta_monthly <- capm_monthly |>
  filter(coefficient == "mkt_excess") |>
  select(permno, date, beta = estimate) |>
  mutate(return_type = "monthly")
```

## Python

``` python
beta_monthly = (capm_monthly
    .filter(pl.col("coefficient") == "mkt_excess")
    .select(["permno", "date", "estimate"])
    .rename({"estimate": "beta"})
    .with_columns(return_type=pl.lit("monthly"))
)
```

To get some feeling, we illustrate the dispersion of the estimated \\\hat\beta_i\\ across different industries and across time below. [Figure 2](#fig-602) shows that typical business models across industries imply different exposure to the general market economy. However, there are barely any firms that exhibit a negative exposure to the market factor.

## R

``` r
crsp_monthly |>
  left_join(beta_monthly, join_by(permno, date)) |>
  drop_na(beta) |>
  group_by(industry, permno) |>
  summarize(beta = mean(beta), .groups = "drop") |>
  ggplot(aes(x = reorder(industry, beta, FUN = median), y = beta)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL,
    title = "Firm-specific beta distributions by industry"
  )
```

[![Title: Firm-specific beta distributions by industry. The figure shows box plots for each industry. Firms with the highest average CAPM beta belong to the public administration industry. Firms from the utility sector have the lowest average CAPM beta. The figure indicates very few outliers with negative CAPM betas. The large majority of all stocks has CAPM betas between 0.5 and 1.5.](beta-estimation_files/figure-html/fig-602-1.png)](beta-estimation_files/figure-html/fig-602-1.png "Figure 2: The box plots show the average firm-specific beta estimates by industry.")

Figure 2: The box plots show the average firm-specific beta estimates by industry.

## Python

``` python
beta_industries = (beta_monthly
    .join(crsp_monthly, how="inner", on=["permno", "date"])
    .drop_nulls("beta")
    .group_by(["industry", "permno"])
    .agg(beta=pl.col("beta").mean())
)

industry_order = (beta_industries
    .group_by("industry")
    .agg(beta=pl.col("beta").median())
    .sort("beta")
    ["industry"].to_list()
)

beta_industries_figure = (
    ggplot(
        beta_industries,
        aes(x="industry", y="beta")
    )
    + geom_boxplot()
    + coord_flip()
    + labs(
        x="",
        y="",
        title="Firm-specific beta distributions by industry"
        )
    + scale_x_discrete(limits=industry_order)
)
beta_industries_figure.show()
```

[![Title: Firm-specific beta distributions by industry. The figure shows box plots for each industry. Firms with the highest average CAPM beta belong to the public administration industry. Firms from the utility sector have the lowest average CAPM beta. The figure indicates very few outliers with negative CAPM betas. The large majority of all stocks has CAPM betas between 0.5 and 1.5.](beta-estimation_files/figure-html/beta-estimation-fig-602-py-1.png)](beta-estimation_files/figure-html/beta-estimation-fig-602-py-1.png "The box plots show the average firm-specific beta estimates by industry.")

The box plots show the average firm-specific beta estimates by industry.

Next, we illustrate the time-variation in the cross-section of estimated betas. [Figure 3](#fig-603) shows the monthly deciles of estimated betas (based on monthly data) and indicates an interesting pattern: First, betas seem to vary over time in the sense that during some periods, there is a clear trend across all deciles. Second, the sample exhibits periods where the dispersion across stocks increases in the sense that the lower decile decreases and the upper decile increases, which indicates that for some stocks the correlation with the market increases while for others it decreases. Note also here: stocks with negative betas are a rare exception.

## R

``` r
beta_monthly |>
  group_by(date) |>
  reframe(
    x = quantile(beta, seq(0.1, 0.9, 0.1)),
    quantile = 100 * seq(0.1, 0.9, 0.1)
  ) |>
  ggplot(aes(
    x = date,
    y = x,
    color = as_factor(quantile),
    linetype = as_factor(quantile)
  )) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "Monthly deciles of estimated betas",
  )
```

[![Title: Monthly deciles of estimated betas. The figure shows time series of deciles of estimated betas to illustrate the distribution of betas over time. The top ten percent quantile on average is around two but varies substantially over time. The lowest ten percent quantile is around 0.4 on average but is highly correlated with the top quantile such that in general CAPM market betas seem to go up and down jointly.](beta-estimation_files/figure-html/fig-603-3.png)](beta-estimation_files/figure-html/fig-603-3.png "Figure 3: The figure shows monthly deciles of estimated betas. Each line corresponds to the monthly cross-sectional quantile of the estimated CAPM beta.")

Figure 3: The figure shows monthly deciles of estimated betas. Each line corresponds to the monthly cross-sectional quantile of the estimated CAPM beta.

## Python

``` python
quantiles = np.arange(0.1, 1.0, 0.1)

beta_quantiles = (
    beta_monthly
    .group_by("date")
    .agg([
        pl.col("beta").quantile(q, interpolation="linear").alias(str(int(round(q * 100))))
        for q in quantiles
    ])
    .unpivot(index="date", variable_name="quantile", value_name="beta")
    .with_columns(pl.col("quantile").cast(pl.Int64))
    .drop_nulls()
)

linetypes = ["-", "--", "-.", ":"]
n_quantiles = beta_quantiles["quantile"].n_unique()

beta_quantiles_figure = (
    ggplot(
        beta_quantiles,
        aes(x="date", y="beta", color="factor(quantile)", linetype="factor(quantile)"),
    )
    + geom_line()
    + labs(
        x="", y="", color="", linetype="", title="Monthly deciles of estimated betas"
    )
    + scale_x_date(date_breaks="5 year", date_labels="%Y")
    + scale_linetype_manual(
        values=[linetypes[l % len(linetypes)] for l in range(n_quantiles)]
    )
)
beta_quantiles_figure.show()
```

[![Title: Monthly deciles of estimated betas. The figure shows time series of deciles of estimated betas to illustrate the distribution of betas over time. The top ten percent quantile on average is around two but varies substantially over time. The lowest ten percent quantile is around 0.4 on average but is highly correlated with the top quantile such that in general CAPM market betas seem to go up and down jointly.](beta-estimation_files/figure-html/beta-estimation-fig-603-py-1.png)](beta-estimation_files/figure-html/beta-estimation-fig-603-py-1.png "The figure shows monthly deciles of estimated betas. Each line corresponds to the monthly cross-sectional quantile of the estimated CAPM beta.")

The figure shows monthly deciles of estimated betas. Each line corresponds to the monthly cross-sectional quantile of the estimated CAPM beta.

To compare the difference between daily and monthly data, we combine beta estimates to a single table.

## R

``` r
beta_daily <- capm_daily |>
  filter(coefficient == "mkt_excess") |>
  select(permno, date, beta = estimate) |>
  mutate(return_type = "daily")

beta <- bind_rows(beta_monthly, beta_daily)
```

## Python

``` python
beta_daily = (capm_daily
    .filter(pl.col("coefficient") == "mkt_excess")
    .select(["permno", "date", "estimate"])
    .rename({"estimate": "beta"})
    .with_columns(return_type=pl.lit("daily"))
)

beta = pl.concat([beta_monthly, beta_daily])
```

Then, we use the table to plot a comparison of beta estimates for our example stocks in [Figure 4](#fig-604).

## R

``` r
beta |>
  inner_join(examples, join_by(permno)) |>
  ggplot(aes(
    x = date,
    y = beta,
    color = return_type,
    linetype = return_type
  )) +
  geom_line() +
  facet_wrap(~company, ncol = 1) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "Comparison of beta estimates using monthly and daily data"
  )
```

[![Title: Comparison of beta estimates using monthly and daily data. The figure shows a time series of beta estimates using five years of monthly versus daily data for Apple, Berkshire Hathaway, Microsoft, and Tesla. The estimates based on monthly data are smooth relative to the estimates based on daily data. However, the general trend and level is similar, irrespective of the choice of frequency.](beta-estimation_files/figure-html/fig-604-1.png)](beta-estimation_files/figure-html/fig-604-1.png "Figure 4: The figure shows the comparison of beta estimates using monthly and daily data. CAPM betas are computed using five years of monthly or daily data. The two lines show the monthly estimates based on a rolling window for few exemplary stocks.")

Figure 4: The figure shows the comparison of beta estimates using monthly and daily data. CAPM betas are computed using five years of monthly or daily data. The two lines show the monthly estimates based on a rolling window for few exemplary stocks.

## Python

``` python
beta_comparison = beta.join(examples, how="inner", on="permno")

beta_comparison_figure = (
    ggplot(
        beta_comparison,
        aes(x="date", y="beta", color="return_type", linetype="return_type"),
    )
    + geom_line()
    + facet_wrap("~company", ncol=1)
    + labs(
        x="",
        y="",
        color="",
        linetype="",
        title="Comparison of beta estimates using monthly and daily data",
    )
    + scale_x_date(date_breaks="10 years", date_labels="%Y")
    + theme(figure_size=(6.4, 6.4))
)
beta_comparison_figure.show()
```

[![Title: Comparison of beta estimates using monthly and daily data. The figure shows a time series of beta estimates using five years of monthly versus daily data for Apple, Berkshire Hathaway, Microsoft, and Tesla. The estimates based on monthly data are smooth relative to the estimates based on daily data. However, the general trend and level is similar, irrespective of the choice of frequency.](beta-estimation_files/figure-html/beta-estimation-fig-604-py-1.png)](beta-estimation_files/figure-html/beta-estimation-fig-604-py-1.png "The figure shows the comparison of beta estimates using monthly and daily data. CAPM betas are computed using five years of monthly or daily data. The two lines show the monthly estimates based on a rolling window for few exemplary stocks.")

The figure shows the comparison of beta estimates using monthly and daily data. CAPM betas are computed using five years of monthly or daily data. The two lines show the monthly estimates based on a rolling window for few exemplary stocks.

The estimates in [Figure 4](#fig-604) look as expected. As you can see, it really depends on the data frequency how your beta estimates turn out because the estimates based on daily data are much smoother due to the higher number of observations in each regression.

Finally, we write the estimates to our local folder such that we can use them in later chapters.

## R

``` r
write_parquet(beta, "data/beta.parquet")
```

## Python

``` python
beta.write_parquet("data/beta.parquet")
```

Whenever you perform some kind of estimation, it also makes sense to do rough plausibility tests. A possible check is to plot the share of stocks with beta estimates over time. This descriptive helps us discover potential errors in our data preparation or estimation procedure. For instance, suppose there was a gap in our output where we do not have any betas. In this case, we would have to go back and check all previous steps to find out what went wrong.

## R

``` r
beta_coverage <- crossing(
  crsp_monthly,
  tibble(return_type = c("monthly", "daily"))
) |>
  left_join(beta, join_by(permno, date, return_type)) |>
  group_by(date, return_type) |>
  summarize(share = sum(!is.na(beta)) / n(), .groups = "drop")

beta_coverage |>
  ggplot(
    aes(x = date, y = share, color = return_type, linetype = return_type)
  ) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    title = "End-of-month share of securities with beta estimates"
  ) +
  coord_cartesian(ylim = c(0, 1))
```

[![Title: End-of-month share of securities with beta estimates. The figure shows two time series with end-of-year shares of securities with beta estimates using five years of monthly or daily data. There is almost no missing data for the estimates based on daily data. For the beta estimates based on monthly data, around 75 percent of all stock-month combinations provide sufficient long historical periods to estimate the beta.](beta-estimation_files/figure-html/fig-605-1.png)](beta-estimation_files/figure-html/fig-605-1.png "Figure 5: The figure shows end-of-month share of securities with beta estimates. The two lines show the share of securities with beta estimates using five years of monthly or daily data.")

Figure 5: The figure shows end-of-month share of securities with beta estimates. The two lines show the share of securities with beta estimates using five years of monthly or daily data.

## Python

``` python
return_types = pl.DataFrame({"return_type": ["monthly", "daily"]})

beta_coverage = (
    crsp_monthly.join(return_types, how="cross")
    .join(beta, on=["permno", "date", "return_type"], how="left")
    .group_by(["date", "return_type"])
    .agg(share=pl.col("beta").is_not_null().mean())
)

beta_coverage_figure = (
    ggplot(
        beta_coverage,
        aes(x="date", y="share", color="return_type", linetype="return_type"),
    )
    + geom_line()
    + labs(
        x="",
        y="",
        color="",
        linetype="",
        title="End-of-month share of securities with beta estimates",
    )
    + scale_y_continuous(labels=percent_format())
    + scale_x_date(date_breaks="10 year", date_labels="%Y")
)
beta_coverage_figure.show()
```

[![Title: End-of-month share of securities with beta estimates. The figure shows two time series with end-of-year shares of securities with beta estimates using five years of monthly or daily data. There is almost no missing data for the estimates based on daily data. For the beta estimates based on monthly data, around 75 percent of all stock-month combinations provide sufficient long historical periods to estimate the beta.](beta-estimation_files/figure-html/beta-estimation-fig-605-py-1.png)](beta-estimation_files/figure-html/beta-estimation-fig-605-py-1.png "The figure shows end-of-month share of securities with beta estimates. The two lines show the share of securities with beta estimates using five years of monthly or daily data.")

The figure shows end-of-month share of securities with beta estimates. The two lines show the share of securities with beta estimates using five years of monthly or daily data.

[Figure 5](#fig-605) shows no issues, as the two coverage lines track each other closely, so we can proceed to the next check.

We also encourage everyone to always look at the distributional summary statistics of variables. You can easily spot outliers or weird distributions when looking at such tables.

## R

``` r
beta |>
  group_by(return_type) |>
  summarize(
    mean = mean(beta),
    sd = sd(beta),
    min = min(beta),
    q05 = quantile(beta, 0.05),
    q50 = quantile(beta, 0.50),
    q95 = quantile(beta, 0.95),
    max = max(beta),
    n = n()
  )
```

    # A tibble: 2 × 9
      return_type  mean    sd    min    q05   q50   q95   max       n
      <chr>       <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>   <int>
    1 daily       0.794 0.495  -3.67 0.0857 0.755  1.66  4.97 2354575
    2 monthly     1.11  0.714 -13.1  0.132  1.04   2.33 11.7  2332769

## Python

``` python
(beta
    .group_by("return_type")
    .agg(
        count=pl.len(),
        mean=pl.col("beta").mean(),
        std=pl.col("beta").std(),
        min=pl.col("beta").min(),
        q05=pl.col("beta").quantile(0.05, interpolation="linear"),
        q50=pl.col("beta").quantile(0.50, interpolation="linear"),
        q95=pl.col("beta").quantile(0.95, interpolation="linear"),
        max=pl.col("beta").max(),
    )
    .sort("return_type")
    .with_columns(pl.col(pl.Float64).round(2))
)
```

shape: (2, 9)

| return_type | count   | mean | std  | min    | q05  | q50  | q95  | max   |
|-------------|---------|------|------|--------|------|------|------|-------|
| str         | u32     | f64  | f64  | f64    | f64  | f64  | f64  | f64   |
| "daily"     | 2354575 | 0.79 | 0.49 | -3.67  | 0.09 | 0.75 | 1.66 | 4.97  |
| "monthly"   | 2332769 | 1.11 | 0.71 | -13.05 | 0.13 | 1.04 | 2.33 | 11.72 |

The summary statistics indicate that estimates based on daily returns are, on average, lower and less variable than those derived from monthly returns.

Finally, since we have two different estimators for the same theoretical object, we expect the estimators should be at least positively correlated (although not perfectly as the estimators are based on different frequencies).

## R

``` r
beta |>
  pivot_wider(names_from = return_type, values_from = beta) |>
  select(monthly, daily) |>
  cor(use = "complete.obs")
```

            monthly daily
    monthly   1.000 0.618
    daily     0.618 1.000

## Python

``` python
(beta
    .pivot(index=["permno", "date"], on="return_type", values="beta")
    .drop_nulls(["monthly", "daily"])
    .select(correlation=pl.corr("monthly", "daily").round(2))
)
```

shape: (1, 1)

| correlation |
|-------------|
| f64         |
| 0.62        |

Indeed, we find a positive correlation between our beta estimates. In the subsequent chapters, we mainly use the estimates based on monthly data, as most readers should be able to replicate them due to potential memory limitations that might arise with the daily data.

## Key Takeaways

- CAPM betas can be estimated with a vectorized rolling-window approach that computes the closed-form OLS coefficients for all stocks at once, avoiding millions of individual regressions.
- Both monthly and daily return data can be used to estimate betas with different frequencies and window lengths, depending on the application.
- Summary statistics, visualization, and plausibility checks help to validate beta estimates across time and industries.

## Exercises

1.  Compute beta estimates based on monthly data using one, three, and five years of data and impose a minimum number of observations of 10, 28, and 48 months with return data, respectively. How strongly correlated are the estimated betas?
2.  Compute beta estimates based on monthly data using five years of data and impose different numbers of minimum observations. How does the share of `permno`-`date` observations with successful beta estimates vary across the different requirements? Do you find a high correlation across the estimated betas?
3.  Instead of using the closed-form estimation, estimate the rolling betas for a subset of 100 permnos of your choice by fitting individual regressions with `lm()` (R) or `pf.feols()` (Python) in a rolling window. Verify that you get the same results as with the vectorized code from above.
4.  Filter out the stocks with negative betas. Do these stocks frequently exhibit negative betas, or do they resemble estimation errors?
5.  Compute beta estimates for multi-factor models such as the Fama-French three-factor model by writing a rolling estimation that fits the model per stock with `lm()` (R) or `pf.feols()` (Python). In particular, your regression should support the model \\ r\_{i, t} - r\_{f, t} = \alpha_i + \sum\limits\_{j=1}^k\beta\_{i,j}(r\_{j, t}-r\_{f,t})+\varepsilon\_{i, t} \tag{3}\\ where \\r\_{j, t}\\ are the \\k\\ factor returns. Thus, for the three-factor model, you estimate four parameters (\\\alpha_i\\ and the slope coefficients). Provide some summary statistics of the cross-section of firms and their exposure to the different factors.

## References

Fischer, Alexander. 2024. *PyFixest: Fast High-Dimensional Fixed Effects Regression in Python*. [Https://pypi.org/project/pyfixest/](https://pypi.org/project/pyfixest/).

Lintner, John. 1965. “Security prices, risk, and maximal gains from diversification.” *The Journal of Finance* 20 (4): 587–615. <https://doi.org/10.1111/j.1540-6261.1965.tb02930.x>.

Mossin, Jan. 1966. “Equilibrium in a capital asset market.” *Econometrica* 34 (4): 768–83. <https://doi.org/10.2307/1910098>.

Sharpe, William F. 1964. “Capital asset prices: A theory of market equilibrium under conditions of risk .” *The Journal of Finance* 19 (3): 425–42. <https://doi.org/10.1111/j.1540-6261.1964.tb02865.x>.

Vaughan, Davis. 2021. *slider: Sliding window functions*. <https://CRAN.R-project.org/package=slider>.
