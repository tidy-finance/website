# Beta Estimation

In this chapter, we introduce an important concept in financial economics: the exposure of an individual stock to changes in the market portfolio. According to the Capital Asset Pricing Model (CAPM) of Sharpe ([1964](#ref-Sharpe1964)), Lintner ([1965](#ref-Lintner1965)), and Mossin ([1966](#ref-Mossin1966)), cross-sectional variation in expected asset returns should be a function of the covariance between the excess return of the asset and the excess return on the market portfolio. The regression coefficient of excess market returns on excess stock returns is usually called the market beta. We show an estimation procedure for the market betas. We do not go into details about the foundations of market beta but simply refer to any treatment of the [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model) for further information. Instead, we provide details about all the functions that we use to compute the results. In particular, we leverage useful computational concepts: rolling-window estimation and parallelization.

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
library(nanoparquet)
library(slider)
library(furrr)
```

Compared to previous chapters, we introduce `slider` ([Vaughan 2021](#ref-slider)) for sliding window functions, and `furrr` ([Vaughan and Dancho 2022](#ref-furrr)) to apply mapping functions in parallel.

## Python

``` python
import polars as pl
import numpy as np
import pyfixest as pf
import os

from plotnine import *
from mizani.formatters import percent_format
from joblib import Parallel, delayed, cpu_count
from itertools import product
from dateutil.relativedelta import relativedelta
```

Compared to previous chapters, we introduce `pyfixest` ([Fischer 2024](#ref-pyfixest)) for regression analysis and for sliding-window regressions and `joblib` ([Team 2023](#ref-joblib)) for parallelization.

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

After we estimated the regression coefficients on an example, we scale the estimation of \\\beta_i\\ to a whole different level and perform rolling-window estimations for the entire CRSP sample. The following function implements the CAPM regression for a data frame (or a part thereof) containing at least `min_obs` observations to avoid huge fluctuations if the time series is too short. The function conveniently returns the regression results as a data frame, which ensures that our approach is scalable. If the `min_obs`-condition is violated, that is, the time series is too short, the function returns an empty data frame for consistency.

## R

``` r
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    return(tibble())
  } else {
    fit <- lm("ret_excess ~ mkt_excess", data = data)
    coefficients <- summary(fit)$coefficients

    capm <- tibble(
      coefficient = rownames(coefficients),
      estimate = coefficients[, "Estimate"],
      t_statistic = coefficients[, "t value"]
    ) |>
      mutate(
        coefficient = if_else(
          coefficient == "(Intercept)",
          "alpha",
          coefficient
        )
      )
  }

  capm
}
```

## Python

``` python
def estimate_capm(data, min_obs=1):
    if data.shape[0] < min_obs:
        capm = pl.DataFrame()
    else:
        coefficients = pf.feols(
            "ret_excess ~ mkt_excess", data=data
        ).tidy()

        capm = pl.DataFrame(
            {
                "coefficient": coefficients.index.tolist(),
                "estimate": coefficients["Estimate"].to_numpy(),
                "t_statistic": coefficients["t value"].to_numpy(),
            }
        ).with_columns(
            coefficient=pl.when(pl.col("coefficient") == "Intercept")
            .then(pl.lit("alpha"))
            .otherwise(pl.col("coefficient"))
        )

    return capm
```

Next, we define a function that performs the rolling estimation. The following function takes input data and slides across the `date` vector, considering only a total of `look_back` months. The function essentially performs three steps: (i) arrange all rows, (ii) compute betas by sliding across months, and (iii) return a data frame with months and corresponding parameter estimates. As we demonstrate further below, we can also apply the same function to daily returns data.

## R

The `slide_period` function is able to handle months in its window input in a straightforward manner.

``` r
roll_capm_estimation <- function(data, look_back = 60, min_obs = 48) {
  data <- data |>
    arrange(date)

  slide_period_dfr(
    .x = data,
    .i = data$date,
    .period = "month",
    .f = function(x) {
      estimate_capm(x, min_obs = min_obs) |>
        mutate(date = max(x$date))
    },
    .before = look_back - 1,
    .complete = FALSE
  )
}
```

## Python

We use a simple for-loop to implement the sliding window estimation in a straightforward manner.

``` python
def roll_capm_estimation(data, look_back=60, min_obs=48):
    results = []
    dates = data["date"].unique().sort()

    for i in range(len(dates)):
        end_date = dates[i]
        start_date = end_date - relativedelta(months=look_back - 1)

        window_data = data.filter(
            (pl.col("date") >= start_date) & (pl.col("date") <= end_date)
        )

        result = estimate_capm(window_data, min_obs=min_obs)
        if result.height > 0:
            result = result.with_columns(date=pl.lit(window_data["date"].max()))
            results.append(result)

    if results:
        rolling_capm_estimation = pl.concat(results)
    else:
        rolling_capm_estimation = pl.DataFrame(
            schema={
                "coefficient": pl.String,
                "estimate": pl.Float64,
                "t_statistic": pl.Float64,
                "date": data["date"].dtype,
            }
        )

    return rolling_capm_estimation
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

## R

The main idea is to apply the function to each stock individually and then combine the results into a single data frame. First, we nest the data by `permno`. Nested data means we now have a list of `permno` with corresponding grouped time series data. We get one row of output for each unique combination of non-nested variables which is only `permno` in this case.

``` r
capm_examples_nested <- crsp_monthly |>
  filter(permno %in% examples$permno) |>
  nest(data = c(date, ret_excess, mkt_excess, industry))
capm_examples_nested
```

      permno
    1  10107
    2  14593
    3  17778
    4  93436
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          data
    1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           5934, 5964, 5995, 6025, 6056, 6087, 6117, 6148, 6178, 6209, 6240, 6268, 6299, 6329, 6360, 6390, 6421, 6452, 6482, 6513, 6543, 6574, 6605, 6634, 6665, 6695, 6726, 6756, 6787, 6818, 6848, 6879, 6909, 6940, 6971, 6999, 7030, 7060, 7091, 7121, 7152, 7183, 7213, 7244, 7274, 7305, 7336, 7364, 7395, 7425, 7456, 7486, 7517, 7548, 7578, 7609, 7639, 7670, 7701, 7729, 7760, 7790, 7821, 7851, 7882, 7913, 7943, 7974, 8004, 8035, 8066, 8095, 8126, 8156, 8187, 8217, 8248, 8279, 8309, 8340, 8370, 8401, 8432, 8460, 8491, 8521, 8552, 8582, 8613, 8644, 8674, 8705, 8735, 8766, 8797, 8825, 8856, 8886, 8917, 8947, 8978, 9009, 9039, 9070, 9100, 9131, 9162, 9190, 9221, 9251, 9282, 9312, 9343, 9374, 9404, 9435, 9465, 9496, 9527, 9556, 9587, 9617, 9648, 9678, 9709, 9740, 9770, 9801, 9831, 9862, 9893, 9921, 9952, 9982, 10013, 10043, 10074, 10105, 10135, 10166, 10196, 10227, 10258, 10286, 10317, 10347, 10378, 10408, 10439, 10470, 10500, 10531, 10561, 10592, 10623, 10651, 10682, 10712, 10743, 10773, 10804, 10835, 10865, 10896, 10926, 10957, 10988, 11017, 11048, 11078, 11109, 11139, 11170, 11201, 11231, 11262, 11292, 11323, 11354, 11382, 11413, 11443, 11474, 11504, 11535, 11566, 11596, 11627, 11657, 11688, 11719, 11747, 11778, 11808, 11839, 11869, 11900, 11931, 11961, 11992, 12022, 12053, 12084, 12112, 12143, 12173, 12204, 12234, 12265, 12296, 12326, 12357, 12387, 12418, 12449, 12478, 12509, 12539, 12570, 12600, 12631, 12662, 12692, 12723, 12753, 12784, 12815, 12843, 12874, 12904, 12935, 12965, 12996, 13027, 13057, 13088, 13118, 13149, 13180, 13208, 13239, 13269, 13300, 13330, 13361, 13392, 13422, 13453, 13483, 13514, 13545, 13573, 13604, 13634, 13665, 13695, 13726, 13757, 13787, 13818, 13848, 13879, 13910, 13939, 13970, 14000, 14031, 14061, 14092, 14123, 14153, 14184, 14214, 14245, 14276, 14304, 14335, 14365, 14396, 14426, 14457, 14488, 14518, 14549, 14579, 14610, 14641, 14669, 14700, 14730, 14761, 14791, 14822, 14853, 14883, 14914, 14944, 14975, 15006, 15034, 15065, 15095, 15126, 15156, 15187, 15218, 15248, 15279, 15309, 15340, 15371, 15400, 15431, 15461, 15492, 15522, 15553, 15584, 15614, 15645, 15675, 15706, 15737, 15765, 15796, 15826, 15857, 15887, 15918, 15949, 15979, 16010, 16040, 16071, 16102, 16130, 16161, 16191, 16222, 16252, 16283, 16314, 16344, 16375, 16405, 16436, 16467, 16495, 16526, 16556, 16587, 16617, 16648, 16679, 16709, 16740, 16770, 16801, 16832, 16861, 16892, 16922, 16953, 16983, 17014, 17045, 17075, 17106, 17136, 17167, 17198, 17226, 17257, 17287, 17318, 17348, 17379, 17410, 17440, 17471, 17501, 17532, 17563, 17591, 17622, 17652, 17683, 17713, 17744, 17775, 17805, 17836, 17866, 17897, 17928, 17956, 17987, 18017, 18048, 18078, 18109, 18140, 18170, 18201, 18231, 18262, 18293, 18322, 18353, 18383, 18414, 18444, 18475, 18506, 18536, 18567, 18597, 18628, 18659, 18687, 18718, 18748, 18779, 18809, 18840, 18871, 18901, 18932, 18962, 18993, 19024, 19052, 19083, 19113, 19144, 19174, 19205, 19236, 19266, 19297, 19327, 19358, 19389, 19417, 19448, 19478, 19509, 19539, 19570, 19601, 19631, 19662, 19692, 19723, 19754, 19783, 19814, 19844, 19875, 19905, 19936, 19967, 19997, 20028, 20058, 0.167527, 0.072619, -0.120308, -0.078371, -0.0046, -0.013272, 0.367081, 0.279971, -0.035051, 0.511344, 0.045273, 0.255886, 0.070535, 0.104373, -0.119767, -0.083031, 0.258598, 0.111289, -0.255057, -0.104003, 0.208391, 0.02475, 0.060422, -0.052821, -0.039998, 0.05912, 0.150272, -0.11704, -0.165564, 0.0388, -0.068301, -0.041414, 0.120684, 0.114218, -0.008196, -0.168465, 0.113601, 0.074874, -0.131067, 0.026019, 0.065659, 0.159457, 0.186631, 0.05732, -0.0061, 0.057518, 0.061868, 0.115119, 0.040504, 0.251821, 0.034796, -0.1318, -0.081788, 0.01839, 0.005105, 0.127633, 0.035523, 0.298787, 0.052525, 0.018492, -0.072438, 0.103886, -0.073107, 0.073999, 0.155264, 0.039388, 0.050575, 0.032052, 0.140159, 0.077499, 0.024227, -0.043886, -0.07282, 0.094706, -0.135431, 0.036186, 0.021455, 0.077937, 0.100184, 0.046996, -0.086022, 0.010877, -0.038327, 0.106945, -0.078076, 0.081133, -0.052433, -0.161491, 0.012703, 0.09557, -0.030988, -0.00406, 0.005513, 0.053314, -0.032937, 0.024573, 0.088745, 0.158962, -0.042635, -0.005221, 0.124941, -0.038109, 0.118694, -0.005684, -0.032233, -0.03283, 0.057053, 0.124368, 0.144985, 0.030533, 0.062459, -0.003117, 0.017399, -0.025922, 0.100272, -0.13295, 0.002274, 0.049831, 0.062992, 0.041065, 0.093582, 0.044365, 0.007579, -0.023231, 0.035137, 0.072131, 0.036558, 0.138887, 0.048787, 0.229993, -0.048018, -0.063915, 0.320853, 0.015676, 0.015453, 0.115383, -0.069913, -0.003454, -0.021678, 0.084562, -0.091372, 0.149955, 0.132254, 0.052147, 0.002683, -0.062946, 0.273719, 0.010418, -0.131645, 0.142631, -0.041247, 0.149202, 0.132983, 0.25833, -0.145643, 0.189705, -0.096448, -0.011086, 0.113738, -0.05231, 0.07476, -0.025507, 0.018184, -0.019974, 0.277907, -0.16577, -0.091146, 0.184111, -0.348129, -0.108047, 0.274721, -0.132144, -0.005, -0.141179, 0.136369, -0.172069, -0.249009, 0.402381, -0.037577, -0.077293, 0.234957, 0.017907, 0.052418, -0.096288, -0.141187, -0.105867, 0.134208, 0.102513, 0.030271, -0.03974, -0.085588, 0.032468, -0.134977, -0.027232, 0.073145, -0.124352, 0.021526, -0.110202, 0.221051, 0.077536, -0.104775, -0.083012, 0.001093, 0.020519, 0.054762, -0.038067, 0.040853, 0.029331, 0.003465, 0.047466, -0.055237, -0.01715, 0.063766, 0.00953, -0.041106, -0.061209, 0.047335, 0.003227, 0.08803, -0.003451, -0.040055, 0.011721, 0.010473, 0.064813, -0.004957, -0.018067, -0.041264, -0.041448, 0.044652, 0.020563, -0.039509, 0.028598, 0.069266, -0.063163, -0.003866, 0.077076, -0.058475, 0.072982, -0.045675, 0.008954, -0.116059, -0.062766, 0.024698, 0.028618, 0.067868, 0.060102, 0.045626, 0.021939, 0.01303, 0.02909, -0.087823, -0.01495, 0.069873, 0.024271, -0.043756, -0.020288, -0.009663, 0.022209, 0.246291, -0.087691, 0.056824, -0.08637, -0.163689, 0.041686, 0.003133, -0.005145, -0.030302, -0.066567, 0.064014, -0.023486, -0.164157, -0.088792, -0.038576, -0.12037, -0.048869, 0.137261, 0.102785, 0.037696, 0.137765, -0.010618, 0.053722, 0.043308, 0.078149, 0.06518, 0.036282, -0.075459, 0.022054, 0.021438, 0.042495, -0.151327, -0.10824, 0.121586, -0.086173, 0.043582, 0.088712, -0.047013, 0.104918, -0.006728, -0.035709, -0.044871, 0.020874, -0.028812, 0.039584, 0.053846, -0.02317, -0.064286, 0.069908, -0.032245, 0.014855, 0.137519, 0.081946, 0.016226, -0.007441, -0.082304, 0.047962, -0.036613, 0.052651, -0.034493, -0.041095, -0.059632, 0.003458, 0.027717, 0.021056, 0.028957, 0.15714, 0.061613, -0.010172, -0.078304, 0.056481, -0.003593, 0.063852, 0.085174, -0.018883, 0.011494, 0.019996, 0.069956, -0.014394, 0.020386, 0.018564, 0.035012, 0.059097, 0.020471, 0.012727, 0.024794, -0.028446, -0.130248, 0.093117, -0.072862, 0.196409, -0.030319, -0.057832, 0.057758, -0.061983, 0.017004, 0.189336, 0.039502, 0.020691, -0.00713, -0.070113, 0.085295, -0.097149, 0.070238, -0.034728, 0.10748, 0.019915, 0.002236, 0.040078, 0.012237, 0.030898, 0.039993, -0.004786, 0.029084, 0.038978, 0.02529, -0.01363, 0.053993, 0.033051, -0.004645, 0.11576, 0.016224, 0.015377, 0.109608, -0.009587, -0.027861, 0.023252, 0.060055, -0.003727, 0.074153, 0.061449, 0.016661, -0.068001, 0.040948, -0.085947, 0.026058, 0.075582, 0.050854, 0.105243, -0.051629, 0.081318, 0.015344, 0.013541, 0.006687, 0.029716, 0.058269, 0.040349, 0.078155, -0.046896, -0.027742, 0.136326, 0.025252, 0.110459, 0.007271, 0.102668, -0.067497, -0.03747, 0.059998, 0.038906, 0.042892, 0.004108, 0.014588, 0.069602, -0.007633, 0.084989, 0.051717, 0.061604, -0.066119, 0.176291, -0.001283, 0.017233, -0.075345, -0.03721, 0.031862, -0.099867, -0.018268, -0.055921, 0.092297, -0.068558, -0.111167, -0.005606, 0.099317, -0.063346, 0.029817, 0.005638, 0.152282, 0.062265, 0.067484, 0.032999, -0.018067, -0.026721, -0.040943, 0.066116, 0.118544, -0.011874, 0.052581, 0.0381, 0.012816, -0.07931, 0.063758, 0.072551, -0.068489, -0.005916, 0.027548, -0.059559, 0.040202, -0.008329, -0.0128, 0.0462, 0.0106, -0.0643, 0.0607, -0.0859, 0.0464, 0.0117, -0.0326, 0.1245, 0.0438, 0.0163, -0.0209, 0.0011, 0.0391, 0.0384, 0.0352, -0.0258, -0.2319, -0.0776, 0.0682, 0.0421, 0.0477, -0.0226, 0.0057, -0.0027, 0.0478, -0.0125, -0.0331, 0.0331, 0.0116, -0.0227, 0.0148, 0.0607, -0.0223, 0.0157, 0.0435, 0.0328, -0.0133, 0.0718, 0.0143, -0.0076, -0.0365, 0.0104, 0.0118, -0.078, 0.0112, 0.0183, -0.0336, 0.0843, -0.0109, -0.019, -0.1012, -0.0611, -0.0192, 0.0635, 0.0246, 0.0469, 0.0719, 0.0266, -0.0028, 0.0366, -0.0494, 0.0424, 0.0232, -0.0159, 0.0129, -0.042, 0.1085, -0.0059, 0.0108, -0.0265, 0.0109, 0.003, -0.0233, 0.0378, -0.0238, 0.0119, 0.0102, 0.0414, 0.0153, 0.0093, 0.0012, 0.0232, -0.0305, 0.0289, 0.003, -0.0034, 0.0372, -0.0012, 0.0142, -0.0187, 0.0165, 0.0287, -0.0255, -0.0479, 0.0068, 0.0058, -0.0304, 0.0282, 0.0402, -0.0228, 0.0135, -0.0403, 0.0086, 0.018, 0.0363, 0.0219, 0.0211, 0.029, 0.0272, 0.0371, 0.0055, 0.0336, -0.0151, 0.0396, 0.0104, 0.0226, 0.0133, 0.0074, 0.0206, 0.0236, -0.0113, -0.0596, 0.0277, 0.0501, 0.0087, 0.0625, -0.017, 0.0496, -0.0048, -0.0502, 0.0404, 0.0673, 0.041, 0.0731, -0.041, 0.0534, -0.038, 0.0299, 0.0133, 0.0015, 0.0703, 0.0476, 0.0074, -0.0307, 0.0319, -0.0242, -0.1605, 0.0619, 0.0698, 0.0607, 0.0615, 0.0348, -0.0408, 0.0348, 0.0434, -0.0246, 0.0477, -0.0349, -0.0137, -0.0277, 0.0609, 0.0341, 0.0766, -0.0474, 0.0246, 0.0521, -0.0639, -0.0439, 0.0468, -0.0248, 0.0703, -0.0544, -0.0278, -0.107, 0.0118, 0.0325, -0.1003, -0.0725, 0.0793, 0.0068, -0.0194, -0.0213, -0.064, -0.0924, 0.0248, 0.0753, 0.0161, -0.0144, -0.023, 0.0424, -0.0521, -0.0136, -0.0723, -0.0819, 0.005, -0.1034, 0.0784, 0.0596, -0.0576, -0.0257, -0.0188, 0.0109, 0.0818, 0.0605, 0.0142, 0.0233, 0.0233, -0.0124, 0.0608, 0.0135, 0.0429, 0.0215, 0.014, -0.0132, -0.0181, 0.0118, 0.0185, -0.0404, 7e-04, 0.016, 0.0142, 0.0453, 0.0342, -0.0275, 0.0188, -0.0194, -0.0261, 0.0365, 0.0058, 0.0392, -0.012, 0.0048, -0.0204, 0.0361, -0.0024, 0.0303, -0.003, 0.0147, 0.0073, -0.0356, -0.0035, -0.0077, 0.0203, 0.0184, 0.0322, 0.017, 0.0085, 0.0138, -0.0196, 0.0071, 0.0349, 0.0323, -0.0196, -0.0372, 0.0094, 0.0322, 0.0179, -0.048, -0.0087, -0.0633, -0.0309, -0.0094, 0.0461, 0.0187, -0.0843, -0.0075, 0.0153, -0.0935, -0.172, -0.0774, 0.0177, -0.0809, -0.1014, 0.0901, 0.1017, 0.0521, 0.0042, 0.0774, 0.0333, 0.0408, -0.0253, 0.0563, 0.028, -0.0335, 0.0339, 0.063, 0.0199, -0.079, -0.0556, 0.0692, -0.0478, 0.0955, 0.0387, 0.0059, 0.0682, 0.0198, 0.0348, 0.0045, 0.029, -0.0127, -0.0174, -0.0235, -0.0596, -0.0758, 0.1133, -0.0027, 0.0075, 0.0505, 0.0442, 0.0312, -0.0085, -0.0617, 0.0389, 0.0079, 0.0254, 0.0273, -0.0176, 0.0078, 0.0118, 0.0557, 0.0128, 0.0404, 0.0156, 0.028, -0.012, 0.0565, -0.0271, 0.0377, 0.0417, 0.0311, 0.0281, -0.0332, 0.0466, 0.0043, -0.0018, 0.0205, 0.026, -0.0203, 0.0424, -0.0196, 0.0251, 0.0256, -5e-04, -0.031, 0.0613, -0.0111, 0.0059, 0.0137, -0.0152, 0.0157, -0.0602, -0.0306, 0.0775, 0.0057, -0.0215, -0.0574, -6e-04, 0.0695, 0.0092, 0.0178, -3e-04, 0.0394, 0.0049, 0.0027, -0.0201, 0.0486, 0.0181, 0.0194, 0.0355, 0.0017, 0.0108, 0.0107, 0.0079, 0.0188, 0.0018, 0.0249, 0.0226, 0.0312, 0.0106, 0.0558, -0.0364, -0.0235, 0.0027, 0.0266, 0.0049, 0.032, 0.0345, 6e-04, -0.0765, 0.0171, -0.0955, 0.0837, 0.0342, 0.011, 0.0397, -0.0692, 0.0699, 0.0123, -0.0255, 0.0141, 0.0207, 0.0387, 0.0277, -0.0011, -0.0815, -0.1337, 0.136, 0.0557, 0.0245, 0.0583, 0.0762, -0.0364, -0.0208, 0.1245, 0.0463, -7e-04, 0.0281, 0.0317, 0.0495, 0.003, 0.0274, 0.0134, 0.0294, -0.044, 0.0663, -0.0158, 0.0323, -0.0616, -0.0228, 0.0308, -0.0941, -0.0034, -0.084, 0.0957, -0.0377, -0.0934, 0.0785, 0.0465, -0.0638, 0.0661, -0.0257, 0.0248, 0.0062, 0.0034, 0.0646, 0.0321, -0.0236, -0.0523, -0.0315, 0.0888, 0.0485, 0.0073, 0.0507, 0.0284, -0.0465, 0.0433, 0.028, 0.0122, 0.016, 0.0172, -0.01, 0.0649, -0.0317, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services, Services
    2 4018, 4049, 4077, 4108, 4138, 4169, 4199, 4230, 4261, 4291, 4322, 4352, 4383, 4414, 4442, 4473, 4503, 4534, 4564, 4595, 4626, 4656, 4687, 4717, 4748, 4779, 4807, 4838, 4868, 4899, 4929, 4960, 4991, 5021, 5052, 5082, 5113, 5144, 5173, 5204, 5234, 5265, 5295, 5326, 5357, 5387, 5418, 5448, 5479, 5510, 5538, 5569, 5599, 5630, 5660, 5691, 5722, 5752, 5783, 5813, 5844, 5875, 5903, 5934, 5964, 5995, 6025, 6056, 6087, 6117, 6148, 6178, 6209, 6240, 6268, 6299, 6329, 6360, 6390, 6421, 6452, 6482, 6513, 6543, 6574, 6605, 6634, 6665, 6695, 6726, 6756, 6787, 6818, 6848, 6879, 6909, 6940, 6971, 6999, 7030, 7060, 7091, 7121, 7152, 7183, 7213, 7244, 7274, 7305, 7336, 7364, 7395, 7425, 7456, 7486, 7517, 7548, 7578, 7609, 7639, 7670, 7701, 7729, 7760, 7790, 7821, 7851, 7882, 7913, 7943, 7974, 8004, 8035, 8066, 8095, 8126, 8156, 8187, 8217, 8248, 8279, 8309, 8340, 8370, 8401, 8432, 8460, 8491, 8521, 8552, 8582, 8613, 8644, 8674, 8705, 8735, 8766, 8797, 8825, 8856, 8886, 8917, 8947, 8978, 9009, 9039, 9070, 9100, 9131, 9162, 9190, 9221, 9251, 9282, 9312, 9343, 9374, 9404, 9435, 9465, 9496, 9527, 9556, 9587, 9617, 9648, 9678, 9709, 9740, 9770, 9801, 9831, 9862, 9893, 9921, 9952, 9982, 10013, 10043, 10074, 10105, 10135, 10166, 10196, 10227, 10258, 10286, 10317, 10347, 10378, 10408, 10439, 10470, 10500, 10531, 10561, 10592, 10623, 10651, 10682, 10712, 10743, 10773, 10804, 10835, 10865, 10896, 10926, 10957, 10988, 11017, 11048, 11078, 11109, 11139, 11170, 11201, 11231, 11262, 11292, 11323, 11354, 11382, 11413, 11443, 11474, 11504, 11535, 11566, 11596, 11627, 11657, 11688, 11719, 11747, 11778, 11808, 11839, 11869, 11900, 11931, 11961, 11992, 12022, 12053, 12084, 12112, 12143, 12173, 12204, 12234, 12265, 12296, 12326, 12357, 12387, 12418, 12449, 12478, 12509, 12539, 12570, 12600, 12631, 12662, 12692, 12723, 12753, 12784, 12815, 12843, 12874, 12904, 12935, 12965, 12996, 13027, 13057, 13088, 13118, 13149, 13180, 13208, 13239, 13269, 13300, 13330, 13361, 13392, 13422, 13453, 13483, 13514, 13545, 13573, 13604, 13634, 13665, 13695, 13726, 13757, 13787, 13818, 13848, 13879, 13910, 13939, 13970, 14000, 14031, 14061, 14092, 14123, 14153, 14184, 14214, 14245, 14276, 14304, 14335, 14365, 14396, 14426, 14457, 14488, 14518, 14549, 14579, 14610, 14641, 14669, 14700, 14730, 14761, 14791, 14822, 14853, 14883, 14914, 14944, 14975, 15006, 15034, 15065, 15095, 15126, 15156, 15187, 15218, 15248, 15279, 15309, 15340, 15371, 15400, 15431, 15461, 15492, 15522, 15553, 15584, 15614, 15645, 15675, 15706, 15737, 15765, 15796, 15826, 15857, 15887, 15918, 15949, 15979, 16010, 16040, 16071, 16102, 16130, 16161, 16191, 16222, 16252, 16283, 16314, 16344, 16375, 16405, 16436, 16467, 16495, 16526, 16556, 16587, 16617, 16648, 16679, 16709, 16740, 16770, 16801, 16832, 16861, 16892, 16922, 16953, 16983, 17014, 17045, 17075, 17106, 17136, 17167, 17198, 17226, 17257, 17287, 17318, 17348, 17379, 17410, 17440, 17471, 17501, 17532, 17563, 17591, 17622, 17652, 17683, 17713, 17744, 17775, 17805, 17836, 17866, 17897, 17928, 17956, 17987, 18017, 18048, 18078, 18109, 18140, 18170, 18201, 18231, 18262, 18293, 18322, 18353, 18383, 18414, 18444, 18475, 18506, 18536, 18567, 18597, 18628, 18659, 18687, 18718, 18748, 18779, 18809, 18840, 18871, 18901, 18932, 18962, 18993, 19024, 19052, 19083, 19113, 19144, 19174, 19205, 19236, 19266, 19297, 19327, 19358, 19389, 19417, 19448, 19478, 19509, 19539, 19570, 19601, 19631, 19662, 19692, 19723, 19754, 19783, 19814, 19844, 19875, 19905, 19936, 19967, 19997, 20028, 20058, -0.180418, -0.072374, -0.087217, 0.14656, 0.152974, -0.228189, -0.050769, -0.207314, -0.253886, 0.298104, -0.079236, 0.178591, -0.086873, -0.113176, -0.084885, -0.136761, -0.061233, -0.098489, 0.048037, 0.324197, 0.008741, 0.379766, 0.249858, -0.069445, 0.361301, 0.110008, -0.080273, 0.188166, 0.136664, -0.16038, -0.293845, 0.0605, -0.386795, -0.029222, -0.106448, 0.189019, 0.007785, 0.053506, -0.064443, 0.259577, -0.071545, -0.105372, -0.045936, 0.030916, -0.060487, -0.01995, -0.012325, 0.170368, -0.010792, -0.152352, -0.112261, -0.046748, -0.188953, 0.030471, -0.124256, -0.060618, 0.044, 0.17604, 0.074437, 0.086668, 0.045536, 0.075781, 0.124, 0.065597, 0.21824, -0.035605, -0.13412, 0.1794, -0.099095, 0.028982, 0.151335, 0.0076, 0.36617, 0.256961, -0.083271, 0.224282, -0.005401, 0.020517, 0.013919, 0.306019, 0.041796, -0.322372, -0.147178, 0.268827, -0.014805, 0.033566, -0.074167, 0.0204, 0.009058, 0.109558, -0.045641, -0.105566, 0.07844, -0.113036, -0.02893, 0.063467, -0.067612, -0.043222, -0.023941, 0.088037, 0.219121, -0.143226, -0.043364, 0.114747, -0.0065, 0.038144, -0.052948, -0.20949, -0.041161, -0.002441, 0.177424, -0.028639, 0.043737, 0.078549, -0.068253, -0.123011, -0.222216, 0.053545, 0.193505, 0.164068, 0.285498, 0.02888, 0.183373, -0.196476, -0.147837, -0.121221, 0.109558, 0.144069, -0.070638, 0.036204, -0.016195, 0.107037, 0.145159, 0.041622, -0.140437, 0.028989, -0.009037, -0.198176, -0.029142, -0.016004, -0.021622, 0.161135, 0.095224, 0.03633, -0.006484, -0.10946, -0.030802, -0.007254, 0.10502, -0.304928, -0.299868, -0.043378, -0.120525, 0.313308, 0.025615, -0.073729, 0.117158, 0.116068, -0.091741, -0.100444, -0.024292, -0.097117, 0.268426, 0.074234, -0.072785, 0.278204, -0.138594, 0.04258, 0.031056, -0.022988, -0.112195, 0.080706, 0.084256, 0.112593, -0.035456, -0.046568, -0.138021, -0.029868, 0.048976, -0.168834, -0.137633, -0.008425, -0.110718, -0.012234, 0.067595, -0.200172, 0.043119, 0.098173, -0.089452, 0.03242, 0.044813, -0.139315, -0.208093, -0.026456, 0.118777, -0.072793, -0.026959, -0.146557, 0.22377, 0.238757, -0.007274, -0.218897, 0.038302, -0.265363, 0.390938, 0.286202, 0.160121, -0.008845, -0.031397, 0.073365, 0.202972, -0.103578, 0.217845, -0.02943, -0.142831, 0.278, 0.002607, -0.15828, 0.028016, 0.2763, -0.04552, 0.047064, 0.198629, 0.167817, -0.033594, 0.261648, 0.217929, 0.046047, 0.005019, 0.100519, 0.180142, -0.091116, -0.327922, 0.243024, -0.034633, 0.194262, -0.582536, -0.245891, -0.16165, -0.103485, 0.448382, -0.159869, 0.205115, 0.151061, -0.22054, 0.162614, -0.194828, -0.015873, -0.166681, 0.129973, 0.211284, 0.026669, 0.127367, -0.123468, 0.089483, 0.023849, -0.041367, -0.240785, -0.140326, -0.034821, -0.018349, 0.106876, -0.03667, -0.076584, 0.001094, 0.044365, -0.058961, 0.004658, 0.261407, 0.060838, 0.105281, 0.071881, -0.084391, 0.10403, -0.087201, 0.021199, 0.054986, 0.059684, 0.129535, -0.047398, 0.087841, 0.158858, -0.007146, 0.065381, 0.122414, 0.351158, 0.27808, -0.041123, 0.192499, 0.16511, -0.07321, -0.136729, 0.100187, -0.076479, 0.156253, 0.096414, 0.140414, 0.07154, 0.174535, 0.056812, 0.046855, -0.096368, -0.087946, 0.11869, -0.155174, -0.045827, 0.18266, -0.005819, 0.130462, 0.049161, 0.126288, -0.078405, 0.00609, -0.016864, 0.093797, 0.069758, 0.210239, 0.003005, 0.075646, 0.046802, 0.105047, 0.234501, -0.044095, 0.084338, -0.31874, -0.077689, 0.146116, 0.210395, 0.083282, -0.114601, -0.052205, 0.065262, -0.331058, -0.054205, -0.138975, -0.07899, 0.056005, -0.009198, 0.176824, 0.196913, 0.079313, 0.048645, 0.14706, 0.0294, 0.101796, 0.016995, 0.060531, 0.054034, -0.088591, 0.06538, 0.14837, 0.110921, -0.016225, -0.020927, 0.022641, -0.055105, 0.167115, 0.060623, 0.03369, 0.03657, 0.051859, 0.040835, -0.013414, 0.004656, -0.006569, -0.03496, 0.163285, -0.014569, -0.009121, 0.061523, -0.055783, 0.059655, 0.127111, 0.188311, 0.105284, -0.02597, -0.010802, 0.010853, 0.045822, 0.09375, 0.002704, -0.107707, -0.012296, -0.090838, -0.144094, -0.025449, 0.002855, 0.000271, 0.02249, -0.118303, 0.141225, 0.083772, -0.021481, 0.096386, 0.070066, 0.008902, -0.107697, 0.057474, 0.019953, 0.099396, 0.07872, 0.027662, 0.028731, 0.077509, -0.017073, 0.07196, 0.105965, -0.071891, 0.061424, 0.100746, -0.031372, 0.005786, 0.045312, -0.037266, -0.032888, -0.066205, -0.021816, 0.083409, -0.005785, -0.110328, -0.075342, -0.001531, 0.127011, -0.140021, 0.071699, -0.04286, 0.089863, 0.023418, 0.065304, 0.004134, -0.021647, 0.047655, 0.047347, 0.133343, 0.04839, -0.00057, 0.06717, -0.057814, 0.032004, 0.106242, -0.061144, 0.095908, 0.01949, -0.016146, -0.011736, 0.067033, -0.059251, -0.01642, 0.133741, -0.010818, 0.026383, 0.198835, -0.009803, -0.032378, -0.182988, -0.118598, 0.053054, 0.042971, 0.095126, 0.054336, -0.126266, 0.128719, 0.074495, -0.01803, 0.071162, 0.109184, 0.076317, 0.097384, 0.05271, -0.115873, -0.070962, 0.155374, 0.084926, 0.147286, 0.165032, 0.21652, -0.102626, -0.060112, 0.095395, 0.114474, -0.005502, -0.079708, 0.00734, 0.076218, -0.050506, 0.099109, 0.064983, 0.042497, -0.068037, 0.058657, 0.105076, 0.074129, -0.015712, -0.054064, 0.057473, -0.097131, -0.054803, -0.08203, 0.187834, -0.033106, -0.122877, 0.107251, -0.035924, -0.125573, 0.107021, 0.019779, 0.115049, 0.025487, 0.042466, 0.09033, 0.008286, -0.046884, -0.092978, -0.00727, 0.109347, 0.009283, -0.046927, -0.022748, -0.055586, -0.011406, 0.125833, 0.091453, 0.049911, 0.027545, 0.013467, -0.034329, 0.047708, 0.051455, -0.0506, 0.0061, 0.0367, -0.0216, 0.0013, -0.0234, -0.0155, -0.0702, -0.0719, 0.0492, 0.034, -0.0363, -0.0323, -0.0586, -0.0182, 0.0324, -0.0393, -0.0308, -0.032, 0.1123, 0.0131, 0.113, 0.0468, 0.0056, 0.0359, 0.0259, 0.0281, 0.0666, 0.0051, 0.0307, -0.0406, -0.005, 0.0092, -0.0343, 0.0217, -0.0177, -0.0192, -0.0477, 0.0065, -0.0051, -0.0598, 0.0181, -0.0272, 0.1027, -0.008, -0.0084, -0.0177, 0.0184, 0.0797, 0.0122, -0.0083, -0.0096, 0.0507, 0.0127, -0.0072, -0.0101, -0.0451, 0.04, 0.0646, 0.0388, 0.0065, 0.0714, 0.0491, -0.0128, 0.0462, 0.0106, -0.0643, 0.0607, -0.0859, 0.0464, 0.0117, -0.0326, 0.1245, 0.0438, 0.0163, -0.0209, 0.0011, 0.0391, 0.0384, 0.0352, -0.0258, -0.2319, -0.0776, 0.0682, 0.0421, 0.0477, -0.0226, 0.0057, -0.0027, 0.0478, -0.0125, -0.0331, 0.0331, 0.0116, -0.0227, 0.0148, 0.0607, -0.0223, 0.0157, 0.0435, 0.0328, -0.0133, 0.0718, 0.0143, -0.0076, -0.0365, 0.0104, 0.0118, -0.078, 0.0112, 0.0183, -0.0336, 0.0843, -0.0109, -0.019, -0.1012, -0.0611, -0.0192, 0.0635, 0.0246, 0.0469, 0.0719, 0.0266, -0.0028, 0.0366, -0.0494, 0.0424, 0.0232, -0.0159, 0.0129, -0.042, 0.1085, -0.0059, 0.0108, -0.0265, 0.0109, 0.003, -0.0233, 0.0378, -0.0238, 0.0119, 0.0102, 0.0414, 0.0153, 0.0093, 0.0012, 0.0232, -0.0305, 0.0289, 0.003, -0.0034, 0.0372, -0.0012, 0.0142, -0.0187, 0.0165, 0.0287, -0.0255, -0.0479, 0.0068, 0.0058, -0.0304, 0.0282, 0.0402, -0.0228, 0.0135, -0.0403, 0.0086, 0.018, 0.0363, 0.0219, 0.0211, 0.029, 0.0272, 0.0371, 0.0055, 0.0336, -0.0151, 0.0396, 0.0104, 0.0226, 0.0133, 0.0074, 0.0206, 0.0236, -0.0113, -0.0596, 0.0277, 0.0501, 0.0087, 0.0625, -0.017, 0.0496, -0.0048, -0.0502, 0.0404, 0.0673, 0.041, 0.0731, -0.041, 0.0534, -0.038, 0.0299, 0.0133, 0.0015, 0.0703, 0.0476, 0.0074, -0.0307, 0.0319, -0.0242, -0.1605, 0.0619, 0.0698, 0.0607, 0.0615, 0.0348, -0.0408, 0.0348, 0.0434, -0.0246, 0.0477, -0.0349, -0.0137, -0.0277, 0.0609, 0.0341, 0.0766, -0.0474, 0.0246, 0.0521, -0.0639, -0.0439, 0.0468, -0.0248, 0.0703, -0.0544, -0.0278, -0.107, 0.0118, 0.0325, -0.1003, -0.0725, 0.0793, 0.0068, -0.0194, -0.0213, -0.064, -0.0924, 0.0248, 0.0753, 0.0161, -0.0144, -0.023, 0.0424, -0.0521, -0.0136, -0.0723, -0.0819, 0.005, -0.1034, 0.0784, 0.0596, -0.0576, -0.0257, -0.0188, 0.0109, 0.0818, 0.0605, 0.0142, 0.0233, 0.0233, -0.0124, 0.0608, 0.0135, 0.0429, 0.0215, 0.014, -0.0132, -0.0181, 0.0118, 0.0185, -0.0404, 7e-04, 0.016, 0.0142, 0.0453, 0.0342, -0.0275, 0.0188, -0.0194, -0.0261, 0.0365, 0.0058, 0.0392, -0.012, 0.0048, -0.0204, 0.0361, -0.0024, 0.0303, -0.003, 0.0147, 0.0073, -0.0356, -0.0035, -0.0077, 0.0203, 0.0184, 0.0322, 0.017, 0.0085, 0.0138, -0.0196, 0.0071, 0.0349, 0.0323, -0.0196, -0.0372, 0.0094, 0.0322, 0.0179, -0.048, -0.0087, -0.0633, -0.0309, -0.0094, 0.0461, 0.0187, -0.0843, -0.0075, 0.0153, -0.0935, -0.172, -0.0774, 0.0177, -0.0809, -0.1014, 0.0901, 0.1017, 0.0521, 0.0042, 0.0774, 0.0333, 0.0408, -0.0253, 0.0563, 0.028, -0.0335, 0.0339, 0.063, 0.0199, -0.079, -0.0556, 0.0692, -0.0478, 0.0955, 0.0387, 0.0059, 0.0682, 0.0198, 0.0348, 0.0045, 0.029, -0.0127, -0.0174, -0.0235, -0.0596, -0.0758, 0.1133, -0.0027, 0.0075, 0.0505, 0.0442, 0.0312, -0.0085, -0.0617, 0.0389, 0.0079, 0.0254, 0.0273, -0.0176, 0.0078, 0.0118, 0.0557, 0.0128, 0.0404, 0.0156, 0.028, -0.012, 0.0565, -0.0271, 0.0377, 0.0417, 0.0311, 0.0281, -0.0332, 0.0466, 0.0043, -0.0018, 0.0205, 0.026, -0.0203, 0.0424, -0.0196, 0.0251, 0.0256, -5e-04, -0.031, 0.0613, -0.0111, 0.0059, 0.0137, -0.0152, 0.0157, -0.0602, -0.0306, 0.0775, 0.0057, -0.0215, -0.0574, -6e-04, 0.0695, 0.0092, 0.0178, -3e-04, 0.0394, 0.0049, 0.0027, -0.0201, 0.0486, 0.0181, 0.0194, 0.0355, 0.0017, 0.0108, 0.0107, 0.0079, 0.0188, 0.0018, 0.0249, 0.0226, 0.0312, 0.0106, 0.0558, -0.0364, -0.0235, 0.0027, 0.0266, 0.0049, 0.032, 0.0345, 6e-04, -0.0765, 0.0171, -0.0955, 0.0837, 0.0342, 0.011, 0.0397, -0.0692, 0.0699, 0.0123, -0.0255, 0.0141, 0.0207, 0.0387, 0.0277, -0.0011, -0.0815, -0.1337, 0.136, 0.0557, 0.0245, 0.0583, 0.0762, -0.0364, -0.0208, 0.1245, 0.0463, -7e-04, 0.0281, 0.0317, 0.0495, 0.003, 0.0274, 0.0134, 0.0294, -0.044, 0.0663, -0.0158, 0.0323, -0.0616, -0.0228, 0.0308, -0.0941, -0.0034, -0.084, 0.0957, -0.0377, -0.0934, 0.0785, 0.0465, -0.0638, 0.0661, -0.0257, 0.0248, 0.0062, 0.0034, 0.0646, 0.0321, -0.0236, -0.0523, -0.0315, 0.0888, 0.0485, 0.0073, 0.0507, 0.0284, -0.0465, 0.0433, 0.028, 0.0122, 0.016, 0.0172, -0.01, 0.0649, -0.0317, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing
    3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        2496, 2526, 2557, 2588, 2616, 2647, 2677, 2708, 2738, 2769, 2800, 2830, 2861, 2891, 2922, 2953, 2981, 3012, 3042, 3073, 3103, 3134, 3165, 3195, 3226, 3256, 3287, 3318, 3346, 3377, 3407, 3438, 3468, 3499, 3530, 3560, 3591, 3621, 3652, 3683, 3712, 3743, 3773, 3804, 3834, 3865, 3896, 3926, 3957, 3987, 4018, 4049, 4077, 4108, 4138, 4169, 4199, 4230, 4261, 4291, 4322, 4352, 4383, 4414, 4442, 4473, 4503, 4534, 4564, 4595, 4626, 4656, 4687, 4717, 4748, 4779, 4807, 4838, 4868, 4899, 4929, 4960, 4991, 5021, 5052, 5082, 5113, 5144, 5173, 5204, 5234, 5265, 5295, 5326, 5357, 5387, 5418, 5448, 5479, 5510, 5538, 5569, 5599, 5630, 5660, 5691, 5722, 5752, 5783, 5813, 5844, 5875, 5903, 5934, 5964, 5995, 6025, 6056, 6087, 6117, 6148, 6178, 6209, 6240, 6268, 6299, 6329, 6360, 6390, 6421, 6452, 6482, 6513, 6543, 6574, 6605, 6634, 6665, 6695, 6726, 6756, 6787, 6818, 6848, 6879, 6909, 6940, 6971, 6999, 7030, 7060, 7091, 7121, 7152, 7183, 7213, 7244, 7274, 7305, 7336, 7364, 7395, 7425, 7456, 7486, 7517, 7548, 7578, 7609, 7639, 7670, 7701, 7729, 7760, 7790, 7821, 7851, 7882, 7913, 7943, 7974, 8004, 8035, 8066, 8095, 8126, 8156, 8187, 8217, 8248, 8279, 8309, 8340, 8370, 8401, 8432, 8460, 8491, 8521, 8552, 8582, 8613, 8644, 8674, 8705, 8735, 8766, 8797, 8825, 8856, 8886, 8917, 8947, 8978, 9009, 9039, 9070, 9100, 9131, 9162, 9190, 9221, 9251, 9282, 9312, 9343, 9374, 9404, 9435, 9465, 9496, 9527, 9556, 9587, 9617, 9648, 9678, 9709, 9740, 9770, 9801, 9831, 9862, 9893, 9921, 9952, 9982, 10013, 10043, 10074, 10105, 10135, 10166, 10196, 10227, 10258, 10286, 10317, 10347, 10378, 10408, 10439, 10470, 10500, 10531, 10561, 10592, 10623, 10651, 10682, 10712, 10743, 10773, 10804, 10835, 10865, 10896, 10926, 10957, 10988, 11017, 11048, 11078, 11109, 11139, 11170, 11201, 11231, 11262, 11292, 11323, 11354, 11382, 11413, 11443, 11474, 11504, 11535, 11566, 11596, 11627, 11657, 11688, 11719, 11747, 11778, 11808, 11839, 11869, 11900, 11931, 11961, 11992, 12022, 12053, 12084, 12112, 12143, 12173, 12204, 12234, 12265, 12296, 12326, 12357, 12387, 12418, 12449, 12478, 12509, 12539, 12570, 12600, 12631, 12662, 12692, 12723, 12753, 12784, 12815, 12843, 12874, 12904, 12935, 12965, 12996, 13027, 13057, 13088, 13118, 13149, 13180, 13208, 13239, 13269, 13300, 13330, 13361, 13392, 13422, 13453, 13483, 13514, 13545, 13573, 13604, 13634, 13665, 13695, 13726, 13757, 13787, 13818, 13848, 13879, 13910, 13939, 13970, 14000, 14031, 14061, 14092, 14123, 14153, 14184, 14214, 14245, 14276, 14304, 14335, 14365, 14396, 14426, 14457, 14488, 14518, 14549, 14579, 14610, 14641, 14669, 14700, 14730, 14761, 14791, 14822, 14853, 14883, 14914, 14944, 14975, 15006, 15034, 15065, 15095, 15126, 15156, 15187, 15218, 15248, 15279, 15309, 15340, 15371, 15400, 15431, 15461, 15492, 15522, 15553, 15584, 15614, 15645, 15675, 15706, 15737, 15765, 15796, 15826, 15857, 15887, 15918, 15949, 15979, 16010, 16040, 16071, 16102, 16130, 16161, 16191, 16222, 16252, 16283, 16314, 16344, 16375, 16405, 16436, 16467, 16495, 16526, 16556, 16587, 16617, 16648, 16679, 16709, 16740, 16770, 16801, 16832, 16861, 16892, 16922, 16953, 16983, 17014, 17045, 17075, 17106, 17136, 17167, 17198, 17226, 17257, 17287, 17318, 17348, 17379, 17410, 17440, 17471, 17501, 17532, 17563, 17591, 17622, 17652, 17683, 17713, 17744, 17775, 17805, 17836, 17866, 17897, 17928, 17956, 17987, 18017, 18048, 18078, 18109, 18140, 18170, 18201, 18231, 18262, 18293, 18322, 18353, 18383, 18414, 18444, 18475, 18506, 18536, 18567, 18597, 18628, 18659, 18687, 18718, 18748, 18779, 18809, 18840, 18871, 18901, 18932, 18962, 18993, 19024, 19052, 19083, 19113, 19144, 19174, 19205, 19236, 19266, 19297, 19327, 19358, 19389, 19417, 19448, 19478, 19509, 19539, 19570, 19601, 19631, 19662, 19692, 19723, 19754, 19783, 19814, 19844, 19875, 19905, 19936, 19967, 19997, 20028, 20058, 0.150412, 0.142497, 0.040844, -0.046053, 0.073978, -0.014109, 0.0588, -0.004, -0.0042, 0.015208, 0.038969, 0.036575, 0.083496, 0.145507, -0.036702, -0.00095, 0.038336, 0.053833, 0.146216, -0.033971, -0.0056, 0.038518, -0.0062, 0.055172, -0.099838, -0.089671, 0.011408, 0.01145, 0.139139, 0.029433, 0.048501, 0.126046, 0.347139, 0.119284, -0.036469, -0.182613, 0.112907, 0.02175, 0.037455, -0.0089, -0.229491, 0.061474, 0.129831, -0.066706, 0.123732, 0.0936, 0.011981, 0.066933, 0.037737, -0.035699, 0.006941, 0.108618, -0.017176, 0.060629, -0.001976, -0.037085, -0.060709, -0.0128, -0.058085, 0.083645, 0.134931, -0.051073, -0.092071, 0.000462000000000001, -0.076786, 0.050239, -0.020262, -0.024234, -0.055055, 0.013125, 0.12688, 0.110692, 0.114182, 0.115164, -0.01968, 0.081179, 0.154414, 0.000592000000000001, 0.005823, -0.082077, 0.033361, 0.068118, 0.201138, 0.044609, 0.034985, -0.036604, 0.001834, -0.040745, -0.038248, -0.000116000000000002, 0.021903, -0.026731, -0.025847, 0.03162, 0.002916, -0.013795, -0.022538, -0.010268, 0.088646, 0.061576, 0.211408, 0.00235, -0.001195, 0.150414, -0.024805, -0.038675, 0.008706, 0.2592, -0.004192, -0.065548, -0.092645, 0.218647, 0.124435, -0.072508, -0.022082, 0.03501, -0.05562, 0.021949, -0.099328, 0.090638, -0.050857, 0.024297, 0.095091, 0.079571, 0.042919, -0.044173, 0.005076, 0.021593, 0.126829, 0.002876, 0.053144, -0.218085, -0.13132, 0.013341, 0.024219, 0.035004, 0.082902, 0.09759, 0.034635, 0.052425, 0.043093, -0.011647, 0.103627, -0.021725, -0.010991, -0.0063, 0.042372, -0.046709, 0.040919, 0.260977, 0.016004, 0.016246, 0.133684, 0.055933, 0.075005, -0.020133, -0.037452, 0.045415, -0.146334, -0.036552, -0.075604, -0.010617, 0.060364, 0.000693, -0.034578, -0.092314, -0.088031, 0.005966, 0.060687, 0.04601, 0.103414, 0.086416, -0.010592, 0.007161, 0.072223, -0.0442, 0.009981, 0.024726, 0.006796, -0.038566, -0.029571, 0.080032, -0.025499, -0.016924, 0.022388, 0.016353, -0.00554, 0.010536, 0.02942, -0.044595, 0.027537, 0.005679, 0.129626, 0.092771, 0.027487, 0.028792, 0.019544, -0.010243, 0.183571, 0.024167, 0.036561, 0.092813, -0.051102, 0.032283, -0.0025, -0.055923, -0.010157, -0.048396, 0.028044, 0.008289, -0.0032, -0.0031, 0.214591, -0.02921, -0.011553, 0.064802, 0.00988, -0.01049, 0.201682, -0.098512, 0.012235, -0.050758, 0.045526, 0.02834, 0.050937, 0.025603, 0.148641, -0.001299, 0.056817, 0.020659, -0.010531, 0.108953, -0.050379, -0.01494, -0.093752, 0.002557, 0.002015, 0.008845, 0.022757, 0.003576, 0.020591, 0.022508, 0.016028, 0.016215, 0.015418, 0.045424, 0.126679, 0.093974, -0.0043, -0.124863, 0.075118, -0.028754, 0.032713, 0.010653, 0.089178, 0.105444, 0.200401, 0.016533, 0.026612, 0.103467, -0.108719, -0.141309, -0.019476, 0.079015, 0.049613, 0.027128, -0.074929, 0.090346, -8.10000000000003e-05, 0.066328, -0.060992, -0.047056, -0.019693, -0.057067, -0.147202, 0.157918, -0.106886, -0.025342, -0.091444, -0.144925, 0.2953, 0.032113, -0.016804, -0.085911, 0.019364, 0.042187, 0.111018, -0.01647, 0.029437, 0.07239, -0.04202, 0.023978, -0.07319, 0.035061, 0.007094, 0.007389, -0.005882, -0.00021, 0.005846, 0.014943, -0.018554, 0.0785, -0.023887, -0.013479, -0.027327, 0.031552, 0.014257, -0.105858, 0.020955, 0.067414, 0.010929, 0.002524, -0.026675, 0.005124, -0.07179, -0.088178, 0.033036, 0.093279, 0.016073, 0.020127, -0.007459, 0.051584, -0.011028, 0.036767, 0.07564, 0.00517, 0.061496, 0.055384, -0.013598, 0.000165, -0.047607, -0.001362, -0.020112, -0.003965, -0.005123, -0.028798, -0.008028, 0.048579, 0.021153, 0.001737, -0.037577, -0.03256, -0.005364, -0.009434, -0.0024, -0.007192, -0.01673, 0.044861, 0.037529, -0.011814, 0.006317, -0.033459, 0.037199, -0.018542, 0.032666, -0.010837, -0.004644, 0.044894, -0.007191, 0.096892, 0.011207, 0.022984, -0.003854, -0.038875, 0.022068, -0.002473, -0.001444, -0.004137, 0.000796, 0.072073, -0.002186, 0.114849, 0.053959, 0.008007, -0.041648, 0.028112, -0.048843, 0.001573, 0.004177, -0.104931, -0.053674, 0.017486, 0.118569, -0.116497, -0.099789, -0.071154, -0.073478, -0.121907, 0.102853, 0.084098, -0.025532, -0.017567, 0.077678, 0.039591, 0.001387, -0.019802, 0.016162, -0.014017, 0.155242, 0.045375, 0.016595, -0.053261, -0.081739, 0.132937, -0.0251, 0.014216, 0.048984, -0.041867, 0.007444, 0.00198, 0.016297, 0.072393, -0.045797, -0.004389, -0.047896, -0.02248, -0.039662, -0.015625, -0.027048, 0.095038, 0.013254, -0.031603, 0.027624, 7.6e-05, 0.033629, -0.009024, -0.016242, 0.051283, 0.020009, -0.007045, 0.048415, -0.024177, 0.018517, 0.016153, 0.088132, 0.046101, 0.024115, 0.017405, 0.077359, -0.015762, 0.031435, -0.039391, 0.020114, 0.015168, 0.010146, 0.018026, -0.047151, 0.024754, 0.078535, 0.031625, -0.006597, -0.010935, -0.009355, 0.094385, 0.004954, 0.014983, 0.062214, 0.013158, -0.044845, 0.024622, -0.016638, -0.018851, 0.00656, -0.046322, 0.044667, -0.053594, -0.035999, 0.047921, -0.015817, -0.01778, -0.017491, 0.041964, 0.053588, 0.025901, -0.033456, 0.024742, -0.004694, 0.044985, -0.042457, -0.002605, 0.098648, 0.029746, 0.007217, 0.044805, -0.028499, -0.008785, 0.002064, 0.024597, 0.031047, 0.03207, 0.01122, 0.019956, 0.038527, 0.020026, 0.08551, -0.041688, -0.037139, -0.029651, -0.01327, -0.019367, 0.067575, 0.045656, 0.0118, -0.040322, 0.057656, -0.06325, 0.015874, -0.031656, -0.005159, 0.077129, -0.088294, 0.069869, -0.032319, -0.019704, 0.027084, 0.021291, 0.035033, 0.026119, -0.011883, -0.081261, -0.121214, 0.035662, -0.010963, -0.040798, 0.098407, 0.11545, -0.023177, -0.054791, 0.136059, 0.011908, -0.010681, 0.059518, 0.057935, 0.069479, 0.05697, -0.039906, 0.000714, 0.026259, -0.043082, 0.052319, -0.03702, 0.080946, 0.042478, 0.013623, 0.1107, -0.084287, -0.021545, -0.137927, 0.103736, -0.069184, -0.037119, 0.092615, 0.07626, -0.027388, 0.00565, -0.023432, 0.000877, 0.076569, -0.033142, 0.057034, 0.02947, 0.016652, -0.03219, -0.030387, 0.049817, -0.010299, 0.060529, 0.062478, 0.024698, -0.059772, 0.042139, -0.028262, 0.072218, 0.080285, -0.03772, -0.024474, 0.065547, -0.063255, 0.0036, 0.0565, -0.0403, -0.0194, -0.0135, 0.0015, -0.0147, 0.0472, -0.0169, -0.0176, -0.0027, -0.0439, 0.04, 0.0027, -0.06, -0.0139, 0.0285, 0.0787, 0.0176, -0.0169, 0.0512, 0.0378, -0.0144, -0.119, 0.0271, 0.0087, 0.0422, -0.0356, 0.0567, -7e-04, -0.022, 0.0384, 0.0083, 0.0554, -0.0083, -0.081, 0.0522, 0.0178, 0.055, -0.0123, -0.129, 0.0395, 0.0526, 0.0298, 0.0651, 0.0175, 0.0219, 0.0104, 0.096, -0.0446, -0.0506, 0.0061, 0.0367, -0.0216, 0.0013, -0.0234, -0.0155, -0.0702, -0.0719, 0.0492, 0.034, -0.0363, -0.0323, -0.0586, -0.0182, 0.0324, -0.0393, -0.0308, -0.032, 0.1123, 0.0131, 0.113, 0.0468, 0.0056, 0.0359, 0.0259, 0.0281, 0.0666, 0.0051, 0.0307, -0.0406, -0.005, 0.0092, -0.0343, 0.0217, -0.0177, -0.0192, -0.0477, 0.0065, -0.0051, -0.0598, 0.0181, -0.0272, 0.1027, -0.008, -0.0084, -0.0177, 0.0184, 0.0797, 0.0122, -0.0083, -0.0096, 0.0507, 0.0127, -0.0072, -0.0101, -0.0451, 0.04, 0.0646, 0.0388, 0.0065, 0.0714, 0.0491, -0.0128, 0.0462, 0.0106, -0.0643, 0.0607, -0.0859, 0.0464, 0.0117, -0.0326, 0.1245, 0.0438, 0.0163, -0.0209, 0.0011, 0.0391, 0.0384, 0.0352, -0.0258, -0.2319, -0.0776, 0.0682, 0.0421, 0.0477, -0.0226, 0.0057, -0.0027, 0.0478, -0.0125, -0.0331, 0.0331, 0.0116, -0.0227, 0.0148, 0.0607, -0.0223, 0.0157, 0.0435, 0.0328, -0.0133, 0.0718, 0.0143, -0.0076, -0.0365, 0.0104, 0.0118, -0.078, 0.0112, 0.0183, -0.0336, 0.0843, -0.0109, -0.019, -0.1012, -0.0611, -0.0192, 0.0635, 0.0246, 0.0469, 0.0719, 0.0266, -0.0028, 0.0366, -0.0494, 0.0424, 0.0232, -0.0159, 0.0129, -0.042, 0.1085, -0.0059, 0.0108, -0.0265, 0.0109, 0.003, -0.0233, 0.0378, -0.0238, 0.0119, 0.0102, 0.0414, 0.0153, 0.0093, 0.0012, 0.0232, -0.0305, 0.0289, 0.003, -0.0034, 0.0372, -0.0012, 0.0142, -0.0187, 0.0165, 0.0287, -0.0255, -0.0479, 0.0068, 0.0058, -0.0304, 0.0282, 0.0402, -0.0228, 0.0135, -0.0403, 0.0086, 0.018, 0.0363, 0.0219, 0.0211, 0.029, 0.0272, 0.0371, 0.0055, 0.0336, -0.0151, 0.0396, 0.0104, 0.0226, 0.0133, 0.0074, 0.0206, 0.0236, -0.0113, -0.0596, 0.0277, 0.0501, 0.0087, 0.0625, -0.017, 0.0496, -0.0048, -0.0502, 0.0404, 0.0673, 0.041, 0.0731, -0.041, 0.0534, -0.038, 0.0299, 0.0133, 0.0015, 0.0703, 0.0476, 0.0074, -0.0307, 0.0319, -0.0242, -0.1605, 0.0619, 0.0698, 0.0607, 0.0615, 0.0348, -0.0408, 0.0348, 0.0434, -0.0246, 0.0477, -0.0349, -0.0137, -0.0277, 0.0609, 0.0341, 0.0766, -0.0474, 0.0246, 0.0521, -0.0639, -0.0439, 0.0468, -0.0248, 0.0703, -0.0544, -0.0278, -0.107, 0.0118, 0.0325, -0.1003, -0.0725, 0.0793, 0.0068, -0.0194, -0.0213, -0.064, -0.0924, 0.0248, 0.0753, 0.0161, -0.0144, -0.023, 0.0424, -0.0521, -0.0136, -0.0723, -0.0819, 0.005, -0.1034, 0.0784, 0.0596, -0.0576, -0.0257, -0.0188, 0.0109, 0.0818, 0.0605, 0.0142, 0.0233, 0.0233, -0.0124, 0.0608, 0.0135, 0.0429, 0.0215, 0.014, -0.0132, -0.0181, 0.0118, 0.0185, -0.0404, 7e-04, 0.016, 0.0142, 0.0453, 0.0342, -0.0275, 0.0188, -0.0194, -0.0261, 0.0365, 0.0058, 0.0392, -0.012, 0.0048, -0.0204, 0.0361, -0.0024, 0.0303, -0.003, 0.0147, 0.0073, -0.0356, -0.0035, -0.0077, 0.0203, 0.0184, 0.0322, 0.017, 0.0085, 0.0138, -0.0196, 0.0071, 0.0349, 0.0323, -0.0196, -0.0372, 0.0094, 0.0322, 0.0179, -0.048, -0.0087, -0.0633, -0.0309, -0.0094, 0.0461, 0.0187, -0.0843, -0.0075, 0.0153, -0.0935, -0.172, -0.0774, 0.0177, -0.0809, -0.1014, 0.0901, 0.1017, 0.0521, 0.0042, 0.0774, 0.0333, 0.0408, -0.0253, 0.0563, 0.028, -0.0335, 0.0339, 0.063, 0.0199, -0.079, -0.0556, 0.0692, -0.0478, 0.0955, 0.0387, 0.0059, 0.0682, 0.0198, 0.0348, 0.0045, 0.029, -0.0127, -0.0174, -0.0235, -0.0596, -0.0758, 0.1133, -0.0027, 0.0075, 0.0505, 0.0442, 0.0312, -0.0085, -0.0617, 0.0389, 0.0079, 0.0254, 0.0273, -0.0176, 0.0078, 0.0118, 0.0557, 0.0128, 0.0404, 0.0156, 0.028, -0.012, 0.0565, -0.0271, 0.0377, 0.0417, 0.0311, 0.0281, -0.0332, 0.0466, 0.0043, -0.0018, 0.0205, 0.026, -0.0203, 0.0424, -0.0196, 0.0251, 0.0256, -5e-04, -0.031, 0.0613, -0.0111, 0.0059, 0.0137, -0.0152, 0.0157, -0.0602, -0.0306, 0.0775, 0.0057, -0.0215, -0.0574, -6e-04, 0.0695, 0.0092, 0.0178, -3e-04, 0.0394, 0.0049, 0.0027, -0.0201, 0.0486, 0.0181, 0.0194, 0.0355, 0.0017, 0.0108, 0.0107, 0.0079, 0.0188, 0.0018, 0.0249, 0.0226, 0.0312, 0.0106, 0.0558, -0.0364, -0.0235, 0.0027, 0.0266, 0.0049, 0.032, 0.0345, 6e-04, -0.0765, 0.0171, -0.0955, 0.0837, 0.0342, 0.011, 0.0397, -0.0692, 0.0699, 0.0123, -0.0255, 0.0141, 0.0207, 0.0387, 0.0277, -0.0011, -0.0815, -0.1337, 0.136, 0.0557, 0.0245, 0.0583, 0.0762, -0.0364, -0.0208, 0.1245, 0.0463, -7e-04, 0.0281, 0.0317, 0.0495, 0.003, 0.0274, 0.0134, 0.0294, -0.044, 0.0663, -0.0158, 0.0323, -0.0616, -0.0228, 0.0308, -0.0941, -0.0034, -0.084, 0.0957, -0.0377, -0.0934, 0.0785, 0.0465, -0.0638, 0.0661, -0.0257, 0.0248, 0.0062, 0.0034, 0.0646, 0.0321, -0.0236, -0.0523, -0.0315, 0.0888, 0.0485, 0.0073, 0.0507, 0.0284, -0.0465, 0.0433, 0.028, 0.0122, 0.016, 0.0172, -0.01, 0.0649, -0.0317, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance, Finance
    4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         14791, 14822, 14853, 14883, 14914, 14944, 14975, 15006, 15034, 15065, 15095, 15126, 15156, 15187, 15218, 15248, 15279, 15309, 15340, 15371, 15400, 15431, 15461, 15492, 15522, 15553, 15584, 15614, 15645, 15675, 15706, 15737, 15765, 15796, 15826, 15857, 15887, 15918, 15949, 15979, 16010, 16040, 16071, 16102, 16130, 16161, 16191, 16222, 16252, 16283, 16314, 16344, 16375, 16405, 16436, 16467, 16495, 16526, 16556, 16587, 16617, 16648, 16679, 16709, 16740, 16770, 16801, 16832, 16861, 16892, 16922, 16953, 16983, 17014, 17045, 17075, 17106, 17136, 17167, 17198, 17226, 17257, 17287, 17318, 17348, 17379, 17410, 17440, 17471, 17501, 17532, 17563, 17591, 17622, 17652, 17683, 17713, 17744, 17775, 17805, 17836, 17866, 17897, 17928, 17956, 17987, 18017, 18048, 18078, 18109, 18140, 18170, 18201, 18231, 18262, 18293, 18322, 18353, 18383, 18414, 18444, 18475, 18506, 18536, 18567, 18597, 18628, 18659, 18687, 18718, 18748, 18779, 18809, 18840, 18871, 18901, 18932, 18962, 18993, 19024, 19052, 19083, 19113, 19144, 19174, 19205, 19236, 19266, 19297, 19327, 19358, 19389, 19417, 19448, 19478, 19509, 19539, 19570, 19601, 19631, 19662, 19692, 19723, 19754, 19783, 19814, 19844, 19875, 19905, 19936, 19967, 19997, 20028, 20058, -0.16334, -0.023169, 0.047385, 0.070226, 0.617574, -0.24635, -0.095106, -0.008814, 0.161474, -0.005405, 0.092029, -0.03351, -0.032956, -0.121861, -0.014147, 0.204182, 0.114743, -0.127673, 0.017857, 0.149295, 0.114636, -0.110365, -0.109668, 0.060678, -0.123682, 0.040017, 0.026548, -0.039328, 0.202115, 0.001378, 0.10747, -0.071448, 0.087855, 0.424914, 0.810706, 0.0982, 0.250745, 0.258564, 0.144201, -0.172881, -0.204202, 0.181875, 0.205951, 0.349485, -0.148523, -0.002687, -0.000577, 0.155412, -0.069816, 0.207792, -0.100185, -0.004038, 0.011667, -0.090422, -0.084574, -0.001277, -0.071653, 0.197489, 0.109489, 0.069617, -0.007866, -0.064212, -0.00265, -0.166948, 0.112743, 0.042243, -0.203467, 0.003618, 0.196955, 0.04773, -0.072911, -0.049253, 0.105839, -0.097223, -0.03784, -0.031078, -0.042228, 0.127947, 0.178551, -0.008101, 0.112945, 0.12803, 0.085177, 0.059809, -0.106173, 0.099357, -0.042485, -0.028956, -0.06921, 0.007195, 0.13688, -0.032852, -0.225446, 0.102947, -0.032601, 0.203074, -0.13226, 0.010207, -0.12379, 0.272111, 0.037213, -0.052345, -0.079564, 0.040087, -0.127009, -0.149209, -0.226366, 0.205048, 0.079323, -0.067822, 0.065839, 0.305927, 0.046495, 0.266497, 0.55386, 0.025577, -0.216757, 0.492137, 0.067839, 0.293086, 0.324911, 0.741352, -0.139187, -0.095599, 0.462636, 0.243152, 0.124506, -0.14874, -0.011207, 0.062147, -0.118713, 0.087137, 0.011034, 0.070605, 0.054042, 0.43653, 0.027612, -0.076955, -0.113609, -0.070768, 0.238009, -0.191945, -0.129497, -0.112488, 0.322965, -0.074389, -0.039489, -0.144468, -0.147226, -0.370634, 0.402735, 0.184165, 0.004907, -0.211492, 0.23753, 0.279627, 0.017122, -0.039462, -0.034756, -0.202046, 0.190979, 0.030688, -0.250957, 0.073701, -0.133535, 0.037908, -0.032772, 0.107086, 0.168281, -0.08219, 0.217942, -0.048925, 0.377469, 0.166308, 0.0692, -0.0478, 0.0955, 0.0387, 0.0059, 0.0682, 0.0198, 0.0348, 0.0045, 0.029, -0.0127, -0.0174, -0.0235, -0.0596, -0.0758, 0.1133, -0.0027, 0.0075, 0.0505, 0.0442, 0.0312, -0.0085, -0.0617, 0.0389, 0.0079, 0.0254, 0.0273, -0.0176, 0.0078, 0.0118, 0.0557, 0.0128, 0.0404, 0.0156, 0.028, -0.012, 0.0565, -0.0271, 0.0377, 0.0417, 0.0311, 0.0281, -0.0332, 0.0466, 0.0043, -0.0018, 0.0205, 0.026, -0.0203, 0.0424, -0.0196, 0.0251, 0.0256, -5e-04, -0.031, 0.0613, -0.0111, 0.0059, 0.0137, -0.0152, 0.0157, -0.0602, -0.0306, 0.0775, 0.0057, -0.0215, -0.0574, -6e-04, 0.0695, 0.0092, 0.0178, -3e-04, 0.0394, 0.0049, 0.0027, -0.0201, 0.0486, 0.0181, 0.0194, 0.0355, 0.0017, 0.0108, 0.0107, 0.0079, 0.0188, 0.0018, 0.0249, 0.0226, 0.0312, 0.0106, 0.0558, -0.0364, -0.0235, 0.0027, 0.0266, 0.0049, 0.032, 0.0345, 6e-04, -0.0765, 0.0171, -0.0955, 0.0837, 0.0342, 0.011, 0.0397, -0.0692, 0.0699, 0.0123, -0.0255, 0.0141, 0.0207, 0.0387, 0.0277, -0.0011, -0.0815, -0.1337, 0.136, 0.0557, 0.0245, 0.0583, 0.0762, -0.0364, -0.0208, 0.1245, 0.0463, -7e-04, 0.0281, 0.0317, 0.0495, 0.003, 0.0274, 0.0134, 0.0294, -0.044, 0.0663, -0.0158, 0.0323, -0.0616, -0.0228, 0.0308, -0.0941, -0.0034, -0.084, 0.0957, -0.0377, -0.0934, 0.0785, 0.0465, -0.0638, 0.0661, -0.0257, 0.0248, 0.0062, 0.0034, 0.0646, 0.0321, -0.0236, -0.0523, -0.0315, 0.0888, 0.0485, 0.0073, 0.0507, 0.0284, -0.0465, 0.0433, 0.028, 0.0122, 0.016, 0.0172, -0.01, 0.0649, -0.0317, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing, Manufacturing

## Python

The main idea is to apply the function to each stock individually and then combine the results into a single data frame. First, we restrict the sample to the example stocks and split it by `permno`. Splitting the data means we now have a list of frames, one per `permno`, each holding the corresponding time series data. We get one block of output for each unique value of `permno`.

``` python
capm_examples_nested = (crsp_monthly
    .filter(pl.col("permno").is_in(examples["permno"]))
    .partition_by("permno")
)
capm_examples_nested
```

    [shape: (465, 5)
    ┌─────────┬────────────┬──────────┬────────────┬────────────┐
    │ permno  ┆ date       ┆ industry ┆ ret_excess ┆ mkt_excess │
    │ ---     ┆ ---        ┆ ---      ┆ ---        ┆ ---        │
    │ f64     ┆ date       ┆ str      ┆ f64        ┆ f64        │
    ╞═════════╪════════════╪══════════╪════════════╪════════════╡
    │ 10107.0 ┆ 1986-04-01 ┆ Services ┆ 0.167527   ┆ -0.0128    │
    │ 10107.0 ┆ 1986-05-01 ┆ Services ┆ 0.072619   ┆ 0.0462     │
    │ 10107.0 ┆ 1986-06-01 ┆ Services ┆ -0.120308  ┆ 0.0106     │
    │ 10107.0 ┆ 1986-07-01 ┆ Services ┆ -0.078371  ┆ -0.0643    │
    │ 10107.0 ┆ 1986-08-01 ┆ Services ┆ -0.0046    ┆ 0.0607     │
    │ …       ┆ …          ┆ …        ┆ …          ┆ …          │
    │ 10107.0 ┆ 2024-08-01 ┆ Services ┆ -0.005916  ┆ 0.016      │
    │ 10107.0 ┆ 2024-09-01 ┆ Services ┆ 0.027548   ┆ 0.0172     │
    │ 10107.0 ┆ 2024-10-01 ┆ Services ┆ -0.059559  ┆ -0.01      │
    │ 10107.0 ┆ 2024-11-01 ┆ Services ┆ 0.040202   ┆ 0.0649     │
    │ 10107.0 ┆ 2024-12-01 ┆ Services ┆ -0.008329  ┆ -0.0317    │
    └─────────┴────────────┴──────────┴────────────┴────────────┘, shape: (528, 5)
    ┌─────────┬────────────┬───────────────┬────────────┬────────────┐
    │ permno  ┆ date       ┆ industry      ┆ ret_excess ┆ mkt_excess │
    │ ---     ┆ ---        ┆ ---           ┆ ---        ┆ ---        │
    │ f64     ┆ date       ┆ str           ┆ f64        ┆ f64        │
    ╞═════════╪════════════╪═══════════════╪════════════╪════════════╡
    │ 14593.0 ┆ 1981-01-01 ┆ Manufacturing ┆ -0.180418  ┆ -0.0506    │
    │ 14593.0 ┆ 1981-02-01 ┆ Manufacturing ┆ -0.072374  ┆ 0.0061     │
    │ 14593.0 ┆ 1981-03-01 ┆ Manufacturing ┆ -0.087217  ┆ 0.0367     │
    │ 14593.0 ┆ 1981-04-01 ┆ Manufacturing ┆ 0.14656    ┆ -0.0216    │
    │ 14593.0 ┆ 1981-05-01 ┆ Manufacturing ┆ 0.152974   ┆ 0.0013     │
    │ …       ┆ …          ┆ …             ┆ …          ┆ …          │
    │ 14593.0 ┆ 2024-08-01 ┆ Manufacturing ┆ 0.027545   ┆ 0.016      │
    │ 14593.0 ┆ 2024-09-01 ┆ Manufacturing ┆ 0.013467   ┆ 0.0172     │
    │ 14593.0 ┆ 2024-10-01 ┆ Manufacturing ┆ -0.034329  ┆ -0.01      │
    │ 14593.0 ┆ 2024-11-01 ┆ Manufacturing ┆ 0.047708   ┆ 0.0649     │
    │ 14593.0 ┆ 2024-12-01 ┆ Manufacturing ┆ 0.051455   ┆ -0.0317    │
    └─────────┴────────────┴───────────────┴────────────┴────────────┘, shape: (578, 5)
    ┌─────────┬────────────┬──────────┬────────────┬────────────┐
    │ permno  ┆ date       ┆ industry ┆ ret_excess ┆ mkt_excess │
    │ ---     ┆ ---        ┆ ---      ┆ ---        ┆ ---        │
    │ f64     ┆ date       ┆ str      ┆ f64        ┆ f64        │
    ╞═════════╪════════════╪══════════╪════════════╪════════════╡
    │ 17778.0 ┆ 1976-11-01 ┆ Finance  ┆ 0.150412   ┆ 0.0036     │
    │ 17778.0 ┆ 1976-12-01 ┆ Finance  ┆ 0.142497   ┆ 0.0565     │
    │ 17778.0 ┆ 1977-01-01 ┆ Finance  ┆ 0.040844   ┆ -0.0403    │
    │ 17778.0 ┆ 1977-02-01 ┆ Finance  ┆ -0.046053  ┆ -0.0194    │
    │ 17778.0 ┆ 1977-03-01 ┆ Finance  ┆ 0.073978   ┆ -0.0135    │
    │ …       ┆ …          ┆ …        ┆ …          ┆ …          │
    │ 17778.0 ┆ 2024-08-01 ┆ Finance  ┆ 0.080285   ┆ 0.016      │
    │ 17778.0 ┆ 2024-09-01 ┆ Finance  ┆ -0.03772   ┆ 0.0172     │
    │ 17778.0 ┆ 2024-10-01 ┆ Finance  ┆ -0.024474  ┆ -0.01      │
    │ 17778.0 ┆ 2024-11-01 ┆ Finance  ┆ 0.065547   ┆ 0.0649     │
    │ 17778.0 ┆ 2024-12-01 ┆ Finance  ┆ -0.063255  ┆ -0.0317    │
    └─────────┴────────────┴──────────┴────────────┴────────────┘, shape: (174, 5)
    ┌─────────┬────────────┬───────────────┬────────────┬────────────┐
    │ permno  ┆ date       ┆ industry      ┆ ret_excess ┆ mkt_excess │
    │ ---     ┆ ---        ┆ ---           ┆ ---        ┆ ---        │
    │ f64     ┆ date       ┆ str           ┆ f64        ┆ f64        │
    ╞═════════╪════════════╪═══════════════╪════════════╪════════════╡
    │ 93436.0 ┆ 2010-07-01 ┆ Manufacturing ┆ -0.16334   ┆ 0.0692     │
    │ 93436.0 ┆ 2010-08-01 ┆ Manufacturing ┆ -0.023169  ┆ -0.0478    │
    │ 93436.0 ┆ 2010-09-01 ┆ Manufacturing ┆ 0.047385   ┆ 0.0955     │
    │ 93436.0 ┆ 2010-10-01 ┆ Manufacturing ┆ 0.070226   ┆ 0.0387     │
    │ 93436.0 ┆ 2010-11-01 ┆ Manufacturing ┆ 0.617574   ┆ 0.0059     │
    │ …       ┆ …          ┆ …             ┆ …          ┆ …          │
    │ 93436.0 ┆ 2024-08-01 ┆ Manufacturing ┆ -0.08219   ┆ 0.016      │
    │ 93436.0 ┆ 2024-09-01 ┆ Manufacturing ┆ 0.217942   ┆ 0.0172     │
    │ 93436.0 ┆ 2024-10-01 ┆ Manufacturing ┆ -0.048925  ┆ -0.01      │
    │ 93436.0 ┆ 2024-11-01 ┆ Manufacturing ┆ 0.377469   ┆ 0.0649     │
    │ 93436.0 ┆ 2024-12-01 ┆ Manufacturing ┆ 0.166308   ┆ -0.0317    │
    └─────────┴────────────┴───────────────┴────────────┴────────────┘]

## R

Next, we want to apply the `roll_capm_estimation()` function to each stock. This situation is an ideal use case for `map()`, which takes a list or vector as input and returns an object of the same length as the input. In our case, `map()` returns a single data frame with a time series of beta estimates for each stock. Therefore, we use `unnest()` to transform the list of outputs to a tidy data frame.

``` r
capm_examples <- capm_examples_nested |>
  mutate(capm = map(data, roll_capm_estimation)) |>
  unnest(capm) |>
  select(permno, date, coefficient, estimate, t_statistic)
capm_examples
```

    # A tibble: 3,114 × 5
      permno date       coefficient estimate t_statistic
       <dbl> <date>     <chr>          <dbl>       <dbl>
    1  10107 1990-03-01 alpha         0.0417        2.31
    2  10107 1990-03-01 mkt_excess    1.40          4.17
    3  10107 1990-04-01 alpha         0.0427        2.41
    4  10107 1990-04-01 mkt_excess    1.39          4.20
    5  10107 1990-05-01 alpha         0.0443        2.53
    # ℹ 3,109 more rows

## Python

Next, we want to apply the `roll_capm_estimation()` function to each stock. We iterate over the list of per-stock frames, call the function on each, attach the corresponding `permno`, and concatenate the results into a single tidy data frame with a time series of beta estimates for each stock.

``` python
capm_examples = (pl.concat([
        roll_capm_estimation(group).with_columns(permno=group["permno"][0])
        for group in capm_examples_nested
    ])
    .select(["permno", "date", "coefficient", "estimate", "t_statistic"])
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

## Parallelized Rolling-Window Estimation

> **TIP:**
>
> For a single-regressor model such as the CAPM, the rolling regressions can also be computed in closed form from precomputed cumulants, which avoids fitting millions of individual models. Our blog post [Fast, Vectorized Beta Estimation](https://blog.tidy-finance.org/posts/fast-beta-estimation/) shows how to reproduce the results of this chapter in seconds, without parallelization.

Even though we could now just apply the function on the whole CRSP sample, we advise against doing it as it is computationally quite expensive. Remember that we have to perform rolling-window estimations across all stocks and time periods. However, this estimation problem is an ideal scenario to employ the power of parallelization. Parallelization means that we split the tasks which perform rolling-window estimations across different workers (or cores on your local machine).

## R

If you have a Windows or Mac machine, it makes most sense to define `multisession`, which means that separate R processes are running in the background on the same machine to perform the individual jobs. If you check out the documentation of `plan()`, you can also see other ways to resolve the parallelization in different environments. Note that we use `availableCores()` to determine the number of cores available for parallelization, but keep one core free for other tasks. Some machines might freeze if all cores are busy with R jobs.

``` r
n_cores = availableCores() - 1
plan(multisession, workers = n_cores)
```

Using eight cores, the estimation for our sample of around 25k stocks takes around 20 minutes. Of course, you can speed up things considerably by having more cores available to share the workload or by having more powerful cores. Notice the difference in the code below? All you need to do is to replace `map()` with `future_map()`, which uses the `furrr` package in the background to handle the parallelization.

``` r
capm_monthly <- crsp_monthly |>
  nest(data = c(date, ret_excess, mkt_excess, industry)) |>
  mutate(capm = future_map(data, roll_capm_estimation)) |>
  unnest(capm) |>
  select(permno, date, coefficient, estimate, t_statistic)
capm_monthly
```

Instead of implementing the rolling-window estimation by hand, you can also use the `estimate_betas()` function from the `tidyfinance` package, which is built precisely for this task and supports parallelization via the `use_furrr` argument:

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

If you have a Windows or Mac machine, it makes most sense to use the default parallelization backend of `joblib`, which means that separate Python processes are running in the background on the same machine to perform the individual jobs. If you check out the documentation of `joblib.parallel_config()`, you can also see other ways to resolve the parallelization in different environments. Note that we use `cpu_count()` to determine the number of cores available for parallelization, but keep one core free for other tasks. Some machines might freeze if all cores are busy with Python jobs.

``` python
n_cores = cpu_count() - 1
```

Using eight cores, the estimation for our sample of around 25k stocks takes around 20 minutes. Of course, you can speed up things considerably by having more cores available to share the workload or by having more powerful cores. Instead of looping over groups, we use `Parallel()` to execute multiple tasks concurrently and `delayed()` to wrap each function call, allowing the calls to be queued and distributed to worker processes rather than executed immediately.

``` python
crsp_monthly_nested = crsp_monthly.partition_by("permno")

capm_monthly = pl.concat(
    Parallel(n_jobs=n_cores)(
        delayed(
            lambda group: roll_capm_estimation(group).with_columns(
                permno=group["permno"][0]
            )
        )(group)
        for group in crsp_monthly_nested
    )
).select(["permno", "date", "coefficient", "estimate", "t_statistic"])
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

We then proceed to perform the same steps as with the monthly CRSP data, just in batches: Load in daily returns, nest the data by stock, and parallelize the beta estimation across stocks.

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

  crsp_daily_sub_nested <- crsp_daily_sub |>
    inner_join(factors_ff3_daily, join_by(date)) |>
    mutate(date = floor_date(date, "month")) |>
    nest(data = c(date, ret_excess, mkt_excess))

  capm_daily[[j]] <- crsp_daily_sub_nested |>
    mutate(
      capm = future_map(
        data,
        \(x) roll_capm_estimation(x, min_obs = min_obs)
      )
    ) |>
    unnest(capm) |>
    select(permno, date, coefficient, estimate, t_statistic)

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
    )

    crsp_daily_sub_nested = (crsp_daily_sub
        .join(factors_ff3_daily, how="inner", on="date")
        .with_columns(date=pl.col("date").dt.truncate("1mo"))
        .partition_by("permno")
    )

    results = Parallel(n_jobs=n_cores)(
        delayed(
            lambda group: roll_capm_estimation(
                group, min_obs=min_obs
            ).with_columns(permno=group["permno"][0])
        )(group)
        for group in crsp_daily_sub_nested
    )

    if results:
        capm_daily_sub = pl.concat(results).select(
            ["permno", "date", "coefficient", "estimate", "t_statistic"]
        )
        capm_daily.append(capm_daily_sub)
    else:
        print(f"Warning: Batch {j} produced no results (insufficient data)")

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
    .select(["monthly", "daily"])
    .to_pandas()
    .corr()
    .round(2)
)
```

             monthly  daily
    monthly     1.00   0.62
    daily       0.62   1.00

Indeed, we find a positive correlation between our beta estimates. In the subsequent chapters, we mainly use the estimates based on monthly data, as most readers should be able to replicate them due to potential memory limitations that might arise with the daily data.

## Key Takeaways

- CAPM betas can be estimated using rolling-window estimation and processed in parallel via `furrr` in R or `joblib` in Python.
- Both monthly and daily return data can be used to estimate betas with different frequencies and window lengths, depending on the application.
- Summary statistics, visualization, and plausibility checks help to validate beta estimates across time and industries.

## Exercises

1.  Compute beta estimates based on monthly data using one, three, and five years of data and impose a minimum number of observations of 10, 28, and 48 months with return data, respectively. How strongly correlated are the estimated betas?
2.  Compute beta estimates based on monthly data using five years of data and impose different numbers of minimum observations. How does the share of `permno`-`date` observations with successful beta estimates vary across the different requirements? Do you find a high correlation across the estimated betas?
3.  Instead of using the parallelized mapping functions, perform the beta estimation in a loop (using either monthly or daily data) for a subset of 100 permnos of your choice. Verify that you get the same results as with the parallelized code from above.
4.  Filter out the stocks with negative betas. Do these stocks frequently exhibit negative betas, or do they resemble estimation errors?
5.  Compute beta estimates for multi-factor models such as the Fama-French three-factor model by extending the `estimate_capm()` function with a `model` parameter. In particular, your regression should support the model \\ r\_{i, t} - r\_{f, t} = \alpha_i + \sum\limits\_{j=1}^k\beta\_{i,k}(r\_{j, t}-r\_{f,t})+\varepsilon\_{i, t} \tag{2}\\ where \\r\_{i, t}\\ are the \\k\\ factor returns. Thus, you estimate four parameters (\\\alpha_i\\ and the slope coefficients). Provide some summary statistics of the cross-section of firms and their exposure to the different factors.

## References

Fischer, Alexander. 2024. *PyFixest: Fast High-Dimensional Fixed Effects Regression in Python*. [Https://pypi.org/project/pyfixest/](https://pypi.org/project/pyfixest/).

Lintner, John. 1965. “Security prices, risk, and maximal gains from diversification.” *The Journal of Finance* 20 (4): 587–615. <https://doi.org/10.1111/j.1540-6261.1965.tb02930.x>.

Mossin, Jan. 1966. “Equilibrium in a capital asset market.” *Econometrica* 34 (4): 768–83. <https://doi.org/10.2307/1910098>.

Sharpe, William F. 1964. “Capital asset prices: A theory of market equilibrium under conditions of risk .” *The Journal of Finance* 19 (3): 425–42. <https://doi.org/10.1111/j.1540-6261.1964.tb02865.x>.

Team, Joblib Development. 2023. *Joblib: Running Python Functions as Pipeline Jobs*. [Https://joblib.readthedocs.io/](https://joblib.readthedocs.io/).

Vaughan, Davis. 2021. *slider: Sliding window functions*. <https://CRAN.R-project.org/package=slider>.

Vaughan, Davis, and Matt Dancho. 2022. *furrr: Apply mapping functions in parallel using futures*. <https://CRAN.R-project.org/package=furrr>.
