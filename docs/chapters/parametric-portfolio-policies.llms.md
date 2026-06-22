# Parametric Portfolio Policies

In this chapter, we apply different portfolio performance measures to evaluate and compare portfolio allocation strategies. For this purpose, we introduce a direct way to estimate optimal portfolio weights for large-scale cross-sectional applications. More precisely, the approach of Brandt et al. ([2009](#ref-Brandt2009)) proposes to parametrize the optimal portfolio weights as a function of stock characteristics instead of estimating the stock’s expected return, variance, and covariances with other stocks in a prior step. We choose weights as a function of the characteristics, which maximize the expected utility of the investor. This approach is feasible for large portfolio dimensions (such as the entire CRSP universe) and has been proposed by Brandt et al. ([2009](#ref-Brandt2009)). See the review paper by Brandt ([2010](#ref-Brandt2010)) for an excellent treatment of related portfolio choice methods.

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
```

    Warning: package 'dplyr' was built under R version 4.5.3

``` r
library(nanoparquet)
```

    Warning: package 'nanoparquet' was built under R version 4.5.3

## Python

``` python
import polars as pl
import pandas as pd
import numpy as np
import pyfixest as pf

from itertools import product, starmap
from scipy.optimize import minimize
```

Compared to previous chapters, we introduce the `scipy.optimize` module from the `scipy` ([Virtanen et al. 2020](#ref-scipy)) for solving optimization problems.

## Data Preparation

To get started, we load the monthly CRSP file, which forms our investment universe. We load the data from our Parquet files introduced in [Accessing and Managing Financial Data](../chapters/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md).

## R

``` r
crsp_monthly <- read_parquet("data/crsp_monthly.parquet") |>
  select(permno, date, ret_excess, mktcap, mktcap_lag)
```

## Python

``` python
crsp_monthly = (
    pl.read_parquet("data/crsp_monthly.parquet")
    .select(["permno", "date", "ret_excess", "mktcap", "mktcap_lag"])
)
```

To evaluate the performance of portfolios, we further use monthly market returns as a benchmark to compute CAPM alphas.

## R

``` r
factors_ff3_monthly <- read_parquet("data/factors_ff3_monthly.parquet") |>
  select(date, mkt_excess)
```

## Python

``` python
factors_ff3_monthly = (
    pl.read_parquet("data/factors_ff3_monthly.parquet")
    .select(["date", "mkt_excess"])
)
```

Next, we retrieve some stock characteristics that have been shown to have an effect on the expected returns or expected variances (or even higher moments) of the return distribution. In particular, we record the lagged one-year return momentum (`momentum_lag`), defined as the compounded return between months \\t-13\\ and \\t-2\\ for each firm, which we calculate using market capitalization for simplicity. In finance, momentum is the empirically observed tendency for rising asset prices to rise further, and falling prices to keep falling ([Jegadeesh and Titman 1993](#ref-Jegadeesh1993)). We refer to the exercise for a more elaborate measure of momentum. The second characteristic is the firm’s market equity (`size_lag`), defined as the log of the price per share times the number of shares outstanding ([Banz 1981](#ref-Banz1981)). To construct the correct lagged values, we use the approach introduced in [WRDS, CRSP, and Compustat](../chapters/wrds-crsp-and-compustat.llms.md).

## R

``` r
crsp_monthly_lags <- crsp_monthly |>
  transmute(permno, date_13 = date %m+% months(13), mktcap)

crsp_monthly <- crsp_monthly |>
  inner_join(
    crsp_monthly_lags,
    join_by(permno, date == date_13),
    suffix = c("", "_13")
  )

data_portfolios <- crsp_monthly |>
  mutate(
    momentum_lag = mktcap_lag / mktcap_13,
    size_lag = log(mktcap_lag)
  ) |>
  drop_na(contains("lag"))
```

## Python

``` python
crsp_monthly_lags = (crsp_monthly
    .with_columns(date=pl.col("date").dt.offset_by("13mo"))
    .select(["permno", "date", "mktcap"])
    .rename({"mktcap": "mktcap_13"})
)

crsp_monthly = (crsp_monthly
    .join(crsp_monthly_lags, how="inner", on=["permno", "date"])
)

data_portfolios = (crsp_monthly
    .with_columns(
        momentum_lag=pl.col("mktcap_lag")/pl.col("mktcap_13"),
        size_lag=pl.col("mktcap_lag").log()
    )
    .drop_nulls(["momentum_lag", "size_lag"])
)
```

## Parametric Portfolio Policies

The basic idea of parametric portfolio weights is as follows. Suppose that at each date \\t\\ we have \\N_t\\ stocks in the investment universe, where each stock \\i\\ has a return of \\r\_{i, t+1}\\ and is associated with a vector of firm characteristics \\x\_{i, t}\\ such as time-series momentum or the market capitalization. The investor’s problem is to choose portfolio weights \\w\_{i,t}\\ to maximize the expected utility of the portfolio return: \\\begin{aligned} \max\_{\omega} E_t\left(u(r\_{p, t+1})\right) = E_t\left\[u\left(\sum\limits\_{i=1}^{N_t}\omega\_{i,t}r\_{i,t+1}\right)\right\] \end{aligned}\\ where \\u(\cdot)\\ denotes the utility function.

Where do the stock characteristics show up? We parameterize the optimal portfolio weights as a function of the stock characteristic \\x\_{i,t}\\ with the following linear specification for the portfolio weights: \\\omega\_{i,t} = \bar{\omega}\_{i,t} + \frac{1}{N_t}\theta'\hat{x}\_{i,t},\\ where \\\bar{\omega}\_{i,t}\\ is a stock’s weight in a benchmark portfolio (we use the value-weighted or naive portfolio in the application below), \\\theta\\ is a vector of coefficients which we are going to estimate, and \\\hat{x}\_{i,t}\\ are the characteristics of stock \\i\\, cross-sectionally standardized to have zero mean and unit standard deviation.

Intuitively, the portfolio strategy is a form of active portfolio management relative to a performance benchmark. Deviations from the benchmark portfolio are derived from the individual stock characteristics. Note that by construction the weights sum up to one as \\\sum\_{i=1}^{N_t}\hat{x}\_{i,t} = 0\\ due to the standardization. Moreover, the coefficients are constant across assets and over time. The implicit assumption is that the characteristics fully capture all aspects of the joint distribution of returns that are relevant for forming optimal portfolios.

We first implement cross-sectional standardization for the entire CRSP universe. We also keep track of (lagged) relative market capitalization `relative_mktcap`, which will represent the value-weighted benchmark portfolio, while `n` denotes the number of traded assets \\N_t\\, which we use to construct the naive portfolio benchmark.

## R

``` r
data_portfolios <- data_portfolios |>
  group_by(date) |>
  mutate(
    n = n(),
    relative_mktcap = mktcap_lag / sum(mktcap_lag),
    across(contains("lag"), ~ (. - mean(.)) / sd(.)),
  ) |>
  ungroup() |>
  select(-mktcap_lag)
```

## Python

``` python
lag_columns = [i for i in data_portfolios.columns if i.endswith("lag")]

data_portfolios = (data_portfolios
    .with_columns(
        relative_mktcap=pl.col("mktcap_lag")/pl.col("mktcap_lag").sum().over("date")
    )
    .with_columns(
        [((pl.col(c)-pl.col(c).mean().over("date"))/
          pl.col(c).std().over("date")).alias(c) for c in lag_columns]
    )
    .drop("mktcap_lag")
)
```

## Computing Portfolio Weights

Next, we move on to identify optimal choices of \\\theta\\. We rewrite the optimization problem together with the weight parametrization and can then estimate \\\theta\\ to maximize the objective function based on our sample \\\begin{aligned} E_t\left(u(r\_{p, t+1})\right) = \frac{1}{T}\sum\limits\_{t=0}^{T-1}u\left(\sum\limits\_{i=1}^{N_t}\left(\bar{\omega}\_{i,t} + \frac{1}{N_t}\theta'\hat{x}\_{i,t}\right)r\_{i,t+1}\right). \end{aligned}\\ The allocation strategy is straightforward because the number of parameters to estimate is small. Instead of a tedious specification of the \\N_t\\ dimensional vector of expected returns and the \\N_t(N_t+1)/2\\ free elements of the covariance matrix, all we need to focus on in our application is the vector \\\theta\\. \\\theta\\ contains only two elements in our application: the relative deviation from the benchmark due to *size* and *momentum*.

To get a feeling for the performance of such an allocation strategy, we start with an arbitrary initial vector \\\theta_0\\. The next step is to choose \\\theta\\ optimally to maximize the objective function. We automatically detect the number of parameters by counting the number of columns with lagged values. Note that the value for \\\theta\\ of 1.5 is an arbitrary choice.

## R

``` r
n_parameters <- sum(str_detect(
  colnames(data_portfolios), "lag"
))

theta <- rep(1.5, n_parameters)

names(theta) <- colnames(data_portfolios)[str_detect(
  colnames(data_portfolios), "lag"
)]
```

## Python

``` python
lag_columns = [i for i in data_portfolios.columns if "lag" in i]
n_parameters = len(lag_columns)
theta = np.array([1.5]*n_parameters)
```

The function `compute_portfolio_weights()` below computes the portfolio weights \\\bar{\omega}\_{i,t} + \frac{1}{N_t}\theta'\hat{x}\_{i,t}\\ according to our parametrization for a given value \\\theta_0\\. Everything happens within a single pipeline. Hence, we provide a short walk-through.

We first compute `characteristic_tilt`, the tilting values \\\frac{1}{N_t}\theta'\hat{x}\_{i, t}\\ which resemble the deviation from the benchmark portfolio. Next, we compute the benchmark portfolio `weight_benchmark`, which can be any reasonable set of portfolio weights. In our case, we choose either the value or equal-weighted allocation. `weight_tilt` completes the picture and contains the final portfolio weights `weight_tilt = weight_benchmark + characteristic_tilt` which deviate from the benchmark portfolio depending on the stock characteristics.

The final few lines go a bit further and implement a simple version of a no-short sale constraint. While it is generally not straightforward to ensure portfolio weight constraints via parameterization, we simply normalize the portfolio weights such that they are enforced to be positive. Finally, we make sure that the normalized weights sum up to one again: \\\omega\_{i,t}^+ = \frac{\max(0, \omega\_{i,t})}{\sum\_{j=1}^{N_t}\max(0, \omega\_{i,t})}.\\

The following function computes the optimal portfolio weights in the way just described.

## R

``` r
compute_portfolio_weights <- function(theta,
                                      data,
                                      value_weighting = TRUE,
                                      allow_short_selling = TRUE) {
  data |>
    group_by(date) |>
    bind_cols(
      characteristic_tilt = data |>
        transmute(across(contains("lag"), ~ . / n)) |>
        as.matrix() %*% theta |> as.numeric()
    ) |>
    mutate(
      # Definition of benchmark weight
      weight_benchmark = case_when(
        value_weighting == TRUE ~ relative_mktcap,
        value_weighting == FALSE ~ 1 / n
      ),
      # Parametric portfolio weights
      weight_tilt = weight_benchmark + characteristic_tilt,
      # Short-sell constraint
      weight_tilt = case_when(
        allow_short_selling == TRUE ~ weight_tilt,
        allow_short_selling == FALSE ~ pmax(0, weight_tilt)
      ),
      # Weights sum up to 1
      weight_tilt = weight_tilt / sum(weight_tilt)
    ) |>
    ungroup()
}
```

## Python

``` python
def compute_portfolio_weights(theta,
                              data,
                              value_weighting=True,
                              allow_short_selling=True):
    """Compute portfolio weights for different strategies."""

    lag_columns = [i for i in data.columns if "lag" in i]
    theta = np.asarray(theta).flatten()

    tilt_expr = pl.sum_horizontal(
        [float(theta[k])*pl.col(lag_columns[k])
         for k in range(len(lag_columns))]
    )

    data = (data
      .with_columns(
          characteristic_tilt=(tilt_expr/pl.len()).over("date")
      )
      .with_columns(
        weight_benchmark=(
          pl.col("relative_mktcap") if value_weighting
          else (1/pl.len()).over("date")
        )
      )
      .with_columns(
        weight_tilt=pl.col("weight_benchmark")+pl.col("characteristic_tilt")
      )
      .drop("characteristic_tilt")
    )

    if not allow_short_selling:
        data = (data
          .with_columns(
            weight_tilt=pl.max_horizontal(pl.lit(0.0), pl.col("weight_tilt")))
        )

    data = (data
      .with_columns(
        weight_tilt=(pl.col("weight_tilt")/pl.col("weight_tilt").sum()).over("date")
      )
    )

    return data
```

In the next step, we compute the portfolio weights for the arbitrary vector \\\theta_0\\. In the example below, we use the value-weighted portfolio as a benchmark and allow negative portfolio weights.

## R

``` r
weights_crsp <- compute_portfolio_weights(
  theta,
  data_portfolios,
  value_weighting = TRUE,
  allow_short_selling = TRUE
)
```

    Warning: There was 1 warning in `mutate()`.
    ℹ In argument: `weight_benchmark = case_when(...)`.
    ℹ In group 1: `date = 1961-03-01`.
    Caused by warning:
    ! Calling `case_when()` with size 1 LHS inputs and size >1 RHS
      inputs was deprecated in dplyr 1.2.0.
    ℹ This `case_when()` statement can result in subtle silent bugs and is very inefficient.

      Please use a series of if statements instead:

      ```
      # Previously
      case_when(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, .default = default)

      # Now
      if (scalar_lhs1) {
        rhs1
      } else if (scalar_lhs2) {
        rhs2
      } else {
        default
      }
      ```

## Python

``` python
weights_crsp = compute_portfolio_weights(
    theta,
    data_portfolios,
    value_weighting=True,
    allow_short_selling=True
)
```

## Portfolio Performance

Are the computed weights optimal in any way? Most likely not, as we picked \\\theta_0\\ arbitrarily. To evaluate the performance of an allocation strategy, one can think of many different approaches. In their original paper, Brandt et al. ([2009](#ref-Brandt2009)) focus on a simple evaluation of the hypothetical utility of an agent equipped with a power utility function \\u\_\gamma(r) = \frac{(1 + r)^{(1-\gamma)}}{1-\gamma}\\, where \\\gamma\\ is the risk aversion factor.

## R

``` r
power_utility <- function(r, gamma = 5) {
  (1 + r)^(1 - gamma) / (1 - gamma)
}
```

## Python

``` python
def power_utility(r, gamma=5):
    """Calculate power utility for given risk aversion."""

    utility = ((1+r)**(1-gamma))/(1-gamma)

    return utility
```

We want to note that Gehrig et al. ([2020](#ref-Gehrig2020)) warn that, in the leading case of constant relative risk aversion (CRRA), strong assumptions on the properties of the returns, the variables used to implement the parametric portfolio policy, and the parameter space are necessary to obtain a well-defined optimization problem.

No doubt, there are many other ways to evaluate a portfolio. The function below provides a summary of all kinds of interesting measures that can be considered relevant. Do we need all these evaluation measures? It depends: the original paper by Brandt et al. ([2009](#ref-Brandt2009)) only cares about the expected utility to choose \\\theta\\. However, if you want to choose optimal values that achieve the highest performance while putting some constraints on your portfolio weights, it is helpful to have everything in one function.

## R

``` r
evaluate_portfolio <- function(weights_crsp,
                               capm_evaluation = TRUE,
                               full_evaluation = TRUE,
                               length_year = 12) {

  evaluation <- weights_crsp |>
    group_by(date) |>
    summarize(
      tilt = weighted.mean(ret_excess, weight_tilt),
      benchmark = weighted.mean(ret_excess, weight_benchmark)
    ) |>
    pivot_longer(
      -date,
      values_to = "portfolio_return",
      names_to = "model"
    )

  evaluation_stats <- evaluation |>
    group_by(model) |>
    left_join(factors_ff3_monthly, join_by(date)) |>
    summarize(tibble(
      "Expected utility" = mean(power_utility(portfolio_return)),
      "Average return" = 100 * mean(length_year * portfolio_return),
      "SD return" = 100 * sqrt(length_year) * sd(portfolio_return),
      "Sharpe ratio" = sqrt(length_year) * mean(portfolio_return) / sd(portfolio_return),

    )) |>
    mutate(model = str_remove(model, "return_"))

  if (capm_evaluation) {
    evaluation_capm <- evaluation |>
      left_join(factors_ff3_monthly, join_by(date)) |>
      group_by(model) |>
      summarize(
      "CAPM alpha" = coefficients(lm(portfolio_return ~ mkt_excess))[1],
      "Market beta" = coefficients(lm(portfolio_return ~ mkt_excess))[2]
      )

    evaluation_stats <- evaluation_stats |>
      left_join(evaluation_capm, join_by(model))
  }

  if (full_evaluation) {
    evaluation_weights <- weights_crsp |>
      select(date, contains("weight")) |>
      pivot_longer(-date, values_to = "weight", names_to = "model") |>
      group_by(model, date) |>
      mutate(
        "Absolute weight" = abs(weight),
        "Max. weight" = max(weight),
        "Min. weight" = min(weight),
        "Avg. sum of negative weights" = -sum(weight[weight < 0]),
        "Avg. fraction of negative weights" = sum(weight < 0) / n(),
        .keep = "none"
      ) |>
      group_by(model) |>
      summarize(across(-date, ~ 100 * mean(.))) |>
      mutate(model = str_remove(model, "weight_"))

    evaluation_stats <- evaluation_stats |>
      left_join(evaluation_weights, join_by(model))
  }

  evaluation_output <- evaluation_stats |>
    pivot_longer(cols = -model, names_to = "measure") |>
    pivot_wider(names_from = model)

  return(evaluation_output)
}
```

## Python

``` python
def evaluate_portfolio(weights_data,
                       full_evaluation=True,
                       capm_evaluation=True,
                       length_year=12):
    """Calculate portfolio evaluation measures."""
    evaluation = (weights_data
        .group_by("date")
        .agg(
          return_tilt=(pl.col("ret_excess")*pl.col("weight_tilt")).sum()
            /pl.col("weight_tilt").sum(),
          return_benchmark=(pl.col("ret_excess")*pl.col("weight_benchmark")).sum()
            /pl.col("weight_benchmark").sum()
        )
        .unpivot(index="date", on=["return_tilt", "return_benchmark"],
                 variable_name="model", value_name="portfolio_return")
        .with_columns(model=pl.col("model").str.replace("return_", ""))
    )

    evaluation_stats = (evaluation
        .group_by("model")
        .agg([
          power_utility(pl.col("portfolio_return")).mean()
            .alias("Expected utility"),
          (length_year*pl.col("portfolio_return")).mean()
            .mul(100).alias("Average return"),
          (pl.col("portfolio_return").std()*np.sqrt(length_year))
            .mul(100).alias("SD return"),
          (pl.col("portfolio_return").mean()/
           pl.col("portfolio_return").std()*np.sqrt(length_year))
            .alias("Sharpe ratio")
        ])
        .to_pandas()
        .set_index("model")
    )

    if capm_evaluation:
        evaluation_capm = (evaluation
            .join(factors_ff3_monthly, how="left", on="date")
            .to_pandas()
            .groupby("model")
            .apply(lambda x:
              pf.feols("portfolio_return ~ 1 + mkt_excess", data=x)
              .coef()
            )
            .rename(columns={"Intercept": "CAPM alpha",
                             "mkt_excess": "Market beta"})
            )
        evaluation_stats = (evaluation_stats
          .merge(evaluation_capm, how="left", on="model")
        )

    if full_evaluation:
        evaluation_weights = (weights_data
          .unpivot(index="date", on=["weight_benchmark", "weight_tilt"],
                   variable_name="model", value_name="weight")
          .group_by(["model", "date"])
          .agg([
            pl.col("weight").abs().mean().alias("Mean abs. weight"),
            pl.col("weight").max().alias("Max. weight"),
            pl.col("weight").min().alias("Min. weight"),
            pl.col("weight").filter(pl.col("weight") < 0).sum()
              .mul(-1).alias("Avg. sum of neg. weights"),
            (pl.col("weight") < 0).mean().alias("Avg. share of neg. weights")
          ])
          .group_by("model")
          .agg(pl.col(pl.Float64).mean()*100)
          .with_columns(model=pl.col("model").str.replace("weight_", ""))
          .to_pandas()
        )

        evaluation_stats = (evaluation_stats
          .merge(evaluation_weights, how="left", on="model")
          .set_index("model")
        )

    evaluation_stats = (evaluation_stats
      .transpose()
      .rename_axis(columns=None)
    )

    return evaluation_stats
```

Let us take a look at the different portfolio strategies and evaluation measures.

## R

``` r
evaluate_portfolio(weights_crsp) |>
  print(n = Inf)
```

    # A tibble: 11 × 3
       measure                            benchmark     tilt
       <chr>                                  <dbl>    <dbl>
     1 Expected utility                  -0.249     -0.260  
     2 Average return                     7.04       0.830  
     3 SD return                         15.4       21.2    
     4 Sharpe ratio                       0.457      0.0393 
     5 CAPM alpha                         0.000104  -0.00480
     6 Market beta                        0.994      0.947  
     7 Absolute weight                    0.0249     0.0638 
     8 Max. weight                        3.67       3.80   
     9 Min. weight                        0.0000266 -0.144  
    10 Avg. sum of negative weights       0         77.9    
    11 Avg. fraction of negative weights  0         49.5    

## Python

``` python
evaluate_portfolio(weights_crsp).round(2)
```

                                 tilt  benchmark
    Expected utility            -0.26      -0.25
    Average return               0.83       7.04
    SD return                   21.15      15.41
    Sharpe ratio                 0.04       0.46
    CAPM alpha                  -0.00       0.00
    Market beta                  0.95       0.99
    Mean abs. weight             0.08       0.03
    Max. weight                  4.29       4.12
    Min. weight                 -0.17       0.00
    Avg. sum of neg. weights    77.97       0.00
    Avg. share of neg. weights  49.05       0.00

The value-weighted portfolio delivers an annualized return of more than six percent and clearly outperforms the tilted portfolio, irrespective of whether we evaluate expected utility, the Sharpe ratio, or the CAPM alpha. We can conclude the market beta is close to one for both strategies (naturally almost identically one for the value-weighted benchmark portfolio). When it comes to the distribution of the portfolio weights, we see that the benchmark portfolio weight takes less extreme positions (lower average absolute weights and lower maximum weight). By definition, the value-weighted benchmark does not take any negative positions, while the tilted portfolio also takes short positions.

## Optimal Parameter Choice

Next, we move to a choice of \\\theta\\ that actually aims to improve some (or all) of the performance measures. We first define a helper function `compute_objective_function()`, which we then pass to an optimizer.

## R

``` r
compute_objective_function <- function(theta,
                                       data,
                                       objective_measure = "Expected utility",
                                       value_weighting = TRUE,
                                       allow_short_selling = TRUE) {
  processed_data <- compute_portfolio_weights(
    theta,
    data,
    value_weighting,
    allow_short_selling
  )

  objective_function <- evaluate_portfolio(
    processed_data,
    capm_evaluation = FALSE,
    full_evaluation = FALSE
  ) |>
    filter(measure == objective_measure) |>
    pull(tilt)

  return(-objective_function)
}
```

## Python

``` python
def objective_function(theta,
                       data,
                       objective_measure="Expected utility",
                       value_weighting=True,
                       allow_short_selling=True):
    """Define portfolio objective function."""

    processed_data = compute_portfolio_weights(
      theta, data, value_weighting, allow_short_selling
    )

    objective_function = evaluate_portfolio(
      processed_data,
      capm_evaluation=False,
      full_evaluation=False
    )

    objective_function = -objective_function.loc[objective_measure, "tilt"]

    return objective_function
```

You may wonder why we return the negative value of the objective function. This is simply due to the common convention for optimization procedures to search for minima as a default. By minimizing the negative value of the objective function, we get the maximum value as a result. Now, we are fully equipped to compute the optimal values of \\\hat\theta\\, which maximize the hypothetical expected utility of the investor.

## R

In its most basic form, R optimization relies on the function `optim()`. As main inputs, the function requires an initial guess of the parameters and the objective function to minimize.

``` r
optimal_theta <- optim(
  par = theta,
  fn = compute_objective_function,
  objective_measure = "Expected utility",
  data = data_portfolios,
  value_weighting = TRUE,
  allow_short_selling = TRUE,
  method = "Nelder-Mead"
)

optimal_theta$par
```

    momentum_lag     size_lag 
           0.273       -1.660 

## Python

In its most basic form, Python optimization uses the function `minimize()`. As main inputs, the function requires an initial guess of the parameters and the objective function to minimize.

``` python
optimal_theta = minimize(
    fun=objective_function,
    x0=[1.5]*n_parameters,
    args=(data_portfolios, "Expected utility", True, True),
    method="Nelder-Mead",
    tol=1e-2
)

(pd.DataFrame(
    optimal_theta.x,
    columns=["Optimal theta"],
    index=["momentum_lag", "size_lag"]).T.round(3)
)
```

                   momentum_lag  size_lag
    Optimal theta         0.267    -1.662

The resulting values of \\\hat\theta\\ are easy to interpret: intuitively, expected utility increases by tilting weights from the value-weighted portfolio toward smaller stocks (negative coefficient for size) and toward past winners (positive value for momentum). Both findings are in line with the well-documented size effect ([Banz 1981](#ref-Banz1981)) and the momentum anomaly ([Jegadeesh and Titman 1993](#ref-Jegadeesh1993)).

## More Model Specifications

How does the portfolio perform for different model specifications? For this purpose, we compute the performance of a number of different modeling choices based on the entire CRSP sample. The next code chunk performs all the heavy lifting.

## R

``` r
evaluate_optimal_performance <- function(data,
                                         objective_measure,
                                         value_weighting,
                                         allow_short_selling) {
  optimal_theta <- optim(
    par = theta,
    fn = compute_objective_function,
    data = data,
    objective_measure = objective_measure,
    value_weighting = value_weighting,
    allow_short_selling = allow_short_selling,
    method = "Nelder-Mead"
  )

  processed_data = compute_portfolio_weights(
    optimal_theta$par,
    data,
    value_weighting,
    allow_short_selling
  )

  portfolio_evaluation = evaluate_portfolio(
    processed_data,
    capm_evaluation = TRUE,
    full_evaluation = TRUE
  )

  return(portfolio_evaluation)
}

specifications <- expand_grid(
  data = list(data_portfolios),
  objective_measure = "Expected utility",
  value_weighting = c(TRUE, FALSE),
  allow_short_selling = c(TRUE, FALSE)
) |>
  mutate(
    portfolio_evaluation = pmap(
      .l = list(data, objective_measure, value_weighting, allow_short_selling),
      .f = evaluate_optimal_performance
    )
)
```

## Python

``` python
def evaluate_optimal_performance(data,
                                 objective_measure="Expected utility",
                                 value_weighting=True,
                                 allow_short_selling=True):
    """Calculate optimal portfolio performance."""

    optimal_theta = minimize(
      fun=objective_function,
      x0=[1.5]*n_parameters,
      args=(data, objective_measure, value_weighting, allow_short_selling),
      method="Nelder-Mead",
      tol=1e-3
    ).x

    processed_data = compute_portfolio_weights(
      optimal_theta, data,
      value_weighting, allow_short_selling
    )

    portfolio_evaluation = evaluate_portfolio(processed_data)

    weight_text = "VW" if value_weighting else "EW"
    short_text = "" if allow_short_selling else " (no s.)"

    strategy_name_dict = {
      "benchmark": weight_text,
      "tilt": f"{weight_text} Optimal{short_text}"
    }

    portfolio_evaluation.columns = [
      strategy_name_dict[i] for i in portfolio_evaluation.columns
    ]

    return(portfolio_evaluation)
```

Finally, we can compare the results. The table below shows summary statistics for all possible combinations: equal- or value-weighted benchmark portfolio, with or without short-selling constraints, and tilted toward maximizing expected utility.

## R

``` r
performance_table <- specifications |>
  select(
    value_weighting,
    allow_short_selling,
    portfolio_evaluation
  ) |>
  unnest(portfolio_evaluation)

performance_table |>
  rename(
    " " = benchmark,
    Optimal = tilt
  ) |>
  mutate(
    value_weighting = case_when(
      value_weighting == TRUE ~ "VW",
      value_weighting == FALSE ~ "EW"
    ),
    allow_short_selling = case_when(
      allow_short_selling == TRUE ~ "",
      allow_short_selling == FALSE ~ "(no s.)"
    )
  ) |>
  pivot_wider(
    names_from = value_weighting:allow_short_selling,
    values_from = " ":Optimal,
    names_glue = "{value_weighting} {allow_short_selling} {.value} "
  ) |>
  select(
    measure,
    `EW    `,
    `VW    `,
    sort(contains("Optimal"))
  ) |>
  print(n = 11)
```

    # A tibble: 11 × 7
       measure     `EW    ` `VW    ` `VW  Optimal ` `VW (no s.) Optimal `
       <chr>          <dbl>    <dbl>          <dbl>                 <dbl>
     1 Expected u… -0.251   -2.49e-1       -0.247                -0.247  
     2 Average re…  9.96     7.04e+0       12.7                  11.9    
     3 SD return   20.4      1.54e+1       19.2                  18.5    
     4 Sharpe rat…  0.487    4.57e-1        0.662                 0.639  
     5 CAPM alpha   0.00175  1.04e-4        0.00479               0.00392
     6 Market beta  1.13     9.94e-1        1.01                  1.03   
     7 Absolute w…  0.0249   2.49e-2        0.0340                0.0249 
     8 Max. weight  0.0249   3.67e+0        3.52                  3.08   
     9 Min. weight  0.0249   2.66e-5       -0.0263                0      
    10 Avg. sum o…  0        0             19.0                   0      
    11 Avg. fract…  0        0             36.5                   0      
    # ℹ 2 more variables: `EW  Optimal ` <dbl>,
    #   `EW (no s.) Optimal ` <dbl>

## Python

``` python
data = [data_portfolios]
value_weighting = [True, False]
allow_short_selling = [True, False]
objective_measure = ["Expected utility"]

permutations = product(
    data, objective_measure,
    value_weighting, allow_short_selling
)
results = list(starmap(
    evaluate_optimal_performance,
    permutations
))
performance_table = (pd.concat(results, axis=1)
    .T.drop_duplicates().T.round(3)
)
performance_table.get(["EW", "VW"])
```

                                    EW      EW      VW      VW
    Expected utility            -0.251  -0.251  -0.249  -0.249
    Average return               9.959   9.959   7.041   7.041
    SD return                   20.431  20.431  15.413  15.413
    Sharpe ratio                 0.487   0.487   0.457   0.457
    CAPM alpha                   0.002   0.002   0.000   0.000
    Market beta                  1.130   1.130   0.994   0.994
    Mean abs. weight             0.030   0.030   0.030   0.030
    Max. weight                  0.030   0.030   4.125   4.125
    Min. weight                  0.030   0.030   0.000   0.000
    Avg. sum of neg. weights     0.000   0.000   0.000   0.000
    Avg. share of neg. weights   0.000   0.000   0.000   0.000

``` python
performance_table.get(["EW Optimal", "VW Optimal"])
```

                                EW Optimal  VW Optimal
    Expected utility                -0.251      -0.247
    Average return                  11.060      12.743
    SD return                       21.466      19.246
    Sharpe ratio                     0.515       0.662
    CAPM alpha                       0.003       0.005
    Market beta                      1.130       1.006
    Mean abs. weight                 0.030       0.040
    Max. weight                      0.140       3.945
    Min. weight                     -0.007      -0.030
    Avg. sum of neg. weights         0.031      18.251
    Avg. share of neg. weights       0.288      36.238

``` python
performance_table.get(["EW Optimal (no s.)", "VW Optimal (no s.)"])
```

                                EW Optimal (no s.)  VW Optimal (no s.)
    Expected utility                        -0.249              -0.247
    Average return                          16.568              11.852
    SD return                               25.524              18.536
    Sharpe ratio                             0.649               0.639
    CAPM alpha                               0.007               0.004
    Market beta                              1.103               1.028
    Mean abs. weight                         0.030               0.030
    Max. weight                              0.199               3.497
    Min. weight                              0.000               0.000
    Avg. sum of neg. weights                 0.000               0.000
    Avg. share of neg. weights               0.000               0.000

The results indicate that the average annualized Sharpe ratio of the equal-weighted portfolio exceeds the Sharpe ratio of the value-weighted benchmark portfolio. Nevertheless, starting with the weighted value portfolio as a benchmark and tilting optimally with respect to momentum and small stocks yields the highest Sharpe ratio across all specifications. Finally, imposing no short-sale constraints does not improve the performance of the portfolios in our application.

## Key Takeaways

- Parametric portfolio policies estimate portfolio weights directly as functions of stock characteristics like momentum and size, avoiding the need to forecast expected returns or covariances.
- This method, based on Brandt et al. ([2009](#ref-Brandt2009)), is computationally efficient and scalable for large cross-sectional datasets such as CRSP.
- Optimization focuses on maximizing expected utility, and evaluation includes measures such as Sharpe ratio, CAPM alpha, and utility-based performance.
- Results highlight that tilting value-weighted portfolios toward small-cap and high-momentum stocks improves performance, aligning with known anomalies in finance.

## Exercises

1.  Define momentum as the rolling 12-month cumulative returns skipping the most recent month. Calculate the correlation with the measure based on market capitalization from above and compare summary statistics. How do the two measures differ?
2.  How do the estimated parameters \\\hat\theta\\ and the portfolio performance change if your objective is to maximize the Sharpe ratio instead of the hypothetical expected utility?
3.  The code above is very flexible in the sense that you can easily add new firm characteristics. Construct a new characteristic of your choice and evaluate the corresponding coefficient \\\hat\theta_i\\.
4.  Tweak the function `optimal_theta()` such that you can impose additional performance constraints in order to determine \\\hat\theta\\, which maximizes expected utility under the constraint that the market beta is below 1.
5.  Does the portfolio performance resemble a realistic out-of-sample backtesting procedure? Verify the robustness of the results by first estimating \\\hat\theta\\ based on *past data* only. Then, use more recent periods to evaluate the actual portfolio performance.
6.  By formulating the portfolio problem as a statistical estimation problem, you can easily obtain standard errors for the coefficients of the weight function. Brandt et al. ([2009](#ref-Brandt2009)) provide the relevant derivations in their paper in Equation (10). Implement a small function that computes standard errors for \\\hat\theta\\.

## References

Banz, Rolf W. 1981. “The relationship between return and market value of common stocks.” *Journal of Financial Economics* 9 (1): 3–18. <https://doi.org/10.1016/0304-405X(81)90018-0>.

Brandt, Michael W. 2010. “Portfolio choice problems.” In *Handbook of Financial Econometrics: Tools and Techniques*, edited by Yacine Ait-Sahalia and Lars Peter Hansen, vol. 1. Handbooks in Finance. North-Holland. <https://doi.org/10.1016/B978-0-444-50897-3.50008-0>.

Brandt, Michael W, Pedro Santa-Clara, and Rossen Valkanov. 2009. “Parametric portfolio policies: Exploiting characteristics in the cross-section of equity returns.” *Review of Financial Studies* 22 (9): 3411–47. <https://doi.org/10.1093/rfs/hhp003>.

Gehrig, Thomas, Leopold Sögner, and Arne Westerkamp. 2020. “Making portfolio policies work.” *Working Paper*. <http://dx.doi.org/10.2139/ssrn.3081100>.

Jegadeesh, Narasimhan, and Sheridan Titman. 1993. “Returns to buying winners and selling losers: Implications for stock market efficiency.” *The Journal of Finance* 48 (1): 65–91. <https://doi.org/10.1111/j.1540-6261.1993.tb04702.x>.

Virtanen, Pauli, Ralf Gommers, Travis E. Oliphant, et al. 2020. “SciPy 1.0: Fundamental Algorithms for Scientific Computing in Python.” *Nature Methods* 17: 261–72. <https://doi.org/10.1038/s41592-019-0686-2>.
