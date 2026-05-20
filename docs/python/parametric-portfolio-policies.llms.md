# Parametric Portfolio Policies

> **NOTE:**
>
> You are reading **Tidy Finance with Python**. You can find the equivalent chapter for the sibling **Tidy Finance with R** [here](../r/parametric-portfolio-policies.llms.md).

In this chapter, we apply different portfolio performance measures to evaluate and compare portfolio allocation strategies. For this purpose, we introduce a direct way to estimate optimal portfolio weights for large-scale cross-sectional applications. More precisely, the approach of Brandt et al. ([2009](#ref-Brandt2009)) proposes to parametrize the optimal portfolio weights as a function of stock characteristics instead of estimating the stock’s expected return, variance, and covariances with other stocks in a prior step. We choose weights as a function of characteristics that maximize the expected utility of the investor. This approach is feasible for large portfolio dimensions (such as the entire CRSP universe) and has been proposed by Brandt et al. ([2009](#ref-Brandt2009)). See the review paper by Brandt ([2010](#ref-Brandt2010)) for an excellent treatment of related portfolio choice methods.

The current chapter relies on the following set of Python packages:

``` python
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

from itertools import product, starmap
from scipy.optimize import minimize
```

Compared to previous chapters, we introduce the `scipy.optimize` module from the `scipy` ([Virtanen et al. 2020](#ref-scipy)) for solving optimization problems.

## Data Preparation

To get started, we load the monthly CRSP file, which forms our investment universe. We load the data from our Parquet files introduced in [Accessing and Managing Financial Data](../python/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../python/wrds-crsp-and-compustat.llms.md).

``` python
crsp_monthly = (
    pd.read_parquet("data-python/crsp_monthly.parquet")
    .dropna()
)
```

To evaluate the performance of portfolios, we further use monthly market returns as a benchmark to compute CAPM alphas.

``` python
factors_ff_monthly = (
    pd.read_parquet("data-python/factors_ff3_monthly.parquet")
    .get(["date", "mkt_excess"])
)
```

Next, we retrieve some stock characteristics that have been shown to have an effect on the expected returns or expected variances (or even higher moments) of the return distribution. In particular, we record the lagged one-year return momentum (`momentum_lag`), defined as the compounded return between months \\t-13\\ and \\t-2\\ for each firm, which we calculate using market capitalization for simplicity. In finance, momentum is the empirically observed tendency for rising asset prices to rise further and falling prices to keep falling ([Jegadeesh and Titman 1993](#ref-Jegadeesh1993)). We refer to the exercise for a more elaborate measure of momentum. The second characteristic is the firm’s market equity (`size_lag`), defined as the log of the price per share times the number of shares outstanding ([Banz 1981](#ref-Banz1981)). To construct the correct lagged values, we use the approach introduced in [WRDS, CRSP, and Compustat](../python/wrds-crsp-and-compustat.llms.md).

``` python
crsp_monthly_lags = (crsp_monthly
    .assign(date=lambda x: x["date"]+pd.DateOffset(months=13))
    .get(["permno", "date", "mktcap"])
)

crsp_monthly = (crsp_monthly
    .merge(crsp_monthly_lags, 
            how="inner", on=["permno", "date"], suffixes=["", "_13"])
)

data_portfolios = (crsp_monthly
    .assign(
        momentum_lag=lambda x: x["mktcap_lag"]/x["mktcap_13"],
        size_lag=lambda x: np.log(x["mktcap_lag"])
    )
    .dropna(subset=["momentum_lag", "size_lag"])
)
```

## Parametric Portfolio Policies

The basic idea of parametric portfolio weights is as follows. Suppose that at each date \\t\\, we have \\N_t\\ stocks in the investment universe, where each stock \\i\\ has a return of \\r\_{i, t+1}\\ and is associated with a vector of firm characteristics \\x\_{i, t}\\ such as time-series momentum or the market capitalization. The investor’s problem is to choose portfolio weights \\w\_{i,t}\\ to maximize the expected utility of the portfolio return:

\\ \begin{aligned} \max\_{\omega} E_t\left(u(r\_{p, t+1})\right) = E_t\left\[u\left(\sum\limits\_{i=1}^{N_t}\omega\_{i,t}\cdot r\_{i,t+1}\right)\right\] \end{aligned} \tag{1}\\

where \\u(\cdot)\\ denotes the utility function.

Where do the stock characteristics show up? We parameterize the optimal portfolio weights as a function of the stock characteristic \\x\_{i,t}\\ with the following linear specification for the portfolio weights:

\\ \omega\_{i,t} = \bar{\omega}\_{i,t} + \frac{1}{N_t}\theta'\hat{x}\_{i,t}, \tag{2}\\

where \\\bar{\omega}\_{i,t}\\ is a stock’s weight in a benchmark portfolio (we use the value-weighted or naive portfolio in the application below), \\\theta\\ is a vector of coefficients which we are going to estimate, and \\\hat{x}\_{i,t}\\ are the characteristics of stock \\i\\, cross-sectionally standardized to have zero mean and unit standard deviation.

Intuitively, the portfolio strategy is a form of active portfolio management relative to a performance benchmark. Deviations from the benchmark portfolio are derived from the individual stock characteristics. Note that by construction, the weights sum up to one as \\\sum\_{i=1}^{N_t}\hat{x}\_{i,t} = 0\\ due to the standardization. Moreover, the coefficients are constant across assets and over time. The implicit assumption is that the characteristics fully capture all aspects of the joint distribution of returns that are relevant for forming optimal portfolios.

We first implement cross-sectional standardization for the entire CRSP universe. We also keep track of (lagged) relative market capitalization `relative_mktcap`, which will represent the value-weighted benchmark portfolio, while `n` denotes the number of traded assets \\N_t\\, which we use to construct the naive portfolio benchmark.

``` python
data_portfolios = (data_portfolios
    .groupby("date")
    .apply(lambda x: x.assign(
        relative_mktcap=x["mktcap_lag"]/x["mktcap_lag"].sum()
        )
    )
    .reset_index(drop=True)
    .set_index("date")
    .groupby(level="date")
    .transform(
        lambda x: (x-x.mean())/x.std() if x.name.endswith("lag") else x
    )
    .reset_index()
    .drop(["mktcap_lag"], axis=1)
)
```

## Computing Portfolio Weights

Next, we move on to identify optimal choices of \\\theta\\. We rewrite the optimization problem together with the weight parametrization and can then estimate \\\theta\\ to maximize the objective function based on our sample

\\ \begin{aligned} E_t\left(u(r\_{p, t+1})\right) = \frac{1}{T}\sum\limits\_{t=0}^{T-1}u\left(\sum\limits\_{i=1}^{N_t}\left(\bar{\omega}\_{i,t} + \frac{1}{N_t}\theta'\hat{x}\_{i,t}\right)r\_{i,t+1}\right). \end{aligned} \tag{3}\\

The allocation strategy is straightforward because the number of parameters to estimate is small. Instead of a tedious specification of the \\N_t\\ dimensional vector of expected returns and the \\N_t(N_t+1)/2\\ free elements of the covariance matrix, all we need to focus on in our application is the vector \\\theta\\. \\\theta\\ contains only two elements in our application: the relative deviation from the benchmark due to *size* and *momentum*.

To get a feeling for the performance of such an allocation strategy, we start with an arbitrary initial vector \\\theta_0\\. The next step is to choose \\\theta\\ optimally to maximize the objective function. We automatically detect the number of parameters by counting the number of columns with lagged values. Note that the value for \\\theta\\ of 1.5 is an arbitrary choice.

``` python
lag_columns = [i for i in data_portfolios.columns if "lag" in i]
n_parameters = len(lag_columns)
theta = pd.DataFrame({"theta": [1.5]*n_parameters}, index=lag_columns)
```

The function `compute_portfolio_weights()` below computes the portfolio weights \\\bar{\omega}\_{i,t} + \frac{1}{N_t}\theta'\hat{x}\_{i,t}\\ according to our parametrization for a given value \\\theta_0\\. Everything happens within a single pipeline. Hence, we provide a short walk-through.

We first compute `characteristic_tilt`, the tilting values \\\frac{1}{N_t}\theta'\hat{x}\_{i, t}\\ which resemble the deviation from the benchmark portfolio. Next, we compute the benchmark portfolio `weight_benchmark`, which can be any reasonable set of portfolio weights. In our case, we choose either the value or equal-weighted allocation. `weight_tilt` completes the picture and contains the final portfolio weights `weight_tilt = weight_benchmark + characteristic_tilt`, which deviate from the benchmark portfolio depending on the stock characteristics.

The final few lines go a bit further and implement a simple version of a no-short sale constraint. While it is generally not straightforward to ensure portfolio weight constraints via parameterization, we simply normalize the portfolio weights such that they are enforced to be positive. Finally, we make sure that the normalized weights sum up to one again:

\\ \omega\_{i,t}^+ = \frac{\max(0, \omega\_{i,t})}{\sum\_{j=1}^{N_t}\max(0, \omega\_{i,t})}. \tag{4}\\

The following function computes the optimal portfolio weights in the way just described.

``` python
def compute_portfolio_weights(theta, 
                              data,
                              value_weighting=True,
                              allow_short_selling=True):
    """Compute portfolio weights for different strategies."""
    
    lag_columns = [i for i in data.columns if "lag" in i]
    theta = pd.DataFrame(theta, index=lag_columns)

    data = (data
      .groupby("date")
      .apply(lambda x: x.assign(
          characteristic_tilt=x[theta.index] @ theta / x.shape[0]
        )
      )
      .reset_index(drop=True)
      .assign(
        weight_benchmark=lambda x: 
          x["relative_mktcap"] if value_weighting else 1/x.shape[0],
        weight_tilt=lambda x: 
          x["weight_benchmark"] + x["characteristic_tilt"]
      )
      .drop(columns=["characteristic_tilt"])
    )

    if not allow_short_selling:
        data = (data
          .assign(weight_tilt=lambda x: np.maximum(0, x["weight_tilt"]))
        )

    data = (data
      .groupby("date")
      .apply(lambda x: x.assign(
        weight_tilt=lambda x: x["weight_tilt"]/x["weight_tilt"].sum()))
      .reset_index(drop=True)
    )

    return data
```

In the next step, we compute the portfolio weights for the arbitrary vector \\\theta_0\\. In the example below, we use the value-weighted portfolio as a benchmark and allow negative portfolio weights.

``` python
weights_crsp = compute_portfolio_weights(
    theta,
    data_portfolios,
    value_weighting=True,
    allow_short_selling=True
)
```

## Portfolio Performance

Are the computed weights optimal in any way? Most likely not, as we picked \\\theta_0\\ arbitrarily. To evaluate the performance of an allocation strategy, one can think of many different approaches. In their original paper, Brandt et al. ([2009](#ref-Brandt2009)) focus on a simple evaluation of the hypothetical utility of an agent equipped with a power utility function

\\ u\_\gamma(r) = \frac{(1 + r)^{(1-\gamma)}}{1-\gamma}, \tag{5}\\ where \\\gamma\\ is the risk aversion factor.

``` python
def power_utility(r, gamma=5):
    """Calculate power utility for given risk aversion."""
    
    utility = ((1+r)**(1-gamma))/(1-gamma)
    
    return utility
```

We want to note that Gehrig et al. ([2020](#ref-Gehrig2020)) warn that, in the leading case of constant relative risk aversion (CRRA), strong assumptions on the properties of the returns, the variables used to implement the parametric portfolio policy, and the parameter space are necessary to obtain a well-defined optimization problem.

No doubt, there are many other ways to evaluate a portfolio. The function below provides a summary of all kinds of interesting measures that can be considered relevant. Do we need all these evaluation measures? It depends: The original paper by Brandt et al. ([2009](#ref-Brandt2009)) only cares about the expected utility to choose \\\theta\\. However, if you want to choose optimal values that achieve the highest performance while putting some constraints on your portfolio weights, it is helpful to have everything in one function.

``` python
def evaluate_portfolio(weights_data,
                       full_evaluation=True,
                       capm_evaluation=True,
                       length_year=12):
    """Calculate portfolio evaluation measures."""
    evaluation = (weights_data
        .groupby("date")
        .apply(lambda x: pd.Series(
          np.average(x[["ret_excess", "ret_excess"]],
                     weights=x[["weight_tilt", "weight_benchmark"]],
                     axis=0),
          ["return_tilt", "return_benchmark"])
        )
        .reset_index()
        .melt(id_vars="date", var_name="model",
              value_vars=["return_tilt", "return_benchmark"],
              value_name="portfolio_return")
        .assign(model=lambda x: x["model"].str.replace("return_", ""))
    )

    evaluation_stats = (evaluation
        .groupby("model")["portfolio_return"]
        .aggregate([
          ("Expected utility", lambda x: np.mean(power_utility(x))),
          ("Average return", lambda x: np.mean(length_year*x)*100),
          ("SD return", lambda x: np.std(x)*np.sqrt(length_year)*100),
          ("Sharpe ratio", lambda x: (np.mean(x)/np.std(x)* 
                                        np.sqrt(length_year)))
        ])
    )

    if capm_evaluation:
        evaluation_capm = (evaluation
            .merge(factors_ff_monthly, how="left", on="date")
            .groupby("model")
            .apply(lambda x: 
              smf.ols(formula="portfolio_return ~ 1 + mkt_excess", data=x)
              .fit().params
            )
            .rename(columns={"const": "CAPM alpha",
                             "mkt_excess": "Market beta"})
            )
        evaluation_stats = (evaluation_stats
          .merge(evaluation_capm, how="left", on="model")
        )

    if full_evaluation:
        evaluation_weights = (weights_data
          .melt(id_vars="date", var_name="model",
                value_vars=["weight_benchmark", "weight_tilt"],
                value_name="weight")
          .groupby(["model", "date"])["weight"]
          .aggregate([
            ("Mean abs. weight", lambda x: np.mean(abs(x))),
            ("Max. weight", lambda x: max(x)),
            ("Min. weight", lambda x: min(x)),
            ("Avg. sum of neg. weights", lambda x: -np.sum(x[x < 0])),
            ("Avg. share of neg. weights", lambda x: np.mean(x < 0))
          ])
          .reset_index()
          .drop(columns=["date"])
          .groupby(["model"])
          .aggregate(lambda x: np.average(x)*100)
          .reset_index()
          .assign(model=lambda x: x["model"].str.replace("weight_", ""))
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

``` python
evaluate_portfolio(weights_crsp).round(2)
```

|                            | benchmark | tilt  |
|----------------------------|-----------|-------|
| Expected utility           | -0.25     | -0.26 |
| Average return             | 7.05      | 0.83  |
| SD return                  | 15.41     | 21.09 |
| Sharpe ratio               | 0.46      | 0.04  |
| Intercept                  | 0.00      | -0.00 |
| Market beta                | 0.99      | 0.94  |
| Mean abs. weight           | 0.04      | 0.09  |
| Max. weight                | 4.29      | 4.48  |
| Min. weight                | 0.00      | -0.20 |
| Avg. sum of neg. weights   | 0.00      | 77.88 |
| Avg. share of neg. weights | 0.00      | 48.71 |

The value-weighted portfolio delivers an annualized return of more than six percent and clearly outperforms the tilted portfolio, irrespective of whether we evaluate expected utility, the Sharpe ratio, or the CAPM alpha. We can conclude the market beta is close to one for both strategies (naturally almost identically one for the value-weighted benchmark portfolio). When it comes to the distribution of the portfolio weights, we see that the benchmark portfolio weight takes less extreme positions (lower average absolute weights and lower maximum weight). By definition, the value-weighted benchmark does not take any negative positions, while the tilted portfolio also takes short positions.

## Optimal Parameter Choice

Next, we move to a choice of \\\theta\\ that actually aims to improve some (or all) of the performance measures. We first define the helper function `compute_objective_function()`, which we then pass to an optimizer.

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

You may wonder why we return the negative value of the objective function. This is simply due to the common convention for optimization procedures to search for minima as a default. By minimizing the negative value of the objective function, we get the maximum value as a result. In its most basic form, Python optimization uses the function `minimize()`. As main inputs, the function requires an initial guess of the parameters and the objective function to minimize. Now, we are fully equipped to compute the optimal values of \\\hat\theta\\, which maximize the hypothetical expected utility of the investor.

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

|               | momentum_lag | size_lag |
|---------------|--------------|----------|
| Optimal theta | 0.219        | -1.625   |

The resulting values of \\\hat\theta\\ are easy to interpret: intuitively, expected utility increases by tilting weights from the value-weighted portfolio toward smaller stocks (negative coefficient for size) and toward past winners (positive value for momentum). Both findings are in line with the well-documented size effect ([Banz 1981](#ref-Banz1981)) and the momentum anomaly ([Jegadeesh and Titman 1993](#ref-Jegadeesh1993)).

## More Model Specifications

How does the portfolio perform for different model specifications? For this purpose, we compute the performance of a number of different modeling choices based on the entire CRSP sample. The next code chunk performs all the heavy lifting.

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
      tol=10e-2
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

|                            | EW     | VW     |
|----------------------------|--------|--------|
| Expected utility           | -0.251 | -0.249 |
| Average return             | 9.992  | 7.048  |
| SD return                  | 20.454 | 15.410 |
| Sharpe ratio               | 0.489  | 0.457  |
| Intercept                  | 0.002  | 0.000  |
| Market beta                | 1.138  | 0.995  |
| Mean abs. weight           | 0.000  | 0.036  |
| Max. weight                | 0.000  | 4.290  |
| Min. weight                | 0.000  | 0.000  |
| Avg. sum of neg. weights   | 0.000  | 0.000  |
| Avg. share of neg. weights | 0.000  | 0.000  |

``` python
performance_table.get(["EW Optimal", "VW Optimal"])
```

|                            | EW Optimal | VW Optimal |
|----------------------------|------------|------------|
| Expected utility           | -25.923    | -0.260     |
| Average return             | -4549.172  | 0.829      |
| SD return                  | 14990.324  | 21.090     |
| Sharpe ratio               | -0.303     | 0.039      |
| Intercept                  | -3.226     | -0.005     |
| Market beta                | -97.529    | 0.939      |
| Mean abs. weight           | 104.055    | 0.092      |
| Max. weight                | 1467.888   | 4.480      |
| Min. weight                | -353.727   | -0.205     |
| Avg. sum of neg. weights   | 82132.315  | 77.884     |
| Avg. share of neg. weights | 51.548     | 48.707     |

``` python
performance_table.get(["EW Optimal (no s.)", "VW Optimal (no s.)"])
```

|                            | EW Optimal (no s.) | VW Optimal (no s.) |
|----------------------------|--------------------|--------------------|
| Expected utility           | -0.252             | -0.250             |
| Average return             | 8.011              | 7.539              |
| SD return                  | 19.113             | 16.654             |
| Sharpe ratio               | 0.419              | 0.453              |
| Intercept                  | 0.000              | 0.000              |
| Market beta                | 1.138              | 1.056              |
| Mean abs. weight           | 0.036              | 0.036              |
| Max. weight                | 1.410              | 2.485              |
| Min. weight                | 0.000              | 0.000              |
| Avg. sum of neg. weights   | 0.000              | 0.000              |
| Avg. share of neg. weights | 0.000              | 0.000              |

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
