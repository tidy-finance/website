# Fama-MacBeth Regressions

> **NOTE:**
>
> You are reading **Tidy Finance with Python**. You can find the equivalent chapter for the sibling **Tidy Finance with R** [here](../r/fama-macbeth-regressions.llms.md).

In this chapter, we present a simple implementation of Fama and MacBeth ([1973](#ref-Fama1973)), a regression approach commonly called Fama-MacBeth regressions. Fama-MacBeth regressions are widely used in empirical asset pricing studies. We use individual stocks as test assets to estimate the risk premium associated with the three factors included in Fama and French ([1993](#ref-Fama1993)).

Researchers use the two-stage regression approach to estimate risk premiums in various markets, but predominately in the stock market. Essentially, the two-step Fama-MacBeth regressions exploit a linear relationship between expected returns and exposure to (priced) risk factors. The basic idea of the regression approach is to project asset returns on factor exposures or characteristics that resemble exposure to a risk factor in the cross-section in each time period. Then, in the second step, the estimates are aggregated across time to test if a risk factor is priced. In principle, Fama-MacBeth regressions can be used in the same way as portfolio sorts introduced in previous chapters.

The Fama-MacBeth procedure is a simple two-step approach: The first step uses the exposures (characteristics) as explanatory variables in \\T\\ cross-sectional regressions. For example, if \\r\_{i,t+1}\\ denote the excess returns of asset \\i\\ in month \\t+1\\, then the famous Fama-French three-factor model implies the following return generating process (see also [Campbell et al. 1998](#ref-Campbell1998)):

\\\begin{aligned}r\_{i,t+1} = \alpha_i + \lambda^{M}\_t \beta^M\_{i,t} + \lambda^{SMB}\_t \beta^{SMB}\_{i,t} + \lambda^{HML}\_t \beta^{HML}\_{i,t} + \epsilon\_{i,t}.\end{aligned} \tag{1}\\

Here, we are interested in the compensation \\\lambda^{f}\_t\\ for the exposure to each risk factor \\\beta^{f}\_{i,t}\\ at each time point, i.e., the risk premium. Note the terminology: \\\beta^{f}\_{i,t}\\ is an asset-specific characteristic, e.g., a factor exposure or an accounting variable. *If* there is a linear relationship between expected returns and the characteristic in a given month, we expect the regression coefficient to reflect the relationship, i.e., \\\lambda_t^{f}\neq0\\.

In the second step, the time-series average \\\frac{1}{T}\sum\_{t=1}^T \hat\lambda^{f}\_t\\ of the estimates \\\hat\lambda^{f}\_t\\ can then be interpreted as the risk premium for the specific risk factor \\f\\. We follow Zaffaroni and Zhou ([2022](#ref-Zaffaroni2022)) and consider the standard cross-sectional regression to predict future returns. If the characteristics are replaced with time \\t+1\\ variables, then the regression approach captures risk attributes rather than risk premiums.

Before we move to the implementation, we want to highlight that the characteristics, e.g., \\\hat\beta^{f}\_{i}\\, are often estimated in a separate step before applying the actual Fama-MacBeth methodology. You can think of this as a *step 0*. You might thus worry that the errors of \\\hat\beta^{f}\_{i}\\ impact the risk premiums’ standard errors. Measurement error in \\\hat\beta^{f}\_{i}\\ indeed affects the risk premium estimates, i.e., they lead to biased estimates. The literature provides adjustments for this bias (see, e.g., [Shanken 1992](#ref-Shanken1992); [Kim 1995](#ref-Kim1995); [Chen et al. 2015](#ref-Chen2015), among others) but also shows that the bias goes to zero as \\T \to \infty\\. We refer to Gagliardini et al. ([2016](#ref-Gagliardini2016)) for an in-depth discussion also covering the case of time-varying betas. Moreover, if you plan to use Fama-MacBeth regressions with individual stocks, Hou et al. ([2020](#ref-Hou2020)) advocates using weighted-least squares to estimate the coefficients such that they are not biased toward small firms. Without this adjustment, the high number of small firms would drive the coefficient estimates.

The current chapter relies on this set of Python packages.

``` python
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
```

## Data Preparation

We illustrate Fama and MacBeth ([1973](#ref-Fama1973)) with the monthly CRSP sample and use three characteristics to explain the cross-section of returns: Market capitalization, the book-to-market ratio, and the CAPM beta (i.e., the covariance of the excess stock returns with the market excess returns). We collect the data from our Parquet files introduced in [Accessing and Managing Financial Data](../python/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../python/wrds-crsp-and-compustat.llms.md).

``` python
crsp_monthly = (
    pd.read_parquet("data-python/crsp_monthly.parquet")
    .get(["permno", "gvkey", "date", "ret_excess", "mktcap"])
)

compustat_annual = (
    pd.read_parquet("data-python/compustat_annual.parquet")
    .get(["datadate", "gvkey", "be"])
)

beta = (
    pd.read_parquet("data-python/beta.parquet")
    .query("return_type == 'monthly'")
    .get(["permno", "date", "beta"])
)
```

We use the Compustat and CRSP data to compute the book-to-market ratio and the (log) market capitalization. Furthermore, we also use the CAPM betas based on monthly returns we computed in the previous chapters.

``` python
characteristics = (compustat_annual
    .assign(date=lambda x: x["datadate"].dt.to_period("M").dt.to_timestamp())
    .merge(crsp_monthly, how="left", on=["gvkey", "date"], )
    .merge(beta, how="left", on=["permno", "date"])
    .assign(
        bm=lambda x: x["be"]/x["mktcap"],
        log_mktcap=lambda x: np.log(x["mktcap"]),
        sorting_date=lambda x: x["date"]+pd.DateOffset(months=6)
    )
    .get(["gvkey", "bm", "log_mktcap", "beta", "sorting_date"])
)

data_fama_macbeth = (crsp_monthly
    .merge(characteristics, 
            how="left",
            left_on=["gvkey", "date"], right_on=["gvkey", "sorting_date"])
    .sort_values(["date", "permno"])
    .groupby("permno")
    .apply(lambda x: x.assign(
        beta=x["beta"].fillna(method="ffill"),
        bm=x["bm"].fillna(method="ffill"),
        log_mktcap=x["log_mktcap"].fillna(method="ffill")
        )
    )
    .reset_index(drop=True)  
)

data_fama_macbeth_lagged = (data_fama_macbeth
    .assign(date=lambda x: x["date"]-pd.DateOffset(months=1))
    .get(["permno", "date", "ret_excess"])
    .rename(columns={"ret_excess": "ret_excess_lead"})
)

data_fama_macbeth = (data_fama_macbeth
    .merge(data_fama_macbeth_lagged, how="left", on=["permno", "date"])
    .get(["permno", "date", "ret_excess_lead", "beta", "log_mktcap", "bm"])
    .dropna()
)
```

## Cross-Sectional Regression

Next, we run the cross-sectional regressions with the characteristics as explanatory variables for each month. We regress the returns of the test assets at a particular time point on the characteristics of each asset. By doing so, we get an estimate of the risk premiums \\\hat\lambda^{f}\_t\\ for each point in time.

``` python
risk_premiums = (data_fama_macbeth
    .groupby("date")
    .apply(lambda x: smf.ols(
        formula="ret_excess_lead ~ beta + log_mktcap + bm", 
        data=x
        ).fit()
        .params
    )
    .reset_index()
)
```

## Time-Series Aggregation

Now that we have the risk premiums’ estimates for each period, we can average across the time-series dimension to get the expected risk premium for each characteristic. Similarly, we manually create the \\t\\-test statistics for each regressor, which we can then compare to usual critical values of 1.96 or 2.576 for two-tailed significance tests at a five percent and a one percent significance level.

``` python
price_of_risk = (risk_premiums
    .melt(id_vars="date", var_name="factor", value_name="estimate")
    .groupby("factor")["estimate"]
    .apply(lambda x: pd.Series({
        "risk_premium": x.mean(),
        "t_statistic": x.mean()/x.std()*np.sqrt(len(x))
        })
    )
    .reset_index()
    .pivot(index="factor", columns="level_1", values="estimate")
    .reset_index()
)
```

It is common to adjust for autocorrelation when reporting standard errors of risk premiums. As in [Univariate Portfolio Sorts](../python/univariate-portfolio-sorts.llms.md), the typical procedure for this is computing Newey and West ([1987](#ref-Newey1987)) standard errors.

``` python
price_of_risk_newey_west = (risk_premiums
    .melt(id_vars="date", var_name="factor", value_name="estimate")
    .groupby("factor")
    .apply(lambda x: (
        x["estimate"].mean()/ 
            smf.ols("estimate ~ 1", x)
            .fit(cov_type="HAC", cov_kwds={"maxlags": 6}).bse
        )
    )
    .reset_index()
    .rename(columns={"Intercept": "t_statistic_newey_west"})
)

(price_of_risk
    .merge(price_of_risk_newey_west, on="factor")
    .round(3)
)
```

|     | factor     | risk_premium | t_statistic | t_statistic_newey_west |
|-----|------------|--------------|-------------|------------------------|
| 0   | Intercept  | 0.012        | 4.660       | 4.067                  |
| 1   | beta       | 0.000        | 0.006       | 0.005                  |
| 2   | bm         | 0.002        | 3.016       | 2.833                  |
| 3   | log_mktcap | -0.001       | -2.812      | -2.639                 |

Finally, let us interpret the results. Stocks with higher book-to-market ratios earn higher expected future returns, which is in line with the value premium. The negative value for log market capitalization reflects the size premium for smaller stocks. Consistent with results from earlier chapters, we detect no relation between beta and future stock returns.

``` python
import tidyfinance as tf

tf.estimate_fama_macbeth(
    data=data_fama_macbeth,
    model="ret_excess_lead ~ beta + bm + log_mktcap",
    vcov="newey-west"
)
```

## Key Takeaways

- Fama-MacBeth regressions provide a two-step approach to estimate risk premiums by running time-series averages of cross-sectional regressions on asset characteristics.
- Fama-MacBeth regressions are commonly used in empirical asset pricing to test whether factors like size, value, or market beta are priced in the cross-section of stock returns.
- Measurement error in factor exposures, especially when estimated beforehand, can bias results, but corrections such as Newey-West standard errors and weighted regressions can improve accuracy.
- The `tidyfinance` Python package provides a user-friendly `estimate_fama_macbeth()` function that simplifies the Fama-MacBeth estimation pipeline.

## Exercises

1.  Estimate stock-specific value and size risk factors to similar to the CAPM-beta using rolling estimation based on Fama-French 3 Factors. Use these estimates instead of the stock characteristics in the Fama-MacBeth regression from above. How do the coefficient estimates differ?
2.  Download the 49 Industry Portfolios from Ken French data library. Use these industry portfolios instead of the stocks to estimate the three rolling risk-factors (beta, value, size). Replicate the Fama-MacBeth regression from above. Are the coefficient estimates similar?
3.  Use individual stocks with weighted-least squares based on a firm’s size as suggested by Hou et al. ([2020](#ref-Hou2020)). Then, repeat the Fama-MacBeth regressions without the weighting-scheme adjustment but drop the smallest 20 percent of firms each month. Compare the results of the three approaches.

## References

Campbell, John Y., Andrew W. Lo, A. Craig MacKinlay, and Robert F. Whitelaw. 1998. “The Econometrics of Financial Markets.” *Macroeconomic Dynamics* 2 (4): 559–62. <https://doi.org/10.1017/S1365100598009092>.

Chen, Hong-Yi, Alice C Lee, and Cheng-Few Lee. 2015. “Alternative errors-in-variables models and their applications in finance research.” *The Quarterly Review of Economics and Finance* 58: 213–27. <https://doi.org/10.1016/j.qref.2014.12.002>.

Fama, Eugene F., and Kenneth R. French. 1993. “Common risk factors in the returns on stocks and bonds.” *Journal of Financial Economics* 33 (1): 3–56. <https://doi.org/10.1016/0304-405X(93)90023-5>.

Fama, Eugene F., and James D. MacBeth. 1973. “Risk, return, and equilibrium: Empirical tests.” *Journal of Political Economy* 81 (3): 607–36. <https://doi.org/10.1086/260061>.

Gagliardini, Patrick, Elisa Ossola, and Olivier Scaillet. 2016. “Time-varying risk premium in large cross-sectional equity data sets.” *Econometrica* 84 (3): 985–1046. <https://doi.org/10.3982/ECTA11069>.

Hou, Kewei, Chen Xue, and Lu Zhang. 2020. “Replicating anomalies.” *Review of Financial Studies* 33 (5): 2019–133. <https://doi.org/10.1093/rfs/hhy131>.

Kim, Dongcheol. 1995. “The errors in the variables problem in the cross-section of expected stock returns.” *The Journal of Finance* 50 (5): 1605–34. <https://doi.org/10.1111/j.1540-6261.1995.tb05190.x>.

Newey, Whitney K., and Kenneth D. West. 1987. “A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance Matrix.” *Econometrica* 55 (3): 703–8. <http://www.jstor.org/stable/1913610>.

Shanken, Jay. 1992. “On the estimation of beta-pricing models.” *Review of Financial Studies* 5 (1): 1–33. <https://doi.org/10.1093/rfs/5.1.1>.

Zaffaroni, Paolo, and Guofu Zhou. 2022. “Asset pricing: Cross-section predictability.” *Working Paper*. <http://dx.doi.org/10.2139/ssrn.4111428>.
