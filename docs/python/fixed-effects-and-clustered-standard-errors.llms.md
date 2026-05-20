# Fixed Effects and Clustered Standard Errors

> **NOTE:**
>
> You are reading **Tidy Finance with Python**. You can find the equivalent chapter for the sibling **Tidy Finance with R** [here](../r/fixed-effects-and-clustered-standard-errors.llms.md).

In this chapter, we provide an intuitive introduction to the two popular concepts of *fixed effects regressions* and *clustered standard errors*. When working with regressions in empirical finance, you will sooner or later be confronted with discussions around how you deal with omitted variables bias and dependence in your residuals. The concepts we introduce in this chapter are designed to address such concerns.

We focus on a classical panel regression common to the corporate finance literature (e.g., [Fazzari et al. 1988](#ref-Fazzari1988); [Erickson and Whited 2012](#ref-Erickson2012); [Gulen and Ion 2015](#ref-Gulen2015)): firm investment modeled as a function that increases in firm cash flow and firm investment opportunities.

Typically, this investment regression uses quarterly balance sheet data provided via Compustat because it allows for richer dynamics in the regressors and more opportunities to construct variables. As we focus on the implementation of fixed effects and clustered standard errors, we use the annual Compustat data from our previous chapters and leave the estimation using quarterly data as an exercise. We demonstrate below that the regression based on annual data yields qualitatively similar results to estimations based on quarterly data from the literature, namely confirming the positive relationships between investment and the two regressors.

The current chapter relies on the following set of Python packages.

``` python
import pandas as pd
import numpy as np
import datetime as dt
import pyfixest as pf
```

Compared to previous chapters, we introduce `pyfixest` ([Fischer 2024](#ref-pyfixest)), which provides tools for estimating various econometric models such as panel regressions.

## Data Preparation

We use CRSP and annual Compustat as data sources from our Parquet files introduced in [Accessing and Managing Financial Data](../python/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../python/wrds-crsp-and-compustat.llms.md). In particular, Compustat provides balance sheet and income statement data on a firm level, while CRSP provides market valuations.

``` python
crsp_monthly = (
    pd.read_parquet("data-python/crsp_monthly.parquet")
    .get(["gvkey", "date", "mktcap"])
)

compustat_annual = (
    pd.read_parquet("data-python/compustat_annual.parquet")
    .get(["datadate", "gvkey", "year", "at", "be", "capx", "oancf", "txdb"])
)
```

The classical investment regressions model is the capital investment of a firm as a function of operating cash flows and Tobin’s q, a measure of a firm’s investment opportunities. We start by constructing investment and cash flows which are usually normalized by lagged total assets of a firm. In the following code chunk, we construct a *panel* of firm-year observations, so we have both cross-sectional information on firms as well as time-series information for each firm.

``` python
data_investment = (compustat_annual
    .assign(
        date=lambda x: (
        pd.to_datetime(x["datadate"]).dt.to_period("M").dt.to_timestamp()
        )
    )
    .merge(compustat_annual
            .get(["gvkey", "year", "at"])
            .rename(columns={"at": "at_lag"})
            .assign(year=lambda x: x["year"]+1), 
            on=["gvkey", "year"], how="left")
    .query("at > 0 and at_lag > 0")
    .assign(investment=lambda x: x["capx"]/x["at_lag"],
            cash_flows=lambda x: x["oancf"]/x["at_lag"])                   
)

data_investment = (data_investment
    .merge(data_investment.get(["gvkey", "year", "investment"])
            .rename(columns={"investment": "investment_lead"})
            .assign(year=lambda x: x["year"]-1), 
            on=["gvkey", "year"], how="left")
)
```

Tobin’s q is the ratio of the market value of capital to its replacement costs. It is one of the most common regressors in corporate finance applications (e.g., [Fazzari et al. 1988](#ref-Fazzari1988); [Erickson and Whited 2012](#ref-Erickson2012)). We follow the implementation of Gulen and Ion ([2015](#ref-Gulen2015)) and compute Tobin’s q as the market value of equity (`mktcap`) plus the book value of assets (`at`) minus book value of equity (`be`) plus deferred taxes (`txdb`), all divided by book value of assets (`at`). Finally, we only keep observations where all variables of interest are non-missing, and the reported book value of assets is strictly positive.

``` python
data_investment = (data_investment
    .merge(crsp_monthly, on=["gvkey", "date"], how="left")
    .assign(
        tobins_q=lambda x: (
        (x["mktcap"]+x["at"]-x["be"]+x["txdb"])/x["at"]
        )
    )
    .get(["gvkey", "year", "investment_lead", "cash_flows", "tobins_q"])
    .dropna()
)
```

As the variable construction typically leads to extreme values that are most likely related to data issues (e.g., reporting errors), many papers include winsorization of the variables of interest. Winsorization involves replacing values of extreme outliers with quantiles on the respective end. The following function implements the winsorization for any percentage cut that should be applied on either end of the distributions. In the specific example, we winsorize the main variables (`investment`, `cash_flows`, and `tobins_q`) at the one percent level.[^1]

``` python
def winsorize(x, cut):
    """Winsorize returns at cut level."""
    
    tmp_x = x.copy()
    upper_quantile=np.nanquantile(tmp_x, 1 - cut)
    lower_quantile=np.nanquantile(tmp_x, cut)
    tmp_x[tmp_x > upper_quantile]=upper_quantile
    tmp_x[tmp_x < lower_quantile]=lower_quantile
    
    return tmp_x

data_investment = (data_investment
    .assign(
        investment_lead=lambda x: winsorize(x["investment_lead"], 0.01),
        cash_flows=lambda x: winsorize(x["cash_flows"], 0.01),
        tobins_q=lambda x: winsorize(x["tobins_q"], 0.01)
    )
)
```

Before proceeding to any estimations, we highly recommend tabulating summary statistics of the variables that enter the regression. These simple tables allow you to check the plausibility of your numerical variables, as well as spot any obvious errors or outliers. Additionally, for panel data, plotting the time series of the variable’s mean and the number of observations is a useful exercise to spot potential problems.

``` python
data_investment_summary = (data_investment
    .melt(id_vars=["gvkey", "year"], var_name="measure",
          value_vars=["investment_lead", "cash_flows", "tobins_q"])
    .get(["measure", "value"])
    .groupby("measure")
    .describe(percentiles=[0.05, 0.5, 0.95])
)
np.round(data_investment_summary, 2)
```

|                 | value    |      |      |       |       |      |      |       |
|-----------------|----------|------|------|-------|-------|------|------|-------|
|                 | count    | mean | std  | min   | 5%    | 50%  | 95%  | max   |
| measure         |          |      |      |       |       |      |      |       |
| cash_flows      | 133619.0 | 0.01 | 0.27 | -1.56 | -0.48 | 0.06 | 0.27 | 0.47  |
| investment_lead | 133619.0 | 0.06 | 0.08 | 0.00  | 0.00  | 0.03 | 0.20 | 0.46  |
| tobins_q        | 133619.0 | 1.99 | 1.69 | 0.56  | 0.79  | 1.39 | 5.35 | 10.80 |

## Fixed Effects

To illustrate fixed effects regressions, we use the `pyfixest` package, which is both computationally powerful and flexible with respect to model specifications. We start out with the basic investment regression using the simple model

\\ \text{Investment}\_{i,t+1} = \alpha + \beta_1\text{Cash Flows}\_{i,t}+\beta_2\text{Tobin's q}\_{i,t}+\varepsilon\_{i,t}, \tag{1}\\

where \\\varepsilon_t\\ is i.i.d. normally distributed across time and firms. We use the `PanelOLS()`-function to estimate the simple model so that the output has the same structure as the other regressions below.

``` python
model_ols = pf.feols(
    "investment_lead ~ cash_flows + tobins_q",
    vcov = "iid",
    data = data_investment
)
model_ols.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: investment_lead, Fixed effects: 0
    Inference:  iid
    Observations:  133619

    | Coefficient   |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:--------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | Intercept     |      0.042 |        0.000 |   130.092 |      0.000 |  0.041 |   0.042 |
    | cash_flows    |      0.049 |        0.001 |    64.316 |      0.000 |  0.047 |   0.050 |
    | tobins_q      |      0.007 |        0.000 |    57.707 |      0.000 |  0.007 |   0.007 |
    ---
    RMSE: 0.074 R2: 0.044 

As expected, the regression output shows significant coefficients for both variables. Higher cash flows and investment opportunities are associated with higher investment. However, the simple model actually may have a lot of omitted variables, so our coefficients are most likely biased. As there is a lot of unexplained variation in our simple model (indicated by the rather low adjusted R-squared), the bias in our coefficients is potentially severe, and the true values could be above or below zero. Note that there are no clear cutoffs to decide when an R-squared is high or low, but it depends on the context of your application and on the comparison of different models for the same data.

One way to tackle the issue of omitted variable bias is to get rid of as much unexplained variation as possible by including *fixed effects*; i.e., model parameters that are fixed for specific groups (e.g., [Wooldridge 2010](#ref-Wooldridge2010)). In essence, each group has its own mean in fixed effects regressions. The simplest group that we can form in the investment regression is the firm level. The firm fixed effects regression is then

\\ \text{Investment}\_{i,t+1} = \alpha_i + \beta_1\text{Cash Flows}\_{i,t}+\beta_2\text{Tobin's q}\_{i,t}+\varepsilon\_{i,t}, \tag{2}\\

where \\\alpha_i\\ is the firm fixed effect and captures the firm-specific mean investment across all years. In fact, you could also compute firms’ investments as deviations from the firms’ average investments and estimate the model without the fixed effects. The idea of the firm fixed effect is to remove the firm’s average investment, which might be affected by firm-specific variables that you do not observe. For example, firms in a specific industry might invest more on average. Or you observe a young firm with large investments but only small concurrent cash flows, which will only happen in a few years. This sort of variation is unwanted because it is related to unobserved variables that can bias your estimates in any direction.

To include the firm fixed effect, we use `gvkey` (Compustat’s firm identifier) as follows:

``` python
model_fe_firm = pf.feols(
    "investment_lead ~ cash_flows + tobins_q | gvkey",
    vcov = "iid",
    data = data_investment
)
model_fe_firm.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: investment_lead, Fixed effects: gvkey
    Inference:  iid
    Observations:  131834

    | Coefficient   |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:--------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | cash_flows    |      0.014 |        0.001 |    15.694 |      0.000 |  0.012 |   0.015 |
    | tobins_q      |      0.011 |        0.000 |    83.032 |      0.000 |  0.010 |   0.011 |
    ---
    RMSE: 0.049 R2: 0.577 R2 Within: 0.056 

The regression output shows a lot of unexplained variation at the firm level that is taken care of by including the firm fixed effect as the adjusted R-squared rises above 50 percent. In fact, it is more interesting to look at the within R-squared that shows the explanatory power of a firm’s cash flow and Tobin’s q *on top* of the average investment of each firm. We can also see that the coefficients changed slightly in magnitude but not in sign.

There is another source of variation that we can get rid of in our setting: average investment across firms might vary over time due to macroeconomic factors that affect all firms, such as economic crises. By including year fixed effects, we can take out the effect of unobservables that vary over time. The two-way fixed effects regression is then

\\ \text{Investment}\_{i,t+1} = \alpha_i + \alpha_t + \beta_1\text{Cash Flows}\_{i,t}+\beta_2\text{Tobin's q}\_{i,t}+\varepsilon\_{i,t}, \tag{3}\\

where \\\alpha_t\\ is the time fixed effect. Here you can think of higher investments during an economic expansion with simultaneously high cash flows. You can include a time fixed effects by using “TimeEffects” in the formula of PanelOLS.

``` python
model_fe_firmyear = pf.feols(
    "investment_lead ~ cash_flows + tobins_q | gvkey + year",
    vcov = "iid",
    data = data_investment
)
model_fe_firmyear.summary()
```

    ###

    Estimation:  OLS
    Dep. var.: investment_lead, Fixed effects: gvkey+year
    Inference:  iid
    Observations:  131834

    | Coefficient   |   Estimate |   Std. Error |   t value |   Pr(>|t|) |   2.5% |   97.5% |
    |:--------------|-----------:|-------------:|----------:|-----------:|-------:|--------:|
    | cash_flows    |      0.017 |        0.001 |    19.615 |      0.000 |  0.015 |   0.019 |
    | tobins_q      |      0.010 |        0.000 |    76.360 |      0.000 |  0.009 |   0.010 |
    ---
    RMSE: 0.048 R2: 0.599 R2 Within: 0.049 

The inclusion of time fixed effects did only marginally affect the R-squared and the coefficients, which we can interpret as a good thing as it indicates that the coefficients are not driven by an omitted variable that varies over time.

How can we further improve the robustness of our regression results? Ideally, we want to get rid of unexplained variation at the firm-year level, which means we need to include more variables that vary across firm *and* time and are likely correlated with investment. Note that we cannot include firm-year fixed effects in our setting because then cash flows and Tobin’s q are colinear with the fixed effects, and the estimation becomes void.

Before we discuss the properties of our estimation errors, we want to point out that regression tables are at the heart of every empirical analysis, where you compare multiple models. Fortunately, the `results.compare()` function provides a convenient way to tabulate the regression output (with many parameters to customize and even print the output in LaTeX). We recommend printing \\t\\-statistics rather than standard errors in regression tables because the latter are typically very hard to interpret across coefficients that vary in size. We also do not print p-values because they are sometimes misinterpreted to signal the importance of observed effects ([Wasserstein and Lazar 2016](#ref-Wasserstein2016)). The \\t\\-statistics provide a consistent way to interpret changes in estimation uncertainty across different model specifications.

``` python
pf.etable([model_ols, model_fe_firm, model_fe_firmyear], coef_fmt = "b (t)")
```

|  | investment_lead |  |  |
|----|----|----|----|
|  | \(1\) | \(2\) | \(3\) |
| coef |  |  |  |
| cash_flows | 0.049\*\*\* (64.316) | 0.014\*\*\* (15.694) | 0.017\*\*\* (19.615) |
| tobins_q | 0.007\*\*\* (57.707) | 0.011\*\*\* (83.032) | 0.010\*\*\* (76.360) |
| Intercept | 0.042\*\*\* (130.092) |  |  |
| fe |  |  |  |
| year | \- | \- | x |
| gvkey | \- | x | x |
| stats |  |  |  |
| Observations | 133619 | 131834 | 131834 |
| S.E. type | iid | iid | iid |
| R² | 0.044 | 0.577 | 0.599 |
| R² Within | \- | 0.056 | 0.049 |
| Significance levels: \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001. Format of coefficient cell: Coefficient (t-stats) |  |  |  |

## Clustering Standard Errors

Apart from biased estimators, we usually have to deal with potentially complex dependencies of our residuals with each other. Such dependencies in the residuals invalidate the i.i.d. assumption of OLS and lead to biased standard errors. With biased OLS standard errors, we cannot reliably interpret the statistical significance of our estimated coefficients.

In our setting, the residuals may be correlated across years for a given firm (time-series dependence), or, alternatively, the residuals may be correlated across different firms (cross-section dependence). One of the most common approaches to dealing with such dependence is the use of *clustered standard errors* ([Petersen 2008](#ref-Petersen2008)). The idea behind clustering is that the correlation of residuals *within* a cluster can be of any form. As the number of clusters grows, the cluster-robust standard errors become consistent ([Donald and Lang 2007](#ref-Lang2007); [Wooldridge 2010](#ref-Wooldridge2010)). A natural requirement for clustering standard errors in practice is hence a sufficiently large number of clusters. Typically, around at least 30 to 50 clusters are seen as sufficient ([Cameron et al. 2011](#ref-Cameron2011)).

Instead of relying on the i.i.d. assumption, we can use the `cov_type="clustered"` option in the `fit()`-function as above. The code chunk below applies both one-way clustering by firm as well as two-way clustering by firm and year.

``` python
model_cluster_firm = pf.feols(
    "investment_lead ~ cash_flows + tobins_q | gvkey + year",
    vcov = {"CRV1": "gvkey"},
    data = data_investment
)

model_cluster_firmyear = pf.feols(
    "investment_lead ~ cash_flows + tobins_q | gvkey + year",
    vcov = {"CRV1": "gvkey + year"},
    data = data_investment
)
```

The table below shows the comparison of the different assumptions behind the standard errors. In the first column, we can see highly significant coefficients on both cash flows and Tobin’s q. By clustering the standard errors on the firm level, the \\t\\-statistics of both coefficients drop in half, indicating a high correlation of residuals within firms. If we additionally cluster by year, we see a drop, particularly for Tobin’s q, again. Even after relaxing the assumptions behind our standard errors, both coefficients are still comfortably significant as the \\t\\-statistics are well above the usual critical values of 1.96 or 2.576 for two-tailed significance tests.

``` python
pf.etable([model_fe_firmyear, model_cluster_firm, model_cluster_firmyear], coef_fmt = "b (t)")
```

|  | investment_lead |  |  |
|----|----|----|----|
|  | \(1\) | \(2\) | \(3\) |
| coef |  |  |  |
| cash_flows | 0.017\*\*\* (19.615) | 0.017\*\*\* (11.388) | 0.017\*\*\* (9.394) |
| tobins_q | 0.010\*\*\* (76.360) | 0.010\*\*\* (35.762) | 0.010\*\*\* (14.965) |
| fe |  |  |  |
| year | x | x | x |
| gvkey | x | x | x |
| stats |  |  |  |
| Observations | 131834 | 131834 | 131834 |
| S.E. type | iid | by: gvkey | by: gvkey+year |
| R² | 0.599 | 0.599 | 0.599 |
| R² Within | 0.049 | 0.049 | 0.049 |
| Significance levels: \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001. Format of coefficient cell: Coefficient (t-stats) |  |  |  |

Inspired by Abadie et al. ([2017](#ref-AbadieEtAl2017)), we want to close this chapter by highlighting that choosing the right dimensions for clustering is a design problem. Even if the data is informative about whether clustering matters for standard errors, they do not tell you whether you should adjust the standard errors for clustering. Clustering at too aggregate levels can hence lead to unnecessarily inflated standard errors.

## Key Takeaways

- Fixed effects regressions control for unobserved firm and time-specific factors, reducing omitted variable bias in panel data models.
- The `pyfixest` Python package streamlines the estimation of fixed effects and supports clustering standard errors for robust inference.
- Clustered standard errors adjust for residual dependence across firms or years, leading to more accurate \\t\\-statistics and confidence in significance tests.
- Two-way clustering by firm and year is commonly used in finance to address both time-series and cross-sectional correlation in residuals.
- Careful model specification, including winsorization and proper clustering choices, enhances the credibility and reliability of empirical finance results.

## Exercises

1.  Estimate the two-way fixed effects model with two-way clustered standard errors using quarterly Compustat data from WRDS.
2.  Following Peters and Taylor ([2017](#ref-Peters2017)), compute Tobin’s q as the market value of outstanding equity `mktcap` plus the book value of debt (`dltt` + `dlc`) minus the current assets `atc` and everything divided by the book value of property, plant and equipment `ppegt`. What is the correlation between the measures of Tobin’s q? What is the impact on the two-way fixed effects regressions?

## References

Abadie, Alberto, Susan Athey, Guido W Imbens, and Jeffrey Wooldridge. 2017. “When should you adjust standard errors for clustering?” *Working Paper*. <http://www.nber.org/papers/w24003>.

Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. 2011. “Robust inference with multiway clustering.” *Journal of Business & Economic Statistics* 29 (2): 238–49. <http://www.jstor.org/stable/25800796>.

Donald, Stephen, and Kevin Lang. 2007. “Inference with difference-in-differences and other panel data.” *The Review of Economics and Statistics* 89 (2): 221–33. <https://doi.org/10.1162/rest.89.2.221>.

Erickson, Timothy, and Toni M. Whited. 2012. “Treating measurement error in Tobin’s q.” *Review of Financial Studies* 25 (4): 1286–329. <https://doi.org/10.1093/rfs/hhr120>.

Fazzari, Steven M., R. Glenn Hubbard, Bruce C. Petersen, Alan S. Blinder, and James M. Poterba. 1988. “Financing constraints and corporate investment.” *Brookings Papers on Economic Activity* 1988 (1): 141–206. <http://www.jstor.org/stable/2534426>.

Fischer, Alexander. 2024. *PyFixest: Fast High-Dimensional Fixed Effects Regression in Python*. [Https://pypi.org/project/pyfixest/](https://pypi.org/project/pyfixest/).

Gulen, Huseyin, and Mihai Ion. 2015. “Policy uncertainty and corporate investment.” *Review of Financial Studies* 29 (3): 523–64. <https://doi.org/10.1093/rfs/hhv050>.

Peters, Ryan H., and Lucian A. Taylor. 2017. “Intangible capital and the investment-q relation.” *Journal of Financial Economics* 123 (2): 251–72. <https://doi.org/10.1016/j.jfineco.2016.03.011>.

Petersen, Mitchell A. 2008. “Estimating standard errors in finance panel data sets: Comparing approaches.” *Review of Financial Studies* 22 (1): 435–80. <https://doi.org/10.1093/rfs/hhn053>.

Wasserstein, Ronald L., and Nicole A. Lazar. 2016. “The ASA Statement on p-Values: Context, process, and purpose.” *The American Statistician* 70 (2): 129–33. <https://doi.org/10.1080/00031305.2016.1154108>.

Wooldridge, Jefrey M. 2010. *Econometric analysis of cross section and panel data*. The MIT Press. <http://www.jstor.org/stable/j.ctt5hhcfr>.

## Footnotes

[^1]: Note that in `pandas`, when you index a dataframee, you receive a reference to the original dataframee. Consequently, modifying a subset will directly impact the initial dataframee. To prevent unintended changes to the original dataframee, it is advisable to use the `copy()` method as we do here in the `winsorize` function.
