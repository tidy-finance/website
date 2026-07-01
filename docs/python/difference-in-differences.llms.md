# Difference in Differences

> **NOTE:**
>
> You are reading **Tidy Finance with Python**. You can find the equivalent chapter for the sibling **Tidy Finance with R** [here](../r/difference-in-differences.llms.md).

In this chapter, we illustrate the concept of *difference in differences* (DiD) estimators by evaluating the effects of climate change regulation on the pricing of bonds across firms. DiD estimators are typically used to recover the treatment effects of natural or quasi-natural experiments that trigger sharp changes in the environment of a specific group. Instead of looking at differences in just one group (e.g., the effect in the treated group), DiD investigates the treatment effects by looking at the difference between differences in two groups. Such experiments are usually exploited to address endogeneity concerns (e.g., [Roberts and Whited 2013](#ref-RobertsWhited2013)). The identifying assumption is that the outcome variable would change equally in both groups without the treatment. This assumption is also often referred to as the assumption of parallel trends. Moreover, we would ideally also want a random assignment to the treatment and control groups. Due to lobbying or other activities, this randomness is often violated in (financial) economics.

In the context of our setting, we investigate the impact of the Paris Agreement (PA), signed on December 12, 2015, on the bond yields of polluting firms. We first estimate the treatment effect of the agreement using panel regression techniques that we discuss in [Fixed Effects and Clustered Standard Errors](../python/fixed-effects-and-clustered-standard-errors.llms.md). We then present two methods to illustrate the treatment effect over time graphically. Although we demonstrate that the treatment effect of the agreement is anticipated by bond market participants well in advance, the techniques we present below can also be applied to many other settings.

The approach we use here replicates the results of Seltzer et al. ([2022](#ref-Seltzer2022)) partly. Specifically, we borrow their industry definitions for grouping firms into green and brown types. Overall, the literature on environmental, social, and governance (ESG) effects in corporate bond markets is already large but continues to grow (for recent examples, see, e.g., Halling et al. ([2021](#ref-Halling2021)), Handler et al. ([2022](#ref-Handler2022)), Huynh and Xia ([2021](#ref-Huynh2021)), among many others).

The current chapter relies on this set of Python packages.

``` python
import pandas as pd
import numpy as np
import pyfixest as pf
import pyarrow.dataset as ds

from plotnine import *
from scipy.stats import norm
```

Compared to previous chapters, we introduce the `scipy.stats` module from the `scipy` ([Virtanen et al. 2020](#ref-scipy)) for simple retrieval of quantiles of the standard normal distribution.

## Data Preparation

We use TRACE and Mergent FISD as data sources from our Parquet files introduced in [Accessing and Managing Financial Data](../python/accessing-and-managing-financial-data.llms.md) and [TRACE and FISD](../python/trace-and-fisd.llms.md).

``` python
fisd = (pd.read_parquet("data-python/fisd.parquet")
    .dropna()
)

trace_enhanced = (ds.dataset("data-r/trace_enhanced")
    .to_table(columns=["cusip_id", "trd_exctn_dt", "rptd_pr", "entrd_vol_qt", "yld_pt"])
    .to_pandas()
    .dropna()
)
```

We start our analysis by preparing the sample of bonds. We only consider bonds with a time to maturity of more than one year to the signing of the PA, so that we have sufficient data to analyze the yield behavior after the treatment date. This restriction also excludes all bonds issued after the agreement. We also consider only the first two digits of the SIC industry code to identify the polluting industries (in line with [Seltzer et al. 2022](#ref-Seltzer2022)).

``` python
treatment_date = pd.to_datetime("2015-12-12")
polluting_industries = [
    49, 13, 45, 29, 28, 33, 40, 20, 26, 42, 10, 53, 32, 99, 37
]

bonds = (fisd
    .query("offering_amt > 0 & sic_code != 'None'")
    .assign(
        time_to_maturity=lambda x: (x["maturity"]-treatment_date).dt.days / 365,
        sic_code=lambda x: x["sic_code"].astype(str).str[:2].astype(int),
        log_offering_amt=lambda x: np.log(x["offering_amt"])
    )
    .query("time_to_maturity >= 1")
    .rename(columns={"complete_cusip": "cusip_id"})
    .get(["cusip_id", "time_to_maturity", "log_offering_amt", "sic_code"])
    .assign(polluter=lambda x: x["sic_code"].isin(polluting_industries))
    .reset_index(drop=True)
)
```

Next, we aggregate the individual transactions as reported in TRACE to a monthly panel of bond yields. We consider bond yields for a bond’s last trading day in a month. Therefore, we first aggregate bond data to daily frequency and apply common restrictions from the literature (see, e.g., [Bessembinder et al. 2008](#ref-Bessembinder2008)). We weigh each transaction by volume to reflect a trade’s relative importance and avoid emphasizing small trades. Moreover, we only consider transactions with reported prices `rptd_pr` larger than 25 (to exclude bonds that are close to default) and only bond-day observations with more than five trades on a corresponding day (to exclude prices based on too few, potentially non-representative transactions).

``` python
trace_enhanced = (trace_enhanced
    .query("rptd_pr > 25")
    .assign(weight=lambda x: x["entrd_vol_qt"]*x["rptd_pr"])
    .assign(weighted_yield=lambda x: x["weight"]*x["yld_pt"])
)

trace_aggregated = (trace_enhanced
    .groupby(["cusip_id", "trd_exctn_dt"])
    .aggregate(
        weighted_yield_sum=("weighted_yield", "sum"),
        weight_sum=("weight", "sum"),
        trades=("rptd_pr", "count")
    )
    .reset_index()
    .assign(avg_yield=lambda x: x["weighted_yield_sum"]/x["weight_sum"])
    .dropna(subset=["avg_yield"])
    .query("trades >= 5")
    .assign(trd_exctn_dt=lambda x: pd.to_datetime(x["trd_exctn_dt"]))
    .assign(date=lambda x: x["trd_exctn_dt"]-pd.offsets.MonthBegin())
)

date_index = (trace_aggregated
    .groupby(["cusip_id", "date"])["trd_exctn_dt"]
    .idxmax()
)

trace_aggregated = (trace_aggregated
    .loc[date_index]
    .get(["cusip_id", "date", "avg_yield"])
)
```

By combining the bond-specific information from Mergent FISD for our bond sample with the aggregated TRACE data, we arrive at the main sample for our analysis.

``` python
bonds_panel = (bonds
    .merge(trace_aggregated, how="inner", on="cusip_id")
    .dropna()
)
```

Before we can run the first regression, we need to define the `treated` indicator,[^1] which is the product of the `post_period` (i.e., all months after the signing of the PA) and the `polluter` indicator defined above.

``` python
bonds_panel = (bonds_panel
    .assign(
        post_period=lambda x: (
        x["date"] >= (treatment_date-pd.offsets.MonthBegin())
        )
    )
    .assign(treated=lambda x: x["polluter"] & x["post_period"])
    .assign(date_cat=lambda x: pd.Categorical(x["date"], ordered=True))
)
```

As usual, we tabulate summary statistics of the variables that enter the regression to check the validity of our variable definitions.

``` python
bonds_panel_summary = (bonds_panel
    .melt(var_name="measure",
            value_vars=["avg_yield", "time_to_maturity", "log_offering_amt"])
    .groupby("measure")
    .describe(percentiles=[0.05, 0.5, 0.95])
)
np.round(bonds_panel_summary, 2)
```

|                  | value    |       |      |      |       |       |       |        |
|------------------|----------|-------|------|------|-------|-------|-------|--------|
|                  | count    | mean  | std  | min  | 5%    | 50%   | 95%   | max    |
| measure          |          |       |      |      |       |       |       |        |
| avg_yield        | 127540.0 | 4.08  | 4.21 | 0.06 | 1.27  | 3.38  | 8.11  | 127.97 |
| log_offering_amt | 127540.0 | 13.27 | 0.82 | 4.64 | 12.21 | 13.22 | 14.51 | 16.52  |
| time_to_maturity | 127540.0 | 8.55  | 8.42 | 1.01 | 1.50  | 5.81  | 27.41 | 100.70 |

## Panel Regressions

The PA is a legally binding international treaty on climate change. It was adopted by 196 parties at COP 21 in Paris on December 12, 2015 and entered into force on November 4, 2016. The PA obliges developed countries to support efforts to build clean, climate-resilient futures. One may thus hypothesize that adopting climate-related policies may affect financial markets. To measure the magnitude of this effect, we first run an ordinary least square (OLS) regression without fixed effects where we include the `treated`, `post_period`, and `polluter` dummies, as well as the bond-specific characteristics `log_offering_amt` and `time_to_maturity`. This simple model assumes that there are essentially two periods (before and after the PA) and two groups (polluters and non-polluters). Nonetheless, it should indicate whether polluters have higher yields following the PA compared to non-polluters.

The second model follows the typical DiD regression approach by including individual (`cusip_id`) and time (`date`) fixed effects. In this model, we do not include any other variables from the simple model because the fixed effects subsume them, and we observe the coefficient of our main variable of interest: `treated`.

``` python
model_without_fe = pf.feols(
    "avg_yield ~ treated + post_period + polluter + log_offering_amt + time_to_maturity",
    vcov = "iid",
    data = bonds_panel
)

model_with_fe = pf.feols(
    "avg_yield ~ treated | cusip_id + date",
    vcov = "iid",
    data = bonds_panel
)

pf.etable([model_without_fe, model_with_fe], coef_fmt = "b (t)")
```

|  | avg_yield |  |
|----|----|----|
|  | \(1\) | \(2\) |
| coef |  |  |
| treated | 0.453\*\*\* (9.133) | 0.975\*\*\* (29.294) |
| post_period | -0.177\*\*\* (-6.036) |  |
| polluter | 0.486\*\*\* (15.426) |  |
| log_offering_amt | -0.550\*\*\* (-38.994) |  |
| time_to_maturity | 0.058\*\*\* (41.527) |  |
| Intercept | 10.733\*\*\* (57.060) |  |
| fe |  |  |
| cusip_id | \- | x |
| date | \- | x |
| stats |  |  |
| Observations | 127540 | 127195 |
| S.E. type | iid | iid |
| R² | 0.032 | 0.648 |
| R² Within | \- | 0.007 |
| Significance levels: \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001. Format of coefficient cell: Coefficient (t-stats) |  |  |

Both models indicate that polluters have significantly higher yields after the PA than non-polluting firms. Note that the magnitude of the `treated` coefficient varies considerably across models.

## Visualizing Parallel Trends

Even though the regressions above indicate that there is an impact of the PA on bond yields of polluters, the tables do not tell us anything about the dynamics of the treatment effect. In particular, the models provide no indication about whether the crucial *parallel trends* assumption is valid. This assumption requires that in the absence of treatment, the difference between the two groups is constant over time. Although there is no well-defined statistical test for this assumption, visual inspection typically provides a good indication.

To provide such visual evidence, we revisit the simple OLS model and replace the `treated` and `post_period` indicators with month dummies for each group. This approach estimates the average yield change of both groups for each period and provides corresponding confidence intervals. Plotting the coefficient estimates for both groups around the treatment date shows us the dynamics of our panel data.

``` python
model_without_fe_time = pf.feols(
    "avg_yield ~ polluter + date*polluter + time_to_maturity + log_offering_amt",
    vcov = "iid",
    data = bonds_panel.assign(date = lambda x: x["date"].astype(str))
)

model_without_fe_coefs = (pd.DataFrame({
        "estimate": model_without_fe_time.coef(),
        "std_error": model_without_fe_time.se()
    })
    .reset_index(names="term")
    .query("term.str.contains('date')")
    .assign(
        treatment=lambda x: x["term"].str.contains(":polluter"),
        date=lambda x: x["term"].str.extract(r"date\[T\.(\d{4}-\d{2}-\d{2})\]"),
        ci_up=lambda x: x["estimate"]+norm.ppf(0.975)*x["std_error"],
        ci_low=lambda x: x["estimate"]+norm.ppf(0.025)*x["std_error"]
    )
)

model_without_fe_figure = (
    ggplot(
        model_without_fe_coefs, 
        aes(x="date", y="estimate", color="treatment",
            linetype="treatment", shape="treatment")
    )
    + geom_vline(xintercept=pd.to_datetime(treatment_date) -
                pd.offsets.MonthBegin(), linetype="dashed")
    + geom_hline(yintercept=0, linetype="dashed")
    + geom_errorbar(aes(ymin="ci_low", ymax="ci_up"), alpha=0.5)
    + geom_point()
    + guides(linetype="none")
    + labs(
        x="", y="Yield", shape="Polluter?", color="Polluter?",
        title="Polluters respond stronger than green firms"
        )
    + scale_linetype_manual(values=["solid", "dashed"])
    + scale_x_datetime(date_breaks="1 year", date_labels="%Y")
)
model_without_fe_figure.show()
```

[![Title: Polluters respond stronger to Paris Agreement than green firms. The figure shows a sequence of monthly dots for two groups. Before the agreement, the dots mainly overlap. Ahead of the agreement, yields start to increase. Then, after the agreement, there is a strong divergence in yields. Polluters have significantly higher yields than non-polluters in the months before and after the signing of the Paris Agreement. However, this yield difference vanishes again towards the end of 2016.](difference-in-differences_files/figure-html/fig-1301-output-1.png)](difference-in-differences_files/figure-html/fig-1301-output-1.png "Figure 1: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters and non-polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).")

Figure 1: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters and non-polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).

[Figure 1](#fig-1301) shows that throughout most of 2014, the yields of the two groups changed in unison. However, starting at the end of 2014, the yields start to diverge, reaching the highest difference around the signing of the PA. Afterward, the yields for both groups fall again, and the polluters arrive at the same level as at the beginning of 2014. The non-polluters, on the other hand, even experience significantly lower yields than polluters after the signing of the agreement.

Instead of plotting both groups using the simple model approach, we can also use the fixed-effects model and focus on the polluter’s yield response to the signing relative to the non-polluters. To perform this estimation, we need to replace the `treated` indicator with separate time dummies for the polluters, each marking a one-month period relative to the treatment date.

``` python
bonds_panel_alt = (bonds_panel
    .assign(
        diff_to_treatment=lambda x: (
        np.round(
            ((x["date"]-(treatment_date- 
                pd.offsets.MonthBegin())).dt.days/365)*12, 0
        ).astype(int)
        )
    )
)

variables = (bonds_panel_alt
    .get(["diff_to_treatment", "date"])
    .drop_duplicates()
    .sort_values("date")
    .copy()
    .assign(variable_name=np.nan)
    .reset_index(drop=True)
)
```

In the next code chunk, we assemble the model formula and regress the monthly yields on the set of time dummies and `cusip_id` and `date` fixed effects.

``` python
formula = "avg_yield ~ 1 + "

for j in range(variables.shape[0]):
    if variables["diff_to_treatment"].iloc[j] != 0:
        old_names=list(bonds_panel_alt.columns)
        
        bonds_panel_alt["new_var"] = (
          bonds_panel_alt["diff_to_treatment"] == 
            variables["diff_to_treatment"].iloc[j]
        ) & bonds_panel_alt["polluter"]
        
        diff_to_treatment_value=variables["diff_to_treatment"].iloc[j]
        direction="lag" if diff_to_treatment_value < 0 else "lead"
        abs_diff_to_treatment=int(abs(diff_to_treatment_value))
        new_var_name=f"{direction}{abs_diff_to_treatment}"
        variables.at[j, "variable_name"]=new_var_name
        bonds_panel_alt[new_var_name]=bonds_panel_alt["new_var"]
        formula += (f" + {new_var_name}" if j > 0 else new_var_name)

formula = formula + " | cusip_id + date "

model_with_fe_time = pf.feols(
    formula,
    vcov = "iid",
    data = bonds_panel_alt
)
```

We then collect the regression results into a dataframe that contains the estimates and corresponding 95 percent confidence intervals. Note that we also add a row with zeros for the (omitted) reference point of the time dummies.

``` python
lag0_row = pd.DataFrame({
    "term": ["lag0"],
    "estimate": [0],
    "std_error": [0],
    "ci_up": [0],
    "ci_low": [0],
    "diff_to_treatment": [0],
    "date": [treatment_date - pd.offsets.MonthBegin()]
})

model_with_fe_time_coefs = (pd.DataFrame({
        "estimate": model_with_fe_time.coef(),
        "std_error": model_with_fe_time.se()
    })
    .reset_index(names="term")
    .assign(
        ci_up=lambda x: x["estimate"]+norm.ppf(0.975)*x["std_error"],
        ci_low=lambda x: x["estimate"]+norm.ppf(0.025)*x["std_error"]
    )
    .merge(variables, how="left", left_on="term", right_on="variable_name")
    .drop(columns="variable_name")
)

model_with_fe_time_coefs = pd.concat(
    [model_with_fe_time_coefs, lag0_row], 
    ignore_index=True
)
```

[Figure 2](#fig-1402) shows the resulting coefficient estimates.

``` python
model_with_fe_time_figure = (
    ggplot(
        model_with_fe_time_coefs,
        aes(x="date", y="estimate")
    )
    + geom_vline(aes(xintercept=treatment_date - pd.offsets.MonthBegin()), 
                    linetype="dashed")
    + geom_hline(aes(yintercept=0), linetype="dashed")
    + geom_errorbar(aes(ymin="ci_low", ymax="ci_up"), alpha=0.5)
    + geom_point(aes(y="estimate"))
    + labs(
        x="", y="Yield",
        title="Polluters' yield patterns around Paris Agreement signing"
        )
    + scale_x_datetime(date_breaks="1 year", date_labels="%Y")
)
model_with_fe_time_figure.show()
```

[![Title: Polluters' yield patterns around Paris Agreement signing. The figure shows a sequence of monthly dots for the treated group. Ahead of the agreement, yields of polluters start to increase. Then, after the agreement, there is a small reversal and yields drop again.](difference-in-differences_files/figure-html/fig-1402-output-1.png)](difference-in-differences_files/figure-html/fig-1402-output-1.png "Figure 2: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).")

Figure 2: The figure shows the coefficient estimates and 95 percent confidence intervals for OLS regressions estimating the treatment effect of the Paris Agreement on bond yields (in percent) for polluters. The horizontal line represents the benchmark yield of polluters before the Paris Agreement. The vertical line indicates the date of the agreement (December 12, 2015).

The resulting graph shown in [Figure 2](#fig-1402) confirms the main conclusion of the previous image: polluters’ yield patterns show a considerable anticipation effect starting toward the end of 2014. Yields only marginally increase after the signing of the agreement. However, as opposed to the simple model, we do not see a complete reversal back to the pre-agreement level. Yields of polluters stay at a significantly higher level even one year after the signing.

Notice that during the year after the PA was signed, the 45th president of the United States was elected (on November 8, 2016). During his campaign there were some indications of intentions to withdraw the US from the PA, which ultimately happened on November 4, 2020. Hence, reversal effects are potentially driven by these actions.

## Key Takeaways

- Difference-in-differences is a powerful tool for estimating causal effects in financial settings, especially when analyzing the impact of policy changes or shocks.
- It is important to assess the parallel trends assumption using graphical methods.
- The `pyfixest` Python package allows you to implement difference-in-differences regressions and visualize parallel trends.
- By combining panel data from TRACE and Mergent FISD with fixed effects regressions, you can evaluate how the Paris Agreement influenced corporate bond yields.
- The application shows that polluting firms experienced significantly higher yields following up to and after the agreement, invalidating the parallel trends assumption.

## Exercises

1.  The 46th president of the US rejoined the Paris Agreement on February 19, 2021. Repeat the difference in differences analysis for the day of his election victory. Note that you will also have to download new TRACE data. How did polluters’ yields react to this action?
2.  Based on the exercise on ratings in [TRACE and FISD](../python/trace-and-fisd.llms.md), include ratings as a control variable in the analysis above. Do the results change?

## References

Bessembinder, Hendrik, Kathleen M Kahle, William F Maxwell, and Danielle Xu. 2008. “Measuring abnormal bond performance.” *Review of Financial Studies* 22 (10): 4219–58. <https://doi.org/10.1093/rfs/hhn105>.

Halling, Michael, Jin Yu, and Josef Zechner. 2021. “Primary Corporate Bond Markets and Social Responsibility.” *Working Paper*. <https://dx.doi.org/10.2139/ssrn.3681666>.

Handler, Lukas, Rainer Jankowitsch, and Alexander Pasler. 2022. “The Effects of ESG Performance and Preferences on US Corporate Bond Prices.” *Working Paper*. <https://dx.doi.org/10.2139/ssrn.4099566>.

Huynh, Thanh D., and Ying Xia. 2021. “Climate Change News Risk and Corporate Bond Returns.” *Journal of Financial and Quantitative Analysis* 56 (6): 1985–2009. <https://doi.org/10.1017/S0022109020000757>.

Roberts, Michael R., and Toni M. Whited. 2013. “Endogeneity in Empirical Corporate Finance.” In *Handbook of the Economics of Finance*, vol. 2. Elsevier. <https://EconPapers.repec.org/RePEc:eee:finchp:2-a-493-572>.

Seltzer, Lee H., Laura Starks, and Qifei Zhu. 2022. “Climate Regulatory Risk and Corporate Bonds.” *Working Paper*. <https://www.nber.org/papers/w29994>.

Virtanen, Pauli, Ralf Gommers, Travis E. Oliphant, et al. 2020. “SciPy 1.0: Fundamental Algorithms for Scientific Computing in Python.” *Nature Methods* 17: 261–72. <https://doi.org/10.1038/s41592-019-0686-2>.

## Footnotes

[^1]: Note that by using a generic name here, everybody can replace ours with their sample data and run the code to produce standard regression tables and illustrations.
