# Fixed Effects and Clustered Standard Errors

> **NOTE:**
>
> You are reading **Tidy Finance with R**. You can find the equivalent chapter for the sibling **Tidy Finance with Python** [here](../python/fixed-effects-and-clustered-standard-errors.llms.md).

In this chapter, we provide an intuitive introduction to the two popular concepts of *fixed effects regressions* and *clustered standard errors*. When working with regressions in empirical finance, you will sooner or later be confronted with discussions around how you deal with omitted variables bias and dependence in your residuals. The concepts we introduce in this chapter are designed to address such concerns.

We focus on a classical panel regression common to the corporate finance literature (e.g., [Fazzari et al. 1988](#ref-Fazzari1988); [Erickson and Whited 2012](#ref-Erickson2012); [Gulen and Ion 2015](#ref-Gulen2015)): firm investment modeled as a function that increases in firm cash flow and firm investment opportunities.

Typically, this investment regression uses quarterly balance sheet data provided via Compustat because it allows for richer dynamics in the regressors and more opportunities to construct variables. As we focus on the implementation of fixed effects and clustered standard errors, we use the annual Compustat data from our previous chapters and leave the estimation using quarterly data as an exercise. We demonstrate below that the regression based on annual data yields qualitatively similar results to estimations based on quarterly data from the literature, namely confirming the positive relationships between investment and the two regressors.

The current chapter relies on the following set of R packages.

``` r
library(tidyverse)
library(arrow)
library(fixest)
```

Compared to previous chapters, we introduce `fixest` ([Bergé 2018](#ref-fixest)) for the fixed effects regressions, the implementation of standard error clusters, and tidy estimation output.

## Data Preparation

We use CRSP and annual Compustat as data sources from our Parquet files introduced in [Accessing and Managing Financial Data](../r/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../r/wrds-crsp-and-compustat.llms.md). In particular, Compustat provides balance sheet and income statement data on a firm level, while CRSP provides market valuations.

``` r
crsp_monthly <- read_parquet("data-r/crsp_monthly.parquet") |>
  select(gvkey, date, mktcap)

compustat_annual <- read_parquet("data-r/compustat_annual.parquet") |>
  select(datadate, gvkey, year, at, be, capx, oancf, txdb)
```

The classical investment regressions model the capital investment of a firm as a function of operating cash flows and Tobin’s q, a measure of a firm’s investment opportunities. We start by constructing investment and cash flows which are usually normalized by lagged total assets of a firm. In the following code chunk, we construct a *panel* of firm-year observations, so we have both cross-sectional information on firms as well as time-series information for each firm.

``` r
data_investment <- compustat_annual |>
  mutate(date = floor_date(datadate, "month")) |>
  left_join(
    compustat_annual |>
      select(gvkey, year, at_lag = at) |>
      mutate(year = year + 1),
    join_by(gvkey, year)
  ) |>
  filter(at > 0, at_lag > 0) |>
  mutate(
    investment = capx / at_lag,
    cash_flows = oancf / at_lag
  )

data_investment <- data_investment |>
  left_join(
    data_investment |>
      select(gvkey, year, investment_lead = investment) |>
      mutate(year = year - 1),
    join_by(gvkey, year)
  )
```

Tobin’s q is the ratio of the market value of capital to its replacement costs. It is one of the most common regressors in corporate finance applications (e.g., [Fazzari et al. 1988](#ref-Fazzari1988); [Erickson and Whited 2012](#ref-Erickson2012)). We follow the implementation of Gulen and Ion ([2015](#ref-Gulen2015)) and compute Tobin’s q as the market value of equity (`mktcap`) plus the book value of assets (`at`) minus book value of equity (`be`) plus deferred taxes (`txdb`), all divided by book value of assets (`at`). Finally, we only keep observations where all variables of interest are non-missing, and the reported book value of assets is strictly positive.

``` r
data_investment <- data_investment |>
  left_join(crsp_monthly, join_by(gvkey, date)) |>
  mutate(tobins_q = (mktcap + at - be + txdb) / at) |>
  select(gvkey, year, investment_lead, cash_flows, tobins_q) |>
  drop_na()
```

As the variable construction typically leads to extreme values that are most likely related to data issues (e.g., reporting errors), many papers include winsorization of the variables of interest. Winsorization involves replacing values of extreme outliers with quantiles on the respective end. The following function implements the winsorization for any percentage cut that should be applied on either end of the distributions. In the specific example, we winsorize the main variables (`investment`, `cash_flows`, and `tobins_q`) at the one percent level.

``` r
winsorize <- function(x, cut) {
  x <- replace(
    x,
    x > quantile(x, 1 - cut, na.rm = T),
    quantile(x, 1 - cut, na.rm = T)
  )
  x <- replace(
    x,
    x < quantile(x, cut, na.rm = T),
    quantile(x, cut, na.rm = T)
  )
  return(x)
}

data_investment <- data_investment |>
  mutate(across(
    c(investment_lead, cash_flows, tobins_q),
    ~ winsorize(., 0.01)
  ))
```

Before proceeding to any estimations, we highly recommend tabulating summary statistics of the variables that enter the regression. These simple tables allow you to check the plausibility of your numerical variables, as well as spot any obvious errors or outliers. Additionally, for panel data, plotting the time series of the variable’s mean and the number of observations is a useful exercise to spot potential problems.

``` r
data_investment |>
  pivot_longer(
    cols = c(investment_lead, cash_flows, tobins_q),
    names_to = "measure"
  ) |>
  group_by(measure) |>
  summarize(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    q05 = quantile(value, 0.05),
    q50 = quantile(value, 0.50),
    q95 = quantile(value, 0.95),
    max = max(value),
    n = n(),
    .groups = "drop"
  )
```

    # A tibble: 3 × 9
      measure      mean     sd    min      q05    q50   q95    max      n
      <chr>       <dbl>  <dbl>  <dbl>    <dbl>  <dbl> <dbl>  <dbl>  <int>
    1 cash_flo… 0.00857 0.275  -1.56  -4.80e-1 0.0625 0.271  0.475 133619
    2 investme… 0.0563  0.0760  0      5.66e-4 0.0317 0.202  0.457 133619
    3 tobins_q  1.99    1.69    0.563  7.90e-1 1.39   5.35  10.8   133619

## Fixed Effects

To illustrate fixed effects regressions, we use the `fixest` package, which is both computationally powerful and flexible with respect to model specifications. We start out with the basic investment regression using the simple model \\ \text{Investment}\_{i,t+1} = \alpha + \beta_1\text{Cash Flows}\_{i,t}+\beta_2\text{Tobin's q}\_{i,t}+\varepsilon\_{i,t},\\ where \\\varepsilon_t\\ is i.i.d. normally distributed across time and firms. We use the `feols()`-function to estimate the simple model so that the output has the same structure as the other regressions below, but you could also use `lm()`.

``` r
model_ols <- feols(
  fml = investment_lead ~ cash_flows + tobins_q,
  vcov = "iid",
  data = data_investment
)
model_ols
```

    OLS estimation, Dep. Var.: investment_lead
    Observations: 133,619
    Standard-errors: IID 
                Estimate Std. Error t value  Pr(>|t|)    
    (Intercept)  0.04170   0.000321   130.1 < 2.2e-16 ***
    cash_flows   0.04888   0.000760    64.3 < 2.2e-16 ***
    tobins_q     0.00715   0.000124    57.7 < 2.2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    RMSE: 0.074324   Adj. R2: 0.043674

As expected, the regression output shows significant coefficients for both variables. Higher cash flows and investment opportunities are associated with higher investment. However, the simple model actually may have a lot of omitted variables, so our coefficients are most likely biased. As there is a lot of unexplained variation in our simple model (indicated by the rather low adjusted R-squared), the bias in our coefficients is potentially severe, and the true values could be above or below zero. Note that there are no clear cutoffs to decide when an R-squared is high or low, but it depends on the context of your application and on the comparison of different models for the same data.

One way to tackle the issue of omitted variable bias is to get rid of as much unexplained variation as possible by including *fixed effects*; i.e., model parameters that are fixed for specific groups (e.g., [Wooldridge 2010](#ref-Wooldridge2010)). In essence, each group has its own mean in fixed effects regressions. The simplest group that we can form in the investment regression is the firm level. The firm fixed effects regression is then

\\ \text{Investment}\_{i,t+1} = \alpha_i + \beta_1\text{Cash Flows}\_{i,t}+\beta_2\text{Tobin's q}\_{i,t}+\varepsilon\_{i,t}, \\

where \\\alpha_i\\ is the firm fixed effect and captures the firm-specific mean investment across all years. In fact, you could also compute firms’ investments as deviations from the firms’ average investments and estimate the model without the fixed effects. The idea of the firm fixed effect is to remove the firm’s average investment, which might be affected by firm-specific variables that you do not observe. For example, firms in a specific industry might invest more on average. Or you observe a young firm with large investments but only small concurrent cash flows, which will only happen in a few years. This sort of variation is unwanted because it is related to unobserved variables that can bias your estimates in any direction.

To include the firm fixed effect, we use `gvkey` (Compustat’s firm identifier) as follows:

``` r
model_fe_firm <- feols(
  investment_lead ~ cash_flows + tobins_q | gvkey,
  vcov = "iid",
  data = data_investment
)
model_fe_firm
```

    OLS estimation, Dep. Var.: investment_lead
    Observations: 133,619
    Fixed-effects: gvkey: 14,696
    Standard-errors: IID 
               Estimate Std. Error t value  Pr(>|t|)    
    cash_flows   0.0138   0.000878    15.7 < 2.2e-16 ***
    tobins_q     0.0105   0.000127    83.0 < 2.2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    RMSE: 0.048853     Adj. R2: 0.535776
                     Within R2: 0.05584 

The regression output shows a lot of unexplained variation at the firm level that is taken care of by including the firm fixed effect as the adjusted R-squared rises above 50 percent. In fact, it is more interesting to look at the within R-squared that shows the explanatory power of a firm’s cash flow and Tobin’s q *on top* of the average investment of each firm. We can also see that the coefficients changed slightly in magnitude but not in sign.

There is another source of variation that we can get rid of in our setting: average investment across firms might vary over time due to macroeconomic factors that affect all firms, such as economic crises. By including year fixed effects, we can take out the effect of unobservables that vary over time. The two-way fixed effects regression is then \\ \text{Investment}\_{i,t+1} = \alpha_i + \alpha_t + \beta_1\text{Cash Flows}\_{i,t}+\beta_2\text{Tobin's q}\_{i,t}+\varepsilon\_{i,t},\\ where \\\alpha_t\\ is the time fixed effect. Here you can think of higher investments during an economic expansion with simultaneously high cash flows.

``` r
model_fe_firmyear <- feols(
  investment_lead ~ cash_flows + tobins_q | gvkey + year,
  vcov = "iid",
  data = data_investment
)
model_fe_firmyear
```

    OLS estimation, Dep. Var.: investment_lead
    Observations: 133,619
    Fixed-effects: gvkey: 14,696,  year: 37
    Standard-errors: IID 
               Estimate Std. Error t value  Pr(>|t|)    
    cash_flows  0.01682   0.000858    19.6 < 2.2e-16 ***
    tobins_q    0.00958   0.000126    76.4 < 2.2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    RMSE: 0.047558     Adj. R2: 0.559916
                     Within R2: 0.048894

The inclusion of time fixed effects did only marginally affect the R-squared and the coefficients, which we can interpret as a good thing as it indicates that the coefficients are not driven by an omitted variable that varies over time.

How can we further improve the robustness of our regression results? Ideally, we want to get rid of unexplained variation at the firm-year level, which means we need to include more variables that vary across firm *and* time and are likely correlated with investment. Note that we cannot include firm-year fixed effects in our setting because then cash flows and Tobin’s q are colinear with the fixed effects, and the estimation becomes void.

Before we discuss the properties of our estimation errors, we want to point out that regression tables are at the heart of every empirical analysis, where you compare multiple models. Fortunately, the `etable()` function provides a convenient way to tabulate the regression output (with many parameters to customize and even print the output in LaTeX). We recommend printing \\t\\-statistics rather than standard errors in regression tables because the latter are typically very hard to interpret across coefficients that vary in size. We also do not print p-values because they are sometimes misinterpreted to signal the importance of observed effects ([Wasserstein and Lazar 2016](#ref-Wasserstein2016)). The \\t\\-statistics provide a consistent way to interpret changes in estimation uncertainty across different model specifications.

``` r
etable(
  model_ols,
  model_fe_firm,
  model_fe_firmyear,
  coefstat = "tstat",
  digits = 3,
  digits.stats = 3
)
```

                           model_ols   model_fe_firm model_fe_firm..
    Dependent Var.:  investment_lead investment_lead investment_lead
                                                                    
    Constant        0.042*** (130.1)                                
    cash_flows       0.049*** (64.3) 0.014*** (15.7) 0.017*** (19.6)
    tobins_q         0.007*** (57.7) 0.011*** (83.0) 0.010*** (76.4)
    Fixed-Effects:  ---------------- --------------- ---------------
    gvkey                         No             Yes             Yes
    year                          No              No             Yes
    _______________ ________________ _______________ _______________
    VCOV type                    IID             IID             IID
    Observations             133,619         133,619         133,619
    R2                         0.044           0.587           0.608
    Within R2                     --           0.056           0.049
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Clustering Standard Errors

Apart from biased estimators, we usually have to deal with potentially complex dependencies of our residuals with each other. Such dependencies in the residuals invalidate the i.i.d. assumption of OLS and lead to biased standard errors. With biased OLS standard errors, we cannot reliably interpret the statistical significance of our estimated coefficients.

In our setting, the residuals may be correlated across years for a given firm (time-series dependence), or, alternatively, the residuals may be correlated across different firms (cross-section dependence). One of the most common approaches to dealing with such dependence is the use of *clustered standard errors* ([Petersen 2008](#ref-Petersen2008)). The idea behind clustering is that the correlation of residuals *within* a cluster can be of any form. As the number of clusters grows, the cluster-robust standard errors become consistent ([Donald and Lang 2007](#ref-Lang2007); [Wooldridge 2010](#ref-Wooldridge2010)). A natural requirement for clustering standard errors in practice is hence a sufficiently large number of clusters. Typically, around at least 30 to 50 clusters are seen as sufficient ([Cameron et al. 2011](#ref-Cameron2011)).

Instead of relying on the iid assumption, we can use the cluster option in the `feols`-function as above. The code chunk below applies both one-way clustering by firm as well as two-way clustering by firm and year.

``` r
model_cluster_firm <- feols(
  investment_lead ~ cash_flows + tobins_q | gvkey + year,
  cluster = "gvkey",
  data = data_investment
)

model_cluster_firmyear <- feols(
  investment_lead ~ cash_flows + tobins_q | gvkey + year,
  cluster = c("gvkey", "year"),
  data = data_investment
)
```

The table below shows the comparison of the different assumptions behind the standard errors. In the first column, we can see highly significant coefficients on both cash flows and Tobin’s q. By clustering the standard errors on the firm level, the \\t\\-statistics of both coefficients drop in half, indicating a high correlation of residuals within firms. If we additionally cluster by year, we see a drop, particularly for Tobin’s q, again. Even after relaxing the assumptions behind our standard errors, both coefficients are still comfortably significant as the \\t\\-statistics are well above the usual critical values of 1.96 or 2.576 for two-tailed significance tests.

``` r
etable(
  model_fe_firmyear,
  model_cluster_firm,
  model_cluster_firmyear,
  coefstat = "tstat",
  digits = 3,
  digits.stats = 3
)
```

                    model_fe_firm.. model_cluster.. model_cluster...1
    Dependent Var.: investment_lead investment_lead   investment_lead
                                                                     
    cash_flows      0.017*** (19.6) 0.017*** (11.4)   0.017*** (9.39)
    tobins_q        0.010*** (76.4) 0.010*** (35.8)   0.010*** (15.0)
    Fixed-Effects:  --------------- ---------------   ---------------
    gvkey                       Yes             Yes               Yes
    year                        Yes             Yes               Yes
    _______________ _______________ _______________   _______________
    VCOV type                   IID       by: gvkey  by: gvkey & year
    Observations            133,619         133,619           133,619
    R2                        0.608           0.608             0.608
    Within R2                 0.049           0.049             0.049
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Inspired by Abadie et al. ([2017](#ref-AbadieEtAl2017)), we want to close this chapter by highlighting that choosing the right dimensions for clustering is a design problem. Even if the data is informative about whether clustering matters for standard errors, they do not tell you whether you should adjust the standard errors for clustering. Clustering at too aggregate levels can hence lead to unnecessarily inflated standard errors.

## Key Takeaways

- Fixed effects regressions control for unobserved firm and time-specific factors, reducing omitted variable bias in panel data models.
- The `fixest` R package streamlines the estimation of fixed effects and supports clustering standard errors for robust inference.
- Clustered standard errors adjust for residual dependence across firms or years, leading to more accurate \\t\\-statistics and confidence in significance tests.
- Two-way clustering by firm and year is commonly used in finance to address both time-series and cross-sectional correlation in residuals.
- Careful model specification, including winsorization and proper clustering choices, enhances the credibility and reliability of empirical finance results.

## Exercises

1.  Estimate the two-way fixed effects model with two-way clustered standard errors using quarterly Compustat data from WRDS. Note that you can access quarterly data via `tbl(wrds, I("comp.fundq"))`.
2.  Following Peters and Taylor ([2017](#ref-Peters2017)), compute Tobin’s q as the market value of outstanding equity `mktcap` plus the book value of debt (`dltt` + `dlc`) minus the current assets `atc` and everything divided by the book value of property, plant and equipment `ppegt`. What is the correlation between the measures of Tobin’s q? What is the impact on the two-way fixed effects regressions?

## References

Abadie, Alberto, Susan Athey, Guido W Imbens, and Jeffrey Wooldridge. 2017. “When should you adjust standard errors for clustering?” *Working Paper*. <http://www.nber.org/papers/w24003>.

Bergé, Laurent. 2018. “Efficient estimation of maximum likelihood models with multiple fixed-effects: The R package FENmlm.” *CREA Discussion Papers*. <https://lrberge.github.io/fixest/>.

Cameron, A. Colin, Jonah B. Gelbach, and Douglas L. Miller. 2011. “Robust inference with multiway clustering.” *Journal of Business & Economic Statistics* 29 (2): 238–49. <http://www.jstor.org/stable/25800796>.

Donald, Stephen, and Kevin Lang. 2007. “Inference with difference-in-differences and other panel data.” *The Review of Economics and Statistics* 89 (2): 221–33. <https://doi.org/10.1162/rest.89.2.221>.

Erickson, Timothy, and Toni M. Whited. 2012. “Treating measurement error in Tobin’s q.” *Review of Financial Studies* 25 (4): 1286–329. <https://doi.org/10.1093/rfs/hhr120>.

Fazzari, Steven M., R. Glenn Hubbard, Bruce C. Petersen, Alan S. Blinder, and James M. Poterba. 1988. “Financing constraints and corporate investment.” *Brookings Papers on Economic Activity* 1988 (1): 141–206. <http://www.jstor.org/stable/2534426>.

Gulen, Huseyin, and Mihai Ion. 2015. “Policy uncertainty and corporate investment.” *Review of Financial Studies* 29 (3): 523–64. <https://doi.org/10.1093/rfs/hhv050>.

Peters, Ryan H., and Lucian A. Taylor. 2017. “Intangible capital and the investment-q relation.” *Journal of Financial Economics* 123 (2): 251–72. <https://doi.org/10.1016/j.jfineco.2016.03.011>.

Petersen, Mitchell A. 2008. “Estimating standard errors in finance panel data sets: Comparing approaches.” *Review of Financial Studies* 22 (1): 435–80. <https://doi.org/10.1093/rfs/hhn053>.

Wasserstein, Ronald L., and Nicole A. Lazar. 2016. “The ASA Statement on p-Values: Context, process, and purpose.” *The American Statistician* 70 (2): 129–33. <https://doi.org/10.1080/00031305.2016.1154108>.

Wooldridge, Jefrey M. 2010. *Econometric analysis of cross section and panel data*. The MIT Press. <http://www.jstor.org/stable/j.ctt5hhcfr>.
