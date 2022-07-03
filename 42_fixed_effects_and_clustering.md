# Fixed effects and clustered standard errors

When working with regressions in empirical finance, you will sooner or later be confronted with the concepts of fixed effects and clustered standard errors. In this chapter, we provide an intuitive introduction to these concepts and provide the code to run a classical panel regression common to the investment literature (CITATIONS). Typically, the investment regression uses quarterly balance sheet data provided via Compustat because it allows for richer dynamics in the regressors and more opportunities to construct variables. As we focus on the implementation of fixed effects and clustered standard errors, we use the same annual Compustat data from previous chapters and leave the estimation using quarterly data as an exercise. We demonstrate below that the regression based on annual data yields results quite similar to estiations based on quarterly data from the literature.  

The current chapter relies on the following set of packages. 

```r
library(tidyverse)
```

```
## Warning: package 'dplyr' was built under R version 4.1.3
```

```r
library(RSQLite)
```

```
## Warning: package 'RSQLite' was built under R version 4.1.3
```

```r
library(lubridate)
library(fixest)
```

```
## Warning: package 'fixest' was built under R version 4.1.3
```

## Databases

We use CRSP and annual Compustat as data sources from our `SQLite`-database introduced in our chapter on *"Accessing & managing financial data"*. In particular, Compustat provides balance sheet and income statement data on a firm level, while CRSP provides market valuations. 


```r
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite",
                          extended_types = TRUE
)

crsp <- tbl(tidy_finance, "crsp_monthly") %>% 
  collect()

compustat <- tbl(tidy_finance, "compustat") %>%
  collect()
```


## Data preparation

The classical investment regressions models the capital investment of a firm as a function that increases in operating cash flows and Tobin's q, a measure for ia firm's investment opportunities. We start by constructing investment and cash flows which are usually normalized by lagged total assets of a firm. 


```r
data_investment <- compustat %>% 
  mutate(month = floor_date(datadate, "month")) %>% 
  left_join(compustat %>% 
              select(gvkey, year, at_lag = at) %>% 
              mutate(year = year + 1), by = c("gvkey", "year")) %>% 
  mutate(investment = capx / at_lag,
         cash_flows = oancf / at_lag)

data_investment <- data_investment %>% 
  left_join(data_investment %>% 
              select(gvkey, year, investment_lead = investment) %>% 
              mutate(year = year - 1), by = c("gvkey", "year"))
```

Tobin's q is the ratio of market value of capital to its replacement costs. It is one of the most common regressors in corporate finance applications (Fazzari, Hubbard, and Petersen 1988, Erickson and Whited, 2012). We follow the implementation of Gulen & Ion 2015 and compute Tobin's q as the market value of equity `mktcap` plus the book value of assets `at` minus book value of equity `bet` plus deferred taxes `txdb`, all divided by book value of assets `at`. Finally, we only keep observations where all variables of interest are non-missing or non-infinite. 


```r
 data_investment <- data_investment %>% 
    left_join(crsp %>% 
              select(gvkey, month, mktcap), by = c("gvkey", "month")) %>% 
  mutate(tobins_q = (mktcap + at - be + txdb) / at)

 data_investment <- data_investment %>% 
  select(gvkey, year, investment_lead, cash_flows, tobins_q) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.))) 
```

As the variable construction typically leads to extreme values that are most likely related to data issues (e.g., reporting errors), many papers include winsorization of the variables of interest. Winsorization involves replacing values of extreme outliers with quantiles on the respective end. The following function implements the winsorization for any percentage cut that should be applied on either end of the distributions.  


```r
winsorize <- function(x, cut){
  cut_point_top <- quantile(x, 1 - cut, na.rm = TRUE)
  cut_point_bottom <- quantile(x, cut, na.rm = TRUE)
  i <- which(x >= cut_point_top)
  x[i] <- cut_point_top
  j <- which(x <= cut_point_bottom)
  x[j] <- cut_point_bottom
  return(x)
}

data_investment <- data_investment %>% 
  mutate(across(c(investment_lead, cash_flows, tobins_q), ~winsorize(., 0.01)))
```

Before proceeding to any estimations, we highly recommend tabulating summary statistics of the variables that enter the regression. These simple tables allow you to check the plausibility of your numerical variables, as well as spot any obvious errors or outliers. 


```r
data_investment %>%
  pivot_longer(cols = c(investment_lead, cash_flows, tobins_q), 
               names_to = "measure", values_drop_na = TRUE) %>% 
  mutate(measure = factor(measure, 
                          levels = c("investment_lead", "cash_flows", "tobins_q"))) %>% 
  group_by(measure) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            min = min(value),
            q05 = quantile(value, 0.05),
            q25 = quantile(value, 0.25),
            q50 = quantile(value, 0.50),
            q75 = quantile(value, 0.75),
            q95 = quantile(value, 0.95),
            max = max(value),
            n = n()) %>%
  ungroup()
```

```
## # A tibble: 3 x 11
##   measure         mean     sd    min      q05      q25    q50    q75   q95    max
##   <fct>          <dbl>  <dbl>  <dbl>    <dbl>    <dbl>  <dbl>  <dbl> <dbl>  <dbl>
## 1 investment_l~ 0.0591 0.0784  0      7.72e-4  0.0126  0.0339 0.0727 0.210  0.470
## 2 cash_flows    0.0167 0.261  -1.46  -4.44e-1 -0.00272 0.0653 0.132  0.273  0.481
## 3 tobins_q      1.98   1.66    0.571  7.91e-1  1.05    1.38   2.18   5.27  10.7  
## # ... with 1 more variable: n <int>
```

## Fixed effects 

To illustrate fixed effects regressions, we use the `fixest` package which is both computationally powerful and flexible with respect to model specifications. We start out with the basic investment regression using the simple model
$$ \text{Investment}_{t+1} = \alpha + \beta_1\text{Cash Flows}_{t}+\beta_2\text{Tobin's q}_t+\varepsilon_t,$$
where $\varepsilon_t$ is iid normally distributed. We use the `fixest::feols()` function to estimate the simple model, so that the output has the same structure as the other regressions. 


```r
model_ols <- feols(
  fml = investment_lead ~ cash_flows + tobins_q,
  se = "iid",
  data = data_investment
)
model_ols
```

```
## OLS estimation, Dep. Var.: investment_lead
## Observations: 121,245 
## Standard-errors: IID 
##             Estimate Std. Error t value  Pr(>|t|)    
## (Intercept)  0.04235   0.000350   120.9 < 2.2e-16 ***
## cash_flows   0.05346   0.000867    61.6 < 2.2e-16 ***
## tobins_q     0.00804   0.000136    59.1 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## RMSE: 0.076561   Adj. R2: 0.046232
```

The regression output shows a highly significant coefficients for both variables: as expected, higher cash flows and investment opportunities are associated with higher investment. However, the simple model actually has a lot of omitted variables, so our coefficients are most likely biased. As there is a lot of unexplained variation in our simple model (indicated by the adjusted R-squared), the bias in our coefficients is potentially severe and the true values could be above or below zero. 

One way to tackle the issue of omitted variable bias is to get rid of as much unexplained variation as possible by including *fixed effects* - model parameters that are fixed for specific groups. In essence, each group has its own mean in fixed effects regressions. The simplest group that we can form in the investment regression is the firm level. The firm fixed effects regression is then
$$ \text{Investment}_{t+1} = \alpha_i + \beta_1\text{Cash Flows}_{t}+\beta_2\text{Tobin's q}_t+\varepsilon_t,$$
where $\alpha_i$ is the firm-specific mean investment. To include the firm fixed effect, we use `gvkey` as follows:

```r
model_fe_firm <- feols(
  investment_lead ~ cash_flows + tobins_q | gvkey,
  se = "iid",
  data = data_investment
)
model_fe_firm
```

```
## OLS estimation, Dep. Var.: investment_lead
## Observations: 121,245 
## Fixed-effects: gvkey: 13,692
## Standard-errors: IID 
##            Estimate Std. Error t value  Pr(>|t|)    
## cash_flows   0.0153   0.000994    15.3 < 2.2e-16 ***
## tobins_q     0.0116   0.000140    82.9 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## RMSE: 0.050367     Adj. R2: 0.534682
##                  Within R2: 0.06126
```
The regression output shows that there is a lot unexplained variation at the firm level that is taken care of by including the firm fixed effect as the adjusted R-squared rises above 50%. In fact, it is more interesting to look at the within R-squared that shows the explanatory power of our cash flow and Tobin's q *on top* of the average investment of each firm. We can also see that the coefficients changed slightly in magnitude, but not in sign. 

There is another source of variation that we can still get rid of in our setting: average investment across firms might vary over time due to macro factors that vary over time and affect all firms the same such as economic crises. By including year fixed effects, we can take out the effect of unbservables that vary over time. The two-way fixed effects regression is then
$$ \text{Investment}_{t+1} = \alpha_i + \alpha_t + \beta_1\text{Cash Flows}_{t}+\beta_2\text{Tobin's q}_t+\varepsilon_t,$$
where $\alpha_t$ is the time fixed effect. 

```r
model_fe_firmyear <- feols(
  investment_lead ~ cash_flows + tobins_q | gvkey + year,
  se = "iid",
  data = data_investment
)
model_fe_firmyear
```

```
## OLS estimation, Dep. Var.: investment_lead
## Observations: 121,245 
## Fixed-effects: gvkey: 13,692,  year: 33
## Standard-errors: IID 
##            Estimate Std. Error t value  Pr(>|t|)    
## cash_flows   0.0189   0.000973    19.5 < 2.2e-16 ***
## tobins_q     0.0105   0.000139    75.4 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## RMSE: 0.049123     Adj. R2: 0.557249
##                  Within R2: 0.052705
```
The inclusion of time fixed effects did only marginally affect the R-squared and the coefficients which we can interpret as a good thing. 

How can we further improve the robustness of our regression results? Ideally, we want to get rid of unexplained variation at the firm-year level which means we need to include more variables that vary across firm *and* time and are likely correlated with investment. Note that we cannot include firm-year fixed effects in our setting because then cash flows and Tobin's q are colinear with the fixed effects and the estimation becomes void.

Before we move on to discuss the properties of our estimation errors, we want to point out that regression tables are at the heart of every empirical analysis where you compare multiple models. Fortunately, the `fixest:etable()` provides a convenient way to tabulate the regression output (with many parameters to customize and even print the output in LaTeX). We recommend printing $t$-statisics rather than standard errors in regression tables because the latter are typically very hard to interpret across coefficients that vary in size. The $t$ statistics provide a consistent way to interpret changes in estimation uncertainty across different model specifications.  

```r
etable(model_ols, model_fe_firm, model_fe_firmyear, 
       coefstat = "tstat")
```

```
##                         model_ols     model_fe_firm model_fe_firmyear
## Dependent Var.:   investment_lead   investment_lead   investment_lead
##                                                                      
## (Intercept)     0.0423*** (120.9)                                    
## cash_flows      0.0535*** (61.64) 0.0153*** (15.35) 0.0189*** (19.46)
## tobins_q        0.0080*** (59.10) 0.0116*** (82.88) 0.0105*** (75.40)
## Fixed-Effects:  ----------------- ----------------- -----------------
## gvkey                          No               Yes               Yes
## year                           No                No               Yes
## _______________ _________________ _________________ _________________
## VCOV type                     IID               IID               IID
## Observations              121,245           121,245           121,245
## R2                        0.04625           0.58723           0.60737
## Within R2                      --           0.06126           0.05271
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Clustering standard errors

Apart from biasedness in the estimators, we usually have to deal with potentially complex dependencies of our residuals with each other, hence invalidating the iid assumption of OLS and causing biased standard errors. With biased OLS standard errors, we cannot interpret the statistical significance of our estimated coefficients. 

In our setting, the residuals may be correlated across years for a given firm (time-series dependence) or, alternatively, the residuals may be orrelated across different forms (cross-section dependence). One of the most common approaches to deal with such dependence is the use of *clustered standard errors* (Petersen, 2009). The idea behind clustering is that the correlation of residuals *within* a cluster can be of any form, i.e., we do not have to assume any parametric form. As the number of clusters grow, the cluster-robust standard errors become consistent (Donald and Lang, 2007). A natural requirement for clustering standard errors in practice is hence a sufficiently large number of clusters. Typically, around at least 30 to 50 clusters are seen as sufficient (Cameron, Gelbach Miller).

Instead of using the iid assumption, we can use the cluster option in the same function as above. The code chunk below applies both one-way clustering by firm, as well as two-way clustering by firm and year.


```r
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

The table below shows the comparison of the different assumptions behind the standard errors. In the first column, we can see highly signficant coefficients on both cash flows and Tobin's q. By clustering the standard errors on the firm level, the $t$-statistics of both coefficients drop in half, indicating indeed high correlation of residuals within firms. If we additionally cluster by year, we see again a drop, in particular for Tobin's q. This suggests that the measurement of Tobin's q involves substantial measurement error that is more strongly correlated across time than for cash flows. Even after relaxing the assumptions behind our standard errros, both coefficients are still comfortably significant as the $t$ statistics are well above the usual critical values of 1.96 or 2.576 for two-tailed significance tests


```r
etable(model_fe_firmyear, model_cluster_firm, model_cluster_firmyear, 
       coefstat = "tstat")
```

```
##                 model_fe_firmyear model_cluster_f.. model_cluster_f..
## Dependent Var.:   investment_lead   investment_lead   investment_lead
##                                                                      
## cash_flows      0.0189*** (19.46) 0.0189*** (11.33) 0.0189*** (9.572)
## tobins_q        0.0105*** (75.40) 0.0105*** (35.60) 0.0105*** (17.00)
## Fixed-Effects:  ----------------- ----------------- -----------------
## gvkey                         Yes               Yes               Yes
## year                          Yes               Yes               Yes
## _______________ _________________ _________________ _________________
## VCOV type                     IID         by: gvkey  by: gvkey & year
## Observations              121,245           121,245           121,245
## R2                        0.60737           0.60737           0.60737
## Within R2                 0.05271           0.05271           0.05271
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Exercises

1. Estimate the two-way fixed effects model with two-way clustered standard errors using quarterly Compustat data from WRDS. Note that you can access quarterly data via `tbl(wrds, in_schema("comp", "fundq"))`.
1. Following Peters and Taylor 2016, compute Tobin's q as the market value of outstanding equity `mktcap` plus the book value of debt (`dltt` + `dlc`) minus the current assets `atc` and everything divided by the book value of property, plant and equipment `ppegt`. What is the correlation between the Tobin's q measures? What is the impact on the two-way fixed effects regressions?
