# Beta estimation

In this chapter, we introduce you to an important concept in financial economics: the exposure of an individual stock to changes in the market portfolio. According to the Capital Asset Pricing Model (CAPM), cross-sectional variation in expected asset returns should be a function of the covariance between the excess return of the asset and the excess return on the market portfolio. The regression coefficient of market returns on excess returns is usually called the market beta. 
In this chapter we show an estimation procedure for the market betas. We do not go into details about the foundations of market beta, but simply refer to any treatment of the [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model) for further information. Instead, we provide details about all the functions that we use to compute the results. In particular, we leverage useful computational concepts: rolling-window estimation and parallelization. 

We use the following packages throughout this chapter:

```r
library(tidyverse)
library(RSQLite)
library(slider)
library(scales)
library(furrr)
```

## Estimating beta using monthly returns

The estimation procedure is based on a rolling-window estimation where we may use either monthly or daily returns and different window lengths. First, let us start with the monthly data we prepared in the previous chapter. 


```r
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite",
  extended_types = TRUE
)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") %>%
  collect()

factors_ff_monthly <- tbl(tidy_finance, "factors_ff_monthly") %>%
  collect()

crsp_monthly <- crsp_monthly %>%
  left_join(factors_ff_monthly, by = "month") %>%
  select(permno, month, industry, ret_excess, mkt_excess)
```

To estimate the CAPM equation 
$$
r_{i, t} - r_{f, t} = \alpha_i + \beta_i(r_{m, t}-r_{f,t})+\varepsilon_{i, t}
$$
we regress excess stock returns `ret_excess` on excess returns of the market portfolio `mkt_excess`. 
R provides a simple solution to estimate (linear) models with the function `lm()`. `lm()` requires a formula as input which is specific in a compact symbolic form. An expression of the form `y ~ model` is interpreted as a specification that the response `y` is modeled by a linear predictor specified symbolically by `model`. Such a model consists of a series of terms separated by `+` operators. In addition to standard linear models, `lm()` provides a lot of flexibility. You should check out the documentation for more information. To start, we restrict the data only to the time series of observations in CRSP that correspond to Apple’s stock (i.e., to `permno` 14593 for Apple) and compute $\alpha_i$ as well as $\beta_i$.


```r
fit <- lm(ret_excess ~ mkt_excess,
  data = crsp_monthly %>%
    filter(permno == "14593")
)

summary(fit)
```

```
## 
## Call:
## lm(formula = ret_excess ~ mkt_excess, data = crsp_monthly %>% 
##     filter(permno == "14593"))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.5167 -0.0610  0.0009  0.0643  0.3940 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.01051    0.00532    1.98    0.049 *  
## mkt_excess   1.40085    0.11748   11.92   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.115 on 478 degrees of freedom
## Multiple R-squared:  0.229,	Adjusted R-squared:  0.228 
## F-statistic:  142 on 1 and 478 DF,  p-value: <2e-16
```

`lm()` returns an object of class `lm` which contains all information we usually care about with linear models. `summary()` returns an overview of the estimated parameters. `coefficients(fit)` would return only the estimated coefficients. The output above indicates that Apple moves excessively with the market as the estimated $\beta_i$ is above one ($\hat\beta_i$ = 1.4). 

## Rolling-window estimation

After we estimated the regression coefficients on an example, we scale the estimation of  $\beta_i$ to a whole different level and perform rolling-window estimations for the entire CRSP sample. The following function implements the CAPM regression for a data frame (or a part thereof) containing at least `min_obs` observations to avoid huge fluctuations if the time series is too short. If the condition is violated, the function returns a missing value. 


```r
estimate_capm <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    beta <- as.numeric(NA)
  } else {
    fit <- lm(ret_excess ~ mkt_excess, data = data)
    beta <- as.numeric(fit$coefficients[2])
  }
  return(beta)
}
```

Next, we define a function that does the rolling estimation. To perform the rolling-window estimation, we use the `slider` package of [Davis Vaughan](https://github.com/DavisVaughan/slider), which provides a family of sliding window functions similar to `purrr::map()`. Most importantly, the `slide_period` function is able to handle months in its window input in a straightforward manner. We thus avoid using any time-series package (e.g., `zoo`) and converting the data to fit the package functions, but rather stay in the world of tibbles.

The following function takes input data and slides across the month vector, considering only a total of `months` months. The function essentially performs three steps: (i) combine all rows into a single data frame (which comes in handy in the case of daily data), (ii) compute betas by sliding across months, and (iii) return a tibble with months and corresponding beta estimates (again particularly useful in the case of daily data).
As we demonstrate further below, we can also apply the same function to daily returns data. 

```r
roll_capm_estimation <- function(data, months, min_obs) {
  data <- bind_rows(data) %>%
    arrange(month)

  betas <- slide_period_vec(
    .x = data,
    .i = data$month,
    .period = "month",
    .f = ~ estimate_capm(., min_obs),
    .before = months - 1,
    .complete = FALSE
  )

  tibble(
    month = unique(data$month),
    beta = betas
  )
}
```

Before we attack the whole CRSP sample, let us focus on a couple of examples for well-known firms.

```r
examples <- tribble(
  ~permno, ~company,
  14593, "Apple",
  10107, "Microsoft",
  93436, "Tesla",
  17778, "Berkshire Hathaway"
)
```
If we want to estimate rolling betas for Apple, we can use `mutate()`. 
We take a total of 5 years of data and require at least 48 months with return data to compute our betas. 
Check out the exercises if you want ot compute beta for different time periods. 

```r
beta_example <- crsp_monthly %>%
  filter(permno == examples$permno[1]) %>%
  mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) %>%
  drop_na()
beta_example
```

```
## # A tibble: 433 x 6
##   permno month      industry      ret_excess mkt_excess  beta
##    <dbl> <date>     <chr>              <dbl>      <dbl> <dbl>
## 1  14593 1984-12-01 Manufacturing     0.170      0.0184  2.05
## 2  14593 1985-01-01 Manufacturing    -0.0108     0.0799  1.90
## 3  14593 1985-02-01 Manufacturing    -0.152      0.0122  1.88
## 4  14593 1985-03-01 Manufacturing    -0.112     -0.0084  1.89
## # ... with 429 more rows
```
It is actually quite simple to perform the rolling-window estimation for an arbitrary number of stocks, which we visualize in the following code chunk. 

```r
beta_examples <- crsp_monthly %>%
  inner_join(examples, by = "permno") %>%
  group_by(permno) %>%
  mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) %>%
  ungroup() %>%
  select(permno, company, month, beta_monthly = beta) %>%
  drop_na()

beta_examples %>%
  ggplot(aes(x = month, y = beta_monthly, color = company)) +
  geom_line() +
  labs(
    x = NULL, y = NULL, color = NULL,
    title = "Monthly beta estimates for example stocks using 5 years of monthly data"
  )
```

<img src="31_beta_files/figure-html/unnamed-chunk-8-1.png" width="672" style="display: block; margin: auto;" />

## Paralellized rolling-window estimation

Even though we could now just apply the function using `group_by()` on the whole CRSP sample, we advise against doing it as it is computationally quite expensive. 
Remember that we have to perform rolling-window estimations across all stocks and time periods. 
However, this estimation problem is an ideal scenario to employ the power of parallelization. 
Parallelization means that we split the tasks which perform rolling-window estimations across different workers (or cores on your local machine). 
It turns out to be quite easy to implement with only a small addition to what we already have learned using `map()`-functions. 

First, we nest the data by `permno`. Nested data means we now have a list of `permno` with corresponding time series data.


```r
crsp_monthly_nested <- crsp_monthly %>%
  nest(data = c(month, ret_excess, mkt_excess))
crsp_monthly_nested
```

```
## # A tibble: 29,207 x 3
##   permno industry      data              
##    <dbl> <chr>         <list>            
## 1  10000 Manufacturing <tibble [16 x 3]> 
## 2  10001 Utilities     <tibble [378 x 3]>
## 3  10002 Finance       <tibble [324 x 3]>
## 4  10003 Finance       <tibble [118 x 3]>
## # ... with 29,203 more rows
```

Note that we could use `map()` across all the `permno`s and get the same results as above. 


```r
crsp_monthly_nested %>%
  inner_join(examples, by = "permno") %>%
  mutate(beta = map(data, ~ roll_capm_estimation(., months = 60, min_obs = 48))) %>%
  unnest(c(beta)) %>%
  select(permno, month, beta_monthly = beta) %>%
  drop_na()
```

```
## # A tibble: 1,362 x 3
##   permno month      beta_monthly
##    <dbl> <date>            <dbl>
## 1  10107 1990-03-01         1.39
## 2  10107 1990-04-01         1.38
## 3  10107 1990-05-01         1.43
## 4  10107 1990-06-01         1.43
## # ... with 1,358 more rows
```

However, instead, we want to perform the estimations of rolling betas for different stocks in parallel. We can use the flexibility of the `future` package, which we use to define how we want to perform the parallel estimation. If you have a Windows machine, it makes most sense to define `multisession`, which means that separate R processes are running in the background on the same machine to perform the individual jobs. If you check out the documentation of `plan()`, you can also see other ways to resolve the parallelization.


```r
plan(multisession, workers = availableCores())
```

Using eight cores, the estimation for our sample of around 25k stocks takes around 20 minutes. Of course, you can speed up things considerably by having more cores available to share the workload or by having more powerful cores. Notice the difference in the code below? All you need to do is to replace `map` with `future_map`.


```r
beta_monthly <- crsp_monthly_nested %>%
  mutate(beta = future_map(data, ~ roll_capm_estimation(., months = 60, min_obs = 48))) %>%
  unnest(c(beta)) %>%
  select(permno, month, beta_monthly = beta) %>%
  drop_na()
```

## Estimating beta using daily returns

Before we provide some descriptive statistics of our beta estimates, we implement the estimation for the daily CRSP sample as well. 
Depending on the application, you might either use longer horizon beta estimates based on monthly data or shorter horizon estimates based on daily returns. 

First, we load daily CRSP data. 
Note that the sample is large compared to the monthly data, so make sure to have enough memory available.


```r
crsp_daily <- tbl(tidy_finance, "crsp_daily") %>%
  collect()
```

We also need the daily Fama-French market excess returns. 


```r
factors_ff_daily <- tbl(tidy_finance, "factors_ff_daily") %>%
  collect()
```

We make sure to keep only relevant data to save memory space. 
However, note that your machine might not have enough memory to read the whole daily CRSP sample. In this case, we refer you to the exercises and try working with loops as in chapter 2. 


```r
crsp_daily <- crsp_daily %>%
  inner_join(factors_ff_daily, by = "date") %>%
  select(permno, month, ret_excess, mkt_excess)
```

Just like above, we nest the data by `permno` for parallelization.


```r
crsp_daily_nested <- crsp_daily %>%
  nest(data = c(month, ret_excess, mkt_excess))
```

This is what the estimation looks like for a couple of examples using `map()`. 
For the daily data, we use the same function as above, but only take 3 months of data and require at least 50 daily return observations in these months. 
These restrictions help us to retrieve somehow smooth coefficient estimates.


```r
crsp_daily_nested %>%
  inner_join(examples, by = "permno") %>%
  mutate(beta_daily = map(data, ~ roll_capm_estimation(., months = 3, min_obs = 50))) %>%
  unnest(c(beta_daily)) %>%
  select(permno, month, beta_daily = beta) %>%
  drop_na()
```

```
## # A tibble: 1,543 x 3
##   permno month      beta_daily
##    <dbl> <date>          <dbl>
## 1  10107 1986-05-01      0.898
## 2  10107 1986-06-01      0.906
## 3  10107 1986-07-01      0.822
## 4  10107 1986-08-01      0.900
## # ... with 1,539 more rows
```

For the sake of completeness, we tell our session again to use multiple workers for parallelization.


```r
plan(multisession, workers = availableCores())
```

The code chunk for beta estimation using daily returns now looks very similar to the one for monthly data. The whole estimation takes around 30 minutes using eight cores and 32gb memory. 


```r
beta_daily <- crsp_daily_nested %>%
  mutate(beta_daily = future_map(data, ~ roll_capm_estimation(., months = 3, min_obs = 50))) %>%
  unnest(c(beta_daily)) %>%
  select(permno, month, beta_daily = beta) %>%
  drop_na()
```

## Comparing beta estimates

What is a typical value for stock betas? To get some feeling, we illustrate the dispersion of the estimated $\hat\beta_i$ across different industries and across time below. The first figure below shows that typical business models across industries imply different exposure to the general market economy. However, there are barely any firms that exhibit a negative exposure to the market factor.   


```r
crsp_monthly %>%
  left_join(beta_monthly, by = c("permno", "month")) %>%
  drop_na(beta_monthly) %>%
  group_by(industry, permno) %>%
  summarise(beta = mean(beta_monthly)) %>%
  ggplot(aes(x = reorder(industry, beta, FUN = median), y = beta)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Average beta estimates by industry"
  )
```

<img src="31_beta_files/figure-html/unnamed-chunk-19-1.png" width="672" style="display: block; margin: auto;" />

Next, we illustrate the time-variation in the cross-section of estimated betas. The figure below shows the monthly deciles of estimated betas (based on monthly data) and indicates an interesting pattern: First, betas seem to vary over time in the sense that during some periods, there is a clear trend across all deciles. Second, the sample exhibits periods where the dispersion across stocks increases in the sense that the lower decile decreases and the upper decile increases, which indicates that for some stocks the correlation with the market increases while for others it decreases. Note also here: stocks with negative betas are an extremely rare exception.


```r
beta_monthly %>%
  drop_na(beta_monthly) %>%
  group_by(month) %>%
  summarise(
    x = quantile(beta_monthly, seq(0.1, 0.9, 0.1)),
    quantile = 100 * seq(0.1, 0.9, 0.1),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = month, y = x, color = as_factor(quantile))) +
  geom_line() +
  labs(
    x = NULL, y = "Beta", color = NULL,
    title = "Distribution of estimated betas",
    subtitle = "Monthly deciles for CRSP cross-section"
  )
```

<img src="31_beta_files/figure-html/unnamed-chunk-20-1.png" width="672" style="display: block; margin: auto;" />

To compare the difference between daily and monthly data, we combine beta estimates to a single table. Then, we use the table to plot a comparison of beta estimates for our example stocks. 


```r
beta <- beta_monthly %>%
  full_join(beta_daily, by = c("permno", "month")) %>%
  arrange(permno, month)

beta %>%
  inner_join(examples, by = "permno") %>%
  pivot_longer(cols = c(beta_monthly, beta_daily)) %>%
  ggplot(aes(x = month, y = value, color = name)) +
  geom_line() +
  facet_wrap(~company, ncol = 1) +
  labs(
    x = NULL, y = NULL, color = NULL,
    title = "Comparison of beta estimates using 5 years of monthly and 3 months of daily data"
  )
```

```
## Warning: Removed 46 row(s) containing missing values (geom_path).
```

<img src="31_beta_files/figure-html/unnamed-chunk-21-1.png" width="672" style="display: block; margin: auto;" />

The estimates look as expected. As you can see, it really depends on the estimation window and data frequency how your beta estimates turn out. 

Finally, we write the estimates to our database such that we can use them in later chapters. 


```r
beta %>%
  dbWriteTable(tidy_finance, "beta", ., overwrite = TRUE)
```

Whenever you perform some kind of estimation, it also makes sense to do rough plausibility tests. A possible check is to plot the share of stocks with beta estimates over time. 
This descriptive helps us discover potential errors in our data preparation or estimation procedure. 
For instance, suppose there was a gap in our output where we do not have any betas. 
In this case, we would have to go back and check all previous steps. 


```r
beta_long <- crsp_monthly %>%
  left_join(beta, by = c("permno", "month")) %>%
  pivot_longer(cols = c(beta_monthly, beta_daily))

beta_long %>%
  group_by(month, name) %>%
  summarize(share = sum(!is.na(value)) / n()) %>%
  ggplot(aes(x = month, y = share, color = name)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  labs(
    x = NULL, y = NULL, color = NULL,
    title = "End-of-month share of securities with beta estimates"
  ) +
  coord_cartesian(ylim = c(0, 1))
```

<img src="31_beta_files/figure-html/unnamed-chunk-23-1.png" width="672" style="display: block; margin: auto;" />

The figure above does not indicate any troubles, so let us move on to the next check. 

We also encourage everyone to always look at the distributional summary statistics of variables. You can easily spot outliers or weird distributions when looking at such tables. 


```r
beta_long %>%
  select(name, value) %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    q05 = quantile(value, 0.05),
    q25 = quantile(value, 0.25),
    q50 = quantile(value, 0.50),
    q75 = quantile(value, 0.75),
    q95 = quantile(value, 0.95),
    max = max(value),
    n = n()
  )
```

```
## # A tibble: 2 x 11
##   name          mean    sd   min    q05   q25   q50   q75   q95   max       n
##   <chr>        <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <int>
## 1 beta_daily   0.743 0.925 -43.7 -0.452 0.203 0.679  1.22  2.22  56.6 3186254
## 2 beta_monthly 1.10  0.711 -13.0  0.123 0.631 1.03   1.47  2.32  10.3 2071015
```

Finally, since we have two different estimators for the same theoretical object, we expect the estimators should be at least positively correlated (although not perfectly as the estimators are based on different sample periods).


```r
beta %>%
  select(beta_daily, beta_monthly) %>%
  cor(., use = "complete.obs")
```

```
##              beta_daily beta_monthly
## beta_daily        1.000        0.322
## beta_monthly      0.322        1.000
```

Indeed, we find a positive correlation between our beta estimates. In the subsequent chapters, we mainly use the estimates based on monthly data as most readers should be able to replicate them due to potential memory limitations that might arise with the daily data. 

## Exercises

1. Compute beta estimates based on monthly data using 1, 3 and 5 years of data and impose a minimum number of observations of 10, 28 and 48 months with return data, respectively. How strongly correlated are the estimated betas?
1. Compute beta estimates based on monthly data using 5 years of data and impose different number of minimum observations. How does the share of permno-month observations with successful beta estimates vary across the different requirements? Do you find high correlation across the estimated betas? 
1. Instead of using `future_map()` perform the beta estimation in a loop (using either monthly or daily data) for a subset of 100 permnos of your choice. Verify that you get the same results as with the parallelized code from above.
1. Filter out the stocks with negative betas. Do these stocks frequently exhibit negative betas or do they resemble estimation errors? 
