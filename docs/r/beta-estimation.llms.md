# Beta Estimation

> **NOTE:**
>
> You are reading **Tidy Finance with R**. You can find the equivalent chapter for the sibling **Tidy Finance with Python** [here](../python/beta-estimation.llms.md).

In this chapter, we introduce an important concept in financial economics: the exposure of an individual stock to changes in the market portfolio. According to the Capital Asset Pricing Model (CAPM) of Sharpe ([1964](#ref-Sharpe1964)), Lintner ([1965](#ref-Lintner1965)), and Mossin ([1966](#ref-Mossin1966)), cross-sectional variation in expected asset returns should be a function of the covariance between the excess return of the asset and the excess return on the market portfolio. The regression coefficient of excess market returns on excess stock returns is usually called the market beta. We show an estimation procedure for the market betas. We do not go into details about the foundations of market beta but simply refer to any treatment of the [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model) for further information. Instead, we provide details about all the functions that we use to compute the results. In particular, we leverage useful computational concepts: rolling-window estimation and parallelization.

We use the following R packages throughout this chapter:

``` r
library(tidyverse)
library(arrow)
library(slider)
library(furrr)
```

Compared to previous chapters, we introduce `slider` ([Vaughan 2021](#ref-slider)) for sliding window functions, and `furrr` ([Vaughan and Dancho 2022](#ref-furrr)) to apply mapping functions in parallel.

## Estimating Beta Using Monthly Returns

The estimation procedure is based on a rolling-window estimation, where we may use either monthly or daily returns and different window lengths. First, let us start with loading the monthly CRSP data from our Parquet files introduced in [Accessing and Managing Financial Data](../r/accessing-and-managing-financial-data.llms.md) and [WRDS, CRSP, and Compustat](../r/wrds-crsp-and-compustat.llms.md).

``` r
crsp_monthly <- read_parquet("data-r/crsp_monthly.parquet") |>
  select(permno, date, industry, ret_excess)

factors_ff3_monthly <- read_parquet("data-r/factors_ff3_monthly.parquet") |>
  select(date, mkt_excess)

crsp_monthly <- crsp_monthly |>
  left_join(factors_ff3_monthly, join_by(date))
```

To estimate the CAPM regression coefficients

\\ r\_{i, t} - r\_{f, t} = \alpha_i + \beta_i(r\_{m, t}-r\_{f,t})+\varepsilon\_{i, t} \\

we regress stock excess returns `ret_excess` on excess returns of the market portfolio `mkt_excess`. R provides a simple solution to estimate (linear) models with the function `lm()`. `lm()` requires a formula as input that is specified in a compact symbolic form. An expression of the form `y ~ model` is interpreted as a specification that the response `y` is modeled by a linear predictor specified symbolically by `model`. Such a model consists of a series of terms separated by `+` operators. In addition to standard linear models, `lm()` provides a lot of flexibility. You should check out the documentation for more information. To start, we restrict the data only to the time series of observations in CRSP that correspond to Apple’s stock (i.e., to `permno` 14593 for Apple) and compute \\\hat\alpha_i\\ as well as \\\hat\beta_i\\.

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
    mkt_excess   1.37566    0.10761   12.78 8.89e-33

`lm()` returns an object of class `lm` which contains all information we usually care about with linear models. `summary()` returns information about the estimated parameters. The output above indicates that Apple moves excessively with the market as the estimated \\\hat\beta_i\\ is above one (\\\hat\beta_i \approx 1.4\\).

## Rolling-Window Estimation

After we estimated the regression coefficients on an example, we scale the estimation of \\\beta_i\\ to a whole different level and perform rolling-window estimations for the entire CRSP sample. The following function implements the CAPM regression for a data frame (or a part thereof) containing at least `min_obs` observations to avoid huge fluctuations if the time series is too short. The function conveniently returns the regression results as a data frame, which ensures that our approach is scalable. If the `min_obs`-condition is violated, that is, the time series is too short, the function returns an empty data frame for consistency.

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

Next, we define a function that performs the rolling estimation. The `slide_period` function is able to handle months in its window input in a straightforward manner. The following function takes input data and slides across the `date` vector, considering only a total of `look_back` months. The function essentially performs three steps: (i) arrange all rows, (ii) compute betas by sliding across months, and (iii) return a tibble with months and corresponding parameter estimates. As we demonstrate further below, we can also apply the same function to daily returns data.

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

Before we attack the whole CRSP sample, let us focus on a couple of examples for well-known firms.

``` r
examples <- tibble(
  permno = c(14593, 10107, 93436, 17778),
  company = c("Apple", "Microsoft", "Tesla", "Berkshire Hathaway")
)
```

The main idea is to apply the function to each stock individually and then combine the results into a single data frame. First, we nest the data by `permno`. Nested data means we now have a list of `permno` with corresponding grouped time series data. We get one row of output for each unique combination of non-nested variables which is only `permno` in this case.

``` r
capm_examples_nested <- crsp_monthly |>
  filter(permno %in% examples$permno) |>
  nest(data = c(date, ret_excess, mkt_excess, industry))
capm_examples_nested
```

    # A tibble: 4 × 2
      permno data              
       <dbl> <list>            
    1  10107 <tibble [465 × 4]>
    2  14593 <tibble [528 × 4]>
    3  17778 <tibble [578 × 4]>
    4  93436 <tibble [174 × 4]>

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

[Figure 1](#fig-601) displays the resulting beta estimates, focusing exclusively on the coefficient fo `"mkt_excess"`.

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

[![Title: Monthly beta estimates for example stocks using 5 years of data. The figure shows a time series of beta estimates based on 5 years of monthly data for Apple, Berkshire Hathaway, Microsoft, and Tesla. The estimated betas vary over time and across varies but are always positive for each stock.](beta-estimation_files/figure-html/fig-601-1.png)](beta-estimation_files/figure-html/fig-601-1.png "Figure 1: The CAPM betas are estimated with monthly data and a rolling window of length 5 years based on adjusted excess returns from CRSP. We use market excess returns from Kenneth French data library.")

Figure 1: The CAPM betas are estimated with monthly data and a rolling window of length 5 years based on adjusted excess returns from CRSP. We use market excess returns from Kenneth French data library.

## Parallelized Rolling-Window Estimation

Even though we could now just apply the function using the approach from above on the whole CRSP sample, we advise against doing it as it is computationally quite expensive. Remember that we have to perform rolling-window estimations across all stocks and time periods. However, this estimation problem is an ideal scenario to employ the power of parallelization. Parallelization means that we split the tasks which perform rolling-window estimations across different workers (or cores on your local machine).

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

    # A tibble: 4,665,570 × 5
      permno date       coefficient estimate t_statistic
       <dbl> <date>     <chr>          <dbl>       <dbl>
    1  10001 1990-01-01 alpha         0.0111       1.38 
    2  10001 1990-01-01 mkt_excess    0.0983       0.677
    3  10001 1990-02-01 alpha         0.0106       1.34 
    4  10001 1990-02-01 mkt_excess    0.0976       0.678
    5  10001 1990-03-01 alpha         0.0105       1.35 
    # ℹ 4,665,565 more rows

## Estimating Beta Using Daily Returns

Before we provide some descriptive statistics of our beta estimates, we implement the estimation for the daily CRSP sample as well. Depending on the application, you might either use longer horizon beta estimates based on monthly data or shorter horizon estimates based on daily returns. As loading the full daily CRSP data requires relatively large amounts of memory, we split the beta estimation into smaller chunks. The logic follows the approach that we use to download the daily CRSP data (see [WRDS, CRSP, and Compustat](../r/wrds-crsp-and-compustat.llms.md)).

First, we load the daily Fama-French market excess returns.

``` r
factors_ff3_daily <- read_parquet("data-r/factors_ff3_daily.parquet") |>
  select(date, mkt_excess)
```

We then create a connection to the daily CRSP data, but we don’t load the whole table into our memory. We only extract all distinct `permno` because we loop the beta estimation over batches of stocks with size 500. To estimate the CAPM over a consistent lookback window while accommodating different return frequencies, we adjust the minimum required number of observations accordingly. Specifically, we require at least 1,000 daily returns over a five‑year period for a valid estimation. This threshold is consistent with the monthly requirement of 48 observations out of 60 months, given that there are roughly 252 trading days in a year.

``` r
crsp_daily_db <- open_dataset("data-r/crsp_daily")

permnos <- crsp_daily_db |>
  distinct(permno) |>
  pull(permno, as_vector = TRUE)

batch_size <- 500
batches <- ceiling(length(permnos) / batch_size)
min_obs <- 1000
```

We then proceed to perform the same steps as with the monthly CRSP data, just in batches: Load in daily returns, nest the data by stock, and parallelize the beta estimation across stocks.

``` r
capm_daily <- list()

for (j in 1:batches) {
  permno_batch <- permnos[
    ((j - 1) * batch_size + 1):min(j * batch_size, length(permnos))
  ]

  crsp_daily_sub <- crsp_daily_db |>
    filter(permno %in% permno_batch) |>
    select(permno, date, ret_excess) |>
    collect()

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

## Comparing Beta Estimates

What is a typical value for stock betas? First, let us extract the relevant estimates from our CAPM results based on monthly returns.

``` r
beta_monthly <- capm_monthly |>
  filter(coefficient == "mkt_excess") |>
  select(permno, date, beta = estimate) |>
  mutate(return_type = "monthly")
```

To get some feeling, we illustrate the dispersion of the estimated \\\hat\beta_i\\ across different industries and across time below. [Figure 2](#fig-602) shows that typical business models across industries imply different exposure to the general market economy. However, there are barely any firms that exhibit a negative exposure to the market factor.

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

Next, we illustrate the time-variation in the cross-section of estimated betas. [Figure 3](#fig-603) shows the monthly deciles of estimated betas (based on monthly data) and indicates an interesting pattern: First, betas seem to vary over time in the sense that during some periods, there is a clear trend across all deciles. Second, the sample exhibits periods where the dispersion across stocks increases in the sense that the lower decile decreases and the upper decile increases, which indicates that for some stocks the correlation with the market increases while for others it decreases. Note also here: stocks with negative betas are a rare exception.

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

[![Title: Monthly deciles of estimated betas. The figure shows time series of deciles of estimated betas to illustrate the distribution of betas over time. The top 10 percent quantile on average is around 2 but varies substantially over time. The lowest 10 percent quantile is around 0.4 on average but is highly correlated with the top quantile such that in general CAPM market betas seem to go up and down jointly.](beta-estimation_files/figure-html/fig-603-1.png)](beta-estimation_files/figure-html/fig-603-1.png "Figure 3: Each line corresponds to the monthly cross-sectional quantile of the estimated CAPM beta.")

Figure 3: Each line corresponds to the monthly cross-sectional quantile of the estimated CAPM beta.

To compare the difference between daily and monthly data, we combine beta estimates to a single table.

``` r
beta_daily <- capm_daily |>
  filter(coefficient == "mkt_excess") |>
  select(permno, date, beta = estimate) |>
  mutate(return_type = "daily")

beta <- bind_rows(beta_monthly, beta_daily)
```

Then, we use the table to plot a comparison of beta estimates for our example stocks in [Figure 4](#fig-604).

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

[![Title: Comparison of beta estimates using monthly and daily data. The figure shows a time series of beta estimates using 5 years of monthly versus daily return data for Apple, Berkshire Hathaway, Microsoft, and Tesla. The estimates based on longer periods of monthly data are smooth relative to the estimates based on daily data. However, the general trend and level is similar, irrespective of the choice of frequency.](beta-estimation_files/figure-html/fig-604-1.png)](beta-estimation_files/figure-html/fig-604-1.png "Figure 4: CAPM betas are computed using 5 years of monthly or daily return data. The two lines show the monthly estimates based on a rolling window for few exemplary stocks.")

Figure 4: CAPM betas are computed using 5 years of monthly or daily return data. The two lines show the monthly estimates based on a rolling window for few exemplary stocks.

The estimates in [Figure 4](#fig-604) look as expected. As you can see, it really depends on the data frequency how your beta estimates turn out because the estimates based on daily data are much smoother due to the higher number of observations in each regression.

Finally, we write the estimates to our local folder such that we can use them in later chapters.

``` r
write_parquet(beta, "data-r/beta.parquet")
```

Whenever you perform some kind of estimation, it also makes sense to do rough plausibility tests. A possible check is to plot the share of stocks with beta estimates over time. This descriptive helps us discover potential errors in our data preparation or estimation procedure. For instance, suppose there was a gap in our output where we do not have any betas. In this case, we would have to go back and check all previous steps to find out what went wrong.

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

[![Title: End-of-month share of securities with beta estimates. The figure shows two time series with end-of-year shares of securities with beta estimates using 5 years of monthly or daily return data. For the beta estimates based on monthly data, around 75 percent of all stock-month combinations provide sufficient long historical periods to estimate the beta.](beta-estimation_files/figure-html/fig-605-1.png)](beta-estimation_files/figure-html/fig-605-1.png "Figure 5: The two lines show the share of securities with beta estimates using 5 years of monthly or daily return data.")

Figure 5: The two lines show the share of securities with beta estimates using 5 years of monthly or daily return data.

[Figure 5](#fig-605) shows no issues, as the two coverage lines track each other closely, so we can proceed to the next check.

We also encourage everyone to always look at the distributional summary statistics of variables. You can easily spot outliers or weird distributions when looking at such tables.

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
    1 daily       0.794 0.495  -3.67 0.0857 0.755  1.66  4.97 2354591
    2 monthly     1.11  0.714 -13.1  0.132  1.04   2.33 11.7  2332785

The summary statistics indicate that estimates based on daily returns are, on average, lower and less variable than those derived from monthly returns.

Finally, since we have two different estimators for the same theoretical object, we expect the estimators should be at least positively correlated (although not perfectly as the estimators are based on different frequencies).

``` r
beta |>
  pivot_wider(names_from = return_type, values_from = beta) |>
  select(monthly, daily) |>
  cor(use = "complete.obs")
```

            monthly daily
    monthly   1.000 0.618
    daily     0.618 1.000

Indeed, we find a positive correlation between our beta estimates. In the subsequent chapters, we mainly use the estimates based on monthly data, as most readers should be able to replicate them due to potential memory limitations that might arise with the daily data.

## Key Takeaways

- CAPM betas can be estimated using rolling-window estimation via the `slider` package and processed in parallel via `furrr`.
- Both monthly and daily return data can be used to estimate betas with different frequencies and window lengths, depending on the application.
- Summary statistics, visualization, and plausibility checks help to validate beta estimates across time and industries.

## Exercises

1.  Compute beta estimates based on monthly data using one, three, and five years of data and impose a minimum number of observations of 10, 28, and 48 months with return data, respectively. How strongly correlated are the estimated betas?
2.  Compute beta estimates based on monthly data using five years of data and impose different numbers of minimum observations. How does the share of `permno`-`date` observations with successful beta estimates vary across the different requirements? Do you find a high correlation across the estimated betas?
3.  Instead of using `future_map()`, perform the beta estimation in a loop (using either monthly or daily data) for a subset of 100 permnos of your choice. Verify that you get the same results as with the parallelized code from above.
4.  Filter out the stocks with negative betas. Do these stocks frequently exhibit negative betas, or do they resemble estimation errors?
5.  Compute beta estimates for multi-factor models such as the Fama-French three-factor model by extending the `estimate_capm()` function with a `model` parameter. In particular, your regression should support the model \\ r\_{i, t} - r\_{f, t} = \alpha_i + \sum\limits\_{j=1}^k\beta\_{i,k}(r\_{j, t}-r\_{f,t})+\varepsilon\_{i, t} \tag{1}\\ where \\r\_{i, t}\\ are the \\k\\ factor returns. Thus, you estimate four parameters (\\\alpha_i\\ and the slope coefficients). Provide some summary statistics of the cross-section of firms and their exposure to the different factors.

## References

Lintner, John. 1965. “Security prices, risk, and maximal gains from diversification.” *The Journal of Finance* 20 (4): 587–615. <https://doi.org/10.1111/j.1540-6261.1965.tb02930.x>.

Mossin, Jan. 1966. “Equilibrium in a capital asset market.” *Econometrica* 34 (4): 768–83. <https://doi.org/10.2307/1910098>.

Sharpe, William F. 1964. “Capital asset prices: A theory of market equilibrium under conditions of risk .” *The Journal of Finance* 19 (3): 425–42. <https://doi.org/10.1111/j.1540-6261.1964.tb02865.x>.

Vaughan, Davis. 2021. *slider: Sliding window functions*. <https://CRAN.R-project.org/package=slider>.

Vaughan, Davis, and Matt Dancho. 2022. *furrr: Apply mapping functions in parallel using futures*. <https://CRAN.R-project.org/package=furrr>.
