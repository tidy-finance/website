We are happy to annouce the initial release of the `tidyfinance` R package [on CRAN](https://cran.r-project.org/web/packages/tidyfinance/index.html). The package contains a set of helper functions for empirical research in financial economics, addressing a variety of topics covered in [Tidy Finance with R](../../r/index.qmd) (TFWR). We designed the package to provide easy shortcuts for the applications that we discuss in the book. If you want to inspect the details of the package or propose new features, feel free to visit the package repository on [Github](https://github.com/tidy-finance/r-tidyfinance).

In this blog post, we demonstrate the features of the initial release. We decided to focus on functions that allow you to download the data that we use in TFWR.

## Install the package

You can install the released version of `tidyfinance` from CRAN via:

``` r
install.packages("tidyfinance")
```

You can install the development version of tidyfinance from GitHub using the `pak` package:

``` r
pak::pak("tidy-finance/r-tidyfinance")
```

## Download data

Let’s start by loading the package

``` r
library(tidyfinance)
```

The main function is `download_data(type, start_date, end_date)` with supported type:

``` r
list_supported_types()
```

    # A tibble: 20 × 3
      type                  dataset_name                   domain  
      <chr>                 <chr>                          <chr>   
    1 factors_q5_daily      q5_factors_daily_2022.csv      Global Q
    2 factors_q5_weekly     q5_factors_weekly_2022.csv     Global Q
    3 factors_q5_weekly_w2w q5_factors_weekly_w2w_2022.csv Global Q
    4 factors_q5_monthly    q5_factors_monthly_2022.csv    Global Q
    5 factors_q5_quarterly  q5_factors_quarterly_2022.csv  Global Q
    # ℹ 15 more rows

So, for instance, if you want to download monthly Fama-French Three-Factor data, you can call:

``` r
download_data("factors_ff3_monthly", "2020-01-01", "2020-12-31")
```

    # A tibble: 12 × 5
      date       risk_free mkt_excess     smb     hml
      <date>         <dbl>      <dbl>   <dbl>   <dbl>
    1 2020-01-01    0.0013    -0.0011 -0.0311 -0.0625
    2 2020-02-01    0.0012    -0.0813  0.0107 -0.0381
    3 2020-03-01    0.0013    -0.134  -0.0483 -0.139 
    4 2020-04-01    0          0.136   0.0245 -0.0133
    5 2020-05-01    0.0001     0.0558  0.0247 -0.0488
    # ℹ 7 more rows

Under the hood, the function uses the `frenchdata` package (see its documentation [here](https://cran.r-project.org/web/packages/frenchdata/index.html)) and applies some cleaning steps, as in TFWR. If you haven’t installed `frenchdata` yet, you’ll get prompted to install it first before you can download this specific data type.

You can also access q-Factor data in this way, by calling:

``` r
download_data("factors_q5_daily", "2020-01-01", "2020-12-31")
```

    # A tibble: 253 × 7
      date       risk_free mkt_excess       me       ia      roe       eg
      <date>         <dbl>      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    1 2020-01-02  0.000055    0.00863 -0.0112  -0.00171  6.84e-4  3.41e-3
    2 2020-01-03  0.000055   -0.00673  0.00234 -0.00193 -1.55e-3  6.83e-4
    3 2020-01-06  0.000055    0.00360 -0.00360 -0.00409 -4.78e-3  6.11e-4
    4 2020-01-07  0.000055   -0.00192 -0.00139 -0.00322 -5.12e-3 -2.74e-3
    5 2020-01-08  0.000055    0.00467 -0.00108 -0.00121  4.56e-3  6.14e-3
    # ℹ 248 more rows

To ensure that we can extend the functionality of the download functions for specific types, we also provide domain-specific download functions. The `download_data("factors_ff3_monthly")` actually calls `download_data_factors("factors_ff3_monthly")`, which in turn calls `download_data_factors_ff("factors_ff3_monthly")`. Why did we decide to have these nested function approach?

Suppose that the q-Factor data changes its URL path and our original function does not work anymore. In this case, you can replace the default `url` value in `download_data_factors_q(type, start_date, end_date, url)` to apply the usual cleaning steps.

This feature becomes more apparent for other data sources such as `wrds_crsp_monthly`. Note that you need to have valid WRDS credentials and need to set them correctly (check `?get_wrds_connection` and [WRDS, CRSP, and Compustat](../../r/wrds-crsp-and-compustat) in TFWR). If you want to download the standard monthly CRSP data, you can call:

``` r
download_data("wrds_crsp_monthly", "2020-01-01", "2020-12-31")
```

If you want to add further columns, you can add them via `...` to `download_data_wrds_crsp()`, for instance:

``` r
download_data_wrds_crsp("wrds_crsp_monthly", "2020-01-01", "2020-12-31", mthvol)
```

Note that the function downloads CRSP v2 as default, as we do in our book since February 2024. If you want to download the old version of CRSP before the update, you can use the `version = v1` parameter in `download_data_wrds_crsp()` .

As another example, you can do the same for Compustat:

``` r
download_data_wrds_compustat("wrds_compustat_annual", "2000-01-01", "2020-12-31", acoxar, amc, aldo)
```

Check out the list of supported types and the corresponding download functions for more information on the respective customization options. We decided to provide limited functionality for the initial release on purpose and rather respond to community request than overengineer the package from the start.

## Browse content from TFWR

We include functions to check out content from TFWR in your browser. If you want to list all available R chapters, simply call the following function:

``` r
list_tidy_finance_chapters()
```

     [1] "setting-up-your-environment"                
     [2] "introduction-to-tidy-finance"               
     [3] "accessing-and-managing-financial-data"      
     [4] "wrds-crsp-and-compustat"                    
     [5] "trace-and-fisd"                             
     [6] "other-data-providers"                       
     [7] "beta-estimation"                            
     [8] "univariate-portfolio-sorts"                 
     [9] "size-sorts-and-p-hacking"                   
    [10] "value-and-bivariate-sorts"                  
    [11] "replicating-fama-and-french-factors"        
    [12] "fama-macbeth-regressions"                   
    [13] "fixed-effects-and-clustered-standard-errors"
    [14] "difference-in-differences"                  
    [15] "factor-selection-via-machine-learning"      
    [16] "option-pricing-via-machine-learning"        
    [17] "parametric-portfolio-policies"              
    [18] "constrained-optimization-and-backtesting"   
    [19] "wrds-dummy-data"                            
    [20] "cover-and-logo-design"                      
    [21] "clean-enhanced-trace-with-r"                
    [22] "proofs"                                     
    [23] "hex-sticker"                                
    [24] "changelog"                                  

The function returns a character vector containing the names of the chapters available in TFWR. If you want to look at a specific chapter, you can call:

``` r
open_tidy_finance_website("beta-estimation")
```

This opens either the specific chapter you requested or the main index page in your default web browser.

## Regression helpers

We discuss winsorization in TFWR, so we figured providing this function could be useful:

``` r
library(tibble)
library(dplyr)

set.seed(123)
data <- tibble(x = rnorm(100)) |> 
  arrange(x)

data |> 
  mutate(x_winsorized = winsorize(x, 0.01))
```

    # A tibble: 100 × 2
          x x_winsorized
      <dbl>        <dbl>
    1 -2.31        -1.97
    2 -1.97        -1.97
    3 -1.69        -1.69
    4 -1.55        -1.55
    5 -1.27        -1.27
    # ℹ 95 more rows

If you rather want to replace the bottom and top quantiles of your distribution with missing values, then you can use `trim()`

``` r
data |> 
  mutate(x_trimmed = trim(x, 0.01))
```

    # A tibble: 100 × 2
          x x_trimmed
      <dbl>     <dbl>
    1 -2.31     NA   
    2 -1.97     -1.97
    3 -1.69     -1.69
    4 -1.55     -1.55
    5 -1.27     -1.27
    # ℹ 95 more rows

We also discuss the importance of providing summary statistics of your data, so there is also a function for that:

``` r
create_summary_statistics(data, x, detail = TRUE)
```

    # A tibble: 1 × 15
      variable     n   mean    sd   min   q01   q05   q10    q25    q50
      <chr>    <int>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
    1 x          100 0.0904 0.913 -2.31 -1.97 -1.27 -1.07 -0.494 0.0618
    # ℹ 5 more variables: q75 <dbl>, q90 <dbl>, q95 <dbl>, q99 <dbl>,
    #   max <dbl>

## Experimental functions

We have two more experimental functions in the sense that it is unclear in which direction they might evolve. First you can assign portfolios based on a sorting variable using `assign_portfolio()`:

``` r
data <- tibble(
  id = 1:100,
  exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
  market_cap = runif(100, 1e6, 1e9)
)

data |> 
  mutate(
    portfolio = assign_portfolio(
      pick(everything()), "market_cap", n_portfolios = 5, exchanges = c("NYSE"))
  )
```

    # A tibble: 100 × 4
         id exchange market_cap portfolio
      <int> <chr>         <dbl>     <int>
    1     1 NASDAQ   784790691.         4
    2     2 NASDAQ    10420475.         1
    3     3 NASDAQ   779286817.         4
    4     4 NYSE     729661261.         4
    5     5 NASDAQ   630501721.         3
    # ℹ 95 more rows

Second, you can estimate the coefficients of a linear model specified by one or more independent variable using `estimate_model()`:

``` r
data <- tibble(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")
```

      mkt_excess     smb    hml
    1    -0.0399 -0.0287 0.0207

## Concluding remarks

We are curious to learn in which direction we should extend the package, so please consider opening an issue in the [package repository](https://github.com/tidy-finance/r-tidyfinance/issues). For instance, we could support more data sources, add more parameters to the `download_*` family of functions, or we could put more emphasis on the generality of portfolio assignment or other modeling functions.
