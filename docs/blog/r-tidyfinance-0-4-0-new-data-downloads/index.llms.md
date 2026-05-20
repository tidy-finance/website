We are happy to annouce the release of version 0.4.0 of the `tidyfinance` R package [on CRAN](https://CRAN.R-project.org/package=tidyfinance). The package contains a set of helper functions for empirical research in financial economics, addressing a variety of topics covered in [Tidy Finance with R](../../r/index.llms.md) (TFWR). We designed the package to provide easy shortcuts for the applications that we discuss in the book. If you want to inspect the details of the package or propose new features, feel free to visit the package repository on [Github](https://github.com/tidy-finance/r-tidyfinance).

As the new release brings many new features, we split them up into two blog posts. In this blog post, we discuss the new data downloads that `tidyfinance` now supports.

## Install the package

You can install the released version of `tidyfinance` from CRAN via:

``` r
install.packages("tidyfinance")
```

You can install the development version of tidyfinance from GitHub using the `pak` package:

``` r
pak::pak("tidy-finance/r-tidyfinance")
```

You then load the package via:

``` r
library(tidyfinance)
```

## Download stock data

You can download stock prices from [Yahoo Finance](https://finance.yahoo.com/) using the type `stock_prices` and provide symbols:

``` r
download_data("stock_prices", symbols = c("AAPL", "MSFT"))
```

    No `start_date` or `end_date` provided.
    Using the range 2022-08-30 to 2023-08-30 to avoid downloading large
    amounts of data.

    # A tibble: 502 × 8
      symbol date         volume  open   low  high close adjusted_close
      <chr>  <date>        <dbl> <dbl> <dbl> <dbl> <dbl>          <dbl>
    1 AAPL   2022-08-30 77906200  162.  158.  163.  159.           157.
    2 AAPL   2022-08-31 87991100  160.  157.  161.  157.           155.
    3 AAPL   2022-09-01 74229900  157.  155.  158.  158.           156.
    4 AAPL   2022-09-02 76957800  160.  155.  160.  156.           154.
    5 AAPL   2022-09-06 73714800  156.  154.  157.  155.           153.
    # ℹ 497 more rows

As you can see, we included defaults for `start_date` and `end_date` that you can of course overwrite. We introduced default dates across all datasets to allow for fast initial data analysis for your convenience.

``` r
download_data("stock_prices", symbols = c("AAPL", "MSFT"),
              start_date = "2020-01-01", end_date = "2020-12-31")
```

    # A tibble: 504 × 8
      symbol date          volume  open   low  high close adjusted_close
      <chr>  <date>         <dbl> <dbl> <dbl> <dbl> <dbl>          <dbl>
    1 AAPL   2020-01-02 135480400  74.1  73.8  75.2  75.1           72.9
    2 AAPL   2020-01-03 146322800  74.3  74.1  75.1  74.4           72.2
    3 AAPL   2020-01-06 118387200  73.4  73.2  75.0  74.9           72.7
    4 AAPL   2020-01-07 108872000  75.0  74.4  75.2  74.6           72.4
    5 AAPL   2020-01-08 132079200  74.3  74.3  76.1  75.8           73.6
    # ℹ 499 more rows

To inspect details about the download approach, you can call the documentation via `?download_data_stock_prices`.

The above function takes stock symbols as inputs (i.e., stock-specific identifiers). To get symbols you have now the following option: you can download index constituents from selected [iShares ETFs](https://www.ishares.com/us/products/etf-investments#/?productView=etf&pageNumber=1&sortColumn=totalNetAssets&sortDirection=desc&dataView=keyFacts) that physically replicate the corresponding index:

``` r
download_data("constituents", index = "DAX")
```

    # A tibble: 40 × 5
      symbol name                  location    exchange      currency
      <chr>  <chr>                 <chr>       <chr>         <chr>   
    1 SAP.DE SAP                   Deutschland Xetra         EUR     
    2 SIE.DE SIEMENS N AG          Deutschland Xetra         EUR     
    3 ALV.DE ALLIANZ               Deutschland Xetra         EUR     
    4 DTE.DE DEUTSCHE TELEKOM N AG Deutschland Xetra         EUR     
    5 AIR.BE AIRBUS                Frankreich  Boerse Berlin EUR     
    # ℹ 35 more rows

The documentation `?download_data_constituents` provides details. The list of supported indexes for `download_data_constituents()` can be inspected via:

``` r
list_supported_indexes()
```

    # A tibble: 13 × 3
      index                        url                               skip
      <chr>                        <chr>                            <dbl>
    1 DAX                          https://www.ishares.com/de/priv…     2
    2 EURO STOXX 50                https://www.ishares.com/de/priv…     2
    3 Dow Jones Industrial Average https://www.ishares.com/de/priv…     2
    4 Russell 1000                 https://www.ishares.com/ch/prof…     9
    5 Russell 2000                 https://www.ishares.com/ch/prof…     9
    # ℹ 8 more rows

You can easily use the index or constituents data to download stock prices:

``` r
constituents_dax <- download_data("constituents", index = "DAX")
download_data("stock_prices", symbols = constituents_dax$symbol)
```

## Download macro data

We also added support for the [Federal Reserve Economic Databa (FRED)](https://fred.stlouisfed.org/), where you can easily download multiple series (see `?download_data_fred` for details):

``` r
download_data("fred", series = c("GDP", "CPIAUCNS"))
```

    No `start_date` or `end_date` provided. Returning the full data set.

    # A tibble: 1,649 × 3
      date       value series
      <date>     <dbl> <chr> 
    1 1947-01-01  243. GDP   
    2 1947-04-01  246. GDP   
    3 1947-07-01  250. GDP   
    4 1947-10-01  260. GDP   
    5 1948-01-01  266. GDP   
    # ℹ 1,644 more rows

Our approach is a simple wrapper around the FRED download data site. If you want to systematically download FRED data via API, please consider using `fredr` ([Boysel and Vaughan 2021](#ref-fredr)) package.

The macro predictors by Ivo Welch and Amig Goyal, which we prepare for immediate use in our book chapter on [Accessing and Managing Financial Data](https://www.tidy-finance.org/r/accessing-and-managing-financial-data.html#macroeconomic-predictors), now also come in different flavors: monthly, quarterly, and annual. For instance, the annual data can be fetched via:

``` r
download_data("macro_predictors_annual")
```

    No `start_date` or `end_date` provided. Returning the full data set.

    # A tibble: 97 × 15
      date       rp_div    dp    dy    ep      de    svar    bm   ntis
      <date>      <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>  <dbl>
    1 1926-01-01  0.232 -2.97 -2.89 -2.39 -0.586  0.0165  0.441 0.0509
    2 1927-01-01  0.275 -3.13 -2.86 -2.77 -0.366  0.00942 0.375 0.0765
    3 1928-01-01 -0.163 -3.36 -3.03 -2.87 -0.485  0.0198  0.260 0.0631
    4 1929-01-01 -0.342 -3.10 -3.22 -2.59 -0.507  0.125   0.338 0.164 
    5 1930-01-01 -0.612 -2.75 -3.09 -2.76  0.0103 0.0666  0.555 0.114 
    # ℹ 92 more rows
    # ℹ 6 more variables: tbl <dbl>, lty <dbl>, ltr <dbl>, tms <dbl>,
    #   dfy <dbl>, infl <dbl>

See `?download_data_macro_predictors` for details.

Finally, we also added support to download a collection of replicated time series of factor portfolios from [Open Source Asset Pricing (OSAP)](https://www.openassetpricing.com/) compiled by Andrew Y. Chen and Tom Zimmermann:

``` r
download_data("osap")
```

    No `start_date` or `end_date` provided. Returning the full data set.

    # A tibble: 1,164 × 213
      date          am   aop abnormal_accruals accruals accruals_bm
      <date>     <dbl> <dbl>             <dbl>    <dbl>       <dbl>
    1 1926-01-30    NA    NA                NA       NA          NA
    2 1926-02-27    NA    NA                NA       NA          NA
    3 1926-03-31    NA    NA                NA       NA          NA
    4 1926-04-30    NA    NA                NA       NA          NA
    5 1926-05-28    NA    NA                NA       NA          NA
    # ℹ 1,159 more rows
    # ℹ 207 more variables: activism1 <dbl>, activism2 <dbl>,
    #   ad_exp <dbl>, age_ipo <dbl>, analyst_revision <dbl>,
    #   analyst_value <dbl>, announcement_return <dbl>,
    #   asset_growth <dbl>, bm <dbl>, bmdec <dbl>, bpebm <dbl>,
    #   beta <dbl>, beta_fp <dbl>, beta_liquidity_ps <dbl>,
    #   beta_tail_risk <dbl>, bid_ask_spread <dbl>, …

You can get more information by calling `?download_data_osap`.

## Concluding remarks

To get a list of supported data types, you can call:

``` r
list_supported_types()
```

    # A tibble: 317 × 3
      type                  dataset_name                   domain  
      <chr>                 <chr>                          <chr>   
    1 factors_q5_daily      q5_factors_daily_2023.csv      Global Q
    2 factors_q5_weekly     q5_factors_weekly_2023.csv     Global Q
    3 factors_q5_weekly_w2w q5_factors_weekly_w2w_2023.csv Global Q
    4 factors_q5_monthly    q5_factors_monthly_2023.csv    Global Q
    5 factors_q5_quarterly  q5_factors_quarterly_2023.csv  Global Q
    # ℹ 312 more rows

We are curious to learn in which direction we should extend the package, so please consider opening an issue in the [package repository](https://github.com/tidy-finance/r-tidyfinance/issues). For instance, we could support more data sources, add more parameters to the `download_*` family of functions, or we could put more emphasis on the generality of portfolio assignment or other modeling functions. Moreover, if you discover a bug, we are very grateful if you raise them in the repository.

## References

Boysel, Sam, and Davis Vaughan. 2021. *fredr: An R client for the ’FRED’ API*. <https://CRAN.R-project.org/package=fredr>.
