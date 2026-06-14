# Accessing and Managing Financial Data

> **NOTE:**
>
> You are reading **Tidy Finance with R**. You can find the equivalent chapter for the sibling **Tidy Finance with Python** [here](../python/accessing-and-managing-financial-data.llms.md).

In this chapter, we suggest a way to organize your financial data. Everybody who has experience with data is also familiar with storing data in various formats like CSV, XLS, XLSX, or other delimited value storage. Reading and saving data can become very cumbersome in the case of using different data formats, both across different projects and across different programming languages. Moreover, storing data in delimited files often leads to problems with respect to column type consistency. For instance, date-type columns frequently lead to inconsistencies across different data formats and programming languages.

This chapter shows how to import different open source data sets. Specifically, our data comes from the application programming interface (API) of Yahoo Finance, a downloaded standard CSV file, an XLSX file stored in a public Google Drive repository, and other macroeconomic time series that can be scraped directly from a website. We show how to process these raw data, as well as how to take a shortcut using the `tidyfinance` package, which provides a consistent interface to tidy financial data. We store all the data in a *single* database, which serves as the only source of data in subsequent chapters. We conclude the chapter by providing some tips on managing databases.

First, we load the global R packages that we use throughout this chapter. Later on, we load more packages in the sections where we need them.

``` r
library(tidyverse)
library(tidyfinance)
library(scales)
```

Moreover, we initially define the date range for which we fetch and store the financial data, making future data updates tractable. In case you need another time frame, you can adjust the dates below. Our data starts with 1960 since most asset pricing studies use data from 1962 on.

``` r
start_date <- ymd("1960-01-01")
end_date <- ymd("2024-12-31")
```

## Fama-French Data

We start by downloading some famous Fama-French factors (e.g., [Fama and French 1993](#ref-Fama1993)) and portfolio returns commonly used in empirical asset pricing. The data are freely available from [Prof. Kenneth French’s finance data library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html), but the raw files come in a rather idiosyncratic format. If you access the data through the website, the manual *raw* workflow looks like this:

1.  Open the data library website.
2.  Find the right dataset.
3.  Download a ZIP file.
4.  Extract the CSV inside.
5.  Select the relevant data table from the file and import it into R.
6.  Clean the dates, scale the returns, fix the column names, and handle missing values.

Doing this once is fine; doing it repeatedly across projects is exactly the kind of boilerplate that is easy to get wrong and annoying to maintain. It is therefore natural to automate these steps in R. In fact, this is precisely what the `tidyfinance` package does under the hood. In this section, we first reproduce the source code behind the package so that you understand what happens when you download Fama-French data, and then we show the convenient shortcut.

### From manual steps to a download script

A minimal download script mirrors the manual steps one by one. To fetch a Fama-French dataset, we first construct the URL of the corresponding ZIP archive on Kenneth French’s server. Each dataset is identified by the file name of its archive; the *Fama/French 3 Factors* set, for instance, lives in `F-F_Research_Data_Factors_CSV.zip`.

``` r
base_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file_name <- "F-F_Research_Data_Factors_CSV.zip"
url <- paste0(base_url, file_name)
```

Next, we replace the browser download with an HTTP request using the `httr2` ([Wickham 2024](#ref-httr2)) package and store the archive in a temporary file. We attach a browser-like user agent because Kenneth French’s server rejects requests that do not provide one.

``` r
library(httr2)

tmp_zip <- tempfile(fileext = ".zip")

request(url) |>
  req_user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  ) |>
  req_perform(path = tmp_zip)
```

The archive contains a single CSV file, which we extract into a temporary directory and read line by line.

``` r
tmp_dir <- tempfile()
dir.create(tmp_dir)
csv_file <- unzip(tmp_zip, exdir = tmp_dir)[1]
raw_lines <- readLines(csv_file, warn = FALSE)
```

The raw file starts with several lines of descriptive text, followed by one or more data tables (e.g., monthly returns and a block of annual summary rows). To emulate *scrolling down until the numbers start*, we flag every line that begins with a date key—an optional run of spaces followed by a digit—and keep the first contiguous block of such lines. The line directly above that block holds the column names.

``` r
is_data <- grepl("^\\s*[0-9]", raw_lines)
runs <- rle(is_data)
first_run <- which(runs$values)[1]
preceding <- sum(runs$lengths[seq_len(first_run - 1)])
first_data <- preceding + 1
last_data <- preceding + runs$lengths[first_run]
block <- raw_lines[c(first_data - 1, first_data:last_data)]
```

We hand this block to `read.csv()` and label the first (unnamed) column `date`.

``` r
factors_ff3_monthly_raw <- read.csv(text = block, check.names = FALSE)
names(factors_ff3_monthly_raw)[1] <- "date"
factors_ff3_monthly_raw <- as_tibble(factors_ff3_monthly_raw)
factors_ff3_monthly_raw
```

    # A tibble: 1,198 × 5
        date `Mkt-RF`   SMB   HML    RF
       <int>    <dbl> <dbl> <dbl> <dbl>
    1 192607     2.89 -2.55 -2.39  0.22
    2 192608     2.64 -1.14  3.81  0.25
    3 192609     0.38 -1.36  0.05  0.23
    4 192610    -3.27 -0.14  0.82  0.32
    5 192611     2.54 -0.11 -0.61  0.31
    # ℹ 1,193 more rows

Finally, we still have to clean the data. The set *Fama/French 3 Factors* contains the return time series of the market `mkt_excess`, size `smb`, and value `hml` factors alongside the risk-free rate `risk_free`. Cleaning involves a handful of steps:

- Parse the integer date keys (e.g., `192607`) into proper dates.
- Replace Kenneth French’s missing-value codes (`-99.99` and `-999`) with `NA`.
- Convert the returns from percent to decimals by dividing by 100.
- Standardize the column names (lower case, `Mkt-RF` to `mkt_excess`, and `RF` to `risk_free`).
- Filter the data to the desired date range.

``` r
factors_ff3_monthly <- factors_ff3_monthly_raw |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(-date, as.numeric),
    across(-date, ~ na_if(., -99.99)),
    across(-date, ~ na_if(., -999)),
    across(-date, ~ . / 100)
  ) |>
  rename_with(str_to_lower, -date) |>
  rename(mkt_excess = `mkt-rf`, risk_free = rf) |>
  filter(date >= start_date & date <= end_date)
factors_ff3_monthly
```

    # A tibble: 780 × 5
      date       mkt_excess     smb     hml risk_free
      <date>          <dbl>   <dbl>   <dbl>     <dbl>
    1 1960-01-01    -0.0698  0.0212  0.0265    0.0033
    2 1960-02-01     0.0116  0.006  -0.0197    0.0029
    3 1960-03-01    -0.0163 -0.0055 -0.0275    0.0035
    4 1960-04-01    -0.0171  0.0022 -0.0214    0.0019
    5 1960-05-01     0.0312  0.0129 -0.0373    0.0027
    # ℹ 775 more rows

All of these steps are doable, but none of them are really about finance—they are just the technical scaffolding required before you can work with the actual factor returns. That is where a dedicated package becomes invaluable.

### Using the tidyfinance shortcut

The `tidyfinance` package performs the entire workflow above under the hood: you request a Fama-French dataset by name and receive a clean, consistently formatted tibble from Kenneth French’s data library. This avoids repetitive boilerplate, reduces errors, and lets you focus on modeling rather than data plumbing. For precise descriptions of the variables, we suggest consulting Prof. Kenneth French’s finance data library directly. We can use the `download_data()` function to download the same monthly *Fama/French 3 Factors* as above.

``` r
factors_ff3_monthly <- download_data(
  domain = "famafrench",
  dataset = "Fama/French 3 Factors",
  start_date = start_date,
  end_date = end_date
)
```

We also download the set *5 Factors (2x3)*, which additionally includes the return time series of the profitability `rmw` and investment `cma` factors. We demonstrate how the monthly factors are constructed in the chapter [Replicating Fama and French Factors](../r/replicating-fama-and-french-factors.llms.md).

``` r
factors_ff5_monthly <- download_data(
  domain = "famafrench",
  dataset = "Fama/French 5 Factors (2x3)",
  start_date = start_date,
  end_date = end_date
)
```

It is straightforward to download the corresponding *daily* Fama-French factors with the same function.

``` r
factors_ff3_daily <- download_data(
  domain = "famafrench",
  dataset = "Fama/French 3 Factors [Daily]",
  start_date = start_date,
  end_date = end_date
)
```

In a subsequent chapter, we also use the monthly returns of 10 industry portfolios, so let us fetch that data, too.

``` r
industries_ff_monthly <- download_data(
  domain = "famafrench",
  dataset = "10 Industry Portfolios",
  start_date = start_date,
  end_date = end_date
)
```

It is worth taking a look at all available portfolio return time series from Kenneth French’s homepage. You can list all the Fama-French datasets that `tidyfinance` supports as follows:

The `tidyfinance` package implements the processing steps as above and returns the same cleaned data frame. The list of supported Fama-French datasets can be called as follows:

``` r
list_supported_datasets(domain = "Fama-French")
```

## q-Factors

In recent years, the academic discourse experienced the rise of alternative factor models, e.g., in the form of the Hou et al. ([2014](#ref-Hou2015)) *q*-factor model. We refer to the [extended background](http://global-q.org/background.html) information provided by the original authors for further information. The *q* factors can be downloaded directly from the authors’ homepage from within `read_csv()`.

We also need to adjust this data. First, we discard information we will not use in the remainder of the book. Then, we rename the columns with the “R\_”-prescript using regular expressions and write all column names in lowercase. You should always try sticking to a consistent style for naming objects, which we try to illustrate here - the emphasis is on *try*. You can check out style guides available online, e.g., [Hadley Wickham’s `tidyverse` style guide.](https://style.tidyverse.org/index.html)

``` r
factors_q_monthly_link <-
  "https://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2024.csv"

factors_q_monthly <- read_csv(factors_q_monthly_link) |>
  mutate(date = ymd(str_c(year, month, "01", sep = "-"))) |>
  select(-year, -month) |>
  rename_with(~ str_remove(., "R_")) |>
  rename_with(str_to_lower) |>
  mutate(across(-date, ~ . / 100)) |>
  select(date, risk_free = f, mkt_excess = mkt, everything()) |>
  filter(date >= start_date & date <= end_date)
```

Again, you can use the `tidyfinance` package for a shortcut:

``` r
download_data(
  domain = "factors_q",
  dataset = "q5_factors_monthly_2024",
  start_date = start_date,
  end_date = end_date
)
```

## Macroeconomic Predictors

Our next data source is a set of macroeconomic variables often used as predictors for the equity premium. Welch and Goyal ([2008](#ref-Goyal2008)) comprehensively reexamine the performance of variables suggested by the academic literature to be good predictors of the equity premium. The authors host the data updated to 2022 on [Amit Goyal’s website.](https://sites.google.com/view/agoyal145) The data is an XLSX-file stored on a public Google drive location and we directly export a CSV file.

``` r
sheet_id <- "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
sheet_name <- "Monthly"
macro_predictors_url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  sheet_id,
  "/gviz/tq?tqx=out:csv&sheet=",
  sheet_name
)
macro_predictors_raw <- read_csv(macro_predictors_url)
```

Next, we transform the columns into the variables that we later use:

1.  The dividend price ratio (`dp`), the difference between the log of dividends and the log of prices, where dividends are 12-month moving sums of dividends paid on the S&P 500 index, and prices are monthly averages of daily closing prices ([Campbell and Shiller 1988](#ref-Campbell1988); [Campbell and Yogo 2006](#ref-Campbell2006)).
2.  Dividend yield (`dy`), the difference between the log of dividends and the log of lagged prices ([Ball 1978](#ref-Ball1978)).
3.  Earnings price ratio (`ep`), the difference between the log of earnings and the log of prices, where earnings are 12-month moving sums of earnings on the S&P 500 index ([Campbell and Shiller 1988](#ref-Campbell1988)).
4.  Dividend payout ratio (`de`), the difference between the log of dividends and the log of earnings ([Lamont 1998](#ref-Lamont1998)).
5.  Stock variance (`svar`), the sum of squared daily returns on the S&P 500 index ([Guo 2006](#ref-Guo2006)).
6.  Book-to-market ratio (`bm`), the ratio of book value to market value for the Dow Jones Industrial Average ([Kothari and Shanken 1997](#ref-Kothari1997)).
7.  Net equity expansion (`ntis`), the ratio of 12-month moving sums of net issues by NYSE listed stocks divided by the total end-of-year market capitalization of NYSE stocks ([Campbell et al. 2008](#ref-Campbell2008)).
8.  Treasury bills (`tbl`), the 3-Month Treasury Bill: Secondary Market Rate from the economic research database at the Federal Reserve Bank at St. Louis ([Campbell 1987](#ref-Campbell1987)).
9.  Long-term yield (`lty`), the long-term government bond yield from Ibbotson’s Stocks, Bonds, Bills, and Inflation Yearbook ([Welch and Goyal 2008](#ref-Goyal2008)).
10. Long-term rate of returns (`ltr`), the long-term government bond returns from Ibbotson’s Stocks, Bonds, Bills, and Inflation Yearbook ([Welch and Goyal 2008](#ref-Goyal2008)).
11. Term spread (`tms`), the difference between the long-term yield on government bonds and the Treasury bill ([Campbell 1987](#ref-Campbell1987)).
12. Default yield spread (`dfy`), the difference between BAA and AAA-rated corporate bond yields ([Fama and French 1989](#ref-Fama1989)).
13. Inflation (`infl`), the Consumer Price Index (All Urban Consumers) from the Bureau of Labor Statistics ([Campbell and Vuolteenaho 2004](#ref-Campbell2004)).

For variable definitions and the required data transformations, you can consult the material on [Amit Goyal’s website](https://sites.google.com/view/agoyal145).

``` r
macro_predictors <- macro_predictors_raw |>
  mutate(date = ym(yyyymm)) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(
    IndexDiv = Index + D12,
    logret = log(IndexDiv) - log(lag(IndexDiv)),
    Rfree = log(Rfree + 1),
    rp_div = lead(logret - Rfree, 1), # Future excess market return
    dp = log(D12) - log(Index), # Dividend Price ratio
    dy = log(D12) - log(lag(Index)), # Dividend yield
    ep = log(E12) - log(Index), # Earnings price ratio
    de = log(D12) - log(E12), # Dividend payout ratio
    tms = lty - tbl, # Term spread
    dfy = BAA - AAA # Default yield spread
  ) |>
  select(
    date,
    rp_div,
    dp,
    dy,
    ep,
    de,
    svar,
    bm = `b/m`,
    ntis,
    tbl,
    lty,
    ltr,
    tms,
    dfy,
    infl
  ) |>
  filter(date >= start_date & date <= end_date) |>
  drop_na()
```

To get the equivalent data through `tidyfinance`, you can call:

``` r
download_data(
  domain = "macro_predictors",
  dataset = "monthly",
  start_date = start_date,
  end_date = end_date
)
```

## Other Macroeconomic Data

The Federal Reserve bank of St. Louis provides the Federal Reserve Economic Data (FRED), an extensive database for macroeconomic data. In total, there are 817,000 US and international time series from 108 different sources. The data can be downloaded directly from FRED by constructing the appropriate URL. For instance, let us consider the consumer price index (CPI) data that can be found under the [CPIAUCNS](https://fred.stlouisfed.org/series/CPIAUCNS):

``` r
series <- "CPIAUCNS"
cpi_url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
  series
)
```

We can then use the `httr2` ([Wickham 2024](#ref-httr2)) package to request the CSV, extract the data from the response body, and convert the columns to a tidy format:

``` r
library(httr2)

resp <- request(cpi_url) |>
  req_perform()
resp_csv <- resp |>
  resp_body_string()

cpi_monthly <- resp_csv |>
  read_csv() |>
  mutate(
    date = as.Date(observation_date),
    value = as.numeric(.data[[series]]),
    series = series,
    .keep = "none"
  ) |>
  filter(date >= start_date & date <= end_date) |>
  mutate(
    cpi = value / value[date == max(date)]
  ) |>
  select(date, series, value, cpi)
```

The last line sets the current (latest) price level as the reference price level.

The `tidyfinance` package can, of course, also fetch the same index data and many more data series:

``` r
download_data(
  domain = "fred",
  series = "CPIAUCNS",
  start_date = start_date,
  end_date = end_date
)
```

    # A tibble: 780 × 3
      date       value series  
      <date>     <dbl> <chr>   
    1 1960-01-01  29.3 CPIAUCNS
    2 1960-02-01  29.4 CPIAUCNS
    3 1960-03-01  29.4 CPIAUCNS
    4 1960-04-01  29.5 CPIAUCNS
    5 1960-05-01  29.5 CPIAUCNS
    # ℹ 775 more rows

To download other time series, we just have to look it up on the FRED website and extract the corresponding key from the address. For instance, the producer price index for gold ores can be found under the [PCU2122212122210](https://fred.stlouisfed.org/series/PCU2122212122210) key. If your desired time series is not supported through `tidyfinance`, we recommend working with the `fredr` package ([Boysel and Vaughan 2021](#ref-fredr)). Note that you need to get an API key to use its functionality. We refer to the package documentation for details.

## Setting Up a Database

Now that we have downloaded some (freely available) data from the web into the memory of our R session, let us store that information for future use. We will use the data stored throughout the following chapters, but you could alternatively implement a different strategy and replace the respective code.

There are many ways to set up and organize your data, depending on the use case. Storing data as Parquet files creates a universal data format that seamlessly works across different programming languages and platforms. Unlike CSV files that often lose data types when shared between R, Python, Julia, or other languages, Parquet files maintain perfect data integrity regardless of which tool reads them. A dataset saved as Parquet in R can be opened directly in Python’s pandas, or Julia’s DataFrames.jl.[^1]

``` r
library(arrow)
```


    Attaching package: 'arrow'

    The following object is masked from 'package:lubridate':

        duration

    The following object is masked from 'package:utils':

        timestamp

Parquet files are easily created - the code below is really all there is. You do not need any external software. We will store everything in the data subfolder, to retrieve data for all subsequent chapters. The initial part of the code ensures that the directory is created if it does not already exist.

``` r
if (!dir.exists("data-r")) {
  dir.create("data-r")
}
```

Next, we create a file with the monthly Fama-French factor data. We do so with the function `write_parquet`.

``` r
write_parquet(factors_ff3_monthly, "data-r/factors_ff3_monthly.parquet")
```

You can retrieve the data directly in your memory by calling `read_parquet()`. It is also possible to evaluate `dplyr` calls lazily, i.e., the data is not in our R session’s memory, and the database does most of the work.

``` r
read_parquet("data-r/factors_ff3_monthly.parquet") |>
  select(date, risk_free) |>
  head(10)
```

    # A tibble: 10 × 2
      date       risk_free
      <date>         <dbl>
    1 1960-01-01    0.0033
    2 1960-02-01    0.0029
    3 1960-03-01    0.0035
    4 1960-04-01    0.0019
    5 1960-05-01    0.0027
    # ℹ 5 more rows

Before we move on to the next data source, let us also store the other five tables in our new data folder.

``` r
lst(
  factors_ff5_monthly,
  factors_ff3_daily,
  industries_ff_monthly,
  factors_q_monthly,
  macro_predictors,
  cpi_monthly
) |>
  iwalk(\(data, name) write_parquet(data, paste0("data-r/", name, ".parquet")))
```

## Key Takeaways

- Importing Fama-French factors, q-factors, macroeconomic indicators, and CPI data is simplified through API calls, CSV parsing, and web scraping techniques.
- The `tidyfinance` R package offers pre-processed access to financial datasets, reducing manual data cleaning and saving valuable time.
- Creating `parquet` files helps manage and organize data efficiently across projects, while maintaining reproducibility.
- Structured database storage supports scalable data access, which is essential for long-term academic projects and collaborative work in finance.

## Exercises

1.  Download the monthly Fama-French factors manually from [Ken French’s data library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html) and read them in via `read_csv()`. Validate that you get the same data as via the download script we developed above.
2.  Download the daily Fama-French 5 factors using `download_data()`. Use `list_supported_datasets(domain = "Fama-French")` to find the corresponding dataset name. After the successful download, compare the `risk_free`, `mkt_excess`, `smb`, and `hml` columns of `factors_ff3_daily` to `factors_ff5_daily`. Discuss any differences you might find.

## References

Ball, Ray. 1978. “Anomalies in relationships between securities’ yields and yield-surrogates.” *Journal of Financial Economics* 6 (2–3): 103–26. <https://doi.org/10.1016/0304-405X(78)90026-0>.

Boysel, Sam, and Davis Vaughan. 2021. *fredr: An R client for the ’FRED’ API*. <https://CRAN.R-project.org/package=fredr>.

Campbell, John Y. 1987. “Stock returns and the term structure.” *Journal of Financial Economics* 18 (2): 373–99. <https://doi.org/10.1016/0304-405X(87)90045-6>.

Campbell, John Y., Jens Hilscher, and Jan Szilagyi. 2008. “In search of distress risk.” *The Journal of Finance* 63 (6): 2899–939. <https://doi.org/10.1111/j.1540-6261.2008.01416.x>.

Campbell, John Y., and Robert J. Shiller. 1988. “Stock prices, earnings, and expected dividends.” *The Journal of Finance* 43 (3): 661–76. <https://doi.org/10.1111/j.1540-6261.1988.tb04598.x>.

Campbell, John Y., and Tuomo Vuolteenaho. 2004. “Inflation illusion and stock prices.” *American Economic Review* 94 (2): 19–23. <https://www.aeaweb.org/articles?id=10.1257/0002828041301533>.

Campbell, John Y., and Motohiro Yogo. 2006. “Efficient tests of stock return predictability.” *Journal of Financial Economics* 81 (1): 27–60. <https://doi.org/10.1016/j.jfineco.2005.05.008>.

Fama, Eugene F., and Kenneth R. French. 1989. “Business conditions and expected returns on stocks and bonds.” *Journal of Financial Economics* 25 (1): 23–49. <https://doi.org/10.1016/0304-405X(89)90095-0>.

Fama, Eugene F., and Kenneth R. French. 1993. “Common risk factors in the returns on stocks and bonds.” *Journal of Financial Economics* 33 (1): 3–56. <https://doi.org/10.1016/0304-405X(93)90023-5>.

Guo, Hui. 2006. “On the out-of-sample predictability of stock market returns.” *The Journal of Business* 79 (2): 645–70. <https://doi.org/10.1086/499134>.

Hou, Kewei, Chen Xue, and Lu Zhang. 2014. “Digesting anomalies: An investment approach.” *Review of Financial Studies* 28 (3): 650–705. <https://doi.org/10.1093/rfs/hhu068>.

Kothari, S. P., and Jay A. Shanken. 1997. “Book-to-market, dividend yield, and expected market returns: A time-series analysis.” *Journal of Financial Economics* 44 (2): 169–203. <https://doi.org/10.1016/S0304-405X(97)00002-0>.

Lamont, Owen. 1998. “Earnings and expected returns.” *The Journal of Finance* 53 (5): 1563–87. <https://doi.org/10.1111/0022-1082.00065>.

Welch, Ivo, and Amit Goyal. 2008. “A comprehensive look at the empirical performance of equity premium prediction.” *Review of Financial Studies* 21 (4): 1455–508. <https://doi.org/10.1093/rfs/hhm014>.

Wickham, Hadley. 2024. *Httr2: Perform HTTP Requests and Process the Responses*. <https://httr2.r-lib.org>.

## Footnotes

[^1]: In previous editions of this book, we suggested using SQLite databases. However, Parquet files are more versatile and easier to handle across different programming languages. Should you still prefer using SQLite, you can find the relevant code snippets in a separate blog post.
