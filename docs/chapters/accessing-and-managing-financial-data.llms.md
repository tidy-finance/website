# Accessing and Managing Financial Data

In this chapter, we suggest a way to organize your financial data. Everybody who has experience with data is also familiar with storing data in various formats like CSV, XLS, XLSX, or other delimited value storage. Reading and saving data can become very cumbersome in the case of using different data formats, both across different projects and across different programming languages. Moreover, storing data in delimited files often leads to problems with respect to column type consistency. For instance, date-type columns frequently lead to inconsistencies across different data formats and programming languages.

This chapter shows how to import different open source data sets. Specifically, our data comes from the application programming interface (API) of Yahoo Finance, a downloaded standard CSV file, an XLSX file stored in a public Google Drive repository, and other macroeconomic time series that can be scraped directly from a website. We show how to process these raw data, as well as how to take a shortcut using the `tidyfinance` package, which provides a consistent interface to tidy financial data. We store all the data in a *single* database, which serves as the only source of data in subsequent chapters. We conclude the chapter by providing some tips on managing databases.

We use the following packages throughout this chapter. Later on, we load more packages in the sections where we need them.

## R

``` r
library(tidyverse)
library(tidyfinance)
library(scales)
```

## Python

``` python
import polars as pl
import tidyfinance as tf
import io
import re
import zipfile
from curl_cffi import requests

tf.set_backend("polars")
```

Moreover, we initially define the date range for which we fetch and store the financial data, making future data updates tractable. In case you need another time frame, you can adjust the dates below. Our data starts with 1960 since most asset pricing studies use data from 1962 on.

## R

``` r
start_date <- ymd("1960-01-01")
end_date <- ymd("2024-12-31")
```

## Python

``` python
start_date = "1960-01-01"
end_date = "2024-12-31"
```

## Fama-French Data

We start by downloading some famous Fama-French factors (e.g., [Fama and French 1993](#ref-Fama1993)) and portfolio returns commonly used in empirical asset pricing. The data are freely available from [Prof. Kenneth French’s finance data library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html), but the raw files come in a rather idiosyncratic format. If you access the data through the website, the manual *raw* workflow looks like this:

1.  Open the data library website.
2.  Find the right dataset.
3.  Download a ZIP file.
4.  Extract the CSV inside.
5.  Select the relevant data table from the file and import it.
6.  Clean the dates, scale the returns, fix the column names, and handle missing values.

Doing this once is fine; doing it repeatedly across projects is exactly the kind of boilerplate that is easy to get wrong and annoying to maintain. It is therefore natural to automate these steps. In fact, this is precisely what the `tidyfinance` package does under the hood. In this section, we first reproduce the source code behind the package so that you understand what happens when you download Fama-French data, and then we show the convenient shortcut.

### From manual steps to a download script

A minimal download script mirrors the manual steps one by one. To fetch a Fama-French dataset, we first construct the URL of the corresponding ZIP archive on Kenneth French’s server. Each dataset is identified by the file name of its archive; the *Fama/French 3 Factors* set, for instance, lives in `F-F_Research_Data_Factors_CSV.zip`.

## R

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

    <httr2_response>
    GET https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip
    Status: 200 OK
    Content-Type: application/x-zip-compressed
    Body: On disk 'C:\Users\ncj140\AppData\Local\Temp\Rtmpa6oEGw\file45d451bd4cf3.zip' (13045 bytes)

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

    # A tibble: 1,199 × 5
        date `Mkt-RF`   SMB   HML    RF
       <int>    <dbl> <dbl> <dbl> <dbl>
    1 192607     2.89 -2.55 -2.39  0.22
    2 192608     2.64 -1.14  3.81  0.25
    3 192609     0.38 -1.36  0.05  0.23
    4 192610    -3.27 -0.14  0.82  0.32
    5 192611     2.54 -0.11 -0.61  0.31
    # ℹ 1,194 more rows

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

## Python

``` python
dataset = "F-F_Research_Data_Factors"
base_url = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
url = f"{base_url}{dataset}_CSV.zip"
```

Next, we replace the browser download with an HTTP request and extract the ZIP in memory.

``` python
resp = requests.get(url)
resp.raise_for_status()

with zipfile.ZipFile(io.BytesIO(resp.content)) as zf:
    file_name = zf.namelist()[0]  # Ken French ZIPs contain one file
    raw_text = zf.read(file_name).decode("latin1")
```

The most important part of this chunk is the `requests.get()` call. This is the moment where we replace all the manual browser work (open the website, click download, save the file) with a single, reproducible line of code. Then, calling `raise_for_status()` ensures we stop immediately if the server returns an error (e.g., HTTP 404 or 500) instead of quietly handling a broken file. Once this succeeds, `resp.content` is guaranteed to contain valid ZIP bytes that we can open in memory.

The raw file contains documentation text followed by the actual data table(s). To emulate *scrolling down until the numbers start*, we split the file into blocks and keep the long one that contains the table.

``` python
chunks = raw_text.split("\r\n\r\n")
table_text = max(chunks, key=len)
```

Within this block, the first CSV header line starts at the first line beginning with a comma. We add a “Date” label for the first column and pass everything to `pl.read_csv()`, which accepts the raw bytes directly. The numeric fields in the raw file carry leading spaces (e.g., `2.89`), which `polars` does not strip automatically — such values are read as strings. We therefore trim the whitespace and cast all non-date columns to floats.

``` python
match = re.search(r"^\s*,", table_text, flags=re.M)
start = match.start()
csv_text = "Date" + table_text[start:]

factors_ff3_monthly_raw = pl.read_csv(csv_text.encode("latin1")).with_columns(
    pl.exclude("Date").cast(pl.String).str.strip_chars().cast(pl.Float64)
)
```

At this point, the `Date` column still consists of integer date codes with different lengths depending on the frequency. We need a bit of logic to convert them into a proper date column.

``` python
s = factors_ff3_monthly_raw["Date"].cast(pl.String)
n = s.str.len_chars()

if (n == 8).all():  # daily: YYYYMMDD
    date = s.str.to_date("%Y%m%d")
elif (n == 6).all():  # monthly: YYYYMM
    date = (s + "01").str.to_date("%Y%m%d")
elif (n == 4).all():  # annual: YYYY, assigned to year-end
    date = (s + "1231").str.to_date("%Y%m%d")
else:
    raise ValueError("Unknown date format in Fama–French index.")

factors_ff3_monthly_raw = factors_ff3_monthly_raw.with_columns(date=date).drop("Date")
```

Finally, we still have to clean the data. The set *Fama/French 3 Factors* contains the return time series of the market `mkt_excess`, size `smb`, and value `hml` factors alongside the risk-free rate `risk_free`. Cleaning involves a handful of steps:

- Replace Kenneth French’s missing-value codes (`-99.99` and `-999`) with actual missing values.
- Convert the returns from percent to decimals by dividing by 100.
- Standardize the column names (lower case, `Mkt-RF` to `mkt_excess`, and `RF` to `risk_free`).
- Filter the data to the desired date range.

``` python
if start_date:
    factors_ff3_monthly_raw = factors_ff3_monthly_raw.filter(
        pl.col("date") >= pl.lit(start_date).str.to_date()
    )
if end_date:
    factors_ff3_monthly_raw = factors_ff3_monthly_raw.filter(
        pl.col("date") <= pl.lit(end_date).str.to_date()
    )

factors_ff3_monthly = (
    factors_ff3_monthly_raw.rename(lambda c: c.lower())
    .rename({"mkt-rf": "mkt_excess", "rf": "risk_free"})
    .with_columns(pl.exclude("date").replace({-99.99: None, -999: None}) / 100)
    .select(pl.col("date"), pl.exclude("date"))
)
factors_ff3_monthly
```

shape: (780, 5)

| date       | mkt_excess | smb     | hml     | risk_free |
|------------|------------|---------|---------|-----------|
| date       | f64        | f64     | f64     | f64       |
| 1960-01-01 | -0.0698    | 0.0212  | 0.0265  | 0.0033    |
| 1960-02-01 | 0.0116     | 0.006   | -0.0197 | 0.0029    |
| 1960-03-01 | -0.0163    | -0.0055 | -0.0275 | 0.0035    |
| 1960-04-01 | -0.0171    | 0.0022  | -0.0214 | 0.0019    |
| 1960-05-01 | 0.0312     | 0.0129  | -0.0373 | 0.0027    |
| …          | …          | …       | …       | …         |
| 2024-08-01 | 0.016      | -0.0349 | -0.0113 | 0.0048    |
| 2024-09-01 | 0.0172     | -0.0013 | -0.0273 | 0.004     |
| 2024-10-01 | -0.01      | -0.0095 | 0.0091  | 0.0039    |
| 2024-11-01 | 0.0649     | 0.0449  | 0.0038  | 0.004     |
| 2024-12-01 | -0.0317    | -0.027  | -0.0297 | 0.0037    |

All of these steps are doable, but none of them are really about finance—they are just the technical scaffolding required before you can work with the actual factor returns. That is where a dedicated package becomes invaluable.

### Using the tidyfinance shortcut

The `tidyfinance` package performs the entire workflow above under the hood: you request a Fama-French dataset by name and receive a clean, consistently formatted data frame from Kenneth French’s data library. This avoids repetitive boilerplate, reduces errors, and lets you focus on modeling rather than data plumbing. For precise descriptions of the variables, we suggest consulting Prof. Kenneth French’s finance data library directly. We can use the `download_data()` function to download the same monthly *Fama/French 3 Factors* as above.

## R

``` r
factors_ff3_monthly <- download_data(
  domain = "Fama-French",
  dataset = "Fama/French 3 Factors",
  start_date = start_date,
  end_date = end_date
)
```

## Python

``` python
factors_ff3_monthly = tf.download_data(
    domain="Fama-French",
    dataset="Fama/French 3 Factors",
    start_date=start_date,
    end_date=end_date,
)
```

We also download the set *5 Factors (2x3)*, which additionally includes the return time series of the profitability `rmw` and investment `cma` factors. We demonstrate how the monthly factors are constructed in the chapter [Replicating Fama and French Factors](../chapters/replicating-fama-and-french-factors.llms.md).

## R

``` r
factors_ff5_monthly <- download_data(
  domain = "Fama-French",
  dataset = "Fama/French 5 Factors (2x3)",
  start_date = start_date,
  end_date = end_date
)
```

## Python

``` python
factors_ff5_monthly = tf.download_data(
    domain="Fama-French",
    dataset="Fama/French 5 Factors (2x3)",
    start_date=start_date,
    end_date=end_date,
)
```

It is straightforward to download the corresponding *daily* Fama-French factors with the same function.

## R

``` r
factors_ff3_daily <- download_data(
  domain = "Fama-French",
  dataset = "Fama/French 3 Factors [Daily]",
  start_date = start_date,
  end_date = end_date
)
```

## Python

``` python
factors_ff3_daily = tf.download_data(
    domain="Fama-French",
    dataset="Fama/French 3 Factors [Daily]",
    start_date=start_date,
    end_date=end_date,
)
```

In a subsequent chapter, we also use the monthly returns of 10 industry portfolios, so let us fetch that data, too.

## R

``` r
industries_ff_monthly <- download_data(
  domain = "Fama-French",
  dataset = "10 Industry Portfolios",
  start_date = start_date,
  end_date = end_date
)
```

## Python

``` python
industries_ff_monthly = tf.download_data(
    domain="Fama-French",
    dataset="10 Industry Portfolios",
    start_date=start_date,
    end_date=end_date,
)
```

It is worth taking a look at all available portfolio return time series from Kenneth French’s homepage.

## R

You can list all the Fama-French datasets that `tidyfinance` supports as follows:

``` r
list_supported_datasets(domain = "Fama-French")
```

## Python

You should check out the other sets by calling `tf.get_available_famafrench_datasets()`.

## q-Factors

In recent years, the academic discourse experienced the rise of alternative factor models, e.g., in the form of the Hou et al. ([2014](#ref-Hou2015)) *q*-factor model. We refer to the [extended background](http://global-q.org/background.html) information provided by the original authors for further information. The *q* factors can be downloaded directly from the authors’ homepage.

We also need to adjust this data. First, we discard information we will not use in the remainder of the book. Then, we rename the columns with the “R\_”-prescript using regular expressions and write all column names in lowercase. Finally, we apply the same transform of dividing by 100 to all factor returns. You should always try sticking to a consistent style for naming objects, which we try to illustrate here - the emphasis is on *try*. You can check out style guides available online, e.g., [Hadley Wickham’s `tidyverse` style guide.](https://style.tidyverse.org/index.html)

## R

We download the CSV from within `read_csv()`:

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

## Python

We download the CSV with `requests` and pass the response body directly to `pl.read_csv()`:

``` python
factors_q_monthly_link = (
    "https://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2024.csv"
)

factors_q_monthly = (
    pl.read_csv(requests.get(factors_q_monthly_link).content)
    .with_columns(
        date=pl.date(pl.col("year"), pl.col("month"), 1)
    )
    .drop("year", "month")
    .rename(lambda x: x.replace("R_", "").lower())
    .rename({"f": "risk_free", "mkt": "mkt_excess"})
    .filter(
        (pl.col("date") >= pl.lit(start_date).str.to_date())
        & (pl.col("date") <= pl.lit(end_date).str.to_date())
    )
    .with_columns(pl.exclude("date") / 100)
    .select("date", "risk_free", "mkt_excess", "me", "ia", "roe", "eg")
)
```

Again, you can use the `tidyfinance` package for a shortcut:

## R

``` r
download_data(
  domain = "Global Q",
  dataset = "q5_factors_monthly_2024",
  start_date = start_date,
  end_date = end_date
)
```

## Python

``` python
tf.download_data(
    domain="Global Q",
    dataset="q5_factors_monthly_2024",
    start_date=start_date,
    end_date=end_date,
)
```

shape: (696, 7)

| date       | risk_free | mkt_excess | me        | ia        | roe       | eg        |
|------------|-----------|------------|-----------|-----------|-----------|-----------|
| date       | f64       | f64        | f64       | f64       | f64       | f64       |
| 1967-01-01 | 0.003927  | 0.081852   | 0.068122  | -0.029263 | 0.018813  | -0.025511 |
| 1967-02-01 | 0.003743  | 0.007557   | 0.016235  | -0.002915 | 0.035399  | 0.021792  |
| 1967-03-01 | 0.003693  | 0.040169   | 0.019836  | -0.016772 | 0.018417  | -0.011192 |
| 1967-04-01 | 0.003344  | 0.038786   | -0.0067   | -0.028972 | 0.010253  | -0.016371 |
| 1967-05-01 | 0.003126  | -0.042807  | 0.027457  | 0.021864  | 0.005901  | 0.001191  |
| …          | …         | …          | …         | …         | …         | …         |
| 2024-08-01 | 0.004419  | 0.016518   | -0.040817 | 0.004687  | 0.018369  | 0.008116  |
| 2024-09-01 | 0.004619  | 0.016806   | -0.011967 | -0.00001  | 0.007408  | -0.03281  |
| 2024-10-01 | 0.003907  | -0.009701  | -0.011261 | -0.011676 | -0.002314 | -0.008335 |
| 2024-11-01 | 0.003955  | 0.065002   | 0.043985  | -0.049491 | -0.01537  | -0.02142  |
| 2024-12-01 | 0.003663  | -0.031637  | -0.051564 | -0.003684 | -0.021442 | 0.049624  |

## Macroeconomic Predictors

Our next data source is a set of macroeconomic variables often used as predictors for the equity premium. Welch and Goyal ([2008](#ref-Goyal2008)) comprehensively reexamine the performance of variables suggested by the academic literature to be good predictors of the equity premium. The authors host the data on [Amit Goyal’s website.](https://sites.google.com/view/agoyal145) The data is an XLSX-file stored on a public Google Drive location and we directly export a CSV file.

## R

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

## Python

Usually, you need to authenticate if you interact with Google Drive directly in Python. Since the data is stored via a public link, we can proceed without any authentication.

``` python
sheet_id = "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
sheet_name = "macro_predictors.xlsx"
macro_predictors_link = (
    f"https://docs.google.com/spreadsheets/d/{sheet_id}"
    f"/gviz/tq?tqx=out:csv&sheet={sheet_name}"
)
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

## R

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

## Python

Some numeric columns in the raw file contain thousands separators (e.g., `4,769.83`), so we read all columns as strings, strip the commas, and cast to floats before the actual transformations. In addition to the predictors, we compute the one-month-ahead excess market return (`rp_div`), which serves as the prediction target for the equity premium predictors.

``` python
macro_predictors = (
    pl.read_csv(requests.get(macro_predictors_link).content, infer_schema=False)
    .with_columns(
        pl.exclude("yyyymm").str.replace_all(",", "").cast(pl.Float64, strict=False)
    )
    .with_columns(index_div=pl.col("Index") + pl.col("D12"))
    .with_columns(
        logret=pl.col("index_div").log() - pl.col("index_div").shift(1).log()
    )
    .with_columns(
        date=pl.col("yyyymm").str.to_date("%Y%m"),
        rp_div=(pl.col("logret") - (pl.col("Rfree") + 1).log()).shift(-1),
        dp=pl.col("D12").log() - pl.col("Index").log(),
        dy=pl.col("D12").log() - pl.col("Index").shift(1).log(),
        ep=pl.col("E12").log() - pl.col("Index").log(),
        de=pl.col("D12").log() - pl.col("E12").log(),
        tms=pl.col("lty") - pl.col("tbl"),
        dfy=pl.col("BAA") - pl.col("AAA"),
    )
    .rename({"b/m": "bm"})
    .select(
        "date",
        "rp_div",
        "dp",
        "dy",
        "ep",
        "de",
        "svar",
        "bm",
        "ntis",
        "tbl",
        "lty",
        "ltr",
        "tms",
        "dfy",
        "infl",
    )
    .filter(
        (pl.col("date") >= pl.lit(start_date).str.to_date())
        & (pl.col("date") <= pl.lit(end_date).str.to_date())
    )
    .drop_nulls()
)
```

To get the equivalent data through `tidyfinance`, you can call:

## R

``` r
download_data(
  domain = "Goyal-Welch",
  dataset = "monthly",
  start_date = start_date,
  end_date = end_date
)
```

## Python

``` python
tf.download_data(
    domain="Goyal-Welch",
    dataset="monthly",
    start_date=start_date,
    end_date=end_date,
)
```

shape: (767, 15)

| date | rp_div | dp | dy | ep | de | svar | bm | ntis | tbl | lty | ltr | tms | dfy | infl |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| date | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 | f64 |
| 1960-01-01 | 0.006165 | -3.394191 | -3.468337 | -2.797533 | -0.596658 | 0.000919 | 0.499502 | 0.022116 | 0.0435 | 0.0441 | 0.0112 | 0.0006 | 0.0073 | -0.003401 |
| 1960-02-01 | -0.015793 | -3.383903 | -3.374774 | -2.806662 | -0.577241 | 0.00115 | 0.493557 | 0.024037 | 0.0396 | 0.0429 | 0.0204 | 0.0033 | 0.0078 | 0.003413 |
| 1960-03-01 | -0.020521 | -3.350808 | -3.364804 | -2.792666 | -0.558142 | 0.000969 | 0.549798 | 0.025593 | 0.0331 | 0.0411 | 0.0282 | 0.008 | 0.0076 | 0.0 |
| 1960-04-01 | 0.023755 | -3.331425 | -3.349108 | -2.787838 | -0.543587 | 0.000645 | 0.563404 | 0.025577 | 0.0323 | 0.0426 | -0.017 | 0.0103 | 0.0075 | 0.003401 |
| 1960-05-01 | 0.016046 | -3.356176 | -3.329677 | -2.827389 | -0.528786 | 0.000424 | 0.541966 | 0.024414 | 0.0329 | 0.0417 | 0.0152 | 0.0088 | 0.0082 | 0.0 |
| … | … | … | … | … | … | … | … | … | … | … | … | … | … | … |
| 2023-07-01 | -0.022065 | -4.198545 | -4.167881 | -3.226908 | -0.971637 | 0.000537 | 0.205917 | -0.016677 | 0.0525 | 0.039 | -0.0035 | -0.0135 | 0.0108 | 0.001908 |
| 2023-08-01 | -0.053627 | -4.17778 | -4.195656 | -3.203119 | -0.974662 | 0.001337 | 0.210885 | -0.016514 | 0.053 | 0.0417 | -0.0052 | -0.0113 | 0.0107 | 0.004367 |
| 2023-09-01 | -0.02609 | -4.124953 | -4.1749 | -3.147294 | -0.97766 | 0.001023 | 0.218528 | -0.017989 | 0.0532 | 0.0438 | -0.0221 | -0.0094 | 0.0103 | 0.002485 |
| 2023-10-01 | 0.079457 | -4.097976 | -4.120201 | -3.110379 | -0.987598 | 0.001678 | 0.221534 | -0.014711 | 0.0534 | 0.048 | -0.0121 | -0.0054 | 0.0102 | -0.000383 |
| 2023-11-01 | 0.038308 | -4.17867 | -4.093246 | -3.181326 | -0.997345 | 0.001341 | 0.203676 | -0.020618 | 0.0527 | 0.045 | 0.0347 | -0.0077 | 0.0101 | -0.002015 |

## Other Macroeconomic Data

The Federal Reserve Bank of St. Louis provides the Federal Reserve Economic Data (FRED), an extensive database for macroeconomic data. In total, there are 817,000 US and international time series from 108 different sources. The data can be downloaded directly from FRED by constructing the appropriate URL. For instance, let us consider the consumer price index (CPI) data that can be found under the [CPIAUCNS](https://fred.stlouisfed.org/series/CPIAUCNS) key.

## R

``` r
series <- "CPIAUCNS"
url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
  series
)
```

We can then use the `httr2` ([Wickham 2024](#ref-httr2)) package to request the CSV, extract the data from the response body, and convert the columns to a tidy format:

``` r
resp <- request(url) |>
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

    Warning: The `file` argument of `read_csv()` should use `I()` for literal
    data as of readr 2.2.0.
      
      # Bad (for example):
      read_csv("x,y\n1,2")
      
      # Good:
      read_csv(I("x,y\n1,2"))

## Python

``` python
series = "CPIAUCNS"
url = f"https://fred.stlouisfed.org/graph/fredgraph.csv?id={series}"
```

We can then use the `requests` module to request the CSV, extract the data from the response body, and convert the columns to a tidy format. Note that we pass `impersonate="chrome"` so that the request mimics a regular browser; FRED rejects downloads that do not look like they come from a browser.

``` python
resp = requests.get(url, impersonate="chrome")

cpi_monthly = (
    pl.read_csv(resp.content)
    .with_columns(
        date=pl.col("observation_date").str.to_date(),
        value=pl.col(series).cast(pl.Float64, strict=False),
        series=pl.lit(series),
    )
    .select("date", "series", "value")
    .filter(
        (pl.col("date") >= pl.lit(start_date).str.to_date())
        & (pl.col("date") <= pl.lit(end_date).str.to_date())
    )
    .with_columns(cpi=pl.col("value") / pl.col("value").last())
)
```

The last line sets the current (latest) price level as the reference price level.

The `tidyfinance` package can, of course, also fetch the same index data and many more data series:

## R

``` r
download_data(
  domain = "FRED",
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

## Python

``` python
tf.download_data(
    domain="FRED", series="CPIAUCNS", start_date=start_date, end_date=end_date
)
```

shape: (780, 3)

| date       | series     | value   |
|------------|------------|---------|
| date       | str        | f64     |
| 1960-01-01 | "CPIAUCNS" | 29.3    |
| 1960-02-01 | "CPIAUCNS" | 29.4    |
| 1960-03-01 | "CPIAUCNS" | 29.4    |
| 1960-04-01 | "CPIAUCNS" | 29.5    |
| 1960-05-01 | "CPIAUCNS" | 29.5    |
| …          | …          | …       |
| 2024-08-01 | "CPIAUCNS" | 314.796 |
| 2024-09-01 | "CPIAUCNS" | 315.301 |
| 2024-10-01 | "CPIAUCNS" | 315.664 |
| 2024-11-01 | "CPIAUCNS" | 315.493 |
| 2024-12-01 | "CPIAUCNS" | 315.605 |

To download other time series, we just have to look it up on the FRED website and extract the corresponding key from the address. For instance, the producer price index for gold ores can be found under the [PCU2122212122210](https://fred.stlouisfed.org/series/PCU2122212122210) key.

## R

If your desired time series is not supported through `tidyfinance`, we recommend working with the `fredr` package ([Boysel and Vaughan 2021](#ref-fredr)). Note that you need to get an API key to use its functionality. We refer to the package documentation for details.

## Python

If your desired time series is not supported through `tidyfinance`, we recommend working with the `fredapi` package. Note that you need to get an API key to use its functionality. We refer to the package documentation for details.

## Setting Up a Database

Now that we have downloaded some (freely available) data from the web into the memory of our session, let us store that information for future use. We will use the data stored throughout the following chapters, but you could alternatively implement a different strategy and replace the respective code.

There are many ways to set up and organize your data, depending on the use case. Storing data as Parquet files creates a universal data format that seamlessly works across different programming languages and platforms. Unlike CSV files that often lose data types when shared between R, Python, Julia, or other languages, Parquet files maintain perfect data integrity regardless of which tool reads them. A dataset saved as Parquet in one language can be opened directly in another, such as Python’s pandas or Julia’s DataFrames.jl.[^1]

Parquet files are easily created - the code below is really all there is. You do not need any external software. We will store everything in the `data` subfolder, to retrieve data for all subsequent chapters. The initial part of the code ensures that the directory is created if it does not already exist.

## R

``` r
library(nanoparquet)
```

``` r
if (!dir.exists("data")) {
  dir.create("data")
}
```

## Python

``` python
import os

if not os.path.exists("data"):
    os.makedirs("data")
```

Next, we create a file with the monthly Fama-French factor data. We do so with the `write_parquet()` function.

## R

``` r
write_parquet(factors_ff3_monthly, "data/factors_ff3_monthly.parquet")
```

## Python

``` python
factors_ff3_monthly.write_parquet("data/factors_ff3_monthly.parquet")
```

You can retrieve the data directly into your memory by calling the corresponding read function.

## R

The `read_parquet()` function loads the file into memory as a regular data frame, which we can pipe directly into `dplyr` verbs.

``` r
read_parquet("data/factors_ff3_monthly.parquet") |>
  select(date, risk_free) |>
  head(10)
```

    # A data frame: 10 × 2
      date       risk_free
    * <date>         <dbl>
    1 1960-01-01    0.0033
    2 1960-02-01    0.0029
    3 1960-03-01    0.0035
    4 1960-04-01    0.0019
    5 1960-05-01    0.0027
    # ℹ 5 more rows

## Python

``` python
pl.read_parquet("data/factors_ff3_monthly.parquet")
```

shape: (780, 5)

| date       | mkt_excess | smb     | hml     | risk_free |
|------------|------------|---------|---------|-----------|
| date       | f64        | f64     | f64     | f64       |
| 1960-01-01 | -0.0698    | 0.0212  | 0.0265  | 0.0033    |
| 1960-02-01 | 0.0116     | 0.006   | -0.0197 | 0.0029    |
| 1960-03-01 | -0.0163    | -0.0055 | -0.0275 | 0.0035    |
| 1960-04-01 | -0.0171    | 0.0022  | -0.0214 | 0.0019    |
| 1960-05-01 | 0.0312     | 0.0129  | -0.0373 | 0.0027    |
| …          | …          | …       | …       | …         |
| 2024-08-01 | 0.016      | -0.0349 | -0.0113 | 0.0048    |
| 2024-09-01 | 0.0172     | -0.0013 | -0.0273 | 0.004     |
| 2024-10-01 | -0.01      | -0.0095 | 0.0091  | 0.0039    |
| 2024-11-01 | 0.0649     | 0.0449  | 0.0038  | 0.004     |
| 2024-12-01 | -0.0317    | -0.027  | -0.0297 | 0.0037    |

The last couple of code chunks are really all there is to organizing a simple database! You can also share the parquet files across devices and programming languages.

Before we move on to the next data source, let us also store the other tables in our new data folder.

## R

``` r
lst(
  factors_ff5_monthly,
  factors_ff3_daily,
  industries_ff_monthly,
  factors_q_monthly,
  macro_predictors,
  cpi_monthly
) |>
  iwalk(\(data, name) write_parquet(data, paste0("data/", name, ".parquet")))
```

## Python

``` python
data_dict = {
    "factors_ff5_monthly": factors_ff5_monthly,
    "factors_ff3_daily": factors_ff3_daily,
    "industries_ff_monthly": industries_ff_monthly,
    "factors_q_monthly": factors_q_monthly,
    "macro_predictors": macro_predictors,
    "cpi_monthly": cpi_monthly,
}

for key, value in data_dict.items():
    value.write_parquet("data/" + key + ".parquet")
```

## Key Takeaways

- Importing Fama-French factors, q-factors, macroeconomic indicators, and CPI data is simplified through API calls, CSV parsing, and web scraping techniques.
- The `tidyfinance` package offers pre-processed access to financial datasets, reducing manual data cleaning and saving valuable time.
- Creating `parquet` files helps manage and organize data efficiently across projects, while maintaining reproducibility.
- Structured database storage supports scalable data access, which is essential for long-term academic projects and collaborative work in finance.

## Exercises

1.  Download the monthly Fama-French factors manually from [Ken French’s data library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html) and read them in via your language’s CSV reader. Validate that you get the same data as via the download script we developed above.
2.  Download the daily Fama-French 5 factors using `download_data()`. After the successful download and conversion to the column format that we used above, compare the `risk_free`, `mkt_excess`, `smb`, and `hml` columns of `factors_ff3_daily` to `factors_ff5_daily`. Discuss any differences you might find.

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
