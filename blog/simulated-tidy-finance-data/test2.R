library(tidyverse)
library(RSQLite)
library(DBI)

# Preparation ------------------------------------------

tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance.sqlite",
  extended_types = TRUE
)

tidy_finance_simulated <- dbConnect(
  SQLite(),
  "data/tidy_finance_simulated.sqlite",
  extended_types = TRUE
)

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2023-08-01")

time_series_years <- seq(year(start_date), year(end_date), 1)
time_series_months <- seq(start_date, end_date, "1 month")
time_series_days <- seq(start_date, end_date, "1 day")

# Create monthly or daily macro tables -----------------

macro_tables <- c(
  "cpi_monthly", "factors_ff_daily", 
  "factors_ff_monthly", "factors_q_monthly",
  "industries_ff_monthly", "macro_predictors"
)

for (table_name in macro_tables) {
  data_original <- tbl(tidy_finance, table_name) |>
    collect() |> 
    drop_na()
  
  if ("month" %in% names(data_original)) {
    time_series <- time_series_months
    date_column <- "month"
  } else if ("date" %in% names(data_original)) {
    time_series <- time_series_days
    date_column <- "date"
  }
  
  ## TODO: this trick to create columns is actually quite nice, 
  ##       will put it into a function because I reuse it below
  relevant_columns <- data_original |> 
    select(-contains(c("month", "date"))) |> 
    names()
  
  commands <- unlist(
    map(
      relevant_columns, 
      ~rlang::exprs(!!..1 := runif(n()))
    )
  )
  
  data_simulated <- tibble(
    !!sym(date_column) := time_series
    ) |> 
    mutate(
      !!!commands
    )
  
  dbWriteTable(tidy_finance_simulated, table_name, 
               data_simulated, overwrite = TRUE)
}

# Create firm and stock tables -------------------------

stock_tables <- c("beta", "compustat", 
                  "crps_daily", "crsp_monthly")

## Simulate a cross section of stocks with characteristics
number_of_stocks <- 1000

crsp_stocks <- tbl(tidy_finance, "crsp_monthly") |>
  group_by(permno) |> 
  filter(month == max(month, na.rm = TRUE)) |> 
  ungroup() |> 
  select(permno, gvkey, industry, exchange, exchcd, siccd) |> 
  collect()

industries <- crsp_stocks |> 
  filter(industry != "Missing") |> 
  count(industry) |> 
  mutate(prob = n / sum(n))

exchanges <- crsp_stocks |> 
  filter(exchange != "Other") |> 
  count(exchange) |> 
  mutate(prob = n / sum(n))

set.seed(123)

stock_identifiers <- 1:number_of_stocks |> 
  map_df(
    function(x) {
      tibble(
        permno = x,
        gvkey = as.character(x + 10000),
        exchange = sample(exchanges$exchange, 1, 
                          prob = exchanges$prob),
        industry = sample(industries$industry, 1, 
                          prob = industries$prob)
      ) |> 
        mutate(
          exchcd = case_when(
            exchange == "NYSE" ~ sample(c(1, 31), 1),
            exchange == "AMEX" ~ sample(c(2, 32), 1),
            exchange == "NASDAQ" ~ sample(c(3, 33), 1)
          ),
          siccd = case_when(
            industry == "Agriculture" ~ sample(1:999, 1),
            industry == "Mining" ~ sample(1000:1499, 1),
            industry == "Construction" ~ sample(1500:1799, 1),
            industry == "Manufacturing" ~ sample(1800:3999, 1),
            industry == "Transportation" ~ sample(4000:4899, 1),
            industry == "Utilities" ~ sample(4900:4999, 1),
            industry == "Wholesale" ~ sample(5000:5199, 1),
            industry == "Retail" ~ sample(5200:5999, 1),
            industry == "Finance" ~ sample(6000:6799, 1),
            industry == "Services" ~ sample(7000:8999, 1),
            industry == "Public" ~ sample(9000:9999, 1)
          )
        )
    }
  )

## Construct panels with different time horizons
stock_panel_yearly <- crossing(
  stock_identifiers, 
  tibble(year = time_series_years)
) |> 
  select(gvkey, year)

stock_panel_monthly <- crossing(
  stock_identifiers, 
  tibble(month = time_series_months)
) |> 
  select(permno, gvkey, month, siccd, industry, exchcd, exchange)

stock_panel_daily <- crossing(
  stock_identifiers, 
  tibble(date = time_series_days)
)|> 
  select(permno, date)

## Simulate beta table
tbl(tidy_finance, "beta")

beta_simulated <- stock_panel_monthly |> 
  mutate(
    beta_monthly = runif(n()),
    beta_daily = runif(n())
  )

dbWriteTable(tidy_finance_simulated, "beta", 
             beta_simulated, overwrite = TRUE)

## Simualte compustat table
tbl(tidy_finance, "compustat")

relevant_columns <- tbl(tidy_finance, "compustat") |> 
  select(-c(gvkey, datadate, year)) |> 
  names()

commands <- unlist(
  map(
    relevant_columns, 
    ~rlang::exprs(!!..1 := runif(n()))
  )
)

compustat_simulated <- stock_panel_yearly |> 
  mutate(
    datadate = ymd(str_c(year, "12", "31")),
    !!!commands
  )

names(tbl(tidy_finance, "compustat")) %in% names(compustat_simulated)

dbWriteTable(tidy_finance_simulated, "compustat", 
             compustat_simulated, overwrite = TRUE)

## Simualte crsp_monthly table
tbl(tidy_finance, "crsp_monthly")

crsp_monthly_simulated <- stock_panel_monthly |> 
  mutate(
    date = ceiling_date(month, "month") - 1,
    ret = runif(n()),
    ret_excess = runif(n()),
    shrout = runif(n()),
    altprc = runif(n()),
    mktcap = runif(n()),
    mktcap_lag = runif(n())
  )

### Check 
names(tbl(tidy_finance, "crsp_monthly")) %in% names(crsp_monthly_simulated)

dbWriteTable(tidy_finance_simulated, "crsp_monthly", 
             crsp_monthly_simulated, overwrite = TRUE)

## Simulate crsp_daily table
tbl(tidy_finance, "crsp_daily")

crsp_daily_simulated <- stock_panel_daily |> 
  mutate(
    month = floor_date(date, "month"),
    ret_excess = runif(n())
  )

dbWriteTable(tidy_finance_simulated, "crsp_daily", 
             crsp_daily_simulated, overwrite = TRUE)

# Create bond tables -----------------------------------


