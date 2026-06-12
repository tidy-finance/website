# Regenerate data-r's downloaded tables by mirroring the data pipeline of
# r/accessing-and-managing-financial-data.qmd as of origin/main
# (includes PR #216: q-factors 2024 vintage + drop year/month,
#  and cpi_monthly column order date, series, value, cpi).
suppressMessages({
  library(tidyverse)
  library(frenchdata)
  library(httr2)
  library(arrow)
})

start_date <- ymd("1960-01-01")
end_date <- ymd("2024-12-31")

factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")
factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML), ~ as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`, risk_free = rf) |>
  filter(date >= start_date & date <= end_date)
cat("ff3_monthly:", dim(factors_ff3_monthly), "\n")

factors_ff5_monthly_raw <- download_french_data("Fama/French 5 Factors (2x3)")
factors_ff5_monthly <- factors_ff5_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~ as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`, risk_free = rf) |>
  filter(date >= start_date & date <= end_date)
cat("ff5_monthly:", dim(factors_ff5_monthly), "\n")

factors_ff3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")
factors_ff3_daily <- factors_ff3_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML), ~ as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`, risk_free = rf) |>
  filter(date >= start_date & date <= end_date)
cat("ff3_daily:", dim(factors_ff3_daily), "\n")

industries_ff_monthly_raw <- download_french_data("10 Industry Portfolios")
industries_ff_monthly <- industries_ff_monthly_raw$subsets$data[[1]] |>
  mutate(date = floor_date(ymd(str_c(date, "01")), "month")) |>
  mutate(across(where(is.numeric), ~ . / 100)) |>
  select(date, everything()) |>
  filter(date >= start_date & date <= end_date) |>
  rename_with(str_to_lower)
cat("industries:", dim(industries_ff_monthly), "\n")

factors_q_monthly_link <-
  "https://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2024.csv"
factors_q_monthly <- read_csv(factors_q_monthly_link, show_col_types = FALSE) |>
  mutate(date = ymd(str_c(year, month, "01", sep = "-"))) |>
  select(-year, -month) |>
  rename_with(~ str_remove(., "R_")) |>
  rename_with(str_to_lower) |>
  mutate(across(-date, ~ . / 100)) |>
  select(date, risk_free = f, mkt_excess = mkt, everything()) |>
  filter(date >= start_date & date <= end_date)
cat("q_monthly:", dim(factors_q_monthly), "\n")

sheet_id <- "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
sheet_name <- "Monthly"
macro_predictors_url <- paste0(
  "https://docs.google.com/spreadsheets/d/", sheet_id,
  "/gviz/tq?tqx=out:csv&sheet=", sheet_name
)
macro_predictors_raw <- read_csv(macro_predictors_url, show_col_types = FALSE)
macro_predictors <- macro_predictors_raw |>
  mutate(date = ym(yyyymm)) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(
    IndexDiv = Index + D12,
    logret = log(IndexDiv) - log(lag(IndexDiv)),
    Rfree = log(Rfree + 1),
    rp_div = lead(logret - Rfree, 1),
    dp = log(D12) - log(Index),
    dy = log(D12) - log(lag(Index)),
    ep = log(E12) - log(Index),
    de = log(D12) - log(E12),
    tms = lty - tbl,
    dfy = BAA - AAA
  ) |>
  select(
    date, rp_div, dp, dy, ep, de, svar, bm = `b/m`, ntis,
    tbl, lty, ltr, tms, dfy, infl
  ) |>
  filter(date >= start_date & date <= end_date) |>
  drop_na()
cat("macro_predictors:", dim(macro_predictors), "\n")

series <- "CPIAUCNS"
cpi_url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series)
resp <- request(cpi_url) |> req_perform()
resp_csv <- resp |> resp_body_string()
cpi_monthly <- resp_csv |>
  read_csv(show_col_types = FALSE) |>
  mutate(
    date = as.Date(observation_date),
    value = as.numeric(.data[[series]]),
    series = series,
    .keep = "none"
  ) |>
  filter(date >= start_date & date <= end_date) |>
  mutate(cpi = value / value[date == max(date)]) |>
  select(date, series, value, cpi)
cat("cpi_monthly:", dim(cpi_monthly), "\n")

write_parquet(factors_ff3_monthly, "data-r/factors_ff3_monthly.parquet")
lst(
  factors_ff5_monthly, factors_ff3_daily, industries_ff_monthly,
  factors_q_monthly, macro_predictors, cpi_monthly
) |>
  iwalk(\(data, name) write_parquet(data, paste0("data-r/", name, ".parquet")))
cat("DATA-R REGENERATION DONE\n")
