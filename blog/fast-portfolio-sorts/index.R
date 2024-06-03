library(dplyr)
library(dtplyr)
library(data.table)
library(bench)
library(purrr)
library(RSQLite)

tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

assign_portfolio <- function(data, sorting_variable, n_portfolios) {
  
  breakpoints <- quantile(
    data[[sorting_variable]], 
    probs = seq(0, 1, length.out = n_portfolios + 1), 
    na.rm = TRUE, names = FALSE
  )

  findInterval(
    data[[sorting_variable]], breakpoints, all.inside = TRUE
  )
}

crsp_monthly_tb <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, month, ret_excess, mktcap_lag) |>
  collect()

crsp_monthly_df <- as.data.frame(crsp_monthly_tb)

crsp_monthly_dt <- as.data.table(crsp_monthly_tb)

sort_base <- function() {
  df_split <- split(crsp_monthly_tb, crsp_monthly_tb$month)
  df_split <- lapply(df_split, function(df) {
    df$portfolio <- assign_portfolio(df, "mktcap_lag", n_portfolios = 10)
    return(df)
  })
  df <- do.call(rbind, df_split)
  aggregate(ret_excess ~ portfolio, data = df, FUN = function(x) mean(x, na.rm = TRUE))
}

sort_dplyr <- function() {
  crsp_monthly_tb |> 
    mutate(
      portfolio = assign_portfolio(
        pick(everything()), "mktcap_lag", n_portfolios = 10),
      by = "month"
    ) |> 
    group_by(portfolio) |> 
    summarize(ret = mean(ret_excess, na.rm = TRUE))
}

sort_dt <- function() {
  copy(crsp_monthly_dt)[, `:=`(portfolio = assign_portfolio(.SD, "mktcap_lag", n_portfolios = 10), by = month)][, .(ret = mean(ret_excess, na.rm = TRUE)), keyby = .(portfolio)]
}

sort_dtplyr <- function() {
  crsp_monthly_dt |> 
    mutate(
      portfolio = assign_portfolio(
        pick(everything()), "mktcap_lag", n_portfolios = 10),
      by = "month"
    )  |> 
    group_by(portfolio) |> 
    summarize(ret = mean(ret_excess, na.rm = TRUE)) |> 
    as.data.table()
}

set.seed(1234)
iterations <- 100

results <- bind_rows(
  bench::mark(sort_base(), iterations = iterations),
  bench::mark(sort_dplyr(), iterations = iterations),
  bench::mark(sort_dt(), iterations = iterations),
  bench::mark(sort_dtplyr(), iterations = iterations) 
)

saveRDS(results, "bench_results.rds") 

ggplot2::autoplot(results, type = "violin") +
  labs(y = NULL, x = "Sorting approach")
