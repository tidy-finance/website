library(tidyfinance)
library(haven)

# Working with Stock Returns ---------------------------------------------

prices_daily_aapl <- download_data(
  type = "stock_prices",
  symbols = "AAPL",
  start_date = "2000-01-01",
  end_date = "2024-12-31"
)

write_dta(prices_daily_aapl, "data-stata/prices_daily_aapl.dta")

symbols <- download_data(
  type = "constituents",
  index = "Dow Jones Industrial Average"
)

prices_daily <- download_data(
  type = "stock_prices",
  symbols = symbols$symbol,
  start_date = "2000-01-01",
  end_date = "2023-12-31"
)

write_dta(prices_daily, "data-stata/prices_daily.dta")
