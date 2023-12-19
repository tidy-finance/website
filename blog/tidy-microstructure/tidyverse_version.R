library(data.table)
library(dtplyr)
library(tidyverse)

# Read-in data
ticker <- "PRU"
quote_data_location <- glue::glue("blog/tidy-microstructure/data/{ticker}_quotes.csv")
quotes <- vroom::vroom(quote_data_location, 
                       col_types = list(`Exch Time` = col_character()))
quotes <- lazy_dt(quotes)

# Clean data (names, columns, date and time)
quotes <- quotes|> 
  transmute(venue = `#RIC`,
            date = as.Date(`Date-Time`),
            time = hms::as_hms(`Exch Time`),
            time = hms::as_hms(time + dhours(`GMT Offset`)),
            bid_price = `Bid Price`, 
            ask_price = `Ask Price`,
            bid_depth = `Bid Size`, 
            ask_depth = `Ask Size`,
  ) |> 
  mutate(venue = str_remove(venue, ticker), 
         venue = case_when(venue == ".L" ~ "lse",
                           venue == "l.TQ" ~ "tqe"),
         ticker = ticker) |>
  arrange(date, time)

# Summary statistics (inline code, I guess)
nrow(as_tibble(quotes)) # Observations
quotes |> pull(date) |> min() # Date range
quotes |> pull(date) |> max()

# Forward filling
quotes <- quotes |>
  group_by(ticker, date) |>
  fill(matches("bid|ask")) |> 
  ungroup()

# Remove Quotes before the opening auction and after the closing auction to missing 
# Setting quotes around the intraday auction to missing 

quotes <- quotes |>
  filter(time > hms::as_hms("08:01:00"), 
         time < hms::as_hms("16:29:00")) |>
  mutate(across(matches("bid|ask"), 
              ~if_else(time > hms::as_hms("11:59:00") & 
                       time < hms::as_hms("12:03:00"), NA_real_, .)))

# Screening
quotes <- quotes |> 
  mutate(quoted_spread = ask_price - bid_price)

ggplot(data = as_tibble(quotes |> drop_na()),
       aes(x = quoted_spread, 
           fill = venue)) +
  geom_histogram(bins = 20, alpha = 0.5, position = "identity") +
  labs(title = "Histogram of nominal bid-ask spread",
       x = "Nominal bid-ask spread (in USD)",
       y = "Count")

# Data export (eval = FALSE)
write_csv(quotes, file = "quotes.csv.gz")

# Quote-based liquidity measures (uniform and duration weighted)

tick_size <- 0.5

quotes_liquidity <- quotes |>
  group_by(ticker, date) |> 
  mutate(duration = c(duration(0), diff(time)),
         midpoint = (bid_price + ask_price) / 2) |>
  summarise(uniform.quoted_spread_norm = mean(quoted_spread, na.rm = TRUE),
            uniform.quoted_spread_relative = mean(quoted_spread / midpoint, na.rm = TRUE) * 1e4,
            uniform.quoted_spread_tick = mean(quoted_spread / tick_size, na.rm = TRUE),
            uniform.quoted_depth = mean(bid_depth * bid_price + ask_depth * ask_price, 
                                        na.rm = TRUE) / 2 * 1e-6,
            duration.quoted_spread_norm = weighted.mean(quoted_spread, w = duration, na.rm = TRUE),
            duration.quoted_spread_relative = weighted.mean(quoted_spread / midpoint, w = duration, na.rm = TRUE) * 1e4,
            duration.quoted_spread_tick = weighted.mean(quoted_spread / tick_size, w = duration, na.rm = TRUE),
            duration.quoted_depth = weighted.mean(bid_depth * bid_price + ask_depth * ask_price, w = duration, 
                                         na.rm = TRUE) / 2 * 1e-6)

quotes_liquidity |> 
  group_by(ticker) |>
  summarise(across(contains("quoted"), 
                   ~round(mean(.), digits = 2))) |>
  pivot_longer(-ticker) |>
  as_tibble() |>
  separate_wider_delim(name, delim = ".", names = c("method", "measure")) |>
  pivot_wider(names_from = method, values_from = value)

# Retaining only the last observation per unit of time

quotes <- quotes |> 
  group_by(ticker, venue, date, time) |>
  slice(n()) |> 
  ungroup()

# Merging quotes from two venues trading the same security, forward-filling quotes

quotes_ebbo <- quotes |> 
  pivot_wider(names_from = venue, 
              values_from = matches("bid|ask"))

quotes_ebbo <- quotes_ebbo |>
  group_by(ticker, date) |>
  fill(matches("bid|ask")) |> 
  ungroup()

#  Obtain the EBBO prices and depths
quotes_ebbo <- quotes_ebbo |>
  mutate(best_bid_price = pmax(bid_price_lse, bid_price_tqe, na.rm = TRUE),
         best_ask_price = pmin(ask_price_lse, ask_price_tqe, na.rm = TRUE),
         best_bid_depth = bid_depth_lse * (bid_price_lse == best_bid_price) + 
           bid_depth_tqe * (bid_price_tqe == best_bid_price),
         best_ask_depth = ask_depth_lse * (ask_price_lse == best_ask_price) + 
           ask_depth_tqe * (ask_price_tqe == best_ask_price)
  )

# Dropping local exchange variables and objects
quotes_ebbo <- quotes_ebbo |>
  select(ticker, date, time, contains("best"))

# Fundamental value
quotes_ebbo <- quotes_ebbo |>
  mutate(midpoint = (best_bid_price + best_ask_price) / 2)
