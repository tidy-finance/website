## TIDY MARKET MICROSTRUCTURE ###
# By Björn Hagströmer and Niklas Landsberg
# For full documentation of the code, see [URL]

## -----------------------------------------------------------------------------------------------------------------------
#| echo=FALSE
library(kableExtra)

variable_table <- data.frame(
  "Variable name" = c("Quoted bid-ask spread", "Quoted depth", "Effective bid-ask spread", "Trade volume", "Price impact", "Realized spread", "Return autocorrelation", "Realized volatility", "Variance ratio"),
  "Variable type" = c("Liquidity", "Liquidity", "Liquidity", "Volume", "Liquidity", "Liquidity", "Efficiency", "Volatility", "Efficiency"),
  "Data type" = c("Quote tick data", "Quote tick data", "Quote and trade tick data", "Trade tick data", "Quote and trade tick data", "Quote and trade tick data", "Equispaced quote data", "Equispaced quote data", "Equispaced quote data"),
  "Section" = c(1, 1, 2, 2, 2, 2, 3, 3, 3),
  check.names = FALSE
  )

kable(variable_table,
      caption = "Market quality variables and data types",
      booktabs = TRUE,
      longtable = FALSE)


## -----------------------------------------------------------------------------------------------------------------------
# Install and load the `data.table` package
# install.packages("data.table")
library(data.table)
quotes_url <- "http://tinyurl.com/8wyfk26c"

# Load the view the quote data
# The function `fread` is similar to `read.csv`, but much faster.
quotes <- fread(quotes_url)

# Preview the data
quotes


## -----------------------------------------------------------------------------------------------------------------------
#| message=FALSE,
#| warning=FALSE

# install.packages("data.table")
# install.packages("dtplyr")
# install.packages("tidyverse")
library(data.table)
library(dtplyr)
library(tidyverse)


## -----------------------------------------------------------------------------------------------------------------------
#| cache=TRUE
tv_quotes <- vroom::vroom("http://tinyurl.com/8wyfk26c", 
                          col_types = list(`Exch Time` = col_character()))

tv_quotes <- lazy_dt(tv_quotes)

tv_quotes <- tv_quotes|> 
  transmute(ticker = `#RIC`,
            date = as.Date(`Date-Time`),
            time = hms::as_hms(`Exch Time`),
            time = hms::as_hms(time + dhours(`GMT Offset`)),
            domain = Domain,
            bid_price = `Bid Price`, 
            ask_price = `Ask Price`,
            bid_depth = `Bid Size`, 
            ask_depth = `Ask Size`,
  )



## ----Quote data subset--------------------------------------------------------------------------------------------------
# Rename the variables
raw_quote_variables <- c("#RIC", "Date-Time", "GMT Offset", "Domain", "Exch Time",
                         "Type", "Bid Price", "Bid Size", "Ask Price", "Ask Size")

new_quote_variables <- c("ticker", "date_time", "gmt_offset", "domain", "exch_time", 
                         "type", "bid_price", "bid_depth", "ask_price", "ask_depth")

setnames(quotes, raw_quote_variables, new_quote_variables)


## -----------------------------------------------------------------------------------------------------------------------



## -----------------------------------------------------------------------------------------------------------------------
# Output a table of sample tickers and values of `domain` and `type`
table(quotes$ticker, quotes$domain)

table(quotes$ticker, quotes$type)

# Delete variables
quotes[, c("domain", "type") := NULL]



## -----------------------------------------------------------------------------------------------------------------------



## ----Extracting dates---------------------------------------------------------------------------------------------------
# Obtain dates
quotes[, date := as.Date(date_time)]


## -----------------------------------------------------------------------------------------------------------------------



## ----Overview stocks and days-------------------------------------------------------------------------------------------
# Output a table of sample dates and tickers
table(quotes$ticker, quotes$date)


## ----Quote time conversion----------------------------------------------------------------------------------------------
# Convert time stamps to numeric format, expressed in seconds past midnight
# The function `strsplit` splits a character string at the point defined by `split`
# `do.call` is a way to call a function, which in this case calls `rbind` to convert a 
# list of vectors to a matrix, where each vector forms one row
quotes[, time := {
	time_elements = strsplit(exch_time, split = ":")
	time_elements = do.call(rbind, time_elements)
	time = as.numeric(time_elements[, 1]) * 3600 + 
		   as.numeric(time_elements[, 2]) * 60 + 
		   as.numeric(time_elements[, 3]) +
		   gmt_offset * 3600
	list(time)}]


## -----------------------------------------------------------------------------------------------------------------------



## ----Cleaning up quotes-------------------------------------------------------------------------------------------------
# Delete raw time variables
quotes[, c("date_time", "exch_time", "gmt_offset") := NULL]


## -----------------------------------------------------------------------------------------------------------------------



## ----Forward-filling quotes---------------------------------------------------------------------------------------------
# Forward-fill quoted prices and depths
lob_variables <- c("bid_price", "bid_depth", "ask_price", "ask_depth")

quotes[, 
  (lob_variables) := lapply(.SD, nafill, type = "locf"),	.SDcols = (lob_variables), 
  by = c("ticker", "date")]


## -----------------------------------------------------------------------------------------------------------------------
tv_quotes <- tv_quotes |>
  group_by(ticker, date) |>
  fill(matches("bid|ask")) |> 
  ungroup()


## ----Opening hours------------------------------------------------------------------------------------------------------
# Exclude quotes around the opening and closing of continuous trading
open_time <- 8 * 3600
close_time <- 16.5 * 3600
quotes <- quotes[time > (open_time + 60) & time < (close_time - 60)]

# Set quotes around the intraday auction to missing 
intraday_auction_time <- 12 * 3600
quotes[time > (intraday_auction_time - 60) & time < (intraday_auction_time + 3 * 60), 
  (lob_variables)] <-  NA


## -----------------------------------------------------------------------------------------------------------------------
tv_quotes <- tv_quotes |>
  filter(time > hms::as_hms("08:01:00"), 
         time < hms::as_hms("16:29:00"))

tv_quotes <- tv_quotes |>
  mutate(across(matches("bid|ask"), 
                ~if_else(time > hms::as_hms("11:59:00") & 
                         time < hms::as_hms("12:03:00"), NA_real_, .)))


## -----------------------------------------------------------------------------------------------------------------------
# Plot a histogram of quoted spreads
library(ggplot2)
ggplot(quotes, 
       aes(x = ask_price - bid_price, fill = ticker)) +
  geom_histogram(bins = 100) +
  labs(title = "Histogram of nominal bid-ask spread",
       x = "Nominal bid-ask spread (pence)") +
  scale_x_continuous(breaks=1:12)


## -----------------------------------------------------------------------------------------------------------------------
# tv_quotes <- tv_quotes |> transmute(ticker, spread= ask_price - bid_price) |> as_tibble()
# ggplot(tv_quotes, aes(spread)) +
#   geom_histogram(bins = 100) +
#   labs(title = "Histogram of nominal bid-ask spread",
#        x = "Nominal bid-ask spread")



## -----------------------------------------------------------------------------------------------------------------------
tv_quotes |> 
  transmute(ticker, spread = ask_price - bid_price) |> 
  as_tibble() |>
  ggplot(aes(spread)) +
  geom_histogram(bins = 100) +
  labs(title = "Histogram of nominal bid-ask spread",
       x = "Nominal bid-ask spread")


## -----------------------------------------------------------------------------------------------------------------------
# Plot a histogram of missing quoted spreads
ggplot(quotes[is.na(ask_price - bid_price)], 
       aes(x = time, fill = ticker)) +
  geom_histogram(bins = 100) + 
  labs(title = "Histogram of missing spreads",
       x = "Time of Day (seconds past midnight)")


## -----------------------------------------------------------------------------------------------------------------------



## ----Fundamental value--------------------------------------------------------------------------------------------------
# Fundamental value
quotes[, midpoint := (bid_price + ask_price) / 2]


## -----------------------------------------------------------------------------------------------------------------------



## ----Ex ante liquidity--------------------------------------------------------------------------------------------------
# Measure the average quote-based liquidity
# This step calculates the mean of each of the market quality variables, for each 
# ticker-day (as indicated in `by = c("ticker", "date")`)
tick_size <- 0.5

quotes_liquidity <- quotes[, {
	quoted_spread = ask_price - bid_price
	list(quoted_spread_nom = mean(quoted_spread, na.rm = TRUE),
	     quoted_spread_rel = mean(quoted_spread / midpoint, na.rm = TRUE),
	     quoted_spread_tic = mean(quoted_spread / tick_size, na.rm = TRUE),
	     quoted_depth = mean(bid_depth * bid_price + ask_depth * ask_price, 
	                         na.rm = TRUE) / 2)},
	by = c("ticker", "date")]

# Output the liquidity measures, averaged across the five trading days for each ticker. 
quotes_liquidity[, 
	list(quoted_spread_nom = round(mean(quoted_spread_nom), digits = 2),
	     quoted_spread_rel = round(mean(quoted_spread_rel) * 1e4, digits = 2),
	     quoted_spread_tic = round(mean(quoted_spread_tic), digits = 2),
	     quoted_depth = round(mean(quoted_depth) * 1e-5), digits = 2), 
	by = "ticker"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Duration weighted ex ante liquidity--------------------------------------------------------------------------------
# Calculate quote durations
quotes[, duration := c(diff(time), 0), by = c("ticker", "date")]

# Measure the duration-weighted average quote-based liquidity
# The specified subset excludes quotes for which no duration can be calculated
quotes_liquidity_dw <- quotes[!is.na(duration), {
	quoted_spread = ask_price - bid_price
	list(quoted_spread_nom = weighted.mean(quoted_spread, 
	                                       w = duration, na.rm = TRUE),
	     quoted_spread_rel = weighted.mean(quoted_spread / midpoint, 
	                                       w = duration, na.rm = TRUE),
	     quoted_spread_tic = weighted.mean(quoted_spread / tick_size, 
	                                       w = duration, na.rm = TRUE),
	     quoted_depth = weighted.mean(bid_depth * bid_price + ask_depth * ask_price, 
	                                  w = duration, na.rm = TRUE) / 2)},
    by = c("ticker", "date")]

# Output liquidity measures, averaged across the five trading days for each ticker 
quotes_liquidity_dw[, 
	list(quoted_spread_nom = round(mean(quoted_spread_nom), digits = 2),
		 quoted_spread_rel = round(mean(quoted_spread_rel) * 1e4, digits = 2),
		 quoted_spread_tic = round(mean(quoted_spread_tic), digits = 2),
		 quoted_depth = round(mean(quoted_depth) * 1e-5, digits = 2)), 
	by = "ticker"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Last entry per unit of time----------------------------------------------------------------------------------------
# Retain only the last observation per unit of time
# The function `duplicated` returns `TRUE` if the observation is a duplicate of another 
# observation based on the columns given in the `by` option, and `FALSE` otherwise.
# The option `fromLast = TRUE` ensures that the last rather than the first observation 
# in each millisecond that returns `FALSE`.
quotes <- quotes[!duplicated(quotes, fromLast = TRUE, by = c("ticker", "date", "time"))]


## -----------------------------------------------------------------------------------------------------------------------



## ----Merging LSE and TQE------------------------------------------------------------------------------------------------
# Merge quotes from two venues trading the same security
# In the `merge` function, we add exchange suffixes to the variable names to keep track of 
# which quote comes from which exchange, using the option `suffixes`. 
# The option `all = TRUE` specifies that unmatched observations from both sets of quotes 
# should be retained (known as an outer join). 
venues <- c("_lse", "_tqe")

quotes_lse <- quotes[ticker == "PRU.L", .SD, .SDcols = c("date", "time", lob_variables)]
quotes_tqe <- quotes[ticker == "PRUl.TQ", .SD, .SDcols = c("date", "time", lob_variables)]

quotes_ebbo <- merge(quotes_lse, quotes_tqe, 
                     by = c("date", "time"), 
                     suffixes = venues, 
                     all = TRUE, sort = TRUE)

# Forward-fill the quoted prices and depth for each exchange 
local_lob_variables <- paste0(lob_variables, rep(venues, each = 4))

quotes_ebbo[, (local_lob_variables) := lapply(.SD, nafill, type = "locf"), 
  .SDcols = (local_lob_variables),
	by = "date"]


## -----------------------------------------------------------------------------------------------------------------------



## ----EBBO---------------------------------------------------------------------------------------------------------------
# Obtain the EBBO prices and depths
quotes_ebbo[, best_bid_price := pmax(bid_price_lse, bid_price_tqe)]
quotes_ebbo[, best_ask_price := pmin(ask_price_lse, ask_price_tqe)]
quotes_ebbo[, best_bid_depth := bid_depth_lse * (bid_price_lse == best_bid_price) + 
              bid_depth_tqe * (bid_price_tqe == best_bid_price)]
quotes_ebbo[, best_ask_depth := ask_depth_lse * (ask_price_lse == best_ask_price) +
              ask_depth_tqe * (ask_price_tqe == best_ask_price)]

# Drop local exchange variables and objects
quotes_ebbo[, (local_lob_variables) := NULL]
rm(quotes_lse, quotes_tqe, quotes, local_lob_variables)


## -----------------------------------------------------------------------------------------------------------------------



## ----EBBO fundamental value---------------------------------------------------------------------------------------------
# Calculate EBBO midpoints
quotes_ebbo[, midpoint := (best_bid_price + best_ask_price) / 2]


## -----------------------------------------------------------------------------------------------------------------------



## ----EBBO quoted spread-------------------------------------------------------------------------------------------------
# Output an overview of the EBBO nominal quoted bid-ask spread 
table(quotes_ebbo$best_ask_price - quotes_ebbo$best_bid_price)


## ----EBBO quote filters-------------------------------------------------------------------------------------------------
# Flag problematic consolidated quotes
threshold <- 0.05

quotes_ebbo[, c("crossed", "locked", "large") := {
	quoted_spread = (best_ask_price - best_bid_price)
	
    list(quoted_spread < 0, quoted_spread == 0, quoted_spread / midpoint > threshold)}]

# Count the incidence of the consolidated quote flags
quotes_ebbo_filters  <- quotes_ebbo[, 
    list(crossed = mean(crossed, na.rm = TRUE),
         locked = mean(locked, na.rm = TRUE),
         large = mean(large, na.rm = TRUE))]

# Output the fraction of quotes that is flagged
quotes_ebbo_filters[, 
	lapply(.SD * 100, round, digits = 2), .SDcols = c("crossed", "locked", "large")]


## -----------------------------------------------------------------------------------------------------------------------



## ----EBBO ex ante liquidity---------------------------------------------------------------------------------------------
# Measure the duration-weighted consolidated quotes liquidity
# Because this is the EBBO, there is no variation across tickers, but different averages 
# across dates are considered
quotes_ebbo[, duration := c(diff(time), 0), by = "date"]

# Note that the subset used here excludes crossed and locked quotes
quotes_liquidity_ebbo_dw <- quotes_ebbo[!crossed & !locked & !large, {
	quoted_spread = best_ask_price - best_bid_price
	
	list(quoted_spread_nom = weighted.mean(quoted_spread, 
	                                       w = duration, na.rm = TRUE),
	     quoted_spread_rel = weighted.mean(quoted_spread / midpoint, 
	                                       w = duration, na.rm = TRUE),
	     quoted_spread_tic = weighted.mean(quoted_spread / tick_size,
	                                       w = duration, na.rm = TRUE),
	     quoted_depth = weighted.mean(best_bid_depth * best_bid_price + 
	                                  best_ask_depth * best_ask_price, 
	                                  w = duration, na.rm = TRUE) / 2)}, 
	by = "date"]

# Output the liquidity measures, averaged across the five trading days 
quotes_liquidity_ebbo_dw[, 
	list(quoted_spread_nom = round(mean(quoted_spread_nom), digits = 2),
	     quoted_spread_rel = round(mean(quoted_spread_rel * 1e4), digits = 2),
	     quoted_spread_tic = round(mean(quoted_spread_tic), digits = 2),
	     quoted_depth = round(mean(quoted_depth * 1e-6), digits = 2))]


## -----------------------------------------------------------------------------------------------------------------------



## ----EBBO data export---------------------------------------------------------------------------------------------------
# Export the consolidated quotes
fwrite(quotes_ebbo, file = "quotes_ebbo.csv.gz")


## -----------------------------------------------------------------------------------------------------------------------



## ----Load trade data----------------------------------------------------------------------------------------------------
# Load the trade data
trades_url <- "http://tinyurl.com/3f4wfj9c"
trades <- fread(trades_url)

# View the trade data
trades

# Rename the variables
raw_trade_variables <- c("#RIC", "Date-Time", "Exch Time", "GMT Offset", 
                         "Price", "Volume", "MMT Classification")
new_trade_variables <- c("ticker", "date_time", "exch_time", "gmt_offset",
                         "price", "size", "mmt")
setnames(trades,
         old = raw_trade_variables, 
         new = new_trade_variables)


## -----------------------------------------------------------------------------------------------------------------------



## ----Trade dates and times----------------------------------------------------------------------------------------------
# Extract dates
trades[, date := as.Date(date_time)]

# Convert time stamps to numeric format, expressed in seconds past midnight
trades[, time := {
	time_elements = strsplit(exch_time, split = ":")
	time_elements = do.call(rbind, time_elements)
	time = as.numeric(time_elements[, 1]) * 3600 + 
	  as.numeric(time_elements[, 2]) * 60 + 
	  as.numeric(time_elements[, 3]) +
	  gmt_offset * 3600
	list(time)}]

# Delete raw time variables
trades[, c("date_time", "exch_time", "gmt_offset") := NULL]

# Retain trades from the continuous trading sessions
trades <- trades[time > (open_time + 60) & time < (close_time - 60) & 
				    (time < (intraday_auction_time - 60) | 
				     time > (intraday_auction_time + 3 * 60))]


## -----------------------------------------------------------------------------------------------------------------------



## ----Trade price plot---------------------------------------------------------------------------------------------------
# Plot the trade prices
ggplot(trades, 
       aes(time, price)) + 
       geom_line() + 
       facet_wrap(.~date) + 
       labs(title = "Trade prices",  y = "Price", x = "Time (seconds past midnight)")


## -----------------------------------------------------------------------------------------------------------------------



## ----MMT overview-------------------------------------------------------------------------------------------------------
# Output an overview of the MMT codes
# The function `substr` is used here to extract the first two characters of the MMT code
table(substr(trades[, mmt], start = 1, stop = 2), trades[, price] < 100)


## -----------------------------------------------------------------------------------------------------------------------



## ----Trade filtering MMT plot-------------------------------------------------------------------------------------------
# Define a subset with continuous trades only
LOB_continuous_trades <- substr(trades[, mmt], start = 1, stop = 2) == "12"

# Plot the prices of continuous trades
ggplot(trades[LOB_continuous_trades], 
       aes(time, price)) + 
  geom_line() + 
  facet_wrap(.~date) + 
  labs(title = "Trade prices", y = "Price", x = "Time (seconds past midnight)")


## -----------------------------------------------------------------------------------------------------------------------



## ----Trade price outliers-----------------------------------------------------------------------------------------------
# View trades with low prices
trades[price < 100]


## -----------------------------------------------------------------------------------------------------------------------



## ----Trade filtering MMT------------------------------------------------------------------------------------------------
# Retain continuous LOB trades only
trades <- trades[LOB_continuous_trades]


## -----------------------------------------------------------------------------------------------------------------------



## ----Matching trades to prevailing quotes-------------------------------------------------------------------------------
# Load the EBBO quote data
quotes_ebbo <- fread(file = "quotes_ebbo.csv.gz")

# Adjust quote time stamps by one microsecond
setnames(quotes_ebbo, old = "time", new = "quote_time")
quotes_ebbo[, time := quote_time + 0.000001]

# Sort trades and quotes (this specifies the matching criteria for the merge function)
setkeyv(trades, cols = c("date", "time"))
setkeyv(quotes_ebbo, cols = c("date", "time"))

# Match trades to quotes prevailing at the time of trade 
# The rolling is done only for the last of the matching variables, in this case "time"
# `mult = "last"` specifies that if there are multiple matches with identical timestamps, 
# the last match is retained
trades <- quotes_ebbo[trades, roll = TRUE, mult = "last"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Trade filtering----------------------------------------------------------------------------------------------------
# Flag trades that should be included
trades[, include := !crossed & !locked & !large & !is.na(size) & size > 0 &  
	   	            !is.na(price) & price > 0 & !is.na(midpoint) & midpoint > 0]

# Report trade filtering stats
trades_filters <- trades[, 
	list(crossed = mean(crossed, na.rm = TRUE),
	     locked = mean(locked, na.rm = TRUE),
    	 large = mean(large, na.rm = TRUE),
    	 no_price = mean(is.na(price) | price == 0),
    	 no_size = mean(is.na(size) | size == 0),
    	 no_quotes = mean(is.na(midpoint) | midpoint <= 0),
    	 included = mean(include)), 
	by = "ticker"]

trades_filters[,
  lapply(.SD * 100, round, digits = 2), 
  .SDcols = c("crossed", "locked", "large", "no_price", "no_size", "no_quotes", "included"),
  by = "ticker"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Direction of trade-------------------------------------------------------------------------------------------------
# Quote rule (the trade price is compared to the midpoint at the time of the trade)
trades[, quote_diff := sign(price - midpoint)]

# Tick rule (each trade is matched to the closest preceding trade price change)
price_tick <- data.table(date = trades$date,
                         time = trades$time,
                         price_change = c(NA, sign(diff(trades$price))))

# Retain trades that imply a trade price change
price_tick <- price_tick[price_change != 0]

# Merge trades and trade price changes
setkeyv(trades, c("date", "time"))
setkeyv(price_tick, c("date", "time"))
trades <- price_tick[trades, roll = TRUE, mult = "last"]

# Apply the Lee-Ready (1991) algorithm
trades[, dir := {
  # 1st step: quote rule
  direction = quote_diff
  # 2nd step: tick rule
  no_direction = is.na(direction) | direction == 0
	direction[no_direction] = price_change[no_direction] 
	
	list(direction)},
	by = "date"]

table(trades$ticker, trades$dir)


## -----------------------------------------------------------------------------------------------------------------------



## ----Effective spread vw------------------------------------------------------------------------------------------------
# Measure average liquidity for each stock-day

# First order the table
setorder(trades, ticker, date, time)

trades_liquidity <- trades[include == TRUE, {             
  list(effective_spread = weighted.mean(2 * dir * (price - midpoint) / midpoint, 
  									  w = size),
       volume = sum(price * size))
  },
	by = c("ticker", "date")]

# Output liquidity measures, averaged across the five trading days for each ticker
trades_liquidity[, 
	list(effective_spread = round(mean(effective_spread * 1e4), digits = 2),
	     volume = round(mean(volume * 1e-8), digits = 2)), 
	by = "ticker"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Depth-weighted effective spread vw---------------------------------------------------------------------------------
# Measure average liquidity for each stock-day, including the effective spread 
# relative to the weighted midpoint
trades_liquidity <- trades[include == TRUE, {
  midpoint_w = (best_bid_price * best_ask_depth + best_ask_price * best_bid_depth) / 
			     (best_ask_depth + best_bid_depth)
	list(effective_spread = weighted.mean(2 * dir * (price - midpoint) / midpoint, 
								          w = size),
  		 effective_spread_w = weighted.mean(2 * dir * (price - midpoint_w) / midpoint, 
  		 							        w = size))},
    by = c("ticker", "date")]

# Output liquidity measures, averaged across the five trading days for each ticker
trades_liquidity[, 
	list(effective_spread = round(mean(effective_spread * 1e4), digits = 2),
	     effective_spread_w = round(mean(effective_spread_w * 1e4), digits = 2)), 
	by = "ticker"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Preparation for the effective spread decomposition-----------------------------------------------------------------
# Adjust quote time stamps by 60 seconds
quotes_ebbo <- quotes_ebbo[, time := quote_time - 60]

# Rename variables to indicate that they correspond to quotes 1 minute after the trade
# The function `paste0` adds the suffix "_1min" to each variable
setnames(quotes_ebbo, 
         old = c("midpoint", "crossed", "locked", "large"), 
         new = paste0(c("midpoint", "crossed", "locked", "large"), "_1min"))

# Merge trades and quotes
setkeyv(quotes_ebbo, cols = c("date", "time"))
setkeyv(trades, cols = c("date", "time"))
trades <- quotes_ebbo[trades, roll = TRUE, mult = "last"]

# Flag valid future quotes
trades[, include_1min := !crossed_1min & !locked_1min & !large_1min]


## -----------------------------------------------------------------------------------------------------------------------



## ----Effective spread decomposition-------------------------------------------------------------------------------------
# Measure trade-based liquidity
effective_spread_decomposition <- trades[include & include_1min, {
	list(effective_spread = weighted.mean(2 * dir * (price - midpoint) / midpoint, 
								  w = size),
	     price_impact = weighted.mean(2 * dir * (midpoint_1min - midpoint) / midpoint, 
  		 						 w = size),
  		 realized_spread = weighted.mean(2 * dir * (price - midpoint_1min) / midpoint, 
  		 						 w = size))}, 
	by = c("ticker", "date")]

# Output the average liquidity measures
effective_spread_decomposition[, 
	list(effective_spread = round(mean(effective_spread * 1e4), digits = 2),
	     price_impact = round(mean(price_impact * 1e4), digits = 2),
	     realized_spread = round(mean(realized_spread * 1e4), digits = 2)), 
	by = "ticker"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Load EBBO data-----------------------------------------------------------------------------------------------------
quotes_ebbo <- fread(file = "quotes_ebbo.csv.gz")

# Delete variables
quotes_ebbo[, c("best_bid_price", "best_bid_depth", "best_ask_price", 
                "best_ask_depth", "duration") := NULL]


## -----------------------------------------------------------------------------------------------------------------------



## ----Equispaced quote data, 1 sec---------------------------------------------------------------------------------------
# Create an equispaced time grid
sampling_freq  <- 1 # 1 second grid
open_time <- 8 * 3600
close_time <- 16.5 * 3600

# The function `seq` creates a sequence of discrete numbers
# the `by` option defines the increment of the sequence, which is here 1 second
time_grid <- seq(from = open_time + 60, to = close_time - 60, by = sampling_freq)

# Repeat the time grid for each date and sort it by date and time
dates <- unique(quotes_ebbo$date)
quotes_1sec <- expand.grid(date = dates, time = time_grid)

# Make it a data.table
quotes_1sec <- data.table(quotes_1sec, key = c("date", "time"))

# View the time grid
quotes_1sec


## -----------------------------------------------------------------------------------------------------------------------



## ----1-second returns---------------------------------------------------------------------------------------------------
# Sort the quotes
setkeyv(quotes_ebbo, cols = c("date", "time"))

# Merge the time grid with the quotes
quotes_1sec <- quotes_ebbo[quotes_1sec, roll = TRUE]

# Set problematic quotes to NA
quotes_1sec$midpoint[quotes_1sec$crossed|quotes_1sec$locked| quotes_1sec$large] <- NA

# Calculate returns, expressed in basis points
# The function `diff` returns the first difference of a time series, which in this case 
# is the log of the midpoint. 
# For each date, a leading `NA` is added to make the resulting vector fit the number of 
# observations in `quotes_1sec`. 
quotes_1sec[, return := 1e4 * c(NA, diff(log(midpoint))), by = "date"]


## -----------------------------------------------------------------------------------------------------------------------



## ----1-second return efficiency-----------------------------------------------------------------------------------------
# Measure market efficiency and volatility 
efficiency_1sec <- quotes_1sec[order(date, time), 
  list(return_corr_1sec = cor(return, shift(return, n = 1, type = "lag"), 
                              use = "complete.obs"),
	     realized_vol_1sec = mean(return^2, na.rm = TRUE),
	     return_var_1sec = var(return, na.rm = TRUE)), 
	by = "date"]

# Output an overview of the average efficiency and volatility 
efficiency_1sec[, 
  list(return_corr_1sec = round(mean(return_corr_1sec), digits = 2),
       realized_vol_1sec = round(mean(realized_vol_1sec), digits = 2),
       return_var_1sec = round(mean(return_var_1sec), digits = 2))]


## -----------------------------------------------------------------------------------------------------------------------



## ----Equispaced quote data, 10 sec--------------------------------------------------------------------------------------
# Subset the 1-second price grid to get the 10-second price grid 
sampling_freq <- 10 # 10 second grid
time_grid <- seq(from = open_time + 60, to = close_time - 60, by = sampling_freq)
quotes_10sec <- quotes_1sec[time %in% time_grid,]

# Calculate returns at the 10-second frequency, expressed in basis points
quotes_10sec[, return := 1e4 * c(NA, diff(log(midpoint))), by = "date"]

# Calculate the return variance at the 10-second frequency, daily
efficiency_10sec <- quotes_10sec[, 
                      list(return_var_10sec = var(return, na.rm = TRUE)), 
                      by = "date"]


## -----------------------------------------------------------------------------------------------------------------------



## ----Variance ratios----------------------------------------------------------------------------------------------------
# Merge the efficiency measures of different return sampling frequencies
efficiency <- efficiency_1sec[efficiency_10sec]

# Obtain the variance ratio
efficiency[, var_ratio := return_var_10sec / (10 * return_var_1sec)]

# Output an overview of the average variance ratio
efficiency[, 
  list(return_var_1sec = round(mean(return_var_1sec), digits = 3),
       return_var_10sec = round(mean(return_var_10sec), digits = 3),
       var_ratio = round(mean(var_ratio), digits = 3))]


## -----------------------------------------------------------------------------------------------------------------------



## ----include= FALSE, eval= FALSE----------------------------------------------------------------------------------------
## # Convert to one R Script
## infile <- "tidy-market-microstructure.qmd"
## 
## knitr::purl(infile)
## 

