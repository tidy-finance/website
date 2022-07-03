library(tidyverse)
library(RSQLite)
library(lubridate)
library(broom)
library(sandwich)

# Load data
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite",
                          extended_types = TRUE
)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") %>% 
  collect()

compustat <- tbl(tidy_finance, "compustat") %>%
  collect()

beta <- tbl(tidy_finance, "beta") %>% 
  collect()

# Prepare sample
# Treat all variables the same: sorting just once a year
bm <- compustat %>%
  mutate(month = floor_date(ymd(datadate), "month")) %>%
  left_join(crsp_monthly, by = c("gvkey", "month")) %>%
  left_join(beta, by = c("permno", "month")) %>% 
  mutate(bm = be / mktcap,
         log_mktcap = log(mktcap),
         sorting_date = month %m+% months(6)) %>% 
  select(gvkey, sorting_date, beta = beta_daily, bm, log_mktcap) %>% 
  drop_na()

data_fama_macbeth <- crsp_monthly %>% 
  left_join(bm, by = c("gvkey", "month"="sorting_date")) %>% 
  group_by(permno) %>% 
  arrange(month) %>% 
  fill(c(beta, bm, log_mktcap), .direction = "down") %>% 
  ungroup() %>% 
  left_join(crsp_monthly %>% 
              select(permno, month, ret_excess_lead = ret) %>% 
              mutate(month = month %m-% months(1)), by = c("permno", "month")) %>% 
  select(permno, month, ret_excess_lead, beta, log_mktcap, bm) %>% 
  drop_na()

# Cross-sectional regressions
regressions <- data_fama_macbeth %>% 
  nest(cross_section = c(permno, ret_excess_lead, beta, log_mktcap, bm)) %>% 
  mutate(model = map(cross_section, ~lm(ret_excess_lead ~ beta + log_mktcap + bm, data = .))) %>% 
  mutate(estimates = map(model, tidy)) %>% 
  unnest(estimates)
  
# Time series aggregation
regressions %>% 
  group_by(factor = term) %>% 
  summarise(risk_premium = mean(estimate), # scaled to percent to get results on two-digit level
            t_statistic = mean(estimate) / sd(estimate) * sqrt(n()))

# Newey-West standard errors
regressions_for_newey_west <- regressions %>% 
  select(month, factor = term, estimate) %>% 
  nest(data = c(month, estimate)) %>% 
  mutate(model = map(data, ~lm(estimate ~ 1, .)),
         mean = map(model, tidy))

# Suppress prewhitening for the OG Newey-west 1987
regressions_for_newey_west %>% 
  mutate(newey_west_se = map_dbl(model, ~sqrt(NeweyWest(., lag = 6, prewhite = FALSE)))) %>% 
  unnest(mean) %>% 
  mutate(newey_west_t_statistic = estimate / newey_west_se) %>% 
  select(factor, risk_premium = estimate, newey_west_t_statistic)

# Automatic bandwidth selection of Newey-west 1994
regressions_for_newey_west %>% 
  mutate(newey_west_se = map_dbl(model, ~sqrt(NeweyWest(.)))) %>% 
  unnest(mean) %>% 
  mutate(newey_west_t_statistic = estimate / newey_west_se) %>% 
  select(factor, risk_premium = estimate, newey_west_t_statistic)

