library(tidyverse)
library(RSQLite)
library(sandwich)
library(broom)
library(truncnorm)

characteristics <- compustat |>
  mutate(month = floor_date(ymd(datadate), "month")) |>
  mutate(sorting_date = month %m+% months(6)) |> 
  select(-month)

data <- crsp_monthly |>
  left_join(characteristics, by = c("gvkey", "month" = "sorting_date")) |>
  left_join(crsp_monthly |>
              select(permno, month, ret_excess_lead = ret_excess) |>
              mutate(month = month %m-% months(1)),
            by = c("permno", "month")
  ) |>
  select(-c(cogs, itcb, pstk, pstkl, pstkrv, sale, seq, shrout, txditc, xint, xsga,
            ret)) |> 
  drop_na()

parameters <- data |> 
  pivot_longer(
    cols = -c(permno, gvkey, datadate, date, month, exchcd, exchange, 
              industry, siccd, year, ret_excess, ret_excess_lead),
    names_to = "measure"
  ) |>
  group_by(measure) |>
  summarize(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value),
    .groups = "drop"
  )

summary(lm(
  str_c("ret_excess ~", str_c(parameters$measure, collapse = "+")),
  data
))

# TODO: draw firm characteristics from truncated multivariate normal distribution!

stocks <- 1000
start_date <- as.Date("2000-01-01")
industries <- data |> 
  count(industry) |> 
  mutate(prob = n / sum(n))

exchanges <- data |> 
  count(exchange) |> 
  mutate(prob = n / sum(n))

data <- list()
for (j in seq_along(1:stocks)) {
  
  set.seed(j)
  years <- round(runif(1, 3, 13))
  nobs <- years * 12
  
  firm_identifiers <- tibble(
    permno = j,
    gvkey = j + 1000,
    exchange = sample(exchanges$exchange, 1, prob = exchanges$prob),
    industry = sample(industries$industry, 1, prob = industries$prob)
  )
  
  draw_firm_characteristics <- function() {
    parameters |> 
      rowwise() |> 
      mutate(draw = rtruncnorm(1, a = min, b = max, mean = mean, sd = sd)) |> 
      select(measure, draw) |> 
      pivot_wider(names_from = measure, values_from = draw)
  }
  
  1:nobs |> 
    map_df(draw_firm_characteristics)
  
  data[[j]] <- tibble(
    beta = rnorm(nobs, mean = 1.11, sd = 0.706),
    bm = rnorm(nobs, mean = 0.891, sd = 0.932),
    log_mktcap = rnorm(nobs, mean = 5.14, sd = 2.29),
    ret_excess_lead = 1.33 + 0.00840*beta + 0.138*bm + -0.114*log_mktcap + rnorm(nobs)
  ) |> 
    mutate(
      month = start_date %m+% months(row_number() - 1),
      mktcap = exp(log_mktcap),
      be = bm * mktcap
    )
}
data <- bind_rows(data)


summary(lm(
  "ret_excess_lead ~ beta + bm + log_mktcap",
  data
))

summary(lm(
  "ret_excess_lead ~ beta + bm + log_mktcap",
  data_fama_macbeth |> 
    mutate(ret_excess_lead = ret_excess_lead * 100)
))


risk_premiums <- data_fama_macbeth |>
  nest(data = c(ret_excess, beta, log_mktcap, bm, permno)) |>
  mutate(estimates = map(
    data,
    ~ broom::tidy(lm(ret_excess ~ beta + log_mktcap + bm, data = .x))
  )) |>
  unnest(estimates)

price_of_risk <- risk_premiums |>
  group_by(factor = term) |>
  summarize(
    risk_premium = mean(estimate) * 100,
    t_statistic = mean(estimate) / sd(estimate) * sqrt(n())
  )

regressions_for_newey_west <- risk_premiums |>
  select(month, factor = term, estimate) |>
  nest(data = c(month, estimate)) |>
  mutate(
    model = map(data, ~ lm(estimate ~ 1, .)),
    mean = map(model, tidy)
  )

price_of_risk_newey_west <- regressions_for_newey_west |>
  mutate(newey_west_se = map_dbl(model, ~ sqrt(NeweyWest(.)))) |>
  unnest(mean) |>
  mutate(t_statistic_newey_west = estimate / newey_west_se) |>
  select(factor,
         risk_premium = estimate,
         t_statistic_newey_west
  )

left_join(price_of_risk,
          price_of_risk_newey_west |>
            select(factor, t_statistic_newey_west),
          by = "factor"
)
