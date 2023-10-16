library(tidyverse)
library(RSQLite)
library(wesanderson)
library(frenchdata)

factors_q_daily_link <-
  "https://www.global-q.org/uploads/1/2/2/6/122679606/q5_factors_daily_2022.csv"

factors_q_daily <- read_csv(factors_q_daily_link) 

factors_q_daily <- factors_q_daily |>
  mutate(DATE = ymd(DATE)) |>
  rename_with(~ str_remove(., "R_")) |>
  rename_with(~ str_to_lower(.)) |>
  mutate(across(-date, ~ . / 100))

factors_ff5_daily_raw <- download_french_data("Fama/French 5 Factors (2x3) [Daily]")

factors_ff5_daily <- factors_ff5_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML, CMA, RMW), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) 

data_plot <- factors_ff5_daily |>
  inner_join(factors_q_daily, by = "date") |> 
  pivot_longer(cols = mkt_excess:eg) |> 
  group_by(year = floor_date(date, "year"), name) |>
  summarize(
    total = prod(1+value),
    vola = sd(value),
    .groups = "keep"
  ) |> 
  mutate(
    day = 2 * pi * (1:n()) / 252,
    ymin = pmin(1 + total, 1),
    ymax = pmax(1 + total, 1)
  ) |> 
  ungroup() 

# data_plot <- factors_ff5_daily |>
#   inner_join(factors_q_daily, by = "date") |> 
#   pivot_longer(cols = mkt_excess:eg) |> 
#   group_by(year = floor_date(date, "year"), name) |>
#   summarize(
#     total = prod(1+value) - 1,
#     vola = sd(value),
#     .groups = "keep"
#   ) |> 
#   mutate(
#     day = 2 * pi * (1:n()) / 252,
#     ymin = pmin(1 + total, 1),
#     ymax = pmax(1 + total, 1)
#   ) |> 
#   ungroup() 

set.seed(12345)

data_plot <- data_plot |> 
  mutate(
    # name = factor(name, levels = sample(unique(data_plot$name), length(unique(data_plot$name)), replace = FALSE)),
    name = factor(name, levels = c("mkt_excess", "rf", "smb",  "me", "f", "mkt",
                                   "hml", "rmw", "cma", "ia", "eg",  "roe")),
    vola = ntile(vola, 6)
  )

levels <- data_plot |>
  distinct(vola) |>
  arrange(vola) |>
  pull(vola)

cp <- coord_polar(
  direction = -1,
  clip = "on"
)

cp$is_free <- function() TRUE
colors <- wes_palette("Zissou1",
                      length(levels),
                      type = "continuous"
)

data_plot |> 
  mutate(vola = factor(vola)) |>
  ggplot(aes(x = year, y = total, fill = vola)) +
  geom_col() +
  theme_void()+
  theme(
    strip.text.x = element_blank(),
    legend.position = "None",
    panel.spacing = unit(-5, "lines")
  ) +
  cp +
  facet_wrap(~name, nrow = 2) +
  scale_fill_manual(values = colors)

