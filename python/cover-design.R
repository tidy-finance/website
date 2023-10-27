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

cover_python <- data_plot |> 
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

ggsave(
  plot = cover_python,
  width = 10,
  height = 6,
  filename = "images/cover_python.png",
  bg = "white"
)


# Macro ------------------------------------------------

library(tidyverse)
library(RSQLite)
library(wesanderson)

tidy_finance <- dbConnect(
  SQLite(),
  "data/tidy_finance_r.sqlite",
  extended_types = TRUE
)

macro_predictors <- tbl(tidy_finance, "macro_predictors") |> 
  collect()

macro_predictors

data_plot <- macro_predictors |> 
  pivot_longer(cols = rp_div:infl) |> 
  group_by(name) |> 
  mutate(
    value = scale(value) + 1,
    # value = value - lag(value, order_by = month)
  ) |> 
  group_by(year = floor_date(month, "year"), name) |>
  summarize(
    total = mean(value, na.rm = TRUE),
    vola = sd(value, na.rm = TRUE),
    .groups = "keep"
  ) |> 
  mutate(
    day = 2 * pi * (1:n()) / 252,
    ymin = pmin(1 + total, 1),
    ymax = pmax(1 + total, 1)
  ) |> 
  ungroup() |> 
  mutate(vola = ntile(vola, 12))

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
  facet_wrap(~name, nrow = 2, scales = "free_y") +
  scale_fill_manual(values = colors)


# Industry portfolios ----------------------------------

library(tidyverse)
library(RSQLite)
library(wesanderson)
library(frenchdata)

industries_raw <- download_french_data("12 Industry Portfolios [Daily]")

industries <- industries_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(-date, ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) 

data_plot <- industries |> 
  pivot_longer(cols = -date) |> 
  # group_by(name) |> 
  # mutate(
  #   value = scale(value),
  # ) |> 
  group_by(year = floor_date(date, "year"), name) |>
  arrange(date) |> 
  summarize(
    total = prod(1+value),
    vola = sd(value),
    .groups = "keep"
  ) |> 
  mutate(
    day = 2 * pi * (1:n()),
    ymin = pmin(1 + total, 1),
    ymax = pmax(1 + total, 1)
  ) |> 
  ungroup() |> 
  mutate(vola = ntile(vola, 24))

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

cover_python <- data_plot |> 
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
  facet_wrap(~name, nrow = 3, scales = "free_y") +
  scale_fill_manual(values = colors)

ggsave(
  plot = cover_python,
  width = 10,
  height = 7,
  filename = "images/cover-python-flowers.png",
  bg = "white"
)
