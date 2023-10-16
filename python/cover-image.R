library(tidyverse)
library(RSQLite)
library(wesanderson)
library(frenchdata)

factors_ff5_daily_raw <- download_french_data("Fama/French 5 Factors (2x3) [Daily]")

factors_ff5_daily <- factors_ff5_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(c(RF, `Mkt-RF`, SMB, HML, CMA, RMW), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) 

# data_plot <- factors_ff5_daily |>
#   pivot_longer(cols = mkt_excess:rf) |> 
#   group_by(year = floor_date(date, "year"), name) |>
#   summarize(
#     total = prod(1+value),
#     vola = sd(value),
#     .groups = "keep"
#   ) |> 
#   mutate(
#     day = 2 * pi * (1:n()) / 252,
#     ymin = pmin(1 + total, 1),
#     ymax = pmax(1 + total, 1)
#   ) |> 
#   ungroup() |> 
#   mutate(
#     name = factor(name, levels = c("smb", "mkt_excess", "hml", "rmw", "rf", "cma"))
#   )

data_plot <- factors_ff5_daily |>
  pivot_longer(cols = mkt_excess:rf) |> 
  group_by(year = floor_date(date, "year"), name) |>
  summarize(
    total = prod(1+value) - 1,
    vola = sd(value),
    .groups = "keep"
  ) |> 
  mutate(
    day = 2 * pi * (1:n()) / 252,
    ymin = pmin(1 + total, 1),
    ymax = pmax(1 + total, 1)
  ) |> 
  ungroup() |> 
  mutate(
    name = factor(name, levels = c("smb", "mkt_excess", "hml", "rmw", "rf", "cma"))
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
  facet_wrap(~name) +
  scale_fill_manual(values = colors)

