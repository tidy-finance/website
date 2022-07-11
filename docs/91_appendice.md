# References {-}

<div id="refs"></div>

# (APPENDIX) Appendix {-} 

# Cover design


```r
library(tidyverse)
library(RSQLite)
library(wesanderson)

tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite", extended_types = TRUE)
mfac <- tbl(tidy_finance, "factors_ff_daily") %>%
  collect()

cp <- coord_polar(direction = -1, clip = "on")
cp$is_free <- function() TRUE

plot_data <- mfac %>%
  select(date, mkt_excess) %>%
  group_by(year = lubridate::floor_date(date, "year")) %>%
  mutate(group_id = cur_group_id())

plot_data <- plot_data %>%
  mutate(
    group_id = if_else(group_id >= 28, group_id + 4, group_id + 0),
    group_id = if_else(group_id >= 36, group_id + 4, group_id + 0),
    group_id = if_else(group_id >= 44, group_id + 4, group_id + 0)
  ) %>%
  bind_rows(plot_data %>%
    filter(group_id %in% c(28:31, 36:39, 44:47)) %>%
    mutate(mkt_excess = NA)) %>%
  group_by(group_id) %>%
  mutate(
    day = 2 * pi * (1:n()) / 252,
    ymin = pmin(1 + mkt_excess, 1),
    ymax = pmax(1 + mkt_excess, 1),
    vola = sd(mkt_excess)
  ) %>%
  filter(year >= "1961-01-01")

colors <- wes_palette("Zissou1", n_groups(plot_data), type = "continuous")
levels <- plot_data %>%
  distinct(group_id, vola) %>%
  arrange(vola) %>%
  pull(vola)

plot <- plot_data %>%
  mutate(vola = factor(vola, levels = levels)) %>%
  ggplot() +
  aes(x = day, y = mkt_excess, group = group_id, fill = vola) +
  cp +
  geom_ribbon(aes(
    ymin = ymin,
    ymax = ymax,
    fill = as_factor(vola)
  ), alpha = 0.90) +
  theme_void() +
  facet_wrap(~group_id, ncol = 8, scales = "free") +
  theme(
    strip.text.x = element_blank(),
    legend.position = "None",
    panel.spacing = unit(-5, "lines")
  ) +
  scale_fill_manual(values = colors)

# ggsave(
#   plot = plot, width = 8, height = 9,
#   filename = "cover.jpg", bg = "white"
# )
```
