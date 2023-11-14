library(tidyverse)
library(wesanderson)
library(frenchdata)

industries_raw <- download_french_data("12 Industry Portfolios [Daily]")

industries <- industries_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(-date, ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |> 
  filter(date >= "1927-01-01" & date <= "2022-12-31")

industries_long <- industries |> 
  pivot_longer(cols = -date) 

industries_order <- sort(unique(industries_long$name))

data_plot <-industries_long |> 
  mutate(name = factor(name, levels = industries_order)) |> 
  group_by(year = floor_date(date, "year"), name) |>
  arrange(date) |> 
  summarize(
    total = mean(value),
    vola = sd(value),
    .groups = "keep"
  ) |> 
  ungroup() |> 
  mutate(vola = ntile(vola, 42))

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
    panel.spacing = unit(-75, "points"),
    plot.margin = unit(c(75, 0, 75, 0), "points") # top, right, bottom, left
  ) +
  cp +
  facet_wrap(~name, nrow = 2, scales = "free_y") +
  scale_fill_manual(values = colors)

cover_python

ggsave(
  plot = cover_python,
  width = 10,
  height = 6,
  filename = "images/cover-python.png",
  bg = "white"
)
