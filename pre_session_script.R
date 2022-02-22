Sys.setlocale("LC_TIME", "English")

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = TRUE,
  cache = FALSE,
  fig.align = "center",
  width = 81
)

options(tibble.print_max = 5,
        tibble.print_min = 5,
        digits = 3,
        width = 81 # http://oreillymedia.github.io/production-resources/styleguide/#code
)

# ggplot2 global theme
library(ggplot2)
theme_set(theme_bw() + theme(legend.position = "bottom"))

library(wesanderson)
assign("scale_colour_continuous ", function(..., values = wes_palette("Zissou1", 50, type = "continuous")) ggplot2::scale_colour_manual(..., values = values), globalenv())
assign("scale_colour_discrete", function(..., values = wes_palette("Zissou1", 50, type = "continuous")) ggplot2::scale_colour_manual(..., values = values), globalenv())
assign("scale_fill_discrete", function(..., values = wes_palette("Zissou1", 50, type = "continuous")) ggplot2::scale_fill_manual(..., values = values), globalenv())