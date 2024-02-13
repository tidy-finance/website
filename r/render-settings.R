if (Sys.info()["machine"] != "arm64") {
  Sys.setlocale("LC_TIME", "English") 
}
Sys.setenv(LANG = "en")

options(
  tibble.print_max = 5,
  tibble.print_min = 5,
  tibble.width = 69,
  htmltools.dir.version = FALSE,
  formatR.indent = 2,
  digits = 3,
  width = 69,
  crayon.enabled = FALSE
)

# ggplot2 global theme
library(ggplot2)
theme_set(theme_bw() + theme(legend.position = "bottom"))

pal <- grDevices::colorRampPalette(list(
  "#3B9AB2",
  "#78B7C5",
  "#EBCC2A",
  "#E1AF00",
  "#F21A00"
))

scale_colour_continuous <- function(...) {
  scale_color_gradientn(
    colours = pal(256),
    ...
  )
}
scale_colour_discrete <- function(...) {
  discrete_scale("colour",
                 scale_name = "pal",
                 palette = pal, ...
  )
}

scale_fill_continuous <- function(...) {
  scale_fill_gradientn(
    colours = pal(256),
    ...
  )
}
scale_fill_discrete <- function(...) {
  discrete_scale("fill",
                 scale_name = "pal",
                 palette = pal, ...
  )
}