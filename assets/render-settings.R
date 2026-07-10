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
