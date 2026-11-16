# English month and day names in dates (the locale is called "English" on Windows)
Sys.setlocale("LC_TIME", if (.Platform$OS.type == "windows") "English" else "C")
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
