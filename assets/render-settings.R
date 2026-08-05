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

# Data frame printing --------------------------------------------------------
# The Python tabs show polars data frames as HTML tables, so we style R data
# frames the same way. `df-print: kable` in _quarto.yml takes care of plain data
# frames, but it prints *every* row, which is not an option for the tibbles in
# this book (some have millions of rows). The knit_print() method below keeps
# the five-row preview that print.tbl_df() gives us, reports the full
# dimensions above the table like polars does, and only then calls kable().
local({

  # Formats one column the way the tibble would print it: pillar keeps the
  # significant digits set above, avoids scientific notation for large counts,
  # and summarizes list columns as, e.g., "tibble [299 x 3]".
  format_column <- function(column) {
    if (is.list(column) && !is.data.frame(column)) {
      return(escape_html(as.character(pillar::obj_sum(column))))
    }
    shaft <- pillar::pillar_shaft(column)
    width <- attr(shaft, "width")
    formatted <- if (is.null(width)) format(shaft) else format(shaft, width = width)
    escape_html(trimws(gsub("\033\\[[0-9;]*m", "", as.character(formatted))))
  }

  # Values are placed in a Markdown table, so angle brackets would otherwise be
  # read as inline HTML by Pandoc.
  escape_html <- function(values) {
    values <- gsub("<", "&lt;", values, fixed = TRUE)
    gsub(">", "&gt;", values, fixed = TRUE)
  }

  kable_tibble <- function(x, n = getOption("tibble.print_min", 5)) {
    cells <- lapply(utils::head(x, n), format_column)
    if (nrow(x) > n) {
      cells <- lapply(cells, function(column) c(column, "\u2026"))
    }
    preview <- as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
    table <- knitr::kable(
      preview,
      row.names = FALSE,
      col.names = names(x),
      align = unname(ifelse(vapply(x, is.numeric, logical(1)), "r", "l"))
    )
    shape <- sprintf("shape: (%s, %s)", format(nrow(x), big.mark = ","), ncol(x))
    paste0("<small>", shape, "</small>\n\n", paste(table, collapse = "\n"), "\n")
  }

  knit_print_tibble <- function(x, options = NULL, ...) {
    if (!knitr::is_html_output()) {
      return(knitr::normal_print(x))
    }
    out <- try(kable_tibble(x), silent = TRUE)
    if (inherits(out, "try-error")) {
      return(knitr::normal_print(x))
    }
    knitr::asis_output(out)
  }

  # rmarkdown registers knit_print.data.frame for df-print, so the method for
  # the more specific tbl_df class wins for tibbles and grouped tibbles.
  register <- function(...) {
    registerS3method(
      "knit_print", "tbl_df", knit_print_tibble,
      envir = asNamespace("knitr")
    )
  }
  if (isNamespaceLoaded("knitr")) {
    register()
  } else {
    setHook(packageEvent("knitr", "onLoad"), register)
  }
})
