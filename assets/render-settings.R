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
# frames the same way. `df-print: kable` in _quarto.yml switches the printing on,
# but on its own it prints *every* row, which is not an option for the data
# frames in this book (some have millions of rows). The knit_print() method
# below keeps the five-row preview that print.tbl_df() gives us, reports the
# full dimensions above the table like polars does, and only then calls kable().
local({

  # Formats one column the way the tibble would print it: pillar keeps the
  # significant digits set above, avoids scientific notation for large counts,
  # and summarizes list columns as, e.g., "tibble [299 x 3]".
  format_column <- function(column) {
    if (is.list(column) && !is.data.frame(column)) {
      summaries <- vapply(column, function(cell) pillar::obj_sum(cell), character(1))
      return(escape_html(summaries))
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

  kable_preview <- function(x, n = getOption("tibble.print_min", 5)) {
    cells <- lapply(utils::head(x, n), format_column)
    if (nrow(x) > n) {
      # An HTML entity, so the ellipsis does not depend on the render locale.
      cells <- lapply(cells, function(column) c(column, "&hellip;"))
    }
    preview <- as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
    table <- knitr::kable(
      preview,
      row.names = FALSE,
      col.names = names(x),
      align = unname(ifelse(vapply(x, is.numeric, logical(1)), "r", "l"))
    )
    shape <- sprintf("shape: (%s, %s)", format(nrow(x), big.mark = ","), ncol(x))
    # Grouping is part of what print.grouped_df reports, so keep it visible.
    if (inherits(x, "grouped_df") && requireNamespace("dplyr", quietly = TRUE)) {
      shape <- sprintf(
        "%s, groups: %s [%s]", shape,
        paste(dplyr::group_vars(x), collapse = ", "), dplyr::n_groups(x)
      )
    }
    paste0("<small>", shape, "</small>\n\n", paste(table, collapse = "\n"), "\n")
  }

  knit_print_data_frame <- function(x, options = NULL, ...) {
    if (!knitr::is_html_output()) {
      return(knitr::normal_print(x))
    }
    out <- try(kable_preview(x), silent = TRUE)
    if (inherits(out, "try-error")) {
      return(knitr::normal_print(x))
    }
    knitr::asis_output(out)
  }

  register <- function(...) {
    for (class in c("data.frame", "tbl_df", "grouped_df", "rowwise_df")) {
      registerS3method(
        "knit_print", class, knit_print_data_frame,
        envir = asNamespace("knitr")
      )
    }
  }
  register_on_load <- function(package) {
    if (isNamespaceLoaded(package)) {
      register()
    } else {
      setHook(packageEvent(package, "onLoad"), register)
    }
  }
  # rmarkdown registers its own knit_print methods for df-print when it loads,
  # for the same classes (its knit_print.tbl_sql for lazy database tables is
  # left alone). They format with getOption("digits"), which would turn a volume
  # of 535796800 into 5.36e+08, so we register once more after rmarkdown.
  register_on_load("knitr")
  register_on_load("rmarkdown")
})
