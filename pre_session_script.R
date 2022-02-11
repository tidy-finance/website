Sys.setlocale("LC_TIME", "English")

# In final book, width can go up to 81
# http://oreillymedia.github.io/production-resources/styleguide/#code

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = TRUE,
  cache = FALSE,
  fig.align = "center",
  width = 81
)

options(tibble.print_max = 4,
        tibble.print_min = 4,
        digits = 3,
        # Suppress crayon since it's falsely on in GHA
        crayon.enabled = FALSE,
        # Better rlang tracebacks
        rlang_trace_top_env = rlang::current_env(),
        width = 81
)



