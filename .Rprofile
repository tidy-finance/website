source("assets/render-settings.R")

# >>> uvr >>>
local({
  lib <- Sys.getenv("UVR_LIBRARY")
  if (!nzchar(lib)) lib <- file.path(getwd(), ".uvr", "library")
  lib <- normalizePath(lib, mustWork = FALSE)
  lock <- file.path(getwd(), "uvr.lock")
  rver_file <- file.path(getwd(), ".r-version")
  count_locked <- function(path) {
    if (!file.exists(path)) return(0L)
    length(grep("^\\[\\[package\\]\\]", readLines(path, warn = FALSE)))
  }
  version_ok <- TRUE
  if (file.exists(rver_file)) {
    pinned <- tryCatch(trimws(readLines(rver_file, warn = FALSE)[1]),
                       error = function(e) "")
    if (nzchar(pinned)) {
      active <- as.character(getRversion())
      pinned_minor <- paste(strsplit(pinned, "\\.")[[1]][1:2], collapse = ".")
      active_minor <- paste(strsplit(active, "\\.")[[1]][1:2], collapse = ".")
      if (pinned_minor != active_minor) {
        message("uvr: R ", active, " active but .r-version pins ", pinned,
                ". Restart R against the pinned version, or run uvr::r_pin() to change the pin.")
        version_ok <- FALSE
      }
    }
  }
  if (version_ok) {
    if (dir.exists(lib)) {
      .libPaths(unique(c(lib, .libPaths())))
      n_locked <- count_locked(lock)
      installed <- list.dirs(lib, recursive = FALSE, full.names = FALSE)
      n_installed <- length(setdiff(installed, "uvr"))
      if (n_locked > 0 && n_installed < n_locked) {
        message("uvr: ", n_locked - n_installed, " of ", n_locked,
                " package(s) not installed. Run uvr::sync() to install.")
      } else if (n_locked > 0) {
        message("uvr: library linked (", n_installed, " packages)")
      } else if (file.exists(lock)) {
        message("uvr: library active, but uvr.lock is empty. Run uvr::lock() to populate it.")
      } else {
        message("uvr: library active, but no uvr.lock found. Run uvr::lock() to create one.")
      }
    } else if (file.exists(lock)) {
      n_locked <- count_locked(lock)
      message("uvr: 0 of ", n_locked, " package(s) installed. Run uvr::sync() to install.")
    }
  }
})
# <<< uvr <<<
