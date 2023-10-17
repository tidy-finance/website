if("renv_location.R" %in% list.files("renv")) source("renv/renv_location.R")
source("renv/activate.R")
source("r/pre_render_script.R")

# Set reticulate python path
if (Sys.info()["machine"] != "arm64") {
  Sys.setenv(RETICULATE_PYTHON = paste0(getwd(), "/renv/python/virtualenvs/renv-python-3.10/Scripts/python.exe"))
} else {
  Sys.setenv(RETICULATE_PYTHON = paste0(getwd(), "/renv/python/virtualenvs/renv-python-3.10/bin/python"))
}
