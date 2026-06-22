# Setting Up Tidy Finance

Tidy Finance supports both R and Python, and both languages follow the same project-based workflow: you open a project folder, restore its environment, and run the code. This chapter takes you from a fresh machine to a working setup as quickly as possible. The two languages share one workflow and differ only in a handful of commands, which we highlight where they occur.

## Install Positron

We recommend [Positron](https://positron.posit.co/) for all Tidy Finance work. Positron is a data-science environment from Posit that supports R and Python in a single application, with an integrated editor, console, terminal, notebooks, and data viewer. Download and install it from the [official website](https://positron.posit.co/).

Positron runs your code but does not bundle the languages themselves, so install the one you plan to use:

- R users need an R installation from [CRAN](https://cran.r-project.org/).
- Python users need [uv](https://docs.astral.sh/uv/), a fast Python package and project manager that installs Python for you.

Follow the official installation instructions linked above for your operating system. There is nothing Tidy Finance-specific about this step.

## Open a Tidy Finance Project

All Tidy Finance work happens inside a *project*: a single folder that holds your code together with the files that describe its dependencies. Working in a project keeps file paths relative, isolates one piece of work from another, and makes your results reproducible on another machine.

A typical workflow looks as follows:

1.  Create a new folder for your project, or clone an existing one (for example, the [Tidy Finance repository](https://github.com/tidy-finance/website)).
2.  Open the folder in Positron via `File > Open Folder`.
3.  Use Positron’s integrated editor to write code and its integrated terminal to run commands.

Everything in the remaining chapters assumes you are working inside such a project folder.

## Restore the Project Environment

Reproducibility means that you and your collaborators can recreate the exact set of packages a project was built with. Tidy Finance records its dependencies in version-controlled lock files, so restoring an environment is a single command. Run it once after opening the project, and again whenever the lock file changes.

## R

R dependencies are declared in `renv.lock`. Restore them with [`renv`](https://rstudio.github.io/renv/):

``` r
renv::restore()
```

## Python

Python dependencies are declared in `pyproject.toml` and pinned in `uv.lock`. Restore them with [`uv`](https://docs.astral.sh/uv/):

``` bash
uv sync
```

## Working in Positron

Positron is the primary development environment we use throughout the book. A few features cover everything you need:

- **Scripts.** Write and run `.R` or `.py` files line by line, or run them as a whole.
- **Notebooks.** Combine code, output, and prose in a single document for exploratory work.
- **Terminal.** Run shell commands, such as the environment commands above, in the integrated terminal.
- **Interpreters.** Select the project’s R or Python interpreter from Positron’s interpreter picker. Choose the interpreter that belongs to the restored project environment (the `renv` library for R, the project `.venv` for Python).

You do not need anything beyond Positron to follow along with the rest of the book.

## Environment Variables

Some chapters require credentials, for example, a WRDS username and password to download data. Never hard-code secrets into your scripts, where they can be read by anyone and accidentally committed to version control. Instead, store them in a project-level file, read them at runtime, and exclude that file from version control by adding it to `.gitignore`.

## R

R reads environment variables from a `.Renviron` file in your project directory. Create one with:

``` r
usethis::edit_r_environ(scope = "project")
```

Add your credentials as `KEY=value` pairs:

    WRDS_USER=your_username
    WRDS_PASSWORD=your_password

After restarting the R session, access them with `Sys.getenv()`:

``` r
Sys.getenv("WRDS_USER")
```

Make sure `.Renviron` is listed in your `.gitignore` so it is never committed.

## Python

Python projects read environment variables from a `.env` file in your project directory. Create the file and add your credentials as `KEY=value` pairs:

    WRDS_USER=your_username
    WRDS_PASSWORD=your_password

Load them at the start of your script with [`python-dotenv`](https://pypi.org/project/python-dotenv/):

``` python
import os
from dotenv import load_dotenv

load_dotenv()
wrds_user = os.getenv("WRDS_USER")
```

Make sure `.env` is listed in your `.gitignore` so it is never committed.

## Further Resources

- [Positron documentation](https://positron.posit.co/) for everything about the development environment.
- [R for Data Science](https://r4ds.hadley.nz/) for a free, thorough introduction to data analysis with R and the tidyverse.
- [uv documentation](https://docs.astral.sh/uv/) for managing Python versions, environments, and dependencies.
