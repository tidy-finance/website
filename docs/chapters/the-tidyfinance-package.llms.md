# The `tidyfinance` package

`tidyfinance` is a package that contains a set of helper functions for empirical research in financial economics, addressing a variety of topics covered in this book. We designed the package to provide easy shortcuts for the applications that we discuss in the book. If you want to inspect the details of the package or propose new features, feel free to visit the package repository on GitHub.

## R

The R package lives in the [r-tidyfinance repository.](https://github.com/tidy-finance/r-tidyfinance)

## Python

The Python package lives in the [py-tidyfinance repository.](https://github.com/tidy-finance/py-tidyfinance)

## Installation

## R

You can install the released version of `tidyfinance` [from CRAN](https://cran.r-project.org/package=tidyfinance) via:

``` r
install.packages("tidyfinance")
```

You can install the development version of `tidyfinance` from [GitHub](https://github.com/tidy-finance/r-tidyfinance) (which might not be fully tested) via:

``` r
# install.packages("pak")
pak::pak("tidy-finance/r-tidyfinance")
```

## Python

You can install the released version of `tidyfinance` [from PyPI](https://pypi.org/project/tidyfinance/) via:

    pip install tidyfinance

You can install the development version of `tidyfinance` from [GitHub](https://github.com/tidy-finance/py-tidyfinance) (which might not be fully tested) via:

    pip install "git+https://github.com/tidy-finance/py-tidyfinance"

## Usage

Throughout the book, we refer to the corresponding features of the `tidyfinance` package. At a high level, the package groups its functionality into a few families:

- **Data access** via a single `download_data()` entry point, which dispatches on a `domain` (e.g., `"WRDS"`, `"Fama-French"`, `"Goyal-Welch"`, `"FRED"`, `"Stock Prices"`, `"Constituents"`, `"Tidy Finance"`) and a `dataset` argument. The package also ships a `domain = "Pseudo Data"` for generating pseudo data with the same schema as the WRDS datasets, which is handy for examples and testing without WRDS access. You can list everything that is available via `list_supported_datasets()`.
- **Portfolio sorts** through `assign_portfolio()`, `compute_breakpoints()`, `compute_portfolio_returns()`, `compute_long_short_returns()`, and the convenience wrapper `implement_portfolio_sort()`, configured via helpers such as `breakpoint_options()`, `data_options()`, and `filter_options()`.
- **Estimation** with `estimate_model()`, `estimate_betas()`, and `estimate_fama_macbeth()`, plus utilities like `create_summary_statistics()`, `winsorize()`, `trim()`, and `add_lagged_columns()`.

## R

For the precise signatures and the latest changes, consult the [package website](https://r.tidy-finance.org/).

## Python

For the precise signatures and the latest changes, consult the [library website](https://python.tidy-finance.org/).

## Feature requests

We are curious to learn in which direction we should extend the package, so please consider opening an issue in the package repository. For instance, we could support more data sources, add more parameters to the family of functions for data downloads, or we could put more emphasis on the generality of portfolio assignment or other modeling functions. Moreover, if you discover a bug, we are very grateful if you report the issue in our repository.
