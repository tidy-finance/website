# Tidy Finance

This is the repository for the website tidy-finance.org combining
- The book **Tidy Finance with R** by [Christoph Scheuch](https://christophscheuch.github.io?utm_source=tidy-finance-repo), [Stefan Voigt](https://voigtstefan.me?utm_source=tidy-finance-repo), and [Patrick Weiss](https://sites.google.com/view/patrick-weiss?utm_source=tidy-finance-repo),
- The book **Tidy Finance with Python** by [Christoph Frey](https://sites.google.com/site/christophfrey/home?utm_source=tidy-finance-repo), [Christoph Scheuch](https://christophscheuch.github.io?utm_source=tidy-finance-repo), [Stefan Voigt](https://voigtstefan.me?utm_source=tidy-finance-repo), and [Patrick Weiss](https://sites.google.com/view/patrick-weiss?utm_source=tidy-finance-repo), and
- The **Tidy Finance Blog** which is open to contributions.

The code is licensed under [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). The website was rendered using [quarto](https://quarto.org/) and it is hosted via GitHub Pages.

[**Tidy Finance with R**](https://www.jdoqocy.com/click-100765519-14339043?url=https%3A%2F%2Fwww.routledge.com%2FTidy-Finance-with-R%2FVoigt-Weiss-Scheuch%2Fp%2Fbook%2F9781032389349) and [**Tidy Finance with Python**](https://www.kqzyfj.com/click-101217142-14339043?url=https%3A%2F%2Fwww.routledge.com%2FTidy-Finance-with-Python%2FScheuch-Voigt-Weiss-Frey%2Fp%2Fbook%2F9781032676418) were released as print versions via Chapman & Hall / CRC. Consider the [following distribution channels](https://www.tidy-finance.org/support.html#get-your-copy-of-the-book) to support the authors. 

<div class="book-container">
  <a href="https://www.jdoqocy.com/click-100765519-14339043?url=https%3A%2F%2Fwww.routledge.com%2FTidy-Finance-with-R%2FVoigt-Weiss-Scheuch%2Fp%2Fbook%2F9781032389349" target="_top"><img src="assets/img/cover-book-r.jpg" class="book" width="214" height="280"/></a>
  <a href="https://www.kqzyfj.com/click-101217142-14339043?url=https%3A%2F%2Fwww.routledge.com%2FTidy-Finance-with-Python%2FScheuch-Voigt-Weiss-Frey%2Fp%2Fbook%2F9781032676418" target="_top"><img src="assets/img/cover-book-python.jpg" class="book" width="214" height="280"/></a>
</div>

## Rendering locally

R packages come from [uvr](https://github.com/nbafrank/uvr) (`uvr.toml`, `uvr.lock`), Python packages from [uv](https://docs.astral.sh/uv/) (`pyproject.toml`, `uv.lock`). In a terminal, in this folder:

```sh
uvr r install 4.6.1                 # once: installs R (pinned in .r-version)
uvr sync                            # R packages into .uvr/library
uv sync                             # Python and its packages into .venv/
eval "$(uvr activate --emit sh)"    # puts uvr's R on the PATH
unset R_ENVIRON_USER                # activation blanks it, which hides the project .Renviron (credentials)
uv run quarto render                # runs Quarto with the Python in .venv
```

On Windows (PowerShell), activate with `uvr activate --emit powershell | Out-String | Invoke-Expression; Remove-Item Env:R_ENVIRON_USER` instead of the `eval` and `unset` lines. If `uvr r install` fails with "Access is denied", close Positron and R, delete the leftover folders in `$HOME\.uvr\r-versions`, and try again; if endpoint protection keeps blocking it, set the user environment variable `UVR_R_INSTALL_DIR` to a folder outside your user profile.

Add R packages with `uvr add pkg` and commit `uvr.toml` and `uvr.lock`.

The site is rendered with Quarto 1.11.9; `_quarto.yml` requires at least that version (`quarto --version`). Commit `docs/` and `_freeze/` after rendering.

To find the **R package** `tidyfinance`, please consider this [GitHub repository](https://github.com/tidy-finance/r-tidyfinance).