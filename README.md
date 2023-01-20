# Tidy Finance with R [<img class="logo" src="https://www.tidy-finance.org/cover.jpg" align="right" style="width:250px;" />](https://www.tidy-finance.org)

This is the repository for the website tidy-finance.org based on the
 book **Tidy Finance with R** by [Christoph Scheuch](https://christophscheuch.github.io?utm_source=tidy-finance-repo), [Stefan Voigt](https://voigtstefan.me?utm_source=tidy-finance-repo), and [Patrick Weiss](https://sites.google.com/view/patrick-weiss?utm_source=tidy-finance-repo). 

The code is licensed under [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/). The book is built with [bookdown](https://bookdown.org/).

Print release via [Chapman & Hall / CRC](https://www.routledge.com/go/chapman-hall?utm_source=tidy-finance.org).

## Instructions 

First, you need to load all required packages: 

```
renv::restore()
renv::use_python()
```
It is recommended to use Python 3.10.6.

Then, open index.qmd -> hit Render button to render all pages (note: this also evaluates all code chunks!).

Or enter in terminal 

```
quarto render
```

If you want to render & publish on quarto-pub, then run

```
quarto publish quarto-pub
```
