★ Open-source · R & Python · Reproducible

# Empirical finance, reproducible in R and Python

Tidy Finance takes an opinionated, fully transparent approach to empirical research in financial economics. One open-source code base now covers R and Python, side by side in every chapter.

[Start here](chapters/working-with-stock-returns.llms.md) [R & Python docs](https://package.tidy-finance.org) [Data platform](https://factors.tidy-finance.org)

download & plot prices

## R

``` r
library(tidyfinance)
library(ggplot2)

prices <- download_data(
  domain = "stock_prices",
  symbols = "AAPL",
  start_date = "2000-01-01",
  end_date = "2024-12-31"
)

prices |>
  ggplot(aes(date, adjusted_close)) +
  geom_line()
```

## Python

``` python
import tidyfinance as tf
from plotnine import ggplot, aes, geom_line

prices = tf.download_data(
  domain="stock_prices",
  symbols="AAPL",
  start_date="2000-01-01",
  end_date="2024-12-31"
)

(prices
  .pipe(ggplot, aes("date", "adjusted_close"))
  + geom_line())
```

20+ Hands-on chapters

2 Languages, one code base

100% Open source & reproducible

## Everything you need for reproducible finance

A single, transparent code base takes you from raw data to publication-quality results, in whichever language you prefer.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjQgMjQiIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PHBvbHlsaW5lIHBvaW50cz0iMTYgMTggMjIgMTIgMTYgNiI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSI4IDYgMiAxMiA4IDE4Ij48L3BvbHlsaW5lPjwvc3ZnPg==)

### One code base, two languages

Every chapter ships R and Python side by side from a single source. Switch with a tab. Your choice follows you through the book.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjQgMjQiIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PHBvbHlsaW5lIHBvaW50cz0iMjMgNCAyMyAxMCAxNyAxMCI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxIDIwIDEgMTQgNyAxNCI+PC9wb2x5bGluZT48cGF0aCBkPSJNMy41MSA5YTkgOSAwIDAgMSAxNC44NS0zLjM2TDIzIDEwTTEgMTRsNC42NCA0LjM2QTkgOSAwIDAgMCAyMC40OSAxNSIgLz48L3N2Zz4=)

### Fully transparent & reproducible

Open code you can run end to end, from raw data to every figure and table, with no black boxes.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjQgMjQiIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PGxpbmUgeDE9IjE2LjUiIHkxPSI5LjQiIHgyPSI3LjUiIHkyPSI0LjIxIj48L2xpbmU+PHBhdGggZD0iTTIxIDE2VjhhMiAyIDAgMCAwLTEtMS43M2wtNy00YTIgMiAwIDAgMC0yIDBsLTcgNEEyIDIgMCAwIDAgMyA4djhhMiAyIDAgMCAwIDEgMS43M2w3IDRhMiAyIDAgMCAwIDIgMGw3LTRBMiAyIDAgMCAwIDIxIDE2eiIgLz48cG9seWxpbmUgcG9pbnRzPSIzLjI3IDYuOTYgMTIgMTIuMDEgMjAuNzMgNi45NiI+PC9wb2x5bGluZT48bGluZSB4MT0iMTIiIHkxPSIyMi4wOCIgeDI9IjEyIiB5Mj0iMTIiPjwvbGluZT48L3N2Zz4=)

### Powered by `tidyfinance`

A companion package for R and Python that wraps data downloads and the routines we use throughout the book.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjQgMjQiIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PGVsbGlwc2UgY3g9IjEyIiBjeT0iNSIgcng9IjkiIHJ5PSIzIj48L2VsbGlwc2U+PHBhdGggZD0iTTIxIDEyYzAgMS42Ni00IDMtOSAzcy05LTEuMzQtOS0zIiAvPjxwYXRoIGQ9Ik0zIDV2MTRjMCAxLjY2IDQgMyA5IDNzOS0xLjM0IDktM1Y1IiAvPjwvc3ZnPg==)

### Real financial data

Work with the data professionals use, like CRSP, Compustat, and TRACE, through clean, documented access patterns.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjQgMjQiIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PHBvbHlsaW5lIHBvaW50cz0iMjMgNiAxMy41IDE1LjUgOC41IDEwLjUgMSAxOCI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxNyA2IDIzIDYgMjMgMTIiPjwvcG9seWxpbmU+PC9zdmc+)

### From sorts to machine learning

Portfolio sorts, factor models, fixed effects, causal inference, and modern ML give you the full empirical toolkit.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjQgMjQiIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCI+PHBhdGggZD0iTTQgMTkuNUEyLjUgMi41IDAgMCAxIDYuNSAxN0gyMCIgLz48cGF0aCBkPSJNNi41IDJIMjB2MjBINi41QTIuNSAyLjUgMCAwIDEgNCAxOS41di0xNUEyLjUgMi41IDAgMCAxIDYuNSAyeiIgLz48L3N2Zz4=)

### A free, open book

Read the whole book online for free, dive into the blog, or join a workshop, all in the open.

## Browse the book by topic

Five parts take you from your first stock return to constrained portfolio backtests.

PART 01

### Getting Started

Stock returns, modern portfolio theory, the CAPM, and financial statement analysis.

Start here →

PART 02

### Financial Data

Access and manage WRDS, CRSP, Compustat, TRACE, and FISD with documented workflows.

Explore data →

PART 03

### Asset Pricing

Beta estimation, univariate and bivariate sorts, Fama-French factors, and Fama-MacBeth.

Price assets →

PART 04

### Modeling & Machine Learning

Fixed effects, difference-in-differences, factor selection, and option pricing via ML.

Model it →

PART 05

### Portfolio Optimization

Parametric portfolio policies and constrained optimization with realistic backtesting.

Optimize →

Praise

## What experts say about Tidy Finance

![A portrait of Harald Lohre](assets/img/quotes/harald_lohre.jpg)

*A clean coding environment is a prerequisite for building a relevant investment platform and conducting meaningful factor research. Tidy Finance is the name of the game, giving aspiring academics and finance practitioners just what they need to perform clean and reproducible research. Highly recommended.*

**Harald Lohre**

Executive Director at Robeco

Honorary Researcher at Lancaster University Management School

![A portrait of Albert J. Menkveld](assets/img/quotes/albert_j_menkveld.jpeg)

*From our crowd-sourced paper on non-standard errors, I learned how important clean coding is. Tidy Finance is a rich resource for empirical finance researchers, offering clean coding techniques that benefit both beginners and experts.*

**Albert J. Menkveld**

Professor of Finance at Vrije Universiteit Amsterdam

Fellow at Tinbergen Institute

![A portrait of Nikolaus Hautsch](assets/img/quotes/nikolaus_hautsch.jpeg)

*A fantastic book bringing together financial theory, sound econometrics, thorough data processing and powerful programming techniques. An absolute must for every student and scholar in empirical finance.*

**Nikolaus Hautsch**

Professor of Finance & Statistics at University of Vienna

![A portrait of Björn Hagströmer](assets/img/quotes/bjoern_hagstroemer.jpeg)

*Tidy Finance is a fantastic resource that lowers the threshold for entry into empirical finance, all in the spirit of open and reproducible science.*

**Björn Hagströmer**

Professor of Finance at Stockholm Business School

![A portrait of Raman Uppal](assets/img/quotes/raman_uppal.jpeg)

*To have a deep understanding of empirical asset pricing, one needs to write code using actual data. To learn how to do this, there is no better starting point than Tidy Finance. \[...\] I strongly recommend Tidy Finance to both beginners and experts.*

**Raman Uppal**

Professor of Finance at EDHEC Business School

![A portrait of Mark Salmon](assets/img/quotes/mark_salmon.jpeg)

*Students and professionals alike are led step by step until they suddenly find themselves coding on their own. A brilliant and required resource!*

**Mark Salmon**

Professor of Economics at University of Cambridge

The team

## Who maintains Tidy Finance

![A portrait of Christoph Scheuch](assets/img/christoph_scheuch.jpeg)

### Christoph Scheuch

Independent Expert in Finance & Data

LinkedIn →

![A portrait of Stefan Voigt](assets/img/stefan_voigt.jpeg)

### Stefan Voigt

Assistant Professor of Finance at University of Copenhagen

Website →

![A portrait of Patrick Weiss](assets/img/patrick_weiss.jpeg)

### Patrick Weiss

Assistant Professor of Finance at Reykjavik University

Website →

![A portrait of Christoph Frey](assets/img/christoph_frey.jpeg)

### Christoph Frey

Quantitative Researcher

Website →

© Christoph Frey, Christoph Scheuch, Stefan Voigt & Patrick Weiss

[Disclaimer](disclaimer.llms.md) [Cookie Preferences](#)

🎓 **Applications open: Summer School "Foundations for Reproducible Research" — closes June 22.** [Apply now →](https://bse.eu/summer-school/data-science-finance/tidy-finance-reproducible-research-in-data-science)

×
