# Tidy Finance in R

-   Christoph Scheuch, wikifolio Financial Technologies AG, Department of Data Science & Business Intelligence
-   Stefan Voigt, University of Copenhagen, Department of Economics and Danish Finance Institute
-   Patrick Weiss, Vienna University of Economics & Business, Department of Finance, Accounting & Statistics

## Introduction to Tidy Finance

| Topic                  | Section   | Data      | Finance               | Data Science                           | Main Dev |
|------------------------|-----------|-----------|-----------------------|----------------------------------------|----------|
| Visualizing stock data | 1.1 - 1.3 | tidyquant | Prices, returns       | RStudio, tidyverse                     | Stefan   |
| Efficient frontier     | 1.4 & 1.5 | tidyquant | vola, diversification | group_by, summarize                    | Stefan   |
| Outlook & conventions  | (tbd)     |           |                       | project management, coding conventions | (tbd)    |

## Accessing & Managing Financial Data

| Topic                    | Section     | Data             | Finance            | Data Science                       | Main Dev  |
|--------------------------|-------------|------------------|--------------------|------------------------------------|-----------|
| Factors & predictors     | 2.1 - 2.3   | French, Q, Goyal |                    | frenchdata, csv, googledrive       | Christoph |
| Setting up a database    | 2.4         |                  |                    | RSQlite, dbplyr                    | Christoph |
| Accessing WRDS           | 2.5         |                  |                    | odbc, PostgreSQL                   | Christoph |
| CRSP                     | 2.6 - 2.8   | CRSP, WRDS       | CRSP insights      | download large tables (daily data) | Christoph |
| Compustat                | 2.9         | Compustat, WRDS  | Accounting data    |                                    | Christoph |
| Merging CRSP & Compustat | 2.10        | CCM links        |                    |                                    | Christoph |
| Database management      | 2.11 & 2.12 |                  |                    | VACUUM, dbListObjects              | Christoph |
| TRACE transaction data   | (eta: soon) | TRACE, WRDS      | Cleaning bond data |                                    | Patrick   |
| Preparing ESG data       | (tbd)       | Sustainalytics?  |                    |                                    | (tbd)     |

## Tidy Asset Pricing

| Topic                      | Section     | Data            | Finance                             | Data Science                      | Main Dev  |
|----------------------------|-------------|-----------------|-------------------------------------|-----------------------------------|-----------|
| Beta estimation            | 3.1 - 3.5   | CRSP, French    | Stock beta                          | lm, rolling-window estimation,    | Christoph |
|                            |             |                 |                                     | parallelization, nest, multidplyr | Christoph |
| Univariate portfolio sorts | 4.2 & 4.4   | CRSP            | Portfolio sorts, information timing | curly-curly and functions         | Patrick   |
| Performance evaluation     | 4.3 & 4.5   | CRSP, French    | Alphas, Newey West                  | sandwich                          | Patrick   |
| Size & p-hacking           | 5.1 - 5.6   | CRSP, French    | Weighting, p-hacking                | regular expressions               | Patrick   |
| Bivariate portfolio sorts  | 6.1 & 6.2   | CRSP, Compustat | Using accounting data along stocks  |                                   | Patrick   |
| In-/dependent sorts        | 6.3 & 6.4   | CRSP, Compustat | Independent and dependent sorts     |                                   | Patrick   |
| Fama-French 3-factor model | 7.1 - 7.5   | CRSP, Compustat | Size and value factors              |                                   | Patrick   |
| Fama-MacBeth regressions   | (eta: soon) | CRSP, French    | Fama-MacBeth regressions            | GMM                               | Patrick   |
| Conditional betas          | (tbd)       |                 |                                     |                                   | (tbd)     |
| Profitability              | (tbd)       |                 |                                     |                                   | (tbd)     |
| Investment                 | (tbd)       |                 |                                     |                                   | (tbd)     |
| Fama-French 5-factor model | (tbd)       |                 |                                     |                                   | (tbd)     |
| Q-Factors                  | (tbd)       |                 |                                     |                                   | (tbd)     |

## Tidy Modeling & Machine Learning

| Section                          | Data                                | Finance                            | Data Science | Remarks / Main Dev    |
|----------------------------------|-------------------------------------|------------------------------------|--------------|-----------------------|
| Firm Characteristics vs. Factors | <https://dachxiu.chicagobooth.edu/> | Difference to factor models        | tidymodels   | Not for first version |
| Shrinkage Estimation             | <http://www.hec.unil.ch/agoyal/>    | Factor selection                   |              | Stefan                |
|                                  | Regression Trees and Random Forests | Factor selection                   | tidymodels   | Stefan                |
| Neural Networks                  | Simulations                         | Option Pricing                     | keras        | Stefan                |
| Alpha Estimation                 | Simulations                         | Market timing vs stock selectivity | Manual GMM   | Christoph             |
| Text Sentiment                   |                                     |                                    |              | Christoph             |

## Tidy Portfolio Optimization

| Section                   | Data | Finance                                         | Data Science                        | Remarks / Main Dev   |
|---------------------------|------|-------------------------------------------------|-------------------------------------|----------------------|
| Modern Portfolio Theory   | CRSP | Intro to Markowitz                              | Numerical optimization              | Stefan               |
|                           |      | Parametric Portfolio Choice: Brandt-Santa Clara | More optimization?                  | Stefan               |
| Covariance Estimation     | CRSP |                                                 |                                     | Not in first edition |
| Transaction Costs         | CRSP |                                                 |                                     | Stefan               |
| Merton's Porfolio Problem | \-   | Intro to continuous time finance                | Intro to reinforcement / Q-learning | Too hard with only R |
