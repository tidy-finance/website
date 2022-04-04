# Tidy Finance in R

-   Christoph Scheuch, wikifolio Financial Technologies AG, Department of Data Science & Business Intelligence
-   Stefan Voigt, University of Copenhagen, Department of Economics and Danish Finance Institute
-   Patrick Weiss, Vienna University of Economics & Business, Department of Finance, Accounting & Statistics

## Introduction to Tidy Finance

| Section                | Data      | Finance                | Data Science                                         | Main Dev  |
|------------------------|-----------|------------------------|------------------------------------------------------|-----------|
| Visualizing Stock Data | tidyquant | Prices + returns       | RStudio + tidyverse                                  | Stefan    |
| Efficient Frontier     | tidyquant | Vola + diversification | group_by + summarise                                 | Stefan    |
| Outlook & Conventions  | \-        |                        | Project management, importance of coding conventions | Christoph |

## Accessing & Managing Financial Data

| Section                      | Data / Access | Finance                   | Data Science     | Main Dev              |
|------------------------------|---------------|---------------------------|------------------|-----------------------|
| Downloading Fama-French Data | French        |                           | frenchdata       | Christoph             |
| Setting-Up a Database        | \-            | \-                        | RSQlite, dbplyr  | Christoph             |
| Accessing WRDS               | WRDS          |                           | odbc, PostgreSQL | Christoph             |
| Preparing CRSP               | WRDS          | CRSP summary              |                  | Christoph             |
| Merging CRSP & Compustat     | WRDS          | Fama-French matching      |                  | Christoph             |
| Preparing Compustat          | WRDS          | Main compustat variables  |                  | Christoph             |
| Preparing TRACE              | WRDS          | Cleaning bond data        | tbd              | Not for first version |
| Preparing ESG data           | ?             | ESG efficient portfolios? | tbd              | Not for first version |

## Tidy Asset Pricing

| Section                        | Data                      | Finance                       | Data Science                                              | Main Dev              |
|--------------------------------|---------------------------|-------------------------------|-----------------------------------------------------------|-----------------------|
| Beta                           | CRSP + French             | Stock beta                    | Rolling window estimation; many models (nest); multidplyr | Christoph             |
| Univariate Portfolio Sorts     | CRSP + French             | Functions + curly curly + map | Patrick                                                   |                       |
| Size                           | CRSP + French             |                               |                                                           | Patrick               |
| Bivariate Portfolio Sorts      | CRSP + French + Compustat |                               |                                                           | Patrick               |
| Value                          | CRSP + French + Compustat |                               |                                                           | Patrick               |
| Fama-MacBeth Regressions       | CRSP + French             |                               | Functions + many models                                   | Patrick               |
| The Fama-French 3-Factor Model | CRSP + French + Compustat |                               |                                                           | Patrick               |
| Conditional Betas              |                           |                               |                                                           | Not for first version |
| Profitability                  | CRSP + French + Compustat |                               |                                                           | Not for first version |
| Investment                     | CRSP + French + Compustat |                               |                                                           | Not for first version |
| The Fama-French 5-Factor Model | CRSP + French + Compustat |                               |                                                           | Not for first version |
| Q-Factors                      | Q-Factos + French         |                               |                                                           | Not for first version |

## Tidy Modeling & Machine Learning

| Section                          | Data                                | Finance                            | Data Science | Remarks / Main Dev    |
|----------------------------------|-------------------------------------|------------------------------------|--------------|-----------------------|
| Firm Characteristics vs. Factors | <https://dachxiu.chicagobooth.edu/> | Difference to factor models        | tidymodels   | Not for first version |
| Shrinkage Estimation             | <http://www.hec.unil.ch/agoyal/>    | Factor selection                   | Stefan       |                       |
|                                  | Regression Trees and Random Forests | Factor selection                   | tidymodels   | Stefan                |
| Neural Networks                  | Simulations                         | Option Pricing                     | keras        | Stefan                |
| Alpha Estimation                 | Simulations                         | Market timing vs stock selectivity | Manual GMM   | Christoph             |
| Text Sentiment                   |                                     |                                    |              | Christoph             |

## Tidy Portfolio Optimization

| Section                   | Data | Finance                                         | Data Science                        | Remarks / Main Dev   |
|---------------------------|------|-------------------------------------------------|-------------------------------------|----------------------|
| Modern Portfolio Theory   | CRSP | Intro to Markowitz                              | Numerical optimization              | Stefan               |
|                           |      | Parametric Portfolio Choice: Brandt-Santa Clara | More optimization?                  | Stefan               |
| Covariance Estimation     | CRSP | Not in first edition                            |                                     |                      |
| Transaction Costs         | CRSP |                                                 |                                     | Stefan               |
| Merton's Porfolio Problem | \-   | Intro to continuous time finance                | Intro to reinforcement / Q-learning | Too hard with only R |
