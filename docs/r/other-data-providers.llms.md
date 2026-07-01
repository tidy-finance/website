# Other Data Providers

> **NOTE:**
>
> You are reading **Tidy Finance with R**. You can find the equivalent chapter for the sibling **Tidy Finance with Python** [here](../python/other-data-providers.llms.md).

In the previous chapters, we introduced many ways to get financial data that researchers regularly use. We showed how to load data into R from Yahoo Finance and commonly used file types, such as comma-separated or Excel files. Then, we introduced remotely connecting to WRDS and downloading data from there. However, this is only a subset of the vast amounts of data available these days.

In this short chapter, we aim to provide an overview of common alternative data providers for which direct access via R packages exists. Such a list requires constant adjustments because both data providers and access methods change. However, we want to emphasize two main insights: First, the number of R packages that provide access to (financial) data is large. Too large actually to survey here exhaustively. Instead, we can only cover the tip of the iceberg. Second, R provides the functionalities to access basically any form of files or data available online. Thus, even if a desired data source does not come with a well-established R package, chances are high that data can be retrieved by establishing your own API connection or by scraping the content.

In our non-exhaustive list below, we restrict ourselves to listing data sources accessed through easy-to-use R packages. For further inspiration on potential data sources, we recommend reading the [R task view empirical finance.](https://cran.r-project.org/web/views/Finance.html) Further inspiration (on more general social sciences) can be found [here.](https://cengel.github.io/gearup2016/SULdataAccess.html)

Apart from the list below, we want to advertise some amazing data compiled by others. First, there is [Open Source Asset Pricing](https://www.openassetpricing.com/data) related to Chen and Zimmermann ([2022](#ref-Chen2022)). They provide return data for over 200 trading strategies with different time periods and specifications. The authors also provide signals and explanations of the factor construction. Moreover, in the same spirit, [Global factor data](https://www.jkpfactors.com) provides the data related to Jensen2022b. They provide return data for characteristic-managed portfolios from around the world. The database includes factors for 153 characteristics in 13 themes, using data from 93 countries.

> **NOTE:**
>
> If you feel that we miss a fantastic financial data source, please get in touch via <contact@tidy-finance.org> - thank you very much for your support!

| Source | Description | R packages |
|----|----|----|
|  | **Macroeconomic Variables** |  |
| FED | The Federal Reserve Bank of St. Louis provides more than 818,000 US and international time series from 109 sources via the API FRED. The data is freely available and can be browsed online on the [FRED homepage.](https://fred.stlouisfed.org/) | `fredr` ([Boysel and Vaughan 2021](#ref-fredr)) and `alfred` ([Kleen 2021](#ref-alfred)) |
| ECB | The European Central Bank’s [Statistical Data Warehouse](https://sdw.ecb.europa.eu/) provides data on Euro area monetary policy, financial stability, and other topics relevant to the activities of the ECB and the European System of Central Banks (ESCB). | `ecb` ([Persson 2021](#ref-ecb)) |
|  | **Financial data** |  |
| [Bloomberg](https://www.bloomberg.com/) | Bloomberg’s Fundamental coverage includes current and normalized historical data for the balance sheet, income statement, cash flows statement, and financial ratios. Additionally, it provides industry-specific data for communications, consumer, energy, health care, and many more. In order to retrieve Bloomberg data, a paid subscription is needed. | `Rblpapi` ([Armstrong et al. 2022](#ref-Rblpapi)) |
| [Refinitiv Eikon](https://www.refinitiv.com/en/financial-data) | Eikon provides access to real-time market data, news, fundamental data, analytics, trading, and messaging tools. Refinitiv’s Eikon is a paid service. Apart from the CRAN version, there is also `https://github.com/philaris/eikonapir`. | `DatastreamDSWS2R` ([Cara 2021](#ref-DatastreamDSWS2R)) and `eikonapir` |
| [Nasdaq Data Link (Quandl)](data.nasdaq.com/publishers/qdl) | Quandl is a publisher of alternative data. Quandl publishes free data, scraped from many different sources from the web. However, some of the data requires specific subscriptions on the Quandl platform. | `Quandl` ([McTaggart et al. 2021](#ref-Quandl)) |
| [Simfin](https://simfin.com/) | Simfin makes fundamental financial data freely available to private investors, researchers, and students. The data provider applies automating data collection processes to collect a large set of publicly available information from firms’ financial statements. | `simfinapi` ([Gomolka 2021](#ref-simfinapi)) |
|  | **High-frequency data** |  |
| IEX | The IEX Group operates the Investors Exchange (IEX), a stock exchange for US equities. IEX offers US reference and market data including end-of-day and *intraday pricing data*. IEX offers an API which is freely available. | `Riex` ([Ibrahim 2021](#ref-Riex)) |
| TAQ | TAQ data provides subscribed users access to all trades and quotes for all issues traded on NYSE, Nasdaq, and the regional exchanges. [TAQ data](https://www.nyse.com/market-data/historical) can be accessed from WRDS via Postgres. The `highfrequency` package delivers useful workflows to clean TAQ data. | `highfrequency` ([Boudt et al. 2022](#ref-highfrequency)) |
|  | **Other (free) data** |  |
| [CoinMarketCap](https://coinmarketcap.com/) | The data provider CoinMarketCap provides cryptocurrency information and historical prices, as well as information on the exchanges they are listed on. | `crypto2` ([Stoeckl 2022](#ref-crypto2)) |
| [CoinGecko](https://www.coingecko.com/) | CoinGecko is an alternative crypto data provider of current and historical data on a myriad of coins and exchanges. | `geckor` ([Mastitsky 2021](#ref-geckor)) |
| Twitter | Twitter provides (limited) access for academic research to extract and analyze Tweets. | `rtweet` ([Kearney 2019](#ref-rtweet)) |
| SEC company fillings | The [EDGAR](https://www.sec.gov/edgar/about) database provides free public access to corporate information, allowing you to research a public company’s financial information and operations by reviewing the filings the company makes with the SEC. You can also research information provided by mutual funds (including money market funds), exchange-traded funds (ETFs), and variable annuities. | `edgarWebR` ([Waldstein 2021](#ref-edgarWebR)) |
| Google trends | Google offers public access to global search volumes through its search engine through the [Google Trends portal.](https://trends.google.com/trends/?geo=DK) | `globaltrends` ([Puhr and Müllner 2021](#ref-globaltrends)) and `gtrends` ([Massicotte and Eddelbuettel 2022](#ref-gtrendsR)) |

## Key Takeaways

- Access a wide range of financial and macroeconomic data through R packages tailored to specific providers.
- Even if a specific R package doesn’t exist, R can often retrieve data through APIs or web scraping methods.

## Exercises

1.  Select one of the data sources in the table above and retrieve some data. Browse the homepage of the data provider or the package documentation to find inspiration on which type of data is available to you and how to download the data into your R session.
2.  Generate summary statistics of the data you retrieved and provide some useful visualization. The possibilities are endless: Maybe there is some interesting economic event you want to analyze, such as stock market responses to Twitter activity.

## References

Armstrong, Whit, Dirk Eddelbuettel, and John Laing. 2022. *Rblpapi: R Interface to ’Bloomberg’*. <https://CRAN.R-project.org/package=Rblpapi>.

Boudt, Kris, Jonathan Cornelissen, Scott Payseur, Onno Kleen, and Emil Sjoerup. 2022. *Highfrequency: Tools for Highfrequency Data Analysis*. <https://CRAN.R-project.org/package=highfrequency>.

Boysel, Sam, and Davis Vaughan. 2021. *fredr: An R client for the ’FRED’ API*. <https://CRAN.R-project.org/package=fredr>.

Cara, Charles. 2021. *DatastreamDSWS2R: Provides a Link Between the ’Refinitiv Datastream’ System and r*. <https://CRAN.R-project.org/package=DatastreamDSWS2R>.

Chen, Andrew Y., and Tom Zimmermann. 2022. “Open Source Cross-Sectional Asset Pricing.” *Critical Finance Review* 11 (2): 207–64. <http://dx.doi.org/10.1561/104.00000112>.

Gomolka, Matthias. 2021. *Simfinapi: Accessing ’SimFin’ Data*. <https://CRAN.R-project.org/package=simfinapi>.

Ibrahim, Myriam. 2021. *Riex: IEX Stocks and Market Data*. <https://CRAN.R-project.org/package=Riex>.

Kearney, Michael W. 2019. “Rtweet: Collecting and Analyzing Twitter Data.” *Journal of Open Source Software* 4 (42): 1829. <https://joss.theoj.org/papers/10.21105/joss.01829>.

Kleen, Onno. 2021. *Alfred: Downloading Time Series from ALFRED Database for Various Vintages*. <https://CRAN.R-project.org/package=alfred>.

Massicotte, Philippe, and Dirk Eddelbuettel. 2022. *gtrendsR: Perform and Isplay Google Trends Queries*. <https://CRAN.R-project.org/package=gtrendsR>.

Mastitsky, Sergey. 2021. *Geckor: R Client for the ’CoinGecko’ API*. <https://CRAN.R-project.org/package=geckor>.

McTaggart, Raymond, Gergely Daroczi, and Clement Leung. 2021. *Quandl: API Wrapper for Quandl.com*. <https://CRAN.R-project.org/package=Quandl>.

Persson, Eric. 2021. *Ecb: Programmatic Access to the European Central Bank’s Statistical Data Warehouse*. <https://CRAN.R-project.org/package=ecb>.

Puhr, Harald, and Jakob Müllner. 2021. “Let me Google that for you: Capturing globalization using Google Trends.” *Working Paper*. <https://www.ssrn.com/abstract=3969013>.

Stoeckl, Sebastian. 2022. *Crypto2: Download Crypto Currency Data from ’CoinMarketCap’ Without ’API’*. <https://CRAN.R-project.org/package=crypto2>.

Waldstein, Micah J. 2021. *edgarWebR: SEC Filings Access*. <https://CRAN.R-project.org/package=edgarWebR>.
