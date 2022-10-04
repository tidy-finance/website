# (PART\*) Getting started {.unnumbered}

# Introduction to Tidy Finance

The main aim of this chapter is to familiarize yourself with the tidyverse. We start by downloading and visualizing stock data from Yahoo!Finance. Then we move to a simple portfolio choice problem and construct the efficient frontier. These examples introduce you to our approach of *Tidy Finance*.

## Working with stock market data

At the start of each session, we load the required packages. 
Throughout the entire book, we always use the `tidyverse` [@Wickham2019].
In this chapter, we also load the convenient `tidyquant` package  [@tidyquant] to download price data. This package provides a convenient wrapper for various quantitative functions compatible with the `tidyverse`.\index{tidyverse}

You typically have to install a package once before you can load it. 
In case you have not done this yet, call `install.packages("tidyquant")`. \index{tidyquant} 
If you have trouble using `tidyquant`, check out the corresponding [documentation.](https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ00-introduction-to-tidyquant.html)


```r
library(tidyverse)
library(tidyquant)
```

We first download daily prices for one stock market ticker, e.g., the Apple stock, *AAPL*, directly from the data provider Yahoo!Finance.
To download the data, you can use the command `tq_get`. 
If you do not know how to use it, make sure you read the help file by calling `?tq_get`. 
We especially recommend taking a look at the examples section of the documentation. We request daily data for a period of more than 20 years.\index{Stock prices}


```r
prices <- tq_get("AAPL",
  get = "stock.prices",
  from = "2000-01-01",
  to = "2021-12-31"
)
prices
```

```
# A tibble: 5,535 × 8
  symbol date        open  high   low close    volume adjusted
  <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
1 AAPL   2000-01-03 0.936 1.00  0.908 0.999 535796800    0.853
2 AAPL   2000-01-04 0.967 0.988 0.903 0.915 512377600    0.781
3 AAPL   2000-01-05 0.926 0.987 0.920 0.929 778321600    0.793
4 AAPL   2000-01-06 0.948 0.955 0.848 0.848 767972800    0.724
5 AAPL   2000-01-07 0.862 0.902 0.853 0.888 460734400    0.759
# … with 5,530 more rows
```

\index{Data!YahooFinance} `tq_get` downloads stock market data from Yahoo!Finance if you do not specify another data source. 
The function returns a tibble with eight quite self-explanatory columns: `symbol`, `date`, the market prices at the `open`, `high`, `low` and `close`, the daily `volume` (in number of traded shares), and the `adjusted` price in USD. 
The adjusted prices are corrected for anything that might affect the stock price after the market closes, e.g., stock splits and dividends. 
These actions affect the quoted prices, but they have no direct impact on the investors who hold the stock. Therefore, we often rely on adjusted prices when it comes to analyzing the returns an investor would have earned by holding the stock continuously.\index{Stock price adjustments}

Next, we use the `ggplot2` package [@ggplot2] to visualize the time series of adjusted prices. This package takes care of visualization tasks based on the principles of the grammar of graphics [@Wilkinson2012].\index{Graph!Time series}


```r
prices |>
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Apple stock prices between beginning of 2000 and end of 2021"
  )
```

<div class="figure" style="text-align: center">
<img src="10_introduction_files/figure-html/fig100-1.png" alt="Title: Apple stock prices between the beginning of 2000 and the end of 2021. The figure shows that the stock price of Apple increased dramatically from about 1 USD to around 125 USD." width="90%" />
<p class="caption">(\#fig:fig100)Prices are in USD, adjusted for dividend payments and stock splits.</p>
</div>

\index{Returns} Instead of analyzing prices, we compute daily net returns defined as $r_t = p_t / p_{t-1} - 1$ where $p_t$ is the adjusted day $t$ price. 
In that context, the function `lag()` is helpful, which returns the previous value in a vector. 


```r
returns <- prices |>
  arrange(date) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  select(symbol, date, ret)
returns
```

```
# A tibble: 5,535 × 3
  symbol date           ret
  <chr>  <date>       <dbl>
1 AAPL   2000-01-03 NA     
2 AAPL   2000-01-04 -0.0843
3 AAPL   2000-01-05  0.0146
4 AAPL   2000-01-06 -0.0865
5 AAPL   2000-01-07  0.0474
# … with 5,530 more rows
```

The resulting tibble contains three columns, where the last contains the daily returns (`ret`). 
Note that the first entry naturally contains a missing value (`NA`) because there is no previous price.\index{Missing value} 
Obviously, the use of `lag()` would be meaningless if the time series is not ordered by ascending dates.\index{Lag observations}
The command `arrange()` provides a convenient way to order observations in the correct way for our application. In case you want to order observations by descending dates, you can use `arrange(desc(date))`.

For the upcoming examples, we remove missing values as these would require separate treatment when computing, e.g., sample averages. In general, however, make sure you understand why `NA` values occur and carefully examine if you can simply get rid of these observations. 


```r
returns <- returns |>
  drop_na(ret)
```

Next, we visualize the distribution of daily returns in a histogram. For convenience, we multiply the returns by 100 to get returns in percent for the visualizations.\index{Graph!Histogram}
Additionally, we add a dashed red line that indicates the 5 percent quantile of the daily returns to the histogram, which is a (crude) proxy for the worst return of the stock with a probability of at most 5 percent. 
The 5 percent quantile is closely connected to the (historical) Value-at-risk, a risk measure commonly monitored by regulators. \index{Value-at-risk} We refer to @Tsay2010 for a more thorough introduction to stylized facts of returns.\index{Returns}


```r
quantile_05 <- quantile(returns |> pull(ret) * 100, probs = 0.05)

returns |>
  ggplot(aes(x = ret * 100)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = quantile_05),
    linetype = "dashed"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of daily Apple stock returns in percent"
  )
```

<div class="figure" style="text-align: center">
<img src="10_introduction_files/figure-html/fig101-1.png" alt="Title: Distribution of daily Apple stock returns in percent. The figure shows a histogram of daily returns. The range indicates a few large negative values, while the remaining returns are distributed around 0. The vertical line indicates that the historical 5 percent quantile of daily returns was around negative 3 percent" width="90%" />
<p class="caption">(\#fig:fig101)The dotted vertical line indicates the historical 5 percent quantile.</p>
</div>

Here, `bins = 100` determines the number of bins used in the illustration and hence implicitly the width of the bins. 
Before proceeding, make sure you understand how to use the geom `geom_vline()` to add a dashed line that indicates the 5 percent quantile of the daily returns. 
A typical task before proceeding with *any* data is to compute summary statistics for the main variables of interest.


```r
returns |>
  mutate(ret = ret * 100) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    )
  ))
```

```
# A tibble: 1 × 4
  ret_daily_mean ret_daily_sd ret_daily_min ret_daily_max
           <dbl>        <dbl>         <dbl>         <dbl>
1          0.130         2.52         -51.9          13.9
```

We see that the maximum *daily* return was 13.905 percent. Perhaps not surprisingly, the average daily return is close to but slightly above 0. 
In line with the illustration above, the large losses on the day with the minimum returns indicate a strong asymmetry in the distribution of returns.     
You can also compute these summary statistics for each year individually by imposing `group_by(year = year(date))`, where the call `year(date)` returns the year. More specifically, the few lines of code below compute the summary statistics from above for individual groups of data, defined by year. The summary statistics, therefore, allow an eyeball analysis of the time-series dynamics of the return distribution. 


```r
returns |>
  mutate(ret = ret * 100) |>
  group_by(year = year(date)) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    ),
    .names = "{.fn}"
  )) |>
  print(n = Inf)
```

```
# A tibble: 22 × 5
    year daily_mean daily_sd daily_min daily_max
   <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
 1  2000   -0.346       5.49    -51.9      13.7 
 2  2001    0.233       3.93    -17.2      12.9 
 3  2002   -0.121       3.05    -15.0       8.46
 4  2003    0.186       2.34     -8.14     11.3 
 5  2004    0.470       2.55     -5.58     13.2 
 6  2005    0.349       2.45     -9.21      9.12
 7  2006    0.0949      2.43     -6.33     11.8 
 8  2007    0.366       2.38     -7.02     10.5 
 9  2008   -0.265       3.67    -17.9      13.9 
10  2009    0.382       2.14     -5.02      6.76
11  2010    0.183       1.69     -4.96      7.69
12  2011    0.104       1.65     -5.59      5.89
13  2012    0.130       1.86     -6.44      8.87
14  2013    0.0472      1.80    -12.4       5.14
15  2014    0.145       1.36     -7.99      8.20
16  2015    0.00199     1.68     -6.12      5.74
17  2016    0.0575      1.47     -6.57      6.50
18  2017    0.164       1.11     -3.88      6.10
19  2018   -0.00573     1.81     -6.63      7.04
20  2019    0.266       1.65     -9.96      6.83
21  2020    0.281       2.94    -12.9      12.0 
22  2021    0.133       1.58     -4.17      5.39
```
\index{Summary statistics} 

In case you wonder: the additional argument `.names = "{.fn}"` in `across()` determines how to name the output columns. The specification is rather flexible and allows almost arbitrary column names, which can be useful for reporting. The `print()` function simply controls the output options for the R console. 

## Scaling up the analysis

As a next step, we generalize the code from before such that all the computations can handle an arbitrary vector of tickers (e.g., all constituents of an index). Following tidy principles, it is quite easy to download the data, plot the price time series, and tabulate the summary statistics for an arbitrary number of assets.

This is where the `tidyverse` magic starts: tidy data makes it extremely easy to generalize the computations from before to as many assets as you like. The following code takes any vector of tickers, e.g., `ticker <- c("AAPL", "MMM", "BA")`, and automates the download as well as the plot of the price time series. 
In the end, we create the table of summary statistics for an arbitrary number of assets. We perform the analysis with data from all current constituents of the [Dow Jones Industrial Average index.](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average) \index{Data!Dow Jones Index}


```r
ticker <- tq_index("DOW")
ticker
```

```
# A tibble: 30 × 8
  symbol company          ident…¹ sedol weight sector share…² local…³
  <chr>  <chr>            <chr>   <chr>  <dbl> <chr>    <dbl> <chr>  
1 UNH    UnitedHealth Gr… 91324P… 2917… 0.116  Healt… 5767893 USD    
2 GS     Goldman Sachs G… 38141G… 2407… 0.0672 Finan… 5767893 USD    
3 HD     Home Depot Inc.  437076… 2434… 0.0633 Consu… 5767893 USD    
4 MSFT   Microsoft Corpo… 594918… 2588… 0.0534 Infor… 5767893 USD    
5 MCD    McDonald's Corp… 580135… 2550… 0.0529 Consu… 5767893 USD    
# … with 25 more rows, and abbreviated variable names ¹​identifier,
#   ²​shares_held, ³​local_currency
```
Conveniently, `tidyquant` provides a function to get all stocks in a stock index with a single call (similarly, `tq_exchange("NASDAQ")` delivers all stocks currently listed on NASDAQ exchange). \index{Exchange!NASDAQ}


```r
index_prices <- tq_get(ticker,
  get = "stock.prices",
  from = "2000-01-01",
  to = "2022-12-31"
)
```

The resulting tibble contains 163733 daily observations for 30 different corporations. 
The figure below illustrates the time series of downloaded *adjusted* prices for each of the constituents of the Dow Jones index. Make sure you understand every single line of code! (What are the arguments of `aes()`? Which alternative `geoms` could you use to visualize the time series? Hint: if you do not know the answers try to change the code to see what difference your intervention causes). 


```r
index_prices |>
  ggplot(aes(
    x = date,
    y = adjusted,
    color = symbol
  )) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Stock prices of DOW index constituents"
  ) +
  theme(legend.position = "none")
```

<div class="figure" style="text-align: center">
<img src="10_introduction_files/figure-html/fig103-1.png" alt="Title: Stock prices of DOW index constituents. The figure shows many time series with daily prices. The general trend seems positive for most stocks in the DOW index." width="90%" />
<p class="caption">(\#fig:fig103)Prices in USD, adjusted for dividend payments and stock splits.</p>
</div>

Do you notice the small differences relative to the code we used before? `tq_get(ticker)` returns a tibble for several symbols as well. All we need to do to illustrate all tickers simultaneously is to include `color = symbol` in the `ggplot2` aesthetics. In this way, we generate a separate line for each ticker. Of course, there are simply too many lines on this graph to identify the individual stocks properly, but it illustrates the point well.

The same holds for stock returns. Before computing the returns, we use `group_by(symbol)` such that the `mutate()` command is performed for each symbol individually. The same logic also applies to the computation of summary statistics: `group_by(symbol)` is the key to aggregating the time series into ticker-specific variables of interest. 


```r
all_returns <- index_prices |>
  group_by(symbol) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  select(symbol, date, ret) |>
  drop_na(ret)

all_returns |>
  mutate(ret = ret * 100) |>
  group_by(symbol) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    ),
    .names = "{.fn}"
  )) |>
  print(n = Inf)
```

```
# A tibble: 30 × 5
   symbol daily_mean daily_sd daily_min daily_max
   <chr>       <dbl>    <dbl>     <dbl>     <dbl>
 1 AMGN       0.0469     1.97     -13.4      15.1
 2 AXP        0.0511     2.30     -17.6      21.9
 3 BA         0.0526     2.23     -23.8      24.3
 4 CAT        0.0654     2.04     -14.5      14.7
 5 CRM        0.113      2.69     -27.1      26.0
 6 CSCO       0.0293     2.38     -16.2      24.4
 7 CVX        0.0527     1.76     -22.1      22.7
 8 DIS        0.0438     1.94     -18.4      16.0
 9 DOW        0.0451     2.63     -21.7      20.9
10 GS         0.0530     2.32     -19.0      26.5
11 HD         0.0527     1.94     -28.7      14.1
12 HON        0.0481     1.94     -17.4      28.2
13 IBM        0.0247     1.65     -15.5      12.0
14 INTC       0.0286     2.36     -22.0      20.1
15 JNJ        0.0397     1.22     -15.8      12.2
16 JPM        0.0547     2.43     -20.7      25.1
17 KO         0.0319     1.32     -10.1      13.9
18 MCD        0.0518     1.48     -15.9      18.1
19 MMM        0.0369     1.50     -12.9      12.6
20 MRK        0.0343     1.68     -26.8      13.0
21 MSFT       0.0516     1.93     -15.6      19.6
22 NKE        0.0692     1.93     -19.8      15.5
23 PG         0.0350     1.34     -30.2      12.0
24 TRV        0.0542     1.84     -20.8      25.6
25 UNH        0.0988     1.98     -18.6      34.8
26 V          0.0905     1.90     -13.6      15.0
27 VZ         0.0237     1.51     -11.8      14.6
28 WBA        0.0258     1.81     -15.0      16.6
29 WMT        0.0304     1.50     -11.4      11.7
30 AAPL       0.122      2.51     -51.9      13.9
```
\index{Summary statistics} 

Note that you are now also equipped with all tools to download price data for *each* ticker listed in the S&P 500 index with the same number of lines of code. Just use `ticker <- tq_index("SP500")`, which provides you with a tibble that contains each symbol that is (currently) part of the S&P 500.\index{Data!SP 500} However, don't try this if you are not prepared to wait for a couple of minutes because this is quite some data to download!

## Other forms of data aggregation 

Of course, aggregation across variables other than `symbol` can also make sense. For instance, suppose you are interested in answering the question: are days with high aggregate trading volume likely followed by days with high aggregate trading volume? To provide some initial analysis on this question, we take the downloaded data and compute aggregate daily trading volume for all Dow Jones constituents in USD. 
Recall that the column `volume` is denoted in the number of traded shares.\index{Trading volume}
Thus, we multiply the trading volume with the daily closing price to get a proxy for the aggregate trading volume in USD. Scaling by `1e9` (R can handle scientific notation) denotes daily trading volume in billion USD.  


```r
volume <- index_prices |>
  group_by(date) |>
  summarize(volume = sum(volume * close / 1e9))

volume |>
  ggplot(aes(x = date, y = volume)) +
  geom_line() +
  labs(
    x = NULL, y = NULL,
    title = "Aggregate daily trading volume of DOW index constitutens"
  )
```

<div class="figure" style="text-align: center">
<img src="10_introduction_files/figure-html/fig104-1.png" alt="Title: Aggregate daily trading volume. The figure shows a volatile time series of daily trading volume, ranging from 15 in 2000 to 20.5 in 2021, with a maximum of more than 100." width="90%" />
<p class="caption">(\#fig:fig104)Total daily trading volume in billion USD.</p>
</div>

The figure indicates a clear upwards trend in aggregated daily trading volume. In particular, since the outbreak of the COVID-19 pandemic, markets have processed substantial trading volumes, as analyzed, for instance, by @Goldstein2021.\index{Covid 19}
One way to illustrate the persistence of trading volume would be to plot volume on day $t$ against volume on day $t-1$ as in the example below. We add a dotted 45°-line to indicate a hypothetical one-to-one relation by `geom_abline()`, addressing potential differences in the axes' scales.


```r
volume |>
  ggplot(aes(x = lag(volume), y = volume)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1),
    linetype = "dashed"
  ) +
  labs(
    x = "Previous day aggregate trading volume",
    y = "Aggregate trading volume",
    title = "Persistence in daily trading volume of DOW index constituents"
  )
```

```
Warning: Removed 1 rows containing missing values (geom_point).
```

<div class="figure" style="text-align: center">
<img src="10_introduction_files/figure-html/fig105-1.png" alt="Title: Persistence in daily trading volume of DOW index constituents. The figure shows a scatterplot where aggregate trading volume and previous-day aggregate trading volume neatly line up along a 45-degree line." width="90%" />
<p class="caption">(\#fig:fig105)Total daily trading volume in billion USD.</p>
</div>

Do you understand where the warning `## Warning: Removed 1 rows containing missing values (geom_point).` comes from and what it means? Purely eye-balling reveals that days with high trading volume are often followed by similarly high trading volume days.\index{Error message}

## Portfolio choice problems

In the previous part, we show how to download stock market data and inspect it with graphs and summary statistics. 
Now, we move to a typical question in Finance: how to allocate wealth across different assets optimally.\index{Portfolio choice} The standard framework for optimal portfolio selection considers investors that prefer higher future returns but dislike future return volatility (defined as the square root of the return variance): the *mean-variance investor* [@Markowitz1952].\index{Markowitz optimization}

\index{Efficient frontier} An essential tool to evaluate portfolios in the mean-variance context is the *efficient frontier*, the set of portfolios which satisfies the condition that no other portfolio exists with a higher expected return but with the same volatility (the square root of the variance, i.e., the risk), see, e.g., @Merton1972.\index{Return volatility}
We compute and visualize the efficient frontier for several stocks. 
First, we extract each asset's *monthly* returns. 
In order to keep things simple, we work with a balanced panel and exclude DOW constituents for which we do not observe a price on every single trading day since the year 2000.


```r
index_prices <- index_prices |>
  group_by(symbol) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n == max(n)) |>
  select(-n)

returns <- index_prices |>
  mutate(month = floor_date(date, "month")) |>
  group_by(symbol, month) |>
  summarize(price = last(adjusted), .groups = "drop_last") |>
  mutate(ret = price / lag(price) - 1) |>
  drop_na(ret) |>
  select(-price)
```

Here, `floor_date()` is a function from the `lubridate` package [@lubridate], which provides useful functions to work with dates and times. 

Next, we transform the returns from a tidy tibble into a $(T \times N)$ matrix with one column for each of the $N$ tickers and one row for each of the $T$ trading days to compute the sample average return vector $$\hat\mu = \frac{1}{T}\sum\limits_{t=1}^T r_t$$ where $r_t$ is the $N$ vector of returns on date $t$ and the sample covariance matrix $$\hat\Sigma = \frac{1}{T-1}\sum\limits_{t=1}^T (r_t - \hat\mu)(r_t - \hat\mu)'.$$ 
We achieve this by using `pivot_wider()` with the new column names from the column `symbol` and setting the values to `ret`.
We compute the vector of sample average returns and the sample variance-covariance matrix, which we consider as proxies for the parameters of the distribution of future stock returns. 
Thus, for simplicity, we refer to $\Sigma$ and $\mu$ instead of explicitly highlighting that the sample moments are estimates. \index{Covariance} In later chapters, we discuss the issues that arise once we take estimation uncertainty into account. 


```r
returns_matrix <- returns |>
  pivot_wider(
    names_from = symbol,
    values_from = ret
  ) |>
  select(-month)

Sigma <- cov(returns_matrix)
mu <- colMeans(returns_matrix)
```

Then, we compute the minimum variance portfolio weights $\omega_\text{mvp}$ as well as the expected portfolio return $\omega_\text{mvp}'\mu$ and volatility $\sqrt{\omega_\text{mvp}'\Sigma\omega_\text{mvp}}$ of this portfolio. 
\index{Minimum variance portfolio} Recall that the minimum variance portfolio is the vector of portfolio weights that are the solution to 
$$\omega_\text{mvp} = \arg\min w'\Sigma w \text{ s.t. } \sum\limits_{i=1}^Nw_i = 1.$$
The constraint that weights sum up to one simply implies that all funds are distributed across the available asset universe, i.e., there is no possibility to retain cash. 
It is easy to show analytically, that $\omega_\text{mvp} = \frac{\Sigma^{-1}\iota}{\iota'\Sigma^{-1}\iota}$ where $\iota$ is a vector of ones and $\Sigma^{-1}$ is the inverse of $\Sigma$. 


```r
N <- ncol(returns_matrix)
iota <- rep(1, N)
mvp_weights <- solve(Sigma) %*% iota
mvp_weights <- mvp_weights / sum(mvp_weights)

tibble(
  average_ret = as.numeric(t(mvp_weights) %*% mu),
  volatility = as.numeric(sqrt(t(mvp_weights) %*% Sigma %*% mvp_weights))
)
```

```
# A tibble: 1 × 2
  average_ret volatility
        <dbl>      <dbl>
1     0.00766     0.0316
```

The command `solve(A, b)` returns the solution of a system of equations $Ax = b$. If `b` is not provided, as in the example above, it defaults to the identity matrix such that `solve(Sigma)` delivers $\Sigma^{-1}$ (if a unique solution exists).  
Note that the *monthly* volatility of the minimum variance portfolio is of the same order of magnitude as the *daily* standard deviation of the individual components. Thus, the diversification benefits in terms of risk reduction are tremendous!\index{Diversification}

Next, we set out to find the weights for a portfolio that achieves, as an example, three times the expected return of the minimum variance portfolio. 
However, mean-variance investors are not interested in any portfolio that achieves the required return but rather in the efficient portfolio, i.e., the portfolio with the lowest standard deviation. 
If you wonder where the solution $\omega_\text{eff}$ comes from: \index{Efficient portfolio} The efficient portfolio is chosen by an investor who aims to achieve minimum variance *given a minimum acceptable expected return* $\bar{\mu}$. Hence, their objective function is to choose $\omega_\text{eff}$ as the solution to
$$\omega_\text{eff}(\bar{\mu}) = \arg\min w'\Sigma w \text{ s.t. } w'\iota = 1 \text{ and } \omega'\mu \geq \bar{\mu}.$$

The code below implements the analytic solution to this optimization problem for a benchmark return $\bar\mu$, which we set to 3 times the expected return of the minimum variance portfolio. We encourage you to verify that it is correct. 


```r
mu_bar <- 3 * t(mvp_weights) %*% mu

C <- as.numeric(t(iota) %*% solve(Sigma) %*% iota)
D <- as.numeric(t(iota) %*% solve(Sigma) %*% mu)
E <- as.numeric(t(mu) %*% solve(Sigma) %*% mu)

lambda_tilde <- as.numeric(2 * (mu_bar - D / C) / (E - D^2 / C))
efp_weights <- mvp_weights +
  lambda_tilde / 2 * (solve(Sigma) %*% mu - D * mvp_weights)
```

## The efficient frontier 

\index{Separation theorem} The mutual fund separation theorem states that as soon as we have two efficient portfolios (such as the minimum variance portfolio $w_{mvp}$ and the efficient portfolio for a higher required level of expected returns $\omega_\text{eff}(\bar{\mu})$, we can characterize the entire efficient frontier by combining these two portfolios. 
That is, any linear combination of the two portfolio weights will again represent an efficient portfolio. 
\index{Efficient frontier} The code below implements the construction of the *efficient frontier*, which characterizes the highest expected return achievable at each level of risk. To understand the code better, make sure to familiarize yourself with the inner workings of the `for` loop.


```r
c <- seq(from = -0.4, to = 1.9, by = 0.01)
res <- tibble(
  c = c,
  mu = NA,
  sd = NA
)

for (i in seq_along(c)) {
  w <- (1 - c[i]) * mvp_weights + (c[i]) * efp_weights
  res$mu[i] <- 12 * 100 * t(w) %*% mu
  res$sd[i] <- 12 * sqrt(100) * sqrt(t(w) %*% Sigma %*% w)
}
```

The code above proceeds in two steps: First, we compute a vector of combination weights $c$ and then we evaluate the resulting linear combination with $c\in\mathbb{R}$:   
$$w^* = cw_\text{eff}(\bar\mu) + (1-c)w_{mvp} = \omega_\text{mvp} + \frac{\lambda^*}{2}\left(\Sigma^{-1}\mu -\frac{D}{C}\Sigma^{-1}\iota \right)$$ with $\lambda^* = 2\frac{c\bar\mu + (1-c)\tilde\mu - D/C}{E-D^2/C}$. 
Finally, it is simple to visualize the efficient frontier alongside the two efficient portfolios within one powerful figure using `ggplot2`. We also add the individual stocks in the same call. 
We compute annualized returns based on the simple assumption that monthly returns are independent and identically distributed. Thus, the average annualized return is just 12 times the expected monthly return.\index{Graph!Efficient frontier} 


```r
res |>
  ggplot(aes(x = sd, y = mu)) +
  geom_point() +
  geom_point(
    data = res |> filter(c %in% c(0, 1)),
    size = 4
  ) +
  geom_point(
    data = tibble(
      mu = 12 * 100 * mu,
      sd = 12 * 10 * sqrt(diag(Sigma))
    ),
    aes(y = mu, x = sd), size = 1
  ) +
  labs(
    x = "Annualized standard deviation (in percent)",
    y = "Annualized expected return (in percent)",
    title = "Efficient frontier for DOW index constituents"
  )
```

<div class="figure" style="text-align: center">
<img src="10_introduction_files/figure-html/fig106-1.png" alt="Title: Efficient frontier for DOW index constituents. The figure shows DOW index constituents in a mean-variance diagram. A hyperbola indicates the efficient frontier of portfolios that dominate the individual holdings in the sense that they deliver higher expected returns for the same level of volatility." width="90%" />
<p class="caption">(\#fig:fig106)The big dots indicate the location of the minimum variance and efficient tangency portfolios, respectively. The small dots indicate the location of the individual constituents.</p>
</div>

The line indicates the efficient frontier: the set of portfolios a mean-variance efficient investor would choose from. Compare the performance relative to the individual assets (the dots) - it should become clear that diversifying yields massive performance gains (at least as long as we take the parameters $\Sigma$ and $\mu$ as given).

## Exercises

1. Download daily prices for another stock market ticker of your choice from Yahoo!Finance with `tq_get()` from the `tidyquant` package. Plot two time series of the ticker's un-adjusted and adjusted closing prices. Explain the differences.
1. Compute daily net returns for the asset and visualize the distribution of daily returns in a histogram. Also, use `geom_vline()` to add a dashed red line that indicates the 5 percent quantile of the daily returns within the histogram. Compute summary statistics (mean, standard deviation, minimum and maximum) for the daily returns
1. Take your code from before and generalize it such that you can perform all the computations for an arbitrary vector of tickers (e.g., `ticker <- c("AAPL", "MMM", "BA")`). Automate the download, the plot of the price time series, and create a table of return summary statistics for this arbitrary number of assets.
1. Consider the research question: Are days with high aggregate trading volume often also days with large absolute price changes? Find an appropriate visualization to analyze the question.
1. Compute monthly returns from the downloaded stock market prices. Compute the vector of historical average returns and the sample variance-covariance matrix. Compute the minimum variance portfolio weights and the portfolio volatility and average returns. Visualize the mean-variance efficient frontier. Choose one of your assets and identify the portfolio which yields the same historical volatility but achieves the highest possible average return.
1. In the portfolio choice analysis, we restricted our sample to all assets trading every day since 2000. How is such a decision a problem when you want to infer future expected portfolio performance from the results?
1. The efficient frontier characterizes the portfolios with the highest expected return for different levels of risk, i.e., standard deviation. Identify the portfolio with the highest expected return per standard deviation. Hint: the ratio of expected return to standard deviation is an important concept in Finance. 
