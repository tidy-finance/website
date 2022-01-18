# Parametric Portfolio Policies



In this section, we introduce different portfolio performance measures to evaluate and compare different allocation strategies. For this purpose, we introduce a direct (and probably simplest) way to estimate optimal portfolio weights when stock characteristics are related to the stock’s expected return, variance, and covariance with other stocks: We parametrize weights as a function of the characteristics such that we maximize expected utility. The approach is feasible for large portfolio dimensions (such as the entire CRSP universe) and has been proposed by Brand et al. (2009) in their influential paper *Parametric Portfolio Policies: Exploiting Characteristics in the Cross Section of Equity Returns*. 

To get started, we load the monthly CRSP file which forms our investment universe. 

```r
library(tidyverse)
library(lubridate)
library(RSQLite)
```


```r
# Load data from database
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance.sqlite", extended_types = TRUE)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") %>%
  collect()
```

For the purpose of performance evaluation, we will need the Fama French 3 factor model monthly returns in order to be able to compute CAPM alphas. 

```r
factors_ff_monthly <- tbl(tidy_finance, "factors_ff_monthly") %>%
  collect()
```

Next we create some characteristics that have been proposed in the literature to have an effect on the expected returns or expected variances (or even higher moments) of the return distribution: We record for each firm the lagged one-year return (mom) defined as the compounded return between months $t − 13$ and $t − 2$ and the firm's market equity (me), defined as the log of the price per share times the number of shares outstanding


```r
crsp_monthly_lags <- crsp_monthly %>% 
  transmute(permno, 
            month_12 = month %m+% months(12), 
            month_1 = month %m+% months(1),
            altprc = abs(altprc))

crsp_monthly <- crsp_monthly %>% 
  inner_join(crsp_monthly_lags %>% select(-month_1), 
             by = c("permno", "month" = "month_12"), suffix = c("", "_12")) %>% 
  inner_join(crsp_monthly_lags %>% select(-month_12), 
             by = c("permno", "month" = "month_1"), suffix = c("", "_1"))

# Create characteristics for portfolio tilts
crsp_monthly <- crsp_monthly %>%
  group_by(permno) %>%
  mutate(
    momentum_lag = altprc_1 / altprc_12, # Gross returns TODO for SV: Do we need slider?
    size_lag = log(mktcap_lag)
  ) %>%
  drop_na(contains("lag"))
```

The basic idea of parametric portfolio weights is easy to explain: Suppose that at each date $t$ we have $N_t$ stocks in the investment universe, where each stock $i$ has return of $r_{i, t+1}$ and is associated with a vector of firm characteristics $x_{i, t}$ such as time-series momentum or the market capitalization. The investors problem is to choose portfolio weights $w_{i,t}$ to maximize the expected utility of the portfolio return:
$$\begin{align}
\max_{w} E_t\left(u(r_{p, t+1})\right) = E_t\left[u\left(\sum\limits_{i=1}^{N_t}w_{i,t}r_{i,t+1}\right)\right].
\end{align}$$
Where do the stock characteristics show up? We parametrize the optimal portfolio weights as a function of $x_{i,t}$ with the following linear specification for the portfolio weights:
$$w_{i,t} = \bar{w}_{i,t} + \frac{1}{N_t}\theta'\hat{x}_{i,t}$$ where $\bar{w}_{i,t}$ is the weight of a benchmark portfolio (we use the value-weighted or naive portfolio in the application below), $\theta$ is a vector of coefficients which we are going to estimate and $\hat{x}_{i,t}$ are the characteristics of stick $i$, cross-sectionally standardized to have zero mean and unit standard deviation. Think of the portfolio strategy as a form of active portfolio management relative to a performance benchmark: Deviations from the benchmark portfolio are derived from the individual stock characteristics. Note that by construction the weights sum up to one as $\sum_{i=1}^{N_t}\hat{x}_{i,t} = 0$ due to the standardization. Note that the coefficients are constant across assets and through time. The implicit assumption is that the characteristics fully capture all aspects of the joint distribution of returns that are relevant for forming optimal portfolios.       

To get started, we first implement the cross-sectional standardization for the entire CRSP universe. We also keep track of (lagged) relative market capitalization which will represent the value-weighted benchmark portfolio.

```r
crsp_monthly <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    n = n(), # number of traded assets N_t (benchmark for naive portfolio)
    relative_mktcap = mktcap_lag / sum(mktcap_lag), # Value weighting benchmark
    across(contains("lag"), ~ (. - mean(.)) / sd(.)), # standardization. Note: Code handles every column with "lag" as a characteristic)
  ) %>%
  ungroup() %>%
  select(-mktcap_lag, -altprc)
```

Next we can move to optimal choices of $\theta$. We rewrite the optimization problem together with the weight parametrisation and can then estimate $\theta$ to maximize the objective function based on our sample 
$$\begin{align}
E_t\left(u(r_{p, t+1})\right) = \frac{1}{T}\sum\limits_{t=0}^{T-1}u\left(\sum\limits_{i=1}^{N_t}\left(\bar{w}_{i,t} + \frac{1}{N_t}\theta'\hat{x}_{i,t}\right)r_{i,t+1}\right).
\end{align}$$
The allocation strategy is simple because the number of parameters to estimate is small. Instead of a tedious specification of the $N_t$ dimensional vector of expected returns and the $N_t(N_t+1)/2$ free elements of the variance covariance, all we need to focus on in our application is the vector $\theta$ which contains only 2 elements - the relative deviation from the benchmark due to size and due to past returns. 

To get a feeling on the performance of such an allocation strategy we start with an arbitrary vector $\theta$ - the next step is then to choose $\theta$ in an optimal fashion to maximize the objective function. 


```r
# Automatic detection of parameters and initialization of parameter vector theta
number_of_param <- sum(grepl("lag", crsp_monthly %>% colnames()))
theta <- rep(1.5, number_of_param) # We start with some arbitrary initial values for theta
names(theta) <- colnames(crsp_monthly)[grepl("lag", crsp_monthly %>% colnames())]
```

The following function computes the portfolio weights $\bar{w}_{i,t} + \frac{1}{N_t}\theta'\hat{x}_{i,t}$ according to our parametrization for a given value of $\theta$ - everything in one pipeline, thus here goes a quick walk-through what is happening:
The weights are computed for each month by first computing the tilting values $\frac{1}{N_t}\theta'\hat{x}_{i, t}$ away from the benchmark portfolio. Then, we compute the benchmark portfolio which can in principle be any reasonable set of weights, in our case we either choose the value- or equal-weighted allocation. `weight_tilt` completes the picture and contains the final portfolio weights which deviate from the benchmark portfolio depending on the tilting. The final few lines go a bit further and implement a simple version of a no-short sale constraint: To ensure portfolio constraints via the parametrization is not straightforward but in our case we simply renormalize the portfolio weights before returning them by computing 
$$w_{i,t}^+ = \frac{\max(0, w_{i,t})}{\sum\limits_{j=1}^{N_t}\max(0, w_{i,t})}.$$
Here we go to optimal portfolio weights in 20 lines. 

```r
compute_portfolio_weights <- function(theta,
                                      data,
                                      value_weighting,
                                      allow_short_selling) {
  data %>%
    group_by(month) %>% # Aggregate to compute portfolio returns
    bind_cols(
      characteristic_tilt = data %>% # Computes theta'x / N_t
        transmute(across(contains("lag"), ~ . / n)) %>%
        as.matrix() %*% theta %>% as.numeric()
    ) %>%
    mutate(
      weight_benchmark = case_when( # Definition of the benchmark weight
        value_weighting == TRUE ~ relative_mktcap,
        value_weighting == FALSE ~ 1 / n
      ),
      weight_tilt = weight_benchmark + characteristic_tilt, # Parametric Portfolio Weights
      weight_tilt = case_when( # Extension as of Brandt, Santa-Clara and Valkanoff: Short-sell constraint
        allow_short_selling == TRUE ~ weight_tilt,
        allow_short_selling == FALSE ~ pmax(0, weight_tilt)
      ),
      weight_tilt = weight_tilt / sum(weight_tilt) # Weights sum up to 1 (even if no-short selling)
    ) %>%
    ungroup()
}
```

Done! Next step is to compute the optimal portfolio weights at your convenience.

```r
weights_crsp <- compute_portfolio_weights(theta,
  crsp_monthly,
  value_weighting = TRUE,
  allow_short_selling = TRUE
)
```

But are these weights optimal in any way? Most likely not as we chose $\theta$ arbitrarily but in order to evaluate the performance of an allocation strategy, one can think of many different approaches. One simple setup as above could be to simply evaluate the hypothetical utility of an agent equipped with a power utility function $u_\gamma(r) = \frac{(1 + r)^\gamma}{1-\gamma}$, implement in R in the function below. 

```r
power_utility <- function(r, gamma = 5) { # gamma is the risk aversion factor
  (1 + r)^(1 - gamma) / (1 - gamma)
}
```

No doubt, there are many more ways to evaluate a portolio. The function below provides an exhaustive summary of interesting measures which all can be considered relevant. 
Do we need all these evaluation measures? It depends: The original paper only cares about
expected utility in order to choose $\theta$. But if you want to choose optimal values that achieve the highest performance while putting some constraints on your portfolio weights it is helpful to have everything in one function.


```r
evaluate_portfolio <- function(weights_crsp,
                               full_evaluation = TRUE) {

  evaluation <- weights_crsp %>%
    group_by(month) %>%
    # Compute monthly portfolio returns
    summarise(
      return_tilt = weighted.mean(ret_excess, weight_tilt),
      return_benchmark = weighted.mean(ret_excess, weight_benchmark)
    ) %>%
    pivot_longer(-month, values_to = "portfolio_return", names_to = "Model") %>%
    group_by(Model) %>%
    left_join(factors_ff_monthly, by = "month") %>% # FF data to compute alpha
    summarise(tibble(
      "Expected utility" = mean(power_utility(portfolio_return)),
      "Average return" = 100 * mean(12 * portfolio_return),
      "SD return" = 100 * sqrt(12) * sd(portfolio_return),
      "Sharpe ratio" = mean(portfolio_return) / sd(portfolio_return),
      "CAPM alpha" = coefficients(lm(portfolio_return ~ mkt_excess))[1],
      "Market beta" = coefficients(lm(portfolio_return ~ mkt_excess))[2]
    )) %>%
    mutate(Model = gsub("return_", "", Model)) %>%
    pivot_longer(-Model, names_to = "measure") %>%
    pivot_wider(names_from = Model, values_from = value)

  if (full_evaluation) { # additional values based on the portfolio weights
    weight_evaluation <- weights_crsp %>%
      select(month, contains("weight")) %>%
      pivot_longer(-month, values_to = "weight", names_to = "Model") %>%
      group_by(Model, month) %>%
      transmute(tibble(
        "Absolute weight" = abs(weight),
        "Max. weight" = max(weight),
        "Min. weight" = min(weight),
        "Avg. sum of negative weights" = -sum(weight[weight < 0]),
        "Avg. fraction of negative weights" = sum(weight < 0) / n()
      )) %>%
      group_by(Model) %>%
      summarise(across(-month, ~ 100 * mean(.))) %>%
      mutate(Model = gsub("weight_", "", Model)) %>%
      pivot_longer(-Model, names_to = "measure") %>%
      pivot_wider(names_from = Model, values_from = value)
    evaluation <- bind_rows(evaluation, weight_evaluation)
  }
  return(evaluation)
}
```

Let's take a look at the different portfolio strategies and evaluation measures.

```r
evaluate_portfolio(weights_crsp)
```

```
## # A tibble: 11 x 3
##   measure          benchmark      tilt
##   <chr>                <dbl>     <dbl>
## 1 Expected utility    -0.249 -0.261   
## 2 Average return       6.86  -0.0509  
## 3 SD return           15.3   20.2     
## 4 Sharpe ratio         0.129 -0.000726
## # ... with 7 more rows
```
The value weighted portfolio delivers an annualized return of above 6 percent and clearly outperforms the tilted portfolio, irrespective of whether we evaluate expected utility, the Sharpe ratio or the CAPM alpha. We can conclude the the market beta is close to one for both strategies (naturally almost identically 1 for the value-weighted benchmark portfolio). When it comes to the distribution of the portfolio weights, we see that the benchmark portfolio weight takes less extreme positions (lower average absolute weights and lower maximum weight). By definition, the value-weighted benchmark does not take any negative positions, while the tilted portfolio also takes short positions. 

Next we move to a choice of $\theta$ that actually aims to to improve some (or all) of the performance measures. We first define a helper function `compute_objective_function` which is then passed to R's optimization schemes. 

```r
compute_objective_function <- function(theta,
                                       data,
                                       objective_measure = "Expected utility",
                                       value_weighting,
                                       allow_short_selling) {
  processed_data <- compute_portfolio_weights(
    theta,
    data,
    value_weighting,
    allow_short_selling
  )

  objective_function <- evaluate_portfolio(processed_data, full_evaluation = FALSE) %>%
    filter(measure == objective_measure) %>%
    pull(tilt)

  return(-objective_function)
}
```
You may wonder why we return the negative value of the objective function. This is simply due to the common convention for optimization procedures to search minima as a default. By minimizing the negative value of the objective function we will get the maximum value as a result.
Optimization in R in its most basic form is done by `optim`. As main inputs, the function requires an initial "guess" of the parameters, and the function to minimize. Next, we are therefore equipped to compute the optimal values of $\theta$ which maximize hypothetical expected utility of the investor. 


```r
optimal_theta <- optim(
  par = theta, # Initial vector of thetas (can be any value)
  compute_objective_function,
  objective_measure = "Expected utility",
  data = crsp_monthly,
  value_weighting = TRUE,
  allow_short_selling = TRUE
)

optimal_theta$par # Optimal values
```

```
## momentum_lag     size_lag 
##    0.5889182   -2.0715581
```
The chosen values of $\theta$ are easy to interpret on an intuitive basis: Expected utility increases by tilting weights from the value weighted portfolio towards smaller stocks (negative coefficient for size) and towards past winners (positive value for momentum). 

A final open question is then: How does the portfolio perform for different model specifications?


```r
full_model_grid <- expand_grid(
  value_weighting = c(TRUE, FALSE),
  allow_short_selling = c(TRUE, FALSE),
  data = list(crsp_monthly)
) %>%
  mutate(optimal_theta = pmap(
    .l = list(
      data,
      value_weighting,
      allow_short_selling
    ),
    .f = ~ optim(
      par = rep(0, number_of_param),
      compute_objective_function,
      data = ..1,
      objective_measure = "Expected utility",
      value_weighting = ..2,
      allow_short_selling = ..3,
    )$par
  ))
```

Finally, let's get to the comparison. The table below shows summary statistics for all possible combinations: Equal- or Value-weighted benchmark portfolio, with or without short-selling constraints. 

```r
table <- full_model_grid %>%
  mutate(
    processed_data = pmap(
      .l = list(optimal_theta, data, value_weighting, allow_short_selling),
      .f = ~ compute_portfolio_weights(..1, ..2, ..3, ..4)
    ),
    portfolio_evaluation = map(processed_data, evaluate_portfolio, full_evaluation = TRUE)
  ) %>%
  select(value_weighting, allow_short_selling, portfolio_evaluation) %>%
  unnest(portfolio_evaluation)

table %>%
  rename(
    " " = benchmark,
    Optimal = tilt
  ) %>%
  mutate(
    value_weighting = case_when(
      value_weighting == TRUE ~ "VW",
      value_weighting == FALSE ~ "EW"
    ),
    allow_short_selling = case_when(
      allow_short_selling == TRUE ~ "",
      allow_short_selling == FALSE ~ "(no s.)"
    )
  ) %>%
  pivot_wider(
    names_from = value_weighting:allow_short_selling,
    values_from = " ":Optimal,
    names_glue = "{value_weighting} {allow_short_selling} {.value} "
  ) %>%
  select(measure, `EW    `, `VW    `, sort(contains("Optimal"))) %>%
  kableExtra::kable()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> measure </th>
   <th style="text-align:right;"> EW     </th>
   <th style="text-align:right;"> VW     </th>
   <th style="text-align:right;"> VW  Optimal  </th>
   <th style="text-align:right;"> VW (no s.) Optimal  </th>
   <th style="text-align:right;"> EW  Optimal  </th>
   <th style="text-align:right;"> EW (no s.) Optimal  </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Expected utility </td>
   <td style="text-align:right;"> -0.2504435 </td>
   <td style="text-align:right;"> -0.2494920 </td>
   <td style="text-align:right;"> -0.2464129 </td>
   <td style="text-align:right;"> -0.2469501 </td>
   <td style="text-align:right;"> -0.2500495 </td>
   <td style="text-align:right;"> -0.2478171 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Average return </td>
   <td style="text-align:right;"> 10.4556311 </td>
   <td style="text-align:right;"> 6.8578289 </td>
   <td style="text-align:right;"> 14.6887869 </td>
   <td style="text-align:right;"> 13.4218964 </td>
   <td style="text-align:right;"> 13.0155232 </td>
   <td style="text-align:right;"> 17.8046560 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SD return </td>
   <td style="text-align:right;"> 20.3327722 </td>
   <td style="text-align:right;"> 15.3478935 </td>
   <td style="text-align:right;"> 20.3340003 </td>
   <td style="text-align:right;"> 19.5943432 </td>
   <td style="text-align:right;"> 22.4020814 </td>
   <td style="text-align:right;"> 25.1791701 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sharpe ratio </td>
   <td style="text-align:right;"> 0.1484441 </td>
   <td style="text-align:right;"> 0.1289874 </td>
   <td style="text-align:right;"> 0.2085319 </td>
   <td style="text-align:right;"> 0.1977391 </td>
   <td style="text-align:right;"> 0.1677191 </td>
   <td style="text-align:right;"> 0.2041275 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CAPM alpha </td>
   <td style="text-align:right;"> 0.0022956 </td>
   <td style="text-align:right;"> 0.0001043 </td>
   <td style="text-align:right;"> 0.0066398 </td>
   <td style="text-align:right;"> 0.0052779 </td>
   <td style="text-align:right;"> 0.0045022 </td>
   <td style="text-align:right;"> 0.0085555 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Market beta </td>
   <td style="text-align:right;"> 1.1351691 </td>
   <td style="text-align:right;"> 0.9924393 </td>
   <td style="text-align:right;"> 0.9907202 </td>
   <td style="text-align:right;"> 1.0448926 </td>
   <td style="text-align:right;"> 1.1221929 </td>
   <td style="text-align:right;"> 1.1111658 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Absolute weight </td>
   <td style="text-align:right;"> 0.0246651 </td>
   <td style="text-align:right;"> 0.0246651 </td>
   <td style="text-align:right;"> 0.0396050 </td>
   <td style="text-align:right;"> 0.0246651 </td>
   <td style="text-align:right;"> 0.0266874 </td>
   <td style="text-align:right;"> 0.0246651 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Max. weight </td>
   <td style="text-align:right;"> 0.0246651 </td>
   <td style="text-align:right;"> 3.5163475 </td>
   <td style="text-align:right;"> 3.3310942 </td>
   <td style="text-align:right;"> 2.6400830 </td>
   <td style="text-align:right;"> 0.3701690 </td>
   <td style="text-align:right;"> 0.1703370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Min. weight </td>
   <td style="text-align:right;"> 0.0246651 </td>
   <td style="text-align:right;"> 0.0000278 </td>
   <td style="text-align:right;"> -0.0454467 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> -0.0422016 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Avg. sum of negative weights </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 31.0181376 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 4.0653550 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Avg. fraction of negative weights </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 39.3535887 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 10.4464146 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
</tbody>
</table>
