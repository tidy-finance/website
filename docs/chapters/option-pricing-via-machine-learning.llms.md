# Option Pricing via Machine Learning

This chapter covers machine learning methods in option pricing. First, we briefly introduce regression trees, random forests, and neural networks; these methods are advocated as highly flexible *universal approximators*, capable of recovering highly non-linear structures in the data. As the focus is on implementation, we leave a thorough treatment of the statistical underpinnings to other textbooks from authors with a real comparative advantage on these issues. We show how to implement random forests and deep neural networks with tidy principles.

Machine learning (ML) is seen as a part of artificial intelligence. ML algorithms build a model based on training data in order to make predictions or decisions without being explicitly programmed to do so. While ML can be specified along a vast array of different branches, this chapter focuses on so-called supervised learning for regressions. The basic idea of supervised learning algorithms is to build a mathematical model for data that contains both the inputs and the desired outputs. In this chapter, we apply well-known methods such as random forests and neural networks to a simple application in option pricing. More specifically, we create an artificial dataset of option prices for different values based on the Black-Scholes pricing equation for call options. Then, we train different models to *learn* how to price call options without prior knowledge of the theoretical underpinnings of the famous option pricing equation by Black and Scholes ([1973](#ref-Black1976)).

We use the following packages throughout this chapter:

## R

``` r
library(tidyverse)
library(tidymodels)
library(torch)
library(brulee)
library(ranger)
library(glmnet)
```

The package `torch` ([Falbel et al. 2023](#ref-torch)) provides functionality to define and train neural networks and is based on `PyTorch` ([Paszke et al. 2019](#ref-PyTorch2019)), while `brulee` ([Kuhn and Falbel 2023](#ref-brulee)) provides several basic modeling functions that use the `torch` infrastructure. The package `ranger` ([Wright and Ziegler 2017](#ref-ranger)) provides a fast implementation for random forests.

## Python

``` python
import polars as pl
import numpy as np

from plotnine import *
from itertools import product
from scipy.stats import norm
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.neural_network import MLPRegressor
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Lasso
```

We rely on `scikit-learn` to implement random forests and neural networks with tidy principles.

## Regression Trees and Random Forests

Regression trees are a popular ML approach for incorporating multiway predictor interactions. In Finance, regression trees are gaining popularity, also in the context of asset pricing (see, e.g., [Bryzgalova et al. 2022](#ref-Bryzgalova2022)). Trees possess a logic that departs markedly from traditional regressions. Trees are designed to find groups of observations that behave similarly to each other. A tree *grows* in a sequence of steps. At each step, a new *branch* sorts the data leftover from the preceding step into bins based on one of the predictor variables. This sequential branching slices the space of predictors into partitions and approximates the unknown function \\f(x)\\ which yields the relation between the predictors \\x\\ and the outcome variable \\y\\ with the average value of the outcome variable within each partition. For a more thorough treatment of regression trees, we refer to Coqueret and Guida ([2020](#ref-Coqueret2020)).

Formally, we partition the predictor space into \\J\\ non-overlapping regions, \\R_1, R_2, \ldots, R_J\\. For any predictor \\x\\ that falls within region \\R_j\\, we estimate \\f(x)\\ with the average of the training observations, \\\hat y_i\\, for which the associated predictor \\x_i\\ is also in \\R_j\\. Once we select a partition \\x\\ to split in order to create the new partitions, we find a predictor \\j\\ and value \\s\\ that define two new partitions, called \\R_1(j,s)\\ and \\R_2(j,s)\\, which split our observations in the current partition by asking if \\x_j\\ is bigger than \\s\\:

\\ R_1(j,s) = \\x \mid x_j \< s\\ \mbox{ and } R_2(j,s) = \\x \mid x_j \geq s\\. \tag{1}\\

To pick \\j\\ and \\s\\, we find the pair that minimizes the residual sum of squares (RSS):

\\ \sum\_{i:\\ x_i \in R_1(j,s)} (y_i - \hat{y}\_{R_1})^2 + \sum\_{i:\\ x_i \in R_2(j,s)} (y_i - \hat{y}\_{R_2})^2 \tag{2}\\

As in [Factor Selection via Machine Learning](../chapters/factor-selection-via-machine-learning.llms.md) in the context of penalized regressions, the first relevant question is: what are the hyperparameter decisions? Instead of a regularization parameter, trees are fully determined by the number of branches used to generate a partition (sometimes one specifies the minimum number of observations in each final branch instead of the maximum number of branches).

Models with a single tree may suffer from high predictive variance. Random forests address these shortcomings of decision trees. The goal is to improve the predictive performance and reduce instability by averaging multiple regression trees. A forest basically implies creating many regression trees and averaging their predictions. To assure that the individual trees are not the same, we use a bootstrap to induce randomness. More specifically, we build \\B\\ decision trees \\T_1, \ldots, T_B\\ using the training sample. In addition, only a random subset of the features is considered as split candidates at each node of each tree. For each observation in the test set, we then form a prediction \\\hat{y} = \frac{1}{B}\sum\limits\_{i=1}^B\hat{y}\_{T_i}\\.

## Neural Networks

Roughly speaking, neural networks propagate information from an input layer, through one or multiple hidden layers, to an output layer. While the number of units (neurons) in the input layer is equal to the dimension of the predictors, the output layer usually consists of one neuron (for regression) or multiple neurons for classification. The output layer predicts the future data, similar to the fitted value in a regression analysis. Neural networks have theoretical underpinnings as *universal approximators* for any smooth predictive association ([Hornik 1991](#ref-Hornik1991)). Their complexity, however, ranks neural networks among the least transparent, least interpretable, and most highly parameterized ML tools. In finance, applications of neural networks can be found in many different contexts, e.g., Avramov et al. ([2022](#ref-Avramov2022)), Chen et al. ([2023](#ref-Chen2019)), and Gu et al. ([2020](#ref-Gu2020)).

Each neuron applies a non-linear *activation function* \\f\\ to its aggregated signal before sending its output to the next layer \\ x_k^l = f\left(\theta^k\_{0} + \sum\limits\_{j = 1}^{N ^l}z_j\theta\_{l,j}^k\right) \tag{3}\\

Here, \\\theta\\ are the parameters to fit, \\N^l\\ denotes the number of units (a hyperparameter to tune), and \\z_j\\ are the input variables which can be either the raw data or, in the case of multiple chained layers, the outcome from a previous layer \\z_j = x_j^{l-1}\\. While the easiest case with \\f(x) = \alpha + \beta x\\ resembles linear regression, typical activation functions are sigmoid (i.e., \\f(x) = (1+e^{-x})^{-1}\\) or ReLU (i.e., \\f(x) = \max(x, 0)\\).

Neural networks gain their flexibility from chaining multiple layers together. Naturally, this imposes many degrees of freedom on the network architecture for which no clear theoretical guidance exists. The specification of a neural network requires, at a minimum, a stance on depth (number of hidden layers), the activation function, the number of neurons, the connection structure of the units (dense or sparse), and the application of regularization techniques to avoid overfitting. Finally, *learning* means to choose optimal parameters relying on numerical optimization, which often requires specifying an appropriate learning rate.

## R

Despite these computational challenges, implementation in R is not tedious at all because we can use the API to `torch`.

## Python

Despite these computational challenges, implementation in Python is not tedious at all because we can use the API to `scikit-learn`.

## Option Pricing

To apply ML methods in a relevant field of finance, we focus on option pricing. The application in its core is taken from Hull ([2020](#ref-Hull2020)). In its most basic form, call options give the owner the right but not the obligation to buy a specific stock (the underlying) at a specific price (the strike price \\K\\) at a specific date (the exercise date \\T\\). The Black-Scholes price ([Black and Scholes 1973](#ref-Black1976)) of a call option for a non-dividend-paying underlying stock is given by \\ \begin{aligned} C(S, T) &= \Phi(d_1)S - \Phi(d_2)Ke^{-r T} \\ d_1 &= \frac{1}{\sigma\sqrt{T}}\left(\ln\left(\frac{S}{K}\right) + \left(r + \frac{\sigma^2}{2}\right)T\right) \\ d_2 &= d_1 - \sigma\sqrt{T} \end{aligned} \tag{4}\\

where \\C(S, T)\\ is the price of the option as a function of today’s stock price of the underlying, \\S\\, with time to maturity \\T\\, \\r\\ is the risk-free interest rate, and \\\sigma\\ is the volatility of the underlying stock return. \\\Phi\\ is the cumulative distribution function of a standard normal random variable.

The Black-Scholes equation provides a way to compute the arbitrage-free price of a call option once the parameters \\S, K, r, T\\, and \\\sigma\\ are specified (arguably, in a realistic context, all parameters are easy to specify except for \\\sigma\\ which has to be estimated). A simple function allows computing the price as we do below.

## R

``` r
black_scholes_price <- function(S, K, r, T, sigma) {

  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)

  return(price)
}
```

## Python

``` python
def black_scholes_price(S, K, r, T, sigma):
    """Calculate Black Scholes option price."""

    d1 = (np.log(S / K) + (r + sigma**2 / 2) * T) / (sigma * np.sqrt(T))
    d2 = d1 - sigma * np.sqrt(T)
    price = S * norm.cdf(d1) - K * np.exp(-r * T) * norm.cdf(d2)

    return price
```

## Learning Black-Scholes

We illustrate the concept of ML by showing how ML methods *learn* the Black-Scholes equation after observing some different specifications and corresponding prices without us revealing the exact pricing equation.

### Data simulation

To that end, we start with simulated data. We compute option prices for call options for a grid of different combinations of times to maturity (`T`), risk-free rates (`r`), volatilities (`sigma`), strike prices (`K`), and current stock prices (`S`). In the code below, we add an idiosyncratic error term to each observation such that the prices considered do not exactly reflect the values implied by the Black-Scholes equation.

In order to keep the analysis reproducible, we set a random seed. A random seed specifies the start point when a computer generates a random number sequence and ensures that our simulated data is the same across different machines.

## R

``` r
set.seed(420)
torch_manual_seed(420)

option_prices <- expand_grid(
  S = 40:60,
  K = 20:90,
  r = seq(from = 0, to = 0.05, by = 0.01),
  T = seq(from = 3 / 12, to = 2, by = 1 / 12),
  sigma = seq(from = 0.1, to = 0.8, by = 0.1)
) |>
  mutate(
    black_scholes = black_scholes_price(S, K, r, T, sigma),
    observed_price = black_scholes + rnorm(n(), sd = 0.15)
  )
```

Note that `set.seed()` controls the random numbers that R generates, e.g., for the noise term above, while `torch_manual_seed()` ensures that the training of neural networks via `torch` further below is reproducible as well.

## Python

``` python
random_state = 42
np.random.seed(random_state)

S = np.arange(40, 61)
K = np.arange(20, 91)
r = np.arange(0, 0.051, 0.01)
T = np.arange(3 / 12, 2.01, 1 / 12)
sigma = np.arange(0.1, 0.81, 0.1)

option_prices = pl.DataFrame(
    list(product(S, K, r, T, sigma)),
    schema=["S", "K", "r", "T", "sigma"],
    orient="row",
)

option_prices = option_prices.with_columns(
    black_scholes=black_scholes_price(
        option_prices["S"].to_numpy(),
        option_prices["K"].to_numpy(),
        option_prices["r"].to_numpy(),
        option_prices["T"].to_numpy(),
        option_prices["sigma"].to_numpy(),
    )
)

option_prices = option_prices.with_columns(
    observed_price=pl.col("black_scholes")
    + np.random.normal(scale=0.15, size=option_prices.height)
)
```

The code above generates more than 1.5 million random parameter constellations. For each of these values, the *true* prices reflecting the Black-Scholes model are given and a random innovation term *pollutes* the observed prices. The intuition of this application is simple: the simulated data provides many observations of option prices, by using the Black-Scholes equation we can evaluate the actual predictive performance of a ML method, which would be hard in a realistic context where the actual arbitrage-free price would be unknown.

Next, we split the data into a training set (which contains 1 percent of all the observed option prices) and a test set that will only be used for the final evaluation.

## R

Note that the entire grid of possible combinations contains 1,574,496 different specifications. Thus, the sample to learn the Black-Scholes price contains only 15,744 observations and is therefore relatively small.

``` r
split <- initial_split(option_prices, prop = 1 / 100)
```

We process the training dataset further before we fit the different ML models. We define a `recipe()` that defines all processing steps for that purpose. For our specific case, we want to explain the observed price by the five variables that enter the Black-Scholes equation. The *true* prices (stored in column `black_scholes`) should obviously not be used to fit the model. The recipe also reflects that we standardize all predictors to ensure that each variable exhibits a sample average of zero and a sample standard deviation of one.

``` r
rec <- recipe(observed_price ~ .,
  data = training(split)
) |>
  step_rm(black_scholes) |>
  step_normalize(all_predictors())
```

## Python

Note that the entire grid of possible combinations contains 1,574,496 different specifications. Thus, the sample to learn the Black-Scholes price contains only 15,744 observations and is therefore relatively small.

``` python
train_data, test_data = train_test_split(
    option_prices.to_pandas(), train_size=0.01, random_state=random_state
)
```

We process the training dataset further before we fit the different ML models. We define a `ColumnTransformer()` that defines all processing steps for that purpose. For our specific case, we want to explain the observed price by the five variables that enter the Black-Scholes equation. The *true* price (stored in column `black_scholes`) should obviously not be used to fit the model. The recipe also reflects that we standardize all predictors via `StandardScaler()` to ensure that each variable exhibits a sample average of zero and a sample standard deviation of one.

``` python
preprocessor = ColumnTransformer(
    transformers=[
        (
            "normalize_predictors",
            StandardScaler(),
            ["S", "K", "r", "T", "sigma"],
        )
    ],
    remainder="drop",
)
```

### Single layer networks and random forests

Next, we show how to fit a neural network to the data. The specification below defines a single layer feed-forward neural network with ten hidden units. Note that we fix the hyperparameters of all models in this chapter at sensible values to focus on the implementation workflow. In a real application, they should be tuned via cross-validation, as we illustrate in [Factor Selection via Machine Learning](../chapters/factor-selection-via-machine-learning.llms.md).

## R

Note that this requires that `torch` is installed on your local machine. The function `mlp()` from the package `parsnip` provides the functionality to initialize a single layer, feed-forward neural network. We set the number of training iterations to `epochs = 500`. The option `set_mode("regression")` specifies a linear activation function for the output layer.

``` r
nnet_model <- mlp(
  epochs = 500,
  hidden_units = 10,
  activation = "sigmoid",
  penalty = 0.0001
) |>
  set_mode("regression") |>
  set_engine("brulee", verbose = FALSE)
```

The `verbose = FALSE` argument prevents logging the results to the console. We can follow the straightforward `tidymodels` workflow as in [Factor Selection via Machine Learning](../chapters/factor-selection-via-machine-learning.llms.md): define a workflow, equip it with the recipe, and specify the associated model. Finally, fit the model with the training data.

``` r
nnet_fit <- workflow() |>
  add_recipe(rec) |>
  add_model(nnet_model) |>
  fit(data = training(split))
```

## Python

The function `MLPRegressor()` from the package `scikit-learn` provides the functionality to initialize a single layer, feed-forward neural network. To keep the specification consistent with the R implementation, we use the logistic (sigmoid) activation function, the quasi-Newton solver LBFGS (which is also the default optimizer of `brulee`), and the same weight decay penalty `alpha=0.0001`. We set the number of training iterations to `max_iter=500`.

``` python
max_iter = 500

nnet_model = MLPRegressor(
    hidden_layer_sizes=10,
    activation="logistic",
    solver="lbfgs",
    alpha=0.0001,
    max_iter=max_iter,
    random_state=random_state,
)
```

We can follow the straightforward workflow as in the chapter before: define a workflow, equip it with the recipe, and specify the associated model. Finally, fit the model with the training data.

``` python
nnet_pipeline = Pipeline(
    [("preprocessor", preprocessor), ("regressor", nnet_model)]
)

nnet_fit = nnet_pipeline.fit(
    train_data.drop(columns=["observed_price"]),
    train_data.get("observed_price"),
)
```

One word of caution regarding the training of neural networks: For illustrative purposes, we sequentially update the parameters by reiterating through the training data many times. Typically, however, early stopping rules are advised that aim to interrupt the process of updating parameters as soon as the predictive performance on a validation set seems to deteriorate. A detailed discussion of these implementation details would go beyond the scope of this book.

Once you are familiar with the workflow, it is a piece of cake to fit other models. For instance, the model below initializes a random forest with 50 trees contained in the ensemble, where a node must contain at least 100 observations to be split further and three of the five predictors are randomly selected as split candidates at each node.

## R

The random forests are trained using the package `ranger`, which is required to be installed in order to run the code below.

``` r
rf_model <- rand_forest(
  mtry = 3,
  trees = 50,
  min_n = 100
) |>
  set_engine("ranger") |>
  set_mode("regression")
```

Fitting the model follows exactly the same convention as for the neural network before.

``` r
rf_fit <- workflow() |>
  add_recipe(rec) |>
  add_model(rf_model) |>
  fit(data = training(split))
```

## Python

The random forests are trained using the function `RandomForestRegressor()`, where the arguments `min_samples_split` and `max_features` correspond to `min_n` and `mtry` in the R implementation.

``` python
rf_model = RandomForestRegressor(
    n_estimators=50,
    min_samples_split=100,
    max_features=3,
    random_state=random_state,
)
```

Fitting the model follows exactly the same convention as for the neural network before.

``` python
rf_pipeline = Pipeline(
    [("preprocessor", preprocessor), ("regressor", rf_model)]
)

rf_fit = rf_pipeline.fit(
    train_data.drop(columns=["observed_price"]),
    train_data.get("observed_price"),
)
```

### Deep neural networks

A deep neural network is a neural network with multiple layers between the input and output layers. By chaining multiple layers together, more complex structures can be represented with fewer parameters than simple shallow (one-layer) networks as the one implemented above. For instance, image or text recognition are typical tasks where deep neural networks are used (for applications of deep neural networks in finance, see, for instance, [Jiang et al. 2023](#ref-Jiang2022); [Jensen et al. 2022](#ref-Jensen2022)).

## R

Fitting deep neural networks is straightforward within the `tidymodels` workflow: `mlp()` accepts a vector of hidden units, where each element corresponds to one hidden layer. The code snippet below initializes a network with three hidden layers with ten units per layer. Under the hood, the `brulee` package translates this specification into a `torch` model and is flexible enough to handle different activation functions.

``` r
deep_nnet_model <- mlp(
  epochs = 500,
  hidden_units = c(10, 10, 10),
  activation = "sigmoid",
  penalty = 0.0001
) |>
  set_mode("regression") |>
  set_engine("brulee", verbose = FALSE)
```

Since we reuse the workflow logic from above, fitting the deep neural network requires no additional steps. Note that we again equip the workflow with the recipe `rec`: Reusing the same recipe ensures that all models are trained with identically processed data, e.g., with standardized predictors, such that a change in the recipe is reflected in the performance of every model.

``` r
deep_nnet_fit <- workflow() |>
  add_recipe(rec) |>
  add_model(deep_nnet_model) |>
  fit(data = training(split))
```

## Python

The following code chunk implements a deep neural network with three hidden layers of size ten each, using the same activation function, solver, and penalty as for the single-layer network above.

``` python
deep_nnet_model = MLPRegressor(
    hidden_layer_sizes=(10, 10, 10),
    activation="logistic",
    solver="lbfgs",
    alpha=0.0001,
    max_iter=max_iter,
    random_state=random_state,
)

deep_nnet_pipeline = Pipeline(
    [("preprocessor", preprocessor), ("regressor", deep_nnet_model)]
)

deep_nnet_fit = deep_nnet_pipeline.fit(
    train_data.drop(columns=["observed_price"]),
    train_data.get("observed_price"),
)
```

### Universal approximation

Before we evaluate the results, we implement one more model. In principle, any non-linear function can also be approximated by a linear model containing the input variables’ polynomial expansions and interaction terms. We fit a Lasso regression model with the polynomially expanded predictors and a pre-specified penalty term (consult [Factor Selection via Machine Learning](../chapters/factor-selection-via-machine-learning.llms.md) on how to tune the model hyperparameters).

## R

We first define a new recipe, `rec_linear`, which processes the training data even further: We include polynomials up to the fifth degree of each predictor and then add all possible pairwise interaction terms. The final recipe step, `step_lincomb()`, removes potentially redundant variables (for instance, the interaction between \\r^2\\ and \\r^3\\ is the same as the term \\r^5\\).

``` r
rec_linear <- rec |>
  step_poly(all_predictors(),
    degree = 5,
    options = list(raw = TRUE)
  ) |>
  step_interact(terms = ~ all_predictors():all_predictors()) |>
  step_lincomb(all_predictors())

lm_model <- linear_reg(penalty = 0.01, mixture = 1) |>
  set_engine("glmnet")

lm_fit <- workflow() |>
  add_recipe(rec_linear) |>
  add_model(lm_model) |>
  fit(data = training(split))
```

## Python

The function `PolynomialFeatures()` generates all polynomial and interaction terms of the predictors up to a total degree of five. Note that the resulting set of features hence differs slightly from the R implementation, which expands each predictor individually up to the fifth degree before adding pairwise interactions of the expanded terms. We use the same penalty value as in the R implementation.

``` python
lm_pipeline = Pipeline(
    [
        (
            "polynomial",
            PolynomialFeatures(
                degree=5, interaction_only=False, include_bias=True
            ),
        ),
        ("scaler", StandardScaler()),
        ("regressor", Lasso(alpha=0.01)),
    ]
)

lm_fit = lm_pipeline.fit(
    train_data.get(["S", "K", "r", "T", "sigma"]),
    train_data.get("observed_price"),
)
```

## Prediction Evaluation

Finally, we collect all predictions to compare the *out-of-sample* prediction error evaluated on 10,000 new data points.

## R

Note that the fitted workflows automatically apply the recipe steps to new data at prediction time, which ensures that all models use consistently pre-processed testing data. We collect the fitted workflows in a named list and call `augment()` on each of them, which returns the data augmented with a column of predictions, `.pred`.

``` r
out_of_sample_data <- testing(split) |>
  slice_sample(n = 10000)

model_fits <- list(
  "Deep NN" = deep_nnet_fit,
  "Lasso" = lm_fit,
  "Random forest" = rf_fit,
  "Single layer" = nnet_fit
)

predictive_performance <- model_fits |>
  map(augment, new_data = out_of_sample_data) |>
  list_rbind(names_to = "Model") |>
  mutate(
    moneyness = S - K,
    pricing_error = abs(.pred - black_scholes)
  )
```

## Python

``` python
out_of_sample_data = test_data.sample(n=10000, random_state=random_state)
test_X = out_of_sample_data.get(["S", "K", "r", "T", "sigma"])
test_y = out_of_sample_data.get("observed_price")

predictive_performance = (
    pl.from_pandas(out_of_sample_data)
    .with_columns(
        **{
            "Random forest": rf_fit.predict(test_X),
            "Single layer": nnet_fit.predict(test_X),
            "Deep NN": deep_nnet_fit.predict(test_X),
            "Lasso": lm_fit.predict(test_X),
        }
    )
    .unpivot(
        index=[
            "S",
            "K",
            "r",
            "T",
            "sigma",
            "black_scholes",
            "observed_price",
        ],
        on=["Random forest", "Single layer", "Deep NN", "Lasso"],
        variable_name="Model",
        value_name="Predicted",
    )
    .with_columns(
        moneyness=pl.col("S") - pl.col("K"),
        pricing_error=(pl.col("Predicted") - pl.col("black_scholes")).abs(),
    )
)
```

In the lines above, we use each of the fitted models to generate predictions for the entire test dataset of option prices. We evaluate the absolute pricing error as one possible measure of pricing accuracy, defined as the absolute value of the difference between predicted option price and the theoretical correct option price from the Black-Scholes model. We show the results graphically in [Figure 1](#fig-431).

## R

``` r
predictive_performance |>
  ggplot(aes(
    x = moneyness,
    y = pricing_error,
    color = Model,
    linetype = Model
    )) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  facet_wrap(~Model, ncol = 2) +
  labs(
    x = "Moneyness (S - K)",
    y = "Absolute prediction error (USD)",
    title = "Prediction errors of call option prices for different models"
  ) +
  theme(legend.position = "none")
```

[![Title: Prediction errors of call option prices for different models. The figure shows the pricing error of the different machine learning methods for call options for different levels of moneyness (stock price minus strike price). The figure indicates variation across the models and across moneyness.](option-pricing-via-machine-learning_files/figure-html/fig-431-1.png)](option-pricing-via-machine-learning_files/figure-html/fig-431-1.png "Figure 1: Absolute prediction error in USD for the different fitted methods. The prediction error is evaluated on a sample of call options that were not used for training.")

Figure 1: Absolute prediction error in USD for the different fitted methods. The prediction error is evaluated on a sample of call options that were not used for training.

## Python

``` python
predictive_performance_figure = (
    ggplot(
        predictive_performance,
        aes(x="moneyness", y="pricing_error")
    )
    + geom_point(alpha=0.05)
    + facet_wrap("Model")
    + labs(
        x="Moneyness (S - K)", y="Absolute prediction error (USD)",
        title="Prediction errors of call option prices for different models"
        )
    + theme(legend_position="")
)
predictive_performance_figure.show()
```

[![Title: Prediction errors of call option prices for different models. The figure shows the pricing error of the different machine learning methods for call options for different levels of moneyness (stock price minus strike price). The figure indicates variation across the models and across moneyness.](option-pricing-via-machine-learning_files/figure-html/option-pricing-via-machine-learning-fig-431-py-1.png)](option-pricing-via-machine-learning_files/figure-html/option-pricing-via-machine-learning-fig-431-py-1.png "Absolute prediction error in USD for the different fitted methods. The prediction error is evaluated on a sample of call options that were not used for training.")

Absolute prediction error in USD for the different fitted methods. The prediction error is evaluated on a sample of call options that were not used for training.

The results can be summarized as follows.

## R

1.  All ML methods seem to be able to price call options after observing the training set.
2.  The average prediction errors increase for far in-the-money options.
3.  Random forest and the Lasso seem to perform consistently worse in predicting option prices than the neural networks.
4.  The complexity of the deep neural network relative to the single-layer neural network does not result in better out-of-sample predictions.

## Python

1.  All ML methods seem to be able to price call options after observing the training set.
2.  Random forest and the Lasso seem to perform consistently worse in predicting option prices than the neural networks.
3.  For random forest and Lasso, the average prediction errors increase for far in-the-money options.
4.  The increased complexity of the deep neural network relative to the single-layer neural network results in lower prediction errors.

## Key Takeaways

- Machine learning methods like random forests and neural networks can be used to estimate call option prices without relying on the Black-Scholes formula.
- Simulating noisy option price data and applying supervised learning models via a tidy modeling framework provides a clean, reproducible analysis.
- Deep neural networks do not consistently outperform single-layer networks, underscoring the trade-off between model complexity and prediction performance.

## Exercises

1.  Write a function that takes `y` and a matrix of predictors `X` as inputs and returns a characterization of the relevant parameters of a regression tree with one branch.
2.  Create a function that creates predictions for a new matrix of predictors `newX` based on the estimated regression tree.
3.  Use the package `rpart` (R) or the module `sklearn.tree` (Python) to *grow* a tree based on the training data and use the corresponding illustration tools to understand which characteristics the tree deems relevant for option pricing.
4.  Make use of a training and a test set to choose the optimal depth (number of sample splits) of the tree.
5.  Use `brulee` (R) or `scikit-learn` (Python) to initialize a neural network that takes the predictors from the training dataset as input, contains at least one hidden layer, and generates continuous predictions. How many parameters does the neural network you aim to fit have?
6.  Fit the network from the previous exercise. Pay special attention to the loss function used for training. Illustrate the difference in predictive accuracy for different architecture choices, e.g., regarding the depth of the network or the number of hidden units.
7.  The test data in this chapter is sampled from the same parameter grid as the training data, so the models only have to *interpolate*. Design an experiment that evaluates *extrapolation* instead: For instance, train the models only on options with volatilities \\\sigma \leq 0.5\\ and evaluate the pricing errors for options with \\\sigma \> 0.5\\. Which of the methods deteriorates most, and why are regression trees particularly ill-suited for extrapolation?

## References

Avramov, Doron, Si Cheng, and Lior Metzker. 2022. “Machine learning vs. economic restrictions: Evidence from stock return predictability.” *Management Science* 69 (5): 2547–3155. <http://dx.doi.org/10.2139/ssrn.3450322>.

Black, Fischer, and Myron Scholes. 1973. “The pricing of options and corporate liabilities.” *Journal of Political Economy* 81 (3): 637–54. <https://www.jstor.org/stable/1831029>.

Bryzgalova, Svetlana, Markus Pelger, and Jason Zhu. 2022. “Forest through the trees: Building cross-sections of stock returns.” *Working Paper*. <http://dx.doi.org/10.2139/ssrn.3493458>.

Chen, Luyang, Markus Pelger, and Jason Zhu. 2023. “Deep learning in asset pricing.” *Management Science*. <https://doi.org/10.48550/arXiv.1904.00745>.

Coqueret, Guillaume, and Tony Guida. 2020. *Machine learning for factor investing: R version*. Chapman; Hall/CRC. <https://www.mlfactor.com/>.

Falbel, Daniel, Javier Luraschi, Dmitriy Selivanov, et al. 2023. *torch: Tensors and Neural Networks with ’GPU’ Acceleration*. <https://CRAN.R-project.org/package=torch>.

Gu, Shihao, Bryan Kelly, and Dacheng Xiu. 2020. “Empirical asset pricing via machine learning.” *Review of Financial Studies* 33 (5): 2223–73. <https://doi.org/10.1093/rfs/hhaa009>.

Hornik, Kurt. 1991. “Approximation capabilities of multilayer feedforward networks.” *Neural Networks* 4 (2): 251–57. <https://doi.org/10.1016/0893-6080(91)90009-T>.

Hull, John C. 2020. *Machine learning in business. An introduction to the world of data science*. Independently published.

Jensen, Theis Ingerslev, Bryan T. Kelly, Semyon Malamud, and Lasse Heje Pedersen. 2022. “Machine Learning and the Implementable Efficient Frontier.” *Working Paper*. <https://doi.org/10.2139/ssrn.3491942>.

Jiang, Jingwen, Bryan T. Kelly, and Dacheng Xiu. 2023. “(Re-)Imag(in)ing Price Trends.” *The Journal of Finance* 78 (6): 3193–249. <https://doi.org/10.1111/jofi.13268>.

Kuhn, Max, and Daniel Falbel. 2023. *brulee: High-Level Modeling Functions with ’torch’*. <https://CRAN.R-project.org/package=brulee>.

Paszke, Adam, Sam Gross, Francisco Massa, et al. 2019. “PyTorch: An Imperative Style, High-Performance Deep Learning Library.” In *Advances in Neural Information Processing Systems 32*. Curran Associates, Inc. <http://papers.neurips.cc/paper/9015-pytorch-an-imperative-style-high-performance-deep-learning-library.pdf>.

Wright, Marvin N., and Andreas Ziegler. 2017. “ranger: A fast implementation of random forests for high dimensional data in C++ and R.” *Journal of Statistical Software* 77 (1): 1–17. <http://dx.doi.org/10.18637/jss.v077.i01>.
