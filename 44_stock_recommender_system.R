library(tidyverse)
library(scales)
library(recommenderlab)

# Prepare data ------------------------------------------------------------
# TODO: check whether we can use wikifolio data and how to host it (maybe pin?)
wikifolio_portfolios <- read_rds("data/wikifolio_portfolios.rds") %>%
  select(wikifolio = WikifolioIsin,
         stock = UnderlyingIsin,
         quantity = UnderlyingQuantity,
         price = UnderlyingPrice,
         weight = UnderlyingPortfolioWeight) %>%
  mutate(in_portfolio = 1) %>%
  group_by(stock) %>%
  filter(n_distinct(wikifolio) > 1) %>%
  ungroup() %>%
  group_by(wikifolio) %>%
  filter(n_distinct(stock) > 1) %>%
  ungroup()

# Convert long data to binary rating matrix where only non-NA values are stored
# explicitely and NA values are represented by dots
binary_rating_matrix <- wikifolio_portfolios %>%
  pivot_wider(id_cols = wikifolio,
              names_from = stock,
              values_from = in_portfolio,
              values_fill = list(in_portfolio = 0)) %>%
  select(-wikifolio) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")
binary_rating_matrix

# Define evaluation schemes -----------------------------------------------
# cross validation: split in to training and test data
# k-fold: k-times recursive estimation of cross validation
# given: -1 means that all but 1 randomly selected item is ignored for evaluation
scheme <- binary_rating_matrix %>%
  evaluationScheme(method = "cross",
                   k      = 5,
                   train  = 0.8,
                   given  = -1)
scheme

# Run different algorithms ------------------------------------------------
recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")

# specify set of algorithms
# popular = number of users who have the item in their profile
# support = how frequently the item set appears
# confidence = how often the rule has been found to be true
# nn = number of nearest neighbors
algorithms <- list(
  "Random Items"             = list(name  = "RANDOM",  param = NULL),
  "Popular Items"            = list(name  = "POPULAR", param = NULL),
  "Association Rules"        = list(name  = "AR", param = list(supp = 0.01, conf = 0.1)),
  "Item-Based Filtering"     = list(name  = "IBCF", param = list(k = 10)),
  "User-Based Filtering"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 100))
)

# run algorithms
number_of_recommendations <- c(1, 3, 5, 10)
results <- evaluate(scheme,
                    algorithms,
                    type = "topNList",
                    progress = TRUE,
                    n = number_of_recommendations
)
results

# get evaluation metrics averaged of cross-validation folds
results_tbl <- results %>%
  avg() %>%
  map(as_tibble) %>%
  bind_rows(.id = "model")

# Plot TPR vs FPR  --------------------------------------------------------
# TPR = true positive rate = True Positives / (True Positives + False Negatives)
# FPR = false positive rate = False Positive / (False Positive + True Negative)
# Note: this thing is also called ROC curve
results_tbl %>%
  ggplot(aes(FPR, TPR, colour = model)) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(x = "False Positive Rate (FPR)",
       y = "True Positive Rate (TPR)",
       title = "Item-based filtering achieves the highest TPR given any FPR",
       colour = "Model") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

# Precision-recall curves -------------------------------------------------
# Precision: True Positives / (True Positives + False Positives)
# Recall / Sensitivity / TPR: True Positives / (True Positives + False Negatives)
# Note: goal is to have a higher precision for any level of recall
results_tbl %>%
  ggplot(aes(x = recall, y = precision, colour = model)) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(x = "Recall", y = "Precision",
       title = "Item-based filtering achieves higher precision than other models",
       colour = "Model") +
  theme_grey(base_size = 14) +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

# Create recommender ------------------------------------------------------
recommender <- Recommender(binary_rating_matrix, method = "IBCF")

# Create predictions for examples -----------------------------------------
sample_portfolio <- c("US67066G1040", "US0378331005")
sample_rating_matrix <- tibble(distinct(wikifolio_portfolios, stock)) %>%
  mutate(in_portfolio = if_else(stock %in% sample_portfolio, 1, 0)) %>%
  pivot_wider(names_from = stock,
              values_from = in_portfolio,
              values_fill = list(in_portfolio = 0)) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")

prediction <- predict(recommender, sample_rating_matrix, n = 1)
as(prediction, "list")

# Save image  -------------------------------------------------------------
save.image("44_stock_recommender_system.RData")

# Render html -------------------------------------------------------------
rmarkdown::render("44_stock_recommender_system.Rmd")
