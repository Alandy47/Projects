### SET-UP ----
library(skimr)
library(readxl)
library(correlation)
library(dplyr)
library(tidymodels)
library(tidyverse)
library(correlation)
library(GGally)
library(ranger)
library(cowplot)
library(packcircles)

games <- read_excel("C:/Users/aland/OneDrive/Desktop/HGEN612/Projects/Assignment_3/Data-Games/board_games_clean.xlsx")

### DATA CLEANING -----
skimr::skim(games)

games_new = games

games_new[games_new == "NA"] = NA

str(games_new)

games_new$Themes = toupper(games_new$Theme)
games_new$Themes = as.factor(games_new$Themes)

games_new$Mechanic = toupper(games_new$Category)
games_new$Mechanic = as.factor(games_new$Mechanic)

games_new$Publishers = as.factor(games_new$Publisher)

games_analysis <- dplyr::select(games_new,1:5, 8, 9, 11:15)
games_analysis <-
  games_analysis %>%
  relocate(Publishers, .after = Years) %>% 
  relocate(Themes, .after = Playing_time) %>%
  relocate(Mechanic, .after = Themes) %>% 
  rename(Play_time = Playing_time) %>% 
  rename(Year_published = Years) %>% 
  rename(Total_ratings = Number_of_ratings)

skimr::skim(games_analysis)
str(games_analysis)

### Data View -----
games_analysis %>% 
  as_tibble()

set.seed(123)
games_small <- games_analysis %>%
  slice_sample(n = 100) %>% 
  mutate(Themes_group = fct_lump(Themes, n = 10),
         Mechanic_group = fct_lump(Mechanic, n = 10),
         Pub_group = fct_lump(Publishers, n = 10)) %>%
  select(Max_players, Min_players, 
         Themes_group, Mechanic_group,
         Mechanic_no.,Year_published, 
         Average_rating, Pub_group, 
         Total_ratings)

ggpairs(games_small, cardinality_threshold = 100)


### Side bar -----

r h3("Objective:")

r h3("Predict which hotel stays include children.")

### Data Processing -----
## Data Spliting
set.seed(123)
games_splits <- 
  games_analysis %>%
  slice(1:1000) %>% 
  select(-NAME) %>% 
  initial_split(prop = 0.6,
                strata = Average_rating)

## Recipe
games_recipe <-
  recipe(Average_rating ~ ., data = training(games_splits)) %>%
  step_unknown(Publishers, new_level = "unknown") %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_naomit() %>% 
  step_shuffle()

games_boot <- bootstraps(training(games_splits))

## Xboost Model
games_xboost <- boost_tree(trees = tune(),
                           learn_rate = tune(),
                           mtry = tune(),
                           tree_depth = tune(),
                           min_n = tune(),
                           loss_reduction = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

## Xboost Workflow
xboost_wflow <- 
  workflow() %>%
  add_model(games_xboost) %>%
  add_recipe(games_recipe)

# RMSE
set.seed(123)
xboost_grid <- tune_grid(
  xboost_wflow,
  resamples = games_boot,
  grid = 100)

lowest_rmse <- select_best(xboost_grid, metric = "rmse")

final_xboost_fit <- 
  finalize_workflow(xboost_wflow, lowest_rmse) %>%
  fit(data = training(games_splits))

# Predictions
final_xboost_fit %>% 
  predict(training(games_splits))

# Validation
final_xboost_fit %>%
  predict(testing(games_splits)) %>%
  bind_cols(testing(games_splits)) %>%
  metrics(truth = Average_rating, estimate = .pred)

### Outcome Plots -----
## Bubble Plot (plotly) (reactive) [min player no. vs duration]
clusters <- split(games_analysis, games_analysis$Mechanic)

sampled_clusters <- lapply(clusters, function(df) {
  if (nrow(df) <= 15) {
    df
  } else {
    df %>% slice_sample(n = 15)
  }
})

games_sampled <- bind_rows(sampled_clusters)

radius <- log(games_sampled$Play_time)
games_sampled_final <- games_sampled %>% 
  mutate(Radius = radius)

Acting_circles <-games_sampled_final %>%
  filter(Mechanic == "ACTING") %>% 
  select(NAME, Min_players, Play_time, Themes, Mechanic, Average_rating, Radius)


## Data Splitting
# **Total Observations**
dim(games_analysis)[1] %>% scales::comma()

# **Training Set**  
dim(training(games_splits))[1] %>% scales::comma()

# **Validation Set**  
(dim(training(games_splits))[1] * prop.validation) %>% scales::comma()

# **Testing Set**  
dim(testing(games_splits))[1] %>% scales::comma()


## Top 10 Popular Mechanics
games_analysis %>%
  separate_rows(Mechanic, sep = ";") %>%
  mutate(Mechanic = str_trim(Mechanic)) %>%
  count(Mechanic, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Mechanic, n), y = n)) +
  geom_col() +
  coord_flip()

## Rating generator? (reactive)


## Data table (reactive)

## Game predictor Quiz


### Model evaluation Plots -----
## Model Accuracy (linear regression)
final_xboost_fit %>% 
  augment(new_data = testing(games_splits)) %>% 
  ggplot(aes(x = .pred, y = Average_rating)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

## Prediction Errors
aug <- final_xboost_fit %>%
  predict(testing(games_splits)) %>%
  bind_cols(testing(games_splits)) %>%
  mutate(residual = Average_rating - .pred)

## Prediction Metrics
metrics <- final_xboost_fit %>%
  predict(testing(games_splits)) %>%
  bind_cols(testing(games_splits)) %>%
  metrics(truth = Average_rating, estimate = .pred)

## VIP Plot
final_xboost_fit %>% 
  extract_fit_parsnip() %>%
  vip::vip() +
  geom_col(aes(fill = Variable))

## Data overview -----
## Data Table (Shiny)

## Dictionary
data.dictionary <- 
  dictionary(games_analysis)



