library(tidymodels)
library(dplyr)
library(readr)
library(tidyverse)
library(embed)
library(vroom)
library(recipes)
library(embed)
library(workflows)
library(rpart)
library(ranger)
library(dials)
library(themis)

data <- read_csv("features.csv")
test <- read_csv("test.csv")
train <- read_csv("train.csv")

test_2 <- test |>
  mutate(
    year = year(Date),
    month = month(Date),
    week = week(Date)
  )

train_weighted <- train %>%
  mutate(
  Year = year(Date),
  Month = month(Date),
  Week = week(Date),
  holiday_weight = if_else(IsHoliday == TRUE, 5, 1)  # Create numeric weights first
) %>%
  mutate(
    holiday_weight = importance_weights(holiday_weight)  # Overwrite with case weights object
  )

my_recipe <- recipe(Weekly_Sales ~ ., data=train_weighted) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors()) 

prepped_recipe <- prep(my_recipe) 
bake(prepped_recipe, new_data=train_weighted)

my_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees=200) %>%
  set_engine("ranger") %>%
  set_mode("regression")

walmart_wf <- workflow() |>
  add_recipe(my_recipe) |>
  add_model(my_mod)

folds <- vfold_cv(train_weighted, v = 5, repeats=1)

tuning_grid <- grid_regular(finalize(mtry(), train_weighted),
                            min_n(range = c(5, 40)),
                            levels = 6
)

CV_results <- walmart_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse))

best_tune <- CV_results |>
  select_best(metric="rmse")

final_wf <- walmart_wf |>
  finalize_workflow(best_tune)|>
  fit(data = train_weighted)

walmart_predictions <- final_wf %>%
  predict(new_data=test_2,
          type="prob") %>%
  bind_cols(test_2) %>%
  rename(Weekly_Sales=.pred_1) %>%
  select(id,Weekly_Sales)