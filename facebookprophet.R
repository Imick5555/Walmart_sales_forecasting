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
library(prophet)

data <- read_csv("features.csv")
test <- read_csv("test.csv")

train <- read_csv("train.csv")


features <- data %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe)) 

fullTrain <- left_join(train, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday=IsHoliday.x) %>%
  select(-MarkDown_Total)
fullTest <- left_join(test, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday=IsHoliday.x) %>%
  select(-MarkDown_Total)

store <- 5
dept <- 5

sd_train <- fullTrain %>%
  filter(Store==store, Dept==dept) %>%
  rename(y=Weekly_Sales, ds=Date)
sd_test <- fullTest %>%
  filter(Store==store, Dept==dept) %>%
  rename(ds=Date)

prophet_model <- prophet() %>%
  add_regressor("CPI") %>%
  add_regressor("Fuel_Price") %>%
  add_regressor("Temperature") %>%
  add_regressor("Unemployment") %>%
  fit.prophet(df=sd_train)

fitted_vals <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values2
test_preds <- predict(prophet_model, df=sd_test)

ggplot() +
  geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="")
