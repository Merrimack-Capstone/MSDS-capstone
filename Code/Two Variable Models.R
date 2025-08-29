# Import Libraries 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(skimr)
library(knitr)
library(kableExtra)
library(tidyr)
library(ggpmisc)
library(forcats)
library(corrplot)
library(naniar)
library(reshape2)
library(scales)
library(stringr)
library(recipes)
library(broom)
library(here)
library(rlang)
library(rsample)
library(xts)
library(randomForest)
library(caTools)
library(caret)
library(RANN)
library(xgboost)
library(Matrix)
library(e1071)
library(parsnip)
library(tidymodels)
library(MASS)

print_regressions <- function(predictions, change = FALSE, model_name){
  chart_title <- "Actual vs Predicted - "
  low_bound <- round(min(c(predictions$Actual, 
                           predictions$Predicted), 
                         na.rm = TRUE), 1)
  high_bound <- round(max(c(predictions$Actual, 
                            predictions$Predicted), 
                          na.rm = TRUE), 1)
  if (change) {
    chart_title <- paste0(chart_title,"Change in HDI Index")
  } else {
    chart_title <- paste0(chart_title,"HDI Index")
  }
  chart_title <- paste0(chart_title, " - ",model_name)
  p_scatter <- ggplot(predictions, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "red",
                linetype = "dashed", linewidth = 1) +
    coord_equal(xlim = c(low_bound, high_bound), ylim = c(low_bound, high_bound)) +
    labs(
      title = chart_title,
      x = "Actual",
      y = "Predicted"
    ) +
    theme_minimal()
  print(p_scatter)
  
  # Residuals histogram
  predictions$Residuals <- predictions$Actual - predictions$Predicted
  p_resid <- ggplot(predictions, aes(x = Residuals)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = paste0("Residuals Distribution - ",model_name),
      x = "Residual (Actual - Predicted)",
      y = "Count"
    ) +
    theme_minimal()
  print(p_resid)
}

# Read in the saved training and test data sets
train_data <- readRDS(here::here("Data", "train_data.rds"))
test_data <- readRDS(here::here("Data", "test_data.rds"))

# MODEL 1Q:  Simple OLS - point in time variables

model_columns <- c(
  "HDI_Index", "InternetUsersPct"
  )

# Filter the training and test data to include only the specified columns

train_filtered <- train_data %>% dplyr::select(all_of(model_columns))
test_filtered <- test_data %>% dplyr::select(all_of(model_columns))

# Use na.omit() to remove rows with any NA values 

train_filtered <- na.omit(train_filtered)
test_filtered <- na.omit(test_filtered)

set.seed(123)

myrecipe <- recipe(HDI_Index ~ InternetUsersPct, data = train_filtered)

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)
predictions <- predictions %>%
  rename(Actual = HDI_Index) %>%
  rename(Predicted = .pred)
metrics <- metric_set(rmse, mae, rsq)
mape <- mean(abs((predictions$Actual - predictions$Predicted) / 
                   ifelse(predictions$Actual == 0, NA, predictions$Actual)),
             na.rm = TRUE) * 100
medAE <- median(abs(predictions$Actual - predictions$Predicted), na.rm = TRUE)
print("MODEL METRICS :  Model 1q")
print(metrics(predictions, truth = Actual, estimate = Predicted))
print(paste0("MAPE: ", mape))
print(paste0("Median AE: ", medAE))
print(tidy(extract_fit_parsnip(fitted_model)))
print(glance(extract_fit_parsnip(fitted_model)))
print_regressions(predictions, FALSE, "Model 1Q")

# MODEL 2Q:  Quadratic OLS with point-in-time variables

set.seed(123)
myrecipe <- recipe(HDI_Index ~ InternetUsersPct, data = train_filtered) %>%
  step_poly(InternetUsersPct, degree=2, options = list(raw = TRUE))

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)
predictions <- predictions %>%
  rename(Actual = HDI_Index) %>%
  rename(Predicted = .pred)

metrics <- metric_set(rmse, mae, rsq)
mape <- mean(abs((predictions$Actual - predictions$Predicted) / 
                   ifelse(predictions$Actual == 0, NA, predictions$Actual)),
             na.rm = TRUE) * 100
medAE <- median(abs(predictions$Actual - predictions$Predicted), na.rm = TRUE)
print("MODEL METRICS :  Model 2q")
print(metrics(predictions, truth = Actual, estimate = Predicted))
print(paste0("MAPE: ", mape))
print(paste0("Median AE: ", medAE))
print(tidy(extract_fit_parsnip(fitted_model)))
print(glance(extract_fit_parsnip(fitted_model)))
print_regressions(predictions, FALSE, "Model 2Q")
#
# MODEL 3Q:  Simple OLS using annual change in internet and HDI
#
model_columns <- c(
  "YearlyChgHDI", "YearlyChgInternet"
)

# Filter the training and test data to include only the specified columns

train_filtered <- train_data %>% dplyr::select(all_of(model_columns))
test_filtered <- test_data %>% dplyr::select(all_of(model_columns))

# Use na.omit() to remove rows with any NA values 

train_filtered <- na.omit(train_filtered)
test_filtered <- na.omit(test_filtered)

set.seed(123)

myrecipe <- recipe(YearlyChgHDI ~ YearlyChgInternet, data = train_filtered)

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)
predictions <- predictions %>%
  rename(Actual = YearlyChgHDI) %>%
  rename(Predicted = .pred)

metrics <- metric_set(rmse, mae, rsq)
mape <- mean(abs((predictions$Actual - predictions$Predicted) / 
                   ifelse(predictions$Actual == 0, NA, predictions$Actual)),
             na.rm = TRUE) * 100
medAE <- median(abs(predictions$Actual - predictions$Predicted), na.rm = TRUE)
print("MODEL METRICS :  Model 3q")
print(metrics(predictions, truth = Actual, estimate = Predicted))
print(paste0("MAPE: ", mape))
print(paste0("Median AE: ", medAE))
print(tidy(extract_fit_parsnip(fitted_model)))
print(glance(extract_fit_parsnip(fitted_model)))
print_regressions(predictions, TRUE, "Model 3Q")


ggplot(train_filtered, aes(x = YearlyChgInternet, y = YearlyChgHDI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    title = "Yearly Change: Internet Access vs HDI",
    x = "Yearly Change in Internet Access (%)",
    y = "Yearly Change in HDI"
  ) +
  theme_minimal()

#################################################
#
# MODEL 4Q: Simple OLS using 3 year change in internet access vs 3 year change
# in HDI

# Filter the training and test data to include only the specified columns

train_filtered <- train_data %>%
  dplyr::select(Cumulative3yrChg_InternetUsersPct, Cumulative3yrChg_HDI) %>%
  na.omit()
test_filtered <- test_data %>%
  dplyr::select(Cumulative3yrChg_InternetUsersPct, Cumulative3yrChg_HDI) %>%
  na.omit()


set.seed(123)

myrecipe <- recipe(Cumulative3yrChg_HDI ~ Cumulative3yrChg_InternetUsersPct, 
                   data = train_filtered)

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)
predictions <- predictions %>%
  rename(Actual = Cumulative3yrChg_HDI) %>%
  rename(Predicted = .pred)

metrics <- metric_set(rmse, mae, rsq)
mape <- mean(abs((predictions$Actual - predictions$Predicted) / 
                   ifelse(predictions$Actual == 0, NA, predictions$Actual)),
             na.rm = TRUE) * 100
medAE <- median(abs(predictions$Actual - predictions$Predicted), na.rm = TRUE)
print("MODEL METRICS :  Model 4q")
print(metrics(predictions, truth = Actual, estimate = Predicted))
print(paste0("MAPE: ", mape))
print(paste0("Median AE: ", medAE))
print(tidy(extract_fit_parsnip(fitted_model)))
print(glance(extract_fit_parsnip(fitted_model)))
print_regressions(predictions, TRUE, "Model 4Q")

ggplot(train_filtered, aes(x = Cumulative3yrChg_InternetUsersPct, y = Cumulative3yrChg_HDI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    title = "3 Year Change: Internet Access vs HDI",
    x = "3 Year Change in Internet Access (%)",
    y = "3 Year Change in HDI Index"
  ) +
  theme_minimal()

# MODEL 5Q: Simple OLS using 3 year change in internet against point in time HDI
train_filtered <- train_data %>%
  dplyr::select(Cumulative3yrChg_InternetUsersPct, HDI_Index) %>%
  na.omit()
test_filtered <- test_data %>%
  dplyr::select(Cumulative3yrChg_InternetUsersPct, HDI_Index) %>%
  na.omit()

set.seed(123)

myrecipe <- recipe(HDI_Index ~ Cumulative3yrChg_InternetUsersPct, data = train_filtered)

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)
predictions <- predictions %>%
  rename(Actual = HDI_Index) %>%
  rename(Predicted = .pred)
metrics <- metric_set(rmse, mae, rsq)
mape <- mean(abs((predictions$Actual - predictions$Predicted) / 
                   ifelse(predictions$Actual == 0, NA, predictions$Actual)),
             na.rm = TRUE) * 100
medAE <- median(abs(predictions$Actual - predictions$Predicted), na.rm = TRUE)
print("MODEL METRICS :  Model 5q")
print(metrics(predictions, truth = Actual, estimate = Predicted))
print(paste0("MAPE: ", mape))
print(paste0("Median AE: ", medAE))
print(tidy(extract_fit_parsnip(fitted_model)))
print(glance(extract_fit_parsnip(fitted_model)))
print_regressions(predictions, FALSE, "Model 5Q")

ggplot(train_filtered, aes(x = Cumulative3yrChg_InternetUsersPct, y = HDI_Index)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    title = "3 Year Change in Internet Access vs HDI",
    x = "3 Year Change in Internet Access (%)",
    y = "HDI Index"
  ) +
  theme_minimal()





############################################################################
#
# Predicting HDI CATEGORY

# Use the category outcome instead of the index
model_columns <- c("HDI_Category", "InternetUsersPct")

train_filtered <- train_data %>% dplyr::select(all_of(model_columns)) %>% drop_na()
test_filtered  <- test_data  %>% dplyr::select(all_of(model_columns)) %>% drop_na()

set.seed(123)

# Recipe: category ~ InternetUsersPct
myrecipe <- recipe(HDI_Category ~ InternetUsersPct, data = train_filtered)

# Model: multinomial logistic regression
mymodel <- multinom_reg() %>% 
  set_engine("nnet") %>% 
  set_mode("classification")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

# Predictions (class + probabilities)
predictions <- augment(fitted_model, new_data = test_filtered)

# Metric: accuracy (simple, minimal change)
Pred_Acc <- accuracy(predictions, truth = HDI_Category, estimate = .pred_class)
print(paste0("Prediction Accuracy: ", Pred_Acc))
Pred_MLL <- mn_log_loss(predictions, truth = HDI_Category, 
                        +             c(.pred_Low, .pred_Medium, 
                                        .pred_High, `.pred_Very high`))
print(paste0("Prediction Mean Log Loss : ", Pred_MLL ))
# Visualize model performance

confusion_matrix <- table(Actual = predictions$HDI_Category, 
                          Predicted = predictions$.pred_class)
print("Confusion Matrix:")
print(confusion_matrix)
# Show distribution of data by category, shortening the category names
short_cat <- gsub(" human development", "", train_data$HDI_Category)
prop.table(table(short_cat)) * 100


#################################################
