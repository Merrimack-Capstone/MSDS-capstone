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

print_regressions <- function(predictions, change = FALSE){
  chart_title <- "Actual vs Predicted - "
  if (change) {
    results_lm <- predictions %>%
      transmute(Actual = YearlyChgHDI, Predicted = .pred)
    chart_title <- paste0(chart_title,"Y-o-Y Change in HDI Index")
    low_bound <- -0.12
    high_bound <- 0.12
  } else {
    results_lm <- predictions %>%
      transmute(Actual = HDI_Index, Predicted = .pred)
    chart_title <- paste0(chart_title,"HDI Index")
    low_bound <- 0.3
    high_bound <- 1
  }

  p_scatter <- ggplot(results_lm, aes(x = Actual, y = Predicted)) +
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
  results_lm$Residuals <- results_lm$Actual - results_lm$Predicted
  p_resid <- ggplot(results_lm, aes(x = Residuals)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Residuals Distribution",
      x = "Residual (Actual - Predicted)",
      y = "Count"
    ) +
    theme_minimal()
  print(p_resid)
}


# Read in the saved training and test data sets
train_data <- readRDS(here::here("Data", "train_data.rds"))
test_data <- readRDS(here::here("Data", "test_data.rds"))

# Start with basic regression

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

metrics <- metric_set(rmse, mae, rsq)
metrics(predictions, truth = HDI_Index, estimate = .pred)

# (optional) coefficients + summary
tidy(extract_fit_parsnip(fitted_model))
glance(extract_fit_parsnip(fitted_model))

print_regressions(predictions)

# Let's try a quadratic regression, see if that's better:
set.seed(123)

myrecipe <- recipe(HDI_Index ~ InternetUsersPct, data = train_filtered) %>%
  step_poly(InternetUsersPct, degree=2)

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)

metrics <- metric_set(rmse, mae, rsq)
metrics(predictions, truth = HDI_Index, estimate = .pred)

# (optional) coefficients + summary
tidy(extract_fit_parsnip(fitted_model))
glance(extract_fit_parsnip(fitted_model))

print_regressions(predictions)

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
accuracy(predictions, truth = HDI_Category, estimate = .pred_class)

# Visualize model performance

confusion_matrix <- table(Actual = predictions$HDI_Category, 
                          Predicted = predictions$.pred_class)
print("Confusion Matrix:")
print(confusion_matrix)
# Show distribution of data by category, shortening the category names
short_cat <- gsub(" human development", "", train_data$HDI_Category)
prop.table(table(short_cat)) * 100


#################################################
#
# Does using "change" variables help?
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

metrics <- metric_set(rmse, mae, rsq)
metrics(predictions, truth = YearlyChgHDI, estimate = .pred)

# (optional) coefficients + summary
tidy(extract_fit_parsnip(fitted_model))
glance(extract_fit_parsnip(fitted_model))

print_regressions(predictions,change = TRUE)

library(ggplot2)

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
# How about incorporating a 3 year lag
#
model_columns <- c(
  "HDI_Index", "Cumulative3yrChg_InternetUsersPct", "Cumulative3yrChg_HDI"
)

# Filter the training and test data to include only the specified columns

train_filtered <- train_data %>% dplyr::select(all_of(model_columns))
test_filtered <- test_data %>% dplyr::select(all_of(model_columns))

# Use na.omit() to remove rows with any NA values 

train_filtered <- na.omit(train_filtered)
test_filtered <- na.omit(test_filtered)

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

metrics <- metric_set(rmse, mae, rsq)
print(metrics(predictions, truth = HDI_Index, estimate = .pred))

# (optional) coefficients + summary
tidy(extract_fit_parsnip(fitted_model))
glance(extract_fit_parsnip(fitted_model))

print_regressions(predictions,change = FALSE)

ggplot(train_filtered, aes(x = Cumulative3yrChg_InternetUsersPct, y = HDI_Index)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    title = "3 Year Change in nternet Access vs HDI",
    x = "3 Year Change in Internet Access (%)",
    y = "HDI Index"
  ) +
  theme_minimal()

set.seed(123)

myrecipe <- recipe(Cumulative3yrChg_HDI ~ Cumulative3yrChg_InternetUsersPct, data = train_filtered)

mymodel <- linear_reg() %>% 
  set_engine("lm")

myworkflow <- workflow() %>% 
  add_recipe(myrecipe) %>% 
  add_model(mymodel)

fitted_model <- fit(myworkflow, data = train_filtered)

predictions <- predict(fitted_model, new_data = test_filtered) %>% 
  bind_cols(test_filtered)

metrics <- metric_set(rmse, mae, rsq)
print(metrics(predictions, truth = Cumulative3yrChg_HDI, estimate = .pred))

# (optional) coefficients + summary
tidy(extract_fit_parsnip(fitted_model))
glance(extract_fit_parsnip(fitted_model))

print_regressions(predictions,change = FALSE)

ggplot(train_filtered, aes(x = Cumulative3yrChg_InternetUsersPct, y = Cumulative3yrChg_HDI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    title = "3 Year Change: Internet Access vs HDI",
    x = "3 Year Change in Internet Access (%)",
    y = "3 Year Change in HDI Index"
  ) +
  theme_minimal()