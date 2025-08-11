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

# INSERT CODE HERE TO READ THE TRAINING AND TEST DATA SETS IN FROM SAVED FILES
#
#
### Random Forest


# Create a List of columns to evaluate

model_columns <- c(
  "CountryName", "CountryCode", "year", "HDI_Index", "HDI_Category",
  "FoodIndex", "ElectricAccess", "PopDensity", "PopInSlums",
  "WaterStress", "InternetUsersPct", "GDPPerCapGrowth", "GDPGrowth",
  "GDP", "PoliticalStability", "RqdEduYears", "GovtEduSpendPctGDP",
  "UHCServiceCoverage", "RuralPopulGrowth", "VoiceAccountability",
  "YearlyChgInternet", "YearlyChgHDI",
  "IncomeGroup"
)

# Filter the training and test data to include only the specified columns
train_filtered <- train_data %>% dplyr::select(all_of(model_columns))
test_filtered <- test_data %>% dplyr::select(all_of(model_columns))

train_filtered <- na.omit(train_filtered)
test_filtered <- na.omit(test_filtered)

# Separate InternetUsersPct from the predictors
train_target <- train_filtered$InternetUsersPct
test_target <- test_filtered$InternetUsersPct

# Remove the target variable from the predictor data frames
train_predictors <- train_filtered %>% dplyr::select(-InternetUsersPct)
test_predictors <- test_filtered %>% dplyr::select(-InternetUsersPct)

# Convert all factor/character columns to a sparse matrix for XGBoost
train_matrix <- sparse.model.matrix(~ . -1, data = train_predictors)
test_matrix <- sparse.model.matrix(~ . -1, data = test_predictors)

# Train the XGBoost Model
dtrain <- xgb.DMatrix(data = train_matrix, label = train_target)
dtest <- xgb.DMatrix(data = test_matrix, label = test_target)

# Define the model parameters
params <- list(
  booster = "gbtree",      
  objective = "reg:squarederror",  
  eta = 0.1,               
  max_depth = 12,           
  subsample = 0.8,        
  colsample_bytree = 0.8  
)

# Train the XGBoost model
xgb_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = 300,           
  verbose = 1             
)

# Make predictions on the test data
predictions <- predict(xgb_model, newdata = dtest)

# Calculate the Mean Absolute Error to evaluate the model's performance
mean_absolute_error <- mean(abs(test_target - predictions))
cat("Mean Absolute Error:", mean_absolute_error, "\n")

# Visualize Feature Importance 

importance_matrix <- xgb.importance(feature_names = colnames(dtrain), model = xgb_model)

# Plot the top 10 most important features
xgb.plot.importance(importance_matrix, top_n = 10)

# Get feature importance from the model
importance_matrix <- xgb.importance(feature_names = colnames(dtrain), model = xgb_model)


# Create a data frame for plotting predictions vs. actuals
results <- data.frame(Actual = test_target, Predicted = predictions)

# Create a scatter plot of Actual vs. Predicted values
xg_scatter <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs. Predicted Values",
    x = "Actual Internet Users Percentage",
    y = "Predicted Internet Users Percentage"
  ) +
  theme_minimal()

print(xg_scatter)

# Create a residual plot
results$Residuals <- results$Actual - results$Predicted
xg_resid <- ggplot(results, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Residuals Distribution",
    x = "Residuals (Actual - Predicted)",
    y = "Count"
  ) +
  theme_minimal()

print(xg_resid)


# Random Forest PCA

# Apply the PCA transformation to the training and test data using the prepared recipe.
numeric_cols_for_pca <- train_data %>%
  select(where(is.numeric)) %>%
  names()

# Build the PCA recipe on the training data.
pca_recipe <- train_data %>%
  recipe() %>%
  step_normalize(all_of(numeric_cols_for_pca)) %>%
  step_impute_mean(all_of(numeric_cols_for_pca)) %>%
  step_pca(all_of(numeric_cols_for_pca), num_comp = 5, id = "pca")

# Prepare the recipe to create the PCA results object.
pca_results <- prep(pca_recipe)


# Data Preparation
PCAData_train <- bake(pca_results, new_data = train_data)
PCAData_test <- bake(pca_results, new_data = test_data)

# Add the target variable 'InternetUsersPct' to the PCA data frames.
PCAData_train <- cbind(PCAData_train, InternetUsersPct = train_data$InternetUsersPct)
PCAData_test <- cbind(PCAData_test, InternetUsersPct = test_data$InternetUsersPct)

# Remove any remaining rows with missing values from both data frames.
PCAData_train_clean <- na.omit(PCAData_train)
PCAData_test_clean <- na.omit(PCAData_test)

# Train a Random Forest model using the PCA results as predictors

rf_model_pca <- randomForest(
  formula = InternetUsersPct ~ PC1 + PC2 + PC3,
  data = PCAData_train_clean,
  ntree = 500,
  importance = TRUE
)

# Print the model summary
print(rf_model_pca)

# Make predictions
rf_pca_predictions <- predict(rf_model_pca, newdata = PCAData_test_clean)

# Calculate the Mean Absolute Error for evaluation.
mean_absolute_error <- mean(abs(PCAData_test_clean$InternetUsersPct - rf_pca_predictions))
cat("Mean Absolute Error:", mean_absolute_error, "\n")


# Feature Importance Plot
varImpPlot(rf_model_pca, 
           main = "Feature Importance for Random Forest (PCA)")


# Actual vs. Predicted Scatter Plot

results <- data.frame(Actual = PCAData_test_clean$InternetUsersPct, Predicted =rf_pcapredictions)

rf_pca_scatter <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs. Predicted Values (Random Forest on PCA) - Test Data",
    x = "Actual Internet Users Percentage",
    y = "Predicted Internet Users Percentage"
  ) +
  theme_minimal()

print(rf_pca_scatter)


# Residuals Distribution Plot
results$Residuals <- results$Actual - results$Predicted

rf_pca_resid <- ggplot(results, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Residuals Distribution (Random Forest on PCA) - Test Data",
    x = "Residuals (Actual - Predicted)",
    y = "Count"
  ) +
  theme_minimal()

print(rf_pca_resid)


