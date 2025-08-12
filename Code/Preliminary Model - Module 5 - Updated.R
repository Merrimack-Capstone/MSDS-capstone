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

# Read in the saved training and test data sets
train_data <- readRDS(here::here("Data", "train_data.rds"))
test_data <- readRDS(here::here("Data", "test_data.rds"))
PCAData <- readRDS(here::here("Data", "PCAData.rds"))
importance_matrix <- readRDS(here::here("Data", "xgboost_importance_matrix.rds"))

model_columns <- c(
  "CountryName", "CountryCode", "year", "HDI_Index", "HDI_Category",
  "FoodIndex", "ElectricAccess", "PopDensity", "PopInSlums",
  "WaterStress", "InternetUsersPct", "GDPPerCapGrowth", "GDPGrowth",
  "GDP", "PoliticalStability", "RqdEduYears", "GovtEduSpendPctGDP",
  "UHCServiceCoverage", "RuralPopulGrowth", "VoiceAccountability",
  "YearlyChgInternet", "YearlyChgHDI", "IncomeGroup"
)

# Filter the training and test data to include only the specified columns

train_filtered <- train_data %>% dplyr::select(all_of(model_columns))
test_filtered <- test_data %>% dplyr::select(all_of(model_columns))

# Use na.omit() to remove rows with any NA values 

train_filtered <- na.omit(train_filtered)
test_filtered <- na.omit(test_filtered)

train_target <- as.integer(as.factor(train_filtered$HDI_Category)) - 1
test_target <- as.integer(as.factor(test_filtered$HDI_Category)) - 1
num_class <- length(unique(train_target))

# Remove the target variable from the predictor data frames
train_predictors <- train_filtered %>% dplyr::select(-HDI_Category)
test_predictors <- test_filtered %>% dplyr::select(-HDI_Category)

# Combine data to ensure consistent columns 

# For classification, the target variable must be a numeric integer 
train_target <- as.integer(as.factor(train_filtered$HDI_Category)) - 1
test_target <- as.integer(as.factor(test_filtered$HDI_Category)) - 1
num_class <- length(unique(train_target))

# Remove the target variable from the predictor data frames

train_predictors <- train_filtered %>% dplyr::select(-HDI_Category)
test_predictors <- test_filtered %>% dplyr::select(-HDI_Category)


# Combine the predictor data frames to create a sparse matrix that has the same
# columns and column order for both training and test data.

combined_predictors <- bind_rows(train_predictors, test_predictors)

# Convert all factor/character columns to a sparse matrix for XGBoost

combined_matrix <- sparse.model.matrix(~ . -1, data = combined_predictors)

# Split the combined matrix back into training and test matrices

train_matrix <- combined_matrix[1:nrow(train_predictors),]
test_matrix <- combined_matrix[(nrow(train_predictors)+1):nrow(combined_matrix),]

# Train XGBoost Model with hyperparameter Tuning and cross-validation 

dtrain <- xgb.DMatrix(data = train_matrix, label = train_target)
dtest <- xgb.DMatrix(data = test_matrix, label = test_target)

# Define a grid of hyperparameters to search over

hyper_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(6, 12, 18),
  nrounds = c(100, 300),
  subsample = c(0.6, 0.8, 1.0) 
)

# Create an empty list to store the results

results_list <- list()

# Iterate over each combination of hyperparameters

for (i in 1:nrow(hyper_grid)) {
  current_eta <- hyper_grid$eta[i]
  current_max_depth <- hyper_grid$max_depth[i]
  current_nrounds <- hyper_grid$nrounds[i]
  current_subsample <- hyper_grid$subsample[i]
  
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = num_class,
    eta = current_eta,
    max_depth = current_max_depth,
    subsample = current_subsample,
    colsample_bytree = 0.8
  )
  
# Use xgb.cv for hyperparameter tuning
  
  cv_model <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = current_nrounds,
    nfold = 5,
    metrics = "mlogloss", 
    early_stopping_rounds = 10,
    verbose = 0
  )
  
# Get the best cross-validation accuracy
  
  best_iteration <- cv_model$best_iteration
  cv_accuracy <- cv_model$evaluation_log[best_iteration,]$test_mlogloss_mean
  
# Store the results
  
  results_list[[i]] <- list(
    eta = current_eta,
    max_depth = current_max_depth,
    nrounds = current_nrounds,
    subsample = current_subsample,
    accuracy = cv_accuracy
  )
  
  cat(paste0("Settings: eta=", current_eta, ", max_depth=", current_max_depth,
             ", nrounds=", current_nrounds, ", subsample=", current_subsample,
             " | CV Logloss=", round(cv_accuracy, 4), " (Best Iteration: ", best_iteration, ")\n"))
}

# Summarize results
results_df <- do.call(rbind, lapply(results_list, as.data.frame))
best_model_settings <- results_df[which.min(results_df$accuracy),]

cat("\n--- Best Model Settings (Based on CV Logloss) ---\n")
print(best_model_settings)
cat("--------------------------\n")

# Final Model with best settings
params_final <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  num_class = num_class,
  eta = best_model_settings$eta,
  max_depth = best_model_settings$max_depth,
  subsample = best_model_settings$subsample,
  colsample_bytree = 0.8
)

final_xgb_model <- xgboost(
  params = params_final,
  data = train_matrix,
  label = train_target,
  nrounds = best_model_settings$nrounds,
  verbose = 1
)

# Make predictions on the test data with the final model

predictions_final <- predict(final_xgb_model, newdata = test_matrix)
predictions_class_final <- matrix(predictions_final, ncol = num_class, byrow = TRUE) %>%
  apply(1, which.max) - 1

# Calculate the Accuracy

final_accuracy <- sum(predictions_class_final == test_target) / length(test_target)
cat("\nFinal XGBoost Accuracy on Test Data:", final_accuracy, "\n")

# Visualize model performance

confusion_matrix <- table(Actual = test_target, Predicted = predictions_class_final)
print("Confusion Matrix:")
print(confusion_matrix)

importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = final_xgb_model)
xgb.plot.importance(importance_matrix, top_n = 10)

cat("\nSummary of Feature Importance Matrix:\n")
print(importance_matrix)

saveRDS(importance_matrix, file = "xgboost_importance_matrix.rds")

# XGBoost InternetUserPct Regression


# Create a List of columns to evaluate

model_columns2 <- c(
  "CountryName", "CountryCode", "year", "HDI_Index", "HDI_Category",
  "FoodIndex", "ElectricAccess", "PopDensity", "PopInSlums",
  "WaterStress", "InternetUsersPct", "GDPPerCapGrowth", "GDPGrowth",
  "GDP", "PoliticalStability", "RqdEduYears", "GovtEduSpendPctGDP",
  "UHCServiceCoverage", "RuralPopulGrowth", "VoiceAccountability",
  "YearlyChgInternet", "YearlyChgHDI",
  "IncomeGroup"
)

# Filter the training and test data to include only the specified columns
train_filtered2 <- train_data %>% dplyr::select(all_of(model_columns2))
test_filtered2 <- test_data %>% dplyr::select(all_of(model_columns2))

# Use na.omit() to remove rows with any NA values 

train_filtered2 <- na.omit(train_filtered2)
test_filtered2 <- na.omit(test_filtered2)

train_target2 <- as.integer(as.factor(train_filtered2$InternetUsersPct)) - 1
test_target2 <- as.integer(as.factor(test_filtered2$InternetUsersPct)) - 1
num_class2 <- length(unique(train_target2))

# Remove the target variable from the predictor data frames
train_predictors2 <- train_filtered2 %>% dplyr::select(-InternetUsersPct)
test_predictors2 <- test_filtered2 %>% dplyr::select(-InternetUsersPct)

# Combine data to ensure consistent columns 

# Remove the target variable from the predictor data frames

train_predictors2 <- train_filtered2 %>% dplyr::select(-InternetUsersPct)
test_predictors2 <- test_filtered2 %>% dplyr::select(-InternetUsersPct)


# Combine the predictor data frames to create a sparse matrix that has the same
# columns and column order for both training and test data.

combined_predictors2 <- bind_rows(train_predictors2, test_predictors2)

# Convert all factor/character columns to a sparse matrix for XGBoost

combined_matrix2 <- sparse.model.matrix(~ . -1, data = combined_predictors2)

# Split the combined matrix back into training and test matrices

train_matrix2 <- combined_matrix2[1:nrow(train_predictors2),]
test_matrix2 <- combined_matrix2[(nrow(train_predictors2)+1):nrow(combined_matrix2),]

# Train XGBoost Model with hyperparameter Tuning and cross-validation 

dtrain2 <- xgb.DMatrix(data = train_matrix2, label = train_target2)
dtest2 <- xgb.DMatrix(data = test_matrix2, label = test_target2)

# Define the model parameters
params2 <- list(
  booster = "gbtree",      
  objective = "reg:squarederror",  
  eta = 0.1,               
  max_depth = 12,           
  subsample = 0.8,        
  colsample_bytree = 0.8  
)

# Train the XGBoost model
xgb_model2 <- xgboost(
  params = params2,
  data = dtrain2,
  nrounds = 300,           
  verbose = 1             
)

# Make predictions on the test data
predictions2 <- predict(xgb_model2, newdata = dtest2)

# Calculate the Mean Absolute Error to evaluate the model's performance
mean_absolute_error2 <- mean(abs(test_target2 - predictions2))
cat("Mean Absolute Error:", mean_absolute_error2, "\n")

# Visualize Feature Importance 

importance_matrix2 <- xgb.importance(feature_names = colnames(dtrain2), model = xgb_model2)

# Plot the top 10 most important features
xgb.plot.importance(importance_matrix2, top_n = 10)

# Get feature importance from the model
importance_matrix2 <- xgb.importance(feature_names = colnames(dtrain2), model = xgb_model2)


# Create a data frame for plotting predictions vs. actuals
results2 <- data.frame(Actual = test_target2, Predicted = predictions2)

# Create a scatter plot of Actual vs. Predicted values
xg_scatter2 <- ggplot(results2, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs. Predicted Values",
    x = "Actual Internet Users Percentage",
    y = "Predicted Internet Users Percentage"
  ) +
  theme_minimal()

print(xg_scatter2)

# Create a residual plot
results2$Residuals <- results2$Actual - results2$Predicted
xg_resid2 <- ggplot(results2, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Residuals Distribution",
    x = "Residuals (Actual - Predicted)",
    y = "Count"
  ) +
  theme_minimal()

print(xg_resid2)

# Random Forest PCA

# Add the 'InternetUsersPct' column if it's not already in the PCAData frame.

if (!"InternetUsersPct" %in% colnames(PCAData)) {
  if (exists("train_data") && "InternetUsersPct" %in% colnames(train_data)) {
    PCAData <- cbind(PCAData, InternetUsersPct = train_data$InternetUsersPct)
    cat("Note: Added 'InternetUsersPct' from 'train_data'.\n")
  } else {
    stop("The 'InternetUsersPct' column is missing and 'train_data' is not available to retrieve it.")
  }
}

# Remove any remaining rows with missing values from the PCA data frame.

PCAData_clean <- na.omit(PCAData)



# Train a Random Forest model using the PCA results as predictors
# for the InternetUsersPct variable on the CLEAN data.

rf_model_pca <- randomForest(
  formula = InternetUsersPct ~ PC1 + PC2 + PC3 + PC4 + PC5,
  data = PCAData_clean,
  ntree = 250,
  importance = TRUE
)

# Print the model summary

print(rf_model_pca)

# Make predictions on the CLEAN data.

predictions <- predict(rf_model_pca, newdata = PCAData_clean)

# Calculate the Mean Absolute Error for evaluation.

mean_absolute_error <- mean(abs(PCAData_clean$InternetUsersPct - predictions))
cat("Random Forest PCA Mean Absolute Error:", mean_absolute_error, "\n")



# Feature Importance Plot

varImpPlot(rf_model_pca,
           main = "Feature Importance for Random Forest (PCA)")


# Actual vs. Predicted Scatter Plot

results <- data.frame(Actual = PCAData_clean$InternetUsersPct, Predicted = predictions)

pca_rf_scatter <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs. Predicted Values (Random Forest on PCA)",
    x = "Actual Internet Users Percentage",
    y = "Predicted Internet Users Percentage"
  ) +
  theme_minimal()

print(pca_rf_scatter)


# Residuals Distribution Plot

results$Residuals <- results$Actual - results$Predicted

pca_resid_plot <- ggplot(results, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Residuals Distribution (Random Forest on PCA)",
    x = "Residuals (Actual - Predicted)",
    y = "Count"
  ) +
  theme_minimal()

print(pca_resid_plot)

# Create a Feature Importance Table

importance_df <- as.data.frame(randomForest::importance(rf_model_pca))
importance_df$Feature <- rownames(importance_df)
importance_df <- importance_df[order(importance_df$IncNodePurity, decreasing = TRUE), ]

print("Feature Importance Table:")
print(importance_df)

# Create a Performance Metrics Table 

metrics <- data.frame(
  Metric = c("Mean Absolute Error (MAE)", "Root Mean Squared Error (RMSE)", "R-squared (R^2)"),
  Value = c(
    mean_absolute_error,
    sqrt(mean(results$Residuals^2)),
    cor(results$Actual, results$Predicted)^2
  )
)

print("Model Performance Metrics:")
print(metrics)
