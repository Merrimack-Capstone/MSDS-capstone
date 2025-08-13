#########################################################################
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
