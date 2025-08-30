# Import Libraries 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(recipes)
library(here)
library(parsnip)
library(tidymodels)
library(ggcorrplot)
library(plm)


PreProcessingData <- readRDS(here::here("Data","FirstCutData.rds")) # Get Data
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

#
# Checking distribution of percent change to see why it was so funky in model
#
hist(train_data$YearlyChgInternet,
     main = "Distribution of Yearly Change in Internet",
     xlab = "YearlyChgInternet",
     col = "lightblue",
     border = "black")

plot(train_data$YearlyChgInternet,
     train_data$YearlyChgHDI,
     main = "Scatterplot of YearlyChgInternet vs YearlyChgHDI",
     xlab = "Yearly Change in Internet",
     ylab = "Yearly Change in HDI",
     col = "blue", pch = 19)
summary(train_data$YearlyChgInternet)
summary(train_data$YearlyChgHDI)

cols <- c("HDI_Index", "YearlyChgHDI","InternetUsersPct", 
          "YearlyChgInternet","Lag1_InternetUsersPct",
          "Lag2_InternetUsersPct", "Lag1_YearlyChgInternet",
          "Lag2_YearlyChgInternet","Cumulative3yrChg_InternetUsersPct",
          "Cumulative3yrChg_HDI")

#
# The correlation heatmap shows that there's really no correlation between the
# change in internet access and the change in HDI.   So trying to relate them 
# directly using linear OLS is a dead-end.  So far, it's still correlation rather
# than causation
df_sub <- train_data[, cols, drop = FALSE]
cor_matrix <- cor(df_sub, use = "pairwise.complete.obs")      # or method = "spearman"
ord <- order(cor_matrix[,"HDI_Index"], decreasing = TRUE)
ggcorrplot(cor_matrix[ord, ord], hc.order = FALSE, lab = TRUE, lab_size = 2.8,
           ggtheme = ggplot2::theme_minimal())

# Just how much is the data auto-correlated?  We will add lagged HDI_Index as a
# predictor to see

train_data <- train_data %>%
  group_by(CountryCode) %>%
  arrange(year) %>%
  mutate(Lag1_HDI = dplyr::lag(HDI_Index, 1)) %>%
  ungroup()

model_dyn <- lm(HDI_Index ~ Lag1_HDI + InternetUsersPct, data = train_data)
summary(model_dyn)

model_dyn_chg <- lm(
  HDI_Index ~ Lag1_HDI + Cumulative3yrChg_InternetUsersPct,
  data = train_data
)

summary(model_dyn_chg)

rec_chg <- recipe(
  HDI_Index ~ Cumulative3yrChg_InternetUsersPct + CountryCode + year,
  data = train_data
) %>%
  step_mutate(year = factor(year)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

wf_chg <- workflow() %>%
  add_model(mod_lm) %>%
  add_recipe(rec_chg)

fit_chg <- fit(wf_chg, data = train_data)

# Predict on test
test_pred_chg <- predict(fit_chg, new_data = test_data) %>%
  bind_cols(test_data %>% select(HDI_Index))

metrics(test_pred_chg, truth = HDI_Index, estimate = .pred)
#
# The net result?  99.9% of HDI variance can be explained using the previous year's
# HDI index, suggesting a VERY high degree of autocorrelation.  The internet coefficient
# is essentially zero.  So again, no help in isolate the impact or causality of internet
# access on HDI
#
# Model 4q
#
# Given the autocorrelation, here we run a panel regression, controlling for country and year, to deal with the
# autocorrelation within countries and across years, and attempt to isolate the impact 
# of changes in internet access

model_columns <- c(
  "CountryCode", "HDI_Category", "FoodIndex", "ElectricAccess", "PopDensity", 
  "PopInSlums", "WaterStress", "InternetUsersPct", "GDPPerCapGrowth", "GDPGrowth",
  "GDP", "PoliticalStability", "RqdEduYears", "GovtEduSpendPctGDP",
  "UHCServiceCoverage", "RuralPopulGrowth", "VoiceAccountability"
)

# Filter the training and test data to include only the specified columns

train_filtered <- train_data %>% dplyr::select(all_of(model_columns))
test_filtered <- test_data %>% dplyr::select(all_of(model_columns))

# Create the panel regression model

## 3-YEAR CHANGE spec
fe_tw <- plm(
  HDI_Index ~ Cumulative3yrChg_InternetUsersPct,
  data  = train_data,
  index = c("CountryCode","year"),
  model = "within",
  effect = "twoways"   # country FE + time FE
)

summary(fe_tw)

