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
# 
# This code module does some refined modeling.   It looks at the relationship between the "change"
# variables which we really wanted to use, but simply can't
#
# It also measures the amount of correlation (how much we can predict a given data point by its
# immediately-previous point), and given how highn that is, the code produces a panel regression
# to try co control for that
#
# REC code
#
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

# First linear regression using the lagged HDI
model_dyn <- lm(HDI_Index ~ Lag1_HDI, data = train_data)
summary(model_dyn)
#
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

# Create the panel regression models

# This one uses 3-year change in Internet Users %

panel_model1 <- plm(
  HDI_Index ~ InternetUsersPct,
  data  = train_data,
  index = c("CountryCode","year"),
  model = "within",
  effect = "twoways"   # country FE + time FE
)
summary(panel_model1)

# This one uses 3-year change in Internet Users %

panel_model2 <- plm(
  HDI_Index ~ Cumulative3yrChg_InternetUsersPct,
  data  = train_data,
  index = c("CountryCode","year"),
  model = "within",
  effect = "twoways"   # country FE + time FE
)
summary(panel_model2)

