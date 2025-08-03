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


RawData <- readRDS(here::here("Data","PreEDA_DataFrame.rds"))
names(RawData) <- make.unique(names(RawData), sep = ".")
FieldActions <- read_excel(here::here("Data", "FieldActions.xlsx"))
CleanedData <- RawData # make a copy to preserve original
IncomeData <- read_excel(here::here("Data", "Income Group Data.xlsx")) # Import income data
stoplight <- c("#1a9641", "#ffea00", "#d7191c") # define heatmap colors

# Here are some functions we use multiple times

# Overall data missingness heatmap

OverallMissingnessHeatmap <- function(PreProcessingData) {
  # Step 1: Compute % missing per variable per year
  missing_heatmap_data <- PreProcessingData %>%
    group_by(year) %>%
    summarize(across(everything(), ~mean(is.na(.)) * 100)) %>%
    pivot_longer(-year, names_to = "variable", values_to = "pct_missing")
  
  # Step 2: Plot with stoplight colors
    stoplight <- c("#1a9641", "#ffea00", "#d7191c")
    print(ggplot(missing_heatmap_data, aes(x = year, y = variable, fill = pct_missing)) +
            geom_tile(color = "white") +
            scale_fill_gradientn(
              colors = stoplight,
              values = scales::rescale(c(0, 25, 100)),
              limits = c(0, 100),
              name = "% Missing"
            ) +
          labs(
            title = "Missing Data Heatmap",
            x = "Year",
            y = "Variable"
          ) +
          theme_minimal(base_size = 11) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank()
          ))
}

# Missingness chart by variable

MissingnessByVariable <- function(PreProcessingData) {
  PreProcessingData %>%
    summarize(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingPct") %>%
    ggplot(aes(x = reorder(Variable, MissingPct), y = MissingPct, fill = MissingPct)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradientn(
      colors = stoplight,
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),  # include 0 and 100
      labels = c("0", "25", "50", "75", "100"),
      name = "% Missing"
    ) +
    labs(
      title = "Overall Sparseness by Variable",
      x = "Variable",
      y = "% Missing"
    ) +
    theme_minimal()

}

# Missingness chart by year
MissingnessByYear <- function(PreProcessingData) {
  PreProcessingData %>%
    group_by(year) %>%
    summarize(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(-year, names_to = "variable", values_to = "pct_missing") %>%
    group_by(year) %>%
    summarize(pct_missing = mean(pct_missing)) %>%
    arrange(year) %>%
    ggplot(aes(y = factor(year), x = pct_missing, fill = pct_missing)) +
    geom_col() +
    scale_fill_gradientn(
      colors = stoplight,
      values = rescale(c(0, 50, 100)),
      name = "% Missing",
      limits = c(0, 100),
      breaks = c(0, 50, 100),
      labels = c("0", "50", "100")
    ) +
    labs(
      y = "Year",
      x = "% Missing (All Variables)",
      title = "Data Sparseness by Year"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "right"
    ) 
}

MissingnessByCountry <- function(PreProcessingData, threshold) {
  PreProcessingData %>%
    group_by(CountryCode) %>%
    summarize(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(-CountryCode, names_to = "variable", values_to = "pct_missing") %>%
    group_by(CountryCode) %>%
    summarize(pct_missing = mean(pct_missing)) %>%
    filter(pct_missing >= threshold) %>%
    arrange(pct_missing) %>%
    ggplot(aes(y = reorder(CountryCode, pct_missing), x = pct_missing, fill = pct_missing)) +
    geom_col() +
    scale_fill_gradientn(
      colors = stoplight,
      values = rescale(c(0, 50, 100)),
      name = "% Missing",
      limits = c(0, 100),
      breaks = c(0, 50, 100),
      labels = c("0", "50", "100")
    ) +
    labs(
      y = paste0("Country (Filtered > ",threshold,"%)"),
      x = "% Missing (All Variables)",
      title = "Data Sparseness by Country (High-Missingness Only)"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 6),
      legend.position = "right"
    )
}

# Read in the data produced in the EDA exercise
PreProcessingData <- readRDS(here::here("Data","FirstCutData.rds"))

# Drop GiniCoeff, IHDI_Index and FixedIntSUBS.  The first 2 we aren't 
# using in the first go-round, and the the last one we may not get to use at all (it
# shows fixed internet subscriptions, which is only part of the picture of internet use. 
# In the first models we will just use InternetUsersPct

PreProcessingData <- PreProcessingData |>
  select(-IHDI_Index, -GiniCoeff, -FixedIntSubs)

# Rename the Income Group column to remove the space - just good naming convention
PreProcessingData <- PreProcessingData |>
  rename(IncomeGroup = `Income Group`)

# Create additional variables for change in Internet access and HDI,
# as they might correlate better and/or indicative causality

PreProcessingData <- PreProcessingData |>
  arrange(CountryCode, year) |>
  group_by(CountryCode) |>
  mutate(
    # Internet access. (alternative predictor)
    YearlyChgInternet = InternetUsersPct - lag(InternetUsersPct),
    # HDI (alternative response)
    YearlyChgHDI = HDI_Index - lag(HDI_Index),
  ) |>
  ungroup()

# Create additional features for 1 and 2 year time lag, and for 3 year cumulative change in 
# internet access, the idea being that it takes time for changes in internet access that to 
# flow through to HDI

PreProcessingData <- PreProcessingData |>
  arrange(CountryCode, year) |>
  group_by(CountryCode) |>
  mutate(
    Lag1_InternetUsersPct = lag(InternetUsersPct, 1),
    Lag2_InternetUsersPct = lag(InternetUsersPct, 2),
    
    Lag1_YearlyChgInternet = lag(YearlyChgInternet, 1),
    Lag2_YearlyChgInternet = lag(YearlyChgInternet, 2),
    
    Cumulative3yrChg_InternetUsersPct = InternetUsersPct - lag(InternetUsersPct, 3)
  ) |>
  ungroup()
write_xlsx(PreProcessingData, here::here("Data","Lag Test Data.xlsx"))


# Plot missingness for all countries
MissingnessByCountry(PreProcessingData, 0)
MissingnessByCountry(PreProcessingData, 35)
# Zero in on subset > 35%

# Delete all the rows with country code starting with ZZ - virtually no data
PreProcessingData <- PreProcessingData %>%
  filter(!str_starts(CountryCode, "ZZ"))

# Re-examine missingness by variable
MissingnessByVariable(PreProcessingData)

# Delete the five features that have over 75% missing data
PreProcessingData <- PreProcessingData %>%
  select(-MortalityFromDirtyness,
         -BankingAccess,
         -MigrantsPct,
         -FoodInsecurityPct,
         -UHCServiceCoverage,
         -RnDInvest,
         -Homicides)

# Re-examine missingness by year
MissingnessByYear(PreProcessingData)

# Delete 1995-1997 (too sparse)
PreProcessingData <- PreProcessingData %>%
  filter(!(year %in% c(1995, 1996, 1997)))

# Rerun all charts.  Now the real work begins
MissingnessByVariable(PreProcessingData)
MissingnessByYear(PreProcessingData)
MissingnessByCountry(PreProcessingData, 30)
OverallMissingnessHeatmap(PreProcessingData)


# HDI Score Classifications based on UNDP classification parameters

Preprocessing_classified_data <- PreProcessingData %>%
  mutate(
    HDI_Category = case_when(
      HDI_Index >= 0.800 ~ "Very high human development",
      HDI_Index >= 0.700 &HDI_Index <= 0.799 ~ "High human development",
      HDI_Index >= 0.550 & HDI_Index <= 0.699 ~ "Medium human development",
      TRUE ~ "Low human development"
    )
  )

print("\nData after HDI classification:")
print(Preprocessing_classified_data)

# move the position of the new classified HDI data to a more relevant spot
Preprocessing_classified_data <- Preprocessing_classified_data%>%
  relocate(HDI_Category, .after = HDI_Index)

# Yeo-Johnson Transformation to handle skewness

numeric_cols <- Preprocessing_classified_data %>%
  select(where(is.numeric), -year) %>%
  names()

# Preprocessing recipe 
data_recipe <- Preprocessing_classified_data %>%
  recipe() %>%
  step_YeoJohnson(all_of(numeric_cols), na_rm = TRUE)

# Prep the data

transformed_recipe <- prep(data_recipe)

# Bake the data
TransformedData <- bake(transformed_recipe, new_data = Preprocessing_classified_data)

# Yeo-Johnson Output
print("\nEstimated lambdas for transformed variables:")
print(transformed_recipe$steps[[1]]$lambdas)

# Yeo-Johnson Histogram Results
par(mfrow=c(1, 2))
hist(Preprocessing_classified_data$InternetUsersPct, main = "Original Internet Users", xlab = "Original Values")
hist(TransformedData$InternetUsersPct, main = "Transformed Original Internet Users", xlab = "Transformed Values")
par(mfrow=c(1, 1))

sum(Preprocessing_classified_data$InternetUsersPct < 0, na.rm = TRUE)

print("\nFirst 5 rows of the Transformed Data:")
print(TransformedData %>% head())


# PCA SECTION 
# PCA Recipe
numeric_cols_for_pca <- TransformedData %>%
  select(where(is.numeric), -year) %>%
  names()


pca_recipe <- TransformedData %>%
  recipe() %>%
  step_normalize(all_of(numeric_cols_for_pca)) %>%
  step_impute_mean(all_of(numeric_cols_for_pca)) %>%
  step_pca(all_of(numeric_cols_for_pca), num_comp = 5, id = "pca")

pca_results <- prep(pca_recipe)


#PCA Data Baking
PCAData <- bake(pca_results, new_data = TransformedData)


# Visualizing the PCA Data

# Scree Plot
pca_tidy <- tidy(pca_results, id = "pca")

pca_scree_plot <- pca_tidy %>%
  filter(component %in% c("PC1", "PC2", "PC3", "PC4", "PC5")) %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "lightblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Scree Plot: Variance Explained by Principal Components",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

print(pca_scree_plot)


# PCA Loadings Results
pca_loadings_long <- tidy(pca_results, id = "pca", type = "coef")

print("\nLoadings for each principal component:")
print(pca_loadings_long)

# PCA Biplot for HDI Category

pca_biplot <- ggplot(PCAData, aes(x = PC1, y = PC2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  
  geom_point(alpha = 0.5, aes(color = HDI_Category)) +
  
  geom_segment(
    data = pca_loadings_wide,
    aes(x = 0, y = 0, xend = PC1 * 6, yend = PC2 * 6), 
    arrow = arrow(length = unit(0.3, "cm")),
    color = "gray",
    linewidth = 0.5 
  ) +
  
  geom_text(
    data = pca_loadings_wide,
    aes(x = PC1 * 6.5, y = PC2 * 6.5, label = terms), 
    color = "black",
    size = 3, 
    hjust = 0.5, vjust = 0.5
  ) +
  
  labs(
    title = "PCA Biplot of PC1 and PC2",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +

  coord_fixed(ratio = 1) +
  
  theme_minimal()

print(pca_biplot)

#PCA Bar Chart
pca_bar_plot <- pca_loadings_long %>%
  filter(component == "PC1") %>%
  ggplot(aes(x = reorder(terms, value), y = value, fill = value)) +
  geom_col() +
  scale_fill_gradient2(low = "lightblue", high = "orange", mid = "gray",
                       midpoint = 0) +
  coord_flip() +
  labs(
    title = "Variable Results for PCA",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal()

print(pca_bar_plot)

# PCA Summary
print("\nFirst 5 rows of the Data with Principal Components:")
print(PCAData %>% summary())