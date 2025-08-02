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