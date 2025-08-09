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

PreProcessingData <- readRDS(here::here("Data","FirstCutData.rds")) # Get Dat
stoplight <- c("#1a9641", "#ffea00", "#d7191c") # define heatmap colors

##########################################################################

# FUNCTIONS

# Overall data missingness heatmap
OneCountryMissingness <- function(data, code) {
  
  data <- data %>%
    select(-Lag1_InternetUsersPct,
           -Lag2_InternetUsersPct,
           -Lag1_YearlyChgInternet,
           -Lag2_YearlyChgInternet,
           -Cumulative3yrChg_InternetUsersPct,
           -YearlyChgHDI,
           -YearlyChgInternet)
  
  missing_heatmap_data <- data %>%
    filter(CountryCode == code) %>%
    select(-CountryCode, -CountryName, -IncomeGroup, -HDI_Category) %>%
    pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
    mutate(Status = ifelse(is.na(value), "Missing", "Present"))
  
  # Order variables by overall missingness (most missing at top)
  var_order <- missing_heatmap_data %>%
    group_by(variable) %>%
    summarize(frac_missing = mean(Status == "Missing"), .groups = "drop") %>%
    arrange(dplyr::desc(frac_missing)) %>%
    pull(variable)
  
  missing_heatmap_data <- missing_heatmap_data %>%
    mutate(variable = factor(variable, levels = var_order))
  
  # 2) Plot: white = Present, red = Missing
  print(
    ggplot(missing_heatmap_data, aes(x = year, y = variable, fill = Status)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("Present" = "white", "Missing" = "red"), name = NULL) +
      labs(
        title = paste0("Data Availability Heatmap: ", code),
        x = "year",
        y = "variable"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  )
}

OverallMissingnessHeatmap <- function(data) {
  data <- data %>%
    select(-Lag1_InternetUsersPct,
           -Lag2_InternetUsersPct,
           -Lag1_YearlyChgInternet,
           -Lag2_YearlyChgInternet,
           -Cumulative3yrChg_InternetUsersPct,
           -YearlyChgHDI,
           -YearlyChgInternet)
  # Step 1: Compute % missing per variable per year
  missing_heatmap_data <- data %>%
    group_by(year) %>%
    summarize(across(everything(), ~mean(is.na(.)) * 100)) %>%
    pivot_longer(-year, names_to = "variable", values_to = "pct_missing")
  
  # Step 2: Plot with stoplight colors
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

MissingnessByVariable <- function(data) {
  PlotData <- data %>%
    select(-Lag1_InternetUsersPct,
           -Lag2_InternetUsersPct,
           -Lag1_YearlyChgInternet,
           -Lag2_YearlyChgInternet,
           -Cumulative3yrChg_InternetUsersPct,
           -YearlyChgHDI,
           -YearlyChgInternet)
  VariableMissing <- PlotData %>%
    summarize(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "MissingPct")
  print(ggplot(data = VariableMissing,
               aes(x = reorder(Variable, MissingPct), 
                   y = MissingPct, fill = MissingPct)) +
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
    theme_minimal())
}

# Missingness chart by year
MissingnessByYear <- function(data) {
  PlotData <- data %>%
    select(-Lag1_InternetUsersPct,
           -Lag2_InternetUsersPct,
           -Lag1_YearlyChgInternet,
           -Lag2_YearlyChgInternet,
           -Cumulative3yrChg_InternetUsersPct,
           -YearlyChgHDI,
           -YearlyChgInternet)
  
  MissingYear <- PlotData %>%
    group_by(year) %>%
    summarize(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(-year, names_to = "variable", values_to = "pct_missing") %>%
    group_by(year) %>%
    summarize(pct_missing = mean(pct_missing)) %>%
    arrange(year)
  print(ggplot(data = MissingYear,
               aes(y = factor(year), 
                   x = pct_missing, fill = pct_missing)) + 
          geom_col() + 
          scale_fill_gradientn(colors = stoplight,
                               values = rescale(c(0, 50, 100)),
                               name = "% Missing",
                               limits = c(0, 100),
                               breaks = c(0, 50, 100),
                               labels = c("0", "50", "100")) +
          labs(y = "Year",
               x = "% Missing (All Variables)",
               title = "Data Sparseness by Year") +
          theme_minimal() +
          theme(axis.text.y = element_text(size = 8),
                legend.position = "right"
    )) 
}

MissingnessByCountry <- function(data, threshold) {
  PlotData <- data %>%
    select(-Lag1_InternetUsersPct,
           -Lag2_InternetUsersPct,
           -Lag1_YearlyChgInternet,
           -Lag2_YearlyChgInternet,
           -Cumulative3yrChg_InternetUsersPct,
           -YearlyChgHDI,
           -YearlyChgInternet)
  CountryMissing <- PlotData %>%
    group_by(CountryCode) %>%
    summarize(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(-CountryCode, names_to = "variable", values_to = "pct_missing") %>%
    group_by(CountryCode) %>%
    summarize(pct_missing = mean(pct_missing)) %>%
    filter(pct_missing >= threshold) %>%
    arrange(pct_missing) 
  print(ggplot(data = CountryMissing, 
               aes(y = reorder(CountryCode, pct_missing), 
                   x = pct_missing, fill = pct_missing)) +
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
    ))
}
# This function just reproduces our four missingness plots
FourPlots <- function(data, cutoff) {
  MissingnessByVariable(data)
  MissingnessByYear(data)
  MissingnessByCountry(data, cutoff)
  OverallMissingnessHeatmap(data)
}

# This function imputes missing data using time trend fitting, as follows:
# The variable name is passed as an argument so the function can work with 
# multiple variables.  A floor and ceiling value are also passed
#
# For the missing data for each CountryCode group:
#
# If there are 3 ore more valid data points, impute the rest quadratically
# If there are 2 valid data points, impute the rest quadratically
# If there is 1 valid data point, carry it throughout the rest of the observations
# If there are none, leave it as NA
# Read in the data produced in the EDA exercise

impute_time_trend <- function(data, var_name, floor, cap) {
  var_sym <- rlang::ensym(var_name)
  
  data %>%
    group_by(CountryName) %>%
    group_modify(~ {
      df_country <- .x
      y <- df_country[[as_name(var_sym)]]
      known <- df_country %>% filter(!is.na(y))
      
      if (nrow(known) >= 3) {
        model <- lm(y ~ poly(year, 2, raw = TRUE), data = df_country)
        preds <- predict(model, newdata = df_country)
      } else if (nrow(known) == 2) {
        model <- lm(y ~ year, data = df_country)
        preds <- predict(model, newdata = df_country)
      } else if (nrow(known) == 1) {
        preds <- rep(known[[rlang::as_name(var_sym)]][1], nrow(df_country))
      } else {
        preds <- rep(NA_real_, nrow(df_country))
      }
      
      missing_idx <- is.na(y) & !is.na(preds)
      preds_clipped <- pmin(cap, pmax(floor, preds[missing_idx]))
      df_country[[rlang::as_name(var_sym)]][missing_idx] <- preds_clipped
      df_country
    }) %>%
    ungroup()
}

###################################################################
#
# FEATURE ENGINEERING

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

# Show missingness heatmap
OverallMissingnessHeatmap(PreProcessingData)

# Plot missingness for all countries
MissingnessByCountry(PreProcessingData, 0)
MissingnessByCountry(PreProcessingData, 35)
# Zero in on subset > 35%

# Delete all the rows with country code starting with ZZ - virtually no data
PreProcessingData <- PreProcessingData %>%
  filter(!str_starts(CountryCode, "ZZ"))

# Show missingness heatmap
OverallMissingnessHeatmap(PreProcessingData)

# Re-examine missingness by variable
MissingnessByVariable(PreProcessingData)

# Delete the six features that have over 75% missing data with no
# pattern to the missingness

PreProcessingData <- PreProcessingData %>%
  select(-MortalityFromDirtyness,
         -BankingAccess,
         -MigrantsPct,
         -FoodInsecurityPct,
         -RnDInvest,
         -ShippingIndex,
         -Homicides)

# Show missingness heatmap
OverallMissingnessHeatmap(PreProcessingData)

# Re-examine missingness by year
MissingnessByYear(PreProcessingData)

# Delete 1995-1997 (too sparse)
PreProcessingData <- PreProcessingData %>%
  filter(!(year %in% c(1995, 1996, 1997, 1998, 1999, 2023)))

# Set up HDI Bins
PreProcessingData <- PreProcessingData %>%
  mutate(
    HDI_Category = case_when(
      HDI_Index >= 0.800 ~ "Very high human development",
      HDI_Index >= 0.700 &HDI_Index <= 0.799 ~ "High human development",
      HDI_Index >= 0.550 & HDI_Index <= 0.699 ~ "Medium human development",
      TRUE ~ "Low human development"
    )
  )

# move the position of the new classified HDI data to a more relevant spot
PreProcessingData <- PreProcessingData%>%
  relocate(HDI_Category, .after = HDI_Index)

print("\nData after HDI classification:")
print(PreProcessingData)

######################################################
#
# SKEWNESS HANDLING
#
# Yeo-Johnson Transformation to handle skewness

affected_cols <- PreProcessingData %>%
  select(InternetUsersPct, 
         Lag1_InternetUsersPct, 
         Lag2_InternetUsersPct,
         YearlyChgInternet, 
         Lag1_YearlyChgInternet,
         Lag2_YearlyChgInternet) %>%
  names()

# Preprocessing recipe 
data_recipe <- PreProcessingData %>%
  recipe() %>%
  step_YeoJohnson(all_of(affected_cols), na_rm = TRUE)

# Prep the data

transformed_recipe <- prep(data_recipe)

# Bake the data
transformed_data <- bake(transformed_recipe, new_data = PreProcessingData)

# Yeo-Johnson Output
print("\nEstimated lambdas for transformed variables:")
print(transformed_recipe$steps[[1]]$lambdas)

# Yeo-Johnson Histogram Results
par(mfrow=c(1, 2))
hist(PreProcessingData$InternetUsersPct, main = "Original Internet Users", 
     xlab = "Original Values")
hist(transformed_data$InternetUsersPct, main = "Transformed Original Internet Users", 
     xlab = "Transformed Values")
par(mfrow=c(1, 1))

sum(PreProcessingData$InternetUsersPct < 0, na.rm = TRUE)

print("\nFirst 5 rows of the Transformed Data:")
print(transformed_data %>% head())
############################################################################

# Combined Governance Score 
PreProcessingData <- PreProcessingData %>%
  rowwise() %>%
  mutate(
    GovernanceScore = mean(c(GovtEffectiveness, RuleOfLaw, CorruptionScore), 
                           na.rm = TRUE)
  )

PreProcessingData <- PreProcessingData %>%
  select(-GovtEffectiveness,
         -RuleOfLaw,
         -CorruptionScore
  )
print("\nData with the new 'Governance_Score' column:")
print(PreProcessingData)

# Combined Health Spend to address collinearity
PreProcessingData <- PreProcessingData %>%
  rowwise() %>%
  mutate(
    HealthSpend = mean(c(HealthSpendPerCapita, GovtHealthSpendPerCapita), 
                       na.rm = TRUE)
  )

PreProcessingData <- PreProcessingData %>%
  select(-HealthSpendPerCapita,
         -GovtHealthSpendPerCapita,
  )
print("\nData with the new 'HealthSpend' column:")
print(PreProcessingData)


# IMPUTING MISSING DATA
#
# First split data into training and test data
# Define the training data
n <- nrow(transformed_data)
split_point <- floor(n * 0.7) 

# Correct way to subset rows
train_data <- transformed_data[1:split_point, ] 
test_data <- transformed_data[(split_point + 1):n, ]

# Verify the dimensions to confirm the split
dim(train_data)
dim(test_data)


# Rerun all charts.  Now the real work begins
FourPlots(train_data,30)

# This function plots average UCHCServiceCoverage by year 

PlotUHC <- function(data){
 data %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    summarize(
      avg_uhc = mean(UHCServiceCoverage, na.rm = TRUE),
      n_countries = sum(!is.na(UHCServiceCoverage))
    ) %>%
    filter(!is.na(avg_uhc)) %>%
    arrange(year) %>%
    ggplot(aes(x = year, y = avg_uhc)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = n_countries), color = "steelblue") +
    scale_x_continuous(breaks = seq(min(train_data$year), 
                                    max(train_data$year), by = 1)) +
    labs(
      title = "Average UHC Service Coverage by Year",
      x = "Year",
      y = "Average UHC Coverage Index",
      size = "Countries with data"
    ) +
    theme_minimal() 
} 

# Plot the UHC curve to see if it's linear enough to interpolate all the missing data
PlotUHC(train_data)

# Since the plot between 2000 and 2015 is pretty straight, we are going to linearly
# interpolate all the missing values.  If this variable turns out to be critically
# important to our predictive model we'll have to note that!

anchor_years <- c(2000, 2005, 2010, 2015)
train_data <- train_data %>%
  arrange(CountryCode, year) %>%
  group_by(CountryCode) %>%
  group_modify(~{
    df <- .x
    anchor_rows <- df$year %in% anchor_years & !is.na(df$UHCServiceCoverage)
    
    if (sum(anchor_rows) >= 2) {
      interp <- approx(
        x = df$year[anchor_rows],
        y = df$UHCServiceCoverage[anchor_rows],
        xout = df$year,
        method = "linear",
        rule = 1
      )$y
      
      # replace missing values only
      df$UHCServiceCoverage[is.na(df$UHCServiceCoverage)] <- interp[is.na(df$UHCServiceCoverage)]
    }
    df
  }) %>%
  ungroup()


# plot the curve again to make sure it did it right
PlotUHC(train_data)

# Now - interpolate the years 2016, 2018 and 2020
train_data <- train_data %>%
  arrange(CountryCode, year) %>%
  group_by(CountryCode) %>%
  mutate(
    UHCServiceCoverage = case_when(
      year == 2016 & !is.na(lag(UHCServiceCoverage, 1)) & !is.na(lead(UHCServiceCoverage, 1)) &
        lag(year, 1) == 2015 & lead(year, 1) == 2017 ~ 
        (lag(UHCServiceCoverage, 1) + lead(UHCServiceCoverage, 1)) / 2,
      
      year == 2018 & !is.na(lag(UHCServiceCoverage, 1)) & !is.na(lead(UHCServiceCoverage, 1)) &
        lag(year, 1) == 2017 & lead(year, 1) == 2019 ~ 
        (lag(UHCServiceCoverage, 1) + lead(UHCServiceCoverage, 1)) / 2,
      
      year == 2020 & !is.na(lag(UHCServiceCoverage, 1)) & !is.na(lead(UHCServiceCoverage, 1)) &
        lag(year, 1) == 2019 & lead(year, 1) == 2021 ~ 
        (lag(UHCServiceCoverage, 1) + lead(UHCServiceCoverage, 1)) / 2,
      
      TRUE ~ UHCServiceCoverage
    )
  ) %>%
  ungroup()

# plot the curve again to make sure it did it right
PlotUHC(train_data)

# Now extrapolate missing 2022 using 2019-2021
train_data <- train_data %>%
  group_by(CountryCode) %>%
  group_modify(~ {
    this_group <- .x
    
    # Try to fit model if at least 2 of 2019-2021 are present
    recent_years <- c(2019, 2020, 2021)
    recent_data <- this_group %>%
      filter(year %in% recent_years & !is.na(UHCServiceCoverage))
    
    # If enough data to fit a line
    if (nrow(recent_data) >= 2) {
      model <- lm(UHCServiceCoverage ~ year, data = recent_data)
      predicted_2022 <- predict(model, newdata = data.frame(year = 2022))[1]
      
      # Fill in 2022 if it's NA
      this_group <- this_group %>%
        mutate(UHCServiceCoverage = ifelse(year == 2022 & is.na(UHCServiceCoverage),
                                           predicted_2022,
                                           UHCServiceCoverage))
    }
    
    this_group
  }) %>%
  ungroup()

# plot the curve again to make sure it did it right
PlotUHC(train_data)

# Show the heatmap again
OverallMissingnessHeatmap(train_data)
# HDI Score Classifications based on UNDP classification parameters

# Function to Plot PopInSlums by year
PlotSlums <- function(data){
  data %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    summarize(
      avg_slums = mean(PopInSlums, na.rm = TRUE),
      n_countries = sum(!is.na(PopInSlums))
    ) %>%
    filter(!is.na(avg_slums)) %>%
    arrange(year) %>%
    ggplot(aes(x = year, y = avg_slums)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = n_countries), color = "steelblue") +
    scale_x_continuous(breaks = seq(min(train_data$year), 
                                    max(train_data$year), by = 1)) +
    labs(
      title = "Average Population in Slums by Year",
      x = "Year",
      y = "Average Population in Slums",
      size = "Countries with data"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} 
# Plot PopInSLums by year
PlotSlums(train_data)

# Rerun all charts.  
FourPlots(train_data,30)

# Linearly interpolate PopInSlums
train_data <- train_data %>%
  mutate(year = as.numeric(year)) %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, year) %>%
  mutate(PopInSlums = if_else(
    year %% 2 == 1 & is.na(PopInSlums),  # Only odd years and NA
    zoo::na.approx(PopInSlums, x = year, na.rm = FALSE),  # interpolate
    PopInSlums  # keep existing values
  )) %>%
  ungroup()

# Re-plot
PlotSlums(train_data)

# Rerun all charts.  
FourPlots(train_data,30)

train_data %>%
  select(VoiceAccountability, RuleOfLaw, PoliticalStability, GovtEffectiveness, CorruptionScore) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarize(
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Count = sum(!is.na(Value))
  ) %>%
  arrange(Variable) %>%
  kable(digits = 3)

# Filter to years 2000–2003 and select the variables of interest
train_data %>%
  filter(year %in% 2000:2003) %>%
  select(CountryCode, year, 
         VoiceAccountability, RuleOfLaw, PoliticalStability, GovtEffectiveness, CorruptionScore) %>%
  pivot_longer(cols = -c(CountryCode, year), names_to = "Variable", values_to = "Value") %>%
  group_by(year, Variable) %>%
  summarize(
    mean_value = mean(Value, na.rm = TRUE),
    n_countries = sum(!is.na(Value)),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = mean_value)) +
  geom_line(color = "darkgreen") +
  geom_point(aes(size = n_countries), color = "darkgreen") +
  facet_wrap(~ Variable, scales = "fixed") +  # force same Y-axis for all plots
 # ylim(-0.1, 0.1) +  # WGI scale range
  labs(
    title = "Average Values (2000–2003) for Key Governance Indicators",
    x = "Year",
    y = "Average Value",
    size = "Countries with data"
  ) +
  scale_x_continuous(breaks = 2000:2003) +
  theme_minimal()

gov_vars <- c("CorruptionScore", "GovtEffectiveness", "PoliticalStability", 
             "RuleOfLaw", "VoiceAccountability")

delta_check <- train_data %>%
  filter(year %in% c(2000, 2002, 2003)) %>%
  select(CountryCode, year, all_of(gov_vars)) %>%
  pivot_longer(cols = -c(CountryCode, year), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = year, values_from = Value, names_prefix = "year_") %>%
  filter(!is.na(year_2000) & !is.na(year_2002) & !is.na(year_2003)) %>%
  mutate(
    slope_2000_2002 = (year_2002 - year_2000) / 2,
    slope_2002_2003 = (year_2003 - year_2002) / 1,
    slope_diff = abs(slope_2000_2002 - slope_2002_2003),
    changed_direction = sign(slope_2000_2002) != sign(slope_2002_2003)
  )

delta_check %>%
  group_by(Variable) %>%
  summarize(
    mean_slope_diff = mean(slope_diff),
    pct_large_deviation = mean(slope_diff > 0.25) * 100,
    max_deviation = max(slope_diff),
    n_chg_direction = sum(changed_direction),
    n = n()
  ) %>%
  arrange(desc(pct_large_deviation))

# Since there is a decent amount of deviation at the country level across 2020-2023,
# including that the slope of the curve changes direction more than 50% of the time,
# we are using a quadratic fit formula to estimate 2001 using data from 2000, 2002
# and 2003 - unless 2003 doesn't exist in which we case we interpolate 2000-2002
# or leave NA if insufficient data to do that

gov_vars <- c("CorruptionScore", "GovtEffectiveness", "PoliticalStability",
              "RuleOfLaw", "VoiceAccountability")

train_data <- train_data %>%
  arrange(CountryCode, year) %>%
  group_by(CountryCode) %>%
  group_modify(~{
    g <- .x
    for (v in gov_vars) {
      # only attempt if 2001 exists and is NA
      if (any(g$year == 2001) && is.na(g[[v]][g$year == 2001])) {
        # gather supporting points
        pts <- g %>% filter(year %in% c(2000, 2002, 2003)) %>%
          select(year, !!rlang::sym(v)) %>% stats::na.omit()
        
        if (nrow(pts) >= 3) {
          # quadratic fit through 2000, 2002, 2003
          mdl <- lm(formula = pts[[v]] ~ poly(pts$year, 2, raw = TRUE))
          pred <- predict(mdl, newdata = data.frame(year = 2001))[1]
          pred <- max(-2.5, min(2.5, pred))  # clamp to WGI range
          g[[v]][g$year == 2001] <- pred
          
        } else {
          # fallback: linear midpoint if 2000 and 2002 exist
          y0 <- g[[v]][g$year == 2000]
          y2 <- g[[v]][g$year == 2002]
          if (length(y0) == 1 && length(y2) == 1 && !is.na(y0) && !is.na(y2)) {
            pred <- (y0 + y2) / 2
            pred <- max(-2.5, min(2.5, pred))
            g[[v]][g$year == 2001] <- pred
          }
        }
      }
    }
    g
  }) %>%
  ungroup()

# Rerun all charts. 
FourPlots(train_data,30)

# The obvious one that's missing:  Delete observations with no HDI Index and/or
# InternetPct since these are fundamental to the study

train_data <- train_data %>%
  filter(!is.na(InternetUsersPct) & !is.na(HDI_Index))

# Rerun all charts. 
FourPlots(train_data,30)

# Heatmap of missingness for Liechtenstein (white=present, red=missing)
code <- "LIE"
OneCountryMissingness(train_data,code)

# Too much data missing from CountryCode "LIE" so drop it
train_data <- train_data %>%
  filter(CountryCode != "LIE")

#Plot Again
FourPlots(train_data,10)

# Now fix the missing WaterStress data for 2022 via extrapolation

target_year <- 2022
lookback_n  <- 7
min_points  <- 3

print(paste0("Total Countries with data for 2022: ",
             sum(train_data$year == 2022)))
      
print(paste0("Countries with NA for 2022 WaterStress: ",
             sum(train_data$year == 2022 & 
                   is.na(train_data$WaterStress))))

print(paste0("Countries with at least 3 years of WaterStress data: ",
             sum(tapply(!is.na(train_data$WaterStress) & 
                          train_data$year < 2022,
                        train_data$CountryCode,sum) >= 3)))

hist_idx <- !is.na(train_data$WaterStress) & train_data$year < target_year
hist_df  <- train_data[hist_idx, c("CountryCode", "year", "WaterStress")]
hist_df  <- hist_df[!duplicated(hist_df[, c("CountryCode", "year")]), ]

## Split by country
by_ctry <- split(hist_df, hist_df$CountryCode)

## For each country, take recent lookback_n rows and fit lm; predict 2022 (or NA)
pred_vec <- sapply(names(by_ctry), function(cc) {
  df <- by_ctry[[cc]]
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  df <- df[order(df$year), ]
  if (nrow(df) > lookback_n) df <- df[(nrow(df) - lookback_n + 1):nrow(df), ]
  if (nrow(df) < min_points) return(NA_real_)
  fit <- try(lm(WaterStress ~ year, data = df), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  p <- as.numeric(predict(fit, newdata = data.frame(year = target_year)))
  p
})

## Assemble predictions table (CountryCode, year=2022, pred)
pred_df <- data.frame(
  CountryCode = names(pred_vec),
  year        = target_year,
  pred        = as.numeric(pred_vec),
  row.names   = NULL,
  stringsAsFactors = FALSE
)

## Merge back to original; create WaterStress_imputed ONLY for 2022 missing rows
out <- merge(
  train_data,
  pred_df,
  by = c("CountryCode", "year"),
  all.x = TRUE,
  sort = FALSE
)

# If 2022 & WaterStress is NA & pred is not NA -> impute; else NA
out$WaterStress_imputed <- ifelse(
  out$year == target_year & is.na(out$WaterStress) & !is.na(out$pred),
  out$pred,
  NA_real_
)

## Drop helper column and return to original object name
out$pred <- NULL
train_data <- out


imputed_2022 <- with(train_data,
                     sum(year == 2022 & !is.na(WaterStress_imputed), 
                         na.rm = TRUE))
imputed_2022

# Show me the ones that are still NA
still_NA_2022 <- unique(train_data$CountryCode[
  train_data$year == 2022 & is.na(train_data$WaterStress_imputed)
])
still_NA_2022

# Countries (among the 13) with >=3 valid pre-2022 WaterStress values
eligible_13 <- names(which(tapply(
  train_data$year < 2022 & !is.na(train_data$WaterStress),
  train_data$CountryCode, sum) >= 3))

eligible_13 <- intersect(still_NA_2022, eligible_13)
eligible_13

subset(train_data,
       CountryCode %in% eligible_13 & year < 2022 ,
       select = c("CountryCode","year","WaterStress"))

ineligible_13 <- setdiff(still_NA_2022, eligible_13)
ineligible_13
subset(train_data,
       CountryCode %in% ineligible_13 & year < 2022 ,
       select = c("CountryCode","year","WaterStress"))
# Turns out there is NO data for any of those 13 countries
# Copy the imputed data over
# Copy 2022 imputed into WaterStress (only where WaterStress is NA), then drop helper column
idx <- train_data$year == 2022 & is.na(train_data$WaterStress) & 
  !is.na(train_data$WaterStress_imputed)
train_data$WaterStress[idx] <- train_data$WaterStress_imputed[idx]
train_data$WaterStress_imputed <- NULL

FourPlots(train_data,10)

# We are down to four features with material missing values.   Let's see if
# we can just drop all rows with NA values first!

# Total number of rows
total_rows <- nrow(train_data)

# Number of rows with any NA
rows_with_na <- sum(!complete.cases(train_data))

# Percentage of rows that would be removed
percent_removed <- (rows_with_na / total_rows) * 100

# Number of rows remaining if you drop them
rows_remaining <- total_rows - rows_with_na

# Print results
cat("Rows with any NA:", rows_with_na, "\n")
cat("Percent of rows removed:", round(percent_removed, 2), "%\n")
cat("Rows remaining:", rows_remaining, "\n")

# Nope - we would lose too much of our data.  So we will
# impute the missing data.  That's fine for all but the top 4 variables, since
# the rest all have <5% missingness.   We'll note it for the top 4 and run separate
# models that don't include them

# We use the impute_time_trend function to do this

train_data <-impute_time_trend(train_data, CorruptionScore, -2.5, 2.5)
train_data <-impute_time_trend(train_data, ElectricAccess, 0,100)
train_data <-impute_time_trend(train_data, FoodIndex,-Inf, Inf)
train_data <-impute_time_trend(train_data, GDP, -Inf, Inf)
train_data <-impute_time_trend(train_data, GDPGrowth, -Inf, Inf)
train_data <-impute_time_trend(train_data, GDPPerCapGrowth, -Inf, Inf)
train_data <-impute_time_trend(train_data, GovtEduSpendPctGDP, 0, 100)
train_data <-impute_time_trend(train_data, GovtEffectiveness, -2.5, 2.5)
train_data <-impute_time_trend(train_data, GovtHealthSpendPerCapita, 0,Inf)
train_data <-impute_time_trend(train_data, HealthSpendPerCapita, 0, Inf)
train_data <-impute_time_trend(train_data, PoliticalStability, -2.5,2.5)
train_data <-impute_time_trend(train_data, PopDensity, 0, Inf)
train_data <-impute_time_trend(train_data, PopInSlums, 0, 100)
train_data <-impute_time_trend(train_data, RqdEduYears, 0, 15)
train_data <-impute_time_trend(train_data, RuleOfLaw, -2.5, 2.5)
train_data <-impute_time_trend(train_data, RuralPopulGrowth, -Inf, Inf)
train_data <-impute_time_trend(train_data, UHCServiceCoverage, 0, 100)
train_data <-impute_time_trend(train_data, VoiceAccountability, -2.5, 2.5)
train_data <-impute_time_trend(train_data, WaterStress, 0,Inf)

test_data <-impute_time_trend(test_data, CorruptionScore, -2.5, 2.5)
test_data <-impute_time_trend(test_data, ElectricAccess, 0,100)
test_data <-impute_time_trend(test_data, FoodIndex,-Inf, Inf)
test_data <-impute_time_trend(test_data, GDP, -Inf, Inf)
test_data <-impute_time_trend(test_data, GDPGrowth, -Inf, Inf)
test_data <-impute_time_trend(test_data, GDPPerCapGrowth, -Inf, Inf)
test_data <-impute_time_trend(test_data, GovtEduSpendPctGDP, 0, 100)
test_data <-impute_time_trend(test_data, GovtEffectiveness, -2.5, 2.5)
test_data <-impute_time_trend(test_data, GovtHealthSpendPerCapita, 0,Inf)
test_data <-impute_time_trend(test_data, HealthSpendPerCapita, 0, Inf)
test_data <-impute_time_trend(test_data, PoliticalStability, -2.5,2.5)
test_data <-impute_time_trend(test_data, PopDensity, 0, Inf)
test_data <-impute_time_trend(test_data, PopInSlums, 0, 100)
test_data <-impute_time_trend(test_data, RqdEduYears, 0, 15)
test_data <-impute_time_trend(test_data, RuleOfLaw, -2.5, 2.5)
test_data <-impute_time_trend(test_data, RuralPopulGrowth, -Inf, Inf)
test_data <-impute_time_trend(test_data, UHCServiceCoverage, 0, 100)
test_data <-impute_time_trend(test_data, VoiceAccountability, -2.5, 2.5)
test_data <-impute_time_trend(test_data, WaterStress, 0,Inf)

FourPlots(train_data,10)
#
########################################################################


# PCA SECTION 
# PCA Recipe
numeric_cols_for_pca <- train_data %>%
  select(where(is.numeric), -year, 
         -HDI_Index,
         -Lag1_InternetUsersPct, 
         -Lag2_InternetUsersPct, 
         -Cumulative3yrChg_InternetUsersPct,
         -Lag2_YearlyChgInternet,
         -Lag1_YearlyChgInternet, 
         -YearlyChgInternet,
         -YearlyChgHDI) %>%
  names()

pca_recipe <- train_data %>%
  recipe() %>%
  step_impute_mean(all_of(numeric_cols_for_pca)) %>%
  step_normalize(all_of(numeric_cols_for_pca)) %>%
  step_pca(all_of(numeric_cols_for_pca), num_comp = 5, id = "pca")

pca_results <- prep(pca_recipe)


#PCA Data Baking
PCAData <- bake(pca_results, new_data = train_data)

# Visualizing the PCA Data

# Scree Plot
# Get variance explained from the tidy output
pca_var <- tidy(pca_results, id = "pca", type = "variance") %>%
  filter(component <= 5) %>%
  pivot_wider(names_from = terms, values_from = value) %>%
  mutate(PC = paste0("PC", component),
         pct_var = `percent variance` / 100,
         cum_pct_var = `cumulative percent variance` / 100)

ggplot(pca_var, aes(x = PC)) +
  geom_col(aes(y = `pct_var`), fill = "lightblue") +
  geom_line(aes(y = `cum_pct_var`, group = 1), color = "red", linewidth = 1) +
  geom_point(aes(y = `cum_pct_var`), color = "red", size = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Scree Plot with Cumulative Variance",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()




# PCA Loadings Results
pca_loadings_long <- tidy(pca_results, id = "pca", type = "coef")
pca_loadings_wide <- pca_loadings_long %>%
  pivot_wider(
    names_from = component,
    values_from = value
  )


print("\nLoadings for each principal component:")
print(pca_loadings_long)

# PCA Biplot for HDI Category
PCAData$HDI_Category <- factor(
  PCAData$HDI_Category,
  levels = c("Very high human development", "High human development","Medium human development","Low human development")
)


pca_biplot <- ggplot(PCAData, aes(x = PC1, y = PC2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  
  geom_point(alpha = 0.5, aes(color = HDI_Category)) +
  
  geom_segment(
    data = pca_loadings_wide,
    aes(x = 0, y = 0, xend = PC1 * 24, yend = PC2 * 24),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "gray",
    linewidth = 0.5
  ) +
  
  geom_text(
    data = pca_loadings_wide,
    aes(x = PC1 * 28, y = PC2 * 32, label = terms),
    color = "black",
    size = 2,
    hjust = 0.5, vjust = 3
  ) +
  
  
  labs(
    title = "PCA Biplot of PC1 and PC2",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(-12, 12)) +
  scale_y_continuous(limits = c(-12, 12)) +
  
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
    title = "Variable Results for PC1",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal()

print(pca_bar_plot)

# PCA Summary
print("\nFirst 5 rows of the Data with Principal Components:")
print(PCAData %>% summary())
print(cor(PCAData$PC1, train_data$HDI_Index, use = "complete.obs"))


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


# Radom Forest PCA

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


