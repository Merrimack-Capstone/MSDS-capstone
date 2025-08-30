# REC CODE
library(tidyverse)
library(writexl)
library(knitr)
library(scales)
library(stringr)
library(recipes)
library(broom)
library(here)
library(rlang)
library(zoo)

PreProcessingData <- readRDS(here::here("Data","FirstCutData.rds")) # Get Data
stoplight <- c("#1a9641", "#ffea00", "#d7191c") # define heatmap colors
sleeptime <- 0.5
##########################################################################

# FUNCTIONS

wait_to_render <- function(seconds){
  Sys.sleep(seconds())
}

# Overall data missingness heatmap
OneCountryMissingness <- function(data, code) {
  
  data <- data %>%
    dplyr::select(-Lag1_InternetUsersPct,
           -Lag2_InternetUsersPct,
           -Lag1_YearlyChgInternet,
           -Lag2_YearlyChgInternet,
           -Cumulative3yrChg_InternetUsersPct,
           -YearlyChgHDI,
           -YearlyChgInternet)
  
  missing_heatmap_data <- data %>%
    filter(CountryCode == code) %>%
    dplyr::select(-CountryCode, -CountryName, -IncomeGroup, -HDI_Category) %>%
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
  wait_to_render(sleeptime)
}

OverallMissingnessHeatmap <- function(data) {
  data <- data %>%
    dplyr::select(-Lag1_InternetUsersPct,
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
   wait_to_render(sleeptime)
}

# Missingness chart by variable

MissingnessByVariable <- function(data) {
   PlotData <- data %>%
    dplyr::select(-Lag1_InternetUsersPct,
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
  wait_to_render(sleeptime)
}

# Missingness chart by year
MissingnessByYear <- function(data) {
  PlotData <- data %>%
    dplyr::select(-Lag1_InternetUsersPct,
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
  wait_to_render(sleeptime)
}

MissingnessByCountry <- function(data, threshold) {
  PlotData <- data %>%
    dplyr::select(-Lag1_InternetUsersPct,
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
  wait_to_render(sleeptime)
}

# This function just reproduces our four missingness plots
FourPlots <- function(data, cutoff) {
  MissingnessByVariable(data)
  MissingnessByYear(data)
  MissingnessByCountry(data, cutoff)
  OverallMissingnessHeatmap(data)
}
# This function plots average UCHCServiceCoverage by year 

PlotUHC <- function(data, sleeptime = 0) {
  data_to_plot <- data %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    summarize(
      avg_uhc = mean(UHCServiceCoverage, na.rm = TRUE),
      n_countries = sum(!is.na(UHCServiceCoverage)),
      .groups = "drop"
    ) %>%
    filter(!is.na(avg_uhc)) %>%
    arrange(year)
  
  plot <- ggplot(data_to_plot, aes(x = year, y = avg_uhc)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = n_countries), color = "steelblue") +
    scale_x_continuous(breaks = seq(min(data_to_plot$year), 
                                    max(data_to_plot$year), by = 1)) +
    labs(
      title = "Average UHC Service Coverage by Year",
      x = "Year",
      y = "Average UHC Coverage Index",
      size = "Countries with data"
    ) +
    theme_minimal()
  
  print(plot)
  Sys.sleep(sleeptime)  
}

# Function to Plot PopInSlums by year
PlotSlums <- function(data){
  data_to_plot <- data %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    summarize(
      avg_slums = mean(PopInSlums, na.rm = TRUE),
      n_countries = sum(!is.na(PopInSlums))
    ) %>%
    filter(!is.na(avg_slums)) %>%
    arrange(year)
  
  plot <- ggplot(data_to_plot, aes(x = year, y = avg_slums)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = n_countries), color = "steelblue") +
    scale_x_continuous(breaks = seq(min(data_to_plot$year), 
                                    max(data_to_plot$year), by = 1)) +
    labs(title = "Average Population in Slums by Year",x = "Year",
         y = "Average Population in Slums", size = "Countries with data") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plot)
  wait_to_render(sleeptime)
} 

# Yeo-Johnson Transformation to handle skewness.  This function creates the
# recipe, preps it and returns the prepped recipe.   That prepped recipe will 
# be used to "bake" both the training and 

Yeo_Johnson <- function(train_data, test_data) {
  
  # Get affected columns from training data
  affected_cols <- train_data %>%
    dplyr::select(InternetUsersPct, 
           Lag1_InternetUsersPct, 
           Lag2_InternetUsersPct) %>%
    names()
  
  # Preprocessing recipe - fit only on training data
  data_recipe <- recipe(~ ., data = train_data) %>%
    update_role(CountryName, CountryCode, new_role = "id") %>%
    step_YeoJohnson(all_of(affected_cols), na_rm = TRUE)
  
  # Prep the recipe on training data
  transformed_recipe <- prep(data_recipe, training = train_data)
  
  # Bake the data
  train_transformed <- bake(transformed_recipe, new_data = train_data)
  test_transformed  <- bake(transformed_recipe, new_data = test_data)
  
  # Yeo-Johnson Output
  cat("\nEstimated lambdas for transformed variables:\n")
  print(transformed_recipe$steps[[1]]$lambdas)
  
  # Histogram comparisons for training data
  par(mfrow = c(1, 2))
  hist(train_data$InternetUsersPct, 
       main = "Train: Original Internet Users", 
       xlab = "Original Values")
  hist(train_transformed$InternetUsersPct, 
       main = "Train: Transformed Internet Users", 
       xlab = "Transformed Values")
  
  # Histogram comparisons for test data
  par(mfrow = c(1, 2))
  hist(test_data$InternetUsersPct, 
       main = "Test: Original Internet Users", 
       xlab = "Original Values")
  hist(test_transformed$InternetUsersPct, 
       main = "Test: Transformed Internet Users", 
       xlab = "Transformed Values")
  
  par(mfrow = c(1, 1)) # reset layout
  
  # Output first 5 rows of transformed training data
  cat("\nFirst 5 rows of Transformed Training Data:\n")
  print(head(train_transformed))
  
  # Return both datasets
  return(list(
    train = train_transformed,
    test = test_transformed
  ))
}
#
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

### FIRST - DO ALLTHE PREPROCESSING THAT CAN BE DONE ON THE ENTIRE SET 
###.(BEFORE SPLITTING INTO TESTING AND TRAINING)
#
# Important to note that since we are splitting our series by country,
# retain all years' data for an individual country together in either
# training or test (no country will have data in both sets, we can apply
# all of our imputation methods to the entire data set because none of those
# methods imputes data across countries - it's all WITHIN a country's time series
# Only the Yeo-Johnson transforms and PCA are done separately since we need to apply
# the results of these obtained from the training data onto the test data to avoid
# leakage and ensure we are using the same modeling approach

###################################################################
#
# Drop GiniCoeff, IHDI_Index and FixedIntSUBS.  The first 2 we aren't 
# using in the first go-round, and the the last one we may not get to use at all (it
# shows fixed internet subscriptions, which is only part of the picture of internet use. 
# In the first models we will just use InternetUsersPct or its derivatives

PreProcessingData <- PreProcessingData |>
  dplyr::select(-IHDI_Index, -GiniCoeff, -FixedIntSubs)

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
    
    Cumulative3yrChg_InternetUsersPct = InternetUsersPct - lag(InternetUsersPct, 3),
    Cumulative3yrChg_HDI = HDI_Index - lag(HDI_Index, 3)
  ) |>
  ungroup()

# Set up HDI Bins
PreProcessingData <- PreProcessingData %>%
  mutate(
    HDI_Category = case_when(
      HDI_Index >= 0.800 ~ "Very high",
      HDI_Index >= 0.700 & HDI_Index <= 0.799 ~ "High",
      HDI_Index >= 0.550 & HDI_Index <= 0.699 ~ "Medium",
      TRUE ~ "Low"
    ),
    HDI_Category = factor(
      HDI_Category,
      levels = c(
        "Low",
        "Medium",
        "High",
        "Very high"
      ),
      ordered = TRUE
    )
  )


# move the position of the new classified HDI data to a more relevant spot
PreProcessingData <- PreProcessingData%>%
  relocate(HDI_Category, .after = HDI_Index)

print("\nData after HDI classification:")
print(PreProcessingData)

# Combined Governance Score to address collinearity
PreProcessingData <- PreProcessingData %>%
  mutate(GovernanceScore = rowMeans(pick(GovtEffectiveness, 
                                         RuleOfLaw, CorruptionScore), 
                                    na.rm = TRUE)) %>%
  mutate(HealthSpend = rowMeans(pick(HealthSpendPerCapita, 
                                     GovtHealthSpendPerCapita), 
                                na.rm = TRUE))

PreProcessingData <- PreProcessingData %>%
  dplyr::select(-GovtEffectiveness,
         -RuleOfLaw,
         -CorruptionScore,
         -HealthSpendPerCapita,
         -GovtHealthSpendPerCapita)

print("\nData with the new columns:")
print(PreProcessingData)

##############################################################
#
# DEALING WITH MISSING DATA - OVERALL
#
# Show missingness heatmap
OverallMissingnessHeatmap(PreProcessingData)

# Plot missingness for all countries
MissingnessByCountry(PreProcessingData, 0)
# Zero in on subset > 35%
MissingnessByCountry(PreProcessingData, 35)

# Delete all the rows with country code starting with ZZ - virtually no data
PreProcessingData <- PreProcessingData %>%
  filter(!str_starts(CountryCode, "ZZ"))

# Show missingness heatmap
OverallMissingnessHeatmap(PreProcessingData)

# Re-examine missingness by year
MissingnessByYear(PreProcessingData)

# Delete 1995-1999 and 2023 (too sparse)
PreProcessingData <- PreProcessingData %>%
  filter(!(year %in% c(1995, 1996, 1997, 1998, 1999, 2023)))

# Review missingness by variable
MissingnessByVariable(PreProcessingData)

# Delete the features that have over 25% missing data with no
# pattern to the missingness

PreProcessingData <- PreProcessingData %>%
  dplyr::select(-MortalityFromDirtyness,
         -BankingAccess,
         -MigrantsPct,
         -FoodInsecurityPct,
         -RnDInvest,
         -ShippingIndex,
         -Homicides)

# Rerun all charts.  Now the real work begins
FourPlots(PreProcessingData,10)

# First we'll tackle UHCServiceCoverage which has a pattern to its missingness.
# We can interpolate the missing late years 92015 on) - can we interpolate the
# big blocks of missing data when it was only being reported every 5 years?
# We Plot the UHC curve to see if it's linear enough to do so
PlotUHC(PreProcessingData)

# Since the plot between 2000 and 2015 is pretty straight, we ARE going to linearly
# interpolate all the missing values where we can.  If this variable turns out to be 
# critically important to our predictive model we'll have to note that!

anchor_years <- c(2000, 2005, 2010, 2015)
PreProcessingData <- PreProcessingData %>%
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
PlotUHC(PreProcessingData)

# Now - interpolate the years 2016, 2018 and 2020
PreProcessingData <- PreProcessingData %>%

  group_by(CountryCode) %>%
  mutate(
    prev_val = UHCServiceCoverage[match(year - 1, year)],
    next_val = UHCServiceCoverage[match(year + 1, year)],
    UHCServiceCoverage = if_else(
      year %in% c(2016, 2018, 2020) &
        is.na(UHCServiceCoverage) &
        !is.na(prev_val) & !is.na(next_val),
      (prev_val + next_val) / 2,
      UHCServiceCoverage
    )
  ) %>%
  dplyr::select(-prev_val, -next_val) %>%
  ungroup()

# plot the curve again to make sure it did it right
PlotUHC(PreProcessingData)

# Now extrapolate missing 2022 using 2019-2021
PreProcessingData <- PreProcessingData %>%
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
PlotUHC(PreProcessingData)

# Show the heatmap again
OverallMissingnessHeatmap(PreProcessingData)
#
# Now we will tackle PopInSlums with straightforward interpolation of missing
# years, which follow an every-other-year pattern except for 2009 and 2019 which
# have a small handful of data points

# Plot PopInSLums by year
PlotSlums(PreProcessingData)

# Linearly interpolate PopInSlums
PreProcessingData <- PreProcessingData %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, year) %>%
  mutate(PopInSlums = if_else(
    year %% 2 == 1 & is.na(PopInSlums),  # Only odd years and NA
    na.approx(PopInSlums, x = year, na.rm = FALSE),  # interpolate
    PopInSlums  # keep existing values
  )) %>%
  ungroup()

# Re-plot
PlotSlums(PreProcessingData)

# Rerun all charts.  
FourPlots(PreProcessingData, 10)

# Now we interpolate the missing 2001 VoiceAccountability, PoliticalStability 
# and GovernanceScore data
PreProcessingData %>%
  dplyr::select(VoiceAccountability, PoliticalStability, GovernanceScore) %>%
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

# Filter to years 2000–2003 and dplyr::select the variables of interest
PreProcessingData %>%
  filter(year %in% 2000:2003) %>%
  dplyr::select(CountryCode, year, 
         VoiceAccountability, PoliticalStability, GovernanceScore) %>%
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

gov_vars <- c("VoiceAccountability", "PoliticalStability", "GovernanceScore")

delta_check <- PreProcessingData %>%
  filter(year %in% c(2000, 2002, 2003)) %>%
  dplyr::select(CountryCode, year, all_of(gov_vars)) %>%
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

PreProcessingData <- PreProcessingData %>%
  arrange(CountryCode, year) %>%
  group_by(CountryCode) %>%
  group_modify(~{
    g <- .x
    for (v in gov_vars) {
      # only attempt if 2001 exists and is NA
      if (any(g$year == 2001) && is.na(g[[v]][g$year == 2001])) {
        # gather supporting points
        pts <- g %>% filter(year %in% c(2000, 2002, 2003)) %>%
          dplyr::select(year, !!rlang::sym(v)) %>% stats::na.omit()
        
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
FourPlots(PreProcessingData,10)

# Heatmap of missingness for the three countries over 20%
code <- "SSD"
OneCountryMissingness(PreProcessingData,code)
code <- "LIE"
OneCountryMissingness(PreProcessingData,code)
code <- "PRK"
OneCountryMissingness(PreProcessingData,code)
code <- "MCO"
OneCountryMissingness(PreProcessingData,code)


# Too much important data missing from all three so drop them entirely
# (Most would be dropped before modeling anyway)
PreProcessingData <- PreProcessingData %>%
  filter(!CountryCode %in% c("SSD", "LIE", "PRK", "MCO"))

#Plot Again
FourPlots(PreProcessingData,10)

# Now fix the missing WaterStress data for 2022 via extrapolation

target_year <- 2022
lookback_n  <- 7
min_points  <- 3

print(paste0("Total Countries with data for 2022: ",
             sum(PreProcessingData$year == 2022)))
      
print(paste0("Countries with NA for 2022 WaterStress: ",
             sum(PreProcessingData$year == 2022 & 
                   is.na(PreProcessingData$WaterStress))))

print(paste0("Countries with at least 3 years of WaterStress data: ",
             sum(tapply(!is.na(PreProcessingData$WaterStress) & 
                          PreProcessingData$year < 2022,
                        PreProcessingData$CountryCode,sum) >= 3)))

hist_idx <- !is.na(PreProcessingData$WaterStress) & PreProcessingData$year < target_year
hist_df  <- PreProcessingData[hist_idx, c("CountryCode", "year", "WaterStress")]
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
  PreProcessingData,
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
PreProcessingData <- out

imputed_2022 <- with(PreProcessingData,
                     sum(year == 2022 & !is.na(WaterStress_imputed), 
                         na.rm = TRUE))
imputed_2022

# Show me the ones that are still NA
still_NA_2022 <- unique(PreProcessingData$CountryCode[
  PreProcessingData$year == 2022 & is.na(PreProcessingData$WaterStress_imputed)
])
still_NA_2022

# Countries (among the 13) with >=3 valid pre-2022 WaterStress values
eligible_13 <- names(which(tapply(
  PreProcessingData$year < 2022 & !is.na(PreProcessingData$WaterStress),
  PreProcessingData$CountryCode, sum) >= 3))

eligible_13 <- intersect(still_NA_2022, eligible_13)
eligible_13

subset(PreProcessingData,
       CountryCode %in% eligible_13 & year < 2022 ,
       select = c("CountryCode","year","WaterStress"))

ineligible_13 <- setdiff(still_NA_2022, eligible_13)
ineligible_13
subset(PreProcessingData,
       CountryCode %in% ineligible_13 & year < 2022 ,
       select = c("CountryCode","year","WaterStress"))
# Turns out there is NO data for any of those 13 countries
# Copy the imputed data over
# Copy 2022 imputed into WaterStress (only where WaterStress is NA), then drop helper column
idx <- PreProcessingData$year == 2022 & is.na(PreProcessingData$WaterStress) & 
  !is.na(PreProcessingData$WaterStress_imputed)
PreProcessingData$WaterStress[idx] <- PreProcessingData$WaterStress_imputed[idx]
PreProcessingData$WaterStress_imputed <- NULL

FourPlots(PreProcessingData,10)

# We are down to four features with material missing values.   Let's see if
# we can just drop all rows with NA values first!

# Total number of rows
total_rows <- nrow(PreProcessingData)

# Number of rows with any NA
rows_with_na <- sum(!complete.cases(PreProcessingData))

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

PreProcessingData <-impute_time_trend(PreProcessingData, WaterStress, 0,Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, VoiceAccountability, -2.5, 2.5)
PreProcessingData <-impute_time_trend(PreProcessingData, UHCServiceCoverage, 0, 100)
PreProcessingData <-impute_time_trend(PreProcessingData, RuralPopulGrowth, -Inf, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, RqdEduYears, 0, 15)
PreProcessingData <-impute_time_trend(PreProcessingData, PopInSlums, 0, 100)
PreProcessingData <-impute_time_trend(PreProcessingData, PopDensity, 0, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, PoliticalStability, -2.5,2.5)
PreProcessingData <-impute_time_trend(PreProcessingData, HealthSpend, 0, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, GovtEduSpendPctGDP, 0, 100)
PreProcessingData <-impute_time_trend(PreProcessingData, GovernanceScore, -2.5, 2.5)
PreProcessingData <-impute_time_trend(PreProcessingData, GDPPerCapGrowth, -Inf, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, GDPGrowth, -Inf, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, GDP, -Inf, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, FoodIndex,-Inf, Inf)
PreProcessingData <-impute_time_trend(PreProcessingData, ElectricAccess, 0,100)

FourPlots(PreProcessingData,10)
#
# DATA SET SPLITTING
#
# We now split data into training and test data.  This is done as follows:
#
# 1) Keep entire countries' data together - since this is time series data, the 
# observations are highly "autocorrelated" (related to nearby prior or future
# periods) and so splitting them up would create leakage.  
#
# 2) Put 70% of the countries in the training data and the remaining 30% in 
# the test data
#
# 3) Maintain the same stratification by IncomeGroup in both training and test data,
# since we showed in EDA that IncomeGroup is a very meaninginful clustering 
# differentiator for internet access and HDI - this prevents model bias
#
train_countries <- PreProcessingData %>%
  distinct(CountryCode, IncomeGroup) %>%
  group_by(IncomeGroup) %>%
  reframe(CountryCode = {
    set.seed(123)                # reset for each group
    sample_frac(pick(everything()), 0.7)$CountryCode
  }) %>%
  ungroup() %>%
  pull(CountryCode)

test_countries <- setdiff(
  unique(PreProcessingData$CountryCode),
  train_countries
)
train_data <- PreProcessingData %>% filter(CountryCode %in% train_countries)
test_data  <- PreProcessingData %>% filter(CountryCode %in% test_countries)
dim(train_data)
dim(test_data)
# Prepare training data distribution
train_dist <- train_data %>%
  distinct(CountryCode, IncomeGroup) %>%
  count(IncomeGroup, name = "Training_N") %>%
  ungroup() %>%
  mutate(Training_Pct = round(100 * Training_N / sum(Training_N), 1))

# Prepare test data distribution
test_dist <- test_data %>%
  distinct(CountryCode, IncomeGroup) %>%
  count(IncomeGroup, name = "Test_N") %>%
  ungroup() %>%
  mutate(Test_Pct = round(100 * Test_N / sum(Test_N), 1))

# Combine into one tibble
combined_dist <- full_join(train_dist, test_dist, by = "IncomeGroup") %>%
  arrange(IncomeGroup)

print(combined_dist)

################################################################
# 
# REMAINING PREPROCESSING
#
# Now we do all the transformations and imputations that have to be done on training 
# and test data separately to avoid leakage

# Fix the skewness
YJ_result <- Yeo_Johnson(train_data, test_data)
train_data <- YJ_result$train
test_data  <- YJ_result$test
#
########################################################################
#
# PCA - needs to be done on training data, with the same loadings applied
# to both training and test data
# PCA Recipe
numeric_cols_for_pca <- train_data %>%
  dplyr::select(where(is.numeric), -year, 
         -HDI_Index,
         -InternetUsersPct,
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
PCAData_train <- bake(pca_results, new_data = train_data)
PCAData_test <- bake(pca_results, new_data = test_data)

# Visualizing the PCA Data
#
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
pca_biplot <- ggplot(PCAData_train, aes(x = PC1, y = PC2)) +
  
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

# SAVE THE PREPPED DATA FILES FOR MODELS TO INGEST
#
saveRDS(train_data, file = here::here("Data", "train_data.rds"))
saveRDS(test_data, file = here::here("Data", "test_data.rds"))
saveRDS(PCAData_test, file = here::here("Data", "PCAData_test.rds"))
saveRDS(PCAData_train, file = here::here("Data", "PCAData_train.rds"))
write_xlsx(test_data, path = here::here("Data","test_data.xlsx"))
write_xlsx(train_data, path = here::here("Data", "train_data.xlsx"))
write_xlsx(PCAData_test, path = here::here("Data","PCAdata_test.xlsx"))
write_xlsx(PCAData_train, path = here::here("Data","PCAdata_train.xlsx"))
