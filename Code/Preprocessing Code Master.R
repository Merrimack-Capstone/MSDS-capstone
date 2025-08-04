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


#RawData <- readRDS(here::here("Data","PreEDA_DataFrame.rds"))
#names(RawData) <- make.unique(names(RawData), sep = ".")
#FieldActions <- read_excel(here::here("Data", "FieldActions.xlsx"))
#CleanedData <- RawData # make a copy to preserve original
#IncomeData <- read_excel(here::here("Data", "Income Group Data.xlsx")) # Import income data

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

# Delete the six features that have over 75% missing data
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

# Rerun all charts.  Now the real work begins
MissingnessByVariable(PreProcessingData)
MissingnessByYear(PreProcessingData)
MissingnessByCountry(PreProcessingData, 30)
OverallMissingnessHeatmap(PreProcessingData)

# This function plots average UCHCServiceCoverage by year 

PlotUHC <- function(PreProcessingData){
 PreProcessingData %>%
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
    scale_x_continuous(breaks = seq(min(PreProcessingData$year), max(PreProcessingData$year), by = 1)) +
    labs(
      title = "Average UHC Service Coverage by Year",
      x = "Year",
      y = "Average UHC Coverage Index",
      size = "Countries with data"
    ) +
    theme_minimal() 
} 

# Plot the UHC curve to see if it's linear enough to interpolate all the missing data
PlotUHC(PreProcessingData)

# Since the plot between 2000 and 2015 is pretty straight, we are going to linearly
# interpolate all the missing values.  If this variable turns out to be critically
# important to our predictive model we'll have to note that!

PreProcessingData <- PreProcessingData %>%
  arrange(CountryCode, year) %>%
  group_by(CountryCode) %>%
  mutate(
    UHCServiceCoverage = if (sum(!is.na(UHCServiceCoverage[year >= 2000 & year <= 2014])) >= 2) {
      if_else(
        year >= 2000 & year <= 2014,
        approx(
          x = year[!is.na(UHCServiceCoverage)],
          y = UHCServiceCoverage[!is.na(UHCServiceCoverage)],
          xout = year,
          method = "linear",
          rule = 1
        )$y,
        UHCServiceCoverage
      )
    } else {
      UHCServiceCoverage
    }
  ) %>%
  ungroup()


# plot the curve again to make sure it did it right
PlotUHC(PreProcessingData)

# Now - interpolate the years 2016, 2018 and 2020
PreProcessingData <- PreProcessingData %>%
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
# HDI Score Classifications based on UNDP classification parameters

# Function to Plot PopInSlums by year
PlotSlums <- function(PreProcessingData){
  PreProcessingData %>%
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
    scale_x_continuous(breaks = seq(min(PreProcessingData$year), 
                                    max(PreProcessingData$year), by = 1)) +
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
PlotSlums(PreProcessingData)

# Rerun all charts.  Now the real work begins
MissingnessByVariable(PreProcessingData)
MissingnessByYear(PreProcessingData)
MissingnessByCountry(PreProcessingData, 30)
OverallMissingnessHeatmap(PreProcessingData)

# Linearly interpolate PopInSlums
PreProcessingData <- PreProcessingData %>%
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
PlotSlums(PreProcessingData)

# Rerun all charts.  Now the real work begins
MissingnessByVariable(PreProcessingData)
MissingnessByYear(PreProcessingData)
MissingnessByCountry(PreProcessingData, 30)
OverallMissingnessHeatmap(PreProcessingData)

PreProcessingData %>%
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
  knitr::kable(digits = 3)

# Filter to years 2000–2003 and select the variables of interest
PreProcessingData %>%
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

delta_check <- PreProcessingData %>%
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
MissingnessByVariable(PreProcessingData)
MissingnessByYear(PreProcessingData)
MissingnessByCountry(PreProcessingData, 30)
OverallMissingnessHeatmap(PreProcessingData)

# The obvious one that's msising:  Delete observations with no HDI Index and/or
# InternetPct since these are fundamental to the study

PreProcessingData <- PreProcessingData %>%
  filter(!is.na(InternetUsersPct) & !is.na(HDI_Index))

# Rerun all charts. 
MissingnessByVariable(PreProcessingData)
MissingnessByYear(PreProcessingData)
MissingnessByCountry(PreProcessingData, 30)
OverallMissingnessHeatmap(PreProcessingData)

# Heatmap of missingness for Liechtenstein (white=present, red=missing)
code <- "LIE"
missing_heatmap_data <- PreProcessingData %>%
  filter(CountryCode == code) %>%
  select(-CountryCode) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  mutate(Status = ifelse(is.na(value), "Missing", "Present"))


# Optional: order variables by overall missingness (most missing at top)
var_order <- missing_heatmap_data %>%
  dplyr::group_by(variable) %>%
  dplyr::summarize(frac_missing = mean(Status == "Missing"), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(frac_missing)) %>%
  dplyr::pull(variable)

missing_heatmap_data <- missing_heatmap_data %>%
  dplyr::mutate(variable = factor(variable, levels = var_order))

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




# Set up HDI Bins
Preprocessing_classified_data <- PreProcessingData %>%
  mutate(
    HDI_Category = case_when(
      HDI_Index >= 0.800 ~ "Very high human development",
      HDI_Index >= 0.700 &HDI_Index <= 0.799 ~ "High human development",
      HDI_Index >= 0.550 & HDI_Index <= 0.699 ~ "Medium human development",
      TRUE ~ "Low human development"
    )
  )

# move the position of the new classified HDI data to a more relevant spot
Preprocessing_classified_data <- Preprocessing_classified_data%>%
  relocate(HDI_Category, .after = HDI_Index)

print("\nData after HDI classification:")
print(Preprocessing_classified_data)

# Combined Governance Score to address collinearity
Preprocessing_combined_scores_data <- Preprocessing_classified_data %>%
  rowwise() %>%
  mutate(
    GovernanceScore = mean(c(GovtEffectiveness, RuleOfLaw, CorruptionScore), na.rm = TRUE)
  )

Preprocessing_combined_scores_data <- Preprocessing_combined_scores_data %>%
  select(-GovtEffectiveness,
         -RuleOfLaw,
         -CorruptionScore
         )
print("\nData with the new 'Governance_Score' column:")
print(Preprocessing_combined_scores_data)

# Combined Health Spend to address collinearity
Preprocessing_combined_scores_data <- Preprocessing_combined_scores_data %>%
  rowwise() %>%
  mutate(
    HealthSpend = mean(c(HealthSpendPerCapita, GovtHealthSpendPerCapita), na.rm = TRUE)
  )

Preprocessing_combined_scores_data <- Preprocessing_combined_scores_data %>%
  select(-HealthSpendPerCapita,
         -GovtHealthSpendPerCapita,
  )
print("\nData with the new 'HealtSpend' column:")
print(Preprocessing_combined_scores_data)


# Yeo-Johnson Transformation to handle skewness

numeric_cols <- Preprocessing_combined_scores_data %>%
  select(where(is.numeric), -year) %>%
  names()

# Preprocessing recipe 
data_recipe <- P %>%
  recipe() %>%
  step_YeoJohnson(all_of(numeric_cols), na_rm = TRUE)

# Prep the data

transformed_recipe <- prep(data_recipe)

# Bake the data
TransformedData <- bake(transformed_recipe, new_data = Preprocessing_combined_scores_data)

# Yeo-Johnson Output
print("\nEstimated lambdas for transformed variables:")
print(transformed_recipe$steps[[1]]$lambdas)

# Yeo-Johnson Histogram Results
par(mfrow=c(1, 2))
hist(Preprocessing_combined_scores_data$InternetUsersPct, main = "Original Internet Users", xlab = "Original Values")
hist(TransformedData$InternetUsersPct, main = "Transformed Original Internet Users", xlab = "Transformed Values")
par(mfrow=c(1, 1))

sum(Preprocessing_combined_scores_data$InternetUsersPct < 0, na.rm = TRUE)

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