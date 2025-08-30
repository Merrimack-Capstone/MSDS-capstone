# REC code

library(tidyverse)
library(readxl)
library(writexl)
library(skimr)
library(knitr)
library(kableExtra)
library(ggpmisc)
library(corrplot)
library(scales)
library(here)

# This module cleans and joins the raw data, saves the cleaned data for use 
# by preprocessing, and performs all Exploratory Data Analysis
#
# Read in data
RawData <- readRDS(here::here("Data","PreEDA_DataFrame.rds"))
names(RawData) <- make.unique(names(RawData), sep = ".") # ensure unique column names
FieldActions <- read_excel(here::here("Data", "FieldActions.xlsx"))
CleanedData <- RawData # make a copy to preserve original
IncomeData <- read_excel(here::here("Data", "Income Group Data.xlsx")) # Import income data

# This code iterates through every field in the raw data, taking the cleanup action on
# it that is specified in the FieldActions data frame.  The net result is that a bunch of
# duplicated and/or unneeded fields are deleted, fields with uninterpretable (coded) column 
# names tied to their source are renamed to be descriptive, and the Year column is forced 
# to an integer.  This was done to automate the cleaning process and make the code more compact.

for (fieldcounter in names(CleanedData)) {
  action_row <- FieldActions[FieldActions$FieldName == fieldcounter, ]
  if (nrow(action_row) == 0) {
    next  # No action specified for this field
  }
  WhatToDo <- action_row$Action # Read the field's action and act accordingly
  if (WhatToDo == "Delete") {
    CleanedData[[fieldcounter]] <- NULL # Drop
    
  } else if (WhatToDo == "Rename") {
    names(CleanedData)[names(CleanedData) == fieldcounter] <- action_row$RenamedTo # Rename 
    
  } else if (WhatToDo == "Make Integer") {
    CleanedData[[fieldcounter]] <- as.integer(CleanedData[[fieldcounter]]) # Convert to integer
  }
}
# Now, pull in the Income Group Data, and join it to the raw data by CountryCode
CleanedData <- left_join(CleanedData, IncomeData, by = c("CountryCode" = "Country Code"))

# Now for initial EDA purposes, we am only going to consider the two wellbeing indexes
# which are tagged as PrimaryResponse fields.  So drop the other more granular Response fields

# Select field names to keep
# First, all the renamed fields that weren't dropped and aren't a Response
PrimaryFields <- FieldActions %>%
  filter(Type %in% c("PrimaryResponse","Predictor","Identifier"),
         is.na(Action) | Action != "Delete") %>%
  pull(RenamedTo)
# Now add the Income Group field (since that was joined from a different data set)
PrimaryFields <- c(PrimaryFields, "Income Group")
# Subset the cleaned dataset using that list of fields
FirstCutData <- CleanedData %>%
  select(all_of(PrimaryFields))

# Done with cleaning so save the data for Preprocessing
saveRDS(FirstCutData, file = here::here("Data", "FirstCutData.rds"))

##############################################
#
# Exploratory Analysis starts here
#
# Summarize the data
SummaryData <- summary(FirstCutData)

# This code block breaks up the Summary data into chunks with 8 columns
# per chunk, and displays each chunk as a separate table.  This was done 
# so that the Summary table can be cleanly displayed
#
cols <- ncol(SummaryData)
cols_per_table <- 8

# Loop through each block of 8
for (i in seq(1, cols, by = cols_per_table)) {
  this_section <- SummaryData[, i:min(i + cols_per_table - 1, cols)] # create sub-table
  
  # Print the subtable using kable to make it look nice
  cat(
    asis_output(
      kable(
        this_section,
        format = "latex",
        booktabs = TRUE,
        caption = paste0("Summary Statistics (Variables ", i, " to ", 
                         min(i + cols_per_table - 1, cols), ")"),
        label = paste0("SummaryData_", i)  # <-- move label here
      ) %>%
        kable_styling(
          latex_options = c("HOLD_position", "striped", "scale_down")
        )
    )
  )
}

# Show summary statistics using skim 
skim(FirstCutData)

# Display a scatterplot of Internet Access versus HDI with trendline and R-squared
ggplot(FirstCutData, aes(x = InternetUsersPct, y = HDI_Index)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  stat_poly_eq(
    aes(label = after_stat(rr.label)),
    formula = y ~ x,
    parse = TRUE,
    output.type = "expression",
    rr.digits = 3,
    label.x = 0.6,           # X-position (adjust if needed)
    label.y = 0.15,         # Y-position (bottom-ish)
    color = "darkred",      # Match regression line
    na.rm = TRUE
  ) +
  labs(
    title = "HDI vs. Internet Access (All Countries)",
    x = "Internet Access (% of Population)",
    y = "Human Development Index (HDI)"
  ) +
  theme_minimal()

# Same plot, but break out data, color differently with separate trendlines and R-squareds,
# by Income Group
FirstCutDataFiltered <- FirstCutData[!is.na(FirstCutData$`Income Group`), ]
FirstCutDataFiltered$`Income Group` <- factor(
  FirstCutDataFiltered$`Income Group`,
  levels = c("Low income", "Lower middle income", "Upper middle income", "High income")
)
ggplot(FirstCutDataFiltered, aes(x = InternetUsersPct, y = HDI_Index, color = `Income Group`)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(
    aes(
      label = after_stat(rr.label),
      group = `Income Group`,
      color = `Income Group`
    ),
    formula = y ~ x,
    parse = FALSE,
    output.type = "text",
    rr.digits = 3,
    label.x = 0.6,
    label.y = c(0.15, 0.2, 0.25, 0.3),  # Stack in bottom right
    na.rm = TRUE
  ) +
  labs(
    title = "HDI vs. Internet Access by Income Group",
    x = "Internet Access (% of Population)",
    y = "Human Development Index (HDI)",
    color = "Income Group"
  ) +
  theme_minimal()

# Show distribution of data by Internet access in 10% increments, by Income Group
FirstCutDataFiltered <- FirstCutData[!is.na(FirstCutData$`Income Group`), ]
FirstCutDataFiltered$`Income Group` <- factor(
  FirstCutDataFiltered$`Income Group`,
  levels = c("Low income", "Lower middle income", "Upper middle income", "High income")
)

FirstCutDataFilteredClean <- FirstCutDataFiltered %>%
  filter(!is.na(InternetUsersPct))

ggplot(FirstCutDataFilteredClean, aes(x = InternetUsersPct, fill = `Income Group`)) +
  geom_histogram(
    aes(y = after_stat(count / tapply(count, PANEL, sum)[as.integer(PANEL)])),
    binwidth = 5,
    color = "black",
    alpha = 0.8
  ) +
  facet_wrap(~ `Income Group`, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Distribution of Internet Access by Income Group",
    x = "Internet Access (% of Population)",
    y = "Percent of Observations"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
# 
#There appears to be a severe left skew problem - we should isolate how much of that is clustered 
# at or near 0 to see if we need to transform it for modeling

# Step 1: Filter to only rows with Internet Access under 5%

# Bin and summarize within the 0%–5% range
zoom_data <- FirstCutDataFiltered %>%
  filter(InternetUsersPct >= 0, InternetUsersPct < 5) %>%
  mutate(Bin = cut(
    InternetUsersPct,
    breaks = seq(0, 5, by = 1),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0–1%", "1–2%", "2–3%", "3–4%", "4–5%")
  ))

# Group totals for each Income Group (full data)
group_totals <- FirstCutDataFiltered %>%
  count(`Income Group`, name = "GroupTotal")

# Bin counts and percentages
zoom_summary <- zoom_data %>%
  count(`Income Group`, Bin, name = "BinCount") %>%
  left_join(group_totals, by = "Income Group") %>%
  mutate(Percent = BinCount / GroupTotal)

# Plot
ggplot(zoom_summary, aes(x = Bin, y = Percent, fill = `Income Group`)) +
  geom_col(color = "black", width = 0.9) +
  facet_wrap(~ `Income Group`, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Zoomed View: Internet Access Below 5% by Income Group",
    x = "Internet Access (% of Population)",
    y = "Percent of Observations"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  ) 

# Violin plot of HDI distribution by income group
ggplot(FirstCutDataFiltered, aes(x = `Income Group`, y = HDI_Index, fill = `Income Group`)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3, color = "black", size = 1) +
  labs(
    title = "Distribution of HDI by Income Group",
    x = "Income Group",
    y = "Human Development Index (HDI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
Sys.sleep(2) # Wait for complicated rendering to finish

#  Violin plot of Internet access by Income Group
ggplot(FirstCutDataFiltered, aes(x = `Income Group`, y = InternetUsersPct, fill = `Income Group`)) +
  geom_violin(trim = TRUE, color = "black", adjust = 1.2) +  # trim removes tails beyond the data
  geom_jitter(width = 0.2, size = 1, alpha = 0.5, color = "black") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +  # Hard clip y-axis
  labs(
    title = "Distribution of Internet Access by Income Group",
    y = "Internet Access (% of Population)",
    x = "Income Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "Low income" = "#F8766D",
    "Lower middle income" = "#7CAE00",
    "Upper middle income" = "#00BFC4",
    "High income" = "#C77CFF"
  ))
Sys.sleep(2) # Wait for rendering

# Now we look at correlations to see strenght of hypothesis and find colinearity

# Step 1: Select only numeric columns and drop unwanted ones
numeric_vars <- FirstCutData %>%
  select(where(is.numeric)) %>%
  select(-year)  # Drop the 'Year' column

# Step 2: Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs", method = "pearson")
cor_matrix_90 <- ifelse(abs(cor_matrix) >= 0.9, cor_matrix, NA)
cor_matrix_70_90 <- ifelse(abs(cor_matrix) >= 0.7 & abs(cor_matrix) < 0.9, cor_matrix, NA)

# Step 3: Plot correlation heatmap for ALL variables
corrplot(cor_matrix,
         method = "color",
         type = "lower",
         tl.col = "black",
         tl.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE)
title("Correlation Heatmap of Numeric Predictors - ALL", line = 1, cex.main = 1.2)

# Step 4: Plot only strong correlations (90%+)
corrplot(cor_matrix_90,
         method = "color",
         type = "lower",
         tl.col = "black",
         tl.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE,
         na.label = " ")  # blank for missing (non-displayed) values
title("Correlation Heatmap of Numeric Predictors - 90%+ correlation (DROP)", 
      line = 1, cex.main = 1.2)

# Step 5: Plot somewhat strong correlations (70%-90%)
corrplot(cor_matrix_70_90,
         method = "color",
         type = "lower",
         tl.col = "black",
         tl.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         diag = FALSE,
         na.label = " ")  # blank for missing (non-displayed) values
title("Correlation Heatmap of Numeric Predictors - 70%-90%+ correlation (VIF)", 
      line = 1, cex.main = 1.2)

# Now we will display the correlation matrix numerically

# Step 1: Compute Pearson correlation matrix using pairwise complete observations
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs", method = "pearson")

# Step 2 Print it out, using the same "break into chunks" method and kable to make it pretty

cols <- ncol(cor_matrix)
col_names <- colnames(cor_matrix)

# Loop through and print in chunks
for (i in seq(1, cols, by = cols_per_table)) {
  # Subset the matrix
  end_col <- min(i + cols_per_table - 1, cols)
  sub_matrix <- cor_matrix[, i:end_col]
  
  # Print each chunk with a dynamic caption
  cat(
    asis_output(
      kable(
        round(sub_matrix, 2), 
        format = "latex", 
        booktabs = TRUE,
        caption = paste0("Correlation Matrix: Columns ", i, " to ", end_col)) %>%
        kable_styling(latex_options = c("HOLD_Position", "striped", "scale_down"))
    )
  )
}

#
# Now let's look at missingness by creating a "stoplight color0-schemed" heatmap for all
# variables by year as a starting point for our imputation and dropping efforts

# Step 1: Compute % missing per variable per year
missing_heatmap_data <- FirstCutData %>%
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

# Now show a cut by variable
FirstCutData %>%
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

# Now show a cut by Year
FirstCutData %>%
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

