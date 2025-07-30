library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
RawData <- readRDS(here::here("Data","PreEDA_DataFrame.rds"))
names(RawData) <- make.unique(names(RawData), sep = ".")
FieldActions <- read_excel(here::here("Data", "FieldActions.xlsx"))
CleanedData <- RawData # make a copy to preserve original

# This code iterates through every field in the raw data, taking the cleanup action on
# it that is specified in the FieldActions data frame.  The net result is that a bunch of
# duplicated and/or unneeded fields are deleted, fields with uninterpretable (coded) column 
# names tied to their source are renamed to be descriptive, and the Year column is forced 
# to an integer

for (fieldcounter in names(CleanedData)) {
  message("Processing field ", fieldcounter)
  action_row <- FieldActions[FieldActions$FieldName == fieldcounter, ]
  if (nrow(action_row) == 0) {
    next  # No action specified for this field
  }
  WhatToDo <- action_row$Action
  if (WhatToDo == "Delete") {
    CleanedData[[fieldcounter]] <- NULL
    
  } else if (WhatToDo == "Rename") {
    names(CleanedData)[names(CleanedData) == fieldcounter] <- action_row$RenamedTo
    
  } else if (WhatToDo == "Make Integer") {
    CleanedData[[fieldcounter]] <- as.integer(CleanedData[[fieldcounter]])
  }
}
# Now, pull in the Income Group Data, and join it to the raw data by CountryCode
IncomeData <- read_excel(here::here("Data", "Income Group Data.xlsx"))
CleanedData <- left_join(CleanedData, IncomeData, by = c("CountryCode" = "Country Code"))

# Now for initial EDA purposes, I am only going to consider the two wellbeing indexes
# which are tagged as my PrimaryResponse fields.  So I want to drop the other more granular
# Response fields

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

write_xlsx(FirstCutData, path = here::here("Data","FirstCutData.xlsx"))
glimpse(FirstCutData)
summary(FirstCutData)