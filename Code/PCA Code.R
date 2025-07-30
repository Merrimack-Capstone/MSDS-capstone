library(tidyverse)
library(FactoMineR)
library(factoextra)

# Step 1: Exclude non-predictors
pca_data <- FirstCutData %>%
  select(-HDI_Index, -IHDI_Index, -GiniCoeff, -CountryCode, -year)

# Step 2: Keep only numeric columns
pca_numeric <- pca_data %>%
  select(where(is.numeric))

# Step 3: Remove columns that are constant or all NA
pca_numeric <- pca_numeric %>%
  select(where(~ !all(is.na(.)) && sd(., na.rm = TRUE) > 0))

# Step 4: Remove rows with any missing data
pca_numeric <- drop_na(pca_numeric)

# Step 5: Run PCA
pca_result <- prcomp(pca_numeric, scale. = TRUE)

# Step 6: Visualize
fviz_pca_ind(pca_result,
             geom.ind = "point",
             label = "none",
             addEllipses = FALSE) +
  labs(title = "PCA of Development Indicators (Excluding Year and Responses)") +
  theme_minimal()


# Identify constant or all-NA columns
bad_cols <- pca_numeric %>%
  summarise(across(everything(), ~ sd(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "sd") %>%
  filter(is.na(sd) | sd == 0)

print(bad_cols)
