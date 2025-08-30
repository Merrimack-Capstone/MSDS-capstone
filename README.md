# Project Title
Bridging the Digital Divide for the Next Billion Users: Can we predicting changes in overall wellbeing and national economic health based on digital inclusion?Will Bridging The Digital Divide Bring Prosperity To The Second “Next Billion 
Users” And Their Home Countries?

## Abstract
History has shown that disadvantaged populations experience improved wellbeing when infrastructure improvements impact their lives.   A diverse set of stakeholders, including governments, aid organizations and private sector technology companies,  would benefit from being able to quantify (at minimum) and predict (ideally) the extent to which expanded access to the internet is one such infrastructure improvement that leads directly to higher population wellbeing.   This report describes in depth the application of statistical and machine-learning based predictive analytics to both correlate and isolate causal links between access to the internet and population wellbeing.   Using 30 years worth of data, consisting of a subset of raw, transformed and abstracted data vectors from the World Bank (for inputs that could influence wellbeing),  and the United Nations (for the Human Development Index chosen as the basis for measuring wellbeing), this study demonstrates that the correlation between internet access and population wellbeing is positive and statistically relevant.  This study also demonstrates that, while it is challenging to firmly quantify a causal relationship between internet access and wellbeing, well-performing broader models that predict wellbeing rely on internet access as an important positive factor in predicted wellbeing.  Finally,this study shows that investing in broadening access to (ideally, renewable) electricity in developing countries is arguably the MOST important factor influencing wellbeing, as well as a necessary precursor to broadening internet access as a wellbeing growth accelerant.   These findings can serve as support for prescriptive strategy for both private and public sector entities.  Furthermore, because the data acquisition modules and predictive models that are the backbone of this study are built on public data and can be updated dynamically as new data becomes available, entities relying on these models to support their strategy can update the results regularly and make interpretative course-corrections as indicated.

## Technical Details

##### _Code blocks commented with "REC" were written by Ray Chandonnet, code blocks commented with "BJV" were written by Barrett Viator_

### Code Modules written in R

#### IMPORTANT:  Setting Up relative file references
Most of the R modules import / export data to/from the /Data folder using the here:here()function.   Before opening any code models, you must open the R project named "MSDS-capstone.Rproj" found in the root folder of the Github repository.   This will ensure that \Data is referenced off the Github root and not the user's local machine.

#### Dependendencies

The R modules below use some or all of the following packages:

broom, corrplot, dplyr, ggcorrplot, ggplot2, ggpmisc, here, kableExtra, knitr, MASS, Matrix, parsnip, plm, readxl, recipes, reticulate, rlang, scales, skimr, stringr, tidymodels, tidyr, tidyverse, writexl, xgboost, zoo

If you wish to install all of these packages at once, simply copy this code snippet into R and run it, and all packages will be installed:

install.packages(c(
  "broom","corrplot","dplyr","ggcorrplot","ggplot2","ggpmisc","here",
  "kableExtra","knitr","MASS","Matrix","parsnip","plm","readxl","recipes",
  "reticulate","rlang","scales","skimr","stringr","tidymodels","tidyr",
  "tidyverse","writexl","xgboost","zoo"
))

#### Code Modules and Purpose

#### _API Data Retrieval.R:_ 

1) Retrieves the Raw Data from The World Bank and from UNDP via API calls
2) Joins data and performs light cleaning
3) Saves cleaned/joined data to an RDS file (PreEDA_DataFrame) in Data folder for ingestion by Cleaning and EDA Module 
**NOTE: This can take up to an hour to process so do not run unless you want more updated data than the base data set saved in /Data folder**

#### _Cleaning and EDA.R and Cleaning and EDA.Rmd:_ 

_A raw annotated script file is provided for interactive execution, but the Rmd file is preferred as the table outputs are designed to render using kable_

1) Retrieves the raw data (PreEDA_DataFrame) created by the most recent API Data Retrieval run
2) Retrieves the set of field-by-field cleaning actions "roadmap" we created to automate cleaning, saved in "FieldActions.xlsx"
3) Retrieves the Income Group Data Excel file that was downloaded from the World Bank website and joins it to the reset of the data
4) Performs heavy duty cleaning based on a cleaning roadmap we created and saved in FieldActions.xlsx
5) Saves the cleaned data in Excel file called FirstCutData.xlsx in the Data folder, for retrieval and use by Preprocessing code module
6) Performs all Exploratory Analysis (EDA) for the project

#### _Preprocessing.R:_  

1) Retrieves the cleaned data file FirstCutDasta.slsx (saved in the Data folder)
2) performs all preprocessing, feature engineering, transformation, splitting, and dimension reduction
3) Performs Principal Component Analysis (PCA) and adds top 5 PC's to data set
4) Saves training and test data in train_data.rds and test_data.rds respectively to be ingested into any predictive model code modules

Custom functions:
  a) wait_to_render: instructs system to pause a given parameter's number of seconds to allow for graphics rendering before continuing
  b) OneCountryMissingness:  Produces a data missingness heatmap for a given country, with the data and country passed as parameters
  c) OverallMissingnessHeatmap:  Produces a data missingness heatmap for all the data, by variable and year
  d) MissingnessByVariable:  Produces an overall data missingness chart by Variable
  e) MissingnessByYear: Produces an overall data missingness chart by Year
  f) MissingnessByCountry: Produces an overall data missingness chart by Country, for countries with missingness over a threshold parameter
  g) FourPlots:  Generates the four primary missingness data plots / heatmaps
  h) PlotUHC:  Plots the variable UCHServiceCoverage (averaged) by year - a variable that requires iterative imputation
  i) PlotSlums: Plots the variable UCHServiceCoverage (averaged) by year - a variable that requires iterative imputation
  j) Yeo_Johnson:  Perform's Yeo-Johnson transformation of the specified training and test data
  k) impute_time_trend :  Imputes missing data for a given variable, by country, using time trend fitting; Parameters include the data set, the variable name to be imputed abnd a floor and cap to be observed on the imputed values

#### _Two Variable Models.R:_  

1) Ingests the training and testing data
2) Builds and evaluates multiple two-variable predictive models (linear and logistic regression)

#### _XGBoost Models.R:_  

1) Ingests the training and testing data
2) Builds and evaluates two XGboost model (quantitative predictor and classification)

#### _Model Refinements.R:_  

1) Ingests the training and testing data
2) Examines the relationship between internet access and "Change in HDI"
3) Measures level of autocorrelation in the data set
4) Builds and evaluates panel regression model
