# Project Title
Bridging the Digital Divide for the Next Billion Users: Can we predicting changes in overall wellbeing and national economic health based on digital inclusion?Will Bridging The Digital Divide Bring Prosperity To The Second “Next Billion 
Users” And Their Home Countries?

## Authors
Raymond E. Chandonnet - _chandonnetr@merrimack.edu_  
Barret J. Viator - _viatorb@merrimack.edu_

## Abstract
History has shown that disadvantaged populations experience improved wellbeing when infrastructure improvements impact their lives.   A diverse set of stakeholders, including governments, aid organizations and private sector technology companies,  would benefit from being able to quantify (at minimum) and predict (ideally) the extent to which expanded access to the internet is one such infrastructure improvement that leads directly to higher population wellbeing.   This report describes in depth the application of statistical and machine-learning based predictive analytics to both correlate and isolate causal links between access to the internet and population wellbeing.   Using 30 years worth of data, consisting of a subset of raw, transformed and abstracted data vectors from the World Bank (for inputs that could influence wellbeing),  and the United Nations (for the Human Development Index chosen as the basis for measuring wellbeing), this study demonstrates that the correlation between internet access and population wellbeing is positive and statistically relevant.  This study also demonstrates that, while it is challenging to firmly quantify a causal relationship between internet access and wellbeing, well-performing broader models that predict wellbeing rely on internet access as an important positive factor in predicted wellbeing.  Finally,this study shows that investing in broadening access to (ideally, renewable) electricity in developing countries is arguably the MOST important factor influencing wellbeing, as well as a necessary precursor to broadening internet access as a wellbeing growth accelerant.   These findings can serve as support for prescriptive strategy for both private and public sector entities.  Furthermore, because the data acquisition modules and predictive models that are the backbone of this study are built on public data and can be updated dynamically as new data becomes available, entities relying on these models to support their strategy can update the results regularly and make interpretative course-corrections as indicated.

## Reports

Written reports on the various stages of this project can be found in the Reports folder of this repository.  They include:

1) Preliminary Project Proposal.docx
2) Final Project Proposal
3) Cleaning and EDA Report.pdf
4) Preprocessing and Initial Model Report.pdf
5) Final Report.pdf

Their contents are self-explanatory given the file names.   They are listed in the order they were produced, which shows the evolution of the project over time.    
The reader may prefer to start with the Final Report and then consult the other reports for more detail as desired. 

## Technical Details

##### _Code blocks commented with "REC" were written by Ray Chandonnet, code blocks commented with "BJV" were written by Barrett Viator_

### Code Modules written in R

#### IMPORTANT:  Setting Up relative file references
Most of the R modules import / export data to/from the /Data folder using the here:here()function.   Before opening any code models, you must open the R project named "MSDS-capstone.Rproj" found in the root folder of the Github repository.   This will ensure that \Data is referenced off the Github root and not the user's local machine.

#### Dependendencies

The R modules below use some or all of the following packages:

broom, corrplot, dplyr, ggcorrplot, ggplot2, ggpmisc, here, kableExtra, knitr, MASS, Matrix, parsnip, plm, readxl, recipes, reticulate, rlang, scales, skimr, stringr, tidymodels, tidyrhttps://github.com/Merrimack-Capstone/MSDS-capstone/blob/main/README.md, tidyverse, writexl, xgboost, zoo

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
5) Saves the cleaned data in FirstCutData.rds in the Data folder, for retrieval and use by Preprocessing code module
6) Performs all Exploratory Analysis (EDA) for the project

#### _Preprocessing.R:_  

1) Retrieves the cleaned data file FirstCutDasta.slsx (saved in the Data folder)
2) performs all preprocessing, feature engineering, transformation, splitting, and dimension reduction
3) Performs Principal Component Analysis (PCA) and adds top 5 PC's to data set
4) Saves training and test data in train_data.rds and test_data.rds respectively to be ingested into any predictive model code modules

Custom functions:  
  a) _wait_to_render_: instructs system to pause a given parameter's number of seconds to allow for graphics rendering before continuing  
  b) _OneCountryMissingness_:  Produces a data missingness heatmap for a given country, with the data and country passed as parameters  
  c) _OverallMissingnessHeatmap_:  Produces a data missingness heatmap for all the data, by variable and year  
  d) _MissingnessByVariable_:  Produces an overall data missingness chart by Variable  
  e) _MissingnessByYear_: Produces an overall data missingness chart by Year  
  f) _MissingnessByCountry_: Produces an overall data missingness chart by Country, for countries with missingness over a threshold parameter  
  g) _FourPlots_:  Generates the four primary missingness data plots / heatmaps  
  h) _PlotUHC_:  Plots the variable UCHServiceCoverage (averaged) by year - a variable that requires iterative imputation  
  i) _PlotSlums_: Plots the variable UCHServiceCoverage (averaged) by year - a variable that requires iterative imputation  
  j) _Yeo_Johnson_:  Perform's Yeo-Johnson transformation of the specified training and test data  
  k) _impute_time_trend_ :  Imputes missing data for a given variable, by country, using time trend fitting; Parameters include the data set, the variable name to be imputed abnd a floor and cap to be observed on the imputed values  

#### _Two Variable Models.R:_  

1) Ingests the training and testing data
2) Builds and evaluates multiple two-variable predictive models (linear and logistic regression)

Custom functions:  
    a) _print_regressions_: displays two plots that evaluate the predictions on a test set using a given model, with different chart title bawsed on the parameters passed

#### _XGBoost Models.R:_  

1) Ingests the training and testing data
2) Builds and evaluates two XGboost model (quantitative predictor and classification)

#### _Model Refinements.R:_  

1) Ingests the training and testing data
2) Examines the relationship between internet access and "Change in HDI"
3) Measures level of autocorrelation in the data set
4) Builds and evaluates panel regression model

### Code Modules written in Python

#### IMPORTANT:  Setting Up relative file references
For Python code, relative file paths are handled using the pyhere package. As long as this repo is cloned properly, the code will automatically locate the /Data folder based on the project root. 

#### Dependendencies

The Python modules below use some or all of the following packages:

pandas, numpy, pyhere, tensorflow, scipy, scikit-learn, wbgapi, requests, pycountry, openpyxl, and matplotlib.

#### Code Modules and Purpose

#### _API Data Retrieval.ipynb:_  
This code retrieves World Bank and UNDP data via API calls.   This code can be run on its own to retrieve a dataset;  
However, it was used to write and test code that was then run in R using Reticulate.  We include this file as background only.

#### _FFNN Final Report_  
This code builds and evaluates feed forward neural network prediction models.   These models did not make the final cut but were referenced in the final report. 

#### _FFNN Hyperparameter Grid Version.ipynb_
This code builds and evaluates feed forward neural network prediction models iteratively using hyperparameter grid and tuning loops. While the results are promising, the validation error plot shows great volatility, the approach is computationally intense and interpretability is poor ("black box") relative to the XGboost models.  We include here as a basis for additional exploration


