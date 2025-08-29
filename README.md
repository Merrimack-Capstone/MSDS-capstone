# Project Title
Bridging the Digital Divide for the Next Billion Users: Can we predicting changes in overall wellbeing and national economic health based on digital inclusion?Will Bridging The Digital Divide Bring Prosperity To The Second “Next Billion 
Users” And Their Home Countries?

## Abstract
History has shown that disadvantaged populations experience improved wellbeing when infrastructure improvements impact their lives.   A diverse set of stakeholders, including governments, aid organizations and private sector technology companies,  would benefit from being able to quantify (at minimum) and predict (ideally) the extent to which expanded access to the internet is one such infrastructure improvement that leads directly to higher population wellbeing.   This report describes in depth the application of statistical and machine-learning based predictive analytics to both correlate and isolate causal links between access to the internet and population wellbeing.   Using 30 years worth of data, consisting of a subset of raw, transformed and abstracted data vectors from the World Bank (for inputs that could influence wellbeing),  and the United Nations (for the Human Development Index chosen as the basis for measuring wellbeing), this study demonstrates that the correlation between internet access and population wellbeing is positive and statistically relevant.  This study also demonstrates that, while it is challenging to firmly quantify a causal relationship between internet access and wellbeing, well-performing broader models that predict wellbeing rely on internet access as an important positive factor in predicted wellbeing.  Finally,this study shows that investing in broadening access to (ideally, renewable) electricity in developing countries is arguably the MOST important factor influencing wellbeing, as well as a necessary precursor to broadening internet access as a wellbeing growth accelerant.   These findings can serve as support for prescriptive strategy for both private and public sector entities.  Furthermore, because the data acquisition modules and predictive models that are the backbone of this study are built on public data and can be updated dynamically as new data becomes available, entities relying on these models to support their strategy can update the results regularly and make interpretative course-corrections as indicated.

## Technical Details

###_Code blocks commented with "REC" were written by Ray Chandonnet, code blocks commented with "BJV" were written by Barrett Viator

### Code Modules written in R

#### IMPORTANT:  Setting Up relative file references
Most of the R modules import / export data to/from the /Data folder using the here:here()function.   Before opening any code models, you must open the R project named "MSDS-capstone.Rproj" found in the root folder of the Github repository.   This will ensure that \Data is referenced off the Github root and not the user's local machine.

#### Code Modules and Purpose

_API Data Retrieval.R:_ Retrieves the Raw Data from The World Bank and from UNDP via API calls, does some light cleaning, and saves it in an RDS file (PreEDA_DataFrame) for further use.   **THis takes up to an hour to process so do not run unless you want updated data**

_Exploratory Data Analysis.R:_  
1) Retrieves the raw data (PreEDA_DataFrame) created by the most recent API Data Retrieval run
2) Retrieves the set of field-by-field cleaning actions "roadmap" we created to automate cleaning, saved in "FieldActions.xlsx"
3) Retrieves the Income Group Data Excel file that was downloaded from the World Bank website
4) Performs heavy duty cleaning based on a cleaning roadmap we created and saved in : and performs all EDA for the project

_Preprocessing.R:_  Retrieves the raw data that's in the RDS file created by the most recent API Data Retrieval run saved in the Data folder) and performs all EDA for the project


