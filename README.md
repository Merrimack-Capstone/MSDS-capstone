# Working Project Title
Bridging the Digital Divide for the Next Billion Users: Can we predicting changes in overall wellbeing and national economic health based on digital inclusion?Will Bridging The Digital Divide Bring Prosperity To The Second “Next Billion 
Users” And Their Home Countries?

## Background
The Rural Electrification Act (REA) of 1936, a cornerstone of President Roosevelt's "New Deal," successfully brought electricity and infrastructure to underserved areas of the United States. This strategic investment  proved to be a “powerful economic springboard, propelling America to its post-war economic dominance” (Taylor, 2024). This historical success offers a compelling parallel to the current global challenge and opportunity presented by the expansion of internet connectivity and digital technologies, particularly in addressing long-standing disparities in economic participation, well-being, and education worldwide.

Today, the "digital divide" extends beyond basic internet access, encompassing disparities in "Advanced Meaningful Connectivity” which is defined as daily access via mobile devices with unlimited data and at least 4G speeds (A4AI, 2022) . For many in the Global South, the prohibitive cost of an entry-level mobile device which often exceeds 30% of monthly income perpetuates "adverse digital incorporation," limiting access to vital information, education, and economic opportunities (GDIP, 2022., Heeks, 2022) . The COVID-19 pandemic highlighted the transformative potential of increased digital access, enabling individuals to gain health knowledge, stay safe, and acquire crucial skills for civic engagement and economic participation (Kloza, 2023).

The multifaceted challenges faced by those who live with “adverse digital incorporation” are heightened by factors like insufficient education, low digital literacy, and unreliable infrastructure. Initiatives from organizations like Google's "Next Billion Users," IEEE, and A4AI, are crucial to bringing these individuals into more modern economies. These efforts emphasize user research in low and middle-income countries to develop inclusive digital tools that accommodate limitations like older devices and high data costs. Ultimately, fostering widespread, affordable internet access is not just about market expansion. Doing so is a humanitarian initiative that can dismantle barriers, improve individual well-being, and drive socio-economic development on a global scale.

The initial phase of our analysis focused on intensive exploratory data analysis to ensure the quality and suitability of the raw datasets. Moving forward we will proceed with the handling of missing values, reducing dimensionality via PCA, and addressing multicolinearity through VIF. Outlier detection and treatment are also crucial to the success of our modeling efforts. Utilizing the interquartile range (IQR) to identify and cap extreme values in key numerical columns will mitigate their disproportionate influence on model performance. Data types will need to be assessed to convert to appropriate formats for modeling which will ensure consistent handling of numerical and categorical data throughout the following modeling stages.

Feature engineering will then be systematically applied to enhance the predictive power of our dataset by transforming existing variables and creating new binned ones to provide deeper insights into the various related columns and to reduce noise that may impact model performance. 

In order to prepare the features for machine learning models, scaling and encoding techniques will be applied. Numerical features often appear with varying scales will undergo standardization using StandardScaler to ensure that no single feature dominates the machine learning process. One-hot encoding will be utilized for nominal categorical variables that do not exhibit an inherent order by creating columns for each unique category and preventing unintended ordinal relationships.

This comprehensive pre-processing and feature engineering pipeline ensures that the data is properly cleaned and well-structured for predictive modeling and other machine learning algorithms. This creates a stronger foundation for the subsequent model training and evaluation phases to test our hypothesis and to compare the relationship of the results to our predictions.


## Research question
We are posing the following research question:  To what extent has growth in internet access of populations influenced population wellbeing in more developed nations, and can that be used to predict improvements in wellbeing and economic progress among global minority populations (the "next billion users")? 


## Hypothesis & Predictions

Hypothesis 

Increased access to the internet in the developing world is a measurably material contributing factor to improved population wellbeing - so much so that we may be able to predict how much wellbeing will improve based on increased digital inclusion.  


Prediction

We expect that digital inclusion will be strongly correlated to education, which is a component of wellbeing, and moderately correlated with overall wellbeing.  We expect that PREDICTING the rate of improvement in wellbeing based on internet inclusion alone will be extremely challenging due to the number of other factors influencing wellbeing, but hope to be able to isolate its impact on wellbeing.  We expect that clustering may help lead to better predictive performance, if we have time to get to it.


