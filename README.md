# Data Mining Health Expenditures
### This data mining project was created for the CIS / STA 3920 class (section PMWA) of Fall 2019 at CUNY - Baruch College. The project is an exploration of health expenditures in the United States and selected associated predictors of interest using the R programming language.

---
## Dataset Description
* The dataset was originally obtained from the Medical Expenditure Panel Survey (MEPS), conducted by the U.S. Agency of Health Research and Quality. MEPS is a comprehensive survey of the U.S. civilian population that includes detailed breakdowns of respondent assets, health expenditures, sources of payment, and insurance coverage as well as a multitude of scalar rating questions about personal conditions.

* A random sample of 2,000 individuals (observations) with 28 variables was selected out of 18,735 individuals between the ages of 18 and 65 from panels 7 and 8 of the 2003 MEPS survey. This sample is hosted by the Wisconsin School of Business on their [“Regression Modeling with Actuarial and Financial Applications” sub-site](https://instruction.bus.wisc.edu/jfrees/jfreesbooks/Regression%20Modeling/BookWebDec2010/data.html).

* Some variable names were not properly defined or were unnecessarily represented by their original text and an arbitrary index column. The index columns have been removed and the data definitions and variable names have been updated to more closely match the original MEPS source. This modified dataset, currently hosted in this GitHub repository under the filename `Health Expenditures (Modified).csv`, is the dataset used for analysis in this project. The set remains at 2,000 observations, but has 22 distinct variables after the removal of 6 columns containing duplicate information (arbitrary indexes).

* There are 2 response variables (EXPENDIP and EXPENDOP), which, along with 4 predictors (AGE, FAMSIZE, COUNTIP, and COUNTOP), are discrete while the remaining 16 predictors are categorical. The predictors consist of mostly demographic information and some self-rating questions relevant to health.

---
## Research Question
* How do demographics, regional characteristics, and self-ratings of mental and physical health correlate to the amounts spent on inpatient and outpatient medical visits?

---
## Methods of Analysis
* A combination of regression and classification was applied to the dependent variables EXPENDIP and EXPENDOP. Originally, `EXPENDIP` and `EXPENDOP` were going to be classified by zero and non-zero expenditures with regressions performed on the entire dataset as well as on only non-zero expenditures for expenditure predictions for ratemaking. Several revisions were made via trial and error as well as additional research on methodologies and other insurance related data mining projects:
    * The models for the multi-linear regressions were reassigned to use the entire dataset with variations in the predictors only. The resulting (6) sets are labeled before each model in the multi-linear regression sections of the code.
    * The separation of zero and non-zero expenditures was used solely in the logistic regression models; accordingly, `EXPENDIP` and `EXPENDOP` were coerced to factors based on these conditions. This is particularly useful for the zero-expenditure individuals as they have the least risk for ratemaking and would be optimal customers to identify as much about as possible.

* I originally intended to use clustering or KNN to identify potential correlations of common characteristic factors (demographics, self-rating questions, etc.) with expenditure amounts. As I spent significant amounts of time researching these methodologies and attempting to implement them, I came to the conclusion that they were too difficult or impossible to feasibly implement and interpret on this dataset due to the complexity of using distance functions categorical predictors, of which this dataset consists almost entirely of.
    * The aforementioned multi-linear regressions were used to determine correlations of predictors to a lesser degree in addition to their role in creating predictive ratemaking models.
    * The desired visual grouping of correlated variables through clustering or KNN was instead achieved using the custom `plotFactors(...)` function which plotted two quantitative variables (one of the response variables on the Y-axis and a predictor on the X-axis) with the observations segemented by one of the categorical / factor variables. The function produces several incomprehensible and useless plots, but this is an intended feature as it is meant for creating batches of plots at once that can then be selected from through visual inspection.

---
## Practical Implications of Examining the Dataset and Research Question
* The primary purpose of analyzing the dataset is to create accurate predictive ratemaking models based on previous counts of, and amounts spent on, health-related visits as well as demographic indicators. Another purpose is to identify customers with the least risk of requiring insurance payouts so that a low-risk customer profile can be created and used for targeted marketing efforts.

---
## Changes That Could Be Made to Improve the Project
* During the process of researching for this project and after its completion, I learned a few other methods of analysis that were not covered until the final weeks of the semester or at all. These methods could have potentially benefited this particular dataset as most of the methods learned in this course were intended for use with datasets consisting largely of quantitative predictors and responses.
    * Specifically, association rule mining could have been used in addition to the logistic regression for a more robust classification section.
    * Regression methods with high potential for improving the accuracy of the ratemaking model achieved via multi-linear regression include lasso, ridge, and smooth-splines.
* Selecting a larger sample of observations from the original MEPS survey would greatly ease the difficulties encountered with this particular dataset as the low number of observations were subject to issues of high leverage and skewdness. Additionally, a selection of more useful predictors (quantitative and self-survey scalar questions) out of the many available in the original survey panels would likely lead to much more useful conclusions and more accurate models as most of the predictors were statistically insignificant and largely irrelvant overall.