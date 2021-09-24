# EDA-Model-Exploration
The aim of this project was to explore using shiny to do exploratory data analysis and provide options to experiment with a glmnet model.

This application is my first attempt at creating an application using shiny. There are some improvements which could be made but the aim was to get experience using shiny and the reactive programming style. This work was completed as part of an assignment for a data science in industry course. 

The data set contains information related to Covid-19 as at 2019. The data summary section and exploratory
data analysis considers the data in its raw format, except for the following processing steps. Any values of -99
are considered to be missing data, strings of ’NA’ and ’–’ are considered missing, all are substituted with a NA
value. Further steps to address missing values are taken before the modelling begins. The unique ID is a categorical
variable with high cardinality so it has been converted to a unique integer variable. The high cardinality
causes issues in some models and makes ugly labels for plots, the integer values fix both these issues. The
data set contains 10 numeric predictor variables and two categorical predictors. There is a numeric outcome
variable DEATH RATE, a categorical test-train split variable OBS TYPE, and the unique country identifier CODE.

The dataset is provided in this repository. This data was provided for an assignment I can not vouch for its origion or reliablity.

The shiny application is deployed on shinyapps.io and can be found here,  https://sam-cleland.shinyapps.io/.
