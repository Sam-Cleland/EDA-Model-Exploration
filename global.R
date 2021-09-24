library(shiny)
library(shinyjs)
library(corrgram)
library(visdat)
library(tidyverse)
library(ggplot2)
library(DT)
library(GGally)
library(plotly)
library(naniar)
library(shinycssloaders)
library(summarytools)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(caret)
library(recipes)
library(glmnet)
library(Metrics)
library(car)

dat = read.csv("covid_data.csv", header = TRUE, na.strings = c("NA", "--"), stringsAsFactors = TRUE) # read in COVID-19 data file
dat$CODE = seq.int(nrow(dat)) # CODE has high cardinality so convert to integer ID
dat[dat == -99] = NA # convert -99 values to NA values

# Create lists for variable options when using user interface options
choices = c("HEALTHCARE_BASIS", "GOVERN_TYPE", "POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", "HEALTHCARE_COST", "DEATH_RATE")
choices_cat = c("HEALTHCARE_BASIS", "GOVERN_TYPE")
choices_quant = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", "POP_DENSITY", "GDP", "INFANT_MORT", "DOCS", "VAX_RATE", "HEALTHCARE_COST", "DEATH_RATE")





