
shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Exploratory Data Analysis and Model Exploration"), # App title 
    
    navlistPanel("Navigation Panel", widths=c(2,8), # create navigation panel for tabs
                 #---------------------------------------------------
                 # Data Summary Navlist
                 #---------------------------------------------------
                 tabPanel("Data Summary",
                          tabsetPanel(type="tabs",
                                      #---------------------------------------------------
                                      # Data Explorer Tab
                                      #---------------------------------------------------
                                      tabPanel(
                                        h3("Data Explorer"),
                                        DT::dataTableOutput(outputId = "raw"), # display DT data table
                                      ),
                                      #---------------------------------------------------
                                      # Data Summary Tab
                                      #---------------------------------------------------
                                      tabPanel(
                                        h3("Summary"),
                                        verbatimTextOutput(outputId = "SummaryA1"), # display data summary
                                      )
                          ),
                 ),
                 #---------------------------------------------------
                 # Data Visualisations Navlist
                 #---------------------------------------------------
                 tabPanel("Data Visualisations", # create tab for missing values
                          tabsetPanel(type="tabs",
                                      #---------------------------------------------------
                                      # Missing Values Tab
                                      #---------------------------------------------------
                                      tabPanel("Missing Values",
                                               withSpinner(
                                                 plotOutput(outputId = "misval") # display missing value chart
                                               ),# tick box for option to cluster chart according to variable missingness
                                               checkboxInput(inputId = "missclust", label = "Sort according to missingness:", value = FALSE), 
                                               # tick box for option to arrange chart according to variable missingness
                                               checkboxInput(inputId = "misssort", label = "Arrange in order of missingness:", value = FALSE),
                                               withSpinner(
                                                 plotOutput(outputId = "misvalsum") # plot missing values chart 
                                               )
                                      ),
                                      #---------------------------------------------------
                                      # Box Plot Tab
                                      #---------------------------------------------------
                                      tabPanel( "Box Plot", # create tab for box plot
                                                withSpinner(
                                                  plotOutput(outputId = "box") # display box plots
                                                ),
                                                # drop down menu to select which numeric variables to plot
                                                selectInput(inputId = "variablebox", label = "Show variables:", choices = choices_quant, multiple = TRUE, selected = choices_quant),
                                                # tick box option to standardise data
                                                checkboxInput(inputId = "boxstandardise", label = "Show Standardized", value = TRUE),
                                                # tick box option to show outliers
                                                checkboxInput(inputId = "boxoutliers", label = "Show Outliers", value = TRUE),
                                                # tick box option to label outliers
                                                checkboxInput(inputId = "labeloutlier", label = "Label Outliers", value = FALSE),
                                                # slider option to select IQR multilplier
                                                sliderInput(inputId = "boxrange", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
                                      ),
                                      #---------------------------------------------------
                                      # Correlation Tab
                                      #---------------------------------------------------
                                      tabPanel("Corelation", # create tab for correlation
                                               withSpinner(
                                                 plotOutput(outputId = "corr") # display correlation chart
                                               ),
                                               # drop menu to select which numeric variables to plot
                                               selectizeInput(inputId = "VariablesB", label = "Show variables:", choices = choices_quant, multiple = TRUE, selected = choices_quant),
                                               # tick box option to use absolute correlation
                                               checkboxInput(inputId = "corrabs", label = "Uses absolute correlation", value = TRUE),
                                               # drop down menu to select which correlation method to use
                                               selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                               # drop down menu to select which grouping method to use
                                               selectInput(inputId = "corrGroup", label = "Grouping method", choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), selected = "OLO")
                                      ),
                                      #---------------------------------------------------
                                      # Pairs Plot Tab
                                      #---------------------------------------------------
                                      tabPanel("Pairs Plot", # create tab for pairs plot
                                               withSpinner(
                                                 plotOutput(outputId = "ggpair") # display pairs plot
                                               ),
                                               # drop down menu to select which variables to plot
                                               selectInput(inputId = "variableC", label = "Show variables:", choices = choices, multiple = TRUE, selected = choices),
                                               # drop down menu to select which categorical variable to color by
                                               selectInput(inputId = "ggpair", label = "Color according to variable:", choices = choices_cat, multiple = FALSE, selected = "GOVERN_TYPE")
                                      ),
                                      #---------------------------------------------------
                                      # Bar Charts Tab
                                      #---------------------------------------------------
                                      tabPanel("Bar Charts", # create tab for bar charts
                                               # drop down menu to select which categorical variables to plot
                                               selectInput(inputId = "variablebar", label = "Show variables:", choices = choices_cat, multiple = FALSE, selected = "GOVERN_TYPE"),
                                               withSpinner(
                                                 plotOutput(outputId = "bar") # display bar chart
                                               )
                                      ),
                                      #---------------------------------------------------
                                      # Mosaic Plot Tab
                                      #---------------------------------------------------
                                      tabPanel("Mosaic Plot", # create tab for mosaic plot
                                               withSpinner(
                                                 plotOutput(outputId = "mosiac") # display mosaic plot
                                               )
                                      ),
                                      #---------------------------------------------------
                                      # Rising Value Tab
                                      #---------------------------------------------------
                                      tabPanel("Rising Value", # create tab for rising value
                                               withSpinner(
                                                 plotOutput(outputId = "rv") # display reising value chart
                                               ),
                                               # drop down menu to select which numeric variables to plot
                                               selectInput(inputId = "variablerv", label = "Show variables:", choices = choices_quant, multiple = TRUE, selected = choices_quant),
                                               checkboxInput(inputId = "rvstandardise", label = "Center and Scale Variables", value = TRUE)
                                      )
                          )
                 ),
                 #---------------------------------------------------
                 # Data Pre-Processing Navlist
                 #---------------------------------------------------
                 tabPanel( "Data Pre-Processing",
                           tabsetPanel(type="tabs",
                                       #---------------------------------------------------
                                       # Pre-Processing Tab
                                       #---------------------------------------------------
                                       tabPanel("Pre-Processing", # create tab for data pre processing
                                                h3("Select pre-processing steps:"),
                                                # slider option to choose variable missing threshold
                                                sliderInput(inputId = "VarThresh", label = "Threshold to remove variables:", min = 0, max = 1, step = 0.1, value = 0.5),
                                                # slider option to choose observation missing threshold
                                                sliderInput(inputId = "ObsThresh", label = "Threshold to remove observations:", min = 0, max = 1, step = 0.1, value = 0.5),
                                                # check box to use knn imputation to replace missing values
                                                checkboxInput(inputId = "impute", label = "Impute missing values? (unchecked will partial delete missing values)", value = TRUE),
                                                # check box option to center and scale numeric variables
                                                checkboxInput(inputId = "centerscale", label = "Center and scale numeric variables?", value = TRUE)
                                       ),
                                       #---------------------------------------------------
                                       # Train Data Tab
                                       #---------------------------------------------------
                                       tabPanel("Train Data",
                                                DT::dataTableOutput(outputId = "traindf") # display data explorer for training data
                                       ),
                                       #---------------------------------------------------
                                       # Test Data Tab
                                       #---------------------------------------------------
                                       tabPanel("Test Data",
                                                DT::dataTableOutput(outputId = "testdf") # display data explorer for testing data
                                       ),
                                       #---------------------------------------------------
                                       # Predict Missing Tab
                                       #---------------------------------------------------
                                       tabPanel("Predict Missing",
                                                h3("Predicting missing values before pre-processing steps applied."),
                                                withSpinner(
                                                plotOutput(outputId = "misstree") # plot tree for predicting missing values
                                                )
                                       )
                           )
                 ),
                 #---------------------------------------------------
                 # GLMNET Model Navlist
                 #---------------------------------------------------          
                 tabPanel("GLMNET Model",
                          tabsetPanel(type="tabs",
                                      #---------------------------------------------------
                                      # Model Summary Tab
                                      #---------------------------------------------------
                                      tabPanel("Model Summary",
                                               h3("Model summary and hyper-parameter tuning using 10-fold cross validation."),
                                               verbatimTextOutput(outputId = "model_output") # show model output for hyperparameter tuneing
                                      ),
                                      #---------------------------------------------------
                                      # Test Data Predictions Tab
                                      #---------------------------------------------------
                                      tabPanel("Test Data Predictions",
                                               withSpinner(
                                                 plotOutput(outputId = "model_predict") # plot predicted vs actual values for testing data
                                               ),
                                               verbatimTextOutput(outputId = "rmse") # show testing RMSE
                                      ),
                                      #---------------------------------------------------
                                      # Variable Importance Tab
                                      #---------------------------------------------------
                                      tabPanel("Variable Importance",
                                               withSpinner(
                                                 plotOutput(outputId = "varImp") # plot missing variable importance
                                               )
                                      ),
                                      #---------------------------------------------------
                                      # Residuals Boxplots Tab
                                      #---------------------------------------------------
                                      tabPanel("Residual Boxplots",
                                               withSpinner(
                                               plotOutput(outputId = "bugs") # plot residual box plots
                                               ),
                                               # check box to show outliers
                                               checkboxInput(inputId = "boxoutliersbox", label = "Show Outliers", value = TRUE),
                                               # slider to choose IQR multiplier
                                               sliderInput(inputId = "boxrangebox", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
                                      )
                          )
                 )
    )
  )
)






