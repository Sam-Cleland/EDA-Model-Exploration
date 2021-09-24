shinyServer(function(input, output, session) {

  #---------------------------------------------------
  # Data Table Explorer for unprocessed data
  #---------------------------------------------------
  output$raw <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(dat)) # create DT data table for data exploration
  })

  #---------------------------------------------------
  # Data Summary for unprocessed data
  #--------------------------------------------------
  output$SummaryA1 <- renderPrint({
    dfSummary(dat, style='grid') # create summary of data
  })
  
  #---------------------------------------------------
  # Missing Value Plot for unprocessed data
  #--------------------------------------------------
  output$misval = renderPlot({
    vis_miss(dat, cluster = input$missclust, sort_miss= input$misssort, show_perc=TRUE) +
      labs(title = "Missing Values") # create missing values chart. Input option to sort by type of variable and cluster missing.
  })
  
  #--------------------------------------------------
  # Missing value percentages for unprocessed data
  #--------------------------------------------------
  output$misvalsum = renderPlot({ # create plot showing percentage of values missing for variables
    gg_miss_var(dat, show_pct = TRUE) + labs(title = "Percentage of Values Missing") 
  })
  
  #--------------------------------------------------
  # Boxplot of quantitative variables for unprocessed data
  #--------------------------------------------------
  output$box = renderPlot({
    # create data frame from selected variable. Input option to scale and center the variables.
    numData = scale(dat[input$variablebox], center = input$boxstandardise, scale = input$boxstandardise)
    # create boxplots. Input options to change IQR multiplier, show outliers, label outliers, and select numeric variables.
    box = car::Boxplot(y=numData, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
                 horizontal = FALSE, outline = input$boxoutliers,
                 col = brewer.pal(n = dim(dat)[2], name = "RdBu"),
                 range = input$boxrange, main = "Boxplots",
                 id = ifelse(input$labeloutlier, list(n = Inf, location = "avoid"), FALSE))
  })
  
  #--------------------------------------------------
  # Correlation chart of quantitative variables for unprocessed data
  #--------------------------------------------------
  output$corr = renderPlot({
    # Create correlation chart, only for numeric variables. Input options to choose correlation method, ordering method, use absolute value,
    # and choose which numerical variables to include.
    corrgram::corrgram(dat[input$VariablesB], order = input$corrGroup,
                       abs = input$corrabs,
                       cor.method = input$CorrMeth,
                       text.panel = panel.txt,
                       main = "Correlation Chart")
  })
  
  #-------------------------------------------------
  # ggPair plot of variables for unprocessed data
  #-------------------------------------------------
  output$ggpair = renderPlot({
    # Create ggpairs plot. Input option to select which variables to plot and which variable to color by.
    GGally::ggpairs(data = dat[input$variableC], title = "ggpairs Plot", 
                    progress = FALSE, cardinality_threshold=NULL,
                    mapping=ggplot2::aes(colour=input$ggcol))
  })
  
  #-------------------------------------------------
  # Bar chart of categorical variables for unprocessed data
  #-------------------------------------------------
  output$bar = renderPlot({
    counts = table(dat[input$variablebar]) # Create counts of selected categorical variables. Input option to select variable.
    barplot(table(dat[input$variablebar]), main = "Bar Chart of Selected Variable", ylim=c(0,max(counts)+25)) # Create bar chart
  })
  
  #-------------------------------------------------
  # Mosaic chart of categorical variables for unprocessed data
  #-------------------------------------------------
  output$mosiac = renderPlot({
    # create mosaic plot, Input option to choose which categorical variables are plotted.
    formula <- as.formula(~ dat$HEALTHCARE_BASIS + dat$GOVERN_TYPE)
    mos = vcd::mosaic(formula, data=dat, shade = TRUE, legend = TRUE, main = "Mosaic Plot")
  })
  
  #--------------------------------------------------
  # Rising value plot of quantitative variable for unprocessed data
  #--------------------------------------------------
  output$rv = renderPlot({
    d = dat[input$variablerv] # input which numeric variables to plot.
    for (col in 1:ncol(d)) { # for each selected variable
      d[,col] = d[order(d[,col]),col] #sort each column in ascending order
    }
    d <- scale(x = d, center = input$rvstandardise, scale = input$rvstandardise) # center the scale the variables
    mypalette = rainbow(ncol(d)) # create list of colors for plotting
    # create rising value plot. Input to choose which numeric variables to plot.
    matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
    legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3)) # create legend
  })

  #--------------------------------------------------
  # Predict missing values in unprocessed data
  #-------------------------------------------------
  output$misstree = renderPlot({
    # create new variable column containing the sum of NA values in a row.
    dat$MISSINGNESS = apply(dat,MARGIN=1,function(x) sum(is.na(x)))
    # train a model using rpart method with MISSINGNESS as target variable.
    tree = train(MISSINGNESS~., data = dat, method="rpart", na.action=na.rpart)
    # plot prediction tree resulting from model evaluation
    rpart.plot(tree$finalModel, main="Predicting missing values in an observation", roundint = TRUE, clip.facs = TRUE)
  })
  
  #--------------------------------------------------
  # Get clean set of data with some processing steps applied
  #--------------------------------------------------
  get_clean_data_model = reactive({
    pMiss <- function(x){ sum(is.na(x))/length(x) } # ratio of missing values in a vector
    variable_ratio = apply(dat, 2, pMiss) # ratio of missing values in variable column
    dat = dat[, !(variable_ratio > input$VarThresh)] # remove if above the input ratio
    obs_ratio = apply(dat, 1, pMiss) # ratio of missing variables in observation row
    dat <- dat[!(obs_ratio > input$ObsThresh), ] # remove if above input ratio
    dat$HEALTHCARE_BASIS = as.character(dat$HEALTHCARE_BASIS) # convert column to character type
    dat$HEALTHCARE_BASIS[is.na(dat$HEALTHCARE_BASIS)] = "None" # change NA values to 'None'
    dat$HEALTHCARE_BASIS = as.factor(dat$HEALTHCARE_BASIS) # convert back to factor
    dat$HEALTHCARE_COST_Shadow = as.numeric(is.na(dat$HEALTHCARE_COST)) # create shadow column for variable
    dat$HEALTHCARE_COST[is.na(dat$HEALTHCARE_COST)] = 0 #Assign missing values a zero values
    data_clean = as.data.frame(dat) # return as data frame
  })

  #---------------------------------------------------
  # Create recipe object for applying processing steps to data
  #---------------------------------------------------
  do_recipe = reactive({
    data = get_clean_data_model() # call function
    dat_train = filter(data, data$OBS_TYPE == "Train") # filter to get only training data
    rec_obj = recipe(DEATH_RATE ~., data=dat_train) %>%  # create recipe object 
      update_role("OBS_TYPE", new_role = "split") # update variable to define split variable
    if (input$impute == TRUE) { # if input is true do knn imputation for missing values
      rec_obj = rec_obj %>% step_impute_knn(all_predictors(), neighbors = 5)
    } 
    if (input$impute == FALSE) { # if input is false do partial deletion for missing values
      rec_obj = rec_obj %>% step_naomit(all_predictors())
    }
    if (input$centerscale == TRUE) { # if user input true scale and center numeric ariables
      rec_obj = rec_obj %>% step_center(all_numeric(), -all_outcomes(), -has_role("id variable")) %>%
        step_scale(all_numeric(), -all_outcomes(), -has_role("id variable"))
    }
    rec_obj = rec_obj  %>%
      step_dummy(all_predictors(), -all_numeric()) # create dummy variables for categorical variables
    rec_obj = prep(rec_obj) # prepare recipe so can be applied in further steps
  })
  
  #---------------------------------------------------
  # Split training data and apply processing recipe
  #---------------------------------------------------
  data_train = reactive({
    data = get_clean_data_model() # get clean data frame
    dat_train = filter(data, data$OBS_TYPE == "Train") # split to get training data
    data_train_clean = bake(do_recipe(), new_data = dat_train) # apply preprocessing recipe
  })
  
  #--------------------------------------------------
  # Split testing data and apply processing recipe
  #--------------------------------------------------
  data_test = reactive({
    data = get_clean_data_model() # get clean data frame
    dat_test = filter(data, data$OBS_TYPE == "Test") # split to get testing data
    data_test_clean = bake(do_recipe(), new_data = dat_test) # apply preprocessing recipe
  })
  
  #--------------------------------------------------
  # Data explorer for processed training data
  #-------------------------------------------------
  output$traindf = DT::renderDataTable({
    data_train = data_train() # get processed training data
    DT::datatable(data = as.data.frame(data_train)) # create DT data table for train data
  })
  
  #--------------------------------------------------
  # Data explorer for processed testing data
  #-------------------------------------------------
  output$testdf = DT::renderDataTable({
    data_test = data_test() # get processed testing data
    DT::datatable(data = as.data.frame(data_test)) # create DT data table for test data
  })
  
  #-------------------------------------------------
  # Output model summary
  #-------------------------------------------------
  output$model_output = renderPrint({
    model = do_model_training() # output the summary of hyperparamter tuneing for glmnet model
    model
  })
  
  #--------------------------------------------------
  # Apply model to do predictions on testing data
  #--------------------------------------------------
  predictions = reactive({
    predictions = predict(do_model_training(), newdata=data_test()) # get predictions from model for test data
  })
  
  #--------------------------------------------------
  # Plot actual vs predicted for testing data
  #--------------------------------------------------
  output$model_predict = renderPlot({ # plot predicted vs actual values for glmnet model given testing data
    plot(predictions(), data_test()$DEATH_RATE, xlab="Prediction", ylab="Actual", main="Predicted vs Actual values for test data", pch=16)
    abline(a=0,b=1, col="blue")
  })
  
  #--------------------------------------------------
  # Print testing RMSE value
  #--------------------------------------------------
  output$rmse = renderPrint({
    rmse = rmse(data_test()$DEATH_RATE, predictions()) # calculate RMSE of testing data predictions
    sprintf("The test RMSE for above plot is, %.4f", rmse)
  })
  
  #--------------------------------------------------
  # Plot variable importance from model object
  #--------------------------------------------------
  output$varImp = renderPlot({
    plot(varImp(do_model_training()), main="Variable Importance") #plot variable importance of model
  })
  
  #--------------------------------------------------
  # Create model and tune parameters using training data
  #--------------------------------------------------
  do_model_training = reactive({
    data = data_train() # get training data
    set.seed(33) # set seed
    # create expanded hyperparameter tuning grid
    tuneGrid <- expand.grid(alpha = seq(0, 1, length = 5), lambda = seq(0.0001, 1, length = 10)) 
    fitControl <- trainControl(method = "cv", number = 10) # define optimisation method 
    # train model using glmnet method and output metric of RMSE
    model = caret::train(DEATH_RATE ~., data=data, method="glmnet", tuneGrid=tuneGrid, trControl = fitControl, metric="RMSE")
  })
  
  #---------------------------------------------------
  # Plot box plots for model residuals on both train and test data
  #----------------------------------------------------
  output$bugs = renderPlot({
    train = data_train() # get training data
    test = data_test() # get testing data
    train_pred = predict(do_model_training(), train) # make predictions using training data
    test_pred = predictions() # get testing data predictions
    train_res = train_pred - train$DEATH_RATE # calculates training data residuals
    test_res = test_pred - test$DEATH_RATE # calculates testing data residuals
    com = rbind(train, test) # create full processed data set
    com_pred = predict(do_model_training(), com) # do predictions for full data set
    com_res = com_pred - com$DEATH_RATE # calculate residuals for full data set
    # create boxplot of residuals. Input to define IQR multiplier and show outliers
    boxplot(train_res, test_res, com_res, main="Residuals", names=c("Train Data", "Test Data", "Train + Test Data"), outline = input$boxoutliersbox, 
                 range = input$boxrangebox, ylab="Residuals of DEATH_RATE Prediction")
  })

  })

  

  
 