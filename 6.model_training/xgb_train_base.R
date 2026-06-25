library(dplyr)
library(xgboost)

working_directory

## Base package Machine Learning
xgb_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  train <- train1[[x]]
  
  ### Dummy code categorical predictor variables
  x_xgb <- stats::model.matrix(model_form[[x]], train)[, -which(names(train) %in% x)]
  y_xgb <- train[[x]]
  #dtrain <- xgb.DMatrix(data = x_xgb, label= y_xgb)
  
  out <- xgboost(data = x_xgb, label = y_xgb,  nrounds = 100
                 ### parameters for default gbtree booster
                 #, eta = 0.1 #default 0.3
                 #, max_depth = 15 #default 6
                 #, subsample = 0.5 #default 1
                 #, colsample_bytree = 0.5 #default 1
                 ### task parameters
                 , objective = "reg:squarederror" #logistic regression for binary classification. Output probability
                 , eval_metric = "rmse" #others "error@0.6", "merror"
                 ### other parameters
                 #, nthread = 10
                 )
  return(out)
  
}, simplify=FALSE)

