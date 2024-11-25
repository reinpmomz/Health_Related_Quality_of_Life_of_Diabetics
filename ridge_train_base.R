library(dplyr)
library(glmnet)

working_directory

## Base package Machine Learning
ridge_train_base <- sapply(outcome_vars, function(x){
  train <- train1[[x]]
  
  # Dummy code categorical predictor variables
  x_penalized <- stats::model.matrix(model_form[[x]], train)[, -which(names(train) %in% x)] 
  
  # Convert the outcome (class) to a numerical variable
  y_penalized <- train[[x]]
  
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  # Find the best lambda using cross-validation
  cv_lambda <- glmnet::cv.glmnet(x_penalized, y_penalized, alpha = 0, family = Gamma(link = "identity"))
  
  out <- glmnet::glmnet(x_penalized, y_penalized, alpha = 0, family = Gamma(link = "identity"),
                        lambda = cv_lambda$lambda.min)
  
  return(out)
  
}, simplify=FALSE)

