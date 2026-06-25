library(dplyr)
library(glmnet)

working_directory

## Base package Machine Learning
enet_train_base <- sapply(outcome_vars, function(x){
  train <- train1[[x]]
  
  ### Dummy code categorical predictor variables
  x_penalized <- stats::model.matrix(model_form[[x]], train)[, -which(names(train) %in% x)] 
  
  ### Convert the outcome (class) to a numerical variable
  y_penalized <- train[[x]]
  
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  ### Find the best alpha and lambda using cross-validation
  foldid <- sample(seq(10),size=length(y_penalized),replace=TRUE)
  alpha <- seq(0, 1, by=0.02)
  cv <- sapply(alpha, function(y){
    ## Set seed for reproducibility
    set.seed(seed_models)
    out <- glmnet::cv.glmnet(x_penalized, y_penalized, foldid=foldid, alpha = y, family = Gamma(link = "identity"))
    lambda <- out$lambda.min
  }, simplify=FALSE)
  
  cv_lambda <- unlist(cv[(which.min(cv))])
  cv_alpha <- unlist(alpha[(which.min(cv))])
  
  out <- glmnet::glmnet(x_penalized, y_penalized, alpha = cv_alpha, family = Gamma(link = "identity"), lambda = cv_lambda)
  return(out)
  
}, simplify=FALSE)

