library(dplyr)
library(randomForest)

working_directory

## Base package Machine Learning
rf_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  out <- randomForest(model_form[[x]], data = train1[[x]],
                      importance = FALSE , ntree=500
                      )
  return(out)
}, simplify=FALSE)

