library(dplyr)
library(gbm)

working_directory

## Base package Machine Learning
gbm_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  out <- gbm(formula = model_form[[x]],
             distribution = "gaussian",
             data = train1[[x]],
             n.trees = 500,
             interaction.depth = 1,
             shrinkage = 0.01,
             cv.folds = 10
  )
  return(out)

}, simplify=FALSE)

