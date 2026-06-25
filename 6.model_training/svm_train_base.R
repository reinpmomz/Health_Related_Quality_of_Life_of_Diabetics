library(dplyr)
library(e1071)

working_directory

## Base package Machine Learning
svm_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  out <- e1071::svm(model_form[[x]],
                    data = train1[[x]],
                    scale = TRUE,
                    kernel = "linear",
                    type = "eps-regression",
                    cross = nfolds,
                    fitted = TRUE,
                    probability = TRUE
                    )
  return(out)
  
}, simplify=FALSE)
