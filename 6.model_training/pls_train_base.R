library(dplyr)
library(pls)

working_directory

## Base package Machine Learning
pls_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  out <- pls::plsr(formula = model_form[[x]],
             data = train1[[x]],
             method = "widekernelpls",
             segments = 20,
             segment.type = "random", #c("random", "consecutive", "interleaved")
             scale = FALSE,
             center = TRUE,
             validation = "CV", #c("none", "CV", "LOO")
             model = TRUE,
             )
  return(out)

}, simplify=FALSE)

