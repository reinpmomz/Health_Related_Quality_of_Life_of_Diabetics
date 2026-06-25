library(dplyr)

working_directory

## Base package Machine Learning
linear_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
	
	out <- stats::glm(model_form[[x]], data = train1[[x]], family = Gamma(link = "identity"))
	return(out)
	  
}, simplify=FALSE)


