library(MASS)
library(dplyr)

working_directory

## Base package Machine Learning
aic_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)

	out <- stats::glm(model_form[[x]], data = train1[[x]], family = Gamma(link = "identity")) %>%
	  MASS::stepAIC(trace = 0, #do not show the step by step process of model selection
	                scale = 0, #estimate of the error variance to be estimated by maximum likelihood
	                k=2, #gives genuine AIC
	                direction = "backward" #default is "backward" if scope argument is missing #options "both", "forward"
	  )
	return(out)
	  
}, simplify=FALSE)

