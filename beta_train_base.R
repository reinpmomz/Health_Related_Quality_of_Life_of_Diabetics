library(dplyr)
library(betareg)

working_directory

## Base package Machine Learning
beta_train_base <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
	
	out <- betareg::betareg(model_form[[x]], data = train1[[x]],
	                        link = "logit",
	                        dist = "beta", #default("beta"), beta mixture distribution("xbetax") 
	                        type = "ML" #maximum likelihood("ML"), ML with bias correction("BC"), ML with bias reduction("BR")
	                        )
	return(out)
	  
}, simplify=FALSE)

