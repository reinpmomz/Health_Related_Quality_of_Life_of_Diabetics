library(dplyr)
library(betareg)
library(gtsummary)

working_directory

## Regression table

multivariate_analysis_model <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  df_new <- df_drop_missing
  
  model <- betareg::betareg(model_form[[x]], data = df_new, 
                            link = "logit",
                            dist = "beta", #default("beta"), beta mixture distribution("xbetax") 
                            type = "ML" #maximum likelihood("ML"), ML with bias correction("BC"), ML with bias reduction("BR")
                            )
  
}, simplify=FALSE)

multivariate_analysis_stats <- sapply(outcome_vars, function(x){
  
	out <- multivariate_analysis_model[[x]] %>%
	  tbl_regression(exponentiate = FALSE,
	                 intercept = TRUE,
	                 estimate_fun = ~style_sigfig(.x, digits = 3),
	                 pvalue_fun = ~style_pvalue(.x, digits = 3)
	                 ) %>%
	  #add_global_p(keep = TRUE) %>% # add global p-value for categorical variables
	  add_glance_source_note() %>%
	  add_vif() %>%
	  bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
	  bold_labels() %>%
	  italicize_levels()%>% 
	  modify_header(label = "**Beta Regression - Logit link**")%>% # update the column header
	  add_significance_stars(
	    pattern = "{estimate} [{conf.low} to {conf.high}]{stars}",
	    hide_ci = TRUE, hide_se = TRUE , hide_p = FALSE) %>%
	  modify_header(estimate ~ "**Beta (95% CI)**") %>%
	  modify_footnote(estimate ~ "Beta = Estimate, CI = Confidence Interval", abbreviation = TRUE) %>%
	  gtsummary::as_flex_table()
	  
}, simplify=FALSE)

print(multivariate_analysis_stats)

