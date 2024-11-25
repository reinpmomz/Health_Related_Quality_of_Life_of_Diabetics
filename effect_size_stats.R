library(dplyr)


working_directory


effect_size_stats <- if (length(outcome_vars)>0) {
  
  sapply( paste0("diabetes-",unique(df_analysis[[strata_vars]])), function(x) {
    
    effectsize_corr_table(df = df_analysis %>% 
                            dplyr::filter(.data[[strata_vars]] == gsub("diabetes-", "", x)) %>%
                            dplyr::select(-any_of(c(strata_vars)))
                            , by_vars = outcome_vars)
    
  }, simplify = TRUE
  )

} else {
  print(paste0("No effect size analysis done. Select outcome variable"))
}

