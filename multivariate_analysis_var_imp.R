library(dplyr)
library(DALEX)
library(betareg)
library(ggplot2)

working_directory

## Permutation-based variable-importance

#model-agnostic method that does not assume anything about the model structure. 
#Therefore, it can be applied to any predictive model or ensemble of models. 
#It allows comparing an explanatory-variable’s importance between models with different structures.

multivariate_analysis_var_imp <- sapply(outcome_vars, function(x){
  ## Set seed for reproducibility
  set.seed(seed_varimp)
  
  df_new <- df_drop_missing
  
  test_df <- df_new[, colnames(df_new)[!colnames(df_new) %in% outcome_vars]]
  y_test <- df_new[[x]]
  
  explainer <- DALEX::explain(model = multivariate_analysis_model[[x]], 
                              data = test_df,
                              y = y_test,
                              verbose = FALSE,
                              precalculate = TRUE,
                              label = "",
                              type = "regression")
  
  lossfunction <- DALEX::loss_root_mean_square(observed = y_test,
                                                predicted = predict(object = multivariate_analysis_model[[x]],
                                                                    newdata = test_df, type = "response"
                                                                    )
                                                )
  
  perform <- DALEX::model_performance(explainer)
  
  importance <- DALEX::model_parts(explainer = explainer,
                                   #loss_function = lossfunction, ## Use when type in explainer not specified
                                   variables = NULL,
                                   variable_groups = NULL,
                                   N = NULL, #number of observations sampled from the data available in the explainer-object
                                   B = 25, #number of permutations to be used for calculation of (mean) variable-importance
                                   type = "difference" #"difference", "ratio", "variable_importance", "raw"
                                   )
  
  #importance$mse <- perform[["measures"]][["mse"]]
  #importance$rmse <- perform[["measures"]][["rmse"]]
  importance$R2 <- perform[["measures"]][["r2"]]
  #importance$mad <- perform[["measures"]][["mad"]]
  return(importance)
  
  
}, simplify=FALSE)


multivariate_analysis_var_imp_plot <- sapply(outcome_vars, function(x){
  
  df_new <- df_drop_missing
  test_df <- df_new[, colnames(df_new)[!colnames(df_new) %in% outcome_vars]]
  
  ## Creating a named vector to quickly rename levels
  new_levels <- selected_vars_df %>%
    dplyr::select(new_label, new_variable) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(new_label = gsub(" grouped| type", "", new_label)) %>%
    dplyr::filter(new_variable %in% names(test_df)) %>%
    tibble::deframe()
  
  df <- multivariate_analysis_var_imp[[x]] %>%
    dplyr::mutate(variable = as.character(forcats::fct_recode(variable, !!!new_levels))
                  ) %>%
    arrange(desc(R2)) %>%
    mutate(label = forcats::as_factor(label))
  
  plot <- plot(df
               , show_boxplots = TRUE
               , bar_width = 5 #default 10
               , desc_sorting = TRUE
               , title = "" #default 'Feature Importance'
               , subtitle = ""
               ) +
    labs(x = "", y = "", title = "") +
    scale_y_continuous(expand = expansion(mult = c(0.01,0.1))
                       , n.breaks = 10)
  
  print(plot)
  
}, simplify=FALSE)


### Saving Regression plot using loops
for (i in seq(length(multivariate_analysis_var_imp_plot))) {
  ggsave(plot=multivariate_analysis_var_imp_plot[[i]], height = 6, width = 9,
         filename = paste0("variable_importance_plot_",names(multivariate_analysis_var_imp_plot)[[i]],".png"),
         path = output_Dir, bg='white')  
}


