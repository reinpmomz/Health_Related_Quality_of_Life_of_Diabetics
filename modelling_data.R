library(dplyr)

working_directory

## Drop variables which won't be used in modelling

drop_vars <- drop_selected_vars_df$new_variable[!is.na(drop_selected_vars_df$new_variable)]

if (length(drop_vars)>0) {
  df_drop_vars <- df_analysis %>%
    dplyr::select(-any_of(drop_vars))
  
  drop_vars_report <- paste0(
    paste0(drop_vars, collapse=", ")," ", length(drop_vars)
    , " dropped variables not relevant to modeling"
  )
  
} else {
  df_drop_vars <- df_analysis
  
  drop_vars_report <- paste0(length(drop_vars)
                             , " dropped variables not relevant to modeling"
                             )
}

print(drop_vars_report)

## Model formula
model_form <- sapply(outcome_vars, function(x) {
  as.formula(paste0(x, "~."))
}, simplify = FALSE)

## correlation cutoff
corr_threshold <- model_params_df$corr_threshold

## effectsize cutoff
effectsize_threshold <- model_params_df$effect_size_threshold

## folds
nfolds <- model_params_df$folds

## seed for reproducibility
seed_partition <- model_params_df$seed_partition
seed_models <- model_params_df$seed_models
seed_metrics <- model_params_df$seed_metrics
seed_varimp <- model_params_df$seed_varimp
