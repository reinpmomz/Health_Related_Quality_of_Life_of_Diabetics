library(dplyr)
library(tidyr)

working_directory

## group variables 
### if empty vector use character()
analysis_vars_df <- selected_vars_df[selected_vars_df$select == "retain" & !is.na(selected_vars_df$select),]
consent_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group == "consent" 
                                              & !is.na(analysis_vars_df$select_group)]
outcome_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "outcome" 
                                              & !is.na(analysis_vars_df$select_group)]
socio_demo_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "socio_demo" 
                                                 & !is.na(analysis_vars_df$select_group)]
diabetes_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "diabetes" 
                                               & !is.na(analysis_vars_df$select_group)]
medical_conditions_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "medical_conditions" 
                                                         & !is.na(analysis_vars_df$select_group)]
lifestyle_history_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "lifestyle_history" 
                                                        & !is.na(analysis_vars_df$select_group)]
health_eating_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "health_eating_pca" 
                                                    | analysis_vars_df$select_group  == "health_eating"
                                                    & !is.na(analysis_vars_df$select_group)]
exercise_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "exercise_pca"
                                               | analysis_vars_df$select_group  == "exercise"
                                               & !is.na(analysis_vars_df$select_group)]
sitting_sleep_behaviour_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "sitting_behaviour" |
                                                                analysis_vars_df$select_group  == "sleep_behaviour" 
                                                              & !is.na(analysis_vars_df$select_group)]
qol_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "quality_of_life" 
                                          & !is.na(analysis_vars_df$select_group)]
clinical_anthropometric_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "clinical_anthropometric" 
                                                              & !is.na(analysis_vars_df$select_group)]

## make dataset with variables for descriptive and inferential statistics
df_analysis <- df_final %>%
  dplyr::filter(wdf_q2_1 == "Yes") %>%
  dplyr::select(any_of(c(consent_vars, outcome_vars, socio_demo_vars, diabetes_vars, medical_conditions_vars,
                         lifestyle_history_vars, health_eating_vars, exercise_vars, sitting_sleep_behaviour_vars,
                         qol_vars, clinical_anthropometric_vars
                         )
                       )
                ) %>%
  tidyr::drop_na(any_of(outcome_vars))


filtered_report <- paste0(nrow(df_final)-nrow(df_analysis), " Dropped observations with non-diabetes", 
                          ", ", nrow(df_analysis), " Final observations with diabetes"
                          )

print(filtered_report)


analysis_report <- paste0(
  paste0(analysis_vars_df$new_variable,collapse=", ")," ", length(analysis_vars_df$new_variable)
  , " variables used for analysis" 
)
print(analysis_report)

none_analysis_report <- paste0(
  paste0(selected_vars_df$new_variable[selected_vars_df$select == "drop"],
         collapse=", ")," ", 
  length(selected_vars_df$new_variable[selected_vars_df$select == "drop"])
  , " variables not used for analysis"
)
print(none_analysis_report)

