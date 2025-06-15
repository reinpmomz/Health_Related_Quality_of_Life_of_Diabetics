library(dplyr)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics
strata_vars <- c("wdf_q2_1")

descriptive_stats <- descriptive_table(df = df_analysis,
                                       foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                       caption = "Descriptive Statistics",
                                       include_vars = names(df_analysis),
                                       mean_vars = c("overall_qol_transformed", "wdf_q3_18", "wdf_q3_20",
                                                     "wdf_q3_33", "wdf_q3_36", "wdf_q3_39","wdf_q3_42", "wdf_q3_45",
                                                     "wdf_q3_34a_b_hours", "wdf_q3_37a_b_hours", "wdf_q3_40a_b_hours",
                                                     "wdf_q3_43a_b_hours", "wdf_q3_46a_b_hours", "wdf_q4_qol10_overall"
                                                     ),
                                       flex_table = FALSE
                                       )

print(descriptive_stats)

## Inferential statistics - Chisquare/t-test

inferential_vars <- selected_vars_df$new_variable[selected_vars_df$inferential == "yes" &
                                                    !is.na(selected_vars_df$inferential)]

inferential_stats <- 
  categorical_inferential_table(df = df_analysis,
                                by_vars = inferential_vars[!(inferential_vars %in% outcome_vars)],
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "Inferential Statistics",
                                include_vars = names(df_analysis),
                                percent = "column", #default
                                p_value = TRUE, #default
                                mean_vars = c("overall_qol_transformed", "wdf_q3_18", "wdf_q3_20",
                                              "wdf_q3_33", "wdf_q3_36", "wdf_q3_39","wdf_q3_42", "wdf_q3_45",
                                              "wdf_q3_34a_b_hours", "wdf_q3_37a_b_hours", "wdf_q3_40a_b_hours",
                                              "wdf_q3_43a_b_hours", "wdf_q3_46a_b_hours", "wdf_q4_qol10_overall"
                                              ),
                                flex_table = FALSE
                                )
print(inferential_stats)

## Inferential statistics - Healthy eating habits and regular physical activity 
inferential_eating_stats <- categorical_inferential_table(df = df_analysis,
                                by_vars = c("healthy_eating_index"),
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "Inferential Statistics",
                                include_vars = health_eating_vars,
                                percent = "column", #default
                                p_value = TRUE, #default
                                mean_vars = c("wdf_q3_18", "wdf_q3_20"),
                                flex_table = TRUE
                                )

print(inferential_eating_stats)

inferential_physical_activity_stats <- 
  categorical_inferential_table(df = df_analysis,
                                by_vars = c("regular_physical_activity_index"),
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "Inferential Statistics",
                                include_vars = exercise_vars,
                                percent = "column", #default
                                p_value = TRUE, #default
                                mean_vars = c("wdf_q3_33", "wdf_q3_36", "wdf_q3_39","wdf_q3_42", "wdf_q3_45",
                                              "wdf_q3_34a_b_hours", "wdf_q3_37a_b_hours", "wdf_q3_40a_b_hours",
                                              "wdf_q3_43a_b_hours", "wdf_q3_46a_b_hours"
                                              ),
                                flex_table = TRUE
                                )

print(inferential_physical_activity_stats)

## Inferential statistics - Anova
inferential_anova_stats <- if (length(outcome_vars)>0) {
  
  sapply(paste0(c(unique(inferential_vars %in% outcome_vars))), function(x) {
    nn <- x
    df_new <- df_analysis %>%
      dplyr::select(-any_of(c(strata_vars)))
    
    index <- inferential_vars[(inferential_vars %in% outcome_vars) == nn]
    
    if (nn == "TRUE") {
      ### continous Outcome (Character/Factor variables)
      ### One Way Anova statistics
      sapply(index, function(z) {
        continous_table(df = df_new,
                        foot_note = "Mean (SD)",
                        caption = "",
                        continous_var = z,
                        p_value = TRUE,
                        display_mean = TRUE,
                        flex_table = FALSE
                        )
        
      }, simplify = FALSE
      )
      
      } else {
        ### Two Way Anova statistics
      sapply(outcome_vars, function(y) {
        gtsummary::tbl_merge(tbls =
          continous_by_table(df = df_new,
                             foot_note = "Mean (SD)",
                             caption = "",
                             continous_var = y,
                             by_vars = index,
                             p_value = TRUE,
                             display_mean = TRUE,
                             flex_table = FALSE
                             )
          , tab_spanner = c(final_attribute$label[final_attribute$variable %in% index])
          ) 
          
      }, simplify = FALSE
      )
    } 
  }, simplify = FALSE
  ) 
  } else {
    ### by factor/character vars
    categorical_inferential_table(df = df_analysis,
                                  by_vars = inferential_vars[!(inferential_vars %in% outcome_vars)],
                                  foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                  caption = "Inferential Statistics",
                                  include = names(df_analysis),
                                  percent = "column", #default
                                  p_value = TRUE, #default
                                  mean_vars = c("overall_qol_transformed", "wdf_q3_18", "wdf_q3_20",
                                                "wdf_q3_33", "wdf_q3_36", "wdf_q3_39","wdf_q3_42", "wdf_q3_45",
                                                "wdf_q3_34a_b_hours", "wdf_q3_37a_b_hours", "wdf_q3_40a_b_hours",
                                                "wdf_q3_43a_b_hours", "wdf_q3_46a_b_hours", "wdf_q4_qol10_overall"
                                                ),
                                  flex_table = FALSE
                                  )
  }

print(inferential_anova_stats)

## Merging gtsummary tables

### Merging descriptive and inferential
descriptive_inferential_stats_merge <- 
  gtsummary::tbl_merge(tbls= c(list(descriptive_stats),
                               inferential_stats
                               ),
                       tab_spanner = c("Overall",
                                       final_attribute$label[final_attribute$variable %in% 
                                                               inferential_vars[!(inferential_vars %in% outcome_vars)]]
                                       )
                       ) %>%
  gtsummary::as_flex_table()

print(descriptive_inferential_stats_merge)

### Merging descriptive and inferential Anova
descriptive_inferential_anova_stats_merge <- if (length(outcome_vars)>0) { 
  
  inferential_vars_no_outcome <- inferential_vars[!(inferential_vars %in% outcome_vars)]
  
  if (length(inferential_anova_stats)==2) {
    gtsummary::tbl_merge(tbls= c(list(descriptive_stats),
                                 inferential_anova_stats[["TRUE"]],
                                 inferential_anova_stats[["FALSE"]] 
                                 ),
                         tab_spanner = c("Overall", final_attribute$label[final_attribute$variable %in% outcome_vars],
                                         final_attribute$label[final_attribute$variable %in% outcome_vars]
                                         )
                         ) %>%
      gtsummary::as_flex_table()
    
  } else if (length(inferential_anova_stats)==1 && names(inferential_anova_stats) == "TRUE" ) {
    gtsummary::tbl_merge(tbls= c(list(descriptive_stats),
                                 inferential_anova_stats[["TRUE"]]
                                 ),
                         tab_spanner = c("Overall", final_attribute$label[final_attribute$variable %in% outcome_vars]
                                         )
                         ) %>%
      gtsummary::as_flex_table()
    
  } 
} else {
  gtsummary::tbl_merge(tbls= c(list(descriptive_stats),
                               inferential_anova_stats
                               ),
                       tab_spanner = c("Overall", final_attribute$label[final_attribute$variable %in% inferential_vars]
                                       )
                       ) %>%
    gtsummary::as_flex_table()
  
}

print(descriptive_inferential_anova_stats_merge)



