library(dplyr)

working_directory
 
## best base model
best_model_base_df <- base_metrics_all_df %>%
  dplyr::select(label, analysis, analysis_name, scores, estimate) %>%
  filter(scores == "R2") %>%
  distinct() %>%
  arrange(desc(estimate)) %>%
  group_by(analysis) %>%
  dplyr::slice(1:2) %>%
  ungroup()

print(best_model_base_df)
