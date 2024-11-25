library(dplyr)
library(caret)

working_directory
## Get metrics and roc for all models

base_metrics_df <- bootEstimates_metrics_base(df=test1, model = ls(pattern = "_train_base$"), nreps = 100)

base_residuals_df <- bootEstimates_residuals_base(df=test1, model = ls(pattern = "_train_base$"), nreps = 100)

base_metrics_all_df <- base_metrics_df %>%
  left_join(data_names_train_df, by = c("analysis"))


base_residuals_all_df <- base_residuals_df %>%
  left_join(data_names_train_df, by = c("analysis"))



