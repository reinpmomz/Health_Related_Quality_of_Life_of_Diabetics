library(dplyr)

working_directory
## Get Variable importance for all models

base_varimp_df <- base_varimp(df=test1, model= ls(pattern = "_train_base$"), nreps = 50)

base_varimp_all_df <- do.call("rbind", base_varimp_df) %>%
  left_join(data_names_train_df, by = c("analysis"))


