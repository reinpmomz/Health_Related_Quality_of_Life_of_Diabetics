library(dplyr)
library(readxl)
library(tibble)

working_directory

## Reading the recode file sheet

recode_file <- read_excel_allsheets("wdf_baseline_recode_file.xlsx")

rename_vars_df <- recode_file[["rename_vars"]] #df for renaming variable labels

selected_vars_df <- recode_file[["selected_vars"]] #df for choosing variables for analysis and plots

drop_selected_vars_df <- recode_file[["drop_selected_vars"]] #df for dropping analysis variables not needed for modelling

model_params_df <- recode_file[["model_params"]] #df for model pre-processing

model_names_df <- recode_file[["model_names"]] #df for model names

data_names_train_df <- recode_file[["data_names_train"]] #df for name type of train data


## Creating a named vector to quickly assign the new variable labels
rename_vars_df <- (rename_vars_df%>%
                     dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
                   )

new_labels <- rename_vars_df %>%
  dplyr::select(new_variable, new_label)%>%
  tibble::deframe()
