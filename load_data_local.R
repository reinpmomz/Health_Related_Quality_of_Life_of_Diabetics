library(dplyr)
library(haven)
library(labelled)

working_directory

## Reading data from local folder

data_files <- list.files(path = data_Dir, full.names = F)

df_list <- sapply(data_files, function(x){
  nn <- x
  
  haven::read_dta(base::file.path(data_Dir, nn))
  
}, simplify=FALSE)

df_raw <- df_list[["wdfbaseline_cleaned.dta"]]

df <- df_raw %>%
  dplyr::mutate(
    dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x))) #converts only labelled columns to factors


## creating data dictionary
attribute <- base::as.data.frame(labelled::generate_dictionary(df, labels = TRUE, values = TRUE))

raw_attribute <- base::as.data.frame(labelled::look_for(df_raw, labels = TRUE, values = TRUE))
