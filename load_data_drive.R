library(dplyr)
library(haven)
library(googledrive)
library(labelled)

working_directory

## Reading the data from google drive
googledrive::drive_auth(
  #email = ""
  )

file_id <- googledrive::drive_find(shared_drive = c(googledrive::shared_drive_find(pattern = "Data Science Programs")$id),
                           q = c("name = 'Microdata Portal'",
                                 "mimeType = 'application/vnd.google-apps.folder'")
                           ) #search for a specific set of shared drives, use the query string q

list_folders <- drive_ls(path = file_id$id, q = c("name = 'Diabetes Data'",
                                                  "mimeType = 'application/vnd.google-apps.folder'"))

list_files <- drive_ls(path = list_folders$id, q = c("name = 'wdfbaseline_cleaned.dta'"))

local_download <- googledrive::drive_download(list_files$id, overwrite = TRUE)

df_raw <- haven::read_dta(paste0(local_download$local_path))

### deletes the local downloaded data
base::unlink(local_download$local_path)

df <- df_raw%>%
  dplyr::mutate(
    dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x))) #converts only labelled columns to factors


## creating data dictionary
attribute <- base::as.data.frame(labelled::generate_dictionary(df, labels = TRUE, values = TRUE))

raw_attribute <- base::as.data.frame(labelled::look_for(df_raw, labels = TRUE, values = TRUE))

