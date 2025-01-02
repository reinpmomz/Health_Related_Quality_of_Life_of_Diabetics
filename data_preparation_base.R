library(dplyr)
library(caret)
library(tidyr)

working_directory

## Data preparation

### dropping factor variables with one level
values_count <- sapply(lapply(df_drop_feature, unique), length)

df_drop_level <- df_drop_feature[ , values_count > 1]

drop_level_report <- paste0(
  paste0(names(df_drop_feature)[!(names(df_drop_feature) %in% names(df_drop_level))],
         collapse=", ")," ", 
  length(names(df_drop_feature)[!(names(df_drop_feature) %in% names(df_drop_level))])
  , " dropped factor variables with one level"
)

print(drop_level_report)

### drop missing values in data

df_drop_missing <- df_drop_level %>%
  tidyr::drop_na()

drop_missing_report <- paste0(nrow(df_analysis)-nrow(df_drop_missing), " Dropped observations with missing", 
                              ", ", nrow(df_drop_missing), " Final observations")
print(drop_missing_report)

