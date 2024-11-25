library(dplyr)
library(caret)
library(tidyr)

working_directory

## Data preparation

### dropping factor variables with one level
values_count <- sapply(lapply(df_drop_vars, unique), length)

df_drop_level <- df_drop_vars[ , values_count > 1]

drop_level_report <- paste0(
  paste0(names(df_drop_vars)[!(names(df_drop_vars) %in% names(df_drop_level))],
         collapse=", ")," ", 
  length(names(df_drop_vars)[!(names(df_drop_vars) %in% names(df_drop_level))])
  , " dropped factor variables with one level"
)

print(drop_level_report)

### drop missing values in data

df_drop_missing <- df_drop_level %>%
  tidyr::drop_na()

drop_missing_report <- paste0(nrow(df_analysis)-nrow(df_drop_missing), " Dropped observations with missing", 
                              ", ", nrow(df_drop_missing), " Final observations")
print(drop_missing_report)


### drop highly correlated variables

#### correlation matrix by pearson method
correlation_x <- cor(df_drop_missing %>%
                       dplyr::select(-any_of(c(outcome_vars))) %>%
                       dplyr::mutate(across(where(~ !is.numeric(.x)), ~as.numeric(.x)) ),
                     method="pearson", use = "pairwise.complete.obs")

#### numeric value for the pair-wise absolute correlation cutoff
hc = caret::findCorrelation(correlation_x, cutoff = corr_threshold, names = TRUE)

#### we sort the elements
hc = sort(hc)

#### delete the elements that has the correlation greater than threshold

 if (length(hc)==0) {
   df_drop_corr <- df_drop_missing
   } else { df_drop_corr <- df_drop_missing %>%
                                  dplyr::select(-any_of(c(hc)))
   }

drop_corr_report <- paste0(
  paste0(names(df_drop_missing)[!(names(df_drop_missing) %in% names(df_drop_corr))],
         collapse=", ")," ", 
  length(names(df_drop_missing)[!(names(df_drop_missing) %in% names(df_drop_corr))])
  , " dropped highly correlated variables" , ", ", ncol(df_drop_corr), " Final variables for modelling"
)
print(drop_corr_report)

