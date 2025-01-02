library(dplyr)
library(caret)
library(rstatix)
library(data.table)

working_directory

## Feature Selection

### drop highly correlated predictor variables

#### correlation matrix by pearson method
correlation_x <- cor(df_drop_vars %>%
                       dplyr::select(-any_of(c(outcome_vars))) %>%
                       dplyr::mutate(across(where(~ !is.numeric(.x)), ~as.numeric(.x)) ),
                     method="pearson", use = "pairwise.complete.obs")

#### numeric value for the pair-wise absolute correlation cutoff
hc = caret::findCorrelation(correlation_x, cutoff = corr_threshold, names = TRUE)

#### we sort the elements
hc = sort(hc)

#### delete the elements that has the correlation greater than threshold

 if (length(hc)==0) {
   df_drop_corr <- df_drop_vars
   } else { df_drop_corr <- df_drop_vars %>%
                                  dplyr::select(-any_of(c(hc)))
   }

drop_corr_report <- paste0(
  paste0(names(df_drop_vars)[!(names(df_drop_vars) %in% names(df_drop_corr))],
         collapse=", ")," ", 
  length(names(df_drop_vars)[!(names(df_drop_vars) %in% names(df_drop_corr))])
  , " dropped highly correlated variables" , ", ", ncol(df_drop_corr), " Final variables for feature selection"
)
print(drop_corr_report)


### drop low associated predictor variables with outcome variable - effect size

effect_size_x <- effectsize_corr_table(df = df_drop_corr ,
                                       by_vars = outcome_vars) %>%
  data.table::rbindlist() %>%
  dplyr::mutate(effsize = ifelse(type == "correlation", cor, effsize)
                , effsize_new = abs(round(effsize, 4))
                ) %>%
  dplyr::filter(effsize_new > effectsize_threshold) %>%
  dplyr::pull(variables)


df_drop_feature <- df_drop_corr %>%
  dplyr::select(any_of(c(outcome_vars, effect_size_x))
                )

drop_feature_report <- paste0(
  paste0(names(df_drop_corr)[!(names(df_drop_corr) %in% names(df_drop_feature))],
         collapse=", ")," ", 
  length(names(df_drop_corr)[!(names(df_drop_corr) %in% names(df_drop_feature))])
  , " dropped lowly associated predictor variables with outcome variable" , ", ", ncol(df_drop_feature),
  " Final variables for modelling"
)

print(drop_feature_report)
