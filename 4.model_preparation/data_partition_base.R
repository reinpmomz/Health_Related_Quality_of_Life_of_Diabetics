library(rsample)
library(dplyr)
library(caret)

working_directory

## Set seed for reproducibility
set.seed(seed_partition)

## Create training and test datasets
split1 <- rsample::initial_split(df_drop_missing, 
                                 prop = model_params_df$test_train_ratio
)

train1 <- sapply(outcome_vars, function(x) {
  nn <- x
  train <- cbind(training(split1) %>%
                   dplyr::select(any_of(c(nn)))
                 , training(split1) %>%
                   dplyr::select(-any_of(c(outcome_vars))
                   )
  )
  
}, simplify = FALSE)

test1 <- testing(split1)