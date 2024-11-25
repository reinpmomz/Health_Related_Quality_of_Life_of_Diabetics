library(MASS)
library(dplyr)
library(DALEX)
library(glmnet)
library(randomForest)
library(gbm)
library(xgboost)
library(e1071)

working_directory
## Permutation-based variable-importance

#model-agnostic method that does not assume anything about the model structure. 
#Therefore, it can be applied to any predictive model or ensemble of models. 
#It allows comparing an explanatory-variable’s importance between models with different structures.

base_varimp <- function(df, model, nreps = 10){

  varimp <- sapply(model, function(x){
    modname <- gsub("\\_train_base", "", x)
    x <- get(x)
    nn <- names(x)
  
    out_ <- sapply(nn, function(v) {
    ## Set seed for reproducibility
      set.seed(seed_varimp)
      
      test_df <- df[, colnames(df)[!colnames(df) %in% outcome_vars]]
      x_test <- if (length(outcome_vars) > 1) {
        stats::model.matrix(model_form[[v]],
                            df[,-which(!outcome_vars %in% v)])[,-which(names(df[,-which(!outcome_vars %in% v)]) %in% v)]
      } else {
        stats::model.matrix(model_form[[v]], df)[, -which(names(df) %in% v)]
      }
      
      y_test <- df[[v]]
      
      matrix_variable_df <- data.frame(names = c(colnames(x_test))) %>%
        mutate(variable = str_extract(names, paste(names(df), sep= "", collapse = '|')))
      
      matrix_variable_groups <- sapply(unique(matrix_variable_df$variable), function(x){
        matrix_variable_df$names[matrix_variable_df$variable == x ]
      }, simplify=FALSE )

      explainer <- if (modname == "lasso" | modname == "ridge" | modname == "enet" | modname == "xgb") {
        DALEX::explain(model = x[[v]], 
                       data = x_test,
                       y = y_test,
                       verbose = FALSE,
                       precalculate = TRUE,
                       label = modname,
                       type = "regression")
        } else if (modname == "pls"){ 
          DALEX::explain(model = x[[v]], 
                         data = test_df,
                         y = y_test,
                         predict_function = function(model, data) {
                           predict(model, data, ncomp = model$ncomp, type = "response")
                         },
                         residual_function = function (model, data, y, predict_function) {
                           y - predict_function(model, data)
                         },
                         verbose = FALSE,
                         precalculate = TRUE,
                         label = modname,
                         type = "regression")
          } else { 
          DALEX::explain(model = x[[v]], 
                         data = test_df,
                         y = y_test,
                         verbose = FALSE,
                         precalculate = TRUE,
                         label = modname,
                         type = "regression")
            }
    
      lossfunction <- if (modname == "lasso" | modname == "ridge" | modname == "enet") {
        DALEX::loss_root_mean_square(observed = y_test, 
                                predicted = predict(object = x[[v]], newx = x_test,
                                                    s = x[[v]][["lambda"]], type = "response"))
        } else if (modname == "xgb" ){
          DALEX::loss_root_mean_square(observed = y_test, predicted = predict(object = x[[v]], newdata = x_test))
          } else if (modname == "gbm" ){
            DALEX::loss_root_mean_square(observed = y_test, 
                                      predicted = predict(object = x[[v]], newdata = test_df, 
                                                    n.trees = gbm.perf(x[[v]], method = "cv"), type = "response"))
            } else if (modname == "rf" ){
                DALEX::loss_root_mean_square(observed = y_test, 
                                predicted = predict(object = x[[v]], newdata = test_df, type = "response"))
                } else if (modname == "svm" ){
                  DALEX::loss_root_mean_square(observed = y_test, 
                                predicted = as.numeric(predict(object = x[[v]], newdata = test_df,
                                                         probability = TRUE)) )
                  } else if (modname == "pls" ){
                      DALEX::loss_root_mean_square(observed = y_test, 
                                predicted = as.numeric(predict(object = x[[v]], newdata = test_df,
                                                               ncomp = x[[v]]$ncomp, type = "response") ) )
                      } else { 
                  DALEX::loss_root_mean_square(observed = y_test, 
                                predicted = predict(object = x[[v]], newdata = test_df, type = "response"))
                  }
    
      perform <- DALEX::model_performance(explainer)
    
      model <- if (modname == "lasso" | modname == "ridge" | modname == "enet" | modname == "xgb") {
        DALEX::model_parts(explainer = explainer,
                         #loss_function = lossfunction, ## Use when type in explainer not specified
                          variables = NULL,
                          variable_groups = matrix_variable_groups,
                          N = NULL, #number of observations sampled from the data available in the explainer-object
                          B = nreps, #number of permutations to be used for calculation of (mean) variable-importance
                          type = "difference" #"difference", "ratio", "variable_importance", "raw"
                         )
        } else { 
          DALEX::model_parts(explainer = explainer,
                            #loss_function = lossfunction, ## Use when type in explainer not specified
                             variables = NULL,
                             variable_groups = NULL,
                             N = NULL, #number of observations sampled from the data available in the explainer-object
                             B = nreps, #number of permutations to be used for calculation of (mean) variable-importance
                             type = "difference" #"difference", "ratio", "variable_importance", "raw"
                            )
          }
    
      model$analysis <- v
      model$R2 <- perform[["measures"]][["r2"]]
      return(model)
      
      }, simplify=FALSE)
    
    out_ <- do.call("rbind", out_)
    
  }, simplify=FALSE)
  
  }

