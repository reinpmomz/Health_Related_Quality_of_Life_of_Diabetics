library(MASS)
library(dplyr)
library(tibble)
library(tidyr)
library(caret)
library(glmnet)
library(randomForest)
library(gbm)
library(xgboost)
library(e1071)

working_directory
## ---- Prediction uncertainities ----

bootMeasures_metrics_base <- function(df, model){
  
  models_base <- sapply(model, function(x){
    modname <- gsub("\\_train_base", "", x)
    x <- get(x)
    nn <- names(x)
    
    out <- sapply(nn, function(v) {
      test_df <- df[, colnames(df)[!colnames(df) %in% outcome_vars]]
      x_test <- if (length(outcome_vars) > 1) {
        stats::model.matrix(model_form[[v]],
                            df[,-which(!outcome_vars %in% v)])[,-which(names(df[,-which(!outcome_vars %in% v)]) %in% v)]
      } else {
        stats::model.matrix(model_form[[v]], df)[, -which(names(df) %in% v)]
      }
      
      y_test <- df[[v]]
      
      preds <- if (modname == "lasso" | modname == "ridge" | modname == "enet") {
        predict(object = x[[v]], newx = x_test, s = x[[v]][["lambda"]], type = "response")
        } else if (modname == "xgb" ){
          predict(object = x[[v]], newdata = x_test)
          } else if (modname == "gbm" ){
            predict(object = x[[v]], newdata = test_df, n.trees = gbm.perf(x[[v]], method = "cv"), type = "response")
            } else if (modname == "rf" ){
                predict(object = x[[v]], newdata = test_df, type = "response")
                } else if (modname == "svm" ){
                  as.numeric(
                  predict(object = x[[v]], newdata = test_df, probability = TRUE))
                  } else if (modname == "pls" ){
                      as.numeric(
                        predict(object = x[[v]], newdata = test_df, ncomp = x[[v]]$ncomp, type = "response"))
                    } else { 
                        predict(object = x[[v]], newdata = test_df, type = "response")
                      }
      
      preds_ <- preds
    
      R2 = caret::R2(preds_, y_test, formula = "corr") #cor(y_test, preds_)^2
      ME = mean((y_test- preds_))
      RMSE = caret::RMSE(preds_, y_test) #sqrt(MSE)
      MAE = caret::MAE(preds_, y_test) #MAE = mean(abs(y_test - preds_))
      MPE = mean((y_test- preds_)*100/preds_)
      MAPE = mean(abs(y_test - preds_)*100/preds_)
      MSE = mean((y_test- preds_)^2)
      
      scores_df <- data.frame(R2 = unname(R2)
                              , RMSE = RMSE
                              , MSE = MSE
                              , ME = ME
                              , MAE = MAE
                              , MPE = MPE
                              , MAPE = MAPE
                              )
      scores_df$label <- modname
      scores_df$analysis <- v
      return(scores_df)

  }, simplify=FALSE)
  
    out <- do.call("rbind", out)
  
}, simplify=FALSE)
  
  models_base <- do.call("rbind", models_base)

}

bootMeasures_residuals_base <- function(df, model){
  
  models_base <- sapply(model, function(x){
    modname <- gsub("\\_train_base", "", x)
    x <- get(x)
    nn <- names(x)
    
    out <- sapply(nn, function(v) {
      test_df <- df[, colnames(df)[!colnames(df) %in% outcome_vars]]
      x_test <- if (length(outcome_vars) > 1) {
        stats::model.matrix(model_form[[v]],
                            df[,-which(!outcome_vars %in% v)])[,-which(names(df[,-which(!outcome_vars %in% v)]) %in% v)]
      } else {
        stats::model.matrix(model_form[[v]], df)[, -which(names(df) %in% v)]
      }
      y_test <- df[[v]]
      
      preds <- if (modname == "lasso" | modname == "ridge" | modname == "enet") {
        predict(object = x[[v]], newx = x_test, s = x[[v]][["lambda"]], type = "response")
        } else if (modname == "xgb" ){
          predict(object = x[[v]], newdata = x_test)
          } else if (modname == "gbm" ){
            predict(object = x[[v]], newdata = test_df, n.trees = gbm.perf(x[[v]], method = "cv"), type = "response")
            } else if (modname == "rf" ){
              predict(object = x[[v]], newdata = test_df, type = "response")
              } else if (modname == "svm" ){
                as.numeric(predict(object = x[[v]], newdata = test_df, probability = TRUE))
                } else if (modname == "pls" ){
                  as.numeric(predict(object = x[[v]], newdata = test_df, ncomp = x[[v]]$ncomp, type = "response"))
                  } else { 
                    predict(object = x[[v]], newdata = test_df, type = "response")
                    }
      
      preds_ <- preds
      
      R2 = caret::R2(preds_, y_test, formula = "corr") #cor(y_test, preds_)^2
      residuals <- y_test - preds_
      
      residual_df <- data.frame(residual = unname(residuals)
                              , predicted = unname(preds_)
                              , observed = unname(y_test)
                              , R2 = unname(R2)
                              )
      residual_df$label <- modname
      residual_df$analysis <- v
      residual_df$number <- 1:length(residual_df$analysis)
      return(residual_df)
      
    }, simplify=FALSE)
    
    out <- do.call("rbind", out)
    
  }, simplify=FALSE)
  
  models_base <- do.call("rbind", models_base)
  
}

bootEstimates_metrics_base <- function(df, model, nreps = 200) {
  ### Set seed for reproducibility
  set.seed(seed_metrics)
  resamples <- createResample(1:nrow(df), times = nreps, list = TRUE)
  est <- lapply(resamples, function(x){
    bootMeasures_metrics_base(df[x, ], model)
  })
  out <- do.call("rbind", est)
  out <- as.data.frame(out) %>%
    tibble::rownames_to_column(var="samples") %>%
    tidyr::pivot_longer(
      cols = !c(samples, label, analysis),
      names_to = "scores", 
      values_to = "value"
    ) %>%
    group_by(label, analysis, scores) %>%
    summarise(lower = quantile(value, probs = 0.05, na.rm = TRUE),
              estimate = quantile(value, probs = 0.5, na.rm = TRUE),
              upper = quantile(value, probs = 0.95, na.rm = TRUE), .groups = "drop")
  return(out)
  
}

bootEstimates_residuals_base <- function(df, model, nreps = 200) {
  ### Set seed for reproducibility
  set.seed(seed_metrics)
  resamples <- createResample(1:nrow(df), times = nreps, list = TRUE)
  est <- lapply(resamples, function(x){
    bootMeasures_residuals_base(df[x, ], model)
  })
  
  out <- do.call("rbind", est)
  out <- as.data.frame(out) %>%
    tibble::rownames_to_column(var="samples") %>%
    group_by(label, analysis, number) %>%
    summarise(residual_estimate = quantile(residual, probs = 0.5, na.rm = TRUE),
              predicted_estimate = quantile(predicted, probs = 0.5, na.rm = TRUE),
              R2_estimate = round(quantile(R2, probs = 0.5, na.rm = TRUE),3), .groups = "drop") %>%
    dplyr::mutate(label_R2 = paste0(label, " (R2=", R2_estimate, ")"))
  return(out)
  
} 
 