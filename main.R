######################################################################
### Restart R
#.rs.restartR()

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T)

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("requirements.R")

### helper/customized functions
source("helperfuns_1.R")
source("helperfuns_2.R")
source("helperfuns_plots_3.R")
source("helperfuns_plots_4.R")
source("helperfuns_5.R")
source("helperfuns_6.R")

### Load data 
#source("load_data_drive.R")
source("load_data_local.R")

### Load recode file 
source("load_recode_file.R")

### Data cleaning
source("cleaning.R")

### Create SES, healthy eating and physical activity based on PCA
source("ses_pca.R")
source("healthy_eating_pca.R")
source("physical_activity_pca.R")

### Select variables for descriptive and inferential analysis
source("analysis_data.R")

### Descriptive and Inferential stats
source("descriptive_inferential_stats.R")

### Effect Size stats
source("effect_size_stats.R")

### correlation and reliability stats
source("reliability_correlation_tools_stats.R")

### Save stats output
source("save_descriptive_inferential_output.R")
source("save_effect_size_output.R")
source("save_reliability_correlation_tools_output.R")

### Descriptive plots
source("descriptive_plots.R")

######################################################################

## Model preparation

### Check normality and distribution of continous outcome variable
source("check_normality_distribution.R")

### Select variables required for modelling
source("modelling_data.R")

### Feature selection - filter method
source("feature_selection.R")

### handle single value variables and missing values
source("data_preparation_base.R")

### Train-test split, up-sampling and down-sampling
source("data_partition_base.R")

######################################################################
## Multivariate Analysis
source("multivariate_analysis.R")
source("save_multivariate_analysis_output.R")
source("multivariate_analysis_plot.R")
source("multivariate_analysis_var_imp.R")

######################################################################

## Model training - Base 

### Linear - Gamma distribution
source("linear_train_base.R")

### Beta - all observations for the dependent variable are in (0, 1)
source("beta_train_base.R")

### Lasso - Gamma distribution
source("lasso_train_base.R")

### Ridge - Gamma distribution
source("ridge_train_base.R")

### Elastic net - Gamma distribution
source("enet_train_base.R")

### Random forest
source("rf_train_base.R")

### Gradient Boosting Machine
source("gbm_train_base.R")

### Extreme Gradient Boosting
source("xgb_train_base.R")

### Support Vector Machine
source("svm_train_base.R")

### Partial least squares
source("pls_train_base.R")

######################################################################

## Predictive performance

### Test data
source("bootfuns_base.R")
source("resamples_test_data_base.R")

### R2 RMSE plots
source("metrics_base_plots.R")

### Residual plots
source("residuals_base_plots.R")

### Best performing model - R2
source("best_model_base.R")

### Variable importance
source("varimpfuns_base.R")
source("varimp_base.R")
source("varimp_best_base_plots.R")

### Rank variable importance
source("varimp_rank_base.R")
source("varimp_rank_base_plots.R")

source("varimp_rank_all_plots.R")

######################################################################

## Save workspace at the end without working directory path
save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_output", "output_Dir",
                                                              "local_download", "file_id", "list_folders", "list_files",
                                                              "Rdata_files")],
     file = "wdf_baseline.RData",
     envir = .GlobalEnv #parent.frame()
     )

######################################################################

## Run all files in Rstudio
source("main.R")

######################################################################

