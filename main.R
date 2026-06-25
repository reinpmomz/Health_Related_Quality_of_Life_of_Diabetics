################################################################################
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
source("./1.setup/requirements.R")

### helper/customized functions
source("./1.setup/helperfuns_read_excel_sheets.R")
source("./1.setup/helperfuns_gt_summary_themes.R")
source("./1.setup/helperfuns_table_summary_categorical.R")
source("./1.setup/helperfuns_table_summary_continous.R")
source("./1.setup/helperfuns_ggplot_themes.R")
source("./1.setup/helperfuns_simple_plots.R")
source("./1.setup/helperfuns_multiple_response_plots.R")
source("./1.setup/helperfuns_effect_size.R")
source("./1.setup/helperfuns_coallesce_columns.R")

################################################################################

### Load data 
#source("./2.load_data_and_clean/load_data_drive.R")
source("./2.load_data_and_clean/load_data_local.R")

### Load recode file 
source("./2.load_data_and_clean/load_recode_file.R")

### Data cleaning
source("./2.load_data_and_clean/cleaning.R")

### Create SES, healthy eating and physical activity based on PCA
source("./2.load_data_and_clean/ses_pca.R")
source("./2.load_data_and_clean/healthy_eating_pca.R")
source("./2.load_data_and_clean/physical_activity_pca.R")

### Select variables for descriptive and inferential analysis
source("./2.load_data_and_clean/analysis_data.R")

################################################################################

### Descriptive and Inferential stats
source("./3.statistical_analysis/descriptive_inferential_stats.R")

### Effect Size stats
source("./3.statistical_analysis/effect_size_stats.R")

### correlation and reliability stats
source("./3.statistical_analysis/reliability_correlation_tools_stats.R")

### Save stats output
source("./3.statistical_analysis/save_descriptive_inferential_output.R")
source("./3.statistical_analysis/save_effect_size_output.R")
source("./3.statistical_analysis/save_reliability_correlation_tools_output.R")

### Descriptive plots
source("./3.statistical_analysis/descriptive_plots.R")

################################################################################

## Model preparation

### Check normality and distribution of continous outcome variable
source("./4.model_preparation/check_normality_distribution.R")

### Select variables required for modelling
source("./4.model_preparation/modelling_data.R")

### Feature selection - filter method
source("./4.model_preparation/feature_selection.R")

### handle single value variables and missing values
source("./4.model_preparation/data_preparation_base.R")

### Train-test split, up-sampling and down-sampling
source("./4.model_preparation/data_partition_base.R")

################################################################################

## Multivariate Analysis

source("./5.multivariate_analysis/multivariate_analysis.R")
source("./5.multivariate_analysis/save_multivariate_analysis_output.R")
source("./5.multivariate_analysis/multivariate_analysis_plot.R")
source("./5.multivariate_analysis/multivariate_analysis_var_imp.R")

################################################################################

## Model training - Base 

### Linear - Gamma distribution
source("./6.model_training/linear_train_base.R")

### AIC - Gamma distribution
#source("./6.model_training/aic_train_base.R")

### Beta - all observations for the dependent variable are in (0, 1)
source("./6.model_training/beta_train_base.R")

### Lasso - Gamma distribution
source("./6.model_training/lasso_train_base.R")

### Ridge - Gamma distribution
source("./6.model_training/ridge_train_base.R")

### Elastic net - Gamma distribution
source("./6.model_training/enet_train_base.R")

### Random forest
source("./6.model_training/rf_train_base.R")

### Gradient Boosting Machine
source("./6.model_training/gbm_train_base.R")

### Extreme Gradient Boosting
source("./6.model_training/xgb_train_base.R")

### Support Vector Machine
source("./6.model_training/svm_train_base.R")

### Partial least squares
source("./6.model_training/pls_train_base.R")

################################################################################

## Predictive performance

### Test data
source("./7.model_performance/bootfuns_base.R")
source("./7.model_performance/resamples_test_data_base.R")

### R2 RMSE plots
source("./7.model_performance/metrics_base_plots.R")

### Residual plots
source("./7.model_performance/residuals_base_plots.R")

### Best performing model - R2
source("./7.model_performance/best_model_base.R")

################################################################################

### Variable importance
source("./8.variable_importance/varimpfuns_base.R")
source("./8.variable_importance/varimp_base.R")
source("./8.variable_importance/varimp_best_base_plots.R")

### Rank variable importance
source("./8.variable_importance/varimp_rank_base.R")
source("./8.variable_importance/varimp_rank_base_plots.R")

source("./8.variable_importance/varimp_rank_all_plots.R")

################################################################################

## Save workspace at the end without working directory path
save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_output", "output_Dir",
                                                              "local_download", "file_id", "list_folders", "list_files",
                                                              "Rdata_files")],
     file = "wdf_baseline.RData",
     envir = .GlobalEnv #parent.frame()
     )

################################################################################

## Run all files in Rstudio
source("main.R")

################################################################################

