library(dplyr)
library(broom)
library(tidyr)
library(performance)
library(stringr)
library(forcats)
library(betareg)
library(ggplot2)

working_directory

## Regression plot
multivariate_analysis_plot <- sapply(outcome_vars, function(x){
  
  df_new <- df_drop_corr
  
  ## Creating a named vector to quickly rename levels
  new_levels <- selected_vars_df %>%
    dplyr::select(new_label, new_variable) %>%
    tidyr::drop_na() %>%
    dplyr::filter(new_variable %in% names(df_new)) %>%
    tibble::deframe()
  
  model_metrics <- bind_rows(broom::glance(multivariate_analysis_model[[x]]) %>%
                               dplyr::mutate(across(!starts_with("df"), ~round(.x, 3))) %>%
                               tidyr::pivot_longer(everything()),
                             performance::model_performance(multivariate_analysis_model[[x]]) %>%
                               dplyr::mutate(across(!starts_with("df"), ~round(.x, 3))) %>%
                               tidyr::pivot_longer(everything())
                             ) %>%
    dplyr::distinct(name, .keep_all = TRUE) %>%
    dplyr::filter(name != "R2") %>%
    dplyr::mutate(name_value = paste0(name, " = ", value)) %>%
    dplyr::pull(name_value)
  
  model_table <- broom::tidy(x = multivariate_analysis_model[[x]]) %>%
    dplyr::mutate(p.value = round(p.value, 3)
                  , p_value = ifelse(p.value < 0.001, "<0.001", p.value)
                  , lower.estimate = estimate-(1.96*std.error)
                  , upper.estimate = estimate+(1.96*std.error)
                  , variable_name = stringr::str_extract(term, paste(names(df_new), sep= "", collapse = '|'))
                  , variable_label = as.character(forcats::fct_recode(variable_name, !!!new_levels))
                  , term_label = gsub(paste(names(df_new), sep= "", collapse = '|'), "", term)
                  , term_label = gsub("Yes", NA, term_label)
                  , term_label = dplyr::na_if(term_label, "")
                  , term_label = if_else(is.na(term_label),variable_label ,term_label)
                  , model = "beta"
                  ) 
    
  model_table_metrics <- model_table %>%
    dplyr::filter(is.na(variable_name)) %>%
    dplyr::mutate(across(!starts_with("p") & where(is.numeric), ~round(.x, 3))
                  , term_label = gsub("\\(|)", "", term_label)
                  , term_label_value = paste0(term_label, " = ", estimate, " (", lower.estimate, ", ", upper.estimate, ")"
                                              )
                  , term_label_pvalue = paste0(term_label, " p-value = ", p_value)
                  ) %>%
    dplyr::select(term_label_value, term_label_pvalue) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::pull(value)
  
  model_table_new <- model_table %>%
    tidyr::drop_na(variable_name) %>%
    dplyr::mutate(variable_label_new = gsub(" grouped| type", "", variable_label)
                  , term_label_new = if_else(variable_label != term_label , 
                                         paste0(term_label, " ", variable_label_new), term_label)
                  , term_label_new = forcats::as_factor(term_label_new)
                  )
  
  ggtheme_regression_plot()
  
  plot <- ggplot(model_table_new, aes(x=forcats::fct_rev(term_label_new), y=estimate, colour=model)) + 
    geom_pointrange(aes(ymin = lower.estimate, ymax = upper.estimate),
                    stat = "identity",
                    position = position_dodge(0.5),
                    na.rm = TRUE,
                    fatten = 0.5,
                    show.legend = FALSE
                    ) +
    coord_flip() +
    geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    labs(x=NULL,y="Beta Coefficients (95% CI)", colour = "", title = ""
         #, caption = stringr::str_wrap(paste0(c(model_metrics, model_table_metrics), collapse = "; "), width = 95)
         ) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60)) +
    scale_y_continuous( n.breaks = 10)
  
  print(plot)
  
}, simplify=FALSE)


### Saving Regression plot using loops
for (i in seq(length(multivariate_analysis_plot))) {
  ggsave(plot=multivariate_analysis_plot[[i]], height = 6, width = 9,
         filename = paste0("regression_plot_",names(multivariate_analysis_plot)[[i]],".png"),
         path = output_Dir, bg='white')  
}



