library(dplyr)
library(forcats)
library(DALEX)
library(ggplot2)
library(gridExtra)
library(ggpubr)

working_directory
## Variable importance base 

base_varimp_plot <- sapply(unique(base_varimp_all_df$analysis_name), function(x){
  nn <- x
  ## Creating a named vector to quickly rename levels
  new_levels <- selected_vars_df %>%
    dplyr::select(new_label, new_variable) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(new_label = gsub(" grouped| type", "", new_label)) %>%
    dplyr::filter(new_variable %in% names(test1)) %>%
    tibble::deframe()
  
  df <- base_varimp_all_df %>%
    dplyr::mutate(variable = as.character(forcats::fct_recode(variable, !!!new_levels))) %>%
    dplyr::filter(analysis_name == nn, 
                  label %in% best_model_base_df$label[best_model_base_df$analysis_name == nn]) %>%
    arrange(desc(R2)) %>%
    mutate(label = forcats::as_factor(label))
  
  p <- sapply(unique(df$label), function(y){
    plot(df %>% dplyr::filter(analysis_name == nn, label == y)
         , show_boxplots = TRUE
         , bar_width = 3 #default 10
         , desc_sorting = TRUE
         , title = "" #default 'Feature Importance'
         , subtitle = ""
         ) +
      labs(x = "", y = "", title = "") +
      scale_y_continuous(expand = expansion(mult = c(0.02,0.05)))
      
  }, simplify=FALSE)
  
  grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 2))
                  )
  
  annotate_grid <- ggpubr::annotate_figure( 
    grid,
    top = text_grob(nn, color = "navyblue", face = "bold", size = 12),
    bottom = text_grob("Variable Importance", color = "navyblue", face = "bold", size = 12),
  )
  
} , simplify=FALSE)

print(base_varimp_plot)

### Saving varimp base plots using loops
for (j in seq(length(base_varimp_plot))) {
  ggsave(plot=base_varimp_plot[[j]], height = 8, width = 12,
         filename = paste0("base_varimp_plot_",names(base_varimp_plot)[[j]],".png"),
         path = output_Dir, bg='white')  
}

