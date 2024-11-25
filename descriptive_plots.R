library(dplyr)
library(ggplot2)
library(gridExtra)

working_directory

ggtheme_descriptive_plot()

## Simple plots and Multiple response plots - Diabetes Individuals

descriptive_plot <- sapply(unique(selected_vars_df$plot[selected_vars_df$plot != "none"]), function(x){
  nn <- x
  index <- selected_vars_df$new_variable[selected_vars_df$plot == nn]
  n_index <- length(index)
  df_new <- df_analysis
  
  plots <- if ( nn == "single 1" | nn == "single 2" | nn == "single 4" ) {
    p <- single_plot(df= df_new, variable = index, text_label_size = 2.3)
    
    grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 3))
                    )
    } else if ( nn == "single 5" ) {
      p <- single_plot(df= df_new, variable = index, text_label_size = 2.3)
      
      grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 4))
                      )  
      } else if ( nn == "single 3" ) {
        p <- single_plot(df= df_new, variable = index, text_label_size = 2.3)
        
        grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 2))
        )  
        } else if (nn == "medication" | nn == "tobacco use" | nn == "alcohol use" | nn == "alcohol disuse") {
          multires_count_plot(df= df_new, 
                              variable = index,
                              filter_level = "Yes",
                              y_axis_breaks = 10,
                              expand_yaxis = c(0.01,0.07),
                              title_label = nn
                              )  
          } else if (nn == "history of medical conditions") {
            multires_count_plot(df= df_new, 
                                variable = index,
                                filter_level = "Yes",
                                y_axis_breaks = 10,
                                x_axis_label_wrap_width = 30,
                                expand_yaxis = c(0.01,0.07),
                                title_label = NULL
                                )  
            } else {
              multires_stack_plot(df= df_new, 
                                  variable = index,
                                  text_angle = 90,
                                  reverse_xaxis = TRUE,
                                  title_label = nn
                                  )
            }
  
  plots
  
}, simplify = TRUE
)

### Saving descriptive simple plots using loops
for (i in seq(length(descriptive_plot))) {
  ggsave(plot=descriptive_plot[[i]], height = 6.5, width = 11,
         filename = paste0("descriptive_plot_",names(descriptive_plot)[[i]],".png"),
         path = output_Dir, bg='white')  
}


