library(dplyr)
library(ggplot2)
library(gridExtra)

working_directory

ggtheme_descriptive_plot(textsize_yaxis=8.5, textsize_xaxis=8.5)

## Simple plots and Multiple response plots - Diabetes Individuals

descriptive_plot <- sapply(unique(selected_vars_df$plot[selected_vars_df$plot != "none"]), function(x){
  nn <- x
  index <- selected_vars_df$new_variable[selected_vars_df$plot == nn]
  n_index <- length(index)
  df_new <- df_analysis
  
  plots <- if ( nn == "single 1" | nn == "single 2" | nn == "single 4" ) {
    p <- simple_plot_list(df= df_new, vars = index, text_label_size = 2.3, 
                          x_axis_label_wrap_width = 10, expand_yaxis = c(0.01,0.07))
    
    grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 3))
                    )
    } else if ( nn == "single 5" ) {
      p <- simple_plot_list(df= df_new, vars = index, text_label_size = 2.3,
                            x_axis_label_wrap_width = 10, expand_yaxis = c(0.01,0.07))
      
      grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 4))
                      )  
      } else if ( nn == "single 3" ) {
        p <- simple_plot_list(df= df_new, vars = index, text_label_size = 2.3,
                              x_axis_label_wrap_width = 10, expand_yaxis = c(0.01,0.07))
        
        grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 2))
        )  
        } else if (nn == "medication" | nn == "tobacco use" | nn == "alcohol use" | nn == "alcohol disuse") {
          multires_count_plot(df= df_new, 
                              vars = index,
                              filter_level = "Yes",
                              y_axis_breaks = 10,
                              text_label_size = 2.7,
                              expand_yaxis = c(0.01,0.08),
                              title_label = nn
                              )  
          } else if (nn == "history of medical conditions") {
            multires_count_plot(df= df_new, 
                                vars = index,
                                filter_level = "Yes",
                                y_axis_breaks = 10,
                                text_label_size = 2.7,
                                x_axis_label_wrap_width = 30,
                                expand_yaxis = c(0.01,0.08),
                                title_label = NULL
                                )  
            } else {
              multires_stack_plot(df= df_new, 
                                  vars = index,
                                  text_angle = 90,
                                  text_label_size = 2.5,
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


