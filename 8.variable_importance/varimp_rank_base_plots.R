library(stringr)
library(ggplot2)

working_directory
ggtheme_rank_plot()

## base rank plot
base_varfreq_plot <- sapply(unique(base_varfreq_df$analysis_name), function(x){
  nn <- x
  df <- base_varfreq_df %>%
    dplyr::filter(analysis_name == nn)
  p <- ggplot(df, aes(x=new_pos, y= stats::reorder(terms, -new_pos, FUN = mean), fill=n)) +
    geom_tile(color="black") +
    scale_fill_distiller(palette = "Greens", direction=1) +
    scale_y_discrete(expand=c(0,0)
                     ,labels = function(x) stringr::str_wrap(x, width = 65)
    ) +
    scale_x_continuous(
      breaks=function(x){1:max(x)}
      , labels=function(x){
        m <- max(x)
        v <- as.character(1:m)
        v[[m]] <- paste0(">", m-1)
        return(v)
      }
      , expand=c(0,0)
    ) +
    labs(y="", x="", fill="Frequency", title = nn)
  p
  
} , simplify=FALSE) 
  
print(base_varfreq_plot)
