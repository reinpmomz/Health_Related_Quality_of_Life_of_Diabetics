library(tidytext)
library(forcats)
library(dplyr)
library(ggplot2)
library(ggpubr)

working_directory
ggtheme_rank_plot()

## Base Package ROC plots
residuals_base_plots <-  sapply(unique(base_residuals_all_df$analysis_name), function(x){
  nn <- x
  df <- base_residuals_all_df %>%
    dplyr::filter(analysis_name == nn) %>%
    arrange(desc(R2_estimate), label, number) %>%
    dplyr::mutate(across(c(label, label_R2), ~forcats::as_factor(.x)))
  
  plot <- if (nn == "health satisfaction") {
    ggplot(df, aes(x=predicted_estimate, y=residual_estimate)) +
      geom_point(colour = "red2")
    } else if (nn == "general qol") {
    ggplot(df, aes(x=predicted_estimate, y=residual_estimate)) +
        geom_point(colour = "blue2")
      } else { 
    ggplot(df, aes(x=predicted_estimate, y=residual_estimate)) +
          geom_point(colour = "green3")
      }
  
  plot1 <- plot +
    geom_hline(yintercept = 0, colour = "darkgrey", linetype = 2) +
    scale_x_continuous(n.breaks = 6) +
    #scale_y_continuous(n.breaks = 5) +
    facet_wrap(analysis_name~label_R2, scales="fixed", ncol = 5, 
               labeller = labeller(.default = label_value, .multi_line = FALSE)) + 
    labs(y = NULL, x = NULL) + 
    theme(legend.position="none",
          axis.text.x = element_text(angle = 0, vjust = 0.5))
  
}, simplify=FALSE)

residuals_base_grid_plots <- ggpubr::annotate_figure(
  p = ggpubr::ggarrange(plotlist = residuals_base_plots,
                        ncol = 1,
                        nrow = NULL),
  top = NULL,
  left = text_grob("Residuals", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Predicted", color = "black", face = "bold", size = 12)
)

print(residuals_base_grid_plots)

ggsave(plot=residuals_base_grid_plots, height = 7, width = 13,
       filename = "base_residuals_plots.png", path = output_Dir, bg='white')
