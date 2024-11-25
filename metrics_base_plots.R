library(tidytext)
library(forcats)
library(dplyr)
library(ggplot2)

working_directory
ggtheme_rank_plot()

metrics_base_plots <- ggplot(base_metrics_all_df %>% 
         dplyr::mutate(analysis_name = forcats::as_factor(analysis_name)) %>%
         filter(scores == "R2" | scores == "RMSE") %>%
         mutate(label= tidytext::reorder_within(label, -estimate, list(analysis_name, scores)))
         , aes(x=label, y=estimate, colour=scores)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(0.5),
                  na.rm = TRUE,
                  fatten = 2
                  ) +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(analysis_name,scores), scales="free", ncol = 2, 
             labeller = labeller(.default = label_value, .multi_line = FALSE)) + 
  labs(y = "Estimate", x = "Model") + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(metrics_base_plots)


ggsave(plot=metrics_base_plots, height = 7, width = 12,
       filename = "base_metrics_plots.png", path = output_Dir, bg='white')
