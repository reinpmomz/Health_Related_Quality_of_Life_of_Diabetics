library(forcats)
library(tidyr)
library(dplyr)

working_directory

base_top_n <- 10
base_total_n <- as.numeric(ncol(test1[, !names(test1) %in% outcome_vars]))

# Mostly frequently identified variables

## Base Package variable importance df
base_varfreq_df <- base_varimp_all_df %>%
                        left_join(final_attribute %>%
                                    dplyr::select(variable, label) %>%
                                    dplyr::rename(terms = label),
                                  by = c("variable")
                                  ) %>%
                        dplyr::mutate(terms = gsub(" grouped| type", "", terms)) %>%
                        tidyr::drop_na(terms) %>%
                        group_by(variable, label, analysis, analysis_name, terms) %>%
                        summarise(overall = mean(dropout_loss, na.rm=TRUE), .groups = "drop") %>%
                        group_by(analysis, analysis_name, label) %>%
                        arrange(desc(overall), .by_group=TRUE) %>%
                        mutate(pos = 1:n()) %>%
                        ungroup() %>% 
                        mutate(NULL
                               , new_pos=ifelse(pos<=base_top_n, pos, base_top_n+1)
                               #, new_terms=fct_reorder(terms, new_pos, mean)
                        ) %>% 
                        #filter(as.numeric(new_terms) <= base_total_n) %>% 
                        group_by(analysis, analysis_name, terms, new_pos) %>% 
                        count() %>% 
                        ungroup() %>%
                        droplevels()

