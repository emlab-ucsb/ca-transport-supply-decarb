## Tracey Mangin
## February 16, 2022
## Updated well entry figs for SI

library(data.table)
library(tidyverse)
library(scales)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## data path
main_path       <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path       <- 'outputs/entry-model-results/'
save_directory  <- 'outputs/academic-out/extraction/figures/si-figs/'

## files
prod_file        <- 'well_prod_m_processed.csv'


# ## read in data
# 
# ## hold out years
# pred_wells_ho <- read_csv(paste0(data_path, "new_wells_pred_10132020_v3_oos.csv")) %>%
#   mutate(doc_field_code = paste0("00", doc_field_code),
#          doc_field_code = str_sub(doc_field_code, start= -3)) 
# 
## final model
pred_wells_fm <- fread(paste0(main_path, data_path, "new_wells_pred_revised.csv"), colClasses = c('doc_field_code' = 'character')) %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = str_sub(doc_field_code, start= -3))

# pred_wells_all <- pred_wells_fm %>%
#   rename(new_wells_pred_fm = new_wells_pred) %>%
#   left_join(pred_wells_ho) %>%
#   rename(new_wells_pred_ho = new_wells_pred) %>%
#   select(doc_field_code, year, new_wells, new_wells_pred_ho, new_wells_pred_fm) %>%
#   pivot_longer(new_wells:new_wells_pred_fm, names_to = "category", values_to = "n_wells") %>%
#   mutate(label_name = ifelse(category == "new_wells", "Observed entry",
#                              ifelse(category == "new_wells_pred_ho", "Predicted entry", "Predicted entry")))


# load historic production
well_prod_org <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), 
                       colClasses = c('api_ten_digit' = 'character', 'doc_field_code' = 'character'))

well_prod <- well_prod_org[year == 2019]

## top producers in 2019
prod2019 <- well_prod %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(rank = rank(-prod)) %>%
  filter(rank <= 10) %>%
  arrange(rank) %>%
  mutate(doc_fieldname = ifelse(doc_fieldname == "Belridge  South", "Belridge South", doc_fieldname))

# ## join
# 
# well_n_df <- pred_wells_all %>%
#   left_join(field_names) %>%
#   mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_field_name, "Non-top field"),
#          field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
#   select(doc_field_code, doc_field_name, field_name_adj, year, category, label_name, n_wells) %>%
#   group_by(field_name_adj, year, category, label_name) %>%
#   summarise(n_wells = sum(n_wells)) %>%
#   ungroup() 
# 
# state_df <- well_n_df %>%
#   mutate(doc_field_code = NA,
#          doc_field_name = "California",
#          field_name_adj = "California") %>%
#   group_by(field_name_adj, year, category, label_name) %>%
#   summarise(n_wells = sum(n_wells)) %>%
#   ungroup()
# 
# # all_well_entry <- well_n_df %>%
# #   rbind(state_df)
# 
# 
# # ## facet figure
# # field_cols <- length(unique(well_n_df$field_name_adj))
# # field_value_cols <- colorRampPalette(ucsb_distinct)(field_cols)
# # 
# ## factor
# well_n_df$field_name_adj <- factor(well_n_df$field_name_adj, levels = c(prod2019$doc_field_name, "Non-top field"))
# 
# ## hold out
# well_fig_ho <- 
#   ggplot(well_n_df %>% filter(category != "new_wells_pred_fm"), aes(x = year, y = n_wells, color = label_name)) +
#   geom_line(size = 0.6, alpha = 0.8) +
#   # labs(title = 'Observed and predicted well entry',
#   #      subtitle = 'Training data: 1978-2009') +
#   geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
#   scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
#   scale_y_continuous(label = comma) +
#   scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
#   facet_wrap(~field_name_adj, scales = "free_y") +
#   ylab("Number of wells") +
#   base_theme +
#   theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
#         legend.position = "top")
# 
# ggsave(filename =  paste0(save_directory, "predictions_oos_topfield.png"), well_fig_ho, width = 10, height = 8, units = "in", dpi = 300)
# 
# ## state hold out
# state_fig_ho <- 
#   ggplot(state_df %>% filter(category != "new_wells_pred_fm"), aes(x = year, y = n_wells, color = label_name)) +
#   geom_line(size = 0.6, alpha = 0.8) +
#   # labs(title = 'Observed and predicted well entry: California',
#   #      subtitle = 'Training data: 1978-2009') +
#   geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
#   scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
#   scale_y_continuous(label = comma) +
#   scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
#   # facet_wrap(~field_name_adj, scales = "free_y") +
#   ylab("Number of wells") +
#   base_theme +
#   theme(legend.position = "top")
# 
# ggsave(filename =  paste0(save_directory, "predictions_oos_state.png"), state_fig_ho, width = 5, height = 4, units = "in", dpi = 300)
# 
# 
# ## full data set
# well_fig_fs <- 
#   ggplot(well_n_df %>% filter(category != "new_wells_pred_ho"), aes(x = year, y = n_wells, color = label_name)) +
#   geom_line(size = 0.6, alpha = 0.8) +
#   # labs(title = 'Observed and predicted well entry',
#   #      subtitle = 'Training data: 1978-2020') +
#   # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
#   scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
#   scale_y_continuous(label = comma) +
#   scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
#   facet_wrap(~field_name_adj, scales = "free_y") +
#   ylab("Number of wells") +
#   base_theme +
#   theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
#         legend.position = "top")
# 
# ggsave(filename =  paste0(save_directory, "predictions_fullsample_topfield.png"), well_fig_fs, width = 10, height = 8, units = "in", dpi = 300)
# 
# ## state hold out
# state_fig_fs <- 
#   ggplot(state_df %>% filter(category != "new_wells_pred_ho"), aes(x = year, y = n_wells, color = label_name)) +
#   geom_line(size = 0.6, alpha = 0.8) +
#   # labs(title = 'Observed and predicted well entry: California',
#   #      subtitle = 'Training data: 1978-2020') +
#   # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
#   scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
#   scale_y_continuous(label = comma) +
#   scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
#   # facet_wrap(~field_name_adj, scales = "free_y") +
#   ylab("Number of wells") +
#   base_theme +
#   theme(legend.position = "top")
# 
# ggsave(filename =  paste0(save_directory, "predictions_fullsample_state.png"), state_fig_fs, width = 5, height = 4, units = "in", dpi = 300)
# 
# 
# 
# 
