## Tracey Mangin
## March 8, 2022
## well entry and exit figures

library(data.table)
library(tidyverse)
library(scales)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items


## data path
main_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path         <- paste0(main_path, 'outputs/entry-model-results/')
save_directory    <- paste0(main_path, 'outputs/academic-out/extraction/figures/si-figs/')

## files
prod_file           <- 'well_prod_m_processed.csv'


## read in data

# ## hold out years
# pred_wells_ho <- read_csv(paste0(data_path, "new_wells_pred_10132020_v3_oos.csv")) %>%
#   mutate(doc_field_code = paste0("00", doc_field_code),
#          doc_field_code = str_sub(doc_field_code, start= -3)) 

## final model
pred_wells_fm <- read_csv(paste0(data_path, "new_wells_pred_revised.csv")) %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = as.character(str_sub(doc_field_code, start= -3))) 

# pred_wells_all <- pred_wells_fm %>%
#   rename(new_wells_pred_fm = new_wells_pred) %>%
#   left_join(pred_wells_ho) %>%
#   rename(new_wells_pred_ho = new_wells_pred) %>%
#   select(doc_field_code, year, new_wells, new_wells_pred_ho, new_wells_pred_fm) %>%
#   pivot_longer(new_wells:new_wells_pred_fm, names_to = "category", values_to = "n_wells") %>%
#   mutate(label_name = ifelse(category == "new_wells", "Observed entry",
#                              ifelse(category == "new_wells_pred_ho", "Predicted entry", "Predicted entry")))


# load historic production
well_prod_org <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

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

## field name
field_name <- well_prod_org %>%
  select(doc_field_code, doc_fieldname) %>%
  unique()


## join
# well_n_df <- pred_wells_all %>%
#   left_join(field_names) %>%
#   mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_field_name, "Non-top field"),
#          field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
#   select(doc_field_code, doc_field_name, field_name_adj, year, category, label_name, n_wells) %>%
#   group_by(field_name_adj, year, category, label_name) %>%
#   summarise(n_wells = sum(n_wells)) %>%
#   ungroup() 

well_n_df_revised <- pred_wells_fm %>%
  left_join(field_name) %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_fieldname, "Non-top fields"),
         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  pivot_longer(new_wells:new_wells_pred, names_to = "category", values_to = "n_wells") %>%
  mutate(label_name = ifelse(category == "new_wells", "Observed entry", "Predicted entry")) %>%
  select(doc_field_code, doc_fieldname, field_name_adj, year, category, label_name, n_wells) %>%
  group_by(field_name_adj, year, category, label_name) %>%
  summarise(n_wells = sum(n_wells)) %>%
  ungroup() 

state_df <- well_n_df_revised %>%
  mutate(field_name_adj = "California") %>%
  group_by(field_name_adj, year, category, label_name) %>%
  summarise(n_wells = sum(n_wells)) %>%
  ungroup()

# all_well_entry <- well_n_df %>%
#   rbind(state_df)


# ## facet figure
# field_cols <- length(unique(well_n_df$field_name_adj))
# field_value_cols <- colorRampPalette(ucsb_distinct)(field_cols)
# 
## factor
well_n_df_revised$field_name_adj <- factor(well_n_df_revised$field_name_adj, levels = c("Belridge South",
                                                                                        "Midway-Sunset",
                                                                                        "Kern River",
                                                                                        "Cymric",
                                                                                        "Wilmington",
                                                                                        "Lost Hills",
                                                                                        "San Ardo",
                                                                                        "Elk Hills",
                                                                                        "Coalinga",
                                                                                        "Poso Creek",
                                                                                        "Non-top fields"))


## full data set
well_fig_fs <- 
  ggplot(well_n_df_revised %>% filter(category != "new_wells_pred_ho"), aes(x = year, y = n_wells, color = label_name)) +
  geom_line(size = 0.6, alpha = 0.8) +
  labs(y = "Number of wells",
       x = NULL,
       color = NULL) +
  # labs(title = 'Observed and predicted well entry',
  #      subtitle = 'Training data: 1978-2020') +
  # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
  scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
  scale_y_continuous(label = comma) +
  # scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
  facet_wrap(~field_name_adj, ncol = 3, scales = "free_y") +
  ylab("Number of wells") +
  theme_line +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "top")

ggsave(filename =  paste0(save_directory, "pred_fullsample_topfield.png"), well_fig_fs, width = 8, height = 10, units = "in", dpi = 300)

## full data set
well_fig_fs_fixed <- 
  ggplot(well_n_df_revised %>% filter(category != "new_wells_pred_ho"), aes(x = year, y = n_wells, color = label_name)) +
  geom_line(size = 0.6, alpha = 0.8) +
  labs(y = "Number of wells",
       x = NULL,
       color = NULL) +
  # labs(title = 'Observed and predicted well entry',
  #      subtitle = 'Training data: 1978-2020') +
  # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
  scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
  scale_y_continuous(label = comma) +
  # scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
  facet_wrap(~field_name_adj, ncol = 3) +
  ylab("Number of wells") +
  theme_line +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "top")

ggsave(filename =  paste0(save_directory, "pred_fullsample_topfield.png"), well_fig_fs, width = 8, height = 10, units = "in", dpi = 300)


## state no hold out
state_fig_fs <- 
  ggplot(state_df %>% filter(category != "new_wells_pred_ho"), aes(x = year, y = n_wells, color = label_name)) +
  geom_line(size = 0.6, alpha = 0.8) +
  labs(y = "Number of wells",
       x = NULL,
       color = NULL) +
  # labs(title = 'Observed and predicted well entry: California',
  #      subtitle = 'Training data: 1978-2020') +
  # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
  scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
  scale_y_continuous(label = comma, limits = c(0, 4000)) +
  # scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
  # facet_wrap(~field_name_adj, scales = "free_y") +
  ylab("Number of wells") +
  theme_line +
  theme(legend.position = "top")

ggsave(filename =  paste0(save_directory, "pred_fullsample_state.png"), state_fig_fs, width = 5, height = 4, units = "in", dpi = 300)

## exit model
## -------------------------------------------

exit_pred_df <- read_csv(paste0(main_path, 'outputs/exit/well_exits_pred.csv'))  %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = as.character(str_sub(doc_field_code, start= -3))) 
## 
well_exit_df2 <- exit_pred_df %>%
  left_join(field_name) %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_fieldname, "Non-top fields"),
         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  pivot_longer(well_exits:well_exits_pred, names_to = "category", values_to = "n_wells") %>%
  mutate(label_name = ifelse(category == "well_exits", "Observed exit", "Predicted exit")) %>%
  select(doc_field_code, doc_fieldname, field_name_adj, year, category, label_name, n_wells) %>%
  group_by(field_name_adj, year, category, label_name) %>%
  summarise(n_wells = sum(n_wells)) %>%
  ungroup() 

state_exit_df <- well_exit_df2 %>%
  mutate(field_name_adj = "California") %>%
  group_by(field_name_adj, year, category, label_name) %>%
  summarise(n_wells = sum(n_wells)) %>%
  ungroup()


## factor
well_exit_df2$field_name_adj <- factor(well_exit_df2$field_name_adj, levels = c("Belridge South",
                                                                              "Midway-Sunset",
                                                                              "Kern River",
                                                                              "Cymric",
                                                                              "Wilmington",
                                                                              "Lost Hills",
                                                                              "San Ardo",
                                                                              "Elk Hills",
                                                                              "Coalinga",
                                                                              "Poso Creek",
                                                                              "Non-top fields"))


## full data set
well_exit_fs <- 
  ggplot(well_exit_df2, aes(x = year, y = n_wells, color = label_name)) +
  geom_line(size = 0.6, alpha = 0.8) +
  labs(y = "Number of well exits",
       x = NULL,
       color = NULL) +
  # labs(title = 'Observed and predicted well entry',
  #      subtitle = 'Training data: 1978-2020') +
  # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
  scale_x_continuous(limits = c(1978, 2010), breaks=c(1978, seq(1990, 2010, 10))) +
  scale_y_continuous(label = comma) +
  # scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
  facet_wrap(~field_name_adj, ncol = 3, scales = "free_y") +
  theme_line +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "top")

ggsave(filename =  paste0(save_directory, "pred_exit_topfield.png"), well_exit_fs, width = 8, height = 10, units = "in", dpi = 300)

## fixed y axis
well_exit_fs <- 
  ggplot(well_exit_df2, aes(x = year, y = n_wells, color = label_name)) +
  geom_line(size = 0.6, alpha = 0.8) +
  labs(y = "Number of well exits",
       x = NULL,
       color = NULL) +
  # labs(title = 'Observed and predicted well entry',
  #      subtitle = 'Training data: 1978-2020') +
  # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
  scale_x_continuous(limits = c(1978, 2010), breaks=c(1978, seq(1990, 2010, 10))) +
  scale_y_continuous(label = comma) +
  # scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
  facet_wrap(~field_name_adj, ncol = 3) +
  theme_line +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "top")




## state no hold out
state_fig_exit <- 
  ggplot(state_exit_df, aes(x = year, y = n_wells, color = label_name)) +
  geom_line(size = 0.6, alpha = 0.8) +
  labs(y = "Number of well exits",
       x = NULL,
       color = NULL) +
  # labs(title = 'Observed and predicted well entry: California',
  #      subtitle = 'Training data: 1978-2020') +
  # geom_vline(xintercept = 2009, lty = "dashed", size = 0.5, color = "black") +
  scale_x_continuous(limits = c(1978, 2010), breaks=c(1978, seq(1990,2020,10))) +
  scale_y_continuous(label = comma, limits = c(0, 3500)) +
  # scale_color_manual(values = c("black", ucsb_pal_prim2[1])) +
  # facet_wrap(~field_name_adj, scales = "free_y") +
  theme_line +
  theme(legend.position = "top")

ggsave(filename =  paste0(save_directory, "pred_exit_state.png"), state_fig_exit, width = 5, height = 4, units = "in", dpi = 300)









