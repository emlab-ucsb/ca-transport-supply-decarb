## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)
library(extrafont)


## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/manuscript-update/figs/review/'

## files to compare
prev <- 'outputs/stocks-flows/field_capex_opex_forecast_final.csv'
prev_his <- 'outputs/stocks-flows/entry_df_final.csv'

curr <- 'outputs/stocks-flows/entry-input-df/final/field_capex_opex_forecast_revised.csv'
curr_his <- 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'

## rystad imputed cost
rystad_curr <- fread(paste0(main_path, "outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed_all_assets.csv"))
rystad_prev <- fread(paste0(main_path, "outputs/stocks-flows/rystad-imputed-cost/Rystad_cost_imputed_10132020_v3.csv"))

## well prod
prod_file <- 'data/stocks-flows/processed/well_prod_m_processed.csv'


## read in dfs
cost_prev <- fread(paste0(main_path, prev))
cost_curr <- fread(paste0(main_path, curr))
hist_prev <- fread(paste0(main_path, prev_his))
hist_curr <- fread(paste0(main_path, curr_his))

## well production
well_prod <- fread(paste0(main_path, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                               'doc_field_code' = 'character'))

## 2019 field production
prod2019 <- well_prod %>%
  filter(year == 2019) %>%
  group_by(doc_field_code) %>%
  summarise(prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

## field names 
field_names <- well_prod %>%
  select(doc_field_code, doc_fieldname) %>%
  unique()

## pad
cost_prev[, doc_field_code := sprintf("%03s", doc_field_code)]
cost_curr[, doc_field_code := sprintf("%03s", doc_field_code)]
hist_prev[, doc_field_code := sprintf("%03s", doc_field_code)]
hist_curr[, doc_field_code := sprintf("%03s", doc_field_code)]


## prepare cost for joining
cost_prev_join <- cost_prev %>%
  select(doc_field_code, year, m_opex_imputed_prev = m_opex_imputed, m_capex_imputed_prev = m_capex_imputed)

cost_comp <- cost_curr %>%
  select(doc_field_code, year, m_opex_imputed_curr = m_opex_imputed, m_capex_imputed_curr = m_capex_imputed) %>%
  left_join(cost_prev_join) %>%
  left_join(field_names) %>%
  left_join(prod2019) %>%
  mutate(prod_bbl = ifelse(is.na(prod_bbl), 0, prod_bbl),
         diff_opex = m_opex_imputed_curr - m_opex_imputed_prev,
         diff_capex = m_capex_imputed_curr - m_capex_imputed_prev)

cost_comp_long <- cost_comp %>%
  select(doc_field_code, doc_fieldname, year, prod_bbl, m_opex_imputed_curr, m_opex_imputed_prev, m_capex_imputed_curr, m_capex_imputed_prev, diff_opex, diff_capex) %>%
  pivot_longer(m_opex_imputed_curr:diff_capex,
               names_to = 'cost_type',
               values_to = 'usd') %>%
  mutate(version = ifelse(cost_type %in% c('m_opex_imputed_curr', 'm_capex_imputed_curr'), "current",
                          ifelse(cost_type %in% c('m_opex_imputed_prev', 'm_capex_imputed_prev'), "prev", "diff")),
         cost = ifelse(cost_type %in% c('m_opex_imputed_curr',
                                        'm_opex_imputed_prev',
                                        'diff_opex'), "opex", "capex"))

## any with no changes?
zero_diff <- cost_comp_long %>%
  filter(doc_field_code != 848) %>%
  filter(cost_type %in% c('diff_opex', 'diff_capex')) %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(diff_all = sum(usd)) %>%
  ungroup() %>%
  mutate(zero = ifelse(diff_all == 0, doc_field_code, "diff"),
         non_zero_n = ifelse(diff_all == 0, 0, 1))

## filter out fields with no changes
cost_comp_long_filt <- cost_comp_long %>%
  filter(!doc_field_code %in% zero_diff$zero) 

## for difference
cost_comp_long_diff <- cost_comp_long %>%
  filter(version == "diff") %>%
  group_by(doc_field_code, cost) %>%
  mutate(sum = sum(usd)) %>%
  ungroup() %>%
  filter(sum != 0) %>%
  select(-sum) %>%
  mutate(prod_lab = round(prod_bbl / 1e6, 2))

## fig 1: forecast comparison
opex_comp_fig <- ggplot(cost_comp_long_filt %>% filter(cost_type %in% c('m_opex_imputed_curr',
                                                                        'm_opex_imputed_prev')), 
                        aes(x = year, y = usd, color = doc_fieldname, lty = version)) +
  geom_line(size = 0.5, alpha = 0.8) +
  theme(legend.position = 'none')

plotly::ggplotly(opex_comp_fig)


capex_comp_fig <- ggplot(cost_comp_long_filt %>% filter(cost_type %in% c('m_capex_imputed_curr',
                                                                        'm_capex_imputed_prev')), 
                        aes(x = year, y = usd, color = doc_fieldname, lty = version)) +
  geom_line(size = 0.5, alpha = 0.8) +
  theme(legend.position = 'none')

plotly::ggplotly(capex_comp_fig)

## fig 2: difference comparison

opex_diff_fig <- ggplot(cost_comp_long_diff %>% filter(cost_type == 'diff_opex'), 
                         aes(x = year, y = usd, color = doc_fieldname)) +
  geom_line(size = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_point(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_opex', prod_lab != 0),
             aes(x = year, y = usd, size = prod_lab), alpha = 0.5) +
  # geom_text(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_opex', prod_lab > 0.5),
  #           aes(x = year, y = usd, label = prod_lab, size = prod_lab, hjust = -0.5)) +
  scale_radius(range = c(1,4)) +
  labs(title = 'Opex projection comparison',
       subtitle = 'Bubble size and represnts 2019 production (million bbls)',
       x = NULL,
       y = 'Opex: delta USD (updated - old)') +
  theme_bw() +
  theme(legend.position = 'none') 

plotly::ggplotly(opex_diff_fig)

opex_diff_filt_fig <- ggplot(cost_comp_long_diff %>% filter(cost_type == 'diff_opex',
                                                            prod_bbl != 0), 
                        aes(x = year, y = usd, color = doc_fieldname)) +
  geom_line(size = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_point(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_opex', prod_lab != 0),
             aes(x = year, y = usd, size = prod_lab), alpha = 0.5) +
  geom_text(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_opex', prod_lab > 0.5),
            aes(x = year, y = usd, label = prod_lab, size = prod_lab, hjust = -0.5)) +
  scale_radius(range = c(1,4)) +
  labs(title = 'Opex projection comparison',
       subtitle = 'Bubble size and value represent 2019 production (million bbls)\nFields with zero 2019 production removed',
       x = NULL,
       y = 'Opex: delta USD (updated - old)') +
  scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2050)) +
  theme_bw() +
  theme(legend.position = 'none')



## capex
capex_diff_fig <- ggplot(cost_comp_long_diff %>% filter(cost_type == 'diff_capex'), 
                        aes(x = year, y = usd, color = doc_fieldname)) +
  geom_line(size = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_point(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_capex', prod_lab != 0),
             aes(x = year, y = usd, size = prod_lab), alpha = 0.5) +
  # geom_text(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_opex', prod_lab > 0.5),
  #           aes(x = year, y = usd, label = prod_lab, size = prod_lab, hjust = -0.5)) +
  scale_radius(range = c(1,4)) +
  labs(title = 'Capex projection comparison',
       subtitle = 'Bubble size and represnts 2019 production (million bbls)',
       x = NULL,
       y = 'Capex: delta USD (updated - old)') +
  theme_bw() +
  theme(legend.position = 'none') 

plotly::ggplotly(capex_diff_fig)

capex_diff_filt_fig <- ggplot(cost_comp_long_diff %>% filter(cost_type == 'diff_capex',
                                                            prod_bbl != 0), 
                             aes(x = year, y = usd, color = doc_fieldname)) +
  geom_line(size = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_point(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_capex', prod_lab != 0),
             aes(x = year, y = usd, size = prod_lab), alpha = 0.5) +
  geom_text(data = cost_comp_long_diff %>% filter(year == 2045, cost_type == 'diff_capex', prod_lab > 0.5),
            aes(x = year, y = usd, label = prod_lab, size = prod_lab, hjust = -0.5)) +
  scale_radius(range = c(1,4)) +
  labs(title = 'Capex projection comparison',
       subtitle = 'Bubble size and value represent 2019 production (million bbls)\nFields with zero 2019 production removed',
       x = NULL,
       y = 'Capex: delta USD (updated - old)') +
  scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2050)) +
  theme_bw() +
  theme(legend.position = 'none')

plotly::ggplotly(capex_diff_filt_fig)

opex_plot_grid <- plot_grid(
  opex_diff_fig,
  opex_diff_filt_fig + labs(y = NULL),
  align = 'vh',
  # labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 1,
  rel_widths = c(1, 1)
)

capex_plot_grid <- plot_grid(
  capex_diff_fig,
  capex_diff_filt_fig + labs(y = NULL),
  align = 'vh',
  # labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 1,
  rel_widths = c(1, 1)
)


## save figure 3
ggsave(capex_plot_grid,
       filename = file.path(main_path, fig_path, 'capex_comparison.pdf'),
       width = 16,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'capex_comparison.pdf'),
            outfile = paste0(main_path, fig_path, 'capex_comparison.pdf'))


ggsave(opex_plot_grid,
       filename = file.path(main_path, fig_path, 'opex_comparison.pdf'),
       width = 16,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'opex_comparison.pdf'),
            outfile = paste0(main_path, fig_path, 'opex_comparison.pdf'))

## capex and opex trajectories
## ---------------------------------------

capex_opex_traj <- ggplot(cost_comp_long %>% filter(version != 'diff') %>%
                            mutate(facet_lab = ifelse(version == 'current', "Updated", "Previous")), 
                         aes(x = year, y = usd, group = doc_field_code)) +
  geom_line(size = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  facet_grid(cost ~ facet_lab) +
  labs(title = 'Capex and opex projections',
       x = NULL,
       y = 'USD') +
  theme_bw() +
  theme(legend.position = 'none') 

## save figure
ggsave(capex_opex_traj,
       filename = file.path(main_path, fig_path, 'capex_opex_proj_all_fields.pdf'),
       width = 10,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'capex_opex_proj_all_fields.pdf'),
            outfile = paste0(main_path, fig_path, 'capex_opex_proj_all_fields.pdf'))



## version in si
topten <- prod2019 %>%
  mutate(rank = rank(-prod_bbl)) %>%
  filter(rank <= 10) %>%
  arrange(rank) %>%
  left_join(field_names) %>%
  mutate(doc_fieldname = ifelse(doc_fieldname == "Belridge  South", "Belridge South", doc_fieldname)) 

cost_summaries <- cost_comp_long %>%
  filter(doc_field_code != '848') %>%
  filter(version != 'diff') %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% topten$doc_field_code, doc_fieldname, "Non-top field"),
                         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  group_by(field_name_adj, cost_type, version, cost, year) %>%
  summarise(usd = mean(usd),
            usd = mean(usd)) %>%
  ungroup() %>%
  mutate(field_type = ifelse(field_name_adj == 'Non-top field', 'Non-top field', 'Top ten field'))


## figure
topten_traj <- ggplot(cost_summaries %>% mutate(facet_lab = ifelse(version == 'current', "Updated", "Previous")), 
                          aes(x = year, y = usd, color = field_name_adj, lty = field_type)) +
  geom_line(size = 0.5, alpha = 0.9) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  facet_grid(cost ~ facet_lab, scales = 'free_y') +
  labs(title = 'Capex and opex projections',
       color = NULL,
       lty = NULL,
       x = NULL,
       y = 'USD') +
  scale_linetype_manual(values = c('dotted', 'solid'),
                        guide = "none") +
  theme_bw() +
  theme(legend.position = 'top') 

## save figure
ggsave(topten_traj,
       filename = file.path(main_path, fig_path, 'capex_opex_proj_top_fields.pdf'),
       width = 10,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'capex_opex_proj_top_fields.pdf'),
            outfile = paste0(main_path, fig_path, 'capex_opex_proj_top_fields.pdf'))

## historic
## prepare historical cost for joining
hist_prev_join <- hist_prev %>%
  select(doc_field_code, year, capex_imputed_prev = capex_imputed, opex_imputed_prev = opex_imputed)

hist_comp <- hist_curr %>%
  select(doc_field_code, year, capex_imputed_curr = capex_imputed, opex_imputed_curr = opex_imputed) %>%
  left_join(hist_prev_join) %>%
  left_join(field_names) %>%
  left_join(prod2019) %>%
  mutate(prod_bbl = ifelse(is.na(prod_bbl), 0, prod_bbl),
         diff_opex = opex_imputed_curr - opex_imputed_prev,
         diff_capex = capex_imputed_curr - capex_imputed_prev)

hist_comp_long <- hist_comp %>%
  select(doc_field_code, doc_fieldname, year, prod_bbl, opex_imputed_curr, opex_imputed_prev, capex_imputed_curr, capex_imputed_prev, diff_opex, diff_capex) %>%
  pivot_longer(opex_imputed_curr:diff_capex,
               names_to = 'cost_type',
               values_to = 'usd') %>%
  mutate(version = ifelse(cost_type %in% c('opex_imputed_curr', 'capex_imputed_curr'), "current",
                          ifelse(cost_type %in% c('opex_imputed_prev', 'capex_imputed_prev'), "prev", "diff")),
         cost = ifelse(cost_type %in% c('opex_imputed_curr',
                                        'opex_imputed_prev',
                                        'diff_opex'), "opex", "capex"))

## top ten
hist_summaries <- hist_comp_long %>%
  filter(doc_field_code != '848') %>%
  filter(version != 'diff') %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% topten$doc_field_code, doc_fieldname, "Non-top field"),
         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  group_by(field_name_adj, cost_type, version, cost, year) %>%
  summarise(usd = mean(usd),
            usd = mean(usd)) %>%
  ungroup() %>%
  mutate(field_type = ifelse(field_name_adj == 'Non-top field', 'Non-top field', 'Top ten field'))

## hist fig
topten_hist <- ggplot(hist_summaries %>% mutate(facet_lab = ifelse(version == 'current', "Updated", "Previous")), 
                      aes(x = year, y = usd, color = field_name_adj, lty = field_type)) +
  geom_line(size = 0.5, alpha = 0.9) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  facet_grid(cost ~ facet_lab, scales = 'free_y') +
  labs(title = 'Historic capex and opex',
       subtitle = "Dotted line = Non-top fields",
       color = NULL,
       lty = NULL,
       x = NULL,
       y = 'USD') +
  scale_linetype_manual(values = c('dotted', 'solid'),
                        guide = "none") +
  theme_bw() +
  theme(legend.position = 'top') 

## save
ggsave(topten_hist,
       filename = file.path(main_path, fig_path, 'capex_opex_hist_top_fields.pdf'),
       width = 10,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'capex_opex_proj_top_fields.pdf'),
            outfile = paste0(main_path, fig_path, 'capex_opex_proj_top_fields.pdf'))


## field-level comparison for top ten
## ---------------------------------------

topten_comp <- rbind(cost_summaries %>% select(-cost_type),
                     hist_summaries %>% select(-cost_type)) %>%
  mutate(facet_lab = ifelse(version == 'current', "Updated", "Previous"))



field_opex_fig <- ggplot(topten_comp %>% filter(cost == "opex"), aes(x = year, y = usd, color = field_name_adj, lty = version)) +
  geom_line(size = 0.5, alpha = 0.9) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_vline(xintercept = 2020, color = "darkgrey") +
  facet_wrap( ~ field_name_adj) +
  labs(title = 'Historic and projected opex',
       color = NULL,
       lty = NULL,
       x = NULL,
       y = 'USD') +
  scale_linetype_manual(values = c('solid', 'dotted')) +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  theme(legend.position = 'top') 

## save
ggsave(field_opex_fig,
       filename = file.path(main_path, fig_path, 'field_opex_top_field_comp.pdf'),
       width = 10,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'field_opex_top_field_comp.pdf'),
            outfile = paste0(main_path, fig_path, 'field_opex_top_field_comp.pdf'))


##
field_capex_fig <- ggplot(topten_comp %>% filter(cost == "capex"), aes(x = year, y = usd, color = field_name_adj, lty = version)) +
  geom_line(size = 0.5, alpha = 0.9) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_vline(xintercept = 2020, color = "darkgrey") +
  facet_wrap( ~ field_name_adj) +
  labs(title = 'Historic and projected capex',
       color = NULL,
       lty = NULL,
       x = NULL,
       y = 'USD') +
  scale_linetype_manual(values = c('solid', 'dotted')) +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  theme(legend.position = 'top') 

## save
ggsave(field_capex_fig,
       filename = file.path(main_path, fig_path, 'field_capex_top_field_comp.pdf'),
       width = 10,
       height = 10,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'field_capex_top_field_comp.pdf'),
            outfile = paste0(main_path, fig_path, 'field_capex_top_field_comp.pdf'))


## rystad assets
rystad_prev_join <- rystad_prev %>%
  select(original_asset_name, year, capex_forecast_prev = capex_forecast, opex_forecast_prev = opex_forecast)

asset_comp <- rystad_curr %>%
  select(original_asset_name, year, capex_forecast_curr = capex_forecast, opex_forecast_curr = opex_forecast) %>%
  left_join(rystad_prev_join) %>%
  pivot_longer(capex_forecast_curr:opex_forecast_prev,
               names_to = 'cost_type',
               values_to = 'usd') %>%
  mutate(version = ifelse(cost_type %in% c('capex_forecast_curr', 'opex_forecast_curr'), "Current", "Previous"),
         cost = ifelse(cost_type %in% c('opex_forecast_prev',
                                        'opex_forecast_curr'), "opex", "capex"))

##
asset_capex_fig <- ggplot(asset_comp %>% filter(cost == "capex" & !is.na(usd)), aes(x = year, y = usd, lty = version)) +
  geom_line(size = 0.5, alpha = 0.9) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_vline(xintercept = 2020, color = "darkgrey") +
  facet_wrap( ~ original_asset_name, scales = "free_y") +
  labs(title = 'Historic and projected capex for Rystad assets',
       color = NULL,
       lty = NULL,
       x = NULL,
       y = 'USD') +
  scale_linetype_manual(values = c('solid', 'dotted')) +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.text = element_text(size = 5)) 

## save
ggsave(asset_capex_fig,
       filename = file.path(main_path, fig_path, 'asset_capex_comp.pdf'),
       width = 15,
       height = 15,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'asset_capex_comp.pdf'),
            outfile = paste0(main_path, fig_path, 'asset_capex_comp.pdf'))



##
asset_opex_fig <- ggplot(asset_comp %>% filter(cost == "opex" & !is.na(usd)), aes(x = year, y = usd, lty = version)) +
  geom_line(size = 0.5, alpha = 0.9) +
  geom_hline(yintercept = 0, lty = 'dashed', color = "grey") +
  geom_vline(xintercept = 2020, color = "darkgrey") +
  facet_wrap( ~ original_asset_name, scales = "free_y") +
  labs(title = 'Historic and projected opex for Rystad assets',
       color = NULL,
       lty = NULL,
       x = NULL,
       y = 'USD') +
  scale_linetype_manual(values = c('solid', 'dotted')) +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  theme(legend.position = 'bottom') 

## save
ggsave(asset_opex_fig,
       filename = file.path(main_path, fig_path, 'asset_opex_comp.pdf'),
       width = 15,
       height = 15,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'asset_opex_comp.pdf'),
            outfile = paste0(main_path, fig_path, 'asset_opex_comp.pdf'))

