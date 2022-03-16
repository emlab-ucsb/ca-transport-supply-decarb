## Tracey Mangin
## February 16, 2022
## SI - modeled outputs

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)
library(xtable)
library(rebus)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path <- 'outputs/predict-production/extraction_2022-02-07/'
extraction_folder_name <- 'all-target/'
fig_path <- 'outputs/academic-out/extraction/figures/all-oil-px/'
save_path <- 'outputs/academic-out/extraction/figures/si-figs/'
data_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
scen_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'


## file names
levels_name       <- 'state_levels_all_oil.csv'
oil_price_file    <- 'oil_price_projections_revised.xlsx'
carbon_file       <- 'carbon_prices_revised.csv'
prod_file         <- "well_prod_m_processed.csv"

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))


## oil price
oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

oilpx_scens <- oilpx_scens %>%
  mutate(scenario_name = ifelse(oil_price_scenario == "reference case", "EIA reference case",
                                ifelse(oil_price_scenario == "high oil price", "EIA high case",
                                       ifelse(oil_price_scenario == "low oil price", "EIA low case", "NA"))))

oilpx_scens$scenario_name <- factor(oilpx_scens$scenario_name, levels = c('EIA low case', 'EIA reference case', 'EIA high case'))

## carbon prices
carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)
# carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
carbonpx_scens[, carbon_price_usd_per_kg := carbon_price / 1000] # convert from usd per metric ton to usd per kg
carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]

## factor
carbonpx_scens$carbon_price_scenario <- factor(carbonpx_scens$carbon_price_scenario , 
                                               levels = c("price floor", "central SCC", "price ceiling"))

carbonpx_scens[, names := fifelse(carbon_price_scenario == "price floor", "price\nfloor",
                                  fifelse(carbon_price_scenario == "central SCC", "central SSC", "price\nfloor"))]




## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

## filter for BAU, oil price scens
levels_dt_oilpx <- levels_dt[policy_intervention == 'BAU']
levels_dt_oilpx[, oil_name := paste0('EIA ', oil_price_scenario)]

## factor
levels_dt_oilpx$oil_name <- factor(levels_dt_oilpx$oil_name , levels = c("EIA low oil price", "EIA reference case", "EIA high oil price"))


## plot production
prod_oil_fig <- ggplot(levels_dt_oilpx %>% filter(metric == "total_state_bbl",
                                           year > 2019), aes(x = year, y = value / 1e6, color = oil_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = "Oil production (million bbls)",
       color = NULL) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(prod_oil_fig,
       filename = file.path(main_path, save_path, 'si-oil-prod-oilpx.png'),
       width = 5,
       height = 4,
       units = "in")

## GHG
## ------------------------------------------

## plot ghg
ghg_oil_fig <- ggplot(levels_dt_oilpx %>% filter(metric == "total_state_ghg_MtCO2",
                                            year > 2019), aes(x = year, y = value, color = oil_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = expression(paste("Million metric tonnes of ", CO[2], "e")),
       color = NULL) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
   theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(ghg_oil_fig,
       filename = file.path(main_path, save_path, 'si-ghg-oilpx.png'),
       width = 5,
       height = 4,
       units = "in")

## carbon price pathway outputs
## filter for BAU, carbon price scenarios

# levels_dt_carbon <- levels_dt[oil_price_scenario == 'reference case' &
#                               carbon_price_scenario %in% c("")]



levels_dt_carbon[, oil_name := paste0('EIA ', oil_price_scenario)]

## factor
levels_dt$oil_name <- factor(levels_dt$oil_name , levels = c("EIA low oil price", "EIA reference case", "EIA high oil price"))


## setback distances
## filter for BAU, setback distances

levels_dt_sb <- levels_dt[oil_price_scenario == 'reference case' &
                          carbon_price_scenario %in% c("price floor") &
                          excise_tax_scenario == "no tax"]

levels_dt_sb[, sb_name := fifelse(setback_scenario == 'no_setback', '0ft setback',
                                  fifelse(setback_scenario == 'setback_1000ft', '1000ft setback',
                                          fifelse(setback_scenario == 'setback_2500ft', '2500ft setback', '5280ft setback')))]


## factor
levels_dt_sb$sb_name <- factor(levels_dt_sb$sb_name, 
                               levels = c("0ft setback", "1000ft setback", "2500ft setback", "5280ft setback"))


## plot ghg
setback_fig <- ggplot(levels_dt_sb %>% filter(metric == "total_state_bbl",
                                                 year > 2019), aes(x = year, y = value / 1e6, color = sb_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = 'Oil production (million bbls)',
       color = NULL) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(setback_fig,
       filename = file.path(main_path, save_path, 'si-prod-setback.png'),
       width = 5,
       height = 4,
       units = "in")

setback_ghg_fig <- ggplot(levels_dt_sb %>% filter(metric == "total_state_ghg_MtCO2",
                                              year > 2019), aes(x = year, y = value, color = sb_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = expression(paste("Million metric tonnes of ", CO[2], "e")),
       color = NULL) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(setback_ghg_fig,
       filename = file.path(main_path, save_path, 'si-ghg-setback.png'),
       width = 5,
       height = 4,
       units = "in")

## all setback targets
## ----------------------------------------------------

targets <- levels_dt[setback_scenario != "no_setback" &
                       year == 2045 &
                       metric == "total_state_ghg_MtCO2" &
                       policy_intervention == "setback", .(oil_price_scenario, setback_scenario, value,
                                                          ghg_2045_perc, ghg_2045_perc_reduction, target_label)]


targets[, sb_name := fifelse(setback_scenario == 'no_setback', '0ft setback',
                                  fifelse(setback_scenario == 'setback_1000ft', '1000ft setback',
                                          fifelse(setback_scenario == 'setback_2500ft', '2500ft setback', '5280ft setback')))]

## factor
targets$sb_name <- factor(targets$sb_name, 
                               levels = c("0ft setback", "1000ft setback", "2500ft setback", "5280ft setback"))

targets$oil_price_scenario <- factor(targets$oil_price_scenario, 
                          levels = rev(c("reference case", "low oil price", "high oil price")))



targets2 <- targets %>%
  select(-target_label, - ghg_2045_perc_reduction, -setback_scenario) %>%
  mutate(ghg_2045_perc = round(ghg_2045_perc * 100, 1),
         ghg_2045_perc = ifelse(ghg_2045_perc > 0, paste0("+", ghg_2045_perc), as.character(ghg_2045_perc)),
         value = round(value, 1)) %>%
  select(oil_price_scenario, sb_name, value, ghg_2045_perc) %>%
  arrange(rev(oil_price_scenario))

## 90 percent reduction target
target_90 <- levels_dt[target == "90perc_reduction" &
                       year == 2045 &
                       oil_price_scenario == "reference case" &
                       metric == "total_state_ghg_MtCO2", .(value, ghg_2045_perc, ghg_2045_perc_reduction, target_label)]

target_90 <- target_90[1,]


## excise and carbon tax values
## -------------------------------------------

## compiled state outputs 
state_out_list <- list()

## files to process
state_files_to_process <- list.files(paste0(main_path, extraction_folder_path, extraction_folder_name, 'state-out/'))


for (i in 1:length(state_files_to_process)) {
  
  print(i)
  
  state_file_name <- state_files_to_process[i]
  
  state_scen_out <- readRDS(paste0(main_path, extraction_folder_path, extraction_folder_name, 'state-out/', state_file_name))
  
  state_out_list[[i]]  <- state_scen_out
  
}

state_subset_all <- rbindlist(state_out_list)

## excise tax values
excise_tax_paths <- state_subset_all %>%
  filter(target_policy == "excise_tax" & setback_scenario == "no_setback") %>%
  select(scen_id, oil_price_scenario, target, year, tax_rate) %>%
  left_join(oilpx_scens) %>%
  mutate(tax_val = tax_rate * oil_price_usd_per_bbl,
         target_name = ifelse(target == "90perc_reduction", "90% reduction",
                              paste0(str_extract(target, pattern = one_or_more(DGT)), 'ft setback')))

## figure
excise_fig <- ggplot(excise_tax_paths %>% filter(year == 2020), aes(x = target_name, y = tax_rate, color = scenario_name)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(y = "Excise tax rate",
       x = NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.25)) +
  theme_line +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = rev(macro_pal)) 

ggsave(filename =  paste0(main_path, save_path, "si-excise-tax-fig.png"), excise_fig, width = 5, height = 4, units = "in", dpi = 300)


## carbon tax values
carbon_tax_paths <- state_subset_all %>%
  filter(target_policy == "carbon_tax") %>%
  select(scen_id, oil_price_scenario, setback_scenario, target, year, carbon_price_usd_per_kg) %>%
  left_join(oilpx_scens) %>%
  mutate(target_name = ifelse(target == "90perc_reduction", "90% reduction",
                              paste0(str_extract(target, pattern = one_or_more(DGT)), 'ft setback')),
         setback_name = paste0(str_extract(setback_scenario, pattern = one_or_more(DGT)), 'ft setback'))




## figure
carbon_fig <- ggplot(carbon_tax_paths %>% filter(setback_scenario == "no_setback"), aes(x = year, y = carbon_price_usd_per_kg, color = target_name, group = scen_id)) +
  geom_line(alpha = 0.8, size = 1) +
  labs(y = "Carbon tax (USD per kg)",
       x = NULL,
       color = "Target reduction:") +
  facet_wrap(~scenario_name) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  theme_line +
  # guides(color = guide_legend(nrow = 2, byrow = TRUE, title = "Target reduction:", position = "bottom")) +
  geom_line(data = carbonpx_scens, aes(x = year, y = carbon_price_usd_per_kg, group = carbon_price_scenario), color = "black", lty = "dotted") +
  ggrepel::geom_text_repel(data = carbonpx_scens %>% filter(year == 2042), 
            aes(x = year, y = carbon_price_usd_per_kg, group = carbon_price_scenario, label = carbon_price_scenario), 
            inherit.aes = F, size = 2.5)

carbon_fig2 <- ggplot(carbon_tax_paths %>% filter(setback_scenario != "no_setback"), aes(x = year, y = carbon_price_usd_per_kg, group = scen_id, color = scenario_name)) +
  geom_line(alpha = 0.8, size = 1) +
  labs(y = "Carbon tax (USD per kg)",
       x = NULL,
       color = NULL) +
  scale_color_manual(values = rev(macro_pal)) +
  facet_wrap(~setback_name) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  theme_line +
  # guides(color = guide_legend(nrow = 2, byrow = TRUE, title = "Target reduction:", position = "bottom")) +
  geom_line(data = carbonpx_scens, aes(x = year, y = carbon_price_usd_per_kg, group = carbon_price_scenario), color = "black", lty = "dotted") +
  ggrepel::geom_text_repel(data = carbonpx_scens %>% filter(year == 2042), 
                           aes(x = year, y = carbon_price_usd_per_kg, group = carbon_price_scenario, label = carbon_price_scenario), 
                           inherit.aes = F, size = 2.5) +
  theme(legend.position = "bottom")
  
  

carbon_tax_fig <- plot_grid(
  carbon_fig,
  carbon_fig2,
  align = 'none',
  labels = c("A", "B"),
  nrow = 2,
  rel_widths = c(1, 1)
)

ggsave(carbon_tax_fig,
       filename = file.path(main_path, save_path, "si-carbon-tax-fig.png"),
       width = 10,
       height = 10,
       units = "in")


## new, old, hist production
## BAU, setback 5280

prod_dt <- state_subset_all[oil_price_scenario == 'reference case' &
                            target %in% c('no_target', 'setback_5280ft') &
                              setback_scenario %in% c('no_setback', 'setback_5280ft')]

prod_dt <- prod_dt[, .(scen_id, year, existing_prod_bbl, new_prod_bbl)]

prod_dt <- prod_dt %>%
  pivot_longer(cols = existing_prod_bbl:new_prod_bbl, names_to = 'prod_type', values_to = 'prod_bbl')

hist_prod <- well_prod %>%
  filter(doc_field_code != "000") %>%
  group_by(year) %>%
  summarise(prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(prod_type = 'Historic') %>%
  select(year, prod_type, prod_bbl)

all_prod <- expand.grid(scen_id = unique(prod_dt$scen_id),
                             year = unique(hist_prod$year)) %>%
  left_join(hist_prod) %>%
  rbind(prod_dt) %>%
  mutate(prod_type = ifelse(prod_type == 'existing_prod_bbl', 'Existing wells',
                            ifelse(prod_type == "new_prod_bbl", 'New wells', prod_type)),
         scen_name = ifelse(scen_id == 'reference case-no_setback-no quota-carbon_target_setback_5280ft-no ccs-low innovation-no tax',
                            'Carbon tax',
                            ifelse(scen_id == 'reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax',
                                   'BAU',
                                   ifelse(scen_id == 'reference case-no_setback-no quota-price floor-no ccs-low innovation-tax_setback_5280ft',
                                          'Excise tax', 'Setback'))))
## factor
all_prod$scen_name <- factor(all_prod$scen_name, levels = c('BAU', 'Setback', 'Excise tax', 'Carbon tax'))

all_prod$prod_type <- factor(all_prod$prod_type, levels = c('Historic', 'Existing wells', 'New wells'))


## figure
prod_plot <-
  ggplot(all_prod, aes(x = year, y = prod_bbl / 1e6, fill = prod_type)) +
  geom_area() +
  geom_vline(xintercept = 2019, lty = "dashed", color = "black") +
  scale_x_continuous(limits = c(1977, 2045), breaks=c(1977,seq(1985,2045,5))) +
  facet_wrap(~scen_name, ncol = 1) +
  labs(y = "Crude oil extraction (million bbls)",
       x = NULL,
       fill = NULL) +
  theme_line +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_manual(values = c('Historic' = '#696773',
                               'Existing wells' = '#FED766',
                               'New wells' = '#009FB7'))

ggsave(filename =  paste0(main_path, save_path, "hist-future-prod.png"), prod_plot, width = 6, height = 10, units = "in", dpi = 300)





