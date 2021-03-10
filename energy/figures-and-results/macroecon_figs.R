## Tracey Mangin
## November 12, 2020
## macroeconomic condition figures

library(data.table)
library(tidyverse)
library(hrbrthemes)
library(openxlsx)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items


data_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
scen_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
save_directory    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model/'

oil_price_file    <- 'oil_price_projections.xlsx'
carbon_file       <- 'carbon_prices.csv'
prod_quota_file   = 'prod_quota_scenarios.csv'

## meas's line theme

theme_line = theme_ipsum(base_family = 'Arial',
                         grid = 'Y', 
                         plot_title_size = 10, 
                         subtitle_size = 9,
                         axis_title_just = 'center',
                         axis_title_size = 9, 
                         axis_text_size = 9,
                         strip_text_size = 9)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 8, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5),
        legend.margin = margin(t = 0, b = 0, unit = 'cm'),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))




## oil prices
oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = 1:5))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price', 'iea_oil_price') #'bp_oil_price'
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price', 'iea_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price', 'iea oil price'))]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

oilpx_scens <- oilpx_scens %>%
  mutate(scenario_name = ifelse(oil_price_scenario == "reference case", "EIA reference case",
                                ifelse(oil_price_scenario == "high oil price", "EIA high case",
                                       ifelse(oil_price_scenario == "low oil price", "EIA low case", "IEA delay recovery"))))

oilpx_scens$scenario_name <- factor(oilpx_scens$scenario_name, levels = c('IEA delay recovery', 'EIA low case', 'EIA reference case', 'EIA high case'))

## figure
oil_fig <- ggplot(oilpx_scens %>% filter(year > 2019), aes(x = year, y = oil_price_usd_per_bbl, color = scenario_name)) +
  geom_line(size = 0.75, alpha = 0.8) +
  ylab("Price (USD) per barrel") +
  scale_color_manual(values = ucsb_pal_prim4) +
  # scale_color_manual(values = c(ucsb_pal_prim2, ucsb_pal_sec6[3], ucsb_pal_sec6[6])) +
  base_theme 
# +
#   guides(color = guide_legend(nrow = 2, byrow = FALSE))

ggsave(filename =  paste0(save_directory, "interim-fuels-f1.png"), oil_fig, width = 6.5, height = 4, units = "in", dpi = 300)

## carbon prices
carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)
# carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
carbonpx_scens[, carbon_price_usd_per_kg := carbon_price/1000] # convert from usd per metric ton to usd per kg
carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]


carbonpx_scens <- carbonpx_scens %>%
  mutate(scenario_name = ifelse(carbon_price_scenario == "price floor", "Low carbon price",
                                ifelse(carbon_price_scenario == "price ceiling", "High carbon price", "Central carbon price")))

carbonpx_scens$scenario_name <- factor(carbonpx_scens$scenario_name, levels = c('Low carbon price', 'Central carbon price', 'High carbon price'))

nb_cols <- length(unique(carbonpx_scens$carbon_price_scenario))
intensity_cols <- colorRampPalette(blue_intensity)(nb_cols)

carbon_fig <- 
  # ggplot(carbonpx_scens %>% filter(year > 2019), aes(x = year, y = carbon_price_usd_per_kg, lty = scenario_name)) +
  ggplot(carbonpx_scens %>% filter(year > 2019), aes(x = year, y = carbon_price_usd_per_kg * 1000, color = scenario_name)) +
  geom_line(size = 0.75, alpha = 1) +
  scale_color_manual(values = ucsb_pal_prim4) +
  # scale_color_manual(values = rev(intensity_cols)) +
  # geom_line(size = 0.75, alpha = 1, color = ucsb_pal_prim4[1]) +
  # scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  ylab("Price (USD) per metric ton") +
  theme_line +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank())
# +
#   theme(legend.key.width = unit(1,"cm"))

ggsave(filename =  paste0(save_directory, "interim-fuels-f2.png"), carbon_fig, width = 6, height = 4, units = "in", dpi = 300)

## prod quota scens

prod_quota_scens = fread(file.path(scen_path, prod_quota_file), header = T)
prod_quota_scens = subset(prod_quota_scens, select = -units)

prod_quota_scens2 <- prod_quota_scens %>%
  mutate(scenario_name = ifelse(prod_quota_scenario == "no quota", "No quota",
                                ifelse(prod_quota_scenario == "quota_10", "90% reduction",
                                       ifelse(prod_quota_scenario == "quota_20", "80% reduction",
                                              ifelse(prod_quota_scenario == "quota_40", "60% reduction",
                                                     "100% reduction"))))) %>%
  filter(scenario_name != "No quota") %>%
  mutate(quota = as.numeric(str_remove_all(quota, pattern = ",")))

## add 2019
# load historic production
well_prod_org <- read_rds("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds") %>% as.data.table()

setnames(well_prod_org, "FieldCode", "doc_field_code")
well_prod <- well_prod_org[year == 2019]


wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  select(doc_field_code = FieldCode, FieldName) %>%
  unique() %>%
  as.data.table()

well_prod19 <- well_prod %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = str_sub(doc_field_code, start= -3)) %>%
  group_by(doc_field_code, year) %>%
  summarise(total_bbls = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(wells_19) %>%
  ## remove 000, remove gas
  filter(doc_field_code != "000") %>%
  mutate(gas = str_extract(FieldName, "Gas")) %>%
  filter(is.na(gas)) %>%
  select(-gas)

prod_hist19 <- well_prod19 %>%
  group_by(year) %>%
  summarise(production_bbl = sum(total_bbls)) %>%
  ungroup()

## create tibble for binding
init_df <- tibble(year = rep(2019, length(unique(prod_quota_scens2$prod_quota_scenario))),
                  prod_quota_scenario = unique(prod_quota_scens2$prod_quota_scenario),
                  quota = rep(as.numeric(prod_hist19$production_bbl), length(unique(prod_quota_scens2$prod_quota_scenario))),
                  scenario_name = unique(prod_quota_scens2$scenario_name))

prod_quota_scens2 <- rbind(prod_quota_scens2, init_df)



#####

quota_cols <- length(unique(prod_quota_scens2$prod_quota_scenario))
quota_intensity_cols <- colorRampPalette(blue_intensity)(quota_cols)

prod_quota_scens2$scenario_name <- factor(prod_quota_scens2$scenario_name, levels = rev(c('60% reduction', '80% reduction', '90% reduction', "100% reduction")))



quota_fig <- 
  # ggplot(carbonpx_scens %>% filter(year > 2019), aes(x = year, y = carbon_price_usd_per_kg, lty = scenario_name)) +
  ggplot(prod_quota_scens2, aes(x = year, y = quota / 1e6, color = scenario_name)) +
  geom_line(size = 0.75, alpha = 1) +
  # geom_point(data = prod_hist19, aes(x = year, y = production_bbl / 1e6), size = 2, color = "black", inherit.aes = F) +
  # scale_color_manual(values = quota_intensity_cols) +
  scale_color_manual(values = rev(ucsb_distinct[2:5])) +
  scale_x_continuous(limits = c(2019, 2045), breaks=c(2019,seq(2025, 2045,5))) +
  # geom_line(size = 0.75, alpha = 1, color = ucsb_pal_prim4[1]) +
  # scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  ylab("Production quota (million bbl)") +
  base_theme +
  guides(color = guide_legend(nrow = 2, byrow = FALSE))
# +
#   theme(legend.key.width = unit(1,"cm"))

ggsave(filename =  paste0(save_directory, "interim-fuels-f3.png"), quota_fig, width = 5, height = 4, units = "in", dpi = 300)


