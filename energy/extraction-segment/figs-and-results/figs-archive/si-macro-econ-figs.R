## Tracey Mangin
## January 26, 2022
## macroeconomic condition figures

library(data.table)
library(tidyverse)
library(hrbrthemes)
library(openxlsx)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items



data_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
scen_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
save_directory    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/figures/si-figs/'

oil_price_file    <- 'oil_price_projections_revised.xlsx'
carbon_file       <- 'carbon_prices_revised.csv'
# prod_quota_file   = 'prod_quota_scenarios_with_sb.csv'


## oil prices
# load oil price data
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

## figure
oil_fig <- ggplot(oilpx_scens %>% filter(year > 2019), aes(x = year, y = oil_price_usd_per_bbl, color = scenario_name)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(y = "Price (USD) per barrel",
       x = NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  theme_line +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = rev(macro_pal)) 

ggsave(filename =  paste0(save_directory, "oil_px_si_fig.png"), oil_fig, width = 5, height = 4, units = "in", dpi = 300)

## carbon prices
carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)
# carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
carbonpx_scens[, carbon_price_usd_per_kg := carbon_price / 1000] # convert from usd per metric ton to usd per kg
carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]

## factor
carbonpx_scens$carbon_price_scenario <- factor(carbonpx_scens$carbon_price_scenario , 
                                               levels = c("price floor", "central SCC", "price ceiling"))

## figure
carbon_fig <- ggplot(carbonpx_scens %>% filter(year > 2019), aes(x = year, y = carbon_price_usd_per_kg, color = carbon_price_scenario)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(y = "Price (USD) per kg",
       x = NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.4)) +
  theme_line +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = rev(macro_pal)) 

ggsave(filename =  paste0(save_directory, "carbon_px_si_fig.png"), carbon_fig, width = 5, height = 4, units = "in", dpi = 300)




## text work for filter
ccs_options <- or1(c("medium CCS", "no ccs", "price floor", "price ceiling", "central SCC"))

carbonpx_scens[, ccs := str_extract(carbon_price_scenario, pattern = ccs_options)]

carbonpx_scens <- carbonpx_scens[ccs %in% c("no ccs", "price floor")]

carbonpx_scens[, target := fifelse(carbon_price_scenario %in% c("carbon_90_perc_reduction-no_setback-no ccs",
                                                                "carbon_sb_90_perc_reduction-setback_1000ft-no ccs",
                                                                "carbon_sb_90_perc_reduction-setback_2500ft-no ccs",
                                                                "carbon_sb_90_perc_reduction-setback_5280ft-no ccs"), "90%",
                                   fifelse(carbon_price_scenario == "carbon_setback_1000ft-no_setback-no ccs", "1000ft setback",
                                           fifelse(carbon_price_scenario == "carbon_setback_2500ft-no_setback-no ccs", "2500ft setback",
                                                   fifelse(carbon_price_scenario == "carbon_sb_90_perc_reduction-setback_5280ft-no ccs", "5280ft setback", "price floor"))))]


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

