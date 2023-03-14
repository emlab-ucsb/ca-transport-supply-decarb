## Tracey Mangin
## June 24, 2021
## random diagnostics 

library(tidyverse)
library(data.table)
library(sf)
library(maps)
library(cowplot)

## paths
proj_dir          = "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
save_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/diagnostics/'

## files
ghg_file        = 'ghg_emissions_x_field_2018-2045.csv'
entry_file      = 'stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
prod_file       = "well_prod_m_processed.csv"

## read in info
ghg_factors = fread(file.path(outputs_path, 'stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors = ghg_factors[year == 2019, .(doc_field_code, year, upstream_kgCO2e_bbl)]


## for opex and capex
entry_dt = fread(file.path(outputs_path, entry_file), header = T, colClasses = c('doc_field_code' = 'character'))

## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))

## county
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))


## for 2020 producton
## read in outputs from most recent run
field_outputs <- fread(paste0(proj_dir, "outputs/predict-production/extraction_2021-08-11/update_final/benchmark-field-level-results.csv"), colClasses = c('doc_field_code' = 'character'))

pred_2020_bau <- field_outputs %>%
  filter(oil_price_scenario == "iea oil price",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario == "no quota",
         setback_scenario %in% c("no_setback")) %>%
  mutate(scenario = "BAU") %>%
  dplyr::select(scenario, year, doc_field_code, total_prod_bbl) %>%
  filter(year == 2020)

pred_all_quota40 <- field_outputs %>%
  filter(oil_price_scenario == "reference case",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario == "quota_40",
         setback_scenario %in% c("no_setback")) %>%
  mutate(scenario = "BAU") %>%
  arrange(doc_field_code, year) %>%
  dplyr::select(scenario, year, doc_field_code, total_prod_bbl) %>%
  group_by(scenario, doc_field_code) %>%
  summarise(cumulative_prod = sum(total_prod_bbl, na.rm = T)) %>%
  ungroup() %>%
  filter(cumulative_prod > 0)


## 2019 well prod
prod_2019 <- well_prod[, .(prod = sum(OilorCondensateProduced, na.rm = T)), by = .(year, doc_field_code)]
prod_2019 <- prod_2019[year == 2019]

## opex
opex_capex <- entry_dt %>%
  filter(year == 2019) %>%
  dplyr::select(doc_field_code, doc_fieldname, opex_imputed, capex_imputed) %>%
  pivot_longer(opex_imputed:capex_imputed, names_to = "indicator", values_to = "usd")

## combine
opex_emis_factors <- left_join(opex_capex, ghg_factors) %>%
  full_join(pred_all_bau) %>%
  mutate(cumulative_prod = ifelse(is.na(cumulative_prod), 0, cumulative_prod)) %>%
  filter(cumulative_prod > 0) %>%
  left_join(county_lut)

# opex_subset <- opex_emis_factors %>%
#   filter(opex_imputed <= 50) %>%
#   mutate(version = 'subset') 
# 
# opex_all <- opex_emis_factors %>%
#   mutate(version = 'all') %>%
#   rbind(opex_subset)
#   

## ggplot

cost_fig <- 
ggplot(opex_emis_factors, aes(x = usd, y = upstream_kgCO2e_bbl, size =  cumulative_prod / 1e6)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~indicator, scales = "free_x") +
  labs(x = '2019 cost (USD)',
       y = 'kgCO2e per bbl',
       size = 'cumulative modeled production\n(quota_40) (mbbls)',
       color = NULL,
       title = '2019 GHG emission factor x 2019 cost value',
       subtitle = 'method = lm; fit weighted by cumulative modeled production') +
  geom_smooth(method = 'lm', mapping = aes(weight = cumulative_prod), 
              color = "black", show.legend = FALSE) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal", legend.box = "vertical")
  
# +
#   geom_text(data = opex_emis_factors, aes(label = doc_fieldname), size = 2, color = 'black')

ggsave(cost_fig, 
       filename = file.path(save_path, 'cost_emisfactor_fig.pdf'), 
       width = 12, 
       height = 10)


# capex_fig <- 
#   ggplot(opex_emis_factors, aes(x = capex_imputed, y = upstream_kgCO2e_bbl, size =  cumulative_prod / 1e6, color = adj_county_name)) +
#   geom_point(alpha = 0.5) +
#   labs(x = '2019 capex imputed',
#        y = 'kgCO2e per bbl',
#        size = 'cumulative modeled production (mbbls)',
#        color = NULL,
#        title = '2019 GHG emission factor x 2019 capex value',
#        subtitle = 'method = lm; fit weighted by cumulative modeled production') +
#   geom_smooth(method = 'lm', mapping = aes(weight = cumulative_prod), 
#               color = "black", show.legend = FALSE) +
#   theme_bw() +
#   theme(legend.position = 'bottom',
#         legend.direction = "horizontal", legend.box = "vertical") 
# 
# # +
# #   geom_text(data = opex_emis_factors, aes(label = doc_fieldname), size = 2, color = 'black')
# 
# ggsave(capex_fig, 
#        filename = file.path(save_path, 'capex_emisfactor_fig.pdf'), 
#        width = 8, 
#        height = 8)

## new well entry by field, by 2045


