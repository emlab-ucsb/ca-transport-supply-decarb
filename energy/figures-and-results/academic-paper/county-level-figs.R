## Tracey Mangin
## February 2, 2022
## county-level indicators

## libraries
library(data.table)
library(tidyverse)


# fig 1: county-level ghg emissions intensity x 2019 production, calculated using weighted mean, w = 2019 production
# fig 2: county-level costs x 2019 production, calculated using weighted mean, w = 2019 production
# fig 3: county-level labor x 2019 production, use the indicator in the mechanisms figure
# fig 4: county-level PM2.5 affected population x 2019 production, use the indicator in the mechanisms figure 
# fig 5: county-level % of county affected by setback x 2019 production, use the setback scenario in mechanisms figures



## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths 
main_path     <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'
outputs_path    <- 'outputs/academic-out/extraction/'

## results path (update this)
county_results <- 'extraction_2021-12-06/county-results/'

## files
ghg_file      <- 'ghg_emissions_x_field_2018-2045.csv'
prod_file     <- 'well_prod_m_processed.csv'
forecast_file <- 'field_capex_opex_forecast_revised.csv'
setback_file  <- 'setback_coverage_R.csv'
county_out_file <- 'subset_county_results.csv'

## figures - county-level indicator x 2019 production, add 90th percentile line
## ghg emission intensity, costs, employment multipliers, pm2.5?, setbacks?

## load data
## ---------------------------------------

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
## ghg factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## load opex/ capex
price_data = fread(file.path(main_path, 'outputs/stocks-flows', forecast_file), header = T)
price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
price_data[, sum_cost := m_opex_imputed + m_capex_imputed]
price_data <- price_data[year == 2020, .(doc_field_code, m_opex_imputed, m_capex_imputed, sum_cost)]

## setback coverage
setback_scens = fread(file.path(main_path, 'outputs/setback', 'model-inputs', setback_file), header = T, colClasses = c('doc_field_code' = 'character'))
setnames(setback_scens, 'rel_coverage', 'area_coverage')
setback_scens <- setback_scens[setback_scenario != "no_setback", .(doc_field_code, setback_scenario, area_coverage)]

## labor
total_multipliers_ext <- read_xlsx(paste0(main_path, labor_processed, 'ica_multipliers_v2.xlsx'), sheet = 'ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, 
         indi_emp_mult = indirect_emp, 
         indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, 
         indi_comp_mult = indirect_comp, 
         indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, 
         ip.indi_comp_mult = ip.indirect_comp, 
         ip.indu_comp_mult = ip.induced_comp) %>%
  as.data.table()

total_multipliers_ext[, ':=' (total_emp = dire_emp_mult + indi_emp_mult + indu_emp_mult,
                              total_comp = dire_comp_mult + ip.indi_comp_mult + ip.indu_comp_mult)]


total_multipliers_ext <- total_multipliers_ext[, .(county, total_emp, total_comp)]

## county results
county_out <- fread(paste0(main_path, outputs_path, county_results, county_out_file), header = T)

## county information
## -------------------------------

## county lut
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## field name look up 
fname_lut <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique()

## get relative county production (most recent year of nonzero production available for each field)
prod_x_county <- well_prod %>%
  left_join(county_lut) %>%
  group_by(doc_field_code, doc_fieldname, year, adj_county_name) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, year) %>%
  mutate(field_total = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_prod = prod / field_total,
         rel_prod = ifelse(is.na(rel_prod) & prod == 0 & field_total == 0, 0, rel_prod)) %>%
  filter(rel_prod > 0) %>%
  group_by(doc_field_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(doc_field_code, adj_county_name, rel_prod)

## how many fields with positive prod?
# View(field_out[, c(prod = sum(total_prod_bbl, na.rm = T)), by = doc_field_code][V1 > 0])

## calculate 2019 production, emissions, revenue
init_prod <- well_prod %>%
  filter(year == 2019) %>%
  select(doc_field_code, doc_fieldname, year, OilorCondensateProduced) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(total_prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

setDT(init_prod)

## merge with ghg factors
init_prod <- merge(init_prod, ghg_factors_2019,
                   by = c('doc_field_code', 'year'),
                   all.x = T)

# init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]
# 
# 
# init_prod <- init_prod[, .(doc_field_code, year, total_prod_bbl, total_ghg_kgCO2e)]

## remove fields that do not ever produce oil and do not show up in results, as well as "Any Field)
init_prod <- init_prod[!doc_field_code %in% c("302", "502", "000")]

## join 
county_out <- merge(init_prod, prod_x_county,
                    by = c("doc_field_code"),
                    all.x = T,
                    allow.cartesian = T)

## county production
county_out[, county_prod_bbl := total_prod_bbl * rel_prod]

## add price data
county_out <- merge(county_out, price_data,
                    by = c("doc_field_code"),
                    all.x = T,
                    allow.cartesian = T)





## labor multipliers (these are county-level)












## county emissions factors, weighted by 2019 production
county_ghg_intensity <- county_out[, .(mean_ghg_intensity = weighted.mean(upstream_kgCO2e_bbl, total_prod_bbl),
                                       total_prod_bbl = sum(total_prod_bbl)), 
                                   by = .(adj_county_name)]

county_ghg_intensity <- county_ghg_intensity[total_prod_bbl > 0]

## plot

county_ghg_plot <- ggplot(county_ghg_intensity, aes(x = total_prod_bbl / 1e6, y = mean_ghg_intensity)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "County-level emission intensity") +
  ggrepel::geom_text_repel(data = county_ghg_intensity %>% filter(mean_ghg_intensity > 40), aes(x = total_prod_bbl / 1e6, y = mean_ghg_intensity, label = adj_county_name), 
                           hjust = 0, nudge_x = 0.1, size = 3) +
  theme_line


## county-level production
# ---------------------------------

ounty_prod <- county_out[oil_price_scenario == 'reference case' &
                           innovation_scenario == 'low innovation'&
                           carbon_price_scenario == 'price floor' &
                           ccs_scenario == "no ccs" &
                           prod_quota_scenario == "no quota" &
                           excise_tax_scenario == 'no tax', .(scen_id, setback_scenario, county, year, total_county_bbl, total_county_ghg_kgCO2e)]

bau_prod <- county_prod[setback_scenario == "no_setback", .(county, year, total_county_bbl, total_county_ghg_kgCO2e)]

setnames(bau_prod, c("total_county_bbl", "total_county_ghg_kgCO2e"), c("bau_county_bbl", "bau_county_ghg"))


county_prod = county_prod[bau_prod, on = .(county, year), allow.cartesian = T, nomatch = 0]

county_prod[, ':=' (diff_prod = total_county_bbl - bau_county_bbl,
                    diff_ghg = total_county_ghg_kgCO2e - bau_county_ghg)]


county_prod[, ':=' (rel_diff_prod = diff_prod / bau_county_bbl,
                    rel_diff_ghg = diff_ghg / bau_county_ghg)]

test <- ggplot(county_prod %>% filter(setback_scenario != "no_setback"), aes(x = year, y = rel_diff_prod, group = county, color = county)) +
  geom_line() +
  facet_wrap(~setback_scenario) +
  theme_line

ggplotly(test)







