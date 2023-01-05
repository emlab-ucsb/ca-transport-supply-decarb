## Tracey Mangin
## January 2, 2023
## Sensitivy analysis for health outputs

## libraries
library(data.table)
library(tidyverse)
library(broom)
library(rebus)
library(readxl)
library(openxlsx)
library(hrbrthemes)
library(extrafont)
library(scales)
library(cowplot)

## paths 
main_path              = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path = 'outputs/academic-out/extraction/extraction_2023-01-04/'
state_save_path        = paste0(main_path, extraction_folder_path, 'state-results/health_sens/')
field_out              = paste0(main_path, "outputs/predict-production/extraction_2022-11-15/revision-setbacks/field-out/")
health_out             = paste0(main_path, "outputs/academic-out/health/")

# ## external paths
# extraction_folder_path = '/Volumes/calepa/academic-out/extraction_2022-11-16/'
# state_save_path        = '/Volumes/calepa/academic-out/extraction_2022-11-16/state-results/'
# field_out              = '/Volumes/calepa/extraction-out/extraction_2022-11-15/revision-setbacks/field-out/'
# health_out             = '/Volumes/calepa/academic-out/extraction_2022-11-16/health/'


## output path
save_info_path        = paste0(main_path, 'outputs/academic-out/extraction/figures/nature-energy-revision/final/')


## files
ghg_file            = 'indust_emissions_2000-2019.csv'
scc_file            = 'social_cost_carbon.csv'
carbon_px_file      = 'carbon_price_scenarios_revised.xlsx'
field_cluster_file  = 'extraction_field_cluster_xwalk.csv'
state_levels        = 'state_levels_all_oil.csv'

## read in social cost of carbon
scc_df <- fread(paste0(main_path, 'data/stocks-flows/processed/', scc_file))

## filter for 3 percent, 
scc_df_filt <- scc_df[discount_rate == 'three_perc_avg', .(year, social_cost_co2)]

## Create population by year time series
ct_population <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, year, pop) %>%
  as.data.table()

state_population <- ct_population %>%
  filter(lower_age > 29)%>%
  group_by(year) %>%
  summarize(total_state_pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  as.data.table()


## DAC and CES
dac_ces <- read_xlsx(paste0(main_path, 'data/health/raw/ces3results.xlsx'))

ces_county <- dac_ces %>%
  select(`Census Tract`, `California County`) %>%
  rename(census_tract = `Census Tract`,
         county = `California County`) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) 

## county-dac info
ces3 <- read.csv(paste0(main_path, "data/health/processed/ces3_data.csv"), stringsAsFactors = FALSE) %>%
  select(census_tract, population, CES3_score, disadvantaged) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  as.data.table()

## add counties
ces3 <- merge(ces3, ces_county, 
              by = "census_tract")

## CPI values
# cpi_df <- setDT(read.xlsx(paste0(main_path, 'data/stocks-flows/processed/', carbon_px_file), sheet = 'BLS Data Series', startRow = 12))
# 
# cpi_df <- cpi_df[Year %in% c(2019, 2020), .(Year, Annual)]
# 
# setnames(cpi_df, c("Year", "Annual"), c("year", "annual"))

# cpi2020 <- cpi_df %>%
#   filter(year == 2020) %>%
#   select(annual) %>%
#   as.numeric()
# 
# cpi2019 <- cpi_df %>%
#   filter(year == 2019) %>%
#   select(annual) %>%
#   as.numeric()

#(https://fred.stlouisfed.org/series/CPALTT01USA661S)
cpi2020 <- 109.1951913
cpi2019 <- 107.8645906
cpi2015 <- 100

## discount rate
discount_rate <- 0.03

## health params
VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015 * cpi2019 / cpi2015 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
# VSL_2020 <- VSL_2015 * cpi2020 / cpi2015
income_elasticity_mort <- 0.4

## for monetary mortality impact
growth_rates <- read.csv(paste0(main_path, "data/benmap/processed/growth_rates.csv"), stringsAsFactors = FALSE) %>%
  filter(year > 2018) %>%
  mutate(growth = ifelse(year == 2019, 0, growth_2030),
         cum_growth = cumprod(1 + growth)) %>%
  select(-growth_2030, -growth) %>%
  as.data.table()

## function for health impacts
future_WTP <- function(elasticity, growth_rate, WTP){
  return(elasticity * growth_rate * WTP + WTP) 
}

## 2019 GHG emissions
## --------------------------
hist_ghg <- fread(paste0(main_path, 'data/stocks-flows/processed/', ghg_file), header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])


## read inputs
state_out <- fread(paste0(state_save_path, "subset_state_hs_results.csv"))



## note that the values in the state_pop column are different than the state_population df created
## in this script. The former only includes census tracts affected by oil production, while the latter
## includes all census tracts.

## merge with population series
state_scens <- merge(state_out, state_population,
                     by = c("year"),
                     all.x = T)

# state_labor_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
#                                       ccs_scenario, excise_tax_scenario, setback_scenario, setback_existing, target, target_policy, year, total_emp, total_comp, state_pop, total_state_pop)]
# 
# 
# ## calc PV and convert labor outputs into 2019 dollars
# state_labor_levels[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]
# 
# ## discount using discount rate
# state_labor_levels[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate) ^ (year - 2019))]
# 

# # state_labor_levels <- state_labor_levels %>%
# #   mutate(total_emp_norm = total_emp / (total_state_pop / 1000),
# #          total_comp_norm = total_comp / (total_state_pop / 1000))
# 
# ## melt
# state_labor_levels <- melt(state_labor_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
#                                                            'carbon_price_scenario', 'ccs_scenario', 'setback_scenario',
#                                                            'setback_existing', 'excise_tax_scenario',
#                                                            'target', 'target_policy',  'year'),
#                            measure.vars = c("total_emp", "total_comp_usd19", "total_comp_PV"),
#                            variable.name = "metric",
#                            value.name = "value")
# 
# 
## emissions and extraction
## ------------------------------
state_extract_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                        ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario, target, target_policy, year, total_state_bbl, total_state_ghg_kgCO2)]

state_extract_levels[, total_state_ghg_MtCO2 := total_state_ghg_kgCO2 / (1000 * 1e6)]
state_extract_levels[, total_state_ghg_kgCO2 := NULL]

state_extract_levels <- melt(state_extract_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario',
                                                               'carbon_price_scenario', 'ccs_scenario', 'setback_scenario',
                                                               'setback_existing', 'excise_tax_scenario',
                                                               'target', 'target_policy', 'year'),
                             measure.vars = c("total_state_bbl", "total_state_ghg_MtCO2"),
                             variable.name = "metric",
                             value.name = "value")

## health
## ------------------------------
state_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario, target, target_policy,
                                       year, mean_total_pm25, mean_delta_total_pm25,
                                       mortality_level, mortality_delta, cost_2019_PV, cost_PV, state_pop)]


# ## convert to 2020 USD
# state_health_levels[, cost_PV_20 := cost_PV / cpi2019 * cpi2020]

# state_health_levels <- state_health_levels %>%
#   left_join(state_population) %>%
#   mutate(mortality_level_norm = mortality_level / (total_state_pop * 1000),
#          cost_PV_20_norm = cost_PV_20 / (total_state_pop * 1000))



state_health_levels <- melt(state_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario',
                                                             'carbon_price_scenario', 'ccs_scenario', 'setback_scenario',
                                                             'setback_existing', 'excise_tax_scenario',
                                                             'target', 'target_policy', 'year'),
                            measure.vars = c("mean_total_pm25", "mean_delta_total_pm25", "mortality_level",
                                             "mortality_delta", "cost_2019_PV", "cost_PV"),
                            variable.name = "metric",
                            value.name = "value")

## combine
##----------------------------------

state_levels <- rbind(state_extract_levels, state_health_levels)





state_levels[, policy_intervention := fifelse(carbon_price_scenario != "price floor" & setback_scenario == "no_setback", "carbon tax",
                                              fifelse(setback_scenario != "no_setback" & carbon_price_scenario == 'price floor' & excise_tax_scenario == 'no tax', "setback",
                                                      fifelse(excise_tax_scenario != "no tax" & setback_scenario == "no_setback", "excise tax",
                                                              fifelse(excise_tax_scenario != 'no tax' & setback_scenario != 'no_setback', 'excise tax & setback',
                                                                      fifelse(carbon_price_scenario != 'price floor' & setback_scenario != "no_setback", "carbon tax & setback", "BAU")))))]


## adjust so that setback is target
state_levels[, target := fifelse(setback_scenario != 'no_setback' & target == 'no_target', setback_scenario, target)]

# ## add indicator for normalized
# state_levels[, normalized := fifelse(metric %in% c("total_emp_norm", "total_comp_norm", "mortality_level_norm", "cost_PV_20_norm"),
#                                      "Normalized per 1000 people (>= 30 yo)", "Not normalized")]

## add GHG target name
## --------------------------------------------------

## 2045 emissions
ghg_2045 <- state_levels[metric == "total_state_ghg_MtCO2" &
                           year == 2045, .(scen_id, setback_existing, policy_intervention, oil_price_scenario, target, year, value)]

setnames(ghg_2045, "value", "ghg_2045")

ghg_2045[, ghg_2045_perc := (ghg_2045 - ghg_2019) / ghg_2019]

## setback 2045 end
setback_2045 <- ghg_2045[policy_intervention == "setback", .(oil_price_scenario, setback_existing, target, ghg_2045_perc)]
setback_2045[, target_label := paste0(round(ghg_2045_perc * -100), "%")]
setback_2045[, ghg_2045_perc := NULL]

## create ghg2045 df
ghg_2045[, ghg_2045 := NULL]
ghg_2045[, year := NULL]
ghg_2045[, policy_intervention := NULL]
ghg_2045[, target := NULL]
ghg_2045[, ghg_2045_perc_reduction := ghg_2045_perc * -100]


## merge with levels, save
state_levels <- merge(state_levels, setback_2045,
                      by = c("oil_price_scenario", "setback_existing", "target"),
                      all.x = T)

state_levels[, target_label := fifelse(policy_intervention == "BAU", target,
                                       fifelse(target == "90perc_reduction", "90%", target_label))]

state_levels <- merge(state_levels, ghg_2045,
                      by = c("scen_id", "setback_existing", "oil_price_scenario"),
                      all.x = T)


fwrite(state_levels, paste0(save_info_path, 'health-sensitivity/state_levels_all_oil.csv'))


## -----------------------------------------------------------------------
## relative to BAU
## -----------------------------------------------------------------------


## select health outputs

rel_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                     ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario, target, target_policy, year,
                                     mean_delta_total_pm25, mortality_delta, cost_2019, cost, cost_2019_PV, cost_PV)]

# ## inflate to 2020 USD
# rel_health_levels[, cost_PV_20 := cost_PV / cpi2019 * cpi2020]


rel_health_levels <- melt(rel_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario',
                                                         'carbon_price_scenario', 'ccs_scenario', 'setback_scenario',
                                                         'setback_existing', 'excise_tax_scenario',
                                                         'target', 'target_policy', 'year'),
                          measure.vars = c("mean_delta_total_pm25", "mortality_delta", "cost_2019", "cost", "cost_2019_PV", "cost_PV"),
                          variable.name = "metric",
                          value.name = "value")

## add target and policy
rel_health_levels[, policy_intervention := fifelse(carbon_price_scenario != "price floor" & setback_scenario == "no_setback", "carbon tax",
                                                   fifelse(setback_scenario != "no_setback" & carbon_price_scenario == 'price floor' & excise_tax_scenario == 'no tax', "setback",
                                                           fifelse(excise_tax_scenario != "no tax" & setback_scenario == "no_setback", "excise tax",
                                                                   fifelse(excise_tax_scenario != 'no tax' & setback_scenario != 'no_setback', 'excise tax & setback',
                                                                           fifelse(carbon_price_scenario != 'price floor' & setback_scenario != "no_setback", "carbon tax & setback", "BAU")))))]



## adjust so that setback is target
rel_health_levels[, target := fifelse(setback_scenario != 'no_setback' & target == 'no_target', setback_scenario, target)]


## rename
setnames(rel_health_levels, "value", "diff_bau")

# DELETE
# rel_health_levels[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", "medium CCS cost")]

rel_health_levels <- rel_health_levels[, .(scen_id, oil_price_scenario, setback_existing, year, metric, policy_intervention, target, diff_bau)]


## BAU outputs for labor and energy
bau_out <- state_levels[target == "no_target" & policy_intervention == "BAU" & metric %in% c("total_state_bbl",
                                                                                             "total_state_ghg_MtCO2"
                                                                                             # ,
                                                                                             # "total_emp",
                                                                                             # "total_comp_usd19",
                                                                                             # "total_comp_PV"
                                                                                             )]

setnames(bau_out, "value", "bau_value")
bau_out <- bau_out[, .(oil_price_scenario, setback_existing, year, metric, bau_value)]

## combine bau with scenario outputs
rel_vals <- state_levels[metric %in% c("total_state_bbl",
                                       "total_state_ghg_MtCO2"
                                       # ,
                                       # "total_emp",
                                       # "total_comp_usd19",
                                       # "total_comp_PV"
                                       )]

rel_vals <- merge(rel_vals, bau_out,
                  by = c("oil_price_scenario", "setback_existing", "year", "metric"),
                  all.x = T)

rel_vals[, diff_bau := value - bau_value]

rel_vals <- rel_vals[, .(scen_id, oil_price_scenario, setback_existing, year, metric, policy_intervention, target, diff_bau)]

## bind
state_rel_vals <- rbind(rel_vals, rel_health_levels)


## --------------------------------------------------------------------------------
## net cumul benefit x target
## -------------------------------------------------------------------------------

## 1) cumul benefit = cumul health benefit -compensation loss + carbon mitigation
## 2) cumul benefit / cumulative GHG emisisons

## social cost of carbon
scc_value <- state_rel_vals[metric == 'total_state_ghg_MtCO2']

## join with social cost of carbon
scc_value <- merge(scc_value, scc_df_filt,
                   by = 'year',
                   all.x = T)

## deflate to 2019 dollars
scc_value[, social_cost_usd19 := social_cost_co2 * (cpi2019 / cpi2020)]

scc_value[, scc_avoided_ghg := diff_bau * -1e6 * social_cost_usd19]

## apply discount rate
scc_value <- scc_value[year > 2019]
scc_value[, scc_avoided_ghg_pv := scc_avoided_ghg / ((1 + discount_rate) ^ (year - 2019))]


## summarise
cumul_scc_value <- scc_value[, .(scc_avoided_ghg = sum(scc_avoided_ghg, na.rm = T),
                                 scc_avoided_ghg_pv = sum(scc_avoided_ghg_pv)), by = .(scen_id, oil_price_scenario, setback_existing,
                                                                                       policy_intervention, target)]


cumul_rel_vals_bau <- state_rel_vals[, .(diff_bau = sum(diff_bau)), by = .(scen_id, oil_price_scenario, setback_existing, policy_intervention,
                                                                           target, metric)]

cumul_rel_vals_bau <- cumul_rel_vals_bau[metric %in% c("total_state_ghg_MtCO2", "total_comp_usd19", "total_comp_PV", "cost_2019",
                                                       "cost", "cost_2019_PV", "cost_PV")]

cumul_rel_vals_bau <- dcast(cumul_rel_vals_bau, scen_id + oil_price_scenario + setback_existing + policy_intervention + target ~ metric, value.var = "diff_bau")

## join witih scc
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, cumul_scc_value,
                            by = c('scen_id', 'oil_price_scenario', 'setback_existing', 'policy_intervention', 'target'),
                            all.x = T)

## total ghg emissions
ghg_total <- state_levels[metric == "total_state_ghg_MtCO2"]
ghg_total <- ghg_total[, .(cumul_ghg = sum(value)), by = .(scen_id)]

## join
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, ghg_total,
                            by = "scen_id",
                            all.x = T)




# ## calc benefit
# cumul_rel_vals_bau[, benefit := (cost_2019_PV * -1) + total_comp_PV + scc_avoided_ghg_pv]
# cumul_rel_vals_bau[, benefit_per_ghg := benefit / (-1 * total_state_ghg_MtCO2)]
# cumul_rel_vals_bau[, benefit_per_ghg := fifelse(is.na(benefit_per_ghg), 0, benefit)]

## benefit x metric
## -----------------------------------------

npv_x_metric <- melt(cumul_rel_vals_bau, id.vars = c('scen_id', 'oil_price_scenario', 'setback_existing', 'policy_intervention', 'target', 'cumul_ghg', 'total_state_ghg_MtCO2'),
                     measure.vars = c("cost_2019_PV", "scc_avoided_ghg_pv"),
                     variable.name = "metric",
                     value.name = "value")

npv_x_metric[, value := fifelse(metric == 'cost_2019_PV', value * -1, value)]
npv_x_metric[, value_per_ghg := value / (total_state_ghg_MtCO2 * -1)]
npv_x_metric[, value_billion := value / 1e9]
npv_x_metric[, value_per_ghg_million := value_per_ghg / 1e6]
npv_x_metric[, title := fifelse(metric == "total_comp_PV", "Labor: Compensation",
                                fifelse(metric == "cost_2019_PV", "Health: Avoided mortality", "Abated GHG"))]


npv_x_metric[, value_per_ghg_million := fifelse(is.na(value_per_ghg_million), 0, value_per_ghg_million)]

npv_x_metric <- merge(npv_x_metric, ghg_2045,
                      by = c('scen_id', 'oil_price_scenario', 'setback_existing'),
                      all.x = T)

## join with target label
npv_x_metric <- merge(npv_x_metric, setback_2045,
                      by = c("oil_price_scenario", "target", 'setback_existing'),
                      all.x = T)

npv_x_metric[, target_label := fifelse(policy_intervention == "BAU", target,
                                       fifelse(target == "90perc_reduction", "90%", target_label))]


fwrite(npv_x_metric, paste0(save_info_path, 'health-sensitivity/npv_x_metric_all_oil.csv'))

# ## -------------------------------------
# ## cumulative difference BAU
# ## -------------------------------------
# 
# 
# bau_cumulative_df <- state_rel_vals[year > 2019, .(sum_diff_bau = sum(diff_bau)), by = .(scen_id, ccs_option, metric, policy_intervention, target)]
# 
# bau_cumulative_df <- merge(bau_cumulative_df, ghg_2045,
#                            by = c("scen_id"),
#                            all.x = T)
# 
# bau_cumulative_df[, scen_name := paste(policy_intervention, target, sep = " - ")]
# 
# setnames(bau_cumulative_df, "sum_diff_bau", "sum_metric")
# 

## ------------------------------------------------------------
## DAC share
## -------------------------------------------------------------

# ## labor, county
# labor_out <- fread(paste0(main_path, extraction_folder_path, 'county-results/subset_county_results.csv'))
# # labor_out <- fread(paste0(extraction_folder_path, 'county-results/subset_county_results.csv'))

# labor_out[, target := NULL]
# 
# # ## delete
# # ## filter scens
# # labor_scens <- labor_out[scen_id %in% state_levels$scen_id]
# 
# ## add target, policy intervention, ccs_option
# labor_scens <- merge(labor_out, unique(state_levels[, .(scen_id, policy_intervention, setback_existing, target)]),
#                      by = c("scen_id", "setback_existing"),
#                      all.x = T)

# ## convert to 2019 usd
# labor_scens[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]
# 
# labor_scens[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate) ^ (year - 2019))]
# 
# labor_scens[, dac_comp := total_comp_usd19 * dac_share]
# labor_scens[, dac_comp_PV := total_comp_PV * dac_share]
# labor_scens[, dac_emp := total_emp * dac_share]
# 
# ## state-level cumulative dac and total 
# labor_dac_state <- labor_scens[, .(cumul_dac_comp = sum(dac_comp), 
#                                    cumul_total_comp = sum(total_comp_usd19),
#                                    cumul_dac_comp_PV = sum(dac_comp_PV),
#                                    cumul_total_comp_PV = sum(total_comp_PV),
#                                    cumul_dac_emp = sum(dac_emp),
#                                    cumul_total_emp = sum(total_emp)), by = .(scen_id, oil_price_scenario, carbon_price_scenario,
#                                                                              setback_scenario, setback_existing, excise_tax_scenario,
#                                                                              target, target_policy, policy_intervention)]
# 
# labor_dac_state[, dac_comp_share := cumul_dac_comp / cumul_total_comp]
# labor_dac_state[, dac_comp_pv_share := cumul_dac_comp_PV / cumul_total_comp_PV]
# labor_dac_state[, dac_emp_share := cumul_dac_emp / cumul_total_emp]
# 
# ## prepare for joining with health
# labor_dac_bind <- labor_dac_state[, .(scen_id, setback_existing, oil_price_scenario, target, target_policy, policy_intervention, 
#                                       cumul_dac_comp, cumul_total_comp, dac_comp_share,
#                                       cumul_dac_comp_PV, cumul_total_comp_PV, dac_comp_pv_share,
#                                       cumul_dac_emp, cumul_total_emp, dac_emp_share)]
# 
# labor_dac_bind <- melt(labor_dac_bind, measure.vars = c('cumul_dac_comp', 'cumul_total_comp', 'dac_comp_share',
#                                                         'cumul_dac_comp_PV', 'cumul_total_comp_PV', 'dac_comp_pv_share',
#                                                         'cumul_dac_emp', 'cumul_total_emp', 'dac_emp_share'),
#                        variable.name = "metric", value.name = "value")
# 
# labor_dac_bind[, type := fifelse(metric %in% c("cumul_dac_comp", "cumul_dac_comp_PV", "cumul_dac_emp"), "DAC",
#                                  fifelse(metric %in% c("cumul_total_comp", "cumul_total_comp_PV", "cumul_total_emp"), "Total", "DAC share"))]
# 
# 
# labor_dac_bind[, category := "Employment"]
# 
# 
# 
##----------------------------------------------------
## Health DAC
##----------------------------------------------------

health_out <- fread(paste0(main_path, extraction_folder_path, 'health-county-results/subset_county_hs_results.csv'))
# health_out <- fread(paste0(extraction_folder_path, 'census-tract-results/subset_census_tract_results.csv'))


health_out[, target := NULL]


## add target, policy intervention, ccs_option
health_scens <- merge(health_out, unique(state_levels[, .(scen_id, policy_intervention, setback_existing, target)]),
                      by = "scen_id",
                      all.x = T)

# ## calculate dac share, mortality
# health_scens[, dac_multiplier := fifelse(disadvantaged == "Yes", 1, 0)]

## calculate the cost associated with premature mortality
health_dac_state <- health_scens %>%
  left_join(growth_rates, by = c("year" = "year")) %>%
  rowwise() %>%
  mutate(VSL = future_WTP(income_elasticity_mort,
                          (cum_growth - 1),
                          VSL_2019),
         cost = mortality_level * VSL) %>%
  ungroup () %>%
  group_by(year) %>%
  mutate(cost_PV = cost/ ((1 + discount_rate) ^ (year - 2019))) %>%
  ungroup() %>%
  select(scen_id, setback_existing, GEOID, county, policy_intervention, target, target_policy, dac_share,
         mortality_level, cost, cost_PV)

setDT(health_dac_state)

## dac
health_dac_state[, dac_mort := dac_share * mortality_level]
health_dac_state[, dac_mort_cost := dac_share * cost]
health_dac_state[, dac_mort_pv := dac_share * cost_PV]

## aggregate at state level
health_dac_state <- health_dac_state[, .(cumul_dac_mort = sum(dac_mort),
                                         cumul_total_mort = sum(mortality_level),
                                         cumul_dac_cost = sum(dac_mort_cost),
                                         cumul_total_cost = sum(cost),
                                         cumul_dac_pv = sum(dac_mort_pv),
                                         cumul_total_pv = sum(cost_PV)), by = .(scen_id, setback_existing, target, target_policy, policy_intervention)]

health_dac_state[, dac_share_mort := cumul_dac_mort / cumul_total_mort]
health_dac_state[, dac_share_cost := cumul_dac_cost / cumul_total_cost]
health_dac_state[, dac_share_pv := cumul_dac_pv / cumul_total_pv]

# prepare for binding with labor
health_dac_bind <- melt(health_dac_state, measure.vars = c("cumul_dac_mort", "cumul_total_mort", "dac_share_mort",
                                                           "cumul_dac_cost", "cumul_total_cost", "dac_share_cost",
                                                           "cumul_dac_pv", "cumul_total_pv", "dac_share_pv"),
                        variable.name = "metric", value.name = "value")

health_dac_bind[, type := fifelse(metric %in% c("cumul_dac_mort", "cumul_dac_pv", "cumul_dac_cost"), "DAC",
                                  fifelse(metric %in% c("cumul_total_mort", "cumul_total_cost", "cumul_total_pv"), "Total", "DAC share"))]


health_dac_bind[, category := "Mortality"]

health_dac_bind[, oil_price_scenario := sub("-.*", "", scen_id)  ]

## bind
# dac_df <- rbind(health_dac_bind, labor_dac_bind)

dac_df <- merge(health_dac_bind, setback_2045,
                by = c("oil_price_scenario", "setback_existing", "target"),
                all.x = T)

dac_df[, target_label := fifelse(policy_intervention == "BAU", target,
                                 fifelse(target == "90perc_reduction", "90%", target_label))]

dac_df <- merge(dac_df, ghg_2045,
                by = c("scen_id", "setback_existing", "oil_price_scenario"),
                all.x = T)


fwrite(dac_df, paste0(save_info_path, 'health-sensitivity/dac_health_all_oil.csv'))




# ## -------------------------------------------------------
# ## Relative to BAU
# ## -------------------------------------------------------
# 
# ## bau
# bau_emp <- labor_scens[policy_intervention == "BAU", .(oil_price_scenario, setback_existing, county, year, total_emp, total_comp_usd19, total_comp_PV)]
# 
# 
# 
# setnames(bau_emp, c("total_emp", "total_comp_usd19", "total_comp_PV"), c("bau_emp", "bau_comp", "bau_pv"))
# 
# ## join
# labor_bau_dac <-  merge(labor_scens[, .(scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario,
#                                         setback_scenario, setback_existing, excise_tax_scenario, policy_intervention,
#                                         target, target_policy, county,
#                                         dac_share, year, total_emp, total_comp_usd19, total_comp_PV)], bau_emp,
#                         by = c("oil_price_scenario","setback_existing", "county", "year"),
#                         all.x = T)
# 
# labor_bau_dac[, diff_emp := total_emp - bau_emp]
# labor_bau_dac[, diff_comp := total_comp_usd19 - bau_comp]
# labor_bau_dac[, diff_pv := total_comp_PV - bau_pv]
# labor_bau_dac[, dac_emp := diff_emp * dac_share]
# labor_bau_dac[, dac_comp := diff_comp * dac_share]
# labor_bau_dac[, dac_pv := diff_pv * dac_share]
# 
# ## calculate dac share, labor FTE
# labor_bau_dac <- labor_bau_dac[, .(cumul_dac_emp_loss = sum(dac_emp),
#                                    cumul_total_emp_loss = sum(diff_emp),
#                                    cumul_dac_comp_loss = sum(dac_comp),
#                                    cumul_total_comp_loss = sum(diff_comp),
#                                    cumul_dac_pv_loss = sum(dac_pv),
#                                    cumul_total_pv_loss = sum(diff_pv)), by = .(scen_id, oil_price_scenario, carbon_price_scenario,
#                                                                                setback_scenario, setback_existing, excise_tax_scenario,
#                                                                                target, target_policy, policy_intervention)]
# 
# labor_bau_dac[, dac_share_emp := cumul_dac_emp_loss / cumul_total_emp_loss]
# labor_bau_dac[, dac_share_comp := cumul_dac_comp_loss / cumul_total_comp_loss]
# labor_bau_dac[, dac_share_pv := cumul_dac_pv_loss / cumul_total_pv_loss]
# 
# 
# # ## cumulative job loss rel bau / cumulative ghg savings rel bau
# # ## ---------------------------------------------------------------
# # 
# # ## add cumulative ghg savings relative to bau
# # cumul_ghg_df <- bau_cumulative_df[metric == "total_state_ghg_MtCO2", .(scen_id, sum_metric, ghg_2045_perc_reduction)]
# # setnames(cumul_ghg_df, "sum_metric", "cumul_ghg_savings")
# # 
# # ## labor ghg
# # labor_ghg_df <- merge(labor_dac_state, cumul_ghg_df,
# #                       by = "scen_id",
# #                       all.x = T)
# # 
# # labor_ghg_df[, dac_loss_ghg := cumul_dac_emp_loss / (cumul_ghg_savings * -1)]
# # labor_ghg_df[, total_loss_ghg := cumul_total_emp_loss / (cumul_ghg_savings * -1)]
# # labor_ghg_df[, dac_share_loss_ghg := dac_loss_ghg / total_loss_ghg]
# 
# labor_bau_dac <- melt(labor_bau_dac, id.vars = c("scen_id", "oil_price_scenario", "setback_existing",
#                                                  "target", "policy_intervention", "target_policy"),
#                       measure.vars = c("cumul_dac_emp_loss", "cumul_total_emp_loss", "dac_share_emp",
#                                        "cumul_dac_comp_loss", "cumul_total_comp_loss", "dac_share_comp",
#                                        "cumul_dac_pv_loss", "cumul_total_pv_loss", "dac_share_pv"),
#                       variable.name = "metric", value.name = "value")
# 
# labor_bau_dac[, type := fifelse(metric %in% c("cumul_dac_emp_loss", "cumul_dac_comp_loss", "cumul_dac_pv_loss"), "DAC population",
#                                 fifelse(metric %in% c("cumul_total_emp_loss", "cumul_total_comp_loss", "cumul_total_pv_loss"), "Total population", "DAC share"))]
# 
# labor_bau_dac$type <- factor(labor_bau_dac$type, levels = c("Total population", "DAC population", "DAC share"))
# 
# labor_bau_dac[, category := "Employment"]
# 
# 
## health, relative to BAU
## -----------------------------------------------------

health_dac_bau <- health_scens[, .(scen_id, setback_existing, target, target_policy, policy_intervention, GEOID, county, year, mortality_delta, cost, cost_PV, dac_share)]
health_dac_bau[, oil_price_scenario := sub("-.*", "", scen_id)]

health_dac_bau[, dac_av_mort := dac_share * mortality_delta]
health_dac_bau[, dac_av_mort_cost := dac_share * cost]
health_dac_bau[, dac_av_mort_cost_pv := dac_share * cost_PV]

## aggregate at state level
health_dac_bau <- health_dac_bau[, .(cumul_dac_av_mort = sum(dac_av_mort),
                                     cumul_total_av_mort = sum(mortality_delta),
                                     cumul_dac_av_mort_cost = sum(dac_av_mort_cost),
                                     cumul_total_av_mort_cost = sum(cost),
                                     cumul_dac_av_mort_pv = sum(dac_av_mort_cost_pv),
                                     cumul_total_av_mort_pv = sum(cost_PV)), by = .(scen_id, setback_existing, oil_price_scenario, target, policy_intervention, target_policy)]

health_dac_bau[, dac_share_av_mort := cumul_dac_av_mort / cumul_total_av_mort]
health_dac_bau[, dac_share_av_cost := cumul_dac_av_mort_cost / cumul_total_av_mort_cost]
health_dac_bau[, dac_share_av_pv := cumul_dac_av_mort_pv / cumul_total_av_mort_pv]

# health_dac_state[, dac_share_health := fifelse(is.na(dac_share_health), 0, dac_share_health)]

# ## add cumulative ghg savings relative to bau
#
# health_dac_ghg <- merge(health_dac_state, cumul_ghg_df,
#                         by = "scen_id",
#                         all.x = T)
#
# # health_dac_ghg[, dac_av_mort_ghg := cumul_dac_av_mort / cumul_ghg_savings]
# # health_dac_ghg[, total_av_mort_ghg := cumul_total_av_mort / cumul_ghg_savings]
# # health_dac_ghg[, DAC_share_av_mort_ghg := dac_av_mort_ghg / total_av_mort_ghg]
# # 
health_dac_bau <- melt(health_dac_bau, id.vars = c("scen_id", "oil_price_scenario", "setback_existing", "target", "policy_intervention",
                                                   "target_policy"),
                       measure.vars = c("cumul_dac_av_mort", "cumul_total_av_mort", "dac_share_av_mort",
                                        "cumul_dac_av_mort_cost", "cumul_total_av_mort_cost", "dac_share_av_cost",
                                        "cumul_dac_av_mort_pv", "cumul_total_av_mort_pv", "dac_share_av_pv"),
                       variable.name = "metric", value.name = "value")

health_dac_bau[, type := fifelse(metric %in% c("cumul_dac_av_mort", "cumul_dac_av_mort_cost", "cumul_dac_av_mort_pv"), "DAC population",
                                 fifelse(metric %in% c("cumul_total_av_mort", "cumul_total_av_mort_cost", "cumul_total_av_mort_pv"), "Total population", "DAC share"))]

health_dac_bau$type <- factor(health_dac_bau$type, levels = c("Total population", "DAC population", "DAC share"))

health_dac_bau[, category := "Avoided mortalities"]


## merge and save
dac_bau_df <- merge(health_dac_bau, setback_2045,
                    by = c("oil_price_scenario", "target", "setback_existing"),
                    all.x = T)

dac_bau_df[, target_label := fifelse(policy_intervention == "BAU", target,
                                     fifelse(target == "90perc_reduction", "90%", target_label))]

dac_bau_df <- merge(dac_bau_df, ghg_2045,
                    by = c("scen_id", "oil_price_scenario", "setback_existing"),
                    all.x = T)


fwrite(dac_bau_df, paste0(save_info_path, 'health-sensitivity/dac_bau_health_labor_all_oil.csv'))

##----------------------------------------------------------------------------
## Figure 3
##----------------------------------------------------------------------------

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items


## 
npv_dt <- npv_x_metric

## cumulative
npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'oil_price_scenario', 'setback_existing', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction', 'target_label'),
               measure.vars = c("value_billion", "value_per_ghg_million"),
               variable.name = "unit",
               value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2019 USD billion)", "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)")]

npv_dt[, measure_unit := fifelse(unit == "value_billion", "2019 USD billion", "2019 USD million / MtCO2e")]


npv_dt <- npv_dt[target != 'no_target']

npv_dt$target <- factor(npv_dt$target, levels = c('setback_1000ft', 'setback_2500ft', 'setback_5280ft',
                                                  '90perc_reduction'))



# npv_dt$target_label <- factor(npv_dt$target_label, levels = c("55%", "60%", "75%", "90%"))
# 

## update setback policy intervention (if setback_existing == 1, all wells)
npv_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 1), "setback (all wells)",
                                        policy_intervention)]

npv_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 0), "setback (new wells)",
                                        policy_intervention)]


npv_dt$policy_intervention <- factor(npv_dt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                            "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))


## figure 3b
fig_mtb <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      setback_existing == 0,
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "b. Health: avoided mortality (county analysis)",
       y = "NPV (2019 USD billion)",
       x = NULL) +
  ylim(0, 2) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## figure 3d
bquotelab <- bquote(MtCO[2]~e)
fig_mtd <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      setback_existing == 0,
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "d.",
       # y = expression(NPV~(2020~USD~million)~per~avoied~GHG ~MtCO[2]~e),
       y = bquote('NPV (2019 USD million)\nper avoided GHG MtCO'[2]~e),
       # y = bquote('NPV per avoided GHG MtCO'[2]~e'\n(2020 USD million / MtCO'[2]~e')'),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  ylim(0, 50) +
  theme_line_n +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

## ---------------------------------------------------------------
## recreate main text figure
## ---------------------------------------------------------------

fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/final/'

## csv names
npv_file_mt <- 'npv_x_metric_all_oil.csv'
dac_bau_file_mt <- 'dac_bau_health_labor_all_oil.csv' ## DAC shares relative to BAU

## read in data
npv_dt_mt <- fread(paste0(main_path, fig_path, npv_file_mt))
dac_bau_dt_mt <- fread(paste0(main_path, fig_path, dac_bau_file_mt))


## cumulative
npv_dt_mt <- npv_dt_mt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt_mt$title <- factor(npv_dt_mt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))

## pivot longer
npv_dt_mt <- melt(npv_dt_mt, id.vars = c('scen_id', 'oil_price_scenario', 'setback_existing', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction', 'target_label'),
               measure.vars = c("value_billion", "value_per_ghg_million"),
               variable.name = "unit",
               value.name = "value")

npv_dt_mt[, measure := fifelse(unit == "value_billion", "NPV (2019 USD billion)", "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)")]

npv_dt_mt[, measure_unit := fifelse(unit == "value_billion", "2019 USD billion", "2019 USD million / MtCO2e")]


npv_dt_mt <- npv_dt_mt[target != 'no_target']

npv_dt_mt$target <- factor(npv_dt_mt$target, levels = c('setback_1000ft', 'setback_2500ft', 'setback_5280ft',
                                                  '90perc_reduction'))


## update setback policy intervention (if setback_existing == 1, all wells)
npv_dt_mt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 1), "setback (all wells)",
                                        policy_intervention)]

npv_dt_mt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 0), "setback (new wells)",
                                        policy_intervention)]


npv_dt_mt$policy_intervention <- factor(npv_dt_mt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                            "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))


## figure 3a
fig_mta <- ggplot(npv_dt_mt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      setback_existing == 0,
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "a. Health: avoided mortality (main analysis)",
       y = "NPV (2019 USD billion)",
       x = NULL) +
  ylim(0, 2) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## figure 3c
fig_mtc <- ggplot(npv_dt_mt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      setback_existing == 0,
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "c.",
       # y = expression(NPV~(2020~USD~million)~per~avoied~GHG ~MtCO[2]~e),
       y = bquote('NPV (2019 USD million)\nper avoided GHG MtCO'[2]~e),
       # y = bquote('NPV per avoided GHG MtCO'[2]~e'\n(2020 USD million / MtCO'[2]~e')'),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  ylim(0, 50) +
  theme_line_n +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

## legend fig
legend_fig <- ggplot(npv_dt %>% filter(target != 'BAU',
                                       oil_price_scenario == "reference case",
                                       setback_existing == 0,
                                       !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                       title == "Labor: forgone wages",
                                       measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

legend_fig_3 <- get_legend(
  legend_fig + 
    theme(legend.title = element_text(size = 7),
          legend.text = element_text(size = 7))
  
)



## combine figure
## ---------------------------------
xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 7)

fig3_health_sens <- plot_grid(
  fig_mta,
  fig_mtb,
  fig_mtc + labs(x = NULL),
  fig_mtd  + labs(x = NULL),
  align = 'vh',
  # labels = c("A", "B", "C", "D", "E", "F"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1)
)

fig3_plot_grid2 <- plot_grid(
  fig3_health_sens,
  xaxis_lab,
  legend_fig_3,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.05, 0.05)
  # rel_widths = c(1, 1),
)


## save figure 3
ggsave(fig3_plot_grid2,
       filename = paste0(save_info_path, 'health-sensitivity/', 'figure3-health-sens.png'),
       width = 120,
       height = 150,
       units = "mm",)

ggsave(fig3_plot_grid2,
       filename = paste0(save_info_path, 'health-sensitivity/', 'figure3-health-sens.pdf'),
       width = 120,
       height = 150,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(save_info_path, 'health-sensitivity/', 'figure3-health-sens.pdf'),
            outfile = paste0(save_info_path, 'health-sensitivity/', 'figure3-health-sens.pdf'))





##----------------------------------------------------------------------------
## Figure 5
##----------------------------------------------------------------------------

dac_bau_df[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 1), "setback (all wells)",
                                            policy_intervention)]

dac_bau_df[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 0), "setback (new wells)",
                                            policy_intervention)]


dac_bau_df$policy_intervention <- factor(dac_bau_df$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                                    "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))




## version 1: relative to BAU
fig_5_health_sens <- ggplot(dac_bau_df %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                              oil_price_scenario == "reference case",
                                              setback_existing == 0,
                                              type == "DAC share",
                                              metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                          mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Health: avoided mortalities"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "b. Health: avoided mortalities\n   (county analysis)",
       color = "Policy",
       y = "DAC share",
       x = NULL) +
  # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.25, 0.35)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text = element_text(vjust = 0.5, hjust=1, size = 7),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.title = element_text(size = 7),
        title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

## main text version
## ------------------------------------------------------

dac_bau_dt_mt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 1), "setback (all wells)",
                                            policy_intervention)]

dac_bau_dt_mt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 0), "setback (new wells)",
                                            policy_intervention)]


dac_bau_dt_mt$policy_intervention <- factor(dac_bau_dt_mt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                                    "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))

## version 1: relative to BAU
fig_5_mt <- ggplot(dac_bau_dt_mt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                                  oil_price_scenario == "reference case",
                                                  setback_existing == 0,
                                                  type == "DAC share",
                                                  metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                              mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                        ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                              filter(facet_lab == "Health: avoided mortalities"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "a. Health: avoided mortalities\n   (main text analysis)",
       color = "Policy",
       y = "DAC share",
       x = NULL) +
  # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.25, 0.35)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text = element_text(vjust = 0.5, hjust=1, size = 7),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.title = element_text(size = 7),
        title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))






fig_5_sens <- plot_grid(
  fig_5_mt + theme(legend.position = "none"),
  fig_5_health_sens + theme(legend.position = "none"),
  align = 'vh',
  # labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 1,
  rel_widths = c(1, 1)
)

fig_5_sens2 <- plot_grid(
  fig_5_sens,
  xaxis_lab,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.025)
  # rel_widths = c(1, 1),
)

fig_5_sens3 <- plot_grid(
  fig_5_sens2,
  NULL,
  legend_fig_3,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.025, 0.05)
  # rel_widths = c(1, 1),
)


## save figure 4, v1
ggsave(fig_5_sens3,
       filename = paste0(save_info_path, 'health-sensitivity/', 'health-sens-dac.png'),
       width = 120,
       height = 100,
       units = "mm")

ggsave(fig_5_sens3,
       filename = paste0(save_info_path, 'health-sensitivity/', 'health-sens-dac.pdf'),
       width = 120,
       height = 100,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(save_info_path, 'health-sensitivity/', 'health-sens-dac.pdf'),
            outfile = paste0(save_info_path, 'health-sensitivity/', 'health-sens-dac.pdf'))







