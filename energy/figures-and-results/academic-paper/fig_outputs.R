## Tracey Mangin
## December 6, 2021
## make outputs for figs

## libraries
library(data.table)
library(broom)
library(rebus)
library(readxl)
library(openxlsx)

## paths 
main_path              = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path = 'outputs/academic-out/extraction/extraction_2021-12-06/'
state_save_path        = paste0(main_path, extraction_folder_path, 'state-results/')
field_out              = paste0(main_path, "outputs/predict-production/extraction_2021-12-06/subset_target_scens/field-out/")
health_out             = paste0(main_path, "outputs/academic-out/health/")

## create a folder to store outputs
cur_date              = Sys.Date()
save_info_path        = paste0(main_path, 'outputs/academic-out/extraction/figures/')
dir.create(save_info_path, showWarnings = FALSE)  

## files
ghg_file            = 'indust_emissions_2000-2019.csv'
scc_file            = 'social_cost_carbon.csv'
carbon_px_file      = 'carbon_price_scenarios_revised.xlsx'
field_cluster_file  = 'extraction_field_cluster_xwalk.csv'
cluster_pop_file    = 'extraction_cluster_affectedpop.csv'


## read in social cost of carbon
scc_df <- fread(paste0(main_path, 'data/stocks-flows/processed/', scc_file))

## filter for 3 percent
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

## CPI values
cpi_df <- setDT(read.xlsx(paste0(main_path, 'data/stocks-flows/processed/', carbon_px_file), sheet = 'BLS Data Series', startRow = 12))

cpi_df <- cpi_df[Year %in% c(2019, 2020), .(Year, Annual)]

setnames(cpi_df, c("Year", "Annual"), c("year", "annual"))

cpi2020 <- cpi_df %>%
  filter(year == 2020) %>%
  select(annual) %>%
  as.numeric()

cpi2019 <- cpi_df %>%
  filter(year == 2019) %>%
  select(annual) %>%
  as.numeric()

## discount rate
discount_rate <- 0.03

## health params
VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015 * 107.8645906/100 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
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
state_out <- fread(paste0(state_save_path, "subset_state_results.csv"))

# setnames(state_out, "county_pop", "state_pop")

## filter for BAU macro (ref oil price, price floor, low innovation, and no CCS cost)
## keep all setback scenarios (no tax, carbon price floor)
## keep all four excise tax scenarios (no setback, carbon price floor)
## keep all carbon taxes match the setback scenarios (no setback, no excise tax)

state_scens <- state_out[(oil_price_scenario == "reference case" &
                            carbon_price_scenario %in% c("carbon_setback_1000ft-no_setback-no ccs", "carbon_setback_2500ft-no_setback-no ccs",
                                                         "carbon_setback_5280ft-no_setback-no ccs", "carbon_90_perc_reduction-no_setback-no ccs") &
                            ccs_scenario %in% c("no ccs") &
                            setback_scenario == "no_setback" &
                            excise_tax_scenario == "no tax") |
                           (oil_price_scenario == "reference case" &
                              carbon_price_scenario %in% c("carbon_setback_1000ft-no_setback-medium CCS cost", "carbon_setback_2500ft-no_setback-medium CCS cost",
                                                           "carbon_setback_5280ft-no_setback-medium CCS cost", "carbon_90_perc_reduction-no_setback-medium CCS cost") &
                              ccs_scenario %in% c("medium CCS cost") &
                              setback_scenario == "no_setback" &
                              excise_tax_scenario == "no tax") |   
                           (oil_price_scenario == "reference case" &
                              carbon_price_scenario == "price floor" &
                              ccs_scenario %in% c("medium CCS cost", "no ccs")) |
                           (oil_price_scenario == "reference case" &
                              carbon_price_scenario == "price floor" &
                              ccs_scenario %in% c("medium CCS cost", "no ccs") &
                              setback_scenario == "no_setback" &
                              excise_tax_scenario == "no tax") |
                           (oil_price_scenario == "reference case" &
                              carbon_price_scenario %in% c("carbon_sb_90_perc_reduction-setback_1000ft-medium CCS cost",
                                                           "carbon_sb_90_perc_reduction-setback_2500ft-medium CCS cost",
                                                           "carbon_sb_90_perc_reduction-setback_5280ft-medium CCS cost",
                                                           "carbon_sb_90_perc_reduction-setback_1000ft-no ccs",
                                                           "carbon_sb_90_perc_reduction-setback_2500ft-no ccs",
                                                           "carbon_sb_90_perc_reduction-setback_5280ft-no ccs") &
                              excise_tax_scenario == "no tax")]

## note that the values in the state_pop column are different than the state_population df created
## in this script. The former only includes census tracts affected by oil production, while the latter
## includes all census tracts.

## merge with population series
state_scens <- merge(state_scens, state_population,
                     by = c("year"),
                     all.x = T)

state_labor_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                      ccs_scenario, excise_tax_scenario,  setback_scenario, year, total_emp, total_comp, state_pop, total_state_pop)]


## calc PV
state_labor_levels[, total_comp_PV := total_comp / ((1 + discount_rate) ^ (year - 2019))]


state_labor_levels <- state_labor_levels %>%
  mutate(total_emp_norm = total_emp / (total_state_pop / 1000),
         total_comp_norm = total_comp / (total_state_pop / 1000))

## melt
state_labor_levels <- melt(state_labor_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                           'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario', 'year'),
                           measure.vars = c("total_emp", "total_emp_norm", "total_comp", "total_comp_norm", "total_comp_PV"),
                           variable.name = "metric",
                           value.name = "value")


## emissions and extraction
## ------------------------------
state_extract_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                        ccs_scenario, setback_scenario, excise_tax_scenario, year, total_state_bbl, total_state_ghg_kgCO2)]

state_extract_levels[, total_state_ghg_MtCO2 := total_state_ghg_kgCO2 / (1000 * 1e6)]
state_extract_levels[, total_state_ghg_kgCO2 := NULL]

state_extract_levels <- melt(state_extract_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                               'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario',  'year'),
                             measure.vars = c("total_state_bbl", "total_state_ghg_MtCO2"),
                             variable.name = "metric",
                             value.name = "value")

## health
## ------------------------------
state_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, setback_scenario, excise_tax_scenario, year, mean_total_pm25, mean_delta_total_pm25, 
                                       mortality_level, mortality_delta, cost_2019_PV, cost_PV, state_pop)]


## convert to 2020 USD
state_health_levels[, cost_PV_20 := cost_PV / cpi2019 * cpi2020]

state_health_levels <- state_health_levels %>%
  left_join(state_population) %>%
  mutate(mortality_level_norm = mortality_level / (total_state_pop * 1000),
         cost_PV_20_norm = cost_PV_20 / (total_state_pop * 1000))



state_health_levels <- melt(state_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                             'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario', 'year'),
                            measure.vars = c("mean_total_pm25", "mean_delta_total_pm25", "mortality_level", 
                                             "mortality_delta", "cost_2019_PV", "cost_PV", "mortality_level_norm",
                                             "cost_PV_20_norm", "cost_PV_20"),
                            variable.name = "metric",
                            value.name = "value")

## combine
##----------------------------------

state_levels <- rbind(state_extract_levels, state_labor_levels, state_health_levels)





state_levels[, policy_intervention := fifelse(carbon_price_scenario != "price floor" & setback_scenario == "no_setback", "carbon tax",
                                              fifelse(setback_scenario != "no_setback" & carbon_price_scenario == 'price floor', "setback",
                                                      fifelse(excise_tax_scenario != "no tax", "excise tax",
                                                              fifelse(carbon_price_scenario != 'price floor' & setback_scenario != "no_setback", "carbon tax & setback",  "BAU"))))]

## add target
# state_levels[, tmp := fifelse(str_dectect(scen_id, 'carbon_sb') == TRUE, '90', carbon)]

state_levels[, target := as.numeric(str_extract(carbon_price_scenario, pattern = one_or_more(DIGIT)))]
state_levels[, target := fifelse(is.na(target), as.numeric(str_extract(excise_tax_scenario, pattern = one_or_more(DIGIT))), target)]
state_levels[, target := fifelse(is.na(target), as.numeric(str_extract(setback_scenario, pattern = one_or_more(DIGIT))), target)]

state_levels[, target := fifelse(target >= 1000, paste0(target, 'ft setback GHG'),
                                 fifelse(target < 1000, paste0(target, '% GHG reduction'), 'BAU'))]

state_levels[, target := fifelse(is.na(target), 'BAU', target)]

## add ccs
state_levels[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", "medium CCS cost")]

state_levels[, normalized := fifelse(metric %in% c("total_emp_norm", "total_comp_norm", "mortality_level_norm", "cost_PV_20_norm"),
                                     "Normalized per 1000 people (>= 30 yo)", "Not normalized")]

## add GHG target name
## --------------------------------------------------

## 2045 emissions
ghg_2045 <- state_levels[metric == "total_state_ghg_MtCO2" &
                           year == 2045, .(scen_id, policy_intervention, ccs_scenario, target, year, value)]

setnames(ghg_2045, "value", "ghg_2045")

ghg_2045[, ghg_2045_perc := (ghg_2045 - ghg_2019) / ghg_2019]

## setback 2045 end
setback_2045 <- ghg_2045[policy_intervention == "setback", .(ccs_scenario, target, ghg_2045_perc)]
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
                      by = c("ccs_scenario", "target"),
                      all.x = T)

state_levels[, target_label := fifelse(target == "BAU", target, 
                                       fifelse(target == "90% GHG reduction", "90%", target_label))]

state_levels <- merge(state_levels, ghg_2045,
                      by = c("scen_id", "ccs_scenario"),
                      all.x = T)


fwrite(state_levels, paste0(save_info_path, 'state_levels_subset.csv'))


## -----------------------------------------------------------------------
## relative to BAU
## -----------------------------------------------------------------------


## select health outputs

rel_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                     ccs_scenario, setback_scenario, excise_tax_scenario, year,
                                     mean_delta_total_pm25, mortality_delta, cost_2019, cost, cost_2019_PV, cost_PV)]


rel_health_levels[, cost_PV_20 := cost_PV / cpi2019 * cpi2020]


rel_health_levels <- melt(rel_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario',
                                                         'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario', 'year'),
                          measure.vars = c("mean_delta_total_pm25", "mortality_delta", "cost_2019", "cost", "cost_2019_PV", "cost_PV", "cost_PV_20"),
                          variable.name = "metric",
                          value.name = "value")

## add target and policy
rel_health_levels[, policy_intervention := fifelse(carbon_price_scenario != "price floor" & setback_scenario == "no_setback", "carbon tax",
                                              fifelse(setback_scenario != "no_setback" & carbon_price_scenario == 'price floor', "setback",
                                                      fifelse(excise_tax_scenario != "no tax", "excise tax",
                                                              fifelse(carbon_price_scenario != 'price floor' & setback_scenario != "no_setback", "carbon tax & setback",  "BAU"))))]

## add target

rel_health_levels[, target := as.numeric(str_extract(carbon_price_scenario, pattern = one_or_more(DIGIT)))]
rel_health_levels[, target := fifelse(is.na(target), as.numeric(str_extract(excise_tax_scenario, pattern = one_or_more(DIGIT))), target)]
rel_health_levels[, target := fifelse(is.na(target), as.numeric(str_extract(setback_scenario, pattern = one_or_more(DIGIT))), target)]

rel_health_levels[, target := fifelse(target >= 1000, paste0(target, 'ft setback GHG'),
                                 fifelse(target < 1000, paste0(target, '% GHG reduction'), 'BAU'))]

rel_health_levels[, target := fifelse(is.na(target), 'BAU', target)]

## rename
setnames(rel_health_levels, "value", "diff_bau")


rel_health_levels[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", "medium CCS cost")]

rel_health_levels <- rel_health_levels[, .(scen_id, ccs_option, year, metric, policy_intervention, target, diff_bau)]


## BAU outputs for labor and energy
bau_out <- state_levels[target == "BAU" & policy_intervention == "BAU" & metric %in% c("total_state_bbl",
                                                                                       "total_state_ghg_MtCO2",
                                                                                       "total_emp",
                                                                                       "total_comp",
                                                                                       "total_comp_PV")]

setnames(bau_out, "value", "bau_value")
bau_out <- bau_out[, .(ccs_option, year, metric, bau_value)]

## combine bau with scenario outputs
rel_vals <- state_levels[metric %in% c("total_state_bbl",
                                       "total_state_ghg_MtCO2",
                                       "total_emp",
                                       "total_comp",
                                       "total_comp_PV")]

rel_vals <- merge(rel_vals, bau_out,
                  by = c("ccs_option", "year", "metric"),
                  all.x = T)

rel_vals[, diff_bau := value - bau_value]

rel_vals[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", "medium CCS cost")]

rel_vals <- rel_vals[, .(scen_id, ccs_option, year, metric, policy_intervention, target, diff_bau)]

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

scc_value[, scc_avoided_ghg := diff_bau * -1e6 * social_cost_co2]

## summarise
cumul_scc_value <- scc_value[, .(scc_avoided_ghg = sum(scc_avoided_ghg, na.rm = T)), by = .(scen_id, ccs_option,
                                                                                            policy_intervention, target)]


cumul_rel_vals_bau <- state_rel_vals[, .(diff_bau = sum(diff_bau)), by = .(scen_id, ccs_option, policy_intervention,
                                                                           target, metric)]

cumul_rel_vals_bau <- cumul_rel_vals_bau[metric %in% c("total_state_ghg_MtCO2", "total_comp", "total_comp_PV", "cost_2019",
                                                       "cost", "cost_2019_PV", "cost_PV", "cost_PV_20")]

cumul_rel_vals_bau <- dcast(cumul_rel_vals_bau, scen_id + ccs_option + policy_intervention + target ~ metric, value.var = "diff_bau")

## join witih scc
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, cumul_scc_value,
                            by = c('scen_id', 'ccs_option', 'policy_intervention', 'target'),
                            all.x = T)

## total ghg emissions
ghg_total <- state_levels[metric == "total_state_ghg_MtCO2"]
ghg_total <- ghg_total[, .(cumul_ghg = sum(value)), by = .(scen_id)]

## join
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, ghg_total,
                            by = "scen_id",
                            all.x = T)




## calc benefit
cumul_rel_vals_bau[, benefit := (cost_PV_20 * -1) + total_comp_PV + scc_avoided_ghg]
cumul_rel_vals_bau[, benefit_per_ghg := benefit / (-1 * total_state_ghg_MtCO2)]
cumul_rel_vals_bau[, benefit_per_ghg := fifelse(is.na(benefit_per_ghg), 0, benefit)]

## benefit x metric
## -----------------------------------------

npv_x_metric <- melt(cumul_rel_vals_bau, id.vars = c('scen_id', 'ccs_option', 'policy_intervention', 'target', 'cumul_ghg', 'total_state_ghg_MtCO2'),
                     measure.vars = c("total_comp_PV", "cost_PV_20", "scc_avoided_ghg"),
                     variable.name = "metric",
                     value.name = "value")

npv_x_metric[, value := fifelse(metric == 'cost_PV_20', value * -1, value)]
npv_x_metric[, value_per_ghg := value / (total_state_ghg_MtCO2 * -1)]
npv_x_metric[, value_billion := value / 1e9]
npv_x_metric[, value_per_ghg_million := value_per_ghg / 1e6]
npv_x_metric[, title := fifelse(metric == "total_comp_PV", "Labor: Compensation",
                                fifelse(metric == "cost_PV_20", "Health: Avoided mortality", "Abated GHG"))]


npv_x_metric[, value_per_ghg_million := fifelse(is.na(value_per_ghg_million), 0, value_per_ghg_million)]

npv_x_metric <- merge(npv_x_metric, ghg_2045,
                       by = 'scen_id',
                       all.x = T)

## join with target label
npv_x_metric <- merge(npv_x_metric, setback_2045,
                      by = c("ccs_scenario", "target"),
                      all.x = T)

npv_x_metric[, target_label := fifelse(target == "BAU", target, 
                                       fifelse(target == "90% GHG reduction", "90%", target_label))]



fwrite(npv_x_metric, paste0(save_info_path, 'npv_x_metric.csv'))

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


## ------------------------------------------------------------
## DAC share
## -------------------------------------------------------------

## labor, county
labor_out <- fread(paste0(main_path, extraction_folder_path, 'county-results/subset_county_results.csv'))

## filter scens
labor_scens <- labor_out[scen_id %in% state_levels$scen_id]

## add target, policy intervention, ccs_option
labor_scens <- merge(labor_scens, unique(state_levels[, .(scen_id, policy_intervention, target, ccs_option)]),
                     by = "scen_id",
                     all.x = T)

labor_scens[, total_comp_PV := total_comp / ((1 + discount_rate) ^ (year - 2019))]

labor_scens[, dac_comp := total_comp * dac_share]
labor_scens[, dac_comp_PV := total_comp_PV * dac_share]
labor_scens[, dac_emp := total_emp * dac_share]

## state-level cumulative dac and total 
labor_dac_state <- labor_scens[, .(cumul_dac_comp = sum(dac_comp), 
                                   cumul_total_comp = sum(total_comp),
                                   cumul_dac_comp_PV = sum(dac_comp_PV),
                                   cumul_total_comp_PV = sum(total_comp_PV),
                                   cumul_dac_emp = sum(dac_emp),
                                   cumul_total_emp = sum(total_emp)), by = .(scen_id, oil_price_scenario, carbon_price_scenario,
                                                                             setback_scenario, excise_tax_scenario,
                                                                             target, policy_intervention, ccs_option)]

labor_dac_state[, dac_comp_share := cumul_dac_comp / cumul_total_comp]
labor_dac_state[, dac_comp_pv_share := cumul_dac_comp_PV / cumul_total_comp_PV]
labor_dac_state[, dac_emp_share := cumul_dac_emp / cumul_total_emp]

## prepare for joining with health
labor_dac_bind <- labor_dac_state[, .(scen_id, target, policy_intervention, ccs_option, 
                                      cumul_dac_comp, cumul_total_comp, dac_comp_share,
                                      cumul_dac_comp_PV, cumul_total_comp_PV, dac_comp_pv_share,
                                      cumul_dac_emp, cumul_total_emp, dac_emp_share)]

labor_dac_bind <- melt(labor_dac_bind, measure.vars = c('cumul_dac_comp', 'cumul_total_comp', 'dac_comp_share',
                                                        'cumul_dac_comp_PV', 'cumul_total_comp_PV', 'dac_comp_pv_share',
                                                        'cumul_dac_emp', 'cumul_total_emp', 'dac_emp_share'),
                       variable.name = "metric", value.name = "value")

labor_dac_bind[, type := fifelse(metric %in% c("cumul_dac_comp", "cumul_dac_comp_PV", "cumul_dac_emp"), "DAC",
                                 fifelse(metric %in% c("cumul_total_comp", "cumul_total_comp_PV", "cumul_total_emp"), "Total", "DAC share"))]


labor_dac_bind[, category := "Employment"]



##----------------------------------------------------
## Health DAC
##----------------------------------------------------

health_out <- fread(paste0(main_path, extraction_folder_path, 'census-tract-results/subset_census_tract_results.csv'))

## filter scens
health_scens <- health_out[scen_id %in% state_levels$scen_id]

## add target, policy intervention, ccs_option
health_scens <- merge(health_scens, unique(state_levels[, .(scen_id, policy_intervention, target, ccs_option)]),
                     by = "scen_id",
                     all.x = T)

## calculate dac share, mortality
health_scens[, dac_multiplier := fifelse(disadvantaged == "Yes", 1, 0)]

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
  select(scen_id, census_tract,policy_intervention, target, ccs_option, disadvantaged, dac_multiplier,
         mortality_level, cost, cost_PV)

setDT(health_dac_state)

## dac 
health_dac_state[, dac_mort := dac_multiplier * mortality_level]
health_dac_state[, dac_mort_cost := dac_multiplier * cost]
health_dac_state[, dac_mort_pv := dac_multiplier * cost_PV]

## aggregate at state level
health_dac_state <- health_dac_state[, .(cumul_dac_mort = sum(dac_mort), 
                                     cumul_total_mort = sum(mortality_level),
                                     cumul_dac_cost = sum(dac_mort_cost),
                                     cumul_total_cost = sum(cost),
                                     cumul_dac_pv = sum(dac_mort_pv),
                                     cumul_total_pv = sum(cost_PV)), by = .(scen_id, target, policy_intervention, ccs_option)]

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

## bind
dac_df <- rbind(health_dac_bind, labor_dac_bind)

dac_df[, ccs_scenario := fifelse(ccs_option == "no CCS", "no ccs", "medium CCS cost")]


dac_df <- merge(dac_df, setback_2045,
                by = c("ccs_scenario", "target"),
                all.x = T)

dac_df[, target_label := fifelse(target == "BAU", target,
                                       fifelse(target == "90% GHG reduction", "90%", target_label))]

dac_df <- merge(dac_df, ghg_2045,
                      by = c("scen_id", "ccs_scenario"),
                      all.x = T)


fwrite(dac_df, paste0(save_info_path, 'dac_health_labor.csv'))




## -------------------------------------------------------
## Relative to BAU
## -------------------------------------------------------

## bau
bau_emp <- labor_scens[policy_intervention == "BAU", .(ccs_scenario, ccs_option, county, year, total_emp, total_comp, total_comp_PV)]
setnames(bau_emp, c("total_emp", "total_comp", "total_comp_PV"), c("bau_emp", "bau_comp", "bau_pv"))

## join
labor_bau_dac <-  merge(labor_scens[, .(scen_id, carbon_price_scenario, ccs_scenario,
                                        setback_scenario, excise_tax_scenario, policy_intervention,
                                        target, ccs_option, county,
                                        dac_share, year, total_emp, total_comp, total_comp_PV)], bau_emp,
                        by = c("ccs_scenario", "ccs_option", "county", "year"),
                        all.x = T)

labor_bau_dac[, diff_emp := total_emp - bau_emp]
labor_bau_dac[, diff_comp := total_comp - bau_comp]
labor_bau_dac[, diff_pv := total_comp_PV - bau_pv]
labor_bau_dac[, dac_emp := diff_emp * dac_share]
labor_bau_dac[, dac_comp := diff_comp * dac_share]
labor_bau_dac[, dac_pv := diff_pv * dac_share]

## calculate dac share, labor FTE
labor_bau_dac <- labor_bau_dac[, .(cumul_dac_emp_loss = sum(dac_emp),
                                   cumul_total_emp_loss = sum(diff_emp),
                                   cumul_dac_comp_loss = sum(dac_comp),
                                   cumul_total_comp_loss = sum(diff_comp),
                                   cumul_dac_pv_loss = sum(dac_pv),
                                   cumul_total_pv_loss = sum(diff_pv)), by = .(scen_id, carbon_price_scenario,
                                                                           setback_scenario, excise_tax_scenario,
                                                                           target, policy_intervention, ccs_option)]

labor_bau_dac[, dac_share_emp := cumul_dac_emp_loss / cumul_total_emp_loss]
labor_bau_dac[, dac_share_comp := cumul_dac_comp_loss / cumul_total_comp_loss]
labor_bau_dac[, dac_share_pv := cumul_dac_pv_loss / cumul_total_pv_loss]


# ## cumulative job loss rel bau / cumulative ghg savings rel bau
# ## ---------------------------------------------------------------
# 
# ## add cumulative ghg savings relative to bau
# cumul_ghg_df <- bau_cumulative_df[metric == "total_state_ghg_MtCO2", .(scen_id, sum_metric, ghg_2045_perc_reduction)]
# setnames(cumul_ghg_df, "sum_metric", "cumul_ghg_savings")
# 
# ## labor ghg
# labor_ghg_df <- merge(labor_dac_state, cumul_ghg_df,
#                       by = "scen_id",
#                       all.x = T)
# 
# labor_ghg_df[, dac_loss_ghg := cumul_dac_emp_loss / (cumul_ghg_savings * -1)]
# labor_ghg_df[, total_loss_ghg := cumul_total_emp_loss / (cumul_ghg_savings * -1)]
# labor_ghg_df[, dac_share_loss_ghg := dac_loss_ghg / total_loss_ghg]

labor_bau_dac <- melt(labor_bau_dac, id.vars = c("scen_id", 
                                               "target", "policy_intervention", "ccs_option"),
                     measure.vars = c("cumul_dac_emp_loss", "cumul_total_emp_loss", "dac_share_emp",
                                      "cumul_dac_comp_loss", "cumul_total_comp_loss", "dac_share_comp",
                                      "cumul_dac_pv_loss", "cumul_total_pv_loss", "dac_share_pv"),
                     variable.name = "metric", value.name = "value")

labor_bau_dac[, type := fifelse(metric %in% c("cumul_dac_emp_loss", "cumul_dac_comp_loss", "cumul_dac_pv_loss"), "DAC population",
                               fifelse(metric %in% c("cumul_total_emp_loss", "cumul_total_comp_loss", "cumul_total_pv_loss"), "Total population", "DAC share"))]

labor_bau_dac$type <- factor(labor_bau_dac$type, levels = c("Total population", "DAC population", "DAC share"))

labor_bau_dac[, category := "Employment"]


## health, relative to BAU
## -----------------------------------------------------

health_dac_bau <- health_scens[, .(scen_id, target, ccs_option, policy_intervention, census_tract, year, mortality_delta, cost, cost_PV, dac_multiplier)]

health_dac_bau[, dac_av_mort := dac_multiplier * mortality_delta]
health_dac_bau[, dac_av_mort_cost := dac_multiplier * cost]
health_dac_bau[, dac_av_mort_cost_pv := dac_multiplier * cost_PV]

## aggregate at state level
health_dac_bau <- health_dac_bau[, .(cumul_dac_av_mort = sum(dac_av_mort),
                                     cumul_total_av_mort = sum(mortality_delta),
                                     cumul_dac_av_mort_cost = sum(dac_av_mort_cost),
                                     cumul_total_av_mort_cost = sum(cost),
                                     cumul_dac_av_mort_pv = sum(dac_av_mort_cost_pv),
                                     cumul_total_av_mort_pv = sum(cost_PV)), by = .(scen_id, target, policy_intervention, ccs_option)]

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
# health_dac_ghg[, dac_av_mort_ghg := cumul_dac_av_mort / cumul_ghg_savings]
# health_dac_ghg[, total_av_mort_ghg := cumul_total_av_mort / cumul_ghg_savings]
# health_dac_ghg[, DAC_share_av_mort_ghg := dac_av_mort_ghg / total_av_mort_ghg]
# 
health_dac_bau <- melt(health_dac_bau, id.vars = c("scen_id", "target", "policy_intervention",
                                                        "ccs_option"),
                            measure.vars = c("cumul_dac_av_mort", "cumul_total_av_mort", "dac_share_av_mort",
                                             "cumul_dac_av_mort_cost", "cumul_total_av_mort_cost", "dac_share_av_cost",
                                             "cumul_dac_av_mort_pv", "cumul_total_av_mort_pv", "dac_share_av_pv"),
                            variable.name = "metric", value.name = "value")

health_dac_bau[, type := fifelse(metric %in% c("cumul_dac_av_mort", "cumul_dac_av_mort_cost", "cumul_dac_av_mort_pv"), "DAC population",
                                          fifelse(metric %in% c("cumul_total_av_mort", "cumul_total_av_mort_cost", "cumul_total_av_mort_pv"), "Total population", "DAC share"))]

health_dac_bau$type <- factor(health_dac_bau$type, levels = c("Total population", "DAC population", "DAC share"))

health_dac_bau[, category := "Avoided mortalities"]


## bind and save
dac_bau_df <- rbind(labor_bau_dac, health_dac_bau)

dac_bau_df[, ccs_scenario := fifelse(ccs_option == "no CCS", "no ccs", "medium CCS cost")]


dac_bau_df <- merge(dac_bau_df, setback_2045,
                by = c("ccs_scenario", "target"),
                all.x = T)

dac_bau_df[, target_label := fifelse(target == "BAU", target,
                                 fifelse(target == "90% GHG reduction", "90%", target_label))]

dac_bau_df <- merge(dac_bau_df, ghg_2045,
                by = c("scen_id", "ccs_scenario"),
                all.x = T)


fwrite(dac_bau_df, paste0(save_info_path, 'dac_bau_health_labor.csv'))






## what is the state dac proportion through time?
## -----------------------------------------------------

dac_ces <- read_xlsx(paste0(main_path, 'data/health/raw/ces3results.xlsx'))

dac_df <- dac_ces %>%
  select(`Census Tract`, `SB 535 Disadvantaged Community`) %>%
  rename(census_tract = `Census Tract`,
         dac_status = `SB 535 Disadvantaged Community`) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) 


# Population and incidence
ct_inc_pop_45 <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

## census tract, population > 29
ct_pop_time_30 <- ct_inc_pop_45 %>%
  filter(lower_age > 29) %>%
  group_by(ct_id, year) %>%
  summarize(ct_pop_30plus = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  rename(census_tract = ct_id) %>%
  as.data.table()


## census tract, total population through time
ct_pop_time_all <- ct_inc_pop_45 %>%
  group_by(ct_id, year) %>%
  summarize(ct_pop_all = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  rename(census_tract = ct_id) %>%
  as.data.table()

## dac share over time
ct_pop_time <- merge(ct_pop_time_30, ct_pop_time_all, 
                     by = c("census_tract", "year"),
                     all.x = T)

## pivot longer
ct_pop_time <- pivot_longer(ct_pop_time, ct_pop_30plus:ct_pop_all, names_to = "pop_type", values_to = "pop")

## merge with dac
ct_pop_time <- merge(ct_pop_time, dac_df, 
                     by = c("census_tract"),
                     all.x = T)

setDT(ct_pop_time)

## calc annual dac share
ct_pop_time[, dac_mult := fifelse(dac_status == "Yes", 1, 0)]

ct_pop_time[, dac_pop := pop * dac_mult]

dac_pop_time <- ct_pop_time[, lapply(.SD, sum, na.rm = T), .SDcols = c("pop", "dac_pop"), by = .(year, pop_type)]
dac_pop_time[, dac_ratio := dac_pop / pop]

## diff between all vs 30+?
diff_dac_df <- pivot_wider(dac_pop_time %>% select(year, pop_type, dac_ratio), names_from = pop_type, values_from = dac_ratio) %>%
  mutate(diff = ct_pop_all - ct_pop_30plus)

## differences are 2-3% (dac pop all > dac share 30+)

## save
fwrite(dac_pop_time, paste0(save_info_path, 'state_dac_ratios.csv'))



### --------------------------------------------------------------------
## avg # of people/workers affected per bbl (remaining field, exiting field)
## --------------------------------------------------------------------

## excise, carbon, setback 1-mile

field_files <- paste0(field_out, c("reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_field.rds",
                                   "reference case_setback_5280ft_no quota_price floor_no ccs_low innovation_no tax_field.rds",
                                   "reference case_no_setback_no quota_carbon_setback_5280ft-no_setback-no ccs_no ccs_low innovation_no tax_field.rds",
                                   "reference case_no_setback_no quota_price floor_no ccs_low innovation_tax_setback_5280ft_field.rds"))

field_dt = setDT(rbindlist(lapply(field_files, readRDS)))

field_dt <- field_dt[, .(total_prod_bbl = sum(total_prod_bbl)), by = .(scen_id, carbon_price_scenario, setback_scenario,
                                                                       excise_tax_scenario, doc_field_code, doc_fieldname,
                                                                       year)]

## people affected

##  cross walk between >200 oil fields and ~20 field cluster. 
## Use this crosswalk to go from oil fields up to field clusters.
field_cluster_xwalk <- fread(paste0(health_out, field_cluster_file))
field_cluster_xwalk[, NAME := NULL]
field_cluster_xwalk[, input_fid := NULL]
field_cluster_xwalk[, doc_field_code := sprintf("%03d", doc_field_code)]


## cluster-level number of affected population, called affected_pop (from a single pulse of PM25) 
## and population weighted DAC share, called share_dac_weighted. Use these two variables.
cluster_pop_dt <- fread(paste0(health_out, cluster_pop_file))

## join the data sets
field_dt_health <- merge(field_dt, field_cluster_xwalk,
                         by = "doc_field_code",
                         all.x = T)

field_dt_health <-  merge(field_dt_health, cluster_pop_dt,
                          by = "id",
                          all.x = T)

## save
fwrite(field_dt_health, paste0(save_info_path, 'field_dt_health.csv'))


