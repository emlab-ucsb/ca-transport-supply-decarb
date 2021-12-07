## Tracey Mangin
## December 6, 2021
## make outputs for figs

## libraries
library(data.table)
library(broom)
library(rebus)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path <- 'outputs/academic-out/extraction/extraction_2021-12-06/'
state_save_path     = paste0(main_path, extraction_folder_path, 'state-results/')


## create a folder to store outputs
cur_date              = Sys.Date()
save_info_path        = paste0(main_path, 'outputs/academic-out/extraction/figures/')
dir.create(save_info_path, showWarnings = FALSE)  

## files
ghg_file <- 'indust_emissions_2000-2019.csv'
scc_file <- 'social_cost_carbon.csv'
carbon_px_file <- 'carbon_price_scenarios_revised.xlsx'

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


fwrite(state_levels, paste0(save_info_path, 'state_levels_subset.csv'))

## cumulative outputs
## ------------------------------------

## 2045 emissions
ghg_2045 <- state_levels[metric == "total_state_ghg_MtCO2" &
                           year == 2045, .(scen_id, policy_intervention, target, year, value)]

setnames(ghg_2045, "value", "ghg_2045")

ghg_2045[, ghg_2045_perc := (ghg_2045 - ghg_2019) / ghg_2019]

ghg_2045[, ghg_2045 := NULL]
ghg_2045[, year := NULL]
ghg_2045[, policy_intervention := NULL]
ghg_2045[, target := NULL]
ghg_2045[, ghg_2045_perc_reduction := ghg_2045_perc * -100]

## 2019 values
vals_2019 <- unique(state_levels[year == 2019, .(metric, value)])
setnames(vals_2019, "value", "value_2019")
vals_2019[, value_2019 := fifelse(metric == "total_state_ghg_MtCO2", ghg_2019, value_2019)]


## calculate relative values
cumul_df <- merge(state_levels, vals_2019,
                  by = c("metric"),
                  all.x = T)

cumul_df[, diff_2019 := value - value_2019]

cumul_df[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", "medium CCS cost")]

cumul_df <- cumul_df[year > 2019, .(sum_metric = sum(diff_2019)), by = .(scen_id, ccs_option, policy_intervention, target, metric)]

cumul_df <- merge(cumul_df, ghg_2045,
                  by = c("scen_id"),
                  all.x = T)

cumul_df[, scen_name := paste(policy_intervention, target, sep = " - ")]

fwrite(cumul_df, paste0(save_info_path, 'state_cumulative_subset.csv'))


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

## add targed and policy
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

fwrite(npv_x_metric, paste0(save_info_path, 'npv_x_metric.csv'))


















## V2a: take the difference between BAU year t and scenario year t, add all years for cumulative impact, x axis == scenario name
bau_cumulative_df <- state_rel_vals[year > 2019, .(sum_diff_bau = sum(diff_bau)), by = .(scen_id, ccs_option, metric, policy_intervention, target)]

bau_cumulative_df <- merge(bau_cumulative_df, ghg_2045,
                           by = c("scen_id"),
                           all.x = T)

bau_cumulative_df[, scen_name := paste(policy_intervention, target, sep = " - ")]

setnames(bau_cumulative_df, "sum_diff_bau", "sum_metric")

