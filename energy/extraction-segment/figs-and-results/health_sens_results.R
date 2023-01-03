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

## paths 
main_path              = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path = 'outputs/academic-out/extraction/extraction_2022-12-27/'
state_save_path        = paste0(main_path, extraction_folder_path, 'state-results/')
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


## --------------------------------------------------------------------
## State levels
## --------------------------------------------------------------------

state_levels <- fread(paste0(save_info_path, state_levels))

# ##-------------------------------------------------------------------
# ## Health DAC -- summarize at county-level for sensitivity analysis
# ##-------------------------------------------------------------------

health_out <- fread(paste0(main_path, extraction_folder_path, 'census-tract-results/subset_census_tract_results.csv'),
                    colClasses = c(census_tract = "character"))
# health_out <- fread(paste0(extraction_folder_path, 'census-tract-results/subset_census_tract_results.csv'))

health_out <- merge(health_out, ces3[, .(census_tract, county)],
                    by = "census_tract",
                    all.x = T)

## remove target column
health_out[, target := NULL]


## add target, policy intervention, ccs_option
health_scens <- merge(health_out, unique(state_levels[, .(scen_id, policy_intervention, setback_existing, target)]),
                      by = "scen_id",
                      all.x = T)

## adjust health outcomes to be county means
health_scens[, m_cost_2019_PV := mean(cost_2019_PV), .(scen_id, year, county)]


# SDcols = c("mortality_delta", "mortality_level", 
#            "cost_2019", "cost", 
#            "cost_2019_PV", "cost_PV"), by = .(scen_id, year)]

## sumarize by scneario and year for fig 3


## rel health values for fig 3


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
  select(scen_id, setback_existing, census_tract, policy_intervention, target, target_policy, disadvantaged, dac_multiplier,
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
dac_df <- rbind(health_dac_bind, labor_dac_bind)

dac_df <- merge(dac_df, setback_2045,
                by = c("oil_price_scenario", "setback_existing", "target"),
                all.x = T)

dac_df[, target_label := fifelse(policy_intervention == "BAU", target,
                                 fifelse(target == "90perc_reduction", "90%", target_label))]

dac_df <- merge(dac_df, ghg_2045,
                by = c("scen_id", "setback_existing", "oil_price_scenario"),
                all.x = T)

# 
# fwrite(dac_df, paste0(save_info_path, 'dac_health_labor_all_oil.csv'))




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
# ## health, relative to BAU
# ## -----------------------------------------------------
# 
# health_dac_bau <- health_scens[, .(scen_id, setback_existing, target, target_policy, policy_intervention, census_tract, year, mortality_delta, cost, cost_PV, dac_multiplier)]
# health_dac_bau[, oil_price_scenario := sub("-.*", "", scen_id)]
# 
# health_dac_bau[, dac_av_mort := dac_multiplier * mortality_delta]
# health_dac_bau[, dac_av_mort_cost := dac_multiplier * cost]
# health_dac_bau[, dac_av_mort_cost_pv := dac_multiplier * cost_PV]
# 
# ## aggregate at state level
# health_dac_bau <- health_dac_bau[, .(cumul_dac_av_mort = sum(dac_av_mort),
#                                      cumul_total_av_mort = sum(mortality_delta),
#                                      cumul_dac_av_mort_cost = sum(dac_av_mort_cost),
#                                      cumul_total_av_mort_cost = sum(cost),
#                                      cumul_dac_av_mort_pv = sum(dac_av_mort_cost_pv),
#                                      cumul_total_av_mort_pv = sum(cost_PV)), by = .(scen_id, setback_existing, oil_price_scenario, target, policy_intervention, target_policy)]
# 
# health_dac_bau[, dac_share_av_mort := cumul_dac_av_mort / cumul_total_av_mort]
# health_dac_bau[, dac_share_av_cost := cumul_dac_av_mort_cost / cumul_total_av_mort_cost]
# health_dac_bau[, dac_share_av_pv := cumul_dac_av_mort_pv / cumul_total_av_mort_pv]
# 
# # health_dac_state[, dac_share_health := fifelse(is.na(dac_share_health), 0, dac_share_health)]
# 
# # ## add cumulative ghg savings relative to bau
# # 
# # health_dac_ghg <- merge(health_dac_state, cumul_ghg_df,
# #                         by = "scen_id",
# #                         all.x = T)
# # 
# # health_dac_ghg[, dac_av_mort_ghg := cumul_dac_av_mort / cumul_ghg_savings]
# # health_dac_ghg[, total_av_mort_ghg := cumul_total_av_mort / cumul_ghg_savings]
# # health_dac_ghg[, DAC_share_av_mort_ghg := dac_av_mort_ghg / total_av_mort_ghg]
# # 
# health_dac_bau <- melt(health_dac_bau, id.vars = c("scen_id", "oil_price_scenario", "setback_existing", "target", "policy_intervention",
#                                                    "target_policy"),
#                        measure.vars = c("cumul_dac_av_mort", "cumul_total_av_mort", "dac_share_av_mort",
#                                         "cumul_dac_av_mort_cost", "cumul_total_av_mort_cost", "dac_share_av_cost",
#                                         "cumul_dac_av_mort_pv", "cumul_total_av_mort_pv", "dac_share_av_pv"),
#                        variable.name = "metric", value.name = "value")
# 
# health_dac_bau[, type := fifelse(metric %in% c("cumul_dac_av_mort", "cumul_dac_av_mort_cost", "cumul_dac_av_mort_pv"), "DAC population",
#                                  fifelse(metric %in% c("cumul_total_av_mort", "cumul_total_av_mort_cost", "cumul_total_av_mort_pv"), "Total population", "DAC share"))]
# 
# health_dac_bau$type <- factor(health_dac_bau$type, levels = c("Total population", "DAC population", "DAC share"))
# 
# health_dac_bau[, category := "Avoided mortalities"]
# 
# 
# ## bind and save
# dac_bau_df <- rbind(labor_bau_dac, health_dac_bau)
# 
# dac_bau_df <- merge(dac_bau_df, setback_2045,
#                     by = c("oil_price_scenario", "target", "setback_existing"),
#                     all.x = T)
# 
# dac_bau_df[, target_label := fifelse(policy_intervention == "BAU", target,
#                                      fifelse(target == "90perc_reduction", "90%", target_label))]
# 
# dac_bau_df <- merge(dac_bau_df, ghg_2045,
#                     by = c("scen_id", "oil_price_scenario", "setback_existing"),
#                     all.x = T)
# 
# 
# fwrite(dac_bau_df, paste0(save_info_path, 'dac_bau_health_labor_all_oil.csv'))
# 
# 
# 
# 
# 
