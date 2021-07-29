## Tracey Mangin
## September 29, 2020
## Scenario selection

library(tidyverse)
library(data.table)

## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
extract_path <- paste0(calepa_path, "outputs/predict-production/scenarios_20_all_scens/")
sselection_path <- paste0(calepa_path, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn")
# scen_figs <- paste0(calepa_path, "model-development/scenario-plot/")
# scen_path <- paste0(calepa_path, "project-materials/scenario-inputs/") 


## read in files
## -------------------------------------

## extraction outputs
extract_out <- read_csv(paste0(extract_path, "summary_output/state_level_extraction_outputs.csv"))

## refinign out
refining_out_net_exp <- read_csv(paste0(sselection_path, "ss_refinery_outputs/state_level_refining_outputs_net_exports.csv")) %>%
  filter(oil_price_scenario != "bp oil price")

refining_out_total_exp <- read_csv(paste0(sselection_path, "ss_refinery_outputs/state_level_refining_outputs_total_exports.csv")) %>%
  filter(oil_price_scenario != "bp oil price")

## refining state out
refinery_state_out_net_exp <- read_csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_state_net_exports.csv'))
refinery_state_out_total_exp <- read_csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_state_total_exports.csv'))


make_ss_outputs <- function(refinery_df, r_state_out, inc_exports) {

refinery_state_df <- r_state_out

refinery_ss_out <- refinery_df

included_exports <- inc_exports


## all scenarios for real
all_scen_combos <- expand.grid(oil_price_scenario = unique(extract_out$oil_price_scenario),
                               innovation_scenario =  unique(extract_out$innovation_scenario),
                               carbon_price_scenario = unique(extract_out$carbon_price_scenario),
                               ccs_scenario = unique(extract_out$ccs_scenario),
                               setback_scenario = unique(extract_out$setback_scenario),
                               prod_quota_scenario = unique(extract_out$prod_quota_scenario),
                               excise_tax_scenario = unique(extract_out$excise_tax_scenario),
                               demand_scenario = unique(refinery_ss_out$demand_scenario),
                               refining_scenario = unique(refinery_ss_out$refining_scenario),
                               segment = c("extraction", "refining"),
                               boundary = unique(refinery_state_df$boundary),
                               indicator = unique(c(unique(extract_out$indicator), unique(refinery_ss_out$indicator))))

## step 0: calculate movement indicator with production bbs in 2045

## refining consumption
refining_consump <- refinery_state_df %>%
  filter(year == 2045,
         type == "consumption",
         fuel == "crude",
         source == "traditional") %>%
  select(demand_scenario:ccs_scenario, refin_consum_2045 = value)

## add extraction, find movement
prod_bbls <- left_join(all_scen_combos, extract_out) %>%
  filter(segment == "refining") %>%
  filter(indicator == "production_bbl") %>%
  select(oil_price_scenario:boundary, prod_2045 = X2045) %>%
  left_join(refining_consump) %>%
  mutate(crude_net_movement = refin_consum_2045 - prod_2045,
         crude_net_movement_mmt = crude_net_movement / 1e6) %>%
  select(-crude_net_movement, - prod_2045, - refin_consum_2045) %>%
  mutate(indicator = "crude_net_movement_mmt") %>%
  rename(X2045 = crude_net_movement_mmt) %>%
  select(oil_price_scenario:boundary, indicator, X2045)

## filter refinign out for indicator
movement_df <- refinery_ss_out %>%
  filter(indicator == "crude_net_movement_mmt") %>%
  select(oil_price_scenario:X2019) 

movement_df2 <- left_join(prod_bbls, movement_df) %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019) %>%
  ungroup() %>%
  unique()

## step 1: merge all scenarios with extraction out and refining out
extract_out2 <- extract_out %>%
  mutate(segment = "extraction")

extract_out3 <- left_join(all_scen_combos, extract_out2) %>%
  filter(segment == "extraction")

## 
refining_out2 <- refinery_ss_out %>%
  mutate(segment = "refining") %>%
  filter(indicator != "crude_net_movement_mmt")

refining_out3 <- left_join(all_scen_combos, refining_out2) %>%
  filter(segment == "refining") %>%
  filter(indicator != "crude_net_movement_mmt") %>%
  rbind(movement_df2) 

## all outputs
all_outputs <- rbind(extract_out3, refining_out3)


## create summary (both segments)

total_df <- all_outputs %>%
  filter(indicator != "dac_ratio") %>%
  group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
           demand_scenario, refining_scenario, boundary, indicator) %>%
  summarise(X2019 = sum(X2019, na.rm = T),
            X2045 = sum(X2045, na.rm = T)) %>%
  ungroup() %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019) %>%
  mutate(segment = "extraction_and_refining") %>%
  mutate(X2019 = ifelse(indicator == "cumul_ghg_mmt", NA, X2019),
         diff = ifelse(indicator == "cumul_ghg_mmt", NA, diff),
         rel_change = ifelse(indicator == "cumul_ghg_mmt", NA, rel_change))

## dac ratio
total_dac <- total_df %>%
  filter(indicator %in% c("dac_pop_affected", "tot_pop_affected")) %>%
  select(-diff, - rel_change) %>%
  pivot_longer(X2019:X2045, names_to = "year", values_to = "population_n") %>%
  pivot_wider(names_from = indicator, values_from = population_n) %>%
  mutate(dac_ratio = dac_pop_affected / tot_pop_affected) %>%
  select(-dac_pop_affected, -tot_pop_affected) %>%
  as.data.table() %>%
  pivot_wider(names_from = year, values_from = dac_ratio) %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019,
         indicator = "dac_ratio")


all_total_df <- rbind(total_df, total_dac)

## all together now (all together now!)
all_segment_outputs <- rbind(all_outputs, all_total_df) %>%
  as.data.table()


write_csv(all_segment_outputs, paste0(extract_path, "summary_output/scenario_selection_outputs_", included_exports, ".csv"))


write_csv(all_segment_outputs, paste0(sselection_path, "ss-all-outputs/scenario_selection_outputs_", included_exports, ".csv"))

return(all_segment_outputs)


}

net_df <- make_ss_outputs(refinery_df = refining_out_net_exp, r_state_out = refinery_state_out_net_exp, inc_exports = "net_exports")
total_df <- make_ss_outputs(refinery_df = refining_out_total_exp, r_state_out = refinery_state_out_total_exp, inc_exports = "total_exports")


# test <- read_csv(paste0(extract_path, "summary_output/scenario_selection_outputs (1).csv")) 
# 
# test <- test %>%
#   rename(oX2019 = X2019,
#          oX2045 = X2045,
#          odiff = diff,
#          orel_change = rel_change) %>%
#   left_join(total_df) %>%
#   mutate(diff_rc = orel_change - rel_change)



# 
# 
# 
# ## refining ggplot
# ## --------------------------------------------------------------------
# 
# ggplot(refining_df, aes(x = rel_delta_emissions, y = total_empl, size = dac_pop_affected_diff * -1, color = adj_scenario, shape = demand_scenario)) +
#   geom_point(alpha = 0.7) +
#   facet_wrap(~buffer_dist) +
#   geom_hline(yintercept = 0, color = "black", size = 0.5) +
#   geom_vline(xintercept = 0, color = "black", size = 0.5)
# 
# ## ratio important or just number of dac pop affected?
# ## buffer only affects # of people affected?
# ## refining price value to calculate revenue?
# 
# ggplot(extract_df, aes(x = rel_delta_emissions, y = total_empl, size = dac_pop_affected_diff * -1, color = oil_price_scenario)) +
#   geom_point(alpha = 0.7) +
#   facet_wrap(~buffer_dist) +
#   geom_hline(yintercept = 0, color = "black", size = 0.5) +
#   geom_vline(xintercept = 0, color = "black", size = 0.5)
# 
