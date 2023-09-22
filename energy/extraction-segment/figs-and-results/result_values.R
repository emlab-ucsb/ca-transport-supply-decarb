## Tracey Mangin
## September 21, 2023
## Pull out values

## libraries
library(data.table)
library(tidyverse)
library(sf)


## labor out
labor_fn  <- "labor_county_results.csv"
health_fn <- "health_ct_results.csv"
ct_fn     <- "tl_2019_06_tract.shp"

## paths
main_path             <- '/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/'
ct_data_path          <- paste0(main_path, 'data/GIS/raw/census-tract/')
save_info_path        <- paste0(main_path, 'outputs/academic-out/extraction/figures/nature-energy-revision/final/')

## read in outputs
labor_df <- fread(paste0(save_info_path, labor_fn))
health_df <- fread(paste0(save_info_path, health_fn))
ct_df <- st_read(paste0(ct_data_path, ct_fn))

## ct and counties
ct_county_df <- ct_df %>%
  select(COUNTYFP, GEOID) %>%
  st_drop_geometry() %>%
  rename(census_tract = GEOID)

## counties in ca
ca_counties <- tigris::counties("California") %>%
  select(COUNTYFP, NAME) %>%
  st_drop_geometry()

## Relative to BAU
## -------------------------------------------------------

## bau
bau_emp <- labor_df[policy_intervention == "BAU", .(oil_price_scenario, setback_existing, county, year, total_emp, total_comp_usd19, total_comp_PV)]

setnames(bau_emp, c("total_emp", "total_comp_usd19", "total_comp_PV"), c("bau_emp", "bau_comp", "bau_pv"))

## join
labor_bau_dac <-  merge(labor_df[, .(scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario,
                                        setback_scenario, setback_existing, excise_tax_scenario, policy_intervention,
                                        target, target_policy, county,
                                        dac_share, year, total_emp, total_comp_usd19, total_comp_PV)], bau_emp,
                        by = c("oil_price_scenario","setback_existing", "county", "year"),
                        all.x = T)

labor_bau_dac[, diff_emp := total_emp - bau_emp]
labor_bau_dac[, diff_comp := total_comp_usd19 - bau_comp]
labor_bau_dac[, diff_pv := total_comp_PV - bau_pv]

## calculate dac share, labor FTE
labor_bau_dac <- labor_bau_dac[, .(cumul_total_emp_loss = sum(diff_emp),
                                   cumul_total_comp_loss = sum(diff_comp),
                                   cumul_total_pv_loss = sum(diff_pv)), by = .(scen_id, county, oil_price_scenario, carbon_price_scenario,
                                                                               setback_scenario, setback_existing, excise_tax_scenario,
                                                                               target, target_policy, policy_intervention)]
## longer, select columns
labor_bau_dac <- labor_bau_dac %>%
  pivot_longer(cumul_total_emp_loss:cumul_total_pv_loss, names_to = "metric", values_to = "value") %>%
  mutate(segment = "labor") %>%
  select(scen_id, county, carbon_price_scenario, setback_scenario, target, target_policy, policy_intervention, segment, metric, value) %>%
  as.data.table()

## total_comp_PV = Labor: forgone wages

## -----------------------------------------------------
## health, relative to BAU
## -----------------------------------------------------

# cost_2019_PV = health avoided mortality

# health_dac_bau <- health_df[, .(scen_id, setback_existing, target, target_policy, policy_intervention, census_tract, year, mortality_delta, cost, cost_PV, dac_multiplier)]

health_df[, census_tract := paste0("0", census_tract)]
health_df[, census_tract := fifelse(census_tract == "06037930401", "06037137000", census_tract)]

## join with county codes
health_df <-  merge(health_df, ct_county_df,
                    by = c("census_tract"),
                    all.x = T)

## join with county names
health_df2 <-  merge(health_df, ca_counties,
                     by = c("COUNTYFP"),
                     all.x = T)

## aggregate at state level
health_df2 <- health_df2[, .(cumul_total_av_mort = sum(mortality_delta),
                             cumul_total_av_mort_cost = sum(cost),
                             cumul_total_av_mort_pv = sum(cost_PV)), by = .(scen_id, COUNTYFP, NAME, setback_existing, target, policy_intervention, target_policy)]


## create df for adding carbon price scenario and setback scenario to health
scen_info <- labor_df %>%
  select(scen_id, carbon_price_scenario, setback_scenario) %>%
  unique()

## align with labor df
health_df2 <- health_df2 %>%
  rename(county = NAME) %>%
  pivot_longer(cumul_total_av_mort:cumul_total_av_mort_pv, names_to = "metric", values_to = "value") %>%
  mutate(segment = "health") %>%
  left_join(scen_info) %>%
  select(scen_id, county, carbon_price_scenario, setback_scenario, target, target_policy, policy_intervention, segment, metric, value) %>%
  as.data.table()

## -----------------------------------------------------
## join and save
## ----------------------------------------------------- 

county_outputs <- rbind(labor_bau_dac, health_df2)


## state -----------------------------------------
state_out <- county_outputs[, .(value = sum(value)), by = .(scen_id, carbon_price_scenario, setback_scenario, target, policy_intervention, target_policy, segment, metric)]

state_sub <- state_out[scen_id %in% scen_list]

# fwrite(state_sub, paste0(save_info_path, 'state_labor_health_subset.csv'))

## compare counties
county_outputs2 <- county_outputs %>%
  left_join(state_out %>% rename(ca_value = value)) %>%
  mutate(rel_to_state = value / ca_value)

## save county outputs
fwrite(county_outputs2, paste0(save_info_path, 'county_state_health_labor_results.csv'))

## subset

## filter for Kern, existing == 0; 2500ft setback, 1 mile setback, 90% reduction with C tax
scen_list <- c("reference case-setback_2500ft-no quota-price floor-no ccs-low innovation-no tax-0",
               "reference case-setback_5280ft-no quota-price floor-no ccs-low innovation-no tax-0",
               "reference case-no_setback-no quota-carbon_target_90perc_reduction-no ccs-low innovation-no tax-0")

county_outputs2_sub <- county_outputs2 %>%
  filter(scen_id %in% scen_list &
           county == "Kern")


## save county outputs
fwrite(county_outputs2_sub, paste0(save_info_path, 'kern_state_health_labor_results.csv'))





