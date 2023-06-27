#From ambient concentration to monetized mortality
#Code chunks taken from ca-transport-supply-decarb/energy/extraction-segment/compile-outputs/compile_extraction_extraction_outputs_full.R


#1 load census tract population and mortality incidence rates
ct_inc_pop_45 <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

#2 census-tract level population-weighted incidence rate (for age>29)
ct_inc_pop_45_weighted <- ct_inc_pop_45 %>%
  filter(lower_age > 29) %>%
  group_by(ct_id, year) %>%
  mutate(ct_pop = sum(pop, na.rm = T),
         share = pop/ct_pop,
         weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
  summarize(weighted_incidence = unique(weighted_incidence),
            pop = unique(ct_pop)) %>%
  ungroup() %>%
  as.data.table()

#3 Coefficients from Krewski et al (2009) for mortality impact
beta <- 0.00582
se <- 0.0009628

#4 for monetary mortality impact - growth in income for use in WTP function
growth_rates <- read.csv(paste0(main_path, "data/benmap/processed/growth_rates.csv"), stringsAsFactors = FALSE) %>%
  filter(year > 2018) %>%
  mutate(growth = ifelse(year == 2019, 0, growth_2030),
         cum_growth = cumprod(1 + growth)) %>%
  select(-growth_2030, -growth) %>%
  as.data.table()

#5 Parameters for monetized health impact
VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015 * 107.8645906/100 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
income_elasticity_mort <- 0.4
discount_rate <- 0.03

#6 Function to grow WTP
future_WTP <- function(elasticity, growth_rate, WTP){
  return(elasticity * growth_rate * WTP + WTP) 
}

#7 calculate change in pm2.5 and mortality impacts by scenarios

for (i in 1:length(scenarios_to_process)) {
  
  scen_file_name <- scenarios_to_process[i]
  
  ct_scen_out <- readRDS(paste0(ct_save_path, scen_file_name, "_ct_results.rds"))
  setDT(ct_scen_out)
  
  ## calculate delta pm 2.5
  delta_extraction <- merge(ct_scen_out, bau_out,
                            by = c("oil_price_scenario", "setback_existing", "census_tract", "year"),
                            all.x = T)
  
  delta_extraction[, delta_total_pm25 := total_pm25 - bau_total_pm25]
  
  
  ## Merge demographic data to pollution scenarios
  ct_incidence <- delta_extraction %>%
    # left_join(ces3, by = c("census_tract")) %>%
    right_join(ct_inc_pop_45_weighted, by = c("census_tract" = "ct_id", "year" = "year")) %>%
    # remove census tracts that are water area only tracts (no population)
    drop_na(scen_id) %>% 
    as.data.table()
  
  ## Calculate mortality impact ######################################
  
  #Mortality impact fold adults (>=29 years old)
  ct_incidence <- ct_incidence[, mortality_delta := ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop]
  ct_incidence <- ct_incidence[, mortality_level := ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop]
  
  ## Monetizing mortality ############################################
  
  ## Calculate the cost per premature mortality
  ct_incidence <- ct_incidence %>%
    mutate(VSL_2019 = VSL_2019)%>%
    left_join(growth_rates, by = c("year" = "year"))%>%
    mutate(VSL = future_WTP(income_elasticity_mort, 
                            (cum_growth-1),
                            VSL_2019),
           cost_2019 = mortality_delta * VSL_2019,
           cost = mortality_delta * VSL)%>%
    group_by(year) %>%
    mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
           cost_PV = cost/((1+discount_rate)^(year-2019))) %>%
    ungroup()
  
  ## final census tract level health outputs
  ct_incidence <- ct_incidence %>%
    select(scen_id, census_tract, CES3_score, disadvantaged, median_hh_income, year, weighted_incidence, pop, total_pm25, bau_total_pm25, delta_total_pm25, 
           mortality_delta, mortality_level, cost_2019, cost, cost_2019_PV, cost_PV) %>%
    as.data.table()
  
  ## resave the census tract data
  saveRDS(ct_incidence, paste0(ct_save_path, scen_file_name, "_ct_results.rds"))
  
}
