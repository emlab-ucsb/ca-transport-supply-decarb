## Tracey Mangin
## September 16, 2021
## Figure 2

## extraction and refining, production, and ghg emissions
## state outputs

## libraries
library(data.table)
library(tidyverse)

## paths
main_path           = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
academic_path       = paste0(main_path, 'outputs/academic-out/extraction/')
academic_refin_path = paste0(main_path, 'outputs/academic-out/refining/refining_2021-09-21/')
output_folder       = 'extraction_2021-09-21'
academic_out_path   = paste0(academic_path, output_folder, "/")
model_outputs_path  = 'outputs/predict-production/refining_2021-09-06/CUF0.6/outputs'

# source figure themes
source(here::here('energy', 'figures-and-results', 'academic-paper', 'figure_themes.R'))

## files
scen_file     <- "scenario_id_list.csv"
prod_file     <- "well_prod_m_processed.csv"
ghg_file      <- "indust_emissions_2000-2019.csv"
refining_file <- "refining_scenario_outputs_refinery_net_exports_revised.csv"
ghg_factors_file   <- "ghg_emissions_x_field_historic.csv"

## well prod
well_prod <- fread(paste0(main_path, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))
## hist ghg
hist_ghg <- fread(paste0(main_path, "data/stocks-flows/processed/", ghg_file))

## hist ghg emissions factors
hist_factors <- fread(paste0(main_path, 'outputs/stocks-flows/', ghg_factors_file))

## refining outputs
refining_out <- fread(file.path(main_path, model_outputs_path, refining_file))

## scenarios
scen_list <- fread(file.path(academic_path, scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, scen_id]

## read in the extraction outputs
## ---------------------------------

state_diff_list <- list()
scen_out_list <- list()

## main scenarios
main_scens <- c('reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax',
                'reference case_setback_5280ft_no quota_price floor_medium CCS cost_low innovation_no tax',
                'reference case_setback_2500ft_no quota_price floor_medium CCS cost_low innovation_no tax',
                'reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_setback_5280ft',
                'reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_setback_2500ft',
                'reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_90_perc_reduction',
                'reference case_no_setback_no quota_carbon_90_perc_reduction_medium CCS cost_low innovation_no tax',
                'reference case_no_setback_no quota_carbon_setback_5280ft_medium CCS cost_low innovation_no tax',
                'reference case_no_setback_no quota_central SCC_medium CCS cost_low innovation_no tax')



for (i in 1:length(subset_ids)) {
  
  id_name_tmp <- subset_ids[i]
  
  state_out_tmp <- readRDS(paste0(academic_out_path, 'state-results/', id_name_tmp, '_state_results.rds'))
  
  diff_tmp <- state_out_tmp[year == 2019 | year == 2045, .(scen_id, oil_price_scenario, innovation_scenario,
                                                                carbon_price_scenario, ccs_scenario, setback_scenario,
                                                                prod_quota_scenario, excise_tax_scenario, year, total_state_bbl, total_state_ghg_kgCO2)]
  
  diff_tmp[, year := paste0("X", year)]
  
  prod_diff_tmp <- dcast(diff_tmp, scen_id + oil_price_scenario + innovation_scenario +
                         carbon_price_scenario + ccs_scenario + setback_scenario +
                         prod_quota_scenario + excise_tax_scenario ~ year, value.var = "total_state_bbl")
  
  
  prod_diff_tmp[, diff := X2045 - X2019]
  prod_diff_tmp[, indicator := "production"]
  
  ## repeat, ghg
  ghg_diff_tmp <- dcast(diff_tmp, scen_id + oil_price_scenario + innovation_scenario +
                           carbon_price_scenario + ccs_scenario + setback_scenario +
                           prod_quota_scenario + excise_tax_scenario ~ year, value.var = "total_state_ghg_kgCO2")
  
  
  ghg_diff_tmp[, diff := X2045 - X2019]
  ghg_diff_tmp[, indicator := "ghg_emissions_kgCO2"]
  
  ## rbind
  diff_df_tmp <- rbind(prod_diff_tmp, ghg_diff_tmp)
  
  
  state_diff_list[[i]] <- diff_df_tmp
  
  if(isTRUE(id_name_tmp %in% main_scens)){
    
    scen_out_list[[i]] <- state_out_tmp
    
  }
  
}

state_diff_vals <- rbindlist(state_diff_list)
scenario_prod <- rbindlist(scen_out_list)



## 2A: production over time (with historic)
hist_prod <- well_prod[doc_field_code != "000", .(total_state_bbl = sum(OilorCondensateProduced, na.rm = T)), by = .(year)]
hist_prod[, scen_name := "Historic"]

est_hist_ghg <- well_prod[doc_field_code != "000", .(total_state_bbl = sum(OilorCondensateProduced, na.rm = T)), by = .(year, doc_field_code, doc_fieldname)]
est_hist_ghg[, scen_name := "Historic (estimated)"]

est_hist_ghg <- merge(est_hist_ghg, hist_factors,
                      by = c("year", "doc_fieldname"),
                      all.x = T)

est_hist_ghg <- est_hist_ghg[total_state_bbl > 0]


hist_ghg <- hist_ghg[segment == "Oil & Gas: Production & Processing", .(year, value)]
hist_ghg[, scen_name := "Historic"]
setnames(hist_ghg, "value", "total_state_ghg_MtCO2e")

hist_df <- merge(hist_prod, hist_ghg,
                 by = c("year", "scen_name"),
                 all.x = T)

setcolorder(hist_df, c("scen_name", "year", "total_state_bbl", "total_state_ghg_MtCO2e"))

scenario_prod[, scen_name := fifelse(setback_scenario == "no_setback" & excise_tax_scenario == "no tax" & carbon_price_scenario == "price floor", "BAU",
                                     fifelse(setback_scenario == "setback_5280ft", "1 mile setback",
                                             fifelse(excise_tax_scenario == "tax_setback_5280ft", "Excise tax: 1 mile setback", 
                                                     fifelse(excise_tax_scenario == "tax_90_perc_reduction", "Excise tax: 90% emissions reduction",
                                                             fifelse(excise_tax_scenario == "tax_setback_2500ft", "Excise tax: 2500ft setback",
                                                                     fifelse(carbon_price_scenario == "carbon_setback_5280ft", "Carbon tax: 1 mile setback", 
                                                                             fifelse(setback_scenario == "setback_2500ft", "2500ft mile setback", 
                                                                                     fifelse(carbon_price_scenario == "central SCC", "Carbon tax: 2500ft setback", "Carbon tax: 90% emissions reduction"))))))))]

scenario_prod <- scenario_prod[, .(year, total_state_bbl, total_state_ghg_kgCO2, scen_name)]
scenario_prod[, total_state_ghg_MtCO2e := total_state_ghg_kgCO2 / (1000 * 1e6)]
scenario_prod[, total_state_ghg_kgCO2 := NULL]

setcolorder(scenario_prod, c("scen_name", "year", "total_state_bbl", "total_state_ghg_MtCO2e"))

## rbind
annual_df <- rbind(hist_df, scenario_prod)
annual_df$scen_name <- factor(annual_df$scen_name, levels = c("Historic", 
                                                              "BAU",
                                                              "2500ft mile setback",
                                                              "1 mile setback", 
                                                              "Excise tax: 2500ft setback",
                                                              "Excise tax: 1 mile setback",
                                                              "Excise tax: 90% emissions reduction",
                                                              "Carbon tax: 2500ft setback",
                                                              "Carbon tax: 1 mile setback",
                                                              "Carbon tax: 90% emissions reduction"))

scenario_colors <- c("Historic" = "grey", 
                     "BAU" = "black",
                     "2500ft mile setback" = "#900C3F",
                     "1 mile setback" = "#581845", 
                     "Excise tax: 2500ft setback" = "#2A7B9B",
                     "Excise tax: 1 mile setback" = "#00BAAD",
                     "Excise tax: 90% emissions reduction" = "#3D3D6B",
                     "Carbon tax: 2500ft setback" = "#FFC200", 
                     "Carbon tax: 1 mile setback" = "#FF8D18",
                     "Carbon tax: 90% emissions reduction" = "#FF5733")


## figure 2a
fig2a <- ggplot(annual_df, aes(x = year, y = total_state_bbl / 1e6, color = scen_name)) +
  geom_line(alpha = 0.8) +
  labs(x = NULL,
       y = "Production (million bbls)",
       color = NULL) +
  theme_line +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  scale_color_manual(values = scenario_colors) +
  geom_vline(xintercept = 2019, color = "black", lty = "dashed") +
  guides(color = guide_legend(nrow = 3, byrow = FALSE))

## state diff vals
state_diff_vals[, scen_name := fifelse(setback_scenario == "no_setback" & excise_tax_scenario == "no tax" & carbon_price_scenario == "price floor", "BAU",
                                       fifelse(setback_scenario == "setback_5280ft", "1 mile setback",
                                               fifelse(excise_tax_scenario == "tax_setback_5280ft", "Excise tax: 1 mile setback", 
                                                       fifelse(excise_tax_scenario == "tax_90_perc_reduction", "Excise tax: 90% emissions reduction",
                                                               fifelse(excise_tax_scenario == "tax_setback_2500ft", "Excise tax: 2500ft setback",
                                                                       fifelse(carbon_price_scenario == "carbon_setback_5280ft", "Carbon tax: 1 mile setback", 
                                                                               fifelse(setback_scenario == "setback_2500ft", "2500ft mile setback", 
                                                                                       fifelse(carbon_price_scenario == "central SCC", "Carbon tax: 2500ft setback", "Carbon tax: 90% emissions reduction"))))))))]


state_diff_vals$scen_name <- factor(state_diff_vals$scen_name, levels = c("Historic", 
                                                              "BAU",
                                                              "2500ft mile setback",
                                                              "1 mile setback", 
                                                              "Excise tax: 2500ft setback",
                                                              "Excise tax: 1 mile setback",
                                                              "Excise tax: 90% emissions reduction",
                                                              "Carbon tax: 2500ft setback",
                                                              "Carbon tax: 1 mile setback",
                                                              "Carbon tax: 90% emissions reduction"))


## figure
## delta BBLs (difference between 2019 and 2045) vs BAU, P1, P2. 
## Each point is delta production for all our scenarios. Color based on oil price, shape based on carbon price. 

fig2b <- ggplot(state_diff_vals %>% filter(indicator == "production",
                                           carbon_price_scenario %in% c("carbon_setback_5280ft", "carbon_90_perc_reduction", "price floor", "central SCC")), aes(x = oil_price_scenario, y = diff / 1e6, color = scen_name, shape = ccs_scenario)) +
  geom_jitter() +
  labs(x = NULL,
       y = "Difference in production: 2045 vs 2019 (million bbls)",
       color = NULL) +
  scale_color_manual(values = scenario_colors) +
  geom_hline(yintercept = 0, color = "black", lty = "dashed") 
  # theme_line +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  # scale_color_manual(values = c("Historic" = "grey",
  #                               "BAU" = "#FFBF00",
  #                               "1 mile setback" = "#FF7F50",
  #                               "Excise tax" = "#DE3163",
  #                               "Excise tax: 90% emissions reduction" = "navy")) 
  



## 2c
fig2c <- ggplot(annual_df, aes(x = year, y = total_state_ghg_MtCO2e, color = scen_name)) +
  geom_line() +
  labs(x = NULL,
       color = NULL) +
  ylab(bquote(GHG~emissions~(MtCO[2]~e))) +
  theme_line +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  scale_color_manual(values = scenario_colors) +
  geom_vline(xintercept = 2019, color = "black", lty = "dashed")  +
  guides(color = guide_legend(nrow = 3, byrow = FALSE))

# fig2d <- ggplot(state_diff_vals %>% filter(indicator == "production"), aes(y = diff / 1e6, color = oil_price_scenario, shape = carbon_price_scenario)) +
#   geom_jitter() +
#   labs(x = NULL,
#        y = "Production (million bbls)",
#        color = NULL) +
#   theme_line +
#   scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
#   scale_color_manual(values = c("Historic" = "grey",
#                                 "BAU" = "#FFBF00",
#                                 "1 mile setback" = "#FF7F50",
#                                 "Excise tax" = "#DE3163",
#                                 "Excise tax: 90% emissions reduction" = "navy")) +
#   geom_vline(xintercept = 2019, color = "black", lty = "dashed")


## refining

