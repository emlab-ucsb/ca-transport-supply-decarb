plot_diagnostic_outputs <- function(oil_price_selection, output_extraction) {
  
  print('Now plotting outputs...')
  
  # baseline comparison output path
  # 
  base_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  # 
  ## cal epa report outputs
  
  report_out <- read.csv("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/production_state_1977-2045.csv")
  report_wells_out <- fread("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/well_entry_state_1977-2045.csv")
  extract_field_out <- fread('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/download/field_level_prod_emissions_2020-2045.csv', header = T,
                             colClasses = ('doc_field_code' = 'character'))
  hist_ghg <- fread('/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/historic_ghg_emissions_og_ng_adjusted.csv')
  
  ## assemble outputs from cal epa report
  ## -------------------------------------------
  
 
  
  ## bau
  report_out_bau <- report_out %>%
    filter(oil_price_scenario == "iea oil price",
           carbon_price_scenario == "price floor",
           innovation_scenario == "low innovation",
           ccs_scenario == "medium CCS cost",
           excise_tax_scenario == "no tax",
           prod_quota_scenario == "no quota",
           setback_scenario %in% c("no_setback")) %>%
    mutate(scenario_name = "BAU",
           scenario = "BAU") %>%
    select(scenario_name, scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario,
           prod_quota_scenario, excise_tax_scenario, year, production_bbl, type) %>%
    arrange(scenario) %>%
    mutate(type = ifelse(type == "future production from new wells", "new_prod_bbl",
                         ifelse(type == "future production from existing wells", "existing_prod_bbl", "historic_prod"))) %>%
    pivot_wider(names_from = type, values_from = production_bbl) %>%
    mutate(existing_prod_bbl = ifelse(year < 2020, historic_prod, existing_prod_bbl)) %>%
    select(scenario:existing_prod_bbl) %>%
    rename(scenario_name = scenario) %>%
    rowwise() %>%
    mutate(total_prod_bbl = sum(new_prod_bbl, existing_prod_bbl, na.rm = T)) %>%
    ungroup() %>%
    arrange(year)
    
  
  ## scenarios
  report_out2 <- report_out %>%
    filter(oil_price_scenario == "iea oil price",
           carbon_price_scenario == "price floor",
           innovation_scenario == "low innovation",
           ccs_scenario == "medium CCS cost",
           excise_tax_scenario == "no tax",
           prod_quota_scenario == "quota_20",
           setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
    mutate(scenario = ifelse(setback_scenario == "no_setback", "LCE1", "LCE2")) %>%
    select(scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario,
           prod_quota_scenario, excise_tax_scenario, year, production_bbl, type) %>%
    arrange(scenario) %>%
    mutate(type = ifelse(type == "future production from new wells", "new_prod_bbl",
                         ifelse(type == "future production from existing wells", "existing_prod_bbl", "historic_prod"))) %>%
    pivot_wider(names_from = type, values_from = production_bbl) %>%
    mutate(existing_prod_bbl = ifelse(year < 2020, historic_prod, existing_prod_bbl)) %>%
    select(scenario:existing_prod_bbl) %>%
    rename(scenario_name = scenario) %>%
    rowwise() %>%
    mutate(total_prod_bbl = sum(new_prod_bbl, existing_prod_bbl, na.rm = T)) %>%
    ungroup() %>%
    arrange(scenario_name, year)
  
  ## all future
  report_out_prod <- rbind(report_out_bau, report_out2) %>%
    pivot_longer(new_prod_bbl:total_prod_bbl, names_to = "type", values_to = "production_bbls") %>%
    mutate(type = ifelse(type == "existing_prod_bbl", "existing",
                         ifelse(type == "total_prod_bbl", "total", "new"))) %>%
    mutate(version = ifelse(year <= 2019, "historic", "calepa-report")) %>%
    select(version, scenario_name:production_bbls) %>%
    rename(scen_name = scenario_name)
  
  
  ## filter scenarios - new wells
  report_wells_out2 <- report_wells_out %>%
    filter(oil_price_scenario == "iea oil price",
           carbon_price_scenario == "price floor",
           innovation_scenario == "low innovation",
           ccs_scenario == "medium CCS cost",
           excise_tax_scenario == "no tax",
           prod_quota_scenario == "quota_20",
           setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
    mutate(scen_name = ifelse(setback_scenario == "no_setback", "LCE1", "LCE2")) %>%
    select(scen_name, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario,
           prod_quota_scenario, excise_tax_scenario, year, new_wells_pred) %>%
    arrange(scen_name, year) %>%
    filter(year != 1977)
  
  report_wells_out_all <- report_wells_out %>%
    filter(oil_price_scenario == "iea oil price",
           carbon_price_scenario == "price floor",
           innovation_scenario == "low innovation",
           ccs_scenario == "medium CCS cost",
           excise_tax_scenario == "no tax",
           prod_quota_scenario == "no quota",
           setback_scenario %in% c("no_setback"),
           year != 1977) %>%
    mutate(scen_name = "BAU") %>%
    select(scen_name, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario,
           prod_quota_scenario, excise_tax_scenario, year, new_wells_pred) %>%
    rbind(report_wells_out2) %>%
    rename(new_wells = new_wells_pred) %>%
    arrange(scen_name, year)

  ## ghg emeissions
  ## ---------------------
  
  ## summarise at state level
  ghg_state <- extract_field_out %>%
    group_by(year, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
             setback_scenario, prod_quota_scenario, excise_tax_scenario, well_type) %>%
    mutate(upstream_kgCO2e = as.numeric(upstream_kgCO2e)) %>%
    summarise(upstream_kgCO2e = sum(upstream_kgCO2e,  na.rm = T)) %>%
    ungroup() %>%
    pivot_wider(names_from = well_type, values_from = upstream_kgCO2e) %>%
    rowwise() %>%
    mutate(total = sum(existing, new, na.rm = T)) %>%
    ungroup() %>%
    pivot_longer(existing:total, names_to = "type", values_to = "upstream_kgCO2e")
  
  
  ## bau
  report_ghg_bau <- ghg_state %>%
    filter(oil_price_scenario == "iea oil price",
           carbon_price_scenario == "price floor",
           innovation_scenario == "low innovation",
           ccs_scenario == "medium CCS cost",
           excise_tax_scenario == "no tax",
           prod_quota_scenario == "no quota",
           setback_scenario %in% c("no_setback")) %>%
    mutate(scen_name = "BAU") %>%
    select(scen_name, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario,
           prod_quota_scenario, excise_tax_scenario, year, type, upstream_kgCO2e) %>%
    arrange(scen_name, year) 
  
  ## scenarios
  report_ghg_all <- ghg_state %>%
    filter(oil_price_scenario == "iea oil price",
           carbon_price_scenario == "price floor",
           innovation_scenario == "low innovation",
           ccs_scenario == "medium CCS cost",
           excise_tax_scenario == "no tax",
           prod_quota_scenario == "quota_20",
           setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
    mutate(scen_name = ifelse(setback_scenario == "no_setback", "LCE1", "LCE2")) %>%
    select(scen_name, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario,
           prod_quota_scenario, excise_tax_scenario, year, type, upstream_kgCO2e) %>%
    arrange(scen_name, year) %>%
    rbind(report_ghg_bau) %>%
    mutate(version = "calepa-report") %>%
    rename(ghg_kgCO2e = upstream_kgCO2e)
  

  # create objects for list items -------
  
  state_out = output_extraction[[3]]

  state_out[, version := paste0("adj-", run_type)]
  
  field_out = output_extraction[[2]]
  
  pos_field_dt = field_out[, .(prod = sum(total_prod_bbl, na.rm = T)), by = .(doc_field_code, oil_price_scenario,
                                                                              innovation_scenario, carbon_price_scenario,
                                                                              ccs_scenario, setback_scenario, prod_quota_scenario,
                                                                              excise_tax_scenario)]

  density_out = output_extraction[[4]]
  
  density_out = merge(density_out, pos_field_dt,
                      by = c('doc_field_code', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'),
                      all = T)
  
  density_out[, prod := fifelse(is.na(prod), 0, prod)]
  density_out = density_out[prod > 0]
  
  
  # read in baseline for comparison ------
  
  state_base = fread(file.path(base_path, 'extraction_2021-07-15', 'revised-edit-exit-model', 'diagnostic-state-level-results.csv'), header = T)
  
  # add column for base case
  state_base[, version := "exit-first"]
  
  # bind data
  state_all <- rbind(state_base, state_out)
  
  state_all[, total_ghg_mtCO2e := NULL]
  
  ## scenario lut
  scen_lut <- unique(state_all[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                   "setback_scenario", "prod_quota_scenario", "excise_tax_scenario")])
  
  scen_lut[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                  fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]
  
  state_all <- merge(state_all, scen_lut)
  
  setcolorder(state_all, 
              c("version", "scen_name", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", "new_wells", 
                "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl", "existing_ghg_kgCO2e", "new_ghg_kgCO2e", 
                "total_ghg_kgCO2e"))
  
  ## melt production
  state_all_prod <- state_all[, c("version", "scen_name", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                  "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", 
                                  "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl")]
  
  state_all_prod <- melt(state_all_prod,
                         measure.vars = c("existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"),
                         variable.name = "type",
                         value.name = "production_bbls")
  
  state_all_prod[, type := str_extract(type, "[^_]+")]
  
  
  ## melt ghg
  state_all_ghg <- state_all[, c("version", "scen_name", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                   "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", "new_wells", 
                                   "existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e")]
  
  state_all_ghg <- melt(state_all_ghg,
                         measure.vars = c("existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e"),
                         variable.name = "type",
                         value.name = "ghg_kgCO2e")
  
  state_all_ghg[, type := str_extract(type, "[^_]+")]
  
  ## merge
  state_all_long <- merge(state_all_prod, state_all_ghg)
  
  ## density outputs
  ## ----------------------------------------
  
  density_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                     fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]
  
  
  # plot theme --------------------
  
  theme_line =  
    theme_ipsum(base_family = 'Arial',
                                         grid = 'Y',
                                         plot_title_size = 16,
                                         subtitle_size = 14,
                                         axis_title_just = 'center',
                                         axis_title_size = 16,
                                         axis_text_size = 16,
                                         strip_text_size = 16)  +
    theme(plot.title = element_text(hjust = 0, face = 'bold'),
          plot.title.position = 'plot',
          plot.subtitle = element_text(hjust = 0),
          plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
          axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
          axis.line.x = element_line(color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          axis.ticks.length.x = unit(0.25, "cm"),
          axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
          plot.margin = unit(c(1,2,1,1), "lines"),
          strip.text = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.position = 'bottom',
          )
  
 
   # extraction (state, old, new)
  extraction_fig = ggplot(state_all_long, 
                                  aes(x = year, y = production_bbls / 1e6, color = version)) + 
    geom_line(alpha = 0.7, size = 1) +
    facet_grid(type ~ scen_name) +
    geom_point(data = state_all_long %>% filter(version == paste0("adj-", run_type), year %in% seq(2020, 2045, by = 5)),
              aes(x = year, y = production_bbls / 1e6, color = version), shape = 3) +
    geom_line(data = report_out_prod, aes(x = year, y = production_bbls / 1e6, color = version), alpha = 0.7, size = 1) +
    facet_grid(type ~ scen_name) +
    geom_vline(xintercept = 2019, color = "darkgrey", size = 0.3, lty = "dashed") +
    scale_color_manual(values = c("#FF6E1B", "#FFD200", "#005581", "black")) +
    labs(title = 'State-level crude oil extraction',
         subtitle = 'million barrels', 
         x = 'Year',
         y = NULL,
         color = '') +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  
  # ghg (state, old, new)
  ghg_fig = ggplot(state_all_long, 
                          aes(x = year, y = ghg_kgCO2e / 1e9, color = version)) + 
    geom_line(alpha = 0.7, size = 1) +
    geom_point(data = state_all_long %>% filter(version == paste0("adj-", run_type), year %in% seq(2020, 2045, by = 5)),
               aes(x = year, y = ghg_kgCO2e / 1e9, color = version), shape = 3) +
    geom_line(data = report_ghg_all, aes(x = as.integer(year), y = ghg_kgCO2e / 1e9, color = version), alpha = 0.7, size = 1) +
    facet_grid(type ~ scen_name) +
    geom_line(data = hist_ghg %>% filter(emissions_type == "mtco2e"), aes(x = year, y = mtco2e), color = "black", alpha = 0.7, size = 1) +
    geom_line(data = hist_ghg %>% filter(emissions_type == "adj_mtco2e"), aes(x = year, y = mtco2e), color = "black", lty = "dotted", alpha = 0.7, size = 1) +
    geom_vline(xintercept = 2019, color = "darkgrey", size = 0.3, lty = "dashed") +
    scale_color_manual(values = c("#FF6E1B", "#FFD200", "#005581")) +
    labs(title = 'State-level emissions',
         subtitle = 'MtCO2e', 
         x = 'Year',
         y = NULL,
         color = '') +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  
  # new wells
  new_wells_fig = ggplot(state_all_long %>% filter(type == "new"), 
                   aes(x = year, y = new_wells, color = version)) + 
    geom_line(alpha = 0.7, size = 1) +
    facet_grid(~ scen_name) +
    geom_point(data = state_all_long %>% filter(version == paste0("adj-", run_type), year %in% seq(2020, 2045, by = 5)),
               aes(x = year, y = new_wells, color = version), shape = 3) +
    geom_line(data = report_wells_out_all %>% mutate(version = ifelse(year <= 2019, "historic", "calepa-report")), aes(x = year, y = new_wells, color = version), alpha = 0.7, size = 1) +
    geom_vline(xintercept = 2019, color = "darkgrey", size = 0.3, lty = "dashed") +
    scale_color_manual(values = c("#FF6E1B", "#FFD200", "#005581", "black")) +
    labs(title = 'State-level new wells',
         subtitle = 'number of new wells', 
         x = 'Year',
         y = NULL,
         color = '') +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  library(scales)
  
  # density fig
  density_fig = ggplot(density_out, 
                         aes(x = year, y = wells_km2, group = doc_field_code)) + 
    geom_line(alpha = 0.7, size = 1) +
    facet_wrap(~ scen_name, scales = "free_y") +
    scale_y_continuous(labels = comma) +
    labs(title = 'Density',
         subtitle = 'wells per km2', 
         x = 'Year',
         y = NULL,
         color = '') +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  
  
  
  # save figures  -----
  
  # save info file
  save_info_path = file.path(save_path, run_type, "diagnostic-figs")
  dir.create(save_info_path, showWarnings = FALSE)
  print(paste0("Saving diagnostic figures to ", save_info_path))
  
  # save figures ----
  extraction_fname = paste0(oil_price_selection, '_extraction_fig.pdf')
  ggsave(extraction_fig, 
         filename = file.path(save_info_path, extraction_fname), 
         width = 23, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, extraction_fname),
              outfile = file.path(save_info_path, extraction_fname))
  print(paste0('Saved diagnostic figures to ', extraction_fname))
 
  
  ghg_fname = paste0(oil_price_selection, '_ghg_fig.pdf')
  ggsave(ghg_fig, 
         filename = file.path(save_info_path, ghg_fname), 
         width = 23, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, ghg_fname),
              outfile = file.path(save_info_path, ghg_fname))
  print(paste0('Saved diagnostic figures to ', ghg_fname))
  
  
  new_wells_fname = paste0(oil_price_selection, '_new_wells_fig.pdf')
  ggsave(new_wells_fig, 
         filename = file.path(save_info_path, new_wells_fname), 
         width = 23, 
         height = 7)
  
  embed_fonts(file.path(save_info_path, new_wells_fname),
              outfile = file.path(save_info_path, new_wells_fname))
  print(paste0('Saved diagnostic figures to ', new_wells_fname))
  
  density_fname = paste0(oil_price_selection, '_density_fig.pdf')
  ggsave(density_fig, 
         filename = file.path(save_info_path, density_fname), 
         width = 23, 
         height = 7)
  
  embed_fonts(file.path(save_info_path, density_fname),
              outfile = file.path(save_info_path, density_fname))
  print(paste0('Saved diagnostic figures to ', density_fname))
  
  
} 