benchmark_outputs <- function(oil_price_selection, output_extraction) {
  
  print('Now creating benchmarks...')
  
  ## proj file
  proj_dir <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
  
  
  # create objects for list items -------
  
  state_out = output_extraction[[3]]
  
  field_out = output_extraction[[2]]

  
  ## read in files
  extract_field_out <- fread('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/download/field_level_prod_emissions_2020-2045.csv', header = T,
                             colClasses = ('doc_field_code' = 'character'))

  report_wells_out <- fread("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/well_entry_state_1977-2045.csv")
  
  ## files
  prod_file    <- "well_prod_m_processed.csv"
  
  # transform to NAD83(NSRS2007) / California Albers
  # units will be in meters
  ca_crs <- 3488
  
  ## ggplot plot, production
  states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
  
  california <- states %>% filter(ID == "california") %>%
    st_transform(ca_crs)
  
  ## counties boundaries
  county_boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% st_transform(ca_crs) %>%
    dplyr::select(adj_county_name = NAME)
  
  ## monthly well production
  well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
  
  
  ## county
  county_lut <- well_prod %>%
    dplyr::select(doc_field_code, county_name) %>%
    unique() %>%
    mutate(adj_county_name = str_remove(county_name, " Offshore"))
  
  
  ## county prod all years
  prod_x_county <- well_prod %>%
    left_join(county_lut) %>%
    group_by(doc_field_code, doc_fieldname, adj_county_name) %>%
    summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
    ungroup() %>%
    group_by(doc_field_code) %>%
    mutate(field_total = sum(prod, na.rm = T)) %>%
    ungroup() %>%
    mutate(rel_prod = prod / field_total) %>%
    dplyr::select(doc_field_code, adj_county_name, rel_prod)
  
  ## county prod for 2019
  county_prod_2019 <- well_prod %>%
    filter(year == 2019) %>%
    group_by(doc_field_code, year) %>%
    summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
    ungroup() %>%
    left_join(prod_x_county) %>%
    mutate(county_prod = prod * rel_prod) %>%
    group_by(year, adj_county_name) %>%
    summarise(county_prod = sum(county_prod, na.rm = T)) %>%
    ungroup() 
  
  sum(county_prod_2019$county_prod)
  
  
  ## all scenario combinations
  scen_combos <- unique(state_out[, c('oil_price_scenario', 'innovation_scenario', "carbon_price_scenario", "ccs_scenario", 
                                      "setback_scenario", "prod_quota_scenario", "excise_tax_scenario")])
  
  
  ## melt production
  state_out_prod <- state_out[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                              "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", 
                              "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl")]
  
  state_out_prod <- melt(state_out_prod,
                         measure.vars = c("existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"),
                         variable.name = "type",
                         value.name = "production_bbls")
  
  state_out_prod[, type := str_extract(type, "[^_]+")]
  
  ## melt ghg
  state_out_ghg <- state_out[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                  "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", 
                                 "new_wells", "existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e")]
  
  state_out_ghg <- melt(state_out_ghg,
                         measure.vars = c("existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e"),
                         variable.name = "type",
                         value.name = "ghg_kgCO2e")
  
  state_out_ghg[, type := str_extract(type, "[^_]+")]
  
  ## merge
  state_all <- merge(state_out_prod, state_out_ghg)
  
  ## MtCO2e
  state_all <- state_all[, ghg_MtCO2e := ghg_kgCO2e / 1e9]
  
  ## avg emissions
  state_all <- state_all[, mean_emis_factor := ghg_kgCO2e / production_bbls]
  
  ## scenarios
  oil_px_scens <- scen_combos[(innovation_scenario == 'low innovation' & 
                            carbon_price_scenario == 'price floor' & 
                            ccs_scenario == 'medium CCS cost' &
                            excise_tax_scenario == 'no tax' &
                            setback_scenario == 'no_setback' &
                            prod_quota_scenario == 'no quota')] 
  oil_px_scens[, scenario := 'oil price scenarios']
  
  innovation_scens <- scen_combos[(oil_price_scenario == 'reference case' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota')]
  innovation_scens[, scenario := 'innovation scenarios']
  
  carbon_px_scens <- scen_combos[(oil_price_scenario == 'reference case' & 
                                     innovation_scenario == 'low innovation' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota')]
  carbon_px_scens[, scenario := 'carbon price scenarios']
  
  ccs_cost_scens <- scen_combos[(oil_price_scenario == 'reference case' & 
                                    innovation_scenario == 'low innovation' & 
                                    carbon_price_scenario == 'central SCC' & 
                                    excise_tax_scenario == 'no tax' &
                                    setback_scenario == 'no_setback' &
                                    prod_quota_scenario == 'no quota')]
  ccs_cost_scens[, scenario := 'CCS cost scenarios']
  
  tax_scens <- scen_combos[(oil_price_scenario == 'reference case' & 
                                 innovation_scenario == 'low innovation' & 
                                 carbon_price_scenario == 'price floor' & 
                                 ccs_scenario == 'medium CCS cost' & 
                                 setback_scenario == 'no_setback' &
                                 prod_quota_scenario == 'no quota')]
  tax_scens[, scenario := 'tax scenarios']
  
  setback_scens <- scen_combos[(oil_price_scenario == 'reference case' & 
                              innovation_scenario == 'low innovation' & 
                              carbon_price_scenario == 'price floor' & 
                              ccs_scenario == 'medium CCS cost' & 
                              excise_tax_scenario == 'no tax' & 
                              prod_quota_scenario == 'no quota')]
  setback_scens[, scenario := 'setback scenarios']
  
  quota_scens <- scen_combos[(oil_price_scenario == 'reference case' & 
                                  innovation_scenario == 'low innovation' & 
                                  carbon_price_scenario == 'price floor' & 
                                  ccs_scenario == 'medium CCS cost' & 
                                  excise_tax_scenario == 'no tax' & 
                                  setback_scenario == 'no_setback')]
  quota_scens[, scenario := 'quota scenarios']
  
  ## all scenarios
  scenarios_df <- rbind(oil_px_scens, innovation_scens, carbon_px_scens, ccs_cost_scens, setback_scens, quota_scens, tax_scens) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario) %>%
    left_join(state_all) %>%
    mutate(oil_price_scenario = as.character(oil_price_scenario),
           ccs_scenario = as.character(ccs_scenario)) %>%
    mutate(option = ifelse(scenario == 'oil price scenarios', oil_price_scenario,
                           ifelse(scenario == 'innovation scenarios', innovation_scenario,
                                  ifelse(scenario == 'carbon price scenarios', carbon_price_scenario,
                                         ifelse(scenario == 'CCS cost scenarios', ccs_scenario,
                                                ifelse(scenario == 'setback scenarios', setback_scenario,
                                                       ifelse(scenario == 'quota scenarios', prod_quota_scenario, excise_tax_scenario))))))) %>%
    mutate(version = "revised")

## repeat for calepa outputs
## --------------------------------
  
  
  
  ## all scenarios, cal epa
  report_wells_df <- rbind(oil_px_scens, innovation_scens, carbon_px_scens, ccs_cost_scens, setback_scens, quota_scens, tax_scens) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario) %>%
    left_join(report_wells_out) %>%
    filter(year >= 2020) %>%
    mutate(oil_price_scenario = as.character(oil_price_scenario),
           ccs_scenario = as.character(ccs_scenario),
           type = 'total') %>%
    rename(new_wells = new_wells_pred) %>%
    select(-oil_price_usd_per_bbl)
  
  report_df <- rbind(oil_px_scens, innovation_scens, carbon_px_scens, ccs_cost_scens, setback_scens, quota_scens, tax_scens) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario) %>%
    left_join(extract_field_out) %>%
    filter(year >= 2020) %>%
    mutate(oil_price_scenario = as.character(oil_price_scenario),
           ccs_scenario = as.character(ccs_scenario)) %>%
    group_by(scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year, well_type) %>%
    summarise(production_bbl = sum(as.numeric(production_bbl), na.rm = T),
              ghg_kgCO2e = sum(as.numeric(upstream_kgCO2e), na.rm = T)) %>%
    ungroup() 
    
  report_df2 <- report_df %>%
    group_by(scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
    summarise(production_bbl = sum(production_bbl, na.rm = T),
              ghg_kgCO2e = sum(ghg_kgCO2e, na.rm = T)) %>%
    ungroup() %>%
    mutate(well_type = 'total') %>%
    rbind(report_df) %>%
    rename(type = well_type,
           production_bbls = production_bbl) %>%
    mutate(ghg_MtCO2e = ghg_kgCO2e / 1e9,
           mean_emis_factor = ghg_kgCO2e / production_bbls,
           year = as.integer(year)) %>%
    left_join(report_wells_df) %>%
    mutate(option = ifelse(scenario == 'oil price scenarios', oil_price_scenario,
                           ifelse(scenario == 'innovation scenarios', innovation_scenario,
                                  ifelse(scenario == 'carbon price scenarios', carbon_price_scenario,
                                         ifelse(scenario == 'CCS cost scenarios', ccs_scenario,
                                                ifelse(scenario == 'setback scenarios', setback_scenario,
                                                       ifelse(scenario == 'quota scenarios', prod_quota_scenario, excise_tax_scenario))))))) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario, year, type, production_bbls, new_wells, ghg_kgCO2e, ghg_MtCO2e, mean_emis_factor, option) %>%
    mutate(version = "calepa") 
  
  
  ## all
  all_scenarios_df <- rbind(report_df2, scenarios_df)
  
  
  # plot theme --------------------
  
  theme_line =  
    theme_ipsum(base_family = 'Arial',
                grid = 'Y',
                plot_title_size = 12,
                subtitle_size = 11,
                axis_title_just = 'center',
                axis_title_size = 12,
                axis_text_size = 12,
                strip_text_size = 12)  +
    theme(plot.title = element_text(hjust = 0, face = 'bold'),
          plot.title.position = 'plot',
          plot.subtitle = element_text(hjust = 0),
          plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
          # axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), angle = 90),
          axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
          axis.line.x = element_line(color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          axis.ticks.length.x = unit(0.25, "cm"),
          axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
          plot.margin = unit(c(1,2,1,1), "lines"),
          strip.text = element_text(hjust = 0.5),
          legend.text = element_text(size = 11),
          legend.position = 'bottom',
    )
  
  
  # save info file
  save_info_path = file.path(save_path, run_type, "benchmark-figs")
  dir.create(save_info_path, showWarnings = FALSE)
  print(paste0("Creating benchmark figures to save in ", save_info_path))
  
  scen_options <- c("oil price scenarios", "innovation scenarios", "carbon price scenarios", "CCS cost scenarios", 
                    "setback scenarios", "quota scenarios", "tax scenarios")
  
  
  all_scenarios_df$option <- factor(all_scenarios_df$option, levels = c('low oil price', 'reference case', 'high oil price', 
                                                                'low innovation', 'high innovation', 'price floor', 'central SCC', 'price ceiling',
                                                                'low CCS cost', 'medium CCS cost', 'high CCS cost', 'low CCS cost - 45Q', 'medium CCS cost - 45Q', 
                                                                'high CCS cost - 45Q', 'low CCS cost - 45Q - LCFS', 'medium CCS cost - 45Q - LCFS', 'high CCS cost - 45Q - LCFS', 
                                                                'high CCS cost - 45Q - LCFS (constrained)', 'medium CCS cost - 45Q - LCFS (constrained)', 'low CCS cost - 45Q - LCFS (constrained)',
                                                                'no_setback', 'setback_1000ft', 'setback_2500ft', 'setback_5280ft', 'no quota', 'quota_40', 'quota_20', 'quota_10', 'quota_00', 
                                                                'no tax', 'tax_05', 'tax_10', 'tax_50', 'tax_90'))
  

  ## make spatial fig plots
  ## change in production 2019 vs 2045 by county
  pred_prod_county <- rbind(oil_px_scens, innovation_scens, carbon_px_scens, ccs_cost_scens, setback_scens, quota_scens, tax_scens) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario) %>%
    mutate(ccs_scenario = as.character(ccs_scenario),
           oil_price_scenario = as.character(oil_price_scenario)) %>%
    mutate(option = ifelse(scenario == 'oil price scenarios', oil_price_scenario,
                           ifelse(scenario == 'innovation scenarios', innovation_scenario,
                                  ifelse(scenario == 'carbon price scenarios', carbon_price_scenario,
                                         ifelse(scenario == 'CCS cost scenarios', ccs_scenario,
                                                ifelse(scenario == 'setback scenarios', setback_scenario,
                                                       ifelse(scenario == 'quota scenarios', prod_quota_scenario, excise_tax_scenario))))))) %>%
    left_join(field_out) %>%
    filter(year == 2045) %>%
    left_join(prod_x_county) %>%
    mutate(county_prod = total_prod_bbl * rel_prod) %>%
    group_by(scenario, year, option, adj_county_name) %>%
    summarise(county_prod = sum(county_prod, na.rm = T)) %>%
    ungroup()
    
  
  ## all county
  all_county_prod <- rbind(oil_px_scens, innovation_scens, carbon_px_scens, ccs_cost_scens, setback_scens, quota_scens, tax_scens) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario) %>%
    mutate(year = 2019) %>%
    mutate(ccs_scenario = as.character(ccs_scenario),
           oil_price_scenario = as.character(oil_price_scenario)) %>%
    mutate(option = ifelse(scenario == 'oil price scenarios', oil_price_scenario,
                           ifelse(scenario == 'innovation scenarios', innovation_scenario,
                                  ifelse(scenario == 'carbon price scenarios', carbon_price_scenario,
                                         ifelse(scenario == 'CCS cost scenarios', ccs_scenario,
                                                ifelse(scenario == 'setback scenarios', setback_scenario,
                                                       ifelse(scenario == 'quota scenarios', prod_quota_scenario, excise_tax_scenario))))))) %>%
    dplyr::select(scenario, option, year) %>%
    left_join(county_prod_2019) %>%
    rbind(pred_prod_county) %>%
    mutate(year = paste0('x', year)) %>%
    pivot_wider(names_from = year, values_from = county_prod) %>%
    mutate(x2019 = ifelse(is.na(x2019), 0, x2019),
           x2045 = ifelse(is.na(x2045), 0, x2045)) %>%
    mutate(diff_2045_2019 = x2045 - x2019,
           rel_2045_2019 = diff_2045_2019 / x2019) %>%
    filter(!is.na(rel_2045_2019)) 
  
  ## plot
  all_county_prod_df <- all_county_prod %>%
    pivot_longer(diff_2045_2019:rel_2045_2019, names_to = 'metric', values_to = 'values') %>%
    mutate(metric = ifelse(metric == 'diff_2045_2019', 'difference (bbls)', '% difference'),
           adj_val = ifelse(metric == 'difference (bbls)', values / 1e6, values * 100)) %>%
    left_join(county_boundaries) 
  
  all_county_prod_df$option <- factor(all_county_prod_df$option, levels = c('low oil price', 'reference case', 'high oil price', 
                                                                        'low innovation', 'high innovation', 'price floor', 'central SCC', 'price ceiling',
                                                                        'low CCS cost', 'medium CCS cost', 'high CCS cost', 'low CCS cost - 45Q', 'medium CCS cost - 45Q', 
                                                                        'high CCS cost - 45Q', 'low CCS cost - 45Q - LCFS', 'medium CCS cost - 45Q - LCFS', 'high CCS cost - 45Q - LCFS',  
                                                                        'high CCS cost - 45Q - LCFS (constrained)', 'medium CCS cost - 45Q - LCFS (constrained)', 'low CCS cost - 45Q - LCFS (constrained)',
                                                                        'no_setback', 'setback_1000ft',
                                                                        'setback_2500ft', 'setback_5280ft', 'no quota', 'quota_40', 'quota_20', 'quota_10', 'quota_00', 
                                                                        'no tax', 'tax_05', 'tax_10', 'tax_50', 'tax_90'))
  
  
  plot_scen <- function(scen_choice) {
  
    if(scen_choice == 1) {name <- scen_options[1]} 
    else if(scen_choice == 2) {name <- scen_options[2]}
    else if(scen_choice == 3) {name <- scen_options[3]}
    else if(scen_choice == 4) {name <- scen_options[4]}
    else if(scen_choice == 5) {name <- scen_options[5]}
    else if(scen_choice == 6) {name <- scen_options[6]}
    else {name <- scen_options[7]}
    
  
  ### the benchmark figs
  ### ------------------------------------------------------
      
  # new well entry
  new_wells_fig = ggplot(all_scenarios_df %>% filter(scenario == name,
                                                 type == 'total'), 
                         aes(x = year, y = new_wells, color = option, lty = version)) + 
    scale_linetype_manual(values = c('calepa' = 'dotted', 'revised' = 'solid')) +
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'State-level new wells',
         # subtitle = 'number of new wells', 
         x = NULL,
         y = "# new wells",
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line 
  
  # oil production
  oil_prod_fig = ggplot(all_scenarios_df %>% filter(scenario == name,
                                                type == 'total'), 
                         aes(x = year, y = production_bbls / 1e6, color = option, lty = version)) + 
    scale_linetype_manual(values = c('calepa' = 'dotted', 'revised' = 'solid')) +
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'State-level oil production',
         # subtitle = 'million barrels', 
         x = NULL,
         y = 'million bbls',
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  # ghg
  ghg_fig = ggplot(all_scenarios_df %>% filter(scenario == name,
                                          type == 'total'), 
                        aes(x = year, y = ghg_MtCO2e, color = option, lty = version)) +
    scale_linetype_manual(values = c('calepa' = 'dotted', 'revised' = 'solid')) +
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'State-level GHG emissions',
         # subtitle = 'MtCO2e', 
         x = NULL,
         y = 'MtCO2e',
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  # mean ghg emissions
  mean_ghg_fig = ggplot(all_scenarios_df %>% filter(scenario == name,
                                           type == 'total'), 
                   aes(x = year, y = mean_emis_factor, color = option, lty = version)) + 
    scale_linetype_manual(values = c('calepa' = 'dotted', 'revised' = 'solid')) +
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'Average GHG emissions factor',
         # subtitle = 'kgCO2e per barrel', 
         x = NULL,
         y = 'kgCO2e per barrel',
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  out <- cowplot::plot_grid(new_wells_fig + theme(legend.position = 'none'),
                            oil_prod_fig + theme(legend.position = 'none'), 
                            ghg_fig + theme(legend.position = 'none'), 
                            mean_ghg_fig + theme(legend.position = 'none'),
                            nrow = 1)
  
  legend <- cowplot::get_legend(new_wells_fig)
  
  scen_plot <- cowplot::plot_grid(out, legend, ncol = 1, rel_heights = c(1, .1))
  
  ## title
  
  if(scen_choice == 4) {
    
    title <- ggdraw() + 
      draw_label(
        paste0(name, ': carbon price = central SCC'),
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
    
    final_out <- plot_grid(
      title, scen_plot,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    )  
    
  } else {
  
  title <- ggdraw() + 
    draw_label(
      name,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  final_out <- plot_grid(
    title, scen_plot,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  
       }

  return(final_out)
  
  }
  
  p1 <- plot_scen(1)
  p2 <- plot_scen(2)
  p3 <- plot_scen(3)
  p4 <- plot_scen(4)
  p5 <- plot_scen(5)
  p6 <- plot_scen(6)
  p7 <- plot_scen(7)
  
  macro_plot <- plot_grid(p1, p2, p3, p4, ncol = 1)
  policy_plot <- plot_grid(p5, p6, p7, ncol = 1)
  
  # save figures  -----
  
  # save figures ----
  macro_fname = paste0(oil_price_selection, '_macro_fig.pdf')
  ggsave(macro_plot, 
         filename = file.path(save_info_path, macro_fname), 
         width = 30, 
         height = 35)
  
  embed_fonts(file.path(save_info_path, macro_fname),
              outfile = file.path(save_info_path, macro_fname))
  print(paste0('Saved benchmark figures to ', macro_fname))
  
  
  policy_fname = paste0(oil_price_selection, '_policy_fig.pdf')
  ggsave(policy_plot, 
         filename = file.path(save_info_path, policy_fname), 
         width = 25, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, policy_fname),
              outfile = file.path(save_info_path, policy_fname))
  print(paste0('Saved diagnostic figures to ', policy_fname))

  
  ##############################################
  ## save spatial fis
  ###############################################
  
  plot_sp_figs <- function(scen_choice) {
    
    if(scen_choice == 1) {name <- scen_options[1]} 
    else if(scen_choice == 2) {name <- scen_options[2]}
    else if(scen_choice == 3) {name <- scen_options[3]}
    else if(scen_choice == 4) {name <- scen_options[4]}
    else if(scen_choice == 5) {name <- scen_options[5]}
    else if(scen_choice == 6) {name <- scen_options[6]}
    else {name <- scen_options[7]}
  
  
    ## spatial fig - bbls
    comp_bbls <- ggplot() +
      geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
      geom_sf(data = all_county_prod_df %>% filter(metric == 'difference (bbls)',
                                                   scenario == name), mapping = aes(geometry = geometry, fill = adj_val), lwd = 0.25, show.legend = TRUE) +
      scale_fill_gradient2(midpoint = 0, low = "red", mid = "white",
                           high = "blue") +
      labs(title = paste0(name, ' - 2045 vs. 2019'),
           fill = 'million bbls',
           x = NULL,
           y = NULL) +
      facet_wrap(~option, ncol = 1) +
      geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.ticks = element_blank()) 
    
    
    ## spacial fig perc
    comp_perc <- ggplot() +
      geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
      geom_sf(data = all_county_prod_df %>% filter(metric != 'difference (bbls)', scenario == name), mapping = aes(geometry = geometry, fill = adj_val), lwd = 0.25, show.legend = TRUE) +
      scale_fill_gradient2(midpoint = 0, low = "red", mid = "white",
                           high = "blue") +
      facet_wrap(~option, ncol = 1) +
      labs(title = paste0(name, ' - 2045 vs. 2019'),
           fill = '% change',
           x = NULL,
           y = NULL) +
      geom_sf_text(data = all_county_prod_df %>% filter(metric != 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n', round(adj_val), '%')), colour = "black", size = 2) +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.ticks = element_blank()) 
    
    fig_comp <- plot_grid(comp_bbls, comp_perc, ncol = 2)
    
    
    save_info_path_sp = file.path(save_info_path, "spatial-figs")
    dir.create(save_info_path_sp, showWarnings = FALSE)
    
    save_name <- gsub(" ", "_", name)
    
    ## save figures
    comp_fname = paste0('change_prod_county_', save_name, '.pdf')
    ggsave(fig_comp, 
           filename = file.path(save_info_path_sp, comp_fname), 
           width = 20, 
           height = 30)
    
    embed_fonts(file.path(save_info_path_sp, comp_fname),
                outfile = file.path(save_info_path_sp, comp_fname))
    print(paste0('Saved sp benchmark figure to ', comp_fname))  
    
  }
  
  
  sp1 <- plot_sp_figs(1)
  sp2 <- plot_sp_figs(2)
  sp3 <- plot_sp_figs(3)
  sp4 <- plot_sp_figs(4)
  sp5 <- plot_sp_figs(5)
  sp6 <- plot_sp_figs(6)
  sp7 <- plot_sp_figs(7)
  
  
  
} 