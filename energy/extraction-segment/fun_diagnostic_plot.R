plot_diagnostic_outputs <- function(oil_price_selection, output_extraction) {
  
  print('Now plotting outputs...')
  
  # baseline comparison output path
  
  base_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  
  # create objects for list items -------
  
  state_out = output_extraction[[3]]

  state_out[, version := paste0("adj_", run_type)]
  
  # read in baseline for comparison ------
  
  state_base = fread(file.path(base_path, 'extraction_2021-04-07', 'baseline', 'diagnostic-state-level-results.csv'), header = T)
  
  # add column for base case
  state_base[, version := "base"]
  
  # bind data
  state_all <- rbind(state_base, state_out)
  
  state_all[, total_ghg_mtCO2e := NULL]
  
  ## scenario lut
  scen_lut <- unique(state_all[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                   "setback_scenario", "prod_quota_scenario", "excise_tax_scenario")])
  
  scen_lut[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LC2",
                                  fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LC1", "BAU"))]
  
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
                                  aes(x = year, y = production_bbls / 1e6, group = type, color = version)) + 
    geom_line(alpha = 0.85, size = 1.5) +
    facet_grid(type ~ scen_name) +
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
    geom_line(alpha = 0.85, size = 2) +
    facet_grid(type ~ scen_name) +
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
    geom_line(alpha = 0.85, size = 2) +
    facet_grid(~ scen_name) +
    labs(title = 'State-level new wells',
         subtitle = 'number of new wells', 
         x = 'Year',
         y = NULL,
         color = '') +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  # save figures  -----
  
  # save info file
  save_info_path = file.path(save_path, run_type)
  print(paste0("Saving diagnostic figures to ", save_info_path))
  
  # save figures ----
  extraction_fname = paste0(oil_price_selection, '-extraction_fig.pdf')
  ggsave(extraction_fig, 
         filename = file.path(save_info_path, extraction_fname), 
         width = 23, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, extraction_fname),
              outfile = file.path(save_info_path, extraction_fname))
  print(paste0('Saved diagnostic figures to ', extraction_fname))
 
  
  ghg_fname = paste0(oil_price_selection, '-ghg_fig.pdf')
  ggsave(ghg_fig, 
         filename = file.path(save_info_path, ghg_fname), 
         width = 23, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, ghg_fname),
              outfile = file.path(save_info_path, ghg_fname))
  print(paste0('Saved diagnostic figures to ', ghg_fname))
  
  
  new_wells_fname = paste0(oil_price_selection, '-new_wells_fig.pdf')
  ggsave(new_wells_fig, 
         filename = file.path(save_info_path, new_wells_fname), 
         width = 23, 
         height = 7)
  
  embed_fonts(file.path(save_info_path, new_wells_fname),
              outfile = file.path(save_info_path, new_wells_fname))
  print(paste0('Saved diagnostic figures to ', new_wells_fname))
  
  
  
} 