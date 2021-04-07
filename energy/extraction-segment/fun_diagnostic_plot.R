plot_diagnostic_outputs <- function(oil_price_selection, output_extraction) {
  
  print('Now plotting outputs...')
  
  # baseline comparison output path
  
  base_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  
  # create objects for list items -------
  
  state_out = output_extraction[[3]]

  # read in baseline for comparison ------
  
  state_base = fread(file.path(base_path, 'extraction_2021-04-05', 'diagnostic', 'diagnostic-state-level-results.csv'), header = T)
  
  # add column for base case
  state_base[, version := "base"]
  
  # select state-level outputs
  state_adj = output_extraction[[3]]
  
  state_adj[, version := paste0("adj-", adj_name)]
  
  # bind data
  state_all <- rbind(state_base, state_adj)
  
  setcolorder(state_all, 
              c("version", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", "new_wells", 
                "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl", "existing_ghg_kgCO2e", "new_ghg_kgCO2e", 
                "total_ghg_kgCO2e", "total_ghg_mtCO2e"))
  
  state_all[, total_ghg_mtCO2e := NULL]
  
  state_all_prod <- state_all[, c("version", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                  "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", "new_wells", 
                                  "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl")]
  
  state_all_prod <- melt(state_all_prod,
                         measure.vars = c("existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"),
                         variable.name = "type",
                         value.name = "production_bbls")
  
  state_all_ghg <- state_all[, c("version", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                   "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", "new_wells", 
                                   "existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e")]
  
  state_all_ghg <- melt(state_all_ghg,
                         measure.vars = c("existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e"),
                         variable.name = "type",
                         value.name = "ghg_kgCO2e")
  
  # plot theme --------------------
  
  theme_line = theme_ipsum(base_family = 'Arial',
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
          legend.text = element_text(size = 12),
          legend.position = 'bottom')
  
  # extraction (state, old, new)
  fig_oilpx_wells_actual = ggplot(oilpx_res_total, 
                                  aes(x = year, y = wm_new_wells_pred, group = oil_price_in_2045, color = oil_price_in_2045)) + 
    geom_line() +
    labs(title = 'New well entry under oil price scenarios',
         subtitle = 'Wells entered', 
         x = 'Year',
         y = NULL,
         color = 'Oil price in 2045 ($/bbl)') +
    scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  
  # save outputs to csv -----
  
  # create subdirectory of save_path for each oil price ------
  
  save_processed_path = file.path(save_path, oil_price_selection)
  dir.create(save_processed_path, showWarnings = FALSE)
  
  # save field-level results -----
  
  field_fname = paste0(oil_price_selection, '-field-level-results.csv')
  fwrite(field_all, file.path(save_processed_path, field_fname), row.names = F)
  print(paste0('Saved field-level results to ', file.path(save_processed_path, field_fname)))
  
  # save state-level results ------
  
  state_fname = paste0(oil_price_selection, '-state-level-results.csv')
  fwrite(state_all, file.path(save_processed_path, state_fname), row.names = F)
  print(paste0('Saved state-level results to ', file.path(save_processed_path, state_fname)))
  
  # create output objects --------
  
  outputs_processed = list(field_all,
                           state_all)
  
  return(outputs_processed)
  
  
} 