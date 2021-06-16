# comparison of historical ghg inventory emissions and opgee modeled emissions
# date created: june 15, 2021
# author: @measrainsey

# inputs -----------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  prod_file       = 'data/stocks-flows/processed/well_prod_m.rds'
  inv_file        = 'data/stocks-flows/processed/historic_ghg_emissions_og_ng_adjusted.csv'
  proj_file       = 'outputs/predict-production/extraction_2021-06-04/revised-remove-plugged/diagnostic-field-level-results.csv'
  opgee_file      = 'outputs/stocks-flows/ghg_emissions_x_field_2015_revised.csv'
  
# outputs ------------
  
  save_path       = 'outputs/ghg-emissions/modeled-vs-reported'
  
# libraries -----------
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# read in data ---------
  
  dt_prod = as.data.table(readRDS(file.path(emlab_path, prod_file)))
  dt_inv = fread(file.path(emlab_path, inv_file))
  dt_proj = fread(file.path(emlab_path, proj_file))
  dt_opgee = fread(file.path(emlab_path, opgee_file))
  
# rename column ------
  
  setnames(dt_prod, 'FieldCode', 'doc_field_code')

# pad field code with leading zeroes -----
  
  dt_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_proj[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_opgee[, doc_field_code := sprintf("%03s", doc_field_code)]

# get field-year level production ------
  
  agg_prod = dt_prod[, .(oil_production_bbl = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code, year)]
  
# merge field-year level production with opgee emission factors ------
  
  prod_ghg = agg_prod[dt_opgee[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)], on = .(doc_field_code)]
  prod_ghg[, kg_co2e := oil_production_bbl*upstream_kgCO2e_bbl]
  prod_ghg[, mtco2e := kg_co2e/1e9]
  
# agg modeled emissions by year -----
  
  prod_ghg_year = prod_ghg[, .(mtco2e = sum(mtco2e, na.rm = T)), by = .(year)]
  prod_ghg_year[, emissions_type := 'opgee (all fields)']
  
# get modeled emissions for only fields that are included in outputs of extraction module -----
  
  prod_ghg_year_sel = prod_ghg[doc_field_code %in% unique(dt_proj[, doc_field_code]), .(mtco2e = sum(mtco2e, na.rm = T)), by = .(year)]
  prod_ghg_year_sel[, emissions_type := 'opgee (output fields only)']
  
# change inventory emissions type names ------
  
  dt_inv[, emissions_type := ifelse(emissions_type == 'mtco2e', 'ghg inventory (unadjusted)', 'ghg inventory (adjusted)')]
  # dt_inv[, emissions_type := gsub('mtco2e', 'inventory', emissions_type)]
  
# combine all emissions ------
  
  ghg_all = rbindlist(list(dt_inv, prod_ghg_year, prod_ghg_year_sel), use.names = T)

# ------------------------------- plot ---------------------------
  
  # theme ---------
  
  theme_line = theme_ipsum(base_family = 'Secca Soft',
                           grid = 'Y', 
                           plot_title_size = 20, 
                           subtitle_size = 18,
                           axis_title_just = 'center',
                           axis_title_size = 18, 
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
          legend.text = element_text(size = 16),
          legend.position = 'bottom')
  

  # line plot comparing emissions (1977-2019) -------
  
  fig_all_years = ggplot(ghg_all, aes(x = year, y = mtco2e, group = emissions_type, color = emissions_type)) +
    geom_line(size = 1) +
    labs(title = 'Reported vs modeled GHG emissions',
         subtitle = 'Million tonnes',
         x = NULL,
         y = NULL,
         color = NULL) + 
    scale_x_continuous(breaks = seq(1975,2020,5), limits = c(1975,2020), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,40,2), limits = c(0,34), expand = c(0,0)) +
    scale_color_ipsum() +
    theme_line
  fig_all_years
  
  ggsave(fig_all_years,
         filename = file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_all_years.png'),
         width = 12,
         height = 8,
         dpi = 600, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_all_years,
         filename = file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_all_years.pdf'),
         width = 12,
         height = 8,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_all_years.pdf'),
              outfile = file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_all_years.pdf'))
  
  
  # line plot comparing emissions (2000-2019) -------
  
  fig_sel_years = ggplot(ghg_all, aes(x = year, y = mtco2e, group = emissions_type, color = emissions_type)) +
    geom_line(size = 1) +
    labs(title = 'Reported vs modeled GHG emissions',
         subtitle = 'Million tonnes',
         x = NULL,
         y = NULL,
         color = NULL) + 
    scale_x_continuous(breaks = seq(2000,2020,1), limits = c(2000,2019), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,30,2), limits = c(0,30), expand = c(0,0)) +
    scale_color_ipsum() +
    theme_line
  fig_sel_years
  
  ggsave(fig_sel_years,
         filename = file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_selected_years.png'),
         width = 12,
         height = 8,
         dpi = 600, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_sel_years,
         filename = file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_selected_years.pdf'),
         width = 12,
         height = 8,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_selected_years.pdf'),
              outfile = file.path(emlab_path, save_path, 'comparison_modeled_vs_reported_selected_years.pdf'))
  
  