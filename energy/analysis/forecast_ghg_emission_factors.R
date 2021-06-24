# forecast ghg emission factors
# created: june 23, 2021
# author: @measrainsey

# inputs ------------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  res_path        = 'data/OPGEE'
  prod_file       = 'outputs/stocks-flows/crude_prod_x_field_revised.csv'
  
# outputs -----------
  
  save_path       = 'outputs/ghg-emissions/opgee-results'
  
# load packages ------
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# read in results files -----
  
  files_res = list.files(path = file.path(emlab_path, res_path), pattern = 'opgee_doc_results')
  list_res = lapply(file.path(emlab_path, res_path, files_res), fread)
  dt_res = rbindlist(list_res)

# calculate upstream emissions ------
  
  dt_res[, upstream_gCO2e_MJ := exploration_gCO2e_MJ + drilling_gCO2e_MJ + crude_production_gCO2e_MJ + 
           surface_processing_gCO2e_MJ + maintenance_gCO2e_MJ + waste_gCO2e_MJ + other_gCO2e_MJ]
  dt_res[, upstream_kgCO2e_bbl := upstream_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  
# get all fields that ever have sor > 0 ------
  
  fields_sor = unique(dt_res[sor_bbl_bbl > 0, field_name])
  
# get top 10 producing fields in 2019 ------
  
  dt_prod = fread(file.path(emlab_path, prod_file))
  dt_prod[, order := frank(-total_bbls), by = .(year)]
  fields_top10 = dt_prod[year == max(year) & order %in% 1:10]
  
  fields_top10[, field_name := fifelse(doc_fieldname == 'Belridge  South', 'Belridge, South', doc_fieldname)]
  
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
  
  
  
  # plot linearly fit upstream emission factors of sor fields ---------
  
    fig_line_sor = ggplot(dt_res[field_name %in% fields_sor & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_line() +
      # geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Emission factors for fields that use steam injection (2000-2018)',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,400,100), limits = c(0,400), expand = c(0,0)) +
      theme_line
  fig_line_sor
  
  ggsave(fig_line_sor,
         filename = file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.png'),
         width = 12,
         height = 8,
         dpi = 600, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_line_sor,
         filename = file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.pdf'),
         width = 12,
         height = 8,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.pdf'),
              outfile = file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.pdf'))
  
  
  # plot linearly fit upstream emission factors of sor fields ---------
    
    fig_lm_sor = ggplot(dt_res[field_name %in% fields_sor & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Linearly regressed emission factors for fields that use steam injection',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
    scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,400,100), limits = c(0,400), expand = c(0,0)) +
      theme_line
    fig_lm_sor
    
    ggsave(fig_lm_sor,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.png'),
           width = 12,
           height = 8,
           dpi = 600, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_lm_sor,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.pdf'),
           width = 12,
           height = 8,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.pdf'),
                outfile = file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.pdf'))
    
    
  # plot emissions factors for top 10 fields ------
  
    fig_line_top10 = ggplot(dt_res[field_name %in% fields_top10[, field_name] & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_line() +
      # geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Emission factors for top 10 producing fields (2000-2018)',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,200,50), limits = c(0,200), expand = c(0,0)) +
      theme_line
    fig_line_top10
    
    ggsave(fig_line_top10,
           filename = file.path(emlab_path, save_path, 'line_emission_factors_top_fields.png'),
           width = 12,
           height = 8,
           dpi = 600, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_line_top10,
           filename = file.path(emlab_path, save_path, 'line_emission_factors_top_fields.pdf'),
           width = 12,
           height = 8,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(emlab_path, save_path, 'line_emission_factors_top_fields.pdf'),
                outfile = file.path(emlab_path, save_path, 'line_emission_factors_top_fields.pdf'))

  # plot linearly fit emissions factors for top 10 fields ------
    
    fig_lm_top10 = ggplot(dt_res[field_name %in% fields_top10[, field_name] & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Linearly regressed emission factors for top 10 producing fields',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,200,50), limits = c(0,200), expand = c(0,0)) +
      theme_line
    fig_lm_top10
    
    ggsave(fig_lm_top10,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.png'),
           width = 12,
           height = 8,
           dpi = 600, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_lm_top10,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.pdf'),
           width = 12,
           height = 8,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.pdf'),
                outfile = file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.pdf'))
    