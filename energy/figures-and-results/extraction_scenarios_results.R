# extraction production and ghg emissions
# created: november 26, 2020
# author: meas meng

# ------------------ inputs ------------------
  
  res_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios'
  res_file        = 'extraction_prod_ghg_outputs.csv'

# ------------------ outputs ------------------
  
  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
  
# libraries --------
  
  library(data.table)
  library(feather)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# source items ------
  
  # source ccs emissions mean b calculation script
    items = list.files(here::here('src'))
    sapply(here::here('src', items), source)

# load results --------
  
  dt_res = fread(file.path(res_path, res_file), header = T)
    
# replace scenario names -------
    
  dt_res[, scenario := gsub('BAU', 'E-BAU', scenario)]
  dt_res[, scenario := gsub('E1', 'LCE1', scenario)]
  dt_res[, scenario := gsub('E2', 'LCE2', scenario)]
    
# ------------------------- plots ------------------------- 
  
  # plot theme & palettes ------
  
    pal_scenarios = c('E-BAU' = '#FFD200',
                      'R-BAU' = '#FFD200',
                      'LCE1' = '#72CDF4',
                      'LCR1' = '#72CDF4',
                      'LCE2' = '#005581',
                      'LCR2' = '#005581')
  
    theme_line = theme_ipsum(base_family = 'Arial',
                             grid = 'Y', 
                             plot_title_size = 10, 
                             subtitle_size = 9,
                             axis_title_just = 'center',
                             axis_title_size = 9, 
                             axis_text_size = 9,
                             strip_text_size = 9)  +
      theme(plot.title = element_text(hjust = 0, face = 'bold'),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.2, 'cm'),
            axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
            axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
            legend.title = element_text(size = 8, vjust = 0.5),
            legend.text = element_text(size = 8, vjust = 0.5),
            legend.margin = margin(t = 0, b = 0, unit = 'cm'),
            legend.position = 'top',
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
    
  
  # plot extraction scenarios: production ----------
  
    fig_prod_ext = ggplot(dt_res, aes(x = year, y = production_bbl/1e6, color = scenario)) + 
      geom_line(size = 1) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'Oil produced (million barrels)',
           color = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 160, 25), limits = c(0, 160), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios) +
      theme_line
    fig_prod_ext
    
    ggsave(fig_prod_ext,
           filename = file.path(fig_path, 'extraction_scenarios_production.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_prod_ext,
           filename = file.path(fig_path, 'extraction_scenarios_production.pdf'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'extraction_scenarios_production.pdf'),
                outfile = file.path(fig_path, 'extraction_scenarios_production.pdf'))
    
    
  # plot extraction scenarios: production ----------
    
    fig_ghg_ext = ggplot(dt_res, aes(x = year, y = ghg_mmt_co2e, color = scenario)) + 
      geom_line(size = 1) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'CO2e emitted (million metric tons)',
           color = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 15, 1), limits = c(0, 14), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios) +
      theme_line
    fig_ghg_ext
    
    ggsave(fig_ghg_ext,
           filename = file.path(fig_path, 'extraction_scenarios_ghg.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_ghg_ext,
           filename = file.path(fig_path, 'extraction_scenarios_ghg.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'extraction_scenarios_ghg.pdf'),
                outfile = file.path(fig_path, 'extraction_scenarios_ghg.pdf'))
    