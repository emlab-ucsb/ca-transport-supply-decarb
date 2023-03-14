# refining production and ghg emissions
# created: november 27, 2020
# author: meas meng

# ------------------ inputs ------------------
  
  prod_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/baseline-2019'
  res_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  prod_file       = 'baseline_site_level_refining.csv'
  ghg_file        = 'refining_emissions_state_2019.csv'
  res_file        = 'refining_scenario_outputs_state_net_exports.csv'

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
  
  dt_prod_2019 = fread(file.path(prod_path, prod_file), header = T)
  dt_ghg_2019 = fread(file.path(res_path, ghg_file), header = T)
  dt_res = fread(file.path(res_path, res_file), header = T)
  
# keep BAU macro conditions ------
  
  dt_res = dt_res[innovation_scenario == 'low innovation' & 
                    carbon_price_scenario == 'price floor' & 
                    ccs_scenario == 'medium CCS cost']
  
# get selected scenarios --------
  
  dt_res[demand_scenario == 'BAU' & refining_scenario == 'historic exports', scenario := 'R-BAU']
  dt_res[demand_scenario == 'LC1' & refining_scenario == 'historic exports', scenario := 'LCR1']
  dt_res[demand_scenario == 'LC1' & refining_scenario == 'low exports', scenario := 'LCR2']
  
  dt_res = dt_res[!is.na(scenario) & fuel == 'crude' & source == 'total' & boundary == 'complete']
  
# get production in 2019 ------
  
  prod_2019 = dt_prod_2019[fuel == 'crude_equivalent', .(type = 'consumption',
                                                         units = 'bbl',
                                                         value = sum(value_bbls)), by = .(year)]
  prod_2019[, j := 1]
  
# get ghg in 2019
  
  ghg_2019 = dt_ghg_2019[boundary == 'complete']
  ghg_2019[, j := 1]
  
# create unique scenarios for 2019 -----
  
  un_scens = data.table(scenario = c('R-BAU', 'LCR1', 'LCR2'), j = 1)
  prod_2019 = prod_2019[un_scens, on = .(j)]
  ghg_2019 = ghg_2019[un_scens, on = .(j)]
  
# bind 2019 with projected -------
  
  dt_res_2 = rbindlist(list(prod_2019, ghg_2019, dt_res), use.names = T, fill = T)
  
# reorder factor levels ------
  
  dt_res_2[, scenario := factor(scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]

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
  
    fig_prod_ref = ggplot(dt_res_2[type == 'consumption'], aes(x = year, y = value/1e6, color = scenario)) + 
      geom_line(size = 1) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'Crude oil and renewable feedstocks\n(million barrels of crude oil equivalent)',
           color = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 700, 50), limits = c(0, 600), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios) +
      theme_line
    fig_prod_ref
    
    ggsave(fig_prod_ref,
           filename = file.path(fig_path, 'refining_scenarios_production.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_prod_ref,
           filename = file.path(fig_path, 'refining_scenarios_production.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_scenarios_production.pdf'),
                outfile = file.path(fig_path, 'refining_scenarios_production.pdf'))
    
  # plot extraction scenarios: production ----------
    
    fig_ghg_ref = ggplot(dt_res_2[type == 'ghg'], aes(x = year, y = value/1e9, color = scenario)) + 
      geom_line(size = 1) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'CO2e emitted (million metric tons)',
           color = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios) +
      theme_line
    fig_ghg_ref
    
    ggsave(fig_ghg_ref,
           filename = file.path(fig_path, 'refining_scenarios_ghg.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_ghg_ref,
           filename = file.path(fig_path, 'refining_scenarios_ghg.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_scenarios_ghg.pdf'),
                outfile = file.path(fig_path, 'refining_scenarios_ghg.pdf'))
    