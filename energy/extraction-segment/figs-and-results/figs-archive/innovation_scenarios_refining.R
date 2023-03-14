# refining production and ghg emissions under innovation scenarios
# created: november 24, 2020
# author: meas meng

# ------------------ inputs ------------------

  res_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  res_file     = 'refining_scenario_outputs_state_net_exports.csv'

# ------------------ outputs ------------------

  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model'

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

  dt_res = fread(file.path(res_path, res_file))

# only keep no tax excise results ------

  # dt_res = dt_res[excise_tax_scenario == 'no tax']

# keep BAU macro conditions ------

  dt_res = dt_res[demand_scenario == 'BAU' & 
                    refining_scenario == 'historic exports' &
                    # innovation_scenario == 'low innovation'
                    carbon_price_scenario == 'price floor'
                    ]
    
# keep ghg emissions --------
  
  dt_res = dt_res[fuel == 'crude' & boundary == 'complete' & source == 'total']

# reorder factor levels -------
    
  dt_res[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
  dt_res[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
  dt_res[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]

# ------------------------- plots ------------------------- 
    
  # plot theme & palettes ------
    
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
            legend.position = 'bottom',
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
    
  # plot: extraction under price paths -------- 
    
    fig_innovation_bbl = ggplot(dt_res[type == 'consumption'], aes(x = year, y = value/1e6, color = innovation_scenario)) +
      geom_line() + 
      labs(title = NULL, # 'Crude consumption under innovation scenarios',
           subtitle = NULL, 
           x = NULL,
           y = 'Million barrels of oil produced',
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 600, 50), limits = c(0, 600), expand = c(0,0)) +
      scale_color_manual(values = ucsb_pal_prim4) + 
      theme_line
    fig_innovation_bbl
    
    ggsave(fig_innovation_bbl,
           filename = file.path(fig_path, 'refining_innovation_scenarios_production.png'),
           width = 6.5,
           height = 4,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_innovation_bbl,
           filename = file.path(fig_path, 'refining_innovation_scenarios_production.pdf'),
           width = 6.5,
           height = 4,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_innovation_scenarios_production.pdf'),
                outfile = file.path(fig_path, 'refining_innovation_scenarios_production.pdf'))
    
    
  # plot: ghg emissions under price paths -------- 
    
    fig_innovation_ghg = ggplot(dt_res[type == 'ghg'], aes(x = year, y = value/1e9, color = innovation_scenario)) +
      geom_line() +
      labs(title = NULL, # 'Greenhouse gas emissions under CCS scenarios',
           subtitle = NULL, 
           x = NULL,
           y = expression(paste("Million metric tonnes of ", CO[2], "e")),
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30), expand = c(0,0)) +
      scale_color_manual(values = ucsb_pal_prim4,
                         labels = c("low innovation" = "Low innovation",
                                   "high innovation" = "High innovation")) + 
      theme_line +
      theme(legend.position = "top")
    fig_innovation_ghg
    
    ggsave(fig_innovation_ghg,
           filename = file.path(fig_path, 'refining_innovation_scenarios_ghg.png'),
           width = 6.5,
           height = 4,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_innovation_ghg,
           filename = file.path(fig_path, 'refining_innovation_scenarios_ghg.pdf'),
           width = 6.5,
           height = 4,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_innovation_scenarios_ghg.pdf'),
                outfile = file.path(fig_path, 'refining_innovation_scenarios_ghg.pdf'))
