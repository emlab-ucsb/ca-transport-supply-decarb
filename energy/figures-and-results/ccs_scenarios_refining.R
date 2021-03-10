# refining production and ghg emissions under ccs scenarios
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
                    innovation_scenario == 'low innovation'
                    # carbon_price_scenario == 'price floor' &
                    ]
    
# keep ghg emissions --------
  
  dt_res = dt_res[fuel == 'crude' & boundary == 'complete' & source == 'total']

# reorder factor levels -------
    
  dt_res[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
  dt_res[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
  dt_res[, ccs_scenario := factor(ccs_scenario, levels = c('low CCS cost', 'medium CCS cost', 'high CCS cost'))]

  ## for plotting
  dt_res[, carbonp_name := ifelse(carbon_price_scenario == "price floor", "Low carbon price",
                                  ifelse(carbon_price_scenario == "price ceiling", "High carbon price", "Central carbon price"))]
  
  dt_res[, carbonp_name := factor(carbonp_name, levels = c('Low carbon price', 'Central carbon price', 'High carbon price'))]    
  
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
    
    fig_ccs_price_bbl = ggplot(dt_res[type == 'consumption'], aes(x = year, y = value/1e6, color = ccs_scenario)) +
      geom_line() + 
      facet_grid(cols = vars(carbon_price_scenario)) + 
      labs(title = 'Crude consumption under CCS scenarios',
           subtitle = 'Million barrels of oil produced', 
           x = NULL,
           y = NULL,
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 600, 50), limits = c(0, 600), expand = c(0,0)) +
      scale_color_manual(values = ucsb_pal_prim4) + 
      theme_line
    fig_ccs_price_bbl
    
    ggsave(fig_ccs_price_bbl,
           filename = file.path(fig_path, 'refining_ccs_price_paths_production.png'),
           width = 6.5,
           height = 4,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_ccs_price_bbl,
           filename = file.path(fig_path, 'refining_ccs_price_paths_production.pdf'),
           width = 6.5,
           height = 4,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_ccs_price_paths_production.pdf'),
                outfile = file.path(fig_path, 'refining_ccs_price_paths_production.pdf'))
    
    
  # plot: ghg emissions under price paths -------- 
    
    fig_ccs_price_ghg = ggplot(dt_res[type == 'ghg'], aes(x = year, y = value/1e9, color = ccs_scenario)) +
      geom_line() +
      facet_grid(cols = vars(carbonp_name)) + 
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = expression(paste("Million metric tonnes of ", CO[2], "e")),
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30), expand = c(0,0)) +
      scale_color_manual(values = ucsb_pal_prim4,
                         labels = c("low CCS cost" = "Low CCS cost",
                                    "medium CCS cost" = "Central CCS cost",
                                    "high CCS cost" = "High CCS cost")) + 
      theme_line +
      theme(legend.position = "top")
    fig_ccs_price_ghg
    
    ggsave(fig_ccs_price_ghg,
           filename = file.path(fig_path, 'refining_ccs_price_paths_ghg.png'),
           width = 6.5,
           height = 4,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_ccs_price_ghg,
           filename = file.path(fig_path, 'refining_ccs_price_paths_ghg.pdf'),
           width = 6.5,
           height = 4,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_ccs_price_paths_ghg.pdf'),
                outfile = file.path(fig_path, 'refining_ccs_price_paths_ghg.pdf'))
