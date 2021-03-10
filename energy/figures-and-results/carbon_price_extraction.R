# extraction production and ghg emissions under carbon price paths
# created: november 24, 2020
# author: meas meng

# ------------------ inputs ------------------

  res_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_20_all_scens'
  res_file     = 'summary_outputs_2020-2045.csv'

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

  dt_res = as.data.table(read_feather(file.path(res_path, res_file)))

# only keep no tax excise results ------

  dt_res = dt_res[excise_tax_scenario == 'no tax']

# keep BAU macro conditions ------

  dt_res = dt_res[oil_price_scenario == 'iea oil price' & 
                    innovation_scenario == 'low innovation' & 
                    ccs_scenario == 'medium CCS cost' & 
                    setback_scenario == 'no_setback' & 
                    prod_quota_scenario == 'no quota']
    
# factor scenarios for plotting
  dt_res$carbon_price_scenario <- factor(dt_res$carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))
    
    
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
    
    fig_carbon_price_bbl = ggplot(dt_res, aes(x = year, y = prod_mbbl, color = carbon_price_scenario)) +
      geom_line() +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'Million barrels of oil produced',
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 150), expand = c(0,0)) +
      scale_color_manual(values = ucsb_pal_prim4,
                         labels = c("price floor" = "Low carbon price",
                                    "central SCC" = "Central carbon price",
                                    "price ceiling" = "High carbon price")) + 
      theme_line +
      theme(legend.position = "top")
    fig_carbon_price_bbl
    
    ggsave(fig_carbon_price_bbl,
           filename = file.path(fig_path, 'extraction_carbon_price_paths_production.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_carbon_price_bbl,
           filename = file.path(fig_path, 'extraction_carbon_price_paths_production.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'extraction_carbon_price_paths_production.pdf'),
                outfile = file.path(fig_path, 'extraction_carbon_price_paths_production.pdf'))
    
    
  # plot: ghg emissions under price paths -------- 
    
    fig_carbon_price_ghg = ggplot(dt_res, aes(x = year, y = ghg_mmt_inno_ccs_adj, color = carbon_price_scenario)) +
      geom_line() +
      labs(title = 'Greenhouse gas emissions under carbon price scenarios',
           subtitle = 'Million metric tonnes of CO2e', 
           x = NULL,
           y = NULL,
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 11, 1), limits = c(0, 11), expand = c(0,0)) +
      scale_color_manual(values = ucsb_pal_prim4,
                         labels = c("price floor" = "CA C&T price floor",
                                    "central SCC" = "Central SCC estimate",
                                    "price ceiling" = "CA C&T price ceiling")) + 
      theme_line
    fig_carbon_price_ghg
    
    ggsave(fig_carbon_price_ghg,
           filename = file.path(fig_path, 'extraction_carbon_price_paths_ghg.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_carbon_price_ghg,
           filename = file.path(fig_path, 'extraction_carbon_price_paths_ghg.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'extraction_carbon_price_paths_ghg.pdf'),
                outfile = file.path(fig_path, 'extraction_carbon_price_paths_ghg.pdf'))
