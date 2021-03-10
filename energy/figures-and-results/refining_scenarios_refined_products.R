# refining scenarios refined products
# created: november 27, 2020
# author: meas meng

# ------------------ inputs ------------------
  
  res_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/refining_20201121/net_exports'
  res_file        = 'state_GJD_and_reGJD_production_all_refineries.csv'

# ------------------ outputs ------------------
  
  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
  
# libraries --------
  
  library(data.table)
  library(stringr) 
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# source items ------
  
  # source ccs emissions mean b calculation script
    items = list.files(here::here('src'))
    sapply(here::here('src', items), source)

# load results --------
  
  dt_res = fread(file.path(res_path, res_file), header = T)
  
# get selected scenarios --------
  
  dt_res[demand_scenario == 'BAU' & refining_scenario == 'historic exports', scenario := 'R-BAU']
  dt_res[demand_scenario == 'LC1' & refining_scenario == 'historic exports', scenario := 'LCR1']
  dt_res[demand_scenario == 'LC1' & refining_scenario == 'low exports', scenario := 'LCR2']
  
  dt_res = dt_res[!is.na(scenario)]
  
# change fuel names -----
  
  dt_res[fuel == 'drop-in gasoline', fuel := 'renewable gasoline']
  dt_res[, fuel := str_to_title(fuel)]
  
# reorder factor levels ------

  dt_res[, scenario := factor(scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]
  dt_res[, fuel := factor(fuel, levels = rev(c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                               'Jet', 'Sustainable Aviation Fuel', 'Exports')))]

# ------------------------- plots ------------------------- 
  
  # plot theme & palettes ------
  
    pal_fuel = c('Gasoline' = '#FF6E1B', 
                 'Renewable Gasoline' = '#ffb286', 
                 'Diesel' = '#E44C9A', 
                 'Renewable Diesel' = '#e59cc2', 
                 'Jet' = '#00778B', 
                 'Sustainable Aviation Fuel' = '#00A3AD',
                 'Exports' = '#DBD5CD')
  
    lab_fuel = c('Gasoline' = 'Gasoline', 
                 'Renewable Gasoline' = 'Renewable gasoline', 
                 'Diesel' = 'Diesel', 
                 'Renewable Diesel' = 'Renewable diesel', 
                 'Jet' = 'Jet', 
                 'Sustainable Aviation Fuel' = 'Sustainable aviation fuel',
                 'Exports' = 'Exports')
  
    pal_fuel_2 = c('Gasoline' = '#4bbbe6', 
                   'Renewable Gasoline' = '#c2eaf7', 
                   'Diesel' = '#fda89c', 
                   'Renewable Diesel' = '#fddad5', 
                   'Jet' = '#169486', 
                   'Sustainable Aviation Fuel' = '#61c8c1',
                   'Exports' = '#dadbdf')
  
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
            legend.position = 'top',
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
    
  
  # plot: area chart of refined products (v1) -------
    
    fig_fuel_demand_1 = ggplot(dt_res, aes(x = year, y = consumption_bge/1e6, fill = fuel, group = fuel)) +
      geom_area() +
      facet_wrap(. ~ scenario, ncol = 1) +
      labs(title = NULL,
           subtitle = NULL,
           x = NULL,
           y = 'Million barrels of gasoline equivalent produced',
           fill = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) +
      scale_fill_manual(values = pal_fuel, labels = lab_fuel, guide = guide_legend(nrow = 4)) + 
      theme_line 
    fig_fuel_demand_1
    
    ggsave(fig_fuel_demand_1,
           filename = file.path(fig_path, 'refining_scenarios_refined_products_v1.png'),
           width = 6.5,
           height = 8,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_fuel_demand_1,
           filename = file.path(fig_path, 'refining_scenarios_refined_products_v1.pdf'),
           width = 6.5,
           height = 8,
           dpi = 400, 
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'refining_scenarios_refined_products_v1.pdf'),
                outfile = file.path(fig_path, 'refining_scenarios_refined_products_v1.pdf'))
    
    
  # plot: area chart of refined products (v2) -------
    
    fig_fuel_demand_2 = ggplot(dt_res, aes(x = year, y = consumption_bge/1e6, fill = fuel, group = fuel)) +
      geom_area() +
      facet_wrap(. ~ scenario, ncol = 1) +
      labs(title = NULL,
           subtitle = NULL,
           x = NULL,
           y = 'Million barrels of gasoline equivalent produced',
           fill = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) +
      scale_fill_manual(values = pal_fuel_2, guide = guide_legend(ncol = 4)) + 
      theme_line 
    fig_fuel_demand_2
    
    ggsave(fig_fuel_demand_2,
           filename = file.path(fig_path, 'refining_scenarios_refined_products_v2.png'),
           width = 6.5,
           height = 8,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
  