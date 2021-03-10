# gather and interpolate petroleum-based demand from ITS
# created: october 21, 2020
# author: meas meng

# inputs -----------------

  data_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw'
  its_file        = 'Study 1 - Preliminary Fuel Volumes BAU & LC1.xlsx'
  # bau_file        = 'Copy of Scenarios for 2045 Study Results Sep 7c.xlsx'
  # lc1_file        = 'CN study LC1 fuel consumption results 8oct.xlsx'
  avgas_file      = 'Distillates 10-10.xlsx'

# outputs ----------------
  
  save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/fuel-demand/prelim-results'
  
# libraries ------
  
  library(data.table)
  library(openxlsx)
  library(lspline)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# import data --------
  
  # import bau data
    dt_bau = setDT(read.xlsx(file.path(data_path, its_file), sheet = 'Sheet1', rows = c(1, 7:19), cols = 2:37))
    colnames(dt_bau) = c('fuel', 'units', 2017:2050)
    dt_bau[, fuel := tolower(fuel)]

  # import lc1 data
    dt_lc1 = setDT(read.xlsx(file.path(data_path, its_file), sheet = 'Sheet1', rows = c(1, 23:34), cols = 2:37))
    colnames(dt_lc1) = c('fuel', 'units', 2017:2050)
    dt_lc1[, fuel := tolower(fuel)]

  # import aviation gasoline data
    dt_avgas = setDT(read.xlsx(file.path(data_path, avgas_file), sheet = 'Sheet1', rows = c(4, 16), cols = c(3:38)))
    
  # import intra-state jet fuel data
    dt_jet = setDT(read.xlsx(file.path(data_path, avgas_file), sheet = 'Sheet1', rows = c(4, 14), cols = c(3:38)))

# melt fuel demand data -----
  
  bau_long = melt(dt_bau, measure.vars = as.character(2017:2050),
                  variable.name = 'year', value.name = 'consumption_million_gge')
  bau_long[, consumption_gge := consumption_million_gge*1e6 ]
  bau_long[, scenario := 'BAU']
  bau_long[, fuel := as.character(fuel)]
    
  lc1_long = melt(dt_lc1, measure.vars = as.character(2017:2050),
                  variable.name = 'year', value.name = 'consumption_million_gge')
  lc1_long[, consumption_gge := consumption_million_gge*1e6 ]
  lc1_long[, scenario := 'LC1']
  lc1_long[, fuel := as.character(fuel)]
  
  avgas_long = melt(dt_avgas, measure.vars = colnames(dt_avgas)[2:35],
                    variable.name = 'year', value.name = 'consumption_million_gge')
  avgas_long[, consumption_gge := consumption_million_gge*1e6]
  avgas_long[, fuel := 'aviation gasoline']
  avgas_long[, X1 := NULL]
  avgas_long[, year := as.numeric(as.character(year))]
  
  jet_long = melt(dt_jet, measure.vars = colnames(dt_jet)[2:35],
                  variable.name = 'year', value.name = 'consumption_million_gge')
  jet_long[, consumption_gge := consumption_million_gge*1e6]
  jet_long[, fuel := 'jet (intrastate)']
  jet_long[, X1 := NULL]
  jet_long[, year := as.numeric(as.character(year))]
  setcolorder(jet_long, c('year', 'fuel', 'consumption_million_gge', 'consumption_gge'))

# add aviation gasoline to gasoline demand for bau and lc1 scenarios ---------
  
  bau_adj = rbindlist(list(bau_long[, .(year, fuel, scenario, consumption_gge)],
                           copy(avgas_long[, .(year, fuel, consumption_gge)])[, scenario := 'BAU']),
                      use.names = T)
  bau_adj[, adj_fuel := ifelse(fuel == 'aviation gasoline', 'gasoline', fuel)]
  bau_adj = bau_adj[, .(consumption_gge = sum(consumption_gge)), by = .(year, adj_fuel, scenario)]
  setnames(bau_adj, 'adj_fuel', 'fuel')
  
  lc1_adj = rbindlist(list(lc1_long[, .(year, fuel, scenario, consumption_gge)],
                           copy(avgas_long[, .(year, fuel, consumption_gge)])[, scenario := 'LC1']),
                      use.names = T)
  lc1_adj[, adj_fuel := ifelse(fuel == 'aviation gasoline', 'gasoline', fuel)]
  lc1_adj = lc1_adj[, .(consumption_gge = sum(consumption_gge)), by = .(year, adj_fuel, scenario)]
  setnames(lc1_adj, 'adj_fuel', 'fuel')
  
  
# concatenate bau and lc1 data -------
  
  demand_all = rbindlist(list(bau_adj[, .(year, fuel, scenario, consumption_gge)],
                              lc1_adj[, .(year, fuel, scenario, consumption_gge)]),
                         use.names = T)
  demand_all[, year := as.numeric(as.character(year))]
  demand_all[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 
                                               'drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel', 'renewable natural gas', 
                                               'ethanol', 'biodiesel', 'hdv electricity', 'hdv hydrogen', 'ldv electricity', 'ldv hydrogen'))]
  
  
# export to csv -------
  
  fwrite(demand_all, file = file.path(save_path, 'its_demand_bau_and_lc1_2020_2045.csv'), row.names = F)
  fwrite(jet_long, file = file.path(save_path, 'its_demand_intrastate_jet_2020_2045.csv'), row.names = F)
  
# ------------------------------ PLOTS --------------------------------
  
  # theme ------
  
    pal_scenarios = c('BAU' = '#576b81',
                      'Mid Case' = '#576b81',
                      'LC1' = '#50a727',
                      'Low Case' = '#50a727',
                      'High Case' = '#f05c0b')
  
    theme_line = theme_ipsum(base_family = 'Secca Soft',
                             grid = 'Y', 
                             plot_title_size = 20, 
                             subtitle_size = 18,
                             axis_title_just = 'center',
                             axis_title_size = 18, 
                             axis_text_size = 14,
                             strip_text_size = 16)  +
      theme(plot.title = element_text(hjust = 0, face = 'bold'),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 12, color = '#5c5c5c', face = 'plain'),
            axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.25, "cm"),
            axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
            plot.margin = unit(c(1,2,1,1), "lines"),
            legend.text = element_text(size = 16),
            legend.position = 'bottom')
  
  # line plot of demand -------
    
    fig_line = ggplot(demand_all[fuel %in% c('gasoline', 'diesel', 'drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel') & year >= 2020], 
                       aes(x = year, y = consumption_gge/1e6, color = scenario)) + 
      geom_line(size = 1.4) +
      facet_grid(cols = vars(fuel), labeller = labeller(fuel = c('gasoline' = 'Gasoline',
                                                                 'diesel' = 'Diesel',
                                                                 'drop-in gasoline' = 'Drop-in Gasoline', 
                                                                 'renewable diesel' = 'Renewable Diesel', 
                                                                 'sustainable aviation fuel' = 'Sustainable Aviation Fuel'))) +
      labs(title = 'ITS fuel demand forecast (2020-2050)',
           subtitle = 'Million gge',
           caption = 'Gasoline includes aviation gasoline',
           x = 'Year',
           y = NULL,
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(0, 16e3, 1e3), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios)+
      theme_line
    
    ggsave(fig_line, 
           filename = file.path(save_path, 'its_demand_bau_and_lc1_2020_2045.pdf'), 
           width = 18, 
           height = 9.5)
    
    embed_fonts(file.path(save_path, 'its_demand_bau_and_lc1_2020_2045.pdf'),
                outfile = file.path(save_path, 'its_demand_bau_and_lc1_2020_2045.pdf'))
    
  
    
  # line plot of intrastate jet demand -------
    
    fig_jet = ggplot(jet_long[year >= 2020], aes(x = year, y = consumption_gge/1e6), color = '#576b81') + 
      geom_line(size = 1.4) +
      labs(title = 'Instra-state jet fuel demand',
           subtitle = 'Million gge',
           x = 'Year',
           y = NULL,
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(0, 700, 100), limits = c(0, 700), expand = c(0,0)) +
      theme_line
    
    ggsave(fig_jet, 
           filename = file.path(save_path, 'its_intrastate_jet_demand_forecast_2020_2045.pdf'), 
           width = 11, 
           height = 6.5)
    
    embed_fonts(file.path(save_path, 'its_intrastate_jet_demand_forecast_2020_2045.pdf'),
                outfile = file.path(save_path, 'its_intrastate_jet_demand_forecast_2020_2045.pdf'))
    