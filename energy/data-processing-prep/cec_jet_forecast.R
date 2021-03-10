# script to interpolate CEC's jet demand forecast
# created: october 21, 2020
# author: meas meng

# inputs -----------------

  data_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/'
  cec_file        = '5-20 Jet Fuel Demand.xlsx'
  hist_file       = 'California Transportion Fuel Consumption - Summary 2020-06-01 GDS_rename.xlsx'
  ei_jet          = 5.670   # units: mmbtu/bbl
                            # jet fuel, kerosene type (source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf)
                            # CEC spreadsheet says military jet fuel contains both JP-5 and JP-8 grades, which are supposedly both kerosene-type fuels
  ei_gasoline     = 5.310   # units: mmbtu/bbl
                            # 2018 value for gasoline cons in transportation sector (source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_4.pdf)
  conv_ratio      = ei_gasoline/ei_jet

# outputs ----------------

  save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/fuel-demand/prelim-results'

# libraries ------
  
  library(data.table)
  library(openxlsx)
  library(lspline)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# import data ------
  
  # import cec's forecasted jet fuel demand 
    dt_jet_forecast = setDT(read.xlsx(paste0(data_path, cec_file), sheet = 'Jet Fuel Demand', cols = 1:4))
    colnames(dt_jet_forecast) = c('year', 'Low Case', 'Mid Case', 'High Case')
    
  # import historic military jet demand
    dt_mil_jet = setDT(read.xlsx(paste0(data_path, hist_file), sheet = 'CA Fuel Consumption Data', cols = c(1, 16), rows = c(7, 9:26)))
    setnames(dt_mil_jet, 'Year', 'year')
    setnames(dt_mil_jet, 'Gallons', 'military_jet_fuel_gallons')
    dt_mil_jet = dt_mil_jet[year %in% 2004:2012] # keep years that are reported and not calculated (based on spreadsheet)
    
# melt data to long format ------
  
  jet_long = melt(dt_jet_forecast, id.vars = 'year', variable.name = 'scenario', value.name = 'jet_fuel_demand_gge')
  jet_long[, year := as.numeric(as.character(year))]
  
# list jet scenarios -----  
  
  jet_scenarios = c('Low Case', 'Mid Case', 'High Case')
  
# create empty data.table of all jet fuel scenarios with all years ------
  
  blank_jet = data.table(year = rep(2017:2045, length(jet_scenarios)),
                         scenario = rep(jet_scenarios, each = length(2017:2045)))
  
# create version of data table with existing forecast and empty years to exptrapolate ------
  
  jet_extrap = jet_long[blank_jet, on = c('year', 'scenario')]
  
# use 2021-2030 jet fuel demand to create linear fit -----
  
  reg_jet = lapply(jet_scenarios, function(z){
    return(lm(formula = jet_fuel_demand_gge ~ year, 
              data = jet_long[scenario == z & year %in% 2021:2030] ))
  })
  
  names(reg_jet) = jet_scenarios
  
# use linear fits for each scenario to predict jet fuel demand in 2031-2045 ------
  
  extrapolate_jet = lapply(jet_scenarios, function(z){
    jet_extrap[year %in% c(2031:2045) & scenario == z, jet_fuel_demand_gge := predict(reg_jet[[z]], data.table(year = 2031:2045))]
  })
  
# reorder scenario levels -------
  
  jet_extrap[, scenario := factor(scenario, levels = c('Low Case', 'Mid Case', 'High Case'))]
  
# calculate 5 year average of military jet ------
  
  avg_mil_jet = mean(dt_mil_jet[, military_jet_fuel_gallons], na.rm = T)
  avg_mil_jet_gge = avg_mil_jet*conv_ratio
  
# add military jet to jet fuel demand -------
  
  jet_extrap[, military_jet_fuel_demand_gge := avg_mil_jet_gge]
  jet_extrap[, total_jet_fuel_demand_gge := jet_fuel_demand_gge + military_jet_fuel_demand_gge]
  
# create wide version (of non-military jet demand) ------
  
  jet_extrap_wide = dcast(jet_extrap, year ~ scenario, value.var = 'jet_fuel_demand_gge')
  jet_extrap_wide[, units := 'gge']

# export jet fuel demand to csv -------
  
  fwrite(jet_extrap, file = file.path(save_path, 'cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv'), row.names = F)
  fwrite(jet_extrap_wide, file = file.path(save_path, 'cec_jet_fuel_demand_excl_military_forecasted_2020_2045_wide.csv'), row.names = F)

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
                             axis_text_size = 16,
                             strip_text_size = 16)  +
      theme(plot.title = element_text(hjust = 0, face = 'bold'),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 11, color = '#5c5c5c', face = 'plain'),
            axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.25, "cm"),
            axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
            plot.margin = unit(c(1,2,1,1), "lines"),
            legend.text = element_text(size = 16),
            legend.position = 'bottom')
  
  # line plot (excl. military jet) ---------
    
    fig_jet = ggplot(jet_extrap, aes(x = year, y = jet_fuel_demand_gge/1e6, color = scenario)) + 
      geom_line(size = 1.4) +
      labs(title = 'Jet fuel demand',
           subtitle = 'Million gge',
           caption = 'Excludes military jet fuel',
           x = 'Year',
           y = NULL,
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(3.6e3, 5.2e3, length.out = 5), limits = c(3.6e3, 5.2e3), expand = c(0,0)) +
      geom_segment(x = 2030, xend = 2030, y = 0, yend = 5.2e3, color = 'black', linetype = 2)  +
      scale_color_manual(values = pal_scenarios)+
      theme_line

    ggsave(fig_jet, 
           filename = file.path(save_path, 'cec_jet_fuel_excl_military_demand_forecast_2020_2045.pdf'), 
           width = 11, 
           height = 6.5)
    
    embed_fonts(file.path(save_path, 'cec_jet_fuel_excl_military_demand_forecast_2020_2045.pdf'),
                outfile = file.path(save_path, 'cec_jet_fuel_excl_military_demand_forecast_2020_2045.pdf'))
    
  # line plot (incl. military jet) ---------
    
    fig_jet_total = ggplot(jet_extrap, aes(x = year, y = total_jet_fuel_demand_gge/1e6, color = scenario)) + 
      geom_line(size = 1.4) +
      labs(title = 'Jet fuel demand',
           subtitle = 'Million gge',
           caption = 'Includes military jet fuel',
           x = 'Year',
           y = NULL,
           color = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(3.6e3, 5.2e3, length.out = 5), limits = c(3.6e3, 5.2e3), expand = c(0,0)) +
      geom_segment(x = 2030, xend = 2030, y = 0, yend = 5.2e3, color = 'black', linetype = 2)  +
      scale_color_manual(values = pal_scenarios)+
      theme_line
    
    ggsave(fig_jet_total, 
           filename = file.path(save_path, 'cec_jet_fuel_incl_military_demand_forecast_2020_2045.pdf'), 
           width = 11, 
           height = 6.5)
    
    embed_fonts(file.path(save_path, 'cec_jet_fuel_incl_military_demand_forecast_2020_2045.pdf'),
                outfile = file.path(save_path, 'cec_jet_fuel_incl_military_demand_forecast_2020_2045.pdf'))
