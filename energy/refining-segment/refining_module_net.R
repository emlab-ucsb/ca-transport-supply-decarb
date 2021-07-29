# updated refining module
# created: october 21, 2020
# author: meas meng

# inputs ---------

  proj_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
  its_file        = 'its_demand_bau_and_lc1_2020_2045.csv'
  jet_file        = 'cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv'
  intra_file      = 'its_demand_intrastate_jet_2020_2045.csv'
  fpm_file        = 'finished_product_movements_weekly_cec.csv'
  fw_file         = 'fuel_watch_data.csv'
  ei_file         = 'fuel-energy-intensities.csv'
  cec_file        = 'California Transportion Fuel Consumption - Summary 2020-06-01 GDS_rename.xlsx'
  refcap_file     = 'refinery_loc_cap_manual.csv'
  renref_file     = 'renewable_refinery_capacity.xlsx'
  altair_file     = 'altair_refinery_capacity.xlsx'
  scen_file       = 'input_variables_scenarios_refining.csv'
  ghg_file        = 'refinery_ghg_factor_x_indiv_refinery_revised.csv'
  dac_file        = 'refinery_weighted_unweighted_population_fixed.csv'
  ei_crude        = 5.698               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_3.pdf
  ei_gasoline     = 5.052               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_4.pdf
  ei_diesel       = 5.770               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf
  ei_jet          = (5.670 + 5.355)/2   # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf

# selection ------  

  ref_threshold   = 0.6
  ren_threshold   = 0.9
  pred_years      = 2020:2045
  a               = 4
  drop_in_perc    = 1
  kern_perc       = 0.9375
  ccs_eff         = 0.474
  
# outputs ---------

  save_path   = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/refining_2021-07-29'
  output_path = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  dir.create(file.path(save_path, 'scenarios'), showWarnings = FALSE)
  dir.create(file.path(save_path, 'scenarios_crude'), showWarnings = FALSE)
  dir.create(file.path(save_path, 'scenarios_ghg'), showWarnings = FALSE)
  
# libraries ------

  library(data.table)
  library(lubridate)
  library(openxlsx)
  library(ggplot2)
  library(hrbrthemes)
  library(directlabels)
  library(grid)
  library(extrafont)
  library(unikn)
  
# load all data -----

  # read in its demand forecast
    dt_its = fread(file.path(proj_path, 'outputs/fuel-demand/prelim-results', its_file), header = T)
  
  # read in cec jet demand forecast
    dt_jet = fread(file.path(proj_path, 'outputs/fuel-demand/prelim-results', jet_file), header = T)
    
  # read in intrastate jet fuel demand forecast
    dt_intra = fread(file.path(proj_path, 'outputs/fuel-demand/prelim-results', intra_file), header = T)
    
  # read in fuel movements data
    dt_fpm = fread(file.path(proj_path, 'data/stocks-flows/processed', fpm_file), header = T)
  
  # read in fuel production data
    dt_prod = fread(file.path(proj_path, 'data/stocks-flows/processed', fw_file), header = T)
  
  # read in energy intensities
    dt_ei = fread(file.path(proj_path, 'data/stocks-flows/processed', ei_file), header = T)
    
  # read in historic renewable diesel consumption
    dt_rediesel = setDT(read.xlsx(file.path(proj_path, 'data/stocks-flows/raw', cec_file), sheet = 'CA Fuel Consumption Data', rows = c(8, 16:25), cols = c(1, 13)))
    dt_rediesel[, fuel := 'renewable diesel']
    setnames(dt_rediesel, 'renewable_diesel', 'consumption_gal')
  
  # read in refinery capacities
    dt_refcap = fread(file.path(proj_path, 'data/stocks-flows/processed', refcap_file), header = T)
    dt_refcap = dt_refcap[, c('site_id', 'refinery_name', 'barrels_per_day', 'location', 'county', 'cluster')]
    setnames(dt_refcap, 'cluster', 'region')
    
  # remove asphalt and non-transportation refineries
    dt_refcap = dt_refcap[! refinery_name %in% c('Greka Energy, Santa Maria Refinery', 'Lunday Thagard, South Gate Refinery', 'Valero Wilmington Asphalt Refinery')]
    
  # read in renewable refinery capacities
    dt_renref = setDT(read.xlsx(file.path(proj_path, 'data/stocks-flows/processed', renref_file), sheet = 'Sheet1'))
    renewables_info = dt_renref[, .(site_id, refinery_name, location, region, cluster)]
    dt_renref = dt_renref[, .(installation_year, refinery_name, installation_capacity_bpd, retired_capacity_bpd)]
    
  # read in altair refinery capacity
    dt_altair = setDT(read.xlsx(file.path(proj_path, 'data/stocks-flows/raw', altair_file), sheet = 'Sheet1'))
    dt_altair = dt_altair[, .(year, refinery_name, barrels_per_day, location, county, region)]
    dt_altair[, bbl_per_year := barrels_per_day * 365]
    dt_altair[, gal_per_year := bbl_per_year * 42]
    dt_altair[, bge_per_year := bbl_per_year * (ei_crude/ei_gasoline)]
    dt_altair[, gge_per_year := bge_per_year * 42]
    
  # read in scenarios 
    dt_scen = fread(file.path(proj_path, 'project-materials/scenario-inputs', scen_file), header = T)
  
  # read in refinery and region emission factors
    dt_ghg = fread(file.path(proj_path, 'outputs/stocks-flows', ghg_file), header = T)
    ghg_region = unique(dt_ghg[, .(year, region, region_barrels, region_co2e_kg, region_kgco2e_bbl)])
    ghg_region_2018 = ghg_region[year == 2018]
    
  # read in dac info
    dt_dac = fread(file.path(proj_path, 'model-development/scenario-plot', dac_file), header = T)

# source items ------
  
  # source ccs emissions mean b calculation script
    source(here::here('energy', 'scenario-prep', 'ccs_parameterization.R'))

# plot theme & palettes ------
  
  pal_scenarios = c('BAU' = '#576b81',
                    'Mid Case' = '#576b81',
                    'LC1' = '#50a727',
                    'Low Case' = '#50a727',
                    'High Case' = '#f05c0b',
                    'crude' = '#f6ac0e',
                    'crude (energy intensity equation)' = '#dd1e24',
                    'crude (regression)' = '#305ff0')
  
  pal_region = c('North' = '#3baeba',
                 'South' = '#05426f',
                 'Renewables' = '#ffa440')
  
  pal_fuel = c('gasoline' = '#4bbbe6', 
               'drop-in gasoline' = '#c2eaf7', 
               'diesel' = '#fda89c', 
               'renewable diesel' = '#fddad5', 
               'jet' = '#169486', 
               'sustainable aviation fuel' = '#61c8c1',
               'exports' = '#dadbdf')
  
  pal_crude = c('crude (traditional)' = '#096497',
                'crude (residual renewable)' = '#29b9cd',
                'crude (main renewable)' = '#99c1d5',
                'traditional' = '#096497',
                'residual renewable' = '#29b9cd',
                'main renewable' = '#99c1d5')
  
  pal_refinery = c(seecol(pal_unikn_pair)[1:16], '#ebd74e')
  names(pal_refinery) = unique(c(dt_refcap[, refinery_name], dt_renref[, refinery_name], 'AltAir Paramount'))
  
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
          legend.position = 'bottom',
          strip.text = element_text(hjust = 0.5))
    
    
# ------------------------- historic data prep & analysis ------------------------- 
    
  # get historic fuel production data ------
    
    prod_refined = dt_prod[stock_flow == 'Refinery Production']
    unique(prod_refined[, .(category, sub_cat)])
    
  # recategorize fuels ------
    
    prod_refined[category == 'Motor Gasoline', fuel := 'gasoline']
    prod_refined[category == 'Distillates', fuel := 'diesel']
    prod_refined[category == 'Jet Fuel: Kerosene-Naphtha', fuel := 'jet']
    prod_refined[category == 'Residual', fuel := 'residual']
    
  # adjust production values to remove ethanol from gasoline
    
    prod_refined[, adj_thous_barrels := ifelse(category == 'Motor Gasoline' & sub_cat == 'Reformulated', 0.9 * thous_barrels, thous_barrels)]
    
  # aggregate new adjusted production by region, week, and fuel ------
    
    prod_refined_week = prod_refined[, .(fuel_prod_bbl = sum(adj_thous_barrels, na.rm = T) * 1e3),
                                     by = .(region, date, year, fuel)]
    prod_refined_week[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet', 'residual'))]
    prod_refined_week_wide = dcast(prod_refined_week, region + date + year ~ fuel, value.var = 'fuel_prod_bbl')
    
  # aggregate crude production by region and week ------
    
    crude_input_week = dt_prod[stock_flow == 'Refinery Input', .(crude_bbl = sum(thous_barrels, na.rm = T) * 1e3),
                               by = .(region, date, year)]
    
  # merge weekly crude input and refined product production -------
    
    crude_refined_week = crude_input_week[prod_refined_week_wide, on = .(region, date, year)]
    
    # convert to gge
      crude_refined_week[, crude_gge := crude_bbl * 42 * (ei_crude/ei_gasoline)]
      crude_refined_week[, gasoline_gge := gasoline * 42]
      crude_refined_week[, diesel_gge := diesel * 42 * (ei_diesel/ei_gasoline)]
      crude_refined_week[, jet_gge := jet * 42 * (ei_jet/ei_gasoline)]
      
  # regress crude input as function of refined product input (in units of bbl) -----
    
    reg_crude_refined = lm(crude_bbl ~ gasoline + diesel + jet, data = crude_refined_week)
    reg_crude_refined_gge = lm(crude_gge ~ gasoline_gge + diesel_gge + jet_gge, data = crude_refined_week)
    
  # calculate: 5 year average of crude input and refined product production ------
    
    crude_refined_annual = crude_refined_week[, lapply(.SD, sum, na.rm = T), 
                                              by = c('region', 'year'), 
                                              .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
    
    ave_crude_refined = crude_refined_annual[year %in% 2015:2019, 
                                             lapply(.SD, mean, na.rm = T), 
                                             by = 'region',
                                             .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')] 
    
    ave_crude_refined = melt(ave_crude_refined, 
                             id.vars = 'region', 
                             measure.vars = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual'),
                             variable.name = 'fuel',
                             value.name = 'ave_hist_bbl')
    
    # convert to gallons
      ave_crude_refined[, ave_hist_gal := ave_hist_bbl * 42]
    
    # convert to gge
      ave_crude_refined[fuel == 'crude_bbl', ave_hist_gge := ave_hist_gal * (ei_crude/ei_gasoline)]
      ave_crude_refined[fuel == 'gasoline', ave_hist_gge := ave_hist_gal]
      ave_crude_refined[fuel == 'diesel', ave_hist_gge := ave_hist_gal * (ei_diesel/ei_gasoline)]
      ave_crude_refined[fuel == 'jet', ave_hist_gge := ave_hist_gal * (ei_jet/ei_gasoline)]
      
    # convert to bge
      ave_crude_refined[, ave_hist_bge := ave_hist_gge / 42]
      
    setcolorder(ave_crude_refined, c('region', 'fuel', 'ave_hist_gge', 'ave_hist_bge', 'ave_hist_gal', 'ave_hist_bbl'))

  # calculate: ratio of refined products in bge to crude input in bge -------
    
    # ave_crude_refined_bge = dcast(ave_crude_refined, region ~ fuel, value.var = 'ave_hist_bge')
    ave_crude_refined_bge = ave_crude_refined[fuel %in% c('gasoline', 'diesel', 'jet'), .(region, fuel, ave_hist_bge)]
    setnames(ave_crude_refined_bge, 'ave_hist_bge', 'fuel_bge')
    ave_crude_refined_bge = ave_crude_refined_bge[ave_crude_refined[fuel == 'crude_bbl', .(region, ave_hist_bge)], on = 'region']
    setnames(ave_crude_refined_bge, 'ave_hist_bge', 'crude_bge')
    # ave_crude_refined_bge[, bge_perc := ave_hist_bge/sum(ave_hist_bge), by = 'region']
    ave_crude_refined_bge[, bge_perc := fuel_bge/crude_bge]
    ave_crude_refined_bge = ave_crude_refined_bge[, .(region, fuel, bge_perc)]

  # calculate: CDU based on 5 year average of crude input -------
    
    region_capacity = dt_refcap[, .(barrels_per_day = sum(barrels_per_day)), by = .(region)]
    region_capacity[, barrels_per_year := barrels_per_day * 365]
    ave_region_cdu = region_capacity[ave_crude_refined[fuel == 'crude_bbl'], on = .(region)]
    ave_region_cdu[, ave_hist_cdu := ave_hist_bbl/barrels_per_year]
    
    
  # calculate: ratio between north and south crude consumption from past 5 years -------
    
    region_fuel_ratio = crude_refined_annual[year %in% 2015:2019, 
                                             lapply(.SD, sum, na.rm = T), 
                                             by = 'region',
                                             .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')] 
    region_fuel_ratio = melt(region_fuel_ratio, 
                             id.vars = 'region', 
                             measure.vars = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual'),
                             variable.name = 'fuel',
                             value.name = 'bbl')
    region_fuel_ratio[, ratio := bbl/sum(bbl), by = 'fuel']
    region_fuel_ratio[fuel == 'crude_bbl', fuel := 'crude']
    region_fuel_ratio[, years_used := '2015-2019']
    
    # fwrite(region_fuel_ratio, file.path(proj_path, 'outputs/stocks-flows/crude_and_refined_products_region_proportion.csv'), row.names = F)
    
    region_fuel_ratio[, years_used := NULL]

  # get refined product exports ------
    
    refined_exports_month = dt_fpm[code %like% 'E$']
    refined_exports_month[, year := year(ymd(date))]
    refined_exports_month[location == 'north', region := 'North']
    refined_exports_month[location == 'south', region := 'South']
    
  # get refined product imports ------
    
    refined_imports_month = dt_fpm[code %like% 'I$']
    refined_imports_month[, year := year(ymd(date))]
    refined_imports_month[location == 'north', region := 'North']
    refined_imports_month[location == 'south', region := 'South']
    
  # calculate: 5 year average of refined product exports ------
    
    refined_exports_annual = refined_exports_month[, .(export_bbl = sum(thous_bbl*1e3, na.rm = T)), by = .(region, fuel, year)]
    # refined_exports_annual[, export_bbl := export_gal/42]
    refined_exports_annual[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet'))]
  
    # refined_exports_annual_wide = dcast(refined_exports_annual, region + year ~ fuel, value.var = 'export_bbl')
    # 
    # ave_refined_exports = refined_exports_annual_wide[year %in% 2015:2019, 
    #                                                   lapply(.SD, mean, na.rm = T), 
    #                                                   by = 'region',
    #                                                   .SDcols = c('gasoline', 'diesel', 'jet')] 
    # ave_refined_exports = melt(ave_refined_exports, 
    #                            id.vars = 'region', 
    #                            measure.vars = c('gasoline', 'diesel', 'jet'),
    #                            variable.name = 'fuel',
    #                            value.name = 'export_bbl')
    # ave_refined_exports[, export_gal := export_bbl * 42]
    # 
    # # convert to gge
    #   ave_refined_exports[fuel == 'gasoline', export_gge := export_gal]
    #   ave_refined_exports[fuel == 'diesel', export_gge := export_gal * (ei_diesel/ei_gasoline)]
    #   ave_refined_exports[fuel == 'jet', export_gge := export_gal * (ei_jet/ei_gasoline)]
    #   
    # # convert to bge
    #   ave_refined_exports[, export_bge := export_gge / 42]
    #       
    # setcolorder(ave_refined_exports, c('region', 'fuel', 'export_gge', 'export_bge', 'export_gal', 'export_bbl'))

  # calculate: 5 year average of refined product imports ------
    
    refined_imports_annual = refined_imports_month[, .(import_bbl = sum(thous_bbl*1e3, na.rm = T)), by = .(region, fuel, year)]
    # refined_imports_annual[, import_bbl := import_gal/42]
    refined_imports_annual[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet'))]
    
    refined_imports_annual_wide = dcast(refined_imports_annual, region + year ~ fuel, value.var = 'import_bbl')
    
    ave_refined_imports = refined_imports_annual_wide[year %in% 2015:2019, 
                                                      lapply(.SD, mean, na.rm = T), 
                                                      by = 'region',
                                                      .SDcols = c('gasoline', 'diesel', 'jet')] 
    ave_refined_imports = melt(ave_refined_imports, 
                               id.vars = 'region', 
                               measure.vars = c('gasoline', 'diesel', 'jet'),
                               variable.name = 'fuel',
                               value.name = 'import_bbl')
    ave_refined_imports[, import_gal := import_bbl * 42]
    
    # convert to gge
    ave_refined_imports[fuel == 'gasoline', import_gge := import_gal]
    ave_refined_imports[fuel == 'diesel', import_gge := import_gal * (ei_diesel/ei_gasoline)]
    ave_refined_imports[fuel == 'jet', import_gge := import_gal * (ei_jet/ei_gasoline)]
    
    # convert to bge
    ave_refined_imports[, import_bge := import_gge / 42]
    
    setcolorder(ave_refined_imports, c('region', 'fuel', 'import_gge', 'import_bge', 'import_gal', 'import_bbl'))
    
    # ave_refined_movements = ave_refined_exports[ave_refined_imports, on = .(fuel, region)]
    
  # calculate: 5 year average of net exports -------
    
    refined_movements_annual = refined_exports_annual[refined_imports_annual, on = .(region, fuel, year)]
    refined_movements_annual[, export_bbl := abs(export_bbl)]
    refined_movements_annual = refined_movements_annual[, lapply(.SD, sum, na.rm = T), by = .(fuel, year), .SDcols = c('export_bbl', 'import_bbl') ] 
    refined_movements_annual[, net_export_bbl := export_bbl - import_bbl]
    
    ave_refined_exports = refined_movements_annual[year %in% 2015:2019, lapply(.SD, mean, na.rm = T), by = .(fuel), .SDcols = c('net_export_bbl')]
    
    ave_refined_exports = ave_refined_exports[region_fuel_ratio[, .(region, fuel, ratio)], on = .(fuel), nomatch = 0]
    ave_refined_exports[, export_bbl := net_export_bbl * ratio]
    ave_refined_exports[, net_export_bbl := NULL]
    ave_refined_exports[, ratio := NULL]
    
    # setnames(ave_refined_exports, 'net_export_bbl_2', 'net_export_bbl')
    # refined_movements_annual_wide = dcast(refined_movements_annual, year ~ fuel, value.var = 'net_export_bbl')

    # ave_refined_exports = refined_movements_annual_wide[year %in% 2015:2019, 
    #                                                     lapply(.SD, mean, na.rm = T), 
    #                                                     .SDcols = c('gasoline', 'diesel', 'jet')] 
    # ave_refined_exports = melt(ave_refined_exports, 
    #                            id.vars = 'region', 
    #                            measure.vars = c('gasoline', 'diesel', 'jet'),
    #                            variable.name = 'fuel',
    #                            value.name = 'export_bbl')
    
    
    ave_refined_exports[, export_gal := export_bbl * 42]
    
    # convert to gge
    ave_refined_exports[fuel == 'gasoline', export_gge := export_gal]
    ave_refined_exports[fuel == 'diesel', export_gge := export_gal * (ei_diesel/ei_gasoline)]
    ave_refined_exports[fuel == 'jet', export_gge := export_gal * (ei_jet/ei_gasoline)]
    
    # convert to bge
    ave_refined_exports[, export_bge := export_gge / 42]
    
    setcolorder(ave_refined_exports, c('region', 'fuel', 'export_gge', 'export_bge', 'export_gal', 'export_bbl'))
    
  # calculate: ratio of refinery capacity within respective region ----------
    
    refinery_capacity_ratio = dt_refcap[, .(site_id, refinery_name, barrels_per_day, location, region)]
    refinery_capacity_ratio[, barrels_per_year := barrels_per_day * 365]
    refinery_capacity_ratio[, capacity_ratio := barrels_per_year/sum(barrels_per_year), by = .(region)]
    sum(refinery_capacity_ratio[region == 'South', capacity_ratio])
    setcolorder(refinery_capacity_ratio, c('site_id', 'refinery_name', 'barrels_per_day', 'barrels_per_year', 'location', 'region', 'capacity_ratio'))
    
    refinery_capacity_ratio[, capacity_ratio_within_region := capacity_ratio]
    refinery_capacity_ratio[, capacity_ratio_within_state := barrels_per_year/sum(barrels_per_year)]
    
    # fwrite(refinery_capacity_ratio[, .(site_id, refinery_name, location, region, barrels_per_day, barrels_per_year,
    #                                    capacity_ratio_within_region, capacity_ratio_within_state)], 
    #        file.path(proj_path, 'outputs/stocks-flows/refinery_capacity_ratios.csv'), row.names = F)
    
    
  # use heat content balance to get historical relationship between crude and refined products -------
    
    crude_refined_region = crude_refined_week[, lapply(.SD, sum, na.rm = T), 
                                              by = c('region'), 
                                              .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]

    crude_refined_region[, coef := ( (crude_bbl*ei_crude) - (gasoline*ei_gasoline) - (diesel*ei_diesel) - (jet*ei_jet)  )/crude_bbl]
    
    crude_refined_region_csv = copy(crude_refined_region)
    setnames(crude_refined_region_csv, 'gasoline', 'gasoline_bbl')
    setnames(crude_refined_region_csv, 'diesel', 'diesel_bbl')
    setnames(crude_refined_region_csv, 'jet', 'jet_bbl')
    crude_refined_region_csv[, residual := NULL]
    crude_refined_region_csv[, ei_crude_mmbtu_bbl := ei_crude]
    crude_refined_region_csv[, ei_gasoline_mmbtu_bbl := ei_gasoline]
    crude_refined_region_csv[, ei_diesel_mmbtu_bbl := ei_diesel]
    crude_refined_region_csv[, ei_jet_mmbtu_bbl := ei_jet]
    
    setcolorder(crude_refined_region_csv, c('region', 'crude_bbl', 'gasoline_bbl', 'diesel_bbl', 'jet_bbl', 
                                            'ei_crude_mmbtu_bbl', 'ei_gasoline_mmbtu_bbl', 'ei_diesel_mmbtu_bbl', 'ei_jet_mmbtu_bbl', 'coef'))
    
    # fwrite(crude_refined_region_csv, file.path(proj_path, 'outputs/stocks-flows/crude_and_refined_products__energy_intensities_and_coefficients.csv'), row.names = F)
    
    crude_refined_tot = crude_refined_week[, lapply(.SD, sum, na.rm = T), 
                                              .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
    
    crude_refined_tot[, coef := ( (crude_bbl*ei_crude) - (gasoline*ei_gasoline) - (diesel*ei_diesel) - (jet*ei_jet)  )/crude_bbl]
    
    
  # compare estimated crude to reported crude consumption -------
    
    compare_hist_crude = crude_refined_annual[crude_refined_region[, .(region, coef)], on = .(region)]
    
    compare_hist_crude[, est_crude_bbl := ((gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
    compare_hist_crude_long = melt(compare_hist_crude, 
                                   id.vars = 1:2,
                                   measure.vars = c('crude_bbl', 'est_crude_bbl'),
                                   variable.name = 'type',
                                   value.name = 'bbl')
    compare_hist_crude_long[, type := ifelse(type == 'crude_bbl', 'reported', 'estimated')]
    
    compare_hist_crude_long_tot = compare_hist_crude_long[, .(bbl = sum(bbl, na.rm = T)), by = .(year, type)]
    
    pal_compare = c('reported' = '#46726f',
                    'estimated' = '#e5938c')
    
    fig_crude_compare = ggplot(compare_hist_crude_long_tot[year < 2020], aes(x = year, y = bbl/1e6, color = type)) + geom_line(size = 1) +
      labs(title = 'Total crude consumed (2014-2019)',
           subtitle = 'Million barrels',
           x = NULL,
           y = NULL,
           color = NULL) + 
      # facet_grid(vars(region)) +
      # scale_y_continuous(breaks = seq(0, 700, 10), limits = c(600,700)) + 
      scale_color_manual(values = pal_compare) +
      theme_line
    
    ggsave(fig_crude_compare, 
           filename = file.path(save_path, 'compare_crude_reported_vs_estimated.pdf'), 
           width = 8.5, 
           height = 6)
    
    embed_fonts(file.path(save_path, 'compare_crude_reported_vs_estimated.pdf'),
                outfile = file.path(save_path, 'compare_crude_reported_vs_estimated.pdf'))
    
    

  # get amount of REdiesel produced by Kern Oil refinery -----
    
    # calculate 5 year average of RE diesel consumed in CA
      ave_kern_rediesel = dt_rediesel[year %in% 2015:2019, lapply(.SD, mean, na.rm = T), by = .(fuel), .SDcols = c('consumption_gal')]
      ave_kern_rediesel[, consumption_bbl := consumption_gal / 42]
      ave_kern_rediesel[, consumption_gge := consumption_gal * (ei_diesel/ei_gasoline)]
      ave_kern_rediesel[, consumption_bge := consumption_gge / 42]
      
    # multiply average annual REdiesel consumption with percent that can be processed at Kern Oil
      cols = c('consumption_gge', 'consumption_bge', 'consumption_gal', 'consumption_bbl')
      ave_kern_rediesel[, (cols) := lapply(.SD, function(x) x*kern_perc), .SDcols = cols]

# ------------------------- forecasted demand prep & analysis ------------------------- 
  
  # change jet fuel demand scenario names ------
    
    jet_mid = dt_jet[scenario == 'Mid Case', .(year, scenario, total_jet_fuel_demand_gge)]
    dt_jet_adj = rbindlist(list(copy(jet_mid)[, adj_scenario := 'BAU'],
                                copy(jet_mid)[, adj_scenario := 'LC1']))
    dt_jet_adj[, scenario := NULL]
    dt_jet_adj[, fuel := 'jet']
    setnames(dt_jet_adj, 'adj_scenario', 'scenario')
    setnames(dt_jet_adj, 'total_jet_fuel_demand_gge', 'consumption_gge')
    
  # combine its, jet fuel, and renewable fuels forecasts -------
    
    demand_state = rbindlist(list(dt_its, dt_jet_adj), use.names = T)
    demand_state[, fuel := factor(fuel, c('gasoline', 'diesel', 'jet',
                                          'drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel', 'renewable natural gas', 
                                          'ethanol', 'biodiesel', 'hdv electricity', 'hdv hydrogen', 'ldv electricity', 'ldv hydrogen'))]
    setorder(demand_state, scenario, fuel, year)
    setcolorder(demand_state, c('scenario', 'fuel', 'year', 'consumption_gge'))
    
  # keep only gasoline, diesel, and jet fuel demand ------
    
    demand_state = demand_state[fuel %in% c('gasoline', 'diesel', 'jet', 'drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel')]
    demand_state[, consumption_bge := consumption_gge / 42]
    
  # convert units of demand from gge to bbls -------
    
    demand_state[fuel %like% 'gasoline', consumption_gal := consumption_gge]
    demand_state[fuel %like% 'diesel', consumption_gal := consumption_gge * (ei_gasoline/ei_diesel)]
    demand_state[fuel %like% 'jet' | fuel %like% 'aviation', consumption_gal := consumption_gge * (ei_gasoline/ei_jet)]
    demand_state[, consumption_bbl := consumption_gal/42]
    
    demand_state = demand_state[year >= 2020 & year <= 2045]
    
  # create eqivalent fuel column ------
    
    demand_state[fuel %like% 'gasoline', fuel_equiv := 'gasoline']
    demand_state[fuel %like% 'diesel', fuel_equiv := 'diesel']
    demand_state[fuel %like% 'jet' | fuel %like% 'aviation', fuel_equiv := 'jet']
    
  # split (GJD only) demand by north and south regions using ratio --------
    
    demand_region_gjd = demand_state[region_fuel_ratio[, .(fuel, region, ratio)], on = 'fuel', nomatch = 0, allow.cartesian = T]
    demand_region_gjd[, region_consumption_gge := consumption_gge * ratio]
    demand_region_gjd[, region_consumption_bge := consumption_bge * ratio]
    demand_region_gjd[, region_consumption_gal := consumption_gal * ratio]
    demand_region_gjd[, region_consumption_bbl := consumption_bbl * ratio]
    
  # split demand by north and south regions using ratio -------
    
    # demand_region = demand_state[region_fuel_ratio[, .(fuel, region, ratio)], on = c('fuel_equiv' = 'fuel'), nomatch = 0, allow.cartesian = T]
    # demand_region[, region_consumption_gge := consumption_gge * ratio]
    # demand_region[, region_consumption_gal := consumption_gal * ratio]
    # demand_region[, region_consumption_bbl := consumption_bbl * ratio]
    # 
    # demand_region = demand_region[, .(scenario, fuel, region, year, region_consumption_gge, region_consumption_gal, region_consumption_bbl)] 
    
  # calculate forecasted crude from GJD forecast (excl. imports) using the two methods -------
    
    demand_region_gjd_wide = dcast(demand_region_gjd, scenario + region + year ~ fuel, value.var = 'region_consumption_bbl')
    demand_region_gjd_wide = demand_region_gjd_wide[crude_refined_region[, .(region, coef)], on = .(region)]
    
    demand_region_gjd_wide[, heat_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
    
    demand_region_gjd_wide[region == 'North', 
                           regression_crude_demand_bbl := predict(lm(crude_bbl ~ gasoline + diesel + jet, data = crude_refined_annual[year < 2020 & region == 'North']), 
                                                                  demand_region_gjd_wide[region == 'North'])]
    demand_region_gjd_wide[region == 'South', 
                           regression_crude_demand_bbl := predict(lm(crude_bbl ~ gasoline + diesel + jet, data = crude_refined_annual[year < 2020 & region == 'South']), 
                                                                  demand_region_gjd_wide[region == 'South'])]
    
    crude_demand_compare = melt(demand_region_gjd_wide, 
                                id.vars = c('scenario', 'region', 'year'), 
                                measure.vars = c('gasoline', 'diesel', 'jet', 'heat_crude_demand_bbl', 'regression_crude_demand_bbl'),
                                variable.name = 'fuel',
                                value.name = 'consumption_bbl')
    
    crude_demand_compare[fuel == 'heat_crude_demand_bbl', fuel := 'crude (energy intensity equation)']
    crude_demand_compare[fuel == 'regression_crude_demand_bbl', fuel := 'crude (regression)']
    
    # convert to gge
      crude_demand_compare[fuel == 'gasoline', consumption_gge := consumption_bbl * 42]
      crude_demand_compare[fuel == 'diesel', consumption_gge := consumption_bbl * 42 * (ei_diesel/ei_gasoline)]
      crude_demand_compare[fuel == 'jet', consumption_gge := consumption_bbl * 42 * (ei_jet/ei_gasoline)]
      crude_demand_compare[fuel %like% 'crude', consumption_gge := consumption_bbl * 42 * (ei_crude/ei_gasoline)]
      
    
  # get annual historic crude and refined products in long form -------
    
    crude_refined_annual_long = melt(crude_refined_annual, 
                                     id.vars = c('region', 'year'), 
                                     measure.vars = c('crude_bbl', 'gasoline', 'diesel', 'jet'),
                                     variable.name = 'fuel',
                                     value.name = 'consumption_bbl')
    crude_refined_annual_long[, scenario := 'historic']
    crude_refined_annual_long = crude_refined_annual_long[year < 2020]
    crude_refined_annual_long[fuel == 'crude_bbl', fuel := 'crude']
    
    # convert to gge
      crude_refined_annual_long[fuel == 'gasoline', consumption_gge := consumption_bbl * 42]
      crude_refined_annual_long[fuel == 'diesel', consumption_gge := consumption_bbl * 42 * (ei_diesel/ei_gasoline)]
      crude_refined_annual_long[fuel == 'jet', consumption_gge := consumption_bbl * 42 * (ei_jet/ei_gasoline)]
      crude_refined_annual_long[fuel %like% 'crude', consumption_gge := consumption_bbl * 42 * (ei_crude/ei_gasoline)]
      
  # combine historic and forecasted crude + GJD ------
      
    compare_fuels = rbindlist(list(crude_refined_annual_long, crude_demand_compare), use.names = T)
    setcolorder(compare_fuels, c('scenario', 'region', 'fuel', 'year', 'consumption_bbl', 'consumption_gge'))
    
  # combine GJD into one value ------
    
    compare_fuels[, fuel := as.character(fuel)]
    compare_fuels[, adj_fuel := ifelse(fuel %like% 'crude', fuel, 'GJD')] 
    
    compare_fuels_agg = compare_fuels[, .(consumption_bbl = sum(consumption_bbl, na.rm = T),
                                          consumption_gge = sum(consumption_gge, na.rm = T)),
                                      by = .(scenario, region, adj_fuel, year)]
    
  # plot historic and forecasted crude + GJD to compare prediction methods -------
    
    fig_bau = ggplot() +
      geom_line(data = compare_fuels_agg[! adj_fuel %like% 'crude' & scenario %in% c('historic', 'BAU')], 
                aes(x = year, y = consumption_gge/42/1e6, linetype = 'GJD'), size = 1) +
      geom_line(data = compare_fuels_agg[ adj_fuel %like% 'crude' & scenario %in% c('historic', 'BAU')], 
                aes(x = year, y = (consumption_bbl/1e6), color = adj_fuel), linetype = 1, size = 1) +
      facet_wrap(vars(region), ncol = 1) +
      labs(title = 'Historic production and forecasted demand for crude and GJD (gasoline, jet, and diesel) under BAU scenario',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2015,2045,5), limits = c(2014, 2045), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(0,367,50), limits = c(0,1.08*375), expand = c(0,0),
                         sec.axis = sec_axis( trans=~./1, breaks = seq(0,367,50))) +
      scale_color_manual(values = pal_scenarios) +
      scale_linetype_manual(breaks = 'GJD',
                            values = 2) +
      annotate("text", label = "GJD (million bge)", x = 2015, y = 1.025*370,
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.2) +
      annotate("text", label = "Crude (million bbl)", x = 2044, y = 1.025*370, 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.79) +
      theme_line
    
    fig_bau = ggplotGrob(fig_bau)
    fig_bau$layout$clip[fig_bau$layout$name == "panel"] = "off"
    
    ggsave(fig_bau, 
           filename = file.path(save_path, 'comparison_bau.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'comparison_bau.pdf'),
                outfile = file.path(save_path, 'comparison_bau.pdf'))
    
    fig_lc1 = ggplot() +
      geom_line(data = compare_fuels_agg[! adj_fuel %like% 'crude' & scenario %in% c('historic', 'LC1')], 
                aes(x = year, y = consumption_gge/42/1e6, linetype = 'GJD'), size = 1) +
      geom_line(data = compare_fuels_agg[ adj_fuel %like% 'crude' & scenario %in% c('historic', 'LC1')], 
                aes(x = year, y = (consumption_bbl/1e6), color = adj_fuel), linetype = 1, size = 1) +
      facet_wrap(vars(region), ncol = 1) +
      labs(title = 'Historic production and forecasted demand for crude and GJD (gasoline, jet, and diesel) under LC1 scenario',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2015,2045,5), limits = c(2014, 2045), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(-100,367,50), limits = c(-100,1.08*375), expand = c(0,0),
                         sec.axis = sec_axis( trans=~./1, breaks = seq(-100,367,50))) +
      scale_color_manual(values = pal_scenarios) +
      scale_linetype_manual(breaks = 'GJD',
                            values = 2) +
      annotate("text", label = "GJD (million bge)", x = 2015, y = 1.025*370,
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.2) +
      annotate("text", label = "Crude (million bbl)", x = 2044, y = 1.025*370, 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.79) +
      theme_line
    
    fig_lc1 = ggplotGrob(fig_lc1)
    fig_lc1$layout$clip[fig_lc1$layout$name == "panel"] = "off"
    
    ggsave(fig_lc1, 
           filename = file.path(save_path, 'comparison_lc1.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'comparison_lc1.pdf'),
                outfile = file.path(save_path, 'comparison_lc1.pdf'))
    
  # create time series of exports from 2019 levels to zero in 2045 ------
    
    # create empty data table
      ts_exports = CJ(year = 2019:2045,
                      fuel = c('gasoline', 'diesel', 'jet'))
      # ts_exports = CJ(year = 2019:2045,
      #                 region = c('North', 'South'),
      #                 fuel = c('gasoline', 'diesel', 'jet'))
    
    # merge with 2019 exports
      ts_exports = refined_movements_annual[year == 2019][ts_exports, on = .(year, fuel)]
      
    # set 2045 exports to zero
      ts_exports[year == 2045, net_export_bbl := 0]

    # linearly interpolate within each group
      ts_exports_wide = dcast(ts_exports, year ~ fuel, value.var = 'net_export_bbl')
      setorder(ts_exports_wide, year)
      setcolorder(ts_exports_wide, c('year', 'gasoline', 'diesel', 'jet'))
      
      ts_exports_wide[, gasoline := ((0 - gasoline[year == 2019])/(2045-2019))*(year - 2045)]
      ts_exports_wide[, diesel := ((0 - diesel[year == 2019])/(2045-2019))*(year - 2045)]
      ts_exports_wide[, jet := ((0 - jet[year == 2019])/(2045-2019))*(year - 2045)]
      
    # get long form
      ts_exports = melt(ts_exports_wide, id.vars = c('year'), measure.vars = c('gasoline', 'diesel', 'jet'), 
                        variable.name = 'fuel', value.name = 'export_bbl')
      
    # split by region
      ts_exports = ts_exports[region_fuel_ratio[, .(region, fuel, ratio)], on = .(fuel), nomatch = 0, allow.cartesian = T]
      setorder(ts_exports, fuel, year, region)
      ts_exports[, export_bbl_2 := export_bbl * ratio]
      ts_exports[, export_bbl := NULL]
      ts_exports[, ratio := NULL]
      setnames(ts_exports, 'export_bbl_2', 'export_bbl')
      
    # convert to bge
      ts_exports[fuel == 'gasoline', export_bge := export_bbl]
      ts_exports[fuel == 'diesel', export_bge := export_bbl * (ei_diesel/ei_gasoline)]
      ts_exports[fuel == 'jet', export_bge := export_bbl * (ei_jet/ei_gasoline)]
      
# ------------------------- loop to decide on regional capacity and demand ------------------------- 
    
  # list scenarios and regions ------
    
    dem_scens = c('BAU', 'LC1')
    ref_scens = c('historic exports', 'historic production', 'low exports')
    clus = c('North', 'South')
    
  # main loop  -------
    
    list_equiv_demand = list()
    list_renew_demand = list()
    list_init_cdu = list()
    list_final_cdu = list()
    list_crude_ref = list()  
    list_renewable_ref = list()
    
    for (k in seq_along(ref_scens)) {
      
      print(ref_scens[k])
      
      temp_equiv_scen = list()
      temp_renew_demand_scen = list()
      temp_init_cdu_scen = list()
      temp_final_cdu_scen = list()
      temp_crude_cap_scen = list()
      temp_renew_cap_scen = list()
      
      for (j in seq_along(dem_scens)) {
        
        print(dem_scens[j])
        
        temp_equiv_year = list()
        temp_renew_demand_year = list()
        temp_init_cdu_year = list()
        temp_final_cdu_year = list()
        temp_crude_cap_year = list()
        temp_renew_cap_year = list()
        temp_delayed_cap = list()
        
        for (i in seq_along(pred_years)){
          
          t = pred_years[i]
          print(t)
          
          # get operating refineries in year 
            if (i == 1) {
              crude_cap = copy(dt_refcap)
              ren_cap = copy(dt_renref)[0,] # empty data table with same columns 
              # if year >= 2023, remove Santa Maria 
            } else {
              crude_cap = temp_crude_cap_year[[i-1]][demand_scenario == dem_scens[j] & refining_scenario == ref_scens[k]]
              ren_cap = temp_renew_cap_year[[i-1]][demand_scenario == dem_scens[j] & refining_scenario == ref_scens[k]]
            }
          
            if (i >= 4) {
              crude_cap = crude_cap[!refinery_name == 'Phillips 66, Santa Maria Refinery']
            }
          
          # get refineries planned to retire in year t
            planned_ren_ref = dt_renref[installation_year == t]
              if (i > 1) {
                delayed_ren_ref = temp_delayed_cap[[i-1]]
                planned_ren_ref = rbindlist(list(planned_ren_ref, delayed_ren_ref), use.names = T, fill = T)
              }

          # combine operating and planned installed renewable refineries to get full (planned) renewable refinery in year t
            ren_cap_t = rbindlist(list(ren_cap, planned_ren_ref), use.names = T, fill = T)
            ren_cap_t[, barrels_per_year := installation_capacity_bpd * 365]
            ren_cap_t[, bge_per_year := barrels_per_year * (ei_crude/ei_gasoline)]

          # remove refineries that are converting from crude refineries
            crude_cap_conv = merge(crude_cap, planned_ren_ref[, .(installation_year, refinery_name, installation_capacity_bpd, retired_capacity_bpd)], 
                                   by = 'refinery_name', all.x = T)
            crude_cap_conv[!is.na(retired_capacity_bpd), barrels_per_day := barrels_per_day - retired_capacity_bpd]
            crude_cap_conv = crude_cap_conv[barrels_per_day > 0]
            crude_cap_conv[, c('installation_year', 'installation_capacity_bpd', 'retired_capacity_bpd') := NULL]
            
          # calculate crude refining capacity and renewable refining capacity under planned retirements
            crude_cap_reg = crude_cap_conv[, .(barrels_per_day = sum(barrels_per_day)), by = region]
            crude_cap_reg[, barrels_per_year := barrels_per_day * 365]

          # get GJD demand
            # gjd_dem = product_demand_all[year == t & demand_scenario == dem_scens[j] & refining_scenario == ref_scens[k]]
            gjd_dem = demand_state[fuel %in% c('gasoline', 'diesel', 'jet') & year == t & scenario == dem_scens[j]]
            setnames(gjd_dem, 'scenario', 'demand_scenario')
            
          # split GJD demand by region
            gjd_dem_2 = gjd_dem[region_fuel_ratio[, .(region, fuel, ratio)], 
                                on = c('fuel_equiv' = 'fuel'), nomatch = 0]
            cols = c('consumption_gge', 'consumption_bge', 'consumption_gal', 'consumption_bbl')
            gjd_dem_2[, (cols) := lapply(.SD, function(x) x*ratio), .SDcols = cols]

            gjd_dem_reg = gjd_dem_2[, .(demand_scenario, fuel, fuel_equiv, year, region, 
                                        consumption_gge, consumption_bge, consumption_gal, consumption_bbl )]
            
            
          # get reGJD demand
            re_gjd_dem = demand_state[fuel %in% c('drop-in gasoline', 'renewable diesel', 'sustainable aviation fuel') & year == t & scenario == dem_scens[j]]
            setnames(re_gjd_dem, 'scenario', 'demand_scenario')
            
          # limit drop-in gasoline demand to limit (if exceeded)
            
            re_gjd_dem[, tot_bge := sum(consumption_bge)]
            re_gjd_dem[, perc_bge := consumption_bge/sum(consumption_bge)]
            re_gjd_dem[, rem_bge := tot_bge - re_gjd_dem[fuel == 'drop-in gasoline', consumption_bge]]
            
            re_gjd_dem[fuel == 'drop-in gasoline', consumption_bge := ifelse(perc_bge > drop_in_perc, 
                                                                             (drop_in_perc * rem_bge) / (1 - drop_in_perc), 
                                                                             consumption_bge)]
            re_gjd_dem[fuel == 'drop-in gasoline', consumption_gge := consumption_bge * 42]
            re_gjd_dem[fuel == 'drop-in gasoline', consumption_gal := consumption_gge]
            re_gjd_dem[fuel == 'drop-in gasoline', consumption_bbl := consumption_bge]
            
            re_gjd_dem[, c('tot_bge', 'perc_bge', 'rem_bge') := NULL]
            
          # compare total renewable gasoline, renewable jet, and renewable diesel (reGJD) demand to renewable refining capacity
            sum_re_gjd_dem = sum(re_gjd_dem[, consumption_bge]) # in units of bge
            
          # calculate renewable refinery capacity
            if (nrow(ren_cap_t) > 0) {
              ren_cap_tot = sum(ren_cap_t[, bge_per_year])*ren_threshold
            } else {
              ren_cap_tot = 0
            }
            
          # if kern oil refinery exists, adjust rediesel demand by subtraction kern oil's rediesel processing capacity
            
            if (nrow(crude_cap_conv[refinery_name %like% 'Kern Oil']) == 1) {
              # sum_re_gjd_dem_adj = sum_re_gjd_dem - ave_kern_rediesel[, consumption_bge]
              re_gjd_dem_adj = copy(re_gjd_dem)
              re_gjd_dem_adj[fuel == 'renewable diesel', consumption_gge := consumption_gge - ave_kern_rediesel[, consumption_gge]]
              re_gjd_dem_adj[fuel == 'renewable diesel', consumption_bge := consumption_bge - ave_kern_rediesel[, consumption_bge]]
              re_gjd_dem_adj[fuel == 'renewable diesel', consumption_gal := consumption_gal - ave_kern_rediesel[, consumption_gal]]
              re_gjd_dem_adj[fuel == 'renewable diesel', consumption_bbl := consumption_bbl - ave_kern_rediesel[, consumption_bbl]]
              sum_re_gjd_dem_adj = sum(re_gjd_dem_adj[, consumption_bge])
            } else {
              re_gjd_dem_adj = copy(re_gjd_dem)
              sum_re_gjd_dem_adj = copy(sum_re_gjd_dem)
            }
            
          # assign renewable demand to altair paramount (if 2020, use initial capacity. if 2021+, use planned expanded capacity)
            
            # if (i == 1) {
            #   altair_cap = dt_altair[year == 2020]
            #   
            #     if (altair_cap[, bge_per_year] < sum_re_gjd_dem_adj) {
            #       
            #       re_gjd_dem_adj[, c('consumption_bbl', 'consumption_gal', 'consumption_bge') := NULL]
            #       re_gjd_dem_adj[, consumption_gge2 := consumption_gge - ((1/3)*(altair_cap[, gge_per_year]))]
            #       re_gjd_dem_adj[, sign := ifelse(consumption_gge2 < 0, 'negative', 'positive')]
            #       re_gjd_dem_adj[, remaining := (1/nrow(re_gjd_dem_adj[sign == 'positive']))*(altair_cap[, gge_per_year] - sum(re_gjd_dem_adj[sign == 'negative', consumption_gge]))]
            #       
            #       re_gjd_dem_adj[, consumption_gge3 := ifelse(sign == 'negative', 0, consumption_gge - remaining)]
            #       
            #       re_gjd_dem_adj[, c('consumption_gge', 'consumption_gge2', 'sign', 'remaining') := NULL]
            #       setnames(re_gjd_dem_adj, 'consumption_gge3', 'consumption_gge')
            #       
            #       re_gjd_dem_adj[, consumption_bge := consumption_gge / 42]
            #       
            #       # re_gjd_dem_adj[, c('consumption_bbl', 'consumption_gal') := NULL]
            #       # re_gjd_dem_adj[, consumption_gge2 := consumption_gge - ((1/3)*(altair_cap[, gge_per_year]))]
            #       # re_gjd_dem_adj[, consumption_bge2 := consumption_bge - ((1/3)*(altair_cap[, bge_per_year]))]
            #       
            #     } else {
            #       
            #       re_gjd_dem_adj[, c('consumption_bbl', 'consumption_gal') := NULL]
            #       re_gjd_dem_adj[, consumption_gge := 0]
            #       re_gjd_dem_adj[, consumption_bge := 0]
            # 
            #     }
            # 
            # } else {
            #   altair_cap = dt_altair[year == 2021]
            #   
            #   if (altair_cap[, bge_per_year] < sum_re_gjd_dem_adj) { 
            #     
            #     re_gjd_dem_adj[, c('consumption_bbl', 'consumption_gal', 'consumption_bge') := NULL]
            #     re_gjd_dem_adj[, consumption_gge2 := consumption_gge - ((1/3)*(altair_cap[, gge_per_year]))]
            #     re_gjd_dem_adj[, sign := ifelse(consumption_gge2 < 0, 'negative', 'positive')]
            #     re_gjd_dem_adj[, remaining := (1/nrow(re_gjd_dem_adj[sign == 'positive']))*(altair_cap[, gge_per_year] - sum(re_gjd_dem_adj[sign == 'negative', consumption_gge]))]
            #     
            #     re_gjd_dem_adj[, consumption_gge3 := ifelse(sign == 'negative', 0, consumption_gge - remaining)]
            #     
            #     re_gjd_dem_adj[, c('consumption_gge', 'consumption_gge2', 'sign', 'remaining') := NULL]
            #     setnames(re_gjd_dem_adj, 'consumption_gge3', 'consumption_gge')
            #     
            #     re_gjd_dem_adj[, consumption_bge := consumption_gge / 42]
            #     
            #   } else {
            #       
            #     re_gjd_dem_adj[, c('consumption_bbl', 'consumption_gal') := NULL]
            #     re_gjd_dem_adj[, consumption_gge := 0]
            #     re_gjd_dem_adj[, consumption_bge := 0]
            #     
            #     }
            # 
            # }
            # 
            # re_gjd_dem_adj[fuel_equiv == 'gasoline', consumption_gal := consumption_gge]
            # re_gjd_dem_adj[fuel_equiv == 'diesel', consumption_gal := consumption_gge * (ei_gasoline/ei_diesel)]
            # re_gjd_dem_adj[fuel_equiv == 'jet', consumption_gal := consumption_gge * (ei_gasoline/ei_jet)]
            # 
            # re_gjd_dem_adj[, consumption_bbl := consumption_gal / 42]
            # 
            # re_gjd_dem_adj = re_gjd_dem_adj[, .(demand_scenario, fuel, year, consumption_gge, consumption_bge, consumption_gal, consumption_bbl, fuel_equiv)]
            # 
            # 
            # sum_re_gjd_dem_adj = sum(re_gjd_dem_adj[, consumption_bge])
            

          # check if residual renewable demand is positive or negative
          
            residual_ren = sum_re_gjd_dem_adj - ren_cap_tot
          
            if (residual_ren > 0) { #  if residual renewable demand is positive 

              # calculate percentage of total renewable demand that is made up of residual renewable demand
              
                # residual_perc = ifelse(ren_cap_tot == 0, residual_ren/sum_re_gjd_dem_adj, residual_ren/sum_re_gjd_dem)
                  
              # calculate demand that can be met by renewables refineries
                
                ren_ref_demand = copy(re_gjd_dem_adj)
                ren_ref_demand[, demand_perc := ren_cap_tot/sum_re_gjd_dem_adj]
                ren_ref_demand[, renewable_refinery_consumption_bge := consumption_bge * demand_perc]
                setnames(ren_ref_demand, 'consumption_gge', 'adj_consumption_gge')
                setnames(ren_ref_demand, 'consumption_bge', 'adj_consumption_bge')
                setnames(ren_ref_demand, 'consumption_gal', 'adj_consumption_gal')
                setnames(ren_ref_demand, 'consumption_bbl', 'adj_consumption_bbl')
                
                ren_ref_demand = ren_ref_demand[, .(demand_scenario, fuel, fuel_equiv, year, adj_consumption_bge, demand_perc, renewable_refinery_consumption_bge)]
                ren_ref_demand[fuel_equiv == 'gasoline', renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline/ei_gasoline)]
                ren_ref_demand[fuel_equiv == 'diesel', renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline/ei_diesel)]
                ren_ref_demand[fuel_equiv == 'jet', renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline/ei_jet)]
                
              # recalculate how much reGJD needs to be processed at crude refineries
                
                re_gjd_dem_2 = merge(re_gjd_dem,
                                     ren_ref_demand[, .(fuel, 
                                                        renewable_refinery_consumption_bge)],
                                     by = 'fuel')
                re_gjd_dem_2[, residual_consumption_bge := consumption_bge - renewable_refinery_consumption_bge]

              # split residual demand by ratio

                re_gjd_dem_2 = re_gjd_dem_2[region_fuel_ratio[, .(region, fuel, ratio)], 
                                          on = c('fuel_equiv' = 'fuel'), nomatch = 0]
                re_gjd_dem_2[, residual_consumption_bge := residual_consumption_bge * ratio]

                re_gjd_dem_reg = re_gjd_dem_2[, .(demand_scenario, fuel, fuel_equiv, year, region, residual_consumption_bge)]
                re_gjd_dem_reg[fuel_equiv == 'gasoline', residual_consumption_bbl := residual_consumption_bge * (ei_gasoline/ei_gasoline)]
                re_gjd_dem_reg[fuel_equiv == 'diesel', residual_consumption_bbl := residual_consumption_bge * (ei_gasoline/ei_diesel)]
                re_gjd_dem_reg[fuel_equiv == 'jet', residual_consumption_bbl := residual_consumption_bge * (ei_gasoline/ei_jet)]
                
              
              # calculate equivalent demand of gasoline, diesel, and jet
                equiv_demand = merge(gjd_dem_reg[, .(demand_scenario, fuel_equiv, year, region, 
                                                     consumption_bge, consumption_bbl)],
                                     re_gjd_dem_reg[, .(demand_scenario, fuel_equiv, year, region,
                                                        residual_consumption_bge, residual_consumption_bbl)],
                                     by = c('demand_scenario', 'fuel_equiv', 'region', 'year'))
                equiv_demand = equiv_demand[, lapply(.SD, sum, na.rm = T), by = c('demand_scenario', 'fuel_equiv', 'region', 'year')] 
                equiv_demand[, equiv_consumption_bge := consumption_bge + residual_consumption_bge]
                equiv_demand[, equiv_consumption_bbl := consumption_bbl + residual_consumption_bbl]
                
              # assign demand based on historic production or historic exports scenario
                
                if (ref_scens[k] == 'historic production') {
                  
                # use heat intensity equation method to solve for equiv crude demand
                  crude_equiv = dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = 'equiv_consumption_bbl')
                  crude_equiv = crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
                  crude_equiv[, equiv_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
                  
                # calculate CDU
                  crude_cdu = crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, equiv_crude_demand_bbl)], on = 'region']
                  crude_cdu[, cdu := equiv_crude_demand_bbl/barrels_per_year]
                  
                # set cdu at historic cdu / production
                  crude_cdu = crude_cdu[ave_region_cdu[, .(region, ave_hist_cdu)], on = .(region)]
                  crude_cdu[, total_crude_demand_bbl := ave_hist_cdu * barrels_per_year]
                  crude_cdu[, cdu := NULL]
                  setnames(crude_cdu, 'ave_hist_cdu', 'cdu')
                  
                # calculate exports
                  crude_cdu[, export_crude_bbl := total_crude_demand_bbl - equiv_crude_demand_bbl]
                  crude_cdu[, export_crude_bge := export_crude_bbl * (ei_crude/ei_gasoline)]
                  temp_exports = ave_crude_refined_bge[crude_cdu[, .(region, export_crude_bge)], on = .(region)]
                  temp_exports[, export_bge := export_crude_bge * bge_perc]
                  temp_exports[fuel == 'gasoline', export_bbl := export_bge]
                  temp_exports[fuel == 'diesel', export_bbl := export_bge * (ei_gasoline/ei_diesel)]
                  temp_exports[fuel == 'jet', export_bbl := export_bge * (ei_jet/ei_diesel)]
                  
                # add exports to demand
                  equiv_demand = equiv_demand[temp_exports[, .(region, fuel, export_bge, export_bbl)], on = c('fuel_equiv' = 'fuel', 'region')]
                  equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
                  equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]
                  

                crude_cdu = crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
                  

                } 
                
                if (ref_scens[k] == 'historic exports') {
                  
                  # merge with historic exports 
                    equiv_demand = equiv_demand[ave_refined_exports[, .(region, fuel, export_bge, export_bbl)], on = c('fuel_equiv' = 'fuel', 'region')]
                    equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
                    equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]
                    
                  # use heat intensity equation method to solve for equiv crude demand
                    crude_equiv = dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = 'total_consumption_bbl')
                    crude_equiv = crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
                    crude_equiv[, total_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
                    
                  # calculate CDU
                    crude_cdu = crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = 'region']
                    crude_cdu[, cdu := total_crude_demand_bbl/barrels_per_year]
                    
                    crude_cdu = crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]

                }
                
                if (ref_scens[k] == 'low exports') {
                  
                  # merge with forecasted declining exports 
                  equiv_demand = equiv_demand[ts_exports, on = c('fuel_equiv' = 'fuel', 'region', 'year'), nomatch = 0]
                  equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
                  equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]
                  
                  # use heat intensity equation method to solve for equiv crude demand
                  crude_equiv = dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = 'total_consumption_bbl')
                  crude_equiv = crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
                  crude_equiv[, total_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
                  
                  # calculate CDU
                  crude_cdu = crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = 'region']
                  crude_cdu[, cdu := total_crude_demand_bbl/barrels_per_year]
                  
                  crude_cdu = crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
                  
                  
                }


            } else { # if residual demand < 0
              
              # calculate demand that can be met by renewables refineries
              
                ren_ref_demand = copy(re_gjd_dem_adj)
                ren_ref_demand[, demand_perc := 1]
                ren_ref_demand[, renewable_refinery_consumption_bge := consumption_bge * demand_perc]
                setnames(ren_ref_demand, 'consumption_gge', 'adj_consumption_gge')
                setnames(ren_ref_demand, 'consumption_bge', 'adj_consumption_bge')
                setnames(ren_ref_demand, 'consumption_gal', 'adj_consumption_gal')
                setnames(ren_ref_demand, 'consumption_bbl', 'adj_consumption_bbl')
                
                ren_ref_demand = ren_ref_demand[, .(demand_scenario, fuel, fuel_equiv, year, adj_consumption_bge, demand_perc, renewable_refinery_consumption_bge)]
                ren_ref_demand[fuel_equiv == 'gasoline', renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline/ei_gasoline)]
                ren_ref_demand[fuel_equiv == 'diesel', renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline/ei_diesel)]
                ren_ref_demand[fuel_equiv == 'jet', renewable_refinery_consumption_bbl := renewable_refinery_consumption_bge * (ei_gasoline/ei_jet)]
                
              # recalculate how much reGJD needs to be processed at crude refineries
              
                re_gjd_dem_2 = merge(re_gjd_dem,
                                     ren_ref_demand[, .(fuel, renewable_refinery_consumption_bge)],
                                     by = 'fuel')
                re_gjd_dem_2[, residual_consumption_bge := consumption_bge - renewable_refinery_consumption_bge]
                
                # assign residual demand to North (Kern Oil)
                
                re_gjd_dem_2[, region := 'North']
                
                re_gjd_dem_reg = re_gjd_dem_2[, .(demand_scenario, fuel, fuel_equiv, year, region, residual_consumption_bge)]
                re_gjd_dem_reg[fuel_equiv == 'gasoline', residual_consumption_bbl := residual_consumption_bge * (ei_gasoline/ei_gasoline)]
                re_gjd_dem_reg[fuel_equiv == 'diesel', residual_consumption_bbl := residual_consumption_bge * (ei_gasoline/ei_diesel)]
                re_gjd_dem_reg[fuel_equiv == 'jet', residual_consumption_bbl := residual_consumption_bge * (ei_gasoline/ei_jet)]
                
              # if residual renewable demand is less than zero, then equivalent crude demand is just crude demand from GJD + residual reGJD at kern oil
                equiv_demand = merge(gjd_dem_reg[, .(demand_scenario, fuel_equiv, year, region, 
                                                     consumption_bge, consumption_bbl)],
                                     re_gjd_dem_reg[, .(demand_scenario, fuel_equiv, year, region,
                                                        residual_consumption_bge, residual_consumption_bbl)],
                                     by = c('demand_scenario', 'fuel_equiv', 'region', 'year'),
                                     all = T)
                equiv_demand = equiv_demand[, lapply(.SD, sum, na.rm = T), by = c('demand_scenario', 'fuel_equiv', 'region', 'year')] 
                equiv_demand[, equiv_consumption_bge := consumption_bge + residual_consumption_bge]
                equiv_demand[, equiv_consumption_bbl := consumption_bbl + residual_consumption_bbl]
                
              if (ref_scens[k] == 'historic production') {
                

              # use heat intensity equation method to solve for equiv crude demand
                crude_equiv = dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = 'equiv_consumption_bbl')
                crude_equiv = crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
                crude_equiv[, equiv_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
                
              # calculate CDU
                crude_cdu = crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, equiv_crude_demand_bbl)], on = 'region']
                crude_cdu[, cdu := equiv_crude_demand_bbl/barrels_per_year]
                
              # set cdu at historic cdu / production
                crude_cdu = crude_cdu[ave_region_cdu[, .(region, ave_hist_cdu)], on = .(region)]
                crude_cdu[, total_crude_demand_bbl := ave_hist_cdu * barrels_per_year]
                crude_cdu[, cdu := NULL]
                setnames(crude_cdu, 'ave_hist_cdu', 'cdu')
                
              # calculate exports
                crude_cdu[, export_crude_bbl := total_crude_demand_bbl - equiv_crude_demand_bbl]
                crude_cdu[, export_crude_bge := export_crude_bbl * (ei_crude/ei_gasoline)]
                temp_exports = ave_crude_refined_bge[crude_cdu[, .(region, export_crude_bge)], on = .(region)]
                temp_exports[, export_bge := export_crude_bge * bge_perc]
                temp_exports[fuel == 'gasoline', export_bbl := export_bge]
                temp_exports[fuel == 'diesel', export_bbl := export_bge * (ei_gasoline/ei_diesel)]
                temp_exports[fuel == 'jet', export_bbl := export_bge * (ei_jet/ei_diesel)]
                
              # add exports to demand
                equiv_demand = equiv_demand[temp_exports[, .(region, fuel, export_bge, export_bbl)], on = c('fuel_equiv' = 'fuel', 'region')]
                
                equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
                equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]
                
              crude_cdu = crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
                
                
              } 
                
              if (ref_scens[k] == 'historic exports') {
                
                # merge with historic exports 
                  equiv_demand = equiv_demand[ave_refined_exports[, .(region, fuel, export_bge, export_bbl)], on = c('fuel_equiv' = 'fuel', 'region')]
                  equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
                  equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]
                
                # use heat intensity equation method to solve for equiv crude demand
                  crude_equiv = dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = 'total_consumption_bbl')
                  crude_equiv = crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
                  crude_equiv[, total_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
                
                # calculate CDU
                  crude_cdu = crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = 'region']
                  crude_cdu[, cdu := total_crude_demand_bbl/barrels_per_year]
                
                  crude_cdu = crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]

                }
              
              if (ref_scens[k] == 'low exports') {
                  
                # merge with forecasted declining exports 
                  equiv_demand = equiv_demand[ts_exports, on = c('fuel_equiv' = 'fuel', 'region', 'year'), nomatch = 0]
                  equiv_demand[, total_consumption_bge := equiv_consumption_bge + abs(export_bge)]
                  equiv_demand[, total_consumption_bbl := equiv_consumption_bbl + abs(export_bbl)]
                  
                # use heat intensity equation method to solve for equiv crude demand
                  crude_equiv = dcast(equiv_demand, demand_scenario + region + year ~ fuel_equiv, value.var = 'total_consumption_bbl')
                  crude_equiv = crude_equiv[crude_refined_region[, .(region, coef)], on = .(region)]
                  crude_equiv[, total_crude_demand_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
                  
                # calculate CDU
                  crude_cdu = crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, gasoline, diesel, jet, total_crude_demand_bbl)], on = 'region']
                  crude_cdu[, cdu := total_crude_demand_bbl/barrels_per_year]
                  
                  crude_cdu = crude_cdu[, .(demand_scenario, region, total_crude_demand_bbl, cdu)]
                  
                  
                }
                
            }
            
            # save equivalent demand at crude refineries
              equiv_demand[, year := t]
              equiv_demand[, demand_scenario := dem_scens[j]]
              equiv_demand[, refining_scenario := ref_scens[k]]
              temp_equiv_year[[i]] = equiv_demand
              
            # save renewables demand at renewables refineries
              ren_ref_demand[, year := t]
              ren_ref_demand[, demand_scenario := dem_scens[j]]
              ren_ref_demand[, refining_scenario := ref_scens[k]]
              temp_renew_demand_year[[i]] = ren_ref_demand
            
            # save initial cdu
              crude_cdu[, year := t]
              crude_cdu[, demand_scenario := dem_scens[j]]
              crude_cdu[, refining_scenario := ref_scens[k]]
              temp_init_cdu_year[[i]] = crude_cdu
              
            # separate CDUs by region and either revert some capacities or retire additional capacity 
              
              ren_cap_t = ren_cap_t[, .(installation_year, refinery_name, installation_capacity_bpd,
                                        retired_capacity_bpd, barrels_per_year, bge_per_year)][renewables_info, on = .(refinery_name), nomatch = 0]
              planned_ren_ref = planned_ren_ref[, .(installation_year, refinery_name, installation_capacity_bpd,
                                                    retired_capacity_bpd)][renewables_info, on = .(refinery_name), nomatch = 0]
              
              temp_crude_reg_cap = list()
              temp_renew_reg_cap = list()
              temp_cdu_reg = list()
              temp_del_ren_ref = list()

              for (r in seq_along(clus)) {
                cur_reg = clus[r]
                cur_cdu = crude_cdu[region == cur_reg]
                temp_crude_ref = crude_cap_conv[region == cur_reg]
                temp_ren_ref = ren_cap_t[region == cur_reg]
                temp_planned = planned_ren_ref[region == cur_reg]
                
                if (cur_cdu[, cdu] > 1) { # if CDU > 1, do not retire crude capacities planned for retirement
                  temp_crude_reg_cap[[r]] = copy(crude_cap[region == cur_reg])
                  temp_planned[, installation_year := t+1]
                  temp_del_ren_ref[[r]] = temp_planned
                  temp_renew_reg_cap[[r]] = temp_ren_ref[!temp_planned, on = 'refinery_name']
                } else {
                  if (cur_cdu[, cdu] > ref_threshold) { # if CDU < 1 and CDU > threshhold, then don't do anything
                    temp_crude_reg_cap[[r]] = copy(crude_cap_conv[region == cur_reg])
                    temp_renew_reg_cap[[r]] = copy(temp_ren_ref)
                    temp_del_ren_ref[[r]] = planned_ren_ref[0,]
                  } else { # if CDU < 1 and CDU < threshhold, then retire smallest crude facilities until CDU > threshold
                    while ( cur_cdu[, cdu] < ref_threshold ) {
                      
                      if (nrow(temp_crude_ref) == 1 ) {
                        temp_crude_reg_cap[[r]] = temp_crude_ref
                        temp_renew_reg_cap[[r]] = copy(ren_cap_t)
                        temp_del_ren_ref[[r]] = planned_ren_ref[0,]
                        break
                      }
                      
                      minref = temp_crude_ref[, .SD[which.min(barrels_per_day)], by = region]

                      temp_crude_ref = temp_crude_ref[! refinery_name == minref[, refinery_name ]]
                      temp_crude_cap_reg = temp_crude_ref[, .(barrels_per_day = sum(barrels_per_day)), by = region]
                      temp_crude_cap_reg[, barrels_per_year := barrels_per_day * 365]
                      cur_cdu = temp_crude_cap_reg[crude_equiv[, .(demand_scenario, region, year, total_crude_demand_bbl)], on = 'region', nomatch = 0]
                      cur_cdu[, cdu := total_crude_demand_bbl/barrels_per_year]
                      
                      if (cur_cdu[, cdu] >= 1 ) {
                        temp_crude_ref = rbindlist(list(temp_crude_ref, minref), use.names = T, fill = T)
                      }
                      
                      # temp_crude_ref[, year := t]
                      temp_crude_reg_cap[[r]] = temp_crude_ref
                      temp_renew_reg_cap[[r]] = copy(ren_cap_t)
                      temp_del_ren_ref[[r]] = planned_ren_ref[0,]
                      # tempclus = tempref[, .(gallons_per_day = sum(barrels_per_day)*42) ]

                      # temp = tempdat[i]
                      # temp = temp[tempclus, on = c('region' = 'cluster'), nomatch = 0]
                      # temp[, no_of_days := ifelse(leap_year(year) == TRUE, 366, 365)]
                      # temp[, annual_capacity := gallons_per_day*no_of_days]
                      # temp[, cdu := crude/annual_capacity]

                      # temp_reg_cap[[r]] = tempref

                      # temp_reg_cap[[r]] = copy(crude_cap_conv[region == cur_reg])
                    } 
                  }
                  
                  temp_cdu_reg[[r]] = cur_cdu 

                }
                
                
              }
              
            # save final cdu 
              
              dt_cdu_reg = rbindlist(temp_cdu_reg, use.names = T, fill = T)
              dt_cdu_reg[, year := t]
              dt_cdu_reg[, demand_scenario := dem_scens[j]]
              dt_cdu_reg[, refining_scenario := ref_scens[k]]
              temp_final_cdu_year[[i]] = dt_cdu_reg

              dt_temp_crude_cap = rbindlist(temp_crude_reg_cap, use.names = T, fill = T)
              dt_temp_crude_cap[, year := t]
              dt_temp_crude_cap[, demand_scenario := dem_scens[j]]
              dt_temp_crude_cap[, refining_scenario := ref_scens[k]]
              
              dt_temp_renew_cap = rbindlist(temp_renew_reg_cap, use.names = T, fill = T)
              dt_temp_renew_cap = unique(dt_temp_renew_cap)
              dt_temp_renew_cap[, year := t]
              dt_temp_renew_cap[, demand_scenario := dem_scens[j]]
              dt_temp_renew_cap[, refining_scenario := ref_scens[k]]
              
              dt_temp_del_ren_ref = rbindlist(temp_del_ren_ref, use.names = T, fill = T)
              dt_temp_del_ren_ref = unique(dt_temp_del_ren_ref)
              dt_temp_del_ren_ref[, year := t]
              dt_temp_del_ren_ref[, demand_scenario := dem_scens[j]]
              dt_temp_del_ren_ref[, refining_scenario := ref_scens[k]]
              
              
          temp_crude_cap_year[[i]] = dt_temp_crude_cap
          temp_renew_cap_year[[i]] = dt_temp_renew_cap
          temp_delayed_cap[[i]] = dt_temp_del_ren_ref


        }
        
        temp_equiv_scen[[j]] = rbindlist(temp_equiv_year, use.names = T, fill = T)
        temp_renew_demand_scen[[j]] = rbindlist(temp_renew_demand_year, use.names = T, fill = T)
        temp_init_cdu_scen[[j]] = rbindlist(temp_init_cdu_year, use.names = T, fill = T)
        temp_final_cdu_scen[[j]] = rbindlist(temp_final_cdu_year, use.names = T, fill = T)
        temp_crude_cap_scen[[j]] = rbindlist(temp_crude_cap_year, use.names = T, fill = T)
        temp_renew_cap_scen[[j]] = rbindlist(temp_renew_cap_year, use.names = T, fill = T)

      }
      
      list_equiv_demand[[k]] = rbindlist(temp_equiv_scen, use.names = T, fill = T)
      list_renew_demand[[k]] = rbindlist(temp_renew_demand_scen, use.names = T, fill = T)
      list_init_cdu[[k]] = rbindlist(temp_init_cdu_scen, use.names = T, fill = T)
      list_final_cdu[[k]] = rbindlist(temp_final_cdu_scen, use.names = T, fill = T)
      list_crude_ref[[k]] = rbindlist(temp_crude_cap_scen, use.names = T, fill = T)
      list_renewable_ref[[k]] = rbindlist(temp_renew_cap_scen, use.names = T, fill = T)
      

    }
    
    rm(crude_cap, ren_cap, planned_ren_ref, ren_cap_t, crude_cap_conv, crude_cap_reg, gjd_dem, re_gjd_dem, sum_re_gjd_dem, ren_cap_tot, 
       residual_ren, crude_equiv, crude_cdu, temp_crude_reg_cap, temp_renew_reg_cap, cur_reg, cur_cdu, temp_crude_ref, minref, 
       dt_temp_crude_cap, dt_temp_renew_cap, dt_cdu_reg)
    
    
  # create data tables of results --------
    
    res_equiv_demand = rbindlist(list_equiv_demand, use.names = T, fill = T)
    res_renew_demand = rbindlist(list_renew_demand, use.names = T, fill = T)
    res_final_cdu = rbindlist(list_final_cdu, use.names = T, fill = T)
    res_crude_ref_reg = rbindlist(list_crude_ref, use.names = T, fill = T)
    res_renew_ref_reg = rbindlist(list_renewable_ref, use.names = T, fill = T)
    
  # create time series of Alt Air paramount capacity installed across all scenarios --------
    
    # for 2020, use smaller capacity
    
      altair_2020 = CJ(installation_year = c(2020),
                       refinery_name = dt_altair[year == 2020, refinery_name],
                       installation_capacity_bpd = dt_altair[year == 2020, barrels_per_day], 
                       retired_capacity_bpd = 0,
                       barrels_per_year = dt_altair[year == 2020, bbl_per_year], 
                       bge_per_year = dt_altair[year == 2020, bge_per_year],
                       site_id = 't-800',
                       location = dt_altair[year == 2020, location],
                       region = dt_altair[year == 2020, region],
                       cluster = 'South',
                       year = 2020, 
                       demand_scenario = dem_scens, 
                       refining_scenario = ref_scens)
    
    # for 2021 onwards, use larger capacity
    
      altair_2021 = CJ(installation_year = c(2021),
                       refinery_name = dt_altair[year == 2021, refinery_name],
                       installation_capacity_bpd = dt_altair[year == 2021, barrels_per_day], 
                       retired_capacity_bpd = 0,
                       barrels_per_year = dt_altair[year == 2021, bbl_per_year], 
                       bge_per_year = dt_altair[year == 2021, bge_per_year],
                       site_id = 't-800',
                       location = dt_altair[year == 2021, location],
                       region = dt_altair[year == 2021, region],
                       cluster = 'South',
                       year = 2021:2045, 
                       demand_scenario = dem_scens, 
                       refining_scenario = ref_scens)
      
      # combine all altair operating year time series 
      
        altair_ref = rbindlist(list(altair_2020, altair_2021), use.names = T)
        
  # combine renewable refineries with altair -------
        
    res_renew_ref_reg = rbindlist(list(res_renew_ref_reg, altair_ref), use.names = T)
    setorder(res_renew_ref_reg, demand_scenario, refining_scenario, year)
    
    renewables_info = rbindlist(list(unique(altair_ref[, .(site_id, refinery_name, location, region, cluster)]),
                                     renewables_info), use.names = T)
    
# ------------------------- diagnostics ------------------------- 
  
  # combine region-level crude consumption with ghg emissions ------
    
    # get total crude demand at crude refineries
      tot_crude_regions = res_final_cdu[ghg_region_2018[, .(region, region_kgco2e_bbl)], on = 'region']
      tot_crude_regions[, total_co2e_kg := total_crude_demand_bbl * region_kgco2e_bbl]
      tot_crude_regions = tot_crude_regions[, .(demand_scenario, refining_scenario, region, year, total_crude_demand_bbl, region_kgco2e_bbl, total_co2e_kg)]
      
    # get crude demand at renewable refineries
      tot_crude_renew = dcast(res_renew_demand, demand_scenario + refining_scenario + year ~ fuel, value.var = 'renewable_refinery_consumption_bbl')
      tot_crude_renew[, coef := crude_refined_tot[, coef]]
      tot_crude_renew[, total_crude_demand_bbl := 
                        ( (`drop-in gasoline`*ei_gasoline) + (`renewable diesel`*ei_diesel)  + (`sustainable aviation fuel`*ei_jet)  ) / (ei_crude - coef)]
      tot_crude_renew[, region_kgco2e_bbl := mean(ghg_region_2018[, region_kgco2e_bbl])]
      tot_crude_renew[, total_co2e_kg := total_crude_demand_bbl * region_kgco2e_bbl]
      tot_crude_renew[, region := 'Renewables']
      
      tot_crude_renew = tot_crude_renew[, .(demand_scenario, refining_scenario, region, year, total_crude_demand_bbl, region_kgco2e_bbl, total_co2e_kg)]
      
    # combine all crude demand
      
      tot_crude_all = rbindlist(list(tot_crude_regions, tot_crude_renew), use.names = T, fill = T)
      tot_crude_all[, region := factor(region, levels = c('North', 'South', 'Renewables'))]
      
    # agg_crude_consumption = res_final_cdu[ghg_region_2018[, .(region, region_kgco2e_bbl)], on = 'region']
    # agg_crude_consumption = agg_crude_consumption[ghg_region_2018[, .(region, region_kgco2e_bbl)], on = 'region']
    # agg_crude_consumption[, equiv_co2e_kg := equiv_crude_demand_bbl * region_kgco2e_bbl]
    
  # refinery crude processing and renewable fuel refining capacity -------
    
    agg_crude_cap = res_crude_ref_reg[, .(crude_barrels_per_day = sum(barrels_per_day, na.rm = T),
                                          no_crude_refineries = uniqueN(site_id)), 
                                      by = .(demand_scenario, refining_scenario, region, year)]
    setorder(agg_crude_cap, demand_scenario, refining_scenario, region)
    
    agg_renew_cap = res_renew_ref_reg[, .(renew_barrels_per_day = sum(installation_capacity_bpd, na.rm = T),
                                          no_renew_refineries = uniqueN(refinery_name)), 
                                      by = .(demand_scenario, refining_scenario, year)]
    agg_renew_cap[, region := 'Renewables']
    setorder(agg_renew_cap, demand_scenario, refining_scenario, region)
    
    agg_cap_all = rbindlist(list(agg_crude_cap, agg_renew_cap), use.names = T, fill = T)
    agg_cap_all[, region := factor(region, levels = c('North', 'South', 'Renewables'))]
    
    
    fig_refinery_cap = ggplot(agg_cap_all) +
      geom_line(aes(x = year, y = (crude_barrels_per_day*365*(ei_crude/ei_gasoline))/1e6, color = region), size = 1.3) +
      geom_line(aes(x = year, y = (renew_barrels_per_day*365*(ei_crude/ei_gasoline))/1e6, color = region), size = 1.3) + 
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual capacity (million barrels of gasoline equivalent per year)',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 450, 50), limits = c(0, 450)) + 
      scale_color_manual(values = pal_region) +
      theme_line

    ggsave(fig_refinery_cap, 
           filename = file.path(save_path, 'refinery_capacity.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'refinery_capacity.pdf'),
                outfile = file.path(save_path, 'refinery_capacity.pdf'))
    
    
    fig_refinery_count = ggplot(agg_cap_all) +
      geom_line(aes(x = year, y = no_crude_refineries, color = region), size = 1.3) +
      geom_line(aes(x = year, y = no_renew_refineries, color = region), size = 1.3) + 
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Number of refineries in each year',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 10, 1), limits = c(0, 8)) + 
      scale_color_manual(values = pal_region) +
      theme_line
    # fig_refinery_count
    
    ggsave(fig_refinery_count, 
           filename = file.path(save_path, 'refinery_count.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'refinery_count.pdf'),
                outfile = file.path(save_path, 'refinery_count.pdf'))
  
  # crude consumption at all refineries (area plot) ----------
    
    fig_crude_cons_all = ggplot(tot_crude_all, aes(x = year, y = total_crude_demand_bbl/1e6, group = region, fill = region)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual crude consumption at all refineries (2020-2045)',
           subtitle = 'Million barrels',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_fill_manual(values = pal_region) + 
      theme_line
    
    ggsave(fig_crude_cons_all, 
           filename = file.path(save_path, 'crude_consumption_all_refineries.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'crude_consumption_all_refineries.pdf'),
                outfile = file.path(save_path, 'crude_consumption_all_refineries.pdf'))
    
    fig_crude_cons_main = ggplot(tot_crude_regions, aes(x = year, y = total_crude_demand_bbl/1e6, group = region, fill = region)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual crude consumption at crude refineries (2020-2045)',
           subtitle = 'Million barrels',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_fill_manual(values = pal_region) + 
      theme_line
    
    ggsave(fig_crude_cons_main, 
           filename = file.path(save_path, 'crude_consumption_crude_refineries.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'crude_consumption_crude_refineries.pdf'),
                outfile = file.path(save_path, 'crude_consumption_crude_refineries.pdf'))
    
    fig_crude_cons_renew = ggplot(tot_crude_renew, aes(x = year, y = total_crude_demand_bbl/1e6, group = region, fill = region)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual crude consumption at renewable refineries (2020-2045)',
           subtitle = 'Million barrels',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_fill_manual(values = pal_region) + 
      theme_line
    
    ggsave(fig_crude_cons_renew, 
           filename = file.path(save_path, 'crude_consumption_renewable_refineries.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'crude_consumption_renewable_refineries.pdf'),
                outfile = file.path(save_path, 'crude_consumption_renewable_refineries.pdf'))
    
    
    
    
  # ghg emissions (area plot) ---------
    
    fig_ghg_all = ggplot(tot_crude_all, aes(x = year, y = total_co2e_kg/1e9, fill = region)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual GHG emissions from crude consumption at all refineries',
           subtitle = 'Million tonnes',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_fill_manual(values = pal_region) + 
      theme_line
    
    ggsave(fig_ghg_all, 
           filename = file.path(save_path, 'ghg_emissions_all_refineries.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'ghg_emissions_all_refineries.pdf'),
                outfile = file.path(save_path, 'ghg_emissions_all_refineries.pdf'))
    
    fig_ghg_main = ggplot(tot_crude_regions, aes(x = year, y = total_co2e_kg/1e9, fill = region)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual GHG emissions from crude consumption at crude refineries',
           subtitle = 'Million tonnes',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_fill_manual(values = pal_region) + 
      theme_line
    
    ggsave(fig_ghg_main, 
           filename = file.path(save_path, 'ghg_emissions_crude_refineries.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'ghg_emissions_crude_refineries.pdf'),
                outfile = file.path(save_path, 'ghg_emissions_crude_refineries.pdf'))
    
    fig_ghg_renew = ggplot(tot_crude_renew, aes(x = year, y = total_co2e_kg/1e9, fill = region)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual GHG emissions from crude consumption at renewable refineries',
           subtitle = 'Million tonnes',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_fill_manual(values = pal_region) + 
      theme_line
    
    ggsave(fig_ghg_renew, 
           filename = file.path(save_path, 'ghg_emissions_renewable_refineries.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'ghg_emissions_renewable_refineries.pdf'),
                outfile = file.path(save_path, 'ghg_emissions_renewable_refineries.pdf'))


  # GJD production at crude refineries only -- region level (area plot) ---------

    # get in-state GJD demand
      agg_petro_demand = res_equiv_demand[, .(demand_scenario, refining_scenario, region, year, fuel_equiv, consumption_bge)]
      # setnames(agg_petro_demand, 'total_consumption_bge', 'consumption_bge')
      setnames(agg_petro_demand, 'fuel_equiv', 'fuel')
    
    # get in-state reGJD demand
    
      agg_renew_demand = res_equiv_demand[, .(demand_scenario, refining_scenario, region, year, fuel_equiv, residual_consumption_bge)]
      agg_renew_demand[is.na(residual_consumption_bge), residual_consumption_bge := 0]
      setnames(agg_renew_demand, 'residual_consumption_bge', 'consumption_bge')
      setnames(agg_renew_demand, 'fuel_equiv', 'fuel')
      agg_renew_demand[fuel == 'gasoline', fuel := 'drop-in gasoline']
      agg_renew_demand[fuel == 'diesel', fuel := 'renewable diesel']
      agg_renew_demand[fuel == 'jet', fuel := 'sustainable aviation fuel']
    
    # get exports
      agg_exports = res_equiv_demand[, .(demand_scenario, refining_scenario, region, year, fuel_equiv, export_bge)]
      agg_exports[, export_bge := abs(export_bge)]
      setnames(agg_exports, 'export_bge', 'consumption_bge')
      setnames(agg_exports, 'fuel_equiv', 'fuel')
      agg_exports[, fuel := 'exports']
      agg_exports = agg_exports[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(demand_scenario, refining_scenario, region, year, fuel)]
      
    # combine together
      agg_fuel_demand = rbindlist(list(agg_petro_demand, agg_renew_demand, agg_exports), use.names = T, fill = T)
      agg_fuel_demand[, fuel := factor(fuel, levels = rev(c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel', 'exports')))]
      
    fig_fuel_demand = ggplot(agg_fuel_demand, aes(x = year, y = consumption_bge/1e6, fill = fuel)) +
      geom_area() +
      facet_wrap(region + demand_scenario ~ refining_scenario, ncol = 4,
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual production at crude refineries by fuel (million barrels of gasoline equivalent)',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 350, 50)) + 
      scale_fill_manual(values = pal_fuel) + 
      theme_line
    
    ggsave(fig_fuel_demand, 
           filename = file.path(save_path, 'region_GJD_and_reGJD_production_crude_refineries.pdf'), 
           width = 18, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'region_GJD_and_reGJD_production_crude_refineries.pdf'),
                outfile = file.path(save_path, 'region_GJD_and_reGJD_production_crude_refineries.pdf'))
    
    
    
  # GJD production at all refineries -- state level (area plot) --------
    
    # get historic demand of GJD
      agg_hist_tot = melt(crude_refined_week, 
                          id.vars = c('year'),
                          measure.vars = c('crude_gge', 'gasoline_gge', 'diesel_gge', 'jet_gge'),
                          value.name = 'consumption_gge',
                          variable.name = 'fuel')
      agg_hist_tot[, fuel := gsub('_gge', '', fuel)]
      agg_hist_tot = agg_hist_tot[year < 2020, .(consumption_gge = sum(consumption_gge, na.rm = T)), by = .(year, fuel)]
      agg_hist_tot[, consumption_bge := consumption_gge / 42]
      
    # get historic exports
      agg_hist_exp = refined_movements_annual[, .(consumption_bbl = sum(abs(net_export_bbl), na.rm = T)), by = .(year, fuel)]
      agg_hist_exp[fuel == 'gasoline', consumption_gge := consumption_bbl * 42]
      agg_hist_exp[fuel == 'diesel', consumption_gge := consumption_bbl * 42 * (ei_diesel/ei_gasoline)]
      agg_hist_exp[fuel == 'jet', consumption_gge := consumption_bbl * 42 * (ei_jet/ei_gasoline)]
      agg_hist_exp[, export_bge := consumption_gge / 42]
      
      # agg_hist_exp = agg_hist_exp[, .(export_gge = sum(consumption_gge, na.rm = T)), by = .(year)]
      
      # agg_hist_exp[, fuel := 'exports']
      # agg_hist_exp[, consumption_bge := consumption_gge / 42]
      
    # subtract exports from fuels
      
      agg_hist_tot = merge(agg_hist_tot,
                           agg_hist_exp[, .(year, fuel, export_bge)],
                           by = c('year', 'fuel'))
      agg_hist_tot[, consumption_bge_new := consumption_bge - export_bge]
      agg_hist_tot[, consumption_gge_new := consumption_bge_new * 42]
      
      agg_hist_tot = agg_hist_tot[, .(year, fuel, consumption_gge_new, consumption_bge_new)]
      setnames(agg_hist_tot, 'consumption_gge_new', 'consumption_gge')
      setnames(agg_hist_tot, 'consumption_bge_new', 'consumption_bge')
      
    # aggregate exports
      
      agg_hist_exp = agg_hist_exp[, .(consumption_gge = sum(consumption_gge, na.rm = T)), by = .(year)]
      agg_hist_exp[, fuel := 'exports']
      agg_hist_exp[, consumption_bge := consumption_gge / 42]
      
    # get historic demand of renewable diesel 
      agg_hist_rediesel = dt_rediesel[, .(consumption_gal = sum(consumption_gal, na.rm = T)), by = .(year, fuel)]
      agg_hist_rediesel[, consumption_gge := consumption_gal * (ei_diesel/ei_gasoline)]
      agg_hist_rediesel[, consumption_bge := consumption_gge / 42]
      agg_hist_rediesel = agg_hist_rediesel[, .(year, fuel, consumption_gge, consumption_bge)]
      
    # combine RE diesel with GJD 
      
      agg_hist_tot = rbindlist(list(agg_hist_tot, agg_hist_rediesel), use.names = T, fill = T)
      
    # create full data table of historic fuel demand for all scenarios
      agg_hist_tot_full = CJ(demand_scenario = dem_scens, refining_scenario = ref_scens, year = 2014:2019,
                             fuel = c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel'))
      agg_hist_tot_full = agg_hist_tot[agg_hist_tot_full, on = .(year, fuel), allow.cartesian = T]
      agg_hist_tot_full = agg_hist_tot_full[! fuel == 'crude', .(demand_scenario, refining_scenario, year, fuel, consumption_bge)]
      agg_hist_tot_full[fuel %in% c('drop-in gasoline', 'sustainable aviation fuel'), consumption_bge := 0]
      setorder(agg_hist_tot_full, demand_scenario, refining_scenario, year, fuel)

      agg_hist_exp_full = CJ(demand_scenario = dem_scens, refining_scenario = ref_scens, year = 2007:2019)
      agg_hist_exp_full = agg_hist_exp_full[agg_hist_exp, on = 'year', allow.cartesian = T]
      agg_hist_exp_full = agg_hist_exp_full[, .(demand_scenario, refining_scenario, year, fuel, consumption_bge)]
      setorder(agg_hist_exp_full, demand_scenario, refining_scenario, year, fuel)

    # get in-state GJD demand
      tot_petro_demand = res_equiv_demand[, .(consumption_bge = sum(consumption_bge, na.rm = T)), 
                                          by = .(demand_scenario, refining_scenario, year, fuel_equiv)]
      setnames(tot_petro_demand, 'fuel_equiv', 'fuel')
    
    # get in-state reGJD demand at crude refineries
      tot_renew_demand_crude = res_equiv_demand[, .(consumption_bge = sum(residual_consumption_bge, na.rm = T)), 
                                                by = .(demand_scenario, refining_scenario, year, fuel_equiv)]
      setnames(tot_renew_demand_crude, 'fuel_equiv', 'fuel')
      tot_renew_demand_crude[fuel == 'gasoline', fuel := 'drop-in gasoline']
      tot_renew_demand_crude[fuel == 'diesel', fuel := 'renewable diesel']
      tot_renew_demand_crude[fuel == 'jet', fuel := 'sustainable aviation fuel']
      
    # get in-state reGJD demand at renewables refineries
      
      tot_renew_demand_ren = res_renew_demand[, .(consumption_bge = sum(renewable_refinery_consumption_bge, na.rm = T)), 
                                              by = .(demand_scenario, refining_scenario, year, fuel_equiv)]
      setnames(tot_renew_demand_ren, 'fuel_equiv', 'fuel')
      tot_renew_demand_ren[fuel == 'gasoline', fuel := 'drop-in gasoline']
      tot_renew_demand_ren[fuel == 'diesel', fuel := 'renewable diesel']
      tot_renew_demand_ren[fuel == 'jet', fuel := 'sustainable aviation fuel']
      
    # combine all reGJD demand
      
      tot_renew_demand = rbindlist(list(tot_renew_demand_crude, tot_renew_demand_ren))
      tot_renew_demand = tot_renew_demand[, .(consumption_bge = sum(consumption_bge, na.rm = T)), 
                                              by = .(demand_scenario, refining_scenario, year, fuel)]
      
    # get exports
      tot_exports = res_equiv_demand[, .(demand_scenario, refining_scenario, region, year, fuel_equiv, export_bge)]
      tot_exports[, export_bge := abs(export_bge)]
      tot_exports = tot_exports[, .(consumption_bge = sum(abs(export_bge), na.rm = T)), 
                                     by = .(demand_scenario, refining_scenario, fuel_equiv, year)]
      setnames(tot_exports, 'fuel_equiv', 'fuel')
      tot_exports[, fuel := 'exports']
      tot_exports = tot_exports[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(demand_scenario, refining_scenario, year, fuel)]
    
    # combine together
      tot_fuel_demand = rbindlist(list(agg_hist_tot_full,
                                       agg_hist_exp_full,
                                       tot_petro_demand, 
                                       tot_renew_demand, 
                                       tot_exports), use.names = T, fill = T)
      setorder(tot_fuel_demand, demand_scenario, refining_scenario, year, fuel)
      tot_fuel_demand[, fuel := factor(fuel, levels = rev(c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel', 'exports')))]
      
      tot_fuel_demand_crude = rbindlist(list(agg_hist_tot_full,
                                             agg_hist_exp_full,
                                             tot_petro_demand, 
                                             tot_renew_demand_crude, 
                                             tot_exports), use.names = T, fill = T)
      setorder(tot_fuel_demand_crude, demand_scenario, refining_scenario, year, fuel)
      tot_fuel_demand_crude[, fuel := factor(fuel, 
                                             levels = rev(c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel', 'exports')))]
      
    fig_fuel_demand_tot = ggplot(tot_fuel_demand, aes(x = year, y = consumption_bge/1e6, fill = fuel)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, ncol = 3,
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual production at all refineries by fuel (2014-2045)',
           subtitle = 'Million barrels of gasoline equivalent',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2015,2040,5), limits = c(2014, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) + 
      scale_fill_manual(values = pal_fuel) + 
      geom_segment(x = 2019, xend = 2019, y = 0, yend = 750, color = 'black', linetype = 2)  +
      theme_line +
      theme(legend.position = 'right')
      
      ggsave(fig_fuel_demand_tot, 
             filename = file.path(save_path, 'state_GJD_and_reGJD_production_all_refineries.pdf'), 
             width = 20, 
             height = 12)
      
      embed_fonts(file.path(save_path, 'state_GJD_and_reGJD_production_all_refineries.pdf'),
                  outfile = file.path(save_path, 'state_GJD_and_reGJD_production_all_refineries.pdf'))
      
    fig_fuel_demand_crude = ggplot(tot_fuel_demand_crude, aes(x = year, y = consumption_bge/1e6, fill = fuel)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, ncol = 3,
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual production at crude refineries by fuel (2014-2045)',
           subtitle = 'Million barrels of gasoline equivalent',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2015,2040,5), limits = c(2014, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) + 
      scale_fill_manual(values = pal_fuel) + 
      geom_segment(x = 2019, xend = 2019, y = 0, yend = 750, color = 'black', linetype = 2)  +
      theme_line +
      theme(legend.position = 'right')
      
      ggsave(fig_fuel_demand_crude, 
             filename = file.path(save_path, 'state_GJD_and_reGJD_production_crude_refineries.pdf'), 
             width = 20, 
             height = 12)
      
      embed_fonts(file.path(save_path, 'state_GJD_and_reGJD_production_crude_refineries.pdf'),
                  outfile = file.path(save_path, 'state_GJD_and_reGJD_production_crude_refineries.pdf'))
      
    fig_fuel_demand_renew = ggplot(tot_renew_demand_ren, aes(x = year, y = consumption_bge/1e6, fill = fuel)) +
      geom_area() +
      facet_wrap(demand_scenario ~ refining_scenario, ncol = 3,
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual production at renewable refineries by fuel (2020-2045)',
           subtitle = 'Million barrels of gasoline equivalent',
           x = NULL,
           y = NULL,
           fill = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 10)) + 
      scale_fill_manual(values = pal_fuel) + 
      geom_segment(x = 2019, xend = 2019, y = 0, yend = 750, color = 'black', linetype = 2)  +
      theme_line +
      theme(legend.position = 'right')
      
      ggsave(fig_fuel_demand_renew, 
             filename = file.path(save_path, 'state_GJD_and_reGJD_production_renewable_refineries.pdf'), 
             width = 20, 
             height = 12)
      
      embed_fonts(file.path(save_path, 'state_GJD_and_reGJD_production_renewable_refineries.pdf'),
                  outfile = file.path(save_path, 'state_GJD_and_reGJD_production_renewable_refineries.pdf'))

  # save statewide GJD production as csv -------
      
    fwrite(tot_fuel_demand, file.path(save_path, 'state_GJD_and_reGJD_production_all_refineries.csv'), row.names = F)
      
  # plot refineries and DAC ------
      
    dac_ref = dt_dac[dt_refcap, on = .(site_id)]
    dac_ref[, ratio := weighted_dac/weighted_population]
    
    fig_cap_pop = ggplot(dac_ref, aes(x = barrels_per_day/1e3, y = weighted_population, group = refinery_name, fill = refinery_name, color = refinery_name)) + 
      geom_point(size = 3) + 
      geom_text(aes(label = gsub(' Refinery', '', refinery_name)), hjust = 0.5, vjust =-0.5) +
      labs(title = 'Weighted population vs refinery capacity',
           subtitle = 'Weighted population',
           x = 'Refinery capacity (thousand barrels per day)',
           y = NULL,
           fill = NULL,
           color = NULL) +
      guides(color = guide_legend(ncol = 2)) +
      scale_x_continuous(expand = c(0.1,0.1), labels = scales::comma) +
      scale_y_continuous(expand = c(0.1,0.1), labels = scales::comma) +
      scale_color_manual(values = pal_refinery) +
      scale_fill_manual(values = pal_refinery) +
      theme_line

    ggsave(fig_cap_pop, 
           filename = file.path(save_path, 'weighted_population_vs_capacity.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'weighted_population_vs_capacity.pdf'),
                outfile = file.path(save_path, 'weighted_population_vs_capacity.pdf'))
    
    fig_cap_dac = ggplot(dac_ref, aes(x = barrels_per_day/1e3, y = weighted_dac, group = refinery_name, fill = refinery_name, color = refinery_name)) + 
      geom_point(size = 3) + 
      geom_text(aes(label = gsub(' Refinery', '', refinery_name)), hjust = 0.5, vjust =-0.5) +
      labs(title = 'Weighted DAC vs refinery capacity',
           subtitle = 'Weighted DAC',
           x = 'Refinery capacity (thousand barrels per day)',
           y = NULL,
           fill = NULL,
           color = NULL) +
      guides(color = guide_legend(ncol = 2)) +
      scale_x_continuous(expand = c(0.1,0.1), labels = scales::comma) +
      scale_y_continuous(expand = c(0.1,0.1), labels = scales::comma, breaks = seq(0, 150e3, 25e3)) +
      scale_color_manual(values = pal_refinery) +
      scale_fill_manual(values = pal_refinery) +
      theme_line

    ggsave(fig_cap_dac, 
           filename = file.path(save_path, 'weighted_DAC_vs_capacity.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'weighted_DAC_vs_capacity.pdf'),
                outfile = file.path(save_path, 'weighted_DAC_vs_capacity.pdf'))
    
    fig_cap_ratio = ggplot(dac_ref) + 
      geom_point(size = 5, aes(x = barrels_per_day/1e3, y = ratio, fill = refinery_name, color = refinery_name)) + 
      geom_text(aes(x = barrels_per_day/1e3, y = ratio, color = refinery_name, label = gsub(' Refinery', '', refinery_name)),
                hjust = 0.3, vjust =0, family = 'Secca Soft', size = 5) +
      stat_smooth(method = 'lm', aes(x = barrels_per_day/1e3, y = ratio), color = '#636363', alpha = 0.2) +
      labs(title = 'Ratio of weighted DAC to weighted total population vs refinery capacity',
           subtitle = 'Ratio of weighted DAC to weighted total population',
           x = 'Refinery capacity (thousand barrels per day)',
           y = NULL,
           fill = NULL,
           color = NULL) +
      guides(color = guide_legend(ncol = 2)) +
      scale_x_continuous(expand = c(0.05, 0.05), labels = scales::comma, limits = c(0, 375)) +
      scale_y_continuous(expand = c(0.05, 0.05), labels = scales::comma, breaks = seq(0, 1, 0.1), limits = c(0,1)) +
      scale_color_manual(values = pal_refinery) +
      scale_fill_manual(values = pal_refinery) +
      theme_line
    # fig_cap_ratio
    
    ggsave(fig_cap_ratio, 
           filename = file.path(save_path, 'weighted_ratio_vs_capacity.pdf'), 
           width = 16, 
           height = 12)
    
    embed_fonts(file.path(save_path, 'weighted_ratio_vs_capacity.pdf'),
                outfile = file.path(save_path, 'weighted_ratio_vs_capacity.pdf'))
    
      
# ------------------------- scenarios ------------------------- 
    
    
  # for each year, calculate the capacity ratio from operating refinery within its respective region -------
    
    res_crude_ref_reg[, barrels_per_year := barrels_per_day * 365]
    res_crude_ref_reg[, capacity_ratio := barrels_per_year/sum(barrels_per_year), by = .(demand_scenario, refining_scenario, year, region)]
    
    res_renew_ref_reg[, capacity_ratio := barrels_per_year/sum(barrels_per_year), by = .(demand_scenario, refining_scenario, year)]
  
  # merge annual regional crude consumption with refineries operating in that year -------

    # agg_crude_consumption_indiv = merge(tot_crude_regions[, .(demand_scenario, refining_scenario, region, year, total_crude_demand_bbl)],
    #                                     res_crude_ref_reg[, .(demand_scenario, refining_scenario, region, year, site_id, refinery_name, location, capacity_ratio)],
    #                                     by = c('demand_scenario', 'refining_scenario', 'region', 'year'),
    #                                     allow.cartesian = T)

  # divide GJD demand within crude refineries -------
    
    # get "traditional" GJD production (regular GJD demand and exports)
      ref_crude_gjd = res_equiv_demand[, .(demand_scenario, refining_scenario, fuel_equiv, year, region, consumption_bge, consumption_bbl, export_bge, export_bbl)]
      ref_crude_gjd[, traditional_production_bge := consumption_bge + abs(export_bge)]
      ref_crude_gjd[, traditional_production_bbl := consumption_bbl + abs(export_bbl)]
      
    # merge demand with operating crude refineries
      ref_crude_gjd = merge(ref_crude_gjd[, .(demand_scenario, refining_scenario, region, fuel_equiv, year, traditional_production_bbl)],
                            res_crude_ref_reg[, .(demand_scenario, refining_scenario, region, year, site_id, refinery_name, location, capacity_ratio)],
                            by = c('demand_scenario', 'refining_scenario', 'region', 'year'),
                            allow.cartesian = T)
      
    # split regional demand to indiv refineries
      ref_crude_gjd[, traditional_production_bbl := traditional_production_bbl * capacity_ratio]

    # melt bbl data to wide format
      ref_crude_gjd = dcast(ref_crude_gjd, 
                            demand_scenario + refining_scenario + region + site_id + refinery_name + location + year ~ fuel_equiv, 
                            value.var = 'traditional_production_bbl')
      
    # use heat intensity equation to solve for refinery-level crude consumption
      ref_crude_gjd = ref_crude_gjd[crude_refined_region[, .(region, coef)], on = .(region)]
      ref_crude_gjd[, traditional_crude_consumption_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
      
    # reorganize data table
      setnames(ref_crude_gjd, 'gasoline', 'gasoline_production_bbl')
      setnames(ref_crude_gjd, 'diesel', 'diesel_production_bbl')
      setnames(ref_crude_gjd, 'jet', 'jet_production_bbl')
      
      ref_crude_gjd = ref_crude_gjd[, .(demand_scenario, refining_scenario, site_id, refinery_name, location, region, year, 
                                        gasoline_production_bbl, diesel_production_bbl, jet_production_bbl, traditional_crude_consumption_bbl)]

  # divide residual reGJD demand within crude refineries --------
      
    # get residual reGJD demand
      ref_crude_res_regjd = res_equiv_demand[, .(demand_scenario, refining_scenario, fuel_equiv, year, region, residual_consumption_bge, residual_consumption_bbl)]
      setnames(ref_crude_res_regjd, 'residual_consumption_bge', 'residual_production_bge')
      setnames(ref_crude_res_regjd, 'residual_consumption_bbl', 'residual_production_bbl')
      
    # create unique list of scenarios and years
      
      un_scens = CJ(demand_scenario = dem_scens, refining_scenario = ref_scens, year = 2020:2045)
      
    # merge to decide if Kern Oil is operating in each unique scenario-year combo
      
      un_scens = res_crude_ref_reg[refinery_name %like% 'Kern Oil'][un_scens, on = .(demand_scenario, refining_scenario, year)]
      un_scens[, kern_oil_operating := ifelse(is.na(refinery_name), 'no', 'yes')]
      un_scens = un_scens[, .(demand_scenario, refining_scenario, year, kern_oil_operating)]
      
    # merge demand with operating crude refineries
      ref_crude_res_regjd = merge(ref_crude_res_regjd[, .(demand_scenario, refining_scenario, region, fuel_equiv, year, residual_production_bbl)],
                                  res_crude_ref_reg[, .(demand_scenario, refining_scenario, region, year, site_id, refinery_name, location, capacity_ratio)],
                                  by = c('demand_scenario', 'refining_scenario', 'region', 'year'),
                                  allow.cartesian = T)
      
    # merge with indicator of kern oil operating
      ref_crude_res_regjd = ref_crude_res_regjd[un_scens, on = .(demand_scenario, refining_scenario, year)]
      
    # for South region, split evenly amongst refineries using capacity ratio
      ref_crude_res_regjd[region == 'South', residual_production_bbl_2 := residual_production_bbl * capacity_ratio]
      
    # for renewable gasoline and renewable jet, split evenly using capacity ratios also
      ref_crude_res_regjd[region == 'North' & fuel_equiv %in% c('gasoline', 'jet'), residual_production_bbl_2 := residual_production_bbl * capacity_ratio]
      
    # for the renewable diesel production in the North region:
      # - if Kern Oil is operating, then give Kern Oil its historic production values and split the rest evenly amongst the other refineries
      # - if Kern Oil is notoperating, then split total renewable diesel demand evenly
      
      ref_crude_res_regjd[region == 'North' & fuel_equiv == 'diesel', 
                          residual_production_bbl_2 := ifelse(kern_oil_operating == 'yes',
                                                              ifelse(residual_production_bbl > ave_kern_rediesel[, consumption_bbl],
                                                                     ifelse(refinery_name %like% 'Kern Oil', 
                                                                            ave_kern_rediesel[, consumption_bbl],
                                                                            (residual_production_bbl - ave_kern_rediesel[, consumption_bbl]) * capacity_ratio),
                                                                     ifelse(refinery_name %like% 'Kern Oil', 
                                                                            residual_production_bbl,
                                                                            0)),
                                                              residual_production_bbl * capacity_ratio)]
      
    # rename
      ref_crude_res_regjd[, residual_production_bbl := NULL]
      setnames(ref_crude_res_regjd, 'residual_production_bbl_2', 'residual_production_bbl')
      
    # melt bbl data to wide format
      ref_crude_res_regjd = dcast(ref_crude_res_regjd, 
                                  demand_scenario + refining_scenario + region + site_id + refinery_name + location + year ~ fuel_equiv, 
                                  value.var = 'residual_production_bbl')
      
    # use heat intensity equation to solve for refinery-level crude consumption
      ref_crude_res_regjd = ref_crude_res_regjd[crude_refined_region[, .(region, coef)], on = .(region)]
      ref_crude_res_regjd[, residual_renewable_crude_consumption_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
      
    # reorganize data table
      setnames(ref_crude_res_regjd, 'gasoline', 'residual_renewable_gasoline_production_bbl')
      setnames(ref_crude_res_regjd, 'diesel', 'residual_renewable_diesel_production_bbl')
      setnames(ref_crude_res_regjd, 'jet', 'residual_renewable_jet_production_bbl')
      
      ref_crude_res_regjd = ref_crude_res_regjd[, .(demand_scenario, refining_scenario, site_id, refinery_name, location, region, year, 
                                                    residual_renewable_gasoline_production_bbl, residual_renewable_diesel_production_bbl, 
                                                    residual_renewable_jet_production_bbl, residual_renewable_crude_consumption_bbl)]

  # divide reGJD demand between renewable refineries ------
    
    # get residual reGJD demand
      ref_renew_gjd = res_renew_demand[, .(demand_scenario, refining_scenario, fuel_equiv, year, renewable_refinery_consumption_bge, renewable_refinery_consumption_bbl)]
      setnames(ref_renew_gjd, 'renewable_refinery_consumption_bge', 'renewable_production_bge')
      setnames(ref_renew_gjd, 'renewable_refinery_consumption_bbl', 'renewable_production_bbl')
      
    # merge demand with operating renewable refineries
      ref_renew_gjd = merge(ref_renew_gjd[, .(demand_scenario, refining_scenario, fuel_equiv, year, renewable_production_bbl)],
                            res_renew_ref_reg[, .(demand_scenario, refining_scenario, year, refinery_name, capacity_ratio)],
                            by = c('demand_scenario', 'refining_scenario',  'year'),
                            allow.cartesian = T)
      
    # add locational info
      ref_renew_gjd = ref_renew_gjd[renewables_info, on = 'refinery_name']

    # split regional demand to indiv refineries
      ref_renew_gjd[, renewable_production_bbl := renewable_production_bbl * capacity_ratio]
      
    # melt bbl data to wide format
      ref_renew_gjd = dcast(ref_renew_gjd, 
                            demand_scenario + refining_scenario + region + site_id + refinery_name + location + year ~ fuel_equiv, 
                            value.var = 'renewable_production_bbl')
      
    # use heat intensity equation to solve for refinery-level crude consumption
      ref_renew_gjd[, coef := crude_refined_tot[, coef]]
      ref_renew_gjd[, main_renewable_crude_consumption_bbl := ( (gasoline*ei_gasoline) + (diesel*ei_diesel)  + (jet*ei_jet)  ) / (ei_crude - coef)]
      
    # reorganize data table
      setnames(ref_renew_gjd, 'gasoline', 'main_renewable_gasoline_production_bbl')
      setnames(ref_renew_gjd, 'diesel', 'main_renewable_diesel_production_bbl')
      setnames(ref_renew_gjd, 'jet', 'main_renewable_jet_production_bbl')
      
      ref_renew_gjd = ref_renew_gjd[, .(demand_scenario, refining_scenario, site_id, refinery_name, location, region, year, 
                                        main_renewable_gasoline_production_bbl, main_renewable_diesel_production_bbl, 
                                        main_renewable_jet_production_bbl, main_renewable_crude_consumption_bbl)]
      
    
  # combine all refinery-level production and consumption -------
      
    
    ref_cons_prod = merge(ref_crude_gjd,
                          ref_crude_res_regjd,
                          by = c('demand_scenario', 'refining_scenario', 'site_id', 'refinery_name', 'location', 'region', 'year'),
                          all = T)
    ref_cons_prod[, site_id := as.character(site_id)]
      
    ref_cons_prod = merge(ref_cons_prod,
                          ref_renew_gjd,
                          by = c('demand_scenario', 'refining_scenario', 'site_id', 'refinery_name', 'location', 'region', 'year'),
                          all = T)
    
    fix_columns = colnames(ref_cons_prod)[8:19]
    ref_cons_prod[, (fix_columns):=lapply(.SD, function(x) ifelse(is.na(x), 0 , x)), .SDcols = fix_columns]
    
    ref_cons_prod[, total_crude_consumption_bbl := traditional_crude_consumption_bbl + residual_renewable_crude_consumption_bbl + main_renewable_crude_consumption_bbl]
      
  # # split regional crude consumption to refinery level --------
  # 
  #   cols = 'total_crude_demand_bbl'
  #   agg_crude_consumption_indiv[, (cols) := lapply(.SD, function(x) x*capacity_ratio), .SDcols = cols]
  #   setcolorder(agg_crude_consumption_indiv, c('demand_scenario', 'refining_scenario', 'region', 'year', 'site_id', 'refinery_name', 'location',
  #                                              'total_crude_demand_bbl', 'capacity_ratio'))
  #   
  # add ghg emissions factor to refinery crude consumption ---------

    ref_cons_prod = ref_cons_prod[ghg_region_2018[, .(region, region_kgco2e_bbl)], on = 'region']
    ref_cons_prod[, total_co2e_kg := total_crude_consumption_bbl * region_kgco2e_bbl]
    
    fig_refinery_emissions = ggplot(ref_cons_prod, aes(x = year, y = total_co2e_kg/1e9, color = refinery_name)) +
      geom_line(size = 1) +
      facet_wrap(demand_scenario ~ refining_scenario,
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual GHG emissions from total crude consumption (million tonnes)',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      guides(col = guide_legend(ncol = 2)) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_refinery) +
      theme_line

    ggsave(fig_refinery_emissions,
           filename = file.path(save_path, 'refinery_equivalent_ghg.pdf'),
           width = 16,
           height = 12)

    embed_fonts(file.path(save_path, 'refinery_equivalent_ghg.pdf'),
                outfile = file.path(save_path, 'refinery_equivalent_ghg.pdf'))

  # determine ccs adoption decision -------

    solve_tc <- function(a, b, q) {
      f <- (q*(a*b - a*(q^(1/a)) + b))/(a + 1)
      return(f)
    }

    # calculate mean b values by scenario
      dt_scen[, mean_b := solve_mean_b(a, ccs_price_usd_per_kg*1e3, 'refining'), by = .(innovation_scenario, carbon_price_scenario, ccs_scenario, year)]

    # combine scenario conditions with operating refinery crude demand
      ref_cons_prod_scens = ref_cons_prod[dt_scen, on = .(year), allow.cartesian = T]

    # adjust ghg factor by innovation factor
      ref_cons_prod_scens[, region_kgco2e_bbl_adj := region_kgco2e_bbl * innovation_multiplier]

    # calculate innovation adjusted emissions
      ref_cons_prod_scens[, total_kgCO2e_adj := region_kgco2e_bbl_adj * total_crude_consumption_bbl]
      ref_cons_prod_scens[, total_mtCO2e_adj := total_kgCO2e_adj/1e3]

      ref_cons_prod_scens[, total_cost := solve_tc(a, mean_b, total_mtCO2e_adj)]
      ref_cons_prod_scens[, ccs_adj_usd_per_mt := total_cost/total_mtCO2e_adj]
      ref_cons_prod_scens[, ccs_adj_usd_per_kg := total_cost/total_kgCO2e_adj]
      ref_cons_prod_scens[is.na(ccs_adj_usd_per_kg), ccs_adj_usd_per_kg := ccs_price_usd_per_kg]
      ref_cons_prod_scens[, ccs_adoption := ccs_adj_usd_per_kg - carbon_price_usd_per_kg]
      ref_cons_prod_scens[, ccs_adopted := ifelse(ccs_adoption < 0, 1, 0)]
      
    # get ccs adoption of previous row
      setorder(ref_cons_prod_scens, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, site_id, year)
      ref_cons_prod_scens[, prev_ccs := shift(ccs_adopted, type = 'lag'), by = .(demand_scenario, refining_scenario, 
                                                                                 innovation_scenario, carbon_price_scenario, ccs_scenario, site_id)]
      ref_cons_prod_scens[is.na(prev_ccs), prev_ccs := 0]
      

    # if ccs previously adopted, adopted for subsequent year
      ref_cons_prod_scens[prev_ccs == 1 & ccs_adopted == 0, ccs_adopted := 1]
    
    # if ccs adopted, set ghg emissions = 0
      ref_cons_prod_scens[, total_kgCO2e_adj := ifelse(ccs_adopted == 1, total_kgCO2e_adj * (1 - ccs_eff), total_kgCO2e_adj)]
      
    # reorder factor levels
      ref_cons_prod_scens[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
      ref_cons_prod_scens[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
      ref_cons_prod_scens[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]

  # plot: refinery-ghg emissions under scenarios ----------

      scen_combos = CJ(demand_scenario = dem_scens, refining_scenario = ref_scens, region = c('North', 'South'))

      for (i in 1:nrow(scen_combos)) {

        # scen = unique(scenarios_dt[, oil_price_scenario])[i]
        # scen_dir = gsub(' ', '_', scen)
        scen = scen_combos[i]
        scen_title = paste(unlist(t(scen)),collapse=" + ")
        scen_title = gsub('_', ' ', scen_title)
        scen_name = paste(unlist(t(scen)), collapse = '_')
        scen_name = gsub(' ', '_', scen_name)
        scen_dir = paste(scen_name, collapse = "_")

        fname = paste0(scen_dir, '_scenario_refinery_equivalent_ghg.pdf')

        dt = ref_cons_prod_scens[scen, on = .(demand_scenario, refining_scenario, region), nomatch = 0]

        fig_scen_ghg = ggplot(dt, aes(x = year, y = total_kgCO2e_adj/1e9, color = refinery_name)) +
          geom_line(size = 1) +
          facet_grid(innovation_scenario + ccs_scenario ~ carbon_price_scenario,
                     labeller = labeller(carbon_price_scenario = c('price floor' = 'low carbon price',
                                                                   'central SCC' = 'medium carbon price',
                                                                   'price ceiling' = 'high carbon price'))) +
          labs(title = paste0('Annual GHG emissions (million tonnes) in ', scen_title, ' scenario'),
               x = NULL,
               y = NULL,
               color = NULL,
               linetype = NULL) +
          guides(col = guide_legend(ncol = 2)) +
          scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
          scale_y_continuous(expand = c(0,0)) +
          scale_color_manual(values = pal_refinery) +
          theme_line

        ggsave(fig_scen_ghg,
               filename = file.path(save_path, 'scenarios', fname),
               width = 12,
               height = 14.5)

        embed_fonts(file.path(save_path, 'scenarios', fname),
                    outfile = file.path(save_path, 'scenarios', fname))

        rm(scen, scen_dir, fname, fig_scen_ghg)


      }



  #     # View(agg_crude_consumption_scens[refinery_name == 'Chevron U.S.A. Inc., Richmond Refinery' & demand_scenario == 'BAU' & refining_scenario == 'historic exports' & carbon_price_scenario == 'price ceiling' & ccs_scenario == 'low CCS cost' & innovation_scenario == 'high innovation'])
  #   # unique(agg_crude_consumption_scens[carbon_price_scenario == 'price ceiling' & ccs_scenario == 'low CCS cost' & demand_scenario == 'BAU' & refining_scenario == 'historic exports', .(demand_scenario, refining_scenario, region, year, refinery_name, mean_b, ccs_adj_usd_per_kg)])
  # 

      
# ------------------------- outputs ------------------------- 
      
      
  # refinery-level: fuel production -----------
      
    # get RJD production at the refinery level
      
      indiv_prod_1 = ref_cons_prod_scens[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                             site_id, refinery_name, location, region, 
                                             gasoline_production_bbl, diesel_production_bbl, jet_production_bbl)]
        
      indiv_prod_1 =  melt(indiv_prod_1,
                           id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year', 
                                       'site_id', 'refinery_name', 'location', 'region'),
                           measure.vars = c('gasoline_production_bbl', 'diesel_production_bbl', 'jet_production_bbl'),
                           variable.name = 'fuel',
                           value.name = 'production_bbl')
      indiv_prod_1[, fuel := gsub('_production_bbl', '', fuel)]
      
    # get reGJD production at the refinery level
      
      indiv_prod_2 = ref_cons_prod_scens[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                             site_id, refinery_name, location, region, year, 
                                             residual_renewable_gasoline_production_bbl, residual_renewable_diesel_production_bbl, 
                                             residual_renewable_jet_production_bbl, main_renewable_gasoline_production_bbl, main_renewable_diesel_production_bbl, 
                                             main_renewable_jet_production_bbl)]
      indiv_prod_2 =  melt(indiv_prod_2,
                           id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                       'site_id', 'refinery_name', 'location', 'region', 'year'),
                           measure.vars = c('residual_renewable_gasoline_production_bbl', 'residual_renewable_diesel_production_bbl', 
                                            'residual_renewable_jet_production_bbl', 'main_renewable_gasoline_production_bbl', 'main_renewable_diesel_production_bbl', 
                                            'main_renewable_jet_production_bbl'),
                           variable.name = 'fuel',
                           value.name = 'production_bbl')
      indiv_prod_2[fuel %like% 'gasoline', fuel := 'drop-in gasoline']
      indiv_prod_2[fuel %like% 'diesel', fuel := 'renewable diesel']
      indiv_prod_2[fuel %like% 'jet', fuel := 'sustainable aviation fuel']
    
    # aggregate residual and main reGJD together
      
      indiv_prod_2 = indiv_prod_2[, .(production_bbl = sum(production_bbl, na.rm = T)), 
                                  by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year,
                                         site_id, refinery_name, location, region, fuel)]
    
    # combine results
      
      indiv_prod = rbindlist(list(indiv_prod_1, indiv_prod_2), use.names = T, fill = T)
      indiv_prod[fuel %like% 'gasoline', production_bge := production_bbl]
      indiv_prod[fuel %like% 'diesel', production_bge := production_bbl * (ei_diesel/ei_gasoline)]
      indiv_prod[fuel %like% 'jet' | fuel %like% 'aviation', production_bge := production_bbl * (ei_jet/ei_gasoline)]
      
    # assign clusters
      
      indiv_prod[region == 'South', cluster := 'South']
      indiv_prod[region == 'North' & location == 'Bakersfield', cluster := 'Bakersfield']
      indiv_prod[region == 'North' & (! location == 'Bakersfield'), cluster := 'Bay Area']
      
    # set factor level order
      
      indiv_prod[, fuel := factor(fuel, levels = c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel'))]
      indiv_prod[, cluster := factor(cluster, levels = c('Bay Area', 'Bakersfield', 'South'))]
      indiv_prod[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
      indiv_prod[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
      indiv_prod[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
      
      setorder(indiv_prod, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, site_id, fuel)

    # get outputs version
      
      indiv_prod_output = indiv_prod[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                         site_id, refinery_name, location, region, cluster, fuel, production_bbl)]
      indiv_prod_output[, type := 'production']
      indiv_prod_output[, units := 'bbl']
      indiv_prod_output[, source := 'total']
      indiv_prod_output[, boundary := 'complete']
      setnames(indiv_prod_output, 'production_bbl', 'value')
      setcolorder(indiv_prod_output, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year', 
                                       'site_id', 'refinery_name', 'location', 'region', 'cluster', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
      
      
  # refinery-level: crude consumption -----------
      
    # get refinery-level consumption
      
      indiv_cons = ref_cons_prod_scens[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                             site_id, refinery_name, location, region, 
                                             traditional_crude_consumption_bbl, residual_renewable_crude_consumption_bbl, main_renewable_crude_consumption_bbl,
                                             total_crude_consumption_bbl)]
      
      indiv_cons =  melt(indiv_cons,
                         id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year', 
                                     'site_id', 'refinery_name', 'location', 'region'),
                         measure.vars = c('traditional_crude_consumption_bbl', 'residual_renewable_crude_consumption_bbl', 'main_renewable_crude_consumption_bbl',
                                          'total_crude_consumption_bbl'),
                         variable.name = 'fuel',
                         value.name = 'consumption_bbl')
      indiv_cons[, fuel := gsub('_crude_consumption_bbl', '', fuel)]
      indiv_cons[, source := gsub('_', ' ', fuel)]
      indiv_cons[, fuel := 'crude']
      # indiv_cons[, fuel := paste0('crude (', fuel, ')')]
      
    # assign clusters
      
      indiv_cons[region == 'South', cluster := 'South']
      indiv_cons[region == 'North' & location == 'Bakersfield', cluster := 'Bakersfield']
      indiv_cons[region == 'North' & (! location == 'Bakersfield'), cluster := 'Bay Area']
      
    # set factor level order
      
      indiv_cons[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable',  'total'))]
      indiv_cons[, cluster := factor(cluster, levels = c('Bay Area', 'Bakersfield', 'South'))]
      indiv_cons[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
      indiv_cons[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
      indiv_cons[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
      
      setorder(indiv_cons, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, site_id, fuel, source)

    # get outputs version
      
      indiv_cons_output = indiv_cons[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                         site_id, refinery_name, location, region, cluster, fuel, source, consumption_bbl)]
      indiv_cons_output[, type := 'consumption']
      indiv_cons_output[, units := 'bbl']
      indiv_cons_output[, boundary := 'complete']
      setnames(indiv_cons_output, 'consumption_bbl', 'value')
      setcolorder(indiv_cons_output, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year', 
                                       'site_id', 'refinery_name', 'location', 'region', 'cluster', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
      
      
  # refinery-level: emissions -----------
      
    # get unique set of adjusted emissions factors and ccs adoption
      
      indiv_ccs = ref_cons_prod_scens[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                          site_id, refinery_name, location, region, 
                                          region_kgco2e_bbl_adj, ccs_adopted)]
      
    # merge with consumption data to get refinery-emissions
      
      indiv_ghg = indiv_cons[indiv_ccs, on = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                               site_id, refinery_name, location, region)]
      
    # calculate emissions
      
      indiv_ghg[, co2e_kg := ifelse(ccs_adopted == 1, consumption_bbl * region_kgco2e_bbl_adj * (1 - ccs_eff), consumption_bbl * region_kgco2e_bbl_adj)]
      
    # assign clusters
      
      indiv_ghg[region == 'South', cluster := 'South']
      indiv_ghg[region == 'North' & location == 'Bakersfield', cluster := 'Bakersfield']
      indiv_ghg[region == 'North' & (! location == 'Bakersfield'), cluster := 'Bay Area']
      
    # set factor level order
      
      indiv_ghg[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable',  'total'))]
      indiv_ghg[, cluster := factor(cluster, levels = c('Bay Area', 'Bakersfield', 'South'))]
      indiv_ghg[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
      indiv_ghg[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
      indiv_ghg[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
      
      setorder(indiv_ghg, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, site_id, source)
      
    # get outputs version
    
      indiv_ghg_output = indiv_ghg[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, 
                                         site_id, refinery_name, location, region, cluster, fuel, source, co2e_kg)]
      indiv_ghg_output[, type := 'ghg']
      indiv_ghg_output[, units := 'kg']
      indiv_ghg_output[, boundary := 'complete']
      setnames(indiv_ghg_output, 'co2e_kg', 'value')
      setcolorder(indiv_ghg_output, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year', 
                                      'site_id', 'refinery_name', 'location', 'region', 'cluster', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
      
  # cluster-level: fuel production, crude consumption, ghg emissions ----------
      
    clus_prod_output = indiv_prod_output[, .(value = sum(value, na.rm = T)), 
                                          by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                 cluster, year, fuel, source, boundary, type, units)]
    
    clus_cons_output = indiv_cons_output[, .(value = sum(value, na.rm = T)), 
                                         by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                cluster, year, fuel, source, boundary, type, units)]
  
    clus_ghg_output = indiv_ghg_output[, .(value = sum(value, na.rm = T)), 
                                       by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                              cluster, year, fuel, source, boundary, type, units)]
    
  # state-level: fuel production, crude consumption, ghg emissions ----------
      
    state_prod_output = indiv_prod_output[, .(value = sum(value, na.rm = T)), 
                                          by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                 year, fuel, source, boundary, type, units)]
      
    state_cons_output = indiv_cons_output[, .(value = sum(value, na.rm = T)), 
                                          by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                 year, fuel, source, boundary, type, units)]
    
    state_ghg_output = indiv_ghg_output[, .(value = sum(value, na.rm = T)), 
                                        by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                               year, fuel, source, boundary, type, units)]
    
  # get state-level fuel exports ------
    
    state_exp = res_equiv_demand[, .(demand_scenario, refining_scenario, region, year, fuel_equiv, export_bbl)]
    state_exp[, export_bbl := abs(export_bbl)]
    setnames(state_exp, 'fuel_equiv', 'fuel')
    state_exp = state_exp[, .(export_bbl = sum(export_bbl, na.rm = T)), by = .(demand_scenario, refining_scenario, year, fuel)]
    
    state_exp_wide = dcast(state_exp, demand_scenario + refining_scenario + year ~ fuel, value.var = 'export_bbl')
    colnames(state_exp_wide)[4:6] = c('diesel_exp_bbl', 'gasoline_exp_bbl', 'jet_exp_bbl')
    
  # get difference between intra-state and inter-state jet demand ------
    
    # select total state-wide forecasted jet fuel production 
      jet_inter = state_prod_output[fuel == 'jet']
      setnames(jet_inter, 'value', 'complete_jet')
    
    # in intrastate jet fuel demand forecast, convert to bbl of jet
      dt_intra[, consumption_bbl := (consumption_gge / 42) * (ei_gasoline / ei_jet)]
    
    # combine total jet demand and intrastate jet demand
      jet_difference = jet_inter[dt_intra[year >= 2020 & year <= 2045, .(year, consumption_bbl)], on = .(year)]
      setnames(jet_difference, 'consumption_bbl', 'intrastate_jet_bbl')
    
    # combine jet demand (total and intrastate) with jet exports
      jet_difference = jet_difference[state_exp[fuel == 'jet'], 
                                      on = .(demand_scenario, refining_scenario, year, fuel)]
    
    # calculate interstate jet demand
      jet_difference[, interstate_jet_bbl := complete_jet - intrastate_jet_bbl - export_bbl]
    
    # select columns to keep
    jet_difference = jet_difference[, .(demand_scenario, refining_scenario, innovation_scenario, 
                                        carbon_price_scenario, ccs_scenario, 
                                        year, interstate_jet_bbl, intrastate_jet_bbl)]
    
  # separate boundaries for state-level ghg emissions (using traditional values) ------
    
    state_prod_wide = state_prod_output[fuel %in% c('gasoline', 'diesel', 'jet')] 
    state_prod_wide = dcast(state_prod_wide, demand_scenario + refining_scenario + innovation_scenario + carbon_price_scenario + ccs_scenario + year ~ fuel,
                            value.var = 'value')
    setnames(state_prod_wide, 'gasoline', 'complete_gasoline_bbl')
    setnames(state_prod_wide, 'diesel', 'complete_diesel_bbl')
    setnames(state_prod_wide, 'jet', 'complete_jet_bbl')
    
    boundary_trad = merge(state_prod_wide,
                          state_ghg_output[source == 'traditional', .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                                      year, value)],
                          by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                 'year'))
    setnames(boundary_trad, 'value', 'traditional_ghg_kg')

    boundary_trad = boundary_trad[jet_difference, on = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                                   year)]
    
    boundary_trad = boundary_trad[state_exp_wide, on = .(demand_scenario, refining_scenario, year)]
    
    boundary_trad[, out_of_state_prop := ((interstate_jet_bbl*ei_jet) + 
                                                   (gasoline_exp_bbl*ei_gasoline) + 
                                                   (diesel_exp_bbl*ei_diesel) + 
                                                   (jet_exp_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) + (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
    
    boundary_trad[, in_state_prop := ((complete_gasoline_bbl - gasoline_exp_bbl)*(ei_gasoline) + (complete_diesel_bbl - diesel_exp_bbl)*(ei_diesel) +
                                        (intrastate_jet_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) + 
                                                                        (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
    
    boundary_trad[, out_of_state_ghg_kg := out_of_state_prop * traditional_ghg_kg]
    boundary_trad[, in_state_ghg_kg := in_state_prop * traditional_ghg_kg]
    
  # organize outputs for in-state emissions ------
    
    state_ghg_wide = dcast(state_ghg_output, demand_scenario + refining_scenario + innovation_scenario + carbon_price_scenario + ccs_scenario + year ~ source, 
                           value.var = 'value')
      
    in_state_ghg = boundary_trad[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, in_state_ghg_kg)]
    setnames(in_state_ghg, 'in_state_ghg_kg', 'traditional')
    in_state_ghg = merge(in_state_ghg,
                         state_ghg_wide[, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 
                                            'ccs_scenario', 'year', 'residual renewable', 'main renewable')],
                         by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'))
    in_state_ghg[, total := traditional + `residual renewable` + `main renewable` ]
    
    in_state_ghg = melt(in_state_ghg,
                        id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'),
                        measure.vars = c('traditional', 'residual renewable', 'main renewable', 'total'),
                        variable.name = 'source',
                        value.name = 'value')

    in_state_ghg[, fuel := 'crude']
    in_state_ghg[, boundary := 'in-state']
    in_state_ghg[, type := 'ghg']
    in_state_ghg[, units := 'kg']
    
    setcolorder(in_state_ghg, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year',  
                                'fuel', 'source', 'boundary', 'type', 'units', 'value'))
    
    # set factor level order
    
    in_state_ghg[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable',  'total'))]
    in_state_ghg[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
    in_state_ghg[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
    in_state_ghg[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
    
    setorder(in_state_ghg, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, source)
    
    
  # organize outputs for out-of-state emissions ------
    
    out_state_ghg = boundary_trad[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, out_of_state_ghg_kg)]
    setnames(out_state_ghg, 'out_of_state_ghg_kg', 'traditional')
    out_state_ghg = merge(out_state_ghg,
                          state_ghg_wide[, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 
                                             'ccs_scenario', 'year', 'residual renewable', 'main renewable')],
                          by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'))
    out_state_ghg[, `residual renewable` := 0]
    out_state_ghg[, `main renewable` := 0]
    out_state_ghg[, total := traditional + `residual renewable` + `main renewable` ]
    
    out_state_ghg = melt(out_state_ghg,
                         id.vars = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year'),
                         measure.vars = c('traditional', 'residual renewable', 'main renewable', 'total'),
                         variable.name = 'source',
                         value.name = 'value')
    
    out_state_ghg[, fuel := 'crude']
    out_state_ghg[, boundary := 'out-of-state']
    out_state_ghg[, type := 'ghg']
    out_state_ghg[, units := 'kg']
    
    setcolorder(out_state_ghg, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 'year',  
                                'fuel', 'source', 'boundary', 'type', 'units', 'value'))
    
    # set factor level order
    
    out_state_ghg[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable',  'total'))]
    out_state_ghg[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
    out_state_ghg[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
    out_state_ghg[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
    
    setorder(out_state_ghg, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, source)

  # ----- check if aggregated production matches with earlier demand plot ------
      
    # state_prod = indiv_prod[, .(production_bbl = sum(production_bbl, na.rm = T),
    #                             production_bge = sum(production_bge, na.rm = T)), 
    #                            by = .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, fuel)]
    # state_prod[fuel %like% 'gasoline', production_bge := production_bbl]
    # state_prod[fuel %like% 'diesel', production_bge := production_bbl * (ei_diesel/ei_gasoline)]
    # state_prod[fuel %like% 'jet' | fuel %like% 'aviation', production_bge := production_bbl * (ei_jet/ei_gasoline)]
      
    # check_state_prod = unique(state_prod[, .(demand_scenario, refining_scenario, year, fuel, production_bge)])
      # ggplot(check_state_prod, aes(x = year, y = production_bge/1e6, fill = fuel)) +
      #   geom_area() +
      #   facet_wrap(demand_scenario ~ refining_scenario, ncol = 2,
      #              labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
      #                                                        'historic production' = 'Historic Production'))) +
      #   labs(title = 'Annual production at all refineries by fuel (2014-2045)',
      #        subtitle = 'Million barrels of gasoline equivalent',
      #        x = NULL,
      #        y = NULL,
      #        fill = NULL,
      #        linetype = NULL) +
      #   scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      #   scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 50)) + 
      #   scale_fill_manual(values = pal_fuel) + 
      #   theme_line +
      #   theme(legend.position = 'right')
      # 
      # ggplot(tot_fuel_demand, aes(x = year, y = consumption_bge/1e6, fill = fuel)) +
      #   geom_area() +
      #   facet_wrap(demand_scenario ~ refining_scenario, ncol = 2,
      #              labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
      #                                                        'historic production' = 'Historic Production'))) +
      #   labs(title = 'Annual production at all refineries by fuel (2014-2045)',
      #        subtitle = 'Million barrels of gasoline equivalent',
      #        x = NULL,
      #        y = NULL,
      #        fill = NULL,
      #        linetype = NULL) +
      #   scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      #   scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 50)) + 
      #   scale_fill_manual(values = pal_fuel) + 
      #   theme_line +
      #   theme(legend.position = 'right')


      
  # join outputs at each spatial resolution -------
    
    outputs_indiv = rbindlist(list(indiv_prod_output, indiv_cons_output, indiv_ghg_output), use.names = T, fill = T)
    outputs_clus = rbindlist(list(clus_prod_output, clus_cons_output, clus_ghg_output), use.names = T, fill = T)
    outputs_state = rbindlist(list(state_prod_output, state_cons_output, state_ghg_output, in_state_ghg, out_state_ghg), use.names = T, fill = T)
    
  # add innovation multipliers to outputs -------
    
    un_mult = unique(dt_scen[, .(year, innovation_scenario, innovation_multiplier)])
    
    outputs_indiv = outputs_indiv[un_mult, on = .(year, innovation_scenario)]
    outputs_clus = outputs_clus[un_mult, on = .(year, innovation_scenario)]
    outputs_state = outputs_state[un_mult, on = .(year, innovation_scenario)]
    
    setcolorder(outputs_indiv, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'innovation_multiplier', 'carbon_price_scenario', 'ccs_scenario', 
                                 'site_id', 'refinery_name', 'location', 'region', 'cluster', 'year', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
    setcolorder(outputs_clus, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'innovation_multiplier', 'carbon_price_scenario', 'ccs_scenario', 
                                'cluster', 'year', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))
    setcolorder(outputs_state, c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'innovation_multiplier', 'carbon_price_scenario', 'ccs_scenario', 
                                 'year', 'fuel', 'source', 'boundary', 'type', 'units', 'value'))

# -------------------- more plots  -------------------------
    
  # plot: cluster level crude consumption in scenarios -----------
    
    scen_combos = CJ(demand_scenario = dem_scens, refining_scenario = ref_scens, innovation_scenario = c('low innovation', 'high innovation'))
    
    for (i in 1:nrow(scen_combos)) {
      
      scen = scen_combos[i]
      scen_title = paste(unlist(t(scen)),collapse=" + ")
      scen_name = paste(unlist(t(scen)), collapse = '_')
      scen_name = gsub(' ', '_', scen_name)
      scen_dir = paste(scen_name, collapse = "_")
      
      fname = paste0(scen_dir, '_cluster_crude_consumption.pdf')
      
      dt = clus_cons_output[scen, on = .(demand_scenario, refining_scenario, innovation_scenario), nomatch = 0]
      dt = dt[! source == 'total']
      
      dt[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable'))]
      dt[, cluster := factor(cluster, levels = c('Bay Area', 'Bakersfield', 'South'))]
      dt[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
      dt[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
      dt[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
      setorder(dt, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, cluster, source)
      
      fig_scen_crude = ggplot(dt, aes(x = year, y = value/1e6, fill = source)) +
        geom_area() +
        facet_grid(cluster ~ carbon_price_scenario + ccs_scenario, scales = 'free_y') +
        labs(title = paste0('Annual crude consumption ', scen_title, ' scenario (2020-2045)'),
             subtitle = 'Million barrels',
             x = NULL,
             y = NULL,
             fill = NULL) +
        guides(col = guide_legend(ncol = 2)) +
        scale_x_continuous(breaks = seq(2020,2040,10), limits = c(2020, 2045), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = pal_crude) +
        theme_line +
        theme(strip.text = element_text(size = 16, face = 'bold'))
      
      ggsave(fig_scen_crude,
             filename = file.path(save_path, 'scenarios_crude', fname),
             width = 21,
             height = 11)
      
      embed_fonts(file.path(save_path, 'scenarios_crude', fname),
                  outfile = file.path(save_path, 'scenarios_crude', fname))
      
      rm(scen, scen_dir, fname, fig_scen_crude)
      
      
    }
    
    
  # plot: cluster level ghg emissions in scenarios -----------
    
    scen_combos = CJ(demand_scenario = dem_scens, refining_scenario = ref_scens, innovation_scenario = c('low innovation', 'high innovation'))
    
    for (i in 1:nrow(scen_combos)) {
      
      # scen = unique(scenarios_dt[, oil_price_scenario])[i]
      # scen_dir = gsub(' ', '_', scen)
      scen = scen_combos[i]
      # scen_title = paste(unlist(t(scen))[1:2],collapse = " + ")
      scen_title = paste(unlist(t(scen)),collapse=" + ")
      # scen_title = gsub('_', ' ', scen_title)
      scen_name = paste(unlist(t(scen)), collapse = '_')
      scen_name = gsub(' ', '_', scen_name)
      scen_dir = paste(scen_name, collapse = "_")
      
      fname = paste0(scen_dir, '_cluster_ghg_emissions.pdf')
      
      dt = clus_ghg_output[scen, on = .(demand_scenario, refining_scenario, innovation_scenario), nomatch = 0]
      dt = dt[! source == 'total']
      
      dt[, source := factor(source, levels = c('traditional', 'residual renewable', 'main renewable'))]
      dt[, cluster := factor(cluster, levels = c('Bay Area', 'Bakersfield', 'South'))]
      dt[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
      dt[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
      dt[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
      setorder(dt, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, year, cluster, source)
      
      fig_scen_ghg = ggplot(dt, aes(x = year, y = value/1e9, fill = source)) +
        geom_area() +
        facet_grid(cluster ~ carbon_price_scenario + ccs_scenario, scales = 'free_y') +
        labs(title = paste0('Annual GHG emissions in the ', scen_title, ' scenario (2020-2045)'),
             subtitle = 'Million tonnes',
             x = NULL,
             y = NULL,
             fill = NULL) +
        guides(col = guide_legend(ncol = 2)) +
        scale_x_continuous(breaks = seq(2020,2040,10), limits = c(2020, 2045), expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = pal_crude) +
        theme_line +
        theme(strip.text = element_text(size = 16, face = 'bold'))
      
      ggsave(fig_scen_ghg,
             filename = file.path(save_path, 'scenarios_ghg', fname),
             width = 21,
             height = 11)
      
      embed_fonts(file.path(save_path, 'scenarios_ghg', fname),
                  outfile = file.path(save_path, 'scenarios_ghg', fname))
      
      rm(scen, scen_dir, fname, fig_scen_ghg)
      
      
    }
    
    
  # save outputs ----------
      
    # fwrite(outputs_indiv, file.path(output_path, 'refining_scenario_outputs_refinery_net_exports.csv'), row.names = F)
    # fwrite(outputs_clus, file.path(output_path, 'refining_scenario_outputs_cluster_net_exports.csv'), row.names = F)
    # fwrite(outputs_state, file.path(output_path, 'refining_scenario_outputs_state_net_exports.csv'), row.names = F)
