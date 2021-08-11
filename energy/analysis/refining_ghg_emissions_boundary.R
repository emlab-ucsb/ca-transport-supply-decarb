# inputs ---------

  proj_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
  its_file        = 'its_demand_bau_and_lc1_2020_2045.csv'
  jet_file        = 'cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv'
  intra_file      = 'its_demand_intrastate_jet_2020_2045.csv'
  fpm_file        = 'finished_product_movements_weekly_cec.csv'
  fw_file         = 'fuel_watch_data.csv'
  ghg_file        = 'refinery_ghg_factor_x_indiv_refinery_revised.csv'
  ei_crude        = 5.698               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_3.pdf
  ei_gasoline     = 5.052               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_4.pdf
  ei_diesel       = 5.770               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf
  ei_jet          = (5.670 + 5.355)/2   # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf

# outputs ---------

  output_path = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  output_file = 'refining_emissions_state_2019_revised.csv'

# libraries ------

  library(data.table)
  library(lubridate)
  library(openxlsx)

# load all data -----

  # read in its demand forecast
    dt_its = fread(file.path(proj_path, 'outputs/fuel-demand/prelim-results', its_file), header = T)
  
  # read in cec jet demand forecast
    dt_jet = fread(file.path(proj_path, 'outputs/fuel-demand/prelim-results', jet_file), header = T)
  
  # read in intrastate jet fuel demand forecast
    dt_intra = fread(file.path(proj_path, 'outputs/fuel-demand/prelim-results', intra_file), header = T)
    dt_intra[, consumption_bbl := (consumption_gge / 42) * (ei_gasoline / ei_jet)]
    dt_intra = dt_intra[year >= 2018 & year < 2020]

  # read in fuel movements data
    dt_fpm = fread(file.path(proj_path, 'data/stocks-flows/processed', fpm_file), header = T)
  
  # read in fuel production data
    dt_prod = fread(file.path(proj_path, 'data/stocks-flows/processed', fw_file), header = T)
    
  # read in refinery and region emission factors
    dt_ghg = fread(file.path(proj_path, 'outputs/stocks-flows', ghg_file), header = T)
    ghg_region = unique(dt_ghg[, .(year, region, region_barrels, region_co2e_kg, region_kgco2e_bbl)])
    ghg_region_2018 = ghg_region[year == 2018]
    
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
    
# aggregate crude and refined products by year -----
  
  crude_refined_annual = crude_refined_week[year %in% 2014:2019, lapply(.SD, sum, na.rm = T), 
                                            by = c('region', 'year'), 
                                            .SDcols = c('crude_bbl', 'gasoline', 'diesel', 'jet', 'residual')]
  setnames(crude_refined_annual, 'crude_bbl', 'complete_crude_bbl')
  setnames(crude_refined_annual, 'gasoline', 'complete_gasoline_bbl')
  setnames(crude_refined_annual, 'diesel', 'complete_diesel_bbl')
  setnames(crude_refined_annual, 'jet', 'complete_jet_bbl')
  
# calculate ghg emissions -------
  
  crude_refined_ghg = crude_refined_annual[ghg_region_2018[, .(region, region_kgco2e_bbl)], on = .(region)]
  crude_refined_ghg[, complete_ghg_kg := complete_crude_bbl * region_kgco2e_bbl]
  
  crude_refined_ghg = crude_refined_ghg[, lapply(.SD, sum, na.rm = T), 
                                        by = c('year'), 
                                        .SDcols = c('complete_crude_bbl', 'complete_gasoline_bbl', 'complete_diesel_bbl', 'complete_jet_bbl', 'complete_ghg_kg')]
  
# get refined product exports ------
  
  refined_exports_month = dt_fpm[code %like% 'E$']
  refined_exports_month[, year := year(ymd(date))]
  refined_exports_month[location == 'north', region := 'North']
  refined_exports_month[location == 'south', region := 'South']

# get annual exports ------
  
  refined_exports_annual = refined_exports_month[, .(export_bbl = sum(thous_bbl*1e3, na.rm = T)), by = .(fuel, year)]
  refined_exports_annual[, fuel := factor(fuel, levels = c('gasoline', 'diesel', 'jet'))]
  refined_exports_annual[, export_bbl := abs(export_bbl)]
  refined_exports_annual_wide = dcast(refined_exports_annual, year ~ fuel, value.var = 'export_bbl')
  setnames(refined_exports_annual_wide, 'gasoline', 'export_gasoline_bbl')
  setnames(refined_exports_annual_wide, 'diesel', 'export_diesel_bbl')
  setnames(refined_exports_annual_wide, 'jet', 'export_jet_bbl')
  
# combine production with exports and intrastate jet ------
  
  ghg_sep = crude_refined_ghg[refined_exports_annual_wide, on = .(year), nomatch = 0]
  ghg_sep = ghg_sep[dt_intra[, .(year, consumption_bbl)], on = .(year), nomatch = 0]
  setnames(ghg_sep, 'consumption_bbl', 'intrastate_jet_bbl')
  
  ghg_sep[, interstate_jet_bbl := complete_jet_bbl - intrastate_jet_bbl - export_jet_bbl]
  
  
  ghg_sep[, out_of_state_prop := ((interstate_jet_bbl*ei_jet) + 
                                    (export_gasoline_bbl*ei_gasoline) + 
                                    (export_diesel_bbl*ei_diesel) + 
                                    (export_jet_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) + (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
  
  ghg_sep[, in_state_prop := ((complete_gasoline_bbl - export_gasoline_bbl)*(ei_gasoline) + (complete_diesel_bbl - export_diesel_bbl)*(ei_diesel) +
                                      (intrastate_jet_bbl*ei_jet))/((complete_gasoline_bbl*ei_gasoline) + 
                                                                      (complete_diesel_bbl*ei_diesel) + (complete_jet_bbl*ei_jet))]
  
  ghg_sep[, out_of_state_ghg_kg := out_of_state_prop * complete_ghg_kg]
  ghg_sep[, in_state_ghg_kg := in_state_prop * complete_ghg_kg]
  
  
# convert from wide to long -------
  
  state_ghg = melt(ghg_sep, id.vars = 'year',
                   measure.vars = c('complete_ghg_kg', 'in_state_ghg_kg', 'out_of_state_ghg_kg'),
                   variable.name = 'boundary',
                   value.name = 'value')
  state_ghg[, boundary := gsub('_ghg_kg', '', boundary)]  
  state_ghg[boundary == 'in_state', boundary := 'in-state']
  state_ghg[boundary == 'out_of_state', boundary := 'out-of-state']
  
# add columns ------
  
  state_ghg[, fuel := 'crude']
  state_ghg[, source := 'total']
  state_ghg[, type := 'ghg']
  state_ghg[, units := 'kg']
  
  setcolorder(state_ghg, c('year',  'fuel', 'source', 'boundary', 'type', 'units', 'value'))
  
# save outputs ----------
  
  fwrite(state_ghg[year == 2019], file.path(output_path, output_file), row.names = F)
  