## meas meng
## september 22, 2020
## predict production from existing wells with exit

# inputs ------

  data_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/decline-historic/'
  prod_file       = 'data/production_api10_yearly_start_year.csv' # meas-note: update to use "production_api10_yearly_start_year.csv"
  param_file      = 'parameters/fitted-parameters_field-start-year_yearly_entry.csv' # meas-note: update to use "fitted-parameters_field-start-year_yearly_entry.csv"
  peak_file       = 'data/field-year_peak-production_yearly.csv' # meas-note: update to use "field-year_peak-production_yearly.csv"
  exit_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/'
  exit_file       = 'well_exit_volume_x_field_v1_revised.csv'
  setback_path    = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/'
  w_setback_file  = 'wells_in_setbacks_revised.csv'

# outputs ------
  
  save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/production_with_exit/'
  save_exit_path  = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/exit/'
  
# load libraries -------- 
  
  library(data.table)
  library(tidyverse)
  
# read in data ------
  
  dt_prod = fread(paste0(data_path, prod_file), header = T, colClasses = c('api_ten_digit' = 'character',
                                                                           'doc_field_code' = 'character'))
  
  exit_prod = fread(paste0(exit_path, exit_file), header = T, colClasses = c('doc_field_code' = 'character'))
  
  decline_params = fread(paste0(data_path, param_file), header = T, colClasses = c('doc_field_code' = 'character'))
  
  peak_prod = fread(paste0(data_path, peak_file), header = T, colClasses = c('doc_field_code' = 'character'))
  
  well_setbacks = fread(paste0(setback_path, w_setback_file), header = T, colClasses = c('api_ten_digit' = 'character'))
  
# # rename field code columns -----
#   
#   setnames(decline_params, 'FieldCode', 'doc_field_code')
#   setnames(decline_params, 'FieldName', 'doc_fieldname')
  
# pad field codes to width = 3 with zeroes -----
  
  # dt_prod[, doc_field_code := sprintf("%03d", doc_field_code)]
  # exit_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  # decline_params[, doc_field_code := sprintf("%03d", doc_field_code)]
  # peak_prod[, doc_field_code := sprintf("%03d", doc_field_code)]

# remove fields with " Gas" in the name
  # dt_prod = dt_prod[!grepl(" Gas", dt_prod$doc_fieldname),]
  
# get fields that produced oil in recent years and the number of wells in each vintage that produced oil -----
  
  dt_prod[, year := as.numeric(substr(ProductionReportDate,1,4))]
  
  ## original code
  # op_wells = unique(dt_prod[, c('api_ten_digit', 'vintage', 'doc_fieldname', 'doc_field_code', 'year', 'oil_prod')])
  # op_wells = op_wells[year %in% c(2015:2019)]
  
  op_wells = unique(dt_prod[, c('api_ten_digit', 'start_year', 'doc_fieldname', 'doc_field_code')]) # meas-note: probably have to change the 'vintage' to 'start_year'
  ## note: there are wells with multiple entries (in more than one field)
  ## example: 0402909443
  
  ## setback df with all wells 
  setback_all <- expand.grid(api_ten_digit = unique(op_wells$api_ten_digit),
                             setback_scenario = unique(well_setbacks$setback_scenario))
  
  ## join with setback policies
  op_wells = left_join(op_wells, setback_all)
  
  ## get number of wells not in setback for calculating density
  n_wells_area <- op_wells %>%
    ## join with setback info
    left_join(well_setbacks) %>%
    ## assume NA means not in setback (note that there are about 20 wells that are na)
    mutate(within_setback = ifelse(is.na(within_setback), 0, within_setback)) %>%
    group_by(setback_scenario, doc_field_code, doc_fieldname) %>%
    summarise(n_wells = n(),
              n_wells_in_setback = sum(within_setback)) %>%
    ungroup() %>%
    mutate(adj_no_wells = n_wells - n_wells_in_setback)
  
  fwrite(n_wells_area, paste0(save_path, 'n_wells_area.csv'))
  
  ## create input for predict production
  op_wells_agg <- op_wells %>%
    ## join with setback info
    left_join(well_setbacks) %>%
    ## assume NA means not in setback (note that there are about 20 wells that are na)
    mutate(within_setback = ifelse(is.na(within_setback), 0, within_setback)) %>%
    ## filter out plugged wells
    filter(well_status != "Plugged") %>%
    ## number of wells in each field vintage by setback scenario
    group_by(setback_scenario, doc_field_code, doc_fieldname, start_year) %>% # meas-note: again, most instances of "vintage" will probably have to be replaced with "start_year"
    summarise(n_wells = n(),
              n_wells_in_setback = sum(within_setback)) %>%
    ungroup() %>%
    mutate(adj_no_wells = n_wells - n_wells_in_setback) %>%
    dplyr::select(-n_wells, -n_wells_in_setback)
  
  ## create version with n wells and adj n wells for density calculation
  n_well_df <- op_wells %>%
    ## join with setback info
    left_join(well_setbacks) %>%
    ## assume NA means not in setback
    mutate(within_setback = ifelse(is.na(within_setback), 0, within_setback)) %>%
    ## filter out plugged wells
    filter(well_status != "Plugged") %>%
    ## number of wells in each field vintage by setback scenario
    group_by(setback_scenario, doc_field_code, doc_fieldname, start_year) %>%
    summarise(n_wells = n(),
              n_wells_in_setback = sum(within_setback)) %>%
    ungroup() %>%
    mutate(adj_no_wells = n_wells - n_wells_in_setback) 
  
  
  fwrite(n_well_df, paste0(save_path, 'n_well_setback_revised.csv'))
  
  ## original objv
  # op_wells_agg = op_wells[, .(annual_bbl_recent = sum(as.numeric(oil_prod), na.rm = T),
  #                             op_no_wells = uniqueN(api_ten_digit, na.rm = T)),  by = .(doc_field_code, doc_fieldname, vintage)]

# functions ------ 
  
  hypfunc = function(b,t,q_i,D_h) { q_i/((1 + b*D_h*t)^(1/b)) }
  expfunc = function(q_i,d,t) {   q_i*exp(-d*t) }
  
# set years -------
  
  fullrange = seq(2020, 2045,1)
  
# match parameters ------
  
  # get fields that have been producing in past 5 years
    dt_pred = merge(op_wells_agg, decline_params, by = c('doc_field_code', 'doc_fieldname', 'start_year'), all.x = T)
    
    setDT(dt_pred)
  
    dt_pred = dt_pred[, c('doc_field_code', 'doc_fieldname', 'start_year', 'setback_scenario', 'no_wells', 'adj_no_wells',
                          'q_i', 'D', 'b', 'd', 'int_yr', 'peak_prod_year', 'peak_tot_prod', 'peak_avg_well_prod', 'peak_well_prod_rate', 'type')]
  
    
  # match peak production information
    # dt_pred = merge(dt_pred,
    #                 peak_prod[, c('doc_field_code', 'doc_fieldname', 'vintage', 'peak_avg_well_prod')],
    #                 by = c('doc_field_code', 'doc_fieldname', 'vintage'),
    #                 all.x = T)
   
    ## original code
    # dt_pred[, peak_tot_prod := peak_avg_well_prod * no_wells]
    
    ## multiply by adj_no_wells (incorporates setbacks)
    dt_pred[, peak_tot_prod := peak_avg_well_prod * adj_no_wells]
    
  # match exit production information
    dt_pred = merge(dt_pred,
                    exit_prod[, c('doc_field_code', 'mean_final_yr_prod_adj')],
                    by = 'doc_field_code',
                    all.x = T)
    dt_pred[is.na(mean_final_yr_prod_adj), mean_final_yr_prod_adj := 0]
    # dt_pred[, fv_final_year_prod := mean_final_yr_prod_adj * no_wells]
    dt_pred[, fv_final_year_prod := mean_final_yr_prod_adj * adj_no_wells]

  # track no. of wells that exit from each field in each year - RL
    dt_pred$exit_year <- 0
      
# loop to calculate production at each year -----
    
  for (i in seq_along(fullrange) ) {
    
    y = fullrange[i]
    
    # dt_pred[is.na(b2), col := hypfunc(b1, y - start_year, peak_tot_prod, D)] # meas-note: the updated parameters file only has b now (no b1 or b2 or anything), so you could probably comment out this line and replace mentions of b2 with b and d2 with d
    dt_pred[is.na(b), col := expfunc(peak_tot_prod, d, y - start_year)] 
    dt_pred[! is.na(b), col :=  ifelse(y < 1978 + int_yr,
                                        hypfunc(b, y - start_year, peak_tot_prod, D),
                                        expfunc(peak_tot_prod, d, y - start_year))  ]
    
    # Track no. wells that exit
    dt_pred[, exit_year := ifelse(exit_year==0, ifelse(col < fv_final_year_prod,
                                                  y,
                                                  exit_year),
                                  exit_year)
                            ]
    dt_pred[, col := ifelse(col < fv_final_year_prod,
                            0,
                            col)]
    colnames(dt_pred)[ncol(dt_pred)] = y
  }

# extract well exits - RL
    well_exits <- dt_pred[, c('doc_field_code', 'doc_fieldname', 'start_year', 'setback_scenario', 'no_wells', 'adj_no_wells', 'exit_year')]
    well_exits[, no_exits := ifelse(exit_year==0, 0, adj_no_wells)]
    
    well_exits1 <- well_exits[, .(no_exits_field = sum(no_exits)),  by = .(setback_scenario, doc_field_code, doc_fieldname, exit_year)]
    fwrite(well_exits1, paste0(save_exit_path, 'well_exits_under_rule_forecast.csv'))

    dt_pred <- dt_pred [, exit_year:=NULL]        
    
# convert to long ------
  
  dt_pred_long = melt(dt_pred, id.vars = c('doc_field_code', 'doc_fieldname', 'setback_scenario', 'start_year', 'q_i', 'D', 'b', 'd', 'int_yr'),
                      measure.vars = as.character(fullrange), variable.name = 'year', value.name = 'production_bbl')
  
  setorderv(dt_pred_long, c('setback_scenario', 'doc_field_code', 'year', 'start_year'))
    
# aggregate predicted production -----
  
  agg_prod_field = dt_pred_long[, .(production_bbl = sum(production_bbl, na.rm = T)),  by = .(setback_scenario, doc_field_code, doc_fieldname, year)]
  agg_prod_field_start_year = dt_pred_long[, .(production_bbl = sum(production_bbl, na.rm = T)),  by = .(setback_scenario, doc_field_code, doc_fieldname, start_year, year)]
  
# save csv -------
  
  # fwrite(agg_prod_field, paste0(save_path, 'predicted-production_2020-2045_field.csv'))
  # fwrite(agg_prod_field_vintage, paste0(save_path, 'predicted-production_2020-2045_field_vintage.csv'))

  ## FOR NOW, SAVING TEST
  ## UPDATE THIS LATER
  
  fwrite(agg_prod_field, paste0(save_path, 'predicted-production_2020-2045_field_revised.csv'))
  fwrite(agg_prod_field_start_year, paste0(save_path, 'predicted-production_2020-2045_field_start_year.csv'))

## test figure  
# test <- agg_prod_field %>%
#   group_by(setback_scenario, year) %>%
#   summarise(prod = sum(production_bbl)) %>%
#   ungroup() %>%
#   mutate(year = as.numeric(year))
# 
# ggplot(test, aes(x = year, y = prod, color = setback_scenario)) +
#   geom_line()
