## meas meng
## september 22, 2020
## predict production from existing wells with exit

# inputs ------

  data_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/decline-historic/'
  prod_file       = 'data/production_api10_monthly_revised.csv'
  param_file      = 'parameters/fitted-parameters_field-vintage_yearly_entry.csv'
  peak_file       = 'data/field-vintage_peak-production_yearly_revised.csv'
  exit_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/'
  exit_file       = 'well_exit_volume_x_field_v1_revised.csv'
  setback_path    = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/'
  w_setback_file  = 'wells_in_setbacks_test.csv'

# outputs ------
  
  save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/production_with_exit/'
  
# load libraries -------- 
  
  library(data.table)
  library(tidyverse)
  
# read in data ------
  
  dt_prod = fread(paste0(data_path, prod_file), header = T)
  exit_prod = fread(paste0(exit_path, exit_file), header = T)
  decline_params = fread(paste0(data_path, param_file), header = T)
  peak_prod = fread(paste0(data_path, peak_file), header = T)
  well_setbacks = fread(paste0(setback_path, w_setback_file), header = T)
  
# rename field code columns -----
  
  setnames(dt_prod, 'FieldCode', 'doc_field_code')
  setnames(decline_params, 'FieldCode', 'doc_field_code')
  setnames(peak_prod, 'FieldCode', 'doc_field_code')
  
  setnames(dt_prod, 'FieldName', 'doc_fieldname')
  setnames(decline_params, 'FieldName', 'doc_fieldname')
  setnames(peak_prod, 'FieldName', 'doc_fieldname')
  
# pad field codes to width = 3 with zeroes -----
  
  dt_prod[, doc_field_code := sprintf("%03d", doc_field_code)]
  exit_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  decline_params[, doc_field_code := sprintf("%03d", doc_field_code)]
  peak_prod[, doc_field_code := sprintf("%03d", doc_field_code)]

# remove fields with " Gas" in the name
  dt_prod = dt_prod[!grepl(" Gas", dt_prod$doc_fieldname),]
  
# get fields that produced oil in recent years and the number of wells in each vintage that produced oil -----
  
  dt_prod[, year := as.numeric(substr(ProductionReportDate,1,4))]
  
  ## original code
  # op_wells = unique(dt_prod[, c('api_ten_digit', 'vintage', 'doc_fieldname', 'doc_field_code', 'year', 'oil_prod')])
  # op_wells = op_wells[year %in% c(2015:2019)]
  
  op_wells = unique(dt_prod[, c('api_ten_digit', 'vintage', 'doc_fieldname', 'doc_field_code')])
  
  ## setback df with all wells 
  setback_all <- expand.grid(api_ten_digit = unique(op_wells$api_ten_digit),
                             setback_scenario = unique(well_setbacks$setback_scenario))
  
  ## join with setback policies
  op_wells = left_join(op_wells, setback_all)

  op_wells_agg <- op_wells %>%
    ## join with setback info
    left_join(well_setbacks) %>%
    ## assume NA means not in setback
    mutate(in_setback = ifelse(is.na(in_setback), 0, in_setback)) %>%
    ## number of wells in each field vintage by setback scenario
    group_by(setback_scenario, doc_field_code, doc_fieldname, vintage) %>%
    summarise(n_wells = n(),
              n_wells_in_setback = sum(in_setback)) %>%
    ungroup() %>%
    mutate(adj_no_wells = n_wells - n_wells_in_setback) %>%
    select(-n_wells, -n_wells_in_setback)
  
  ## create version with n wells and adj n wells for density calculation
  n_well_df <- op_wells %>%
    ## join with setback info
    left_join(well_setbacks) %>%
    ## assume NA means not in setback
    mutate(in_setback = ifelse(is.na(in_setback), 0, in_setback)) %>%
    ## number of wells in each field vintage by setback scenario
    group_by(setback_scenario, doc_field_code, doc_fieldname, vintage) %>%
    summarise(n_wells = n(),
              n_wells_in_setback = sum(in_setback)) %>%
    ungroup() %>%
    mutate(adj_no_wells = n_wells - n_wells_in_setback) 
  
  
  fwrite(n_well_df, paste0(save_path, 'n_well_setbacks.csv'))

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
    dt_pred = merge(op_wells_agg, decline_params, by = c('doc_field_code', 'doc_fieldname', 'vintage'), all.x = T)
    
    dt_pred <- dt_pred %>%
      select(doc_field_code:setback_scenario, no_wells, adj_no_wells, q_i:peak_tot_prod, peak_avg_well_prod, peak_well_prod_rate) 
  
    dt_pred = as.data.table(dt_pred)
    dt_pred[, start_year := as.numeric(ifelse(vintage == 'pre 1978',
                                   1977,
                                   substr(vintage,1,4)))]
    
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

# loop to calculate production at each year -----
    
  for (i in seq_along(fullrange) ) {
    
    y = fullrange[i]
    
    dt_pred[is.na(b2), col := hypfunc(b1, y - start_year, peak_tot_prod, D)]
    dt_pred[! is.na(b2), col :=  ifelse(y < 1978 + int_yr,
                                        hypfunc(b2, y - start_year, peak_tot_prod, D),
                                        expfunc(peak_tot_prod, d, y - start_year))  ]
    dt_pred[, col := ifelse(col < fv_final_year_prod,
                            0,
                            col)]
    colnames(dt_pred)[ncol(dt_pred)] = y
    
  }
    
# convert to long ------
  
  dt_pred_long = melt(dt_pred, id.vars = c('doc_field_code', 'doc_fieldname', 'vintage', 'setback_scenario', 'start_year', 'q_i', 'D', 'b1', 'b2', 'd', 'int_yr'),
                      measure.vars = as.character(fullrange), variable.name = 'year', value.name = 'production_bbl')
  
  setorderv(dt_pred_long, c('setback_scenario', 'doc_field_code', 'year', 'start_year'))
    
# aggregate predicted production -----
  
  agg_prod_field = dt_pred_long[, .(production_bbl = sum(production_bbl, na.rm = T)),  by = .(setback_scenario, doc_field_code, doc_fieldname, year)]
  agg_prod_field_vintage = dt_pred_long[, .(production_bbl = sum(production_bbl, na.rm = T)),  by = .(setback_scenario, doc_field_code, doc_fieldname, vintage, year)]
  
# save csv -------
  
  # fwrite(agg_prod_field, paste0(save_path, 'predicted-production_2020-2045_field.csv'))
  # fwrite(agg_prod_field_vintage, paste0(save_path, 'predicted-production_2020-2045_field_vintage.csv'))

  ## FOR NOW, SAVING TEST
  ## UPDATE THIS LATER
  
  fwrite(agg_prod_field, paste0(save_path, 'predicted-production_2020-2045_field_test.csv'))
  fwrite(agg_prod_field_vintage, paste0(save_path, 'predicted-production_2020-2045_field_vintage_test.csv'))

## test figure  
# test <- agg_prod_field %>%
#   group_by(setback_scenario, year) %>%
#   summarise(prod = sum(production_bbl)) %>%
#   ungroup() %>%
#   mutate(year = as.numeric(year))
# 
# ggplot(test, aes(x = year, y = prod, color = setback_scenario)) +
#   geom_line()
