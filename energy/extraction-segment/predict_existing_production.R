## meas meng
## september 22, 2020
## predict production from existing wells with exit

# inputs ------

data_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/decline-historic/'
prod_file       = 'data/production_api10_yearly_start_year.csv' # meas-note: update to use "production_api10_yearly_start_year.csv"
param_file      = 'parameters/fitted-parameters_field-start-year_yearly_entry.csv' # meas-note: update to use "fitted-parameters_field-start-year_yearly_entry.csv"
peak_file       = 'data/field-year_peak-production_yearly.csv' # meas-note: update to use "field-year_peak-production_yearly.csv"
prod_adj_file   = 'data/adj_val_field-year_pred_prod.csv'
setback_path    = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/'
w_setback_file  = 'wells_in_setbacks_revised.csv'


# outputs ------

save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/existing_production/'

# load libraries -------- 

library(data.table)
library(tidyverse)

# read in data ------

dt_prod = fread(paste0(data_path, prod_file), header = T, colClasses = c('api_ten_digit' = 'character',
                                                                         'doc_field_code' = 'character'))

pred_adj_vals = fread(paste0(data_path, prod_adj_file), header = T, colClasses = c("doc_field_code" = "character"))

decline_params = fread(paste0(data_path, param_file), header = T, colClasses = c('doc_field_code' = 'character'))

peak_prod = fread(paste0(data_path, peak_file), header = T, colClasses = c('doc_field_code' = 'character'))

well_setbacks = fread(paste0(setback_path, w_setback_file), header = T, colClasses = c('api_ten_digit' = 'character'))

# get fields that produced oil in recent years and the number of wells in each vintage that produced oil -----

dt_prod[, year := as.numeric(substr(ProductionReportDate,1,4))]

op_wells = unique(dt_prod[, c('api_ten_digit', 'start_year', 'doc_fieldname', 'doc_field_code')]) # meas-note: probably have to change the 'vintage' to 'start_year'

## how many entries per well?
View(op_wells[, .N, by = api_ten_digit][N > 1])
## note: there are wells with multiple entries (in more than one field)

## predict production with all existing wells -------------------------------
## --------------------------------------------------------------------------

# functions ------ 

hypfunc = function(b,t,q_i,D_h) { q_i/((1 + b*D_h*t)^(1/b)) }
expfunc = function(q_i,d,t) {   q_i*exp(-d*t) }

# set years -------

fullrange = seq(2020, 2045,1)

# match parameters ------

# merge op_wells with the decline params
# dt_pred = merge(op_wells, decline_params, by = c('doc_field_code', 'doc_fieldname', 'start_year'), all.x = T)
# 
# setDT(dt_pred)
# 
# dt_pred = dt_pred[, c('doc_field_code', 'doc_fieldname', 'start_year', 'setback_scenario', 'no_wells', 'adj_no_wells',
#                       'q_i', 'D', 'b', 'd', 'int_yr', 'peak_prod_year', 'peak_tot_prod', 'peak_avg_well_prod', 'peak_well_prod_rate', 'type')]

dt_pred = copy(decline_params)


# loop to calculate production at each year -----

for (i in seq_along(fullrange) ) {
  
  y = fullrange[i]
  
  # dt_pred[is.na(b2), col := hypfunc(b1, y - start_year, peak_tot_prod, D)] # meas-note: the updated parameters file only has b now (no b1 or b2 or anything), so you could probably comment out this line and replace mentions of b2 with b and d2 with d
  dt_pred[is.na(b), col := expfunc(peak_tot_prod, d, y - start_year)] 
  dt_pred[! is.na(b), col :=  ifelse(y < 1978 + int_yr,
                                     hypfunc(b, y - start_year, peak_tot_prod, D),
                                     expfunc(peak_tot_prod, d, y - start_year))]
  
  colnames(dt_pred)[ncol(dt_pred)] = y
}

# convert to long ------

dt_pred_long = melt(dt_pred, id.vars = c('doc_fieldname', 'doc_field_code', 'start_year', 'no_wells', 'q_i', 'D', 'b', 'd', 'int_yr'),
                    measure.vars = as.character(fullrange), variable.name = 'year', value.name = 'production_bbl')

setorderv(dt_pred_long, c('doc_field_code', 'year', 'start_year'))

dt_pred_long[, c('q_i', 'D', 'b', 'd', 'int_yr') := NULL]


## DO NOT ADJUST NOW.
# ## incorporate plugged wells by reducing projected bbl using adj values
# dt_pred_long <- merge(dt_pred_long, pred_adj_vals,
#                       by = c("doc_field_code", "start_year"),
#                       all.x = T)
# 
# dt_pred_long[, production_bbl_adj := production_bbl * non_plug_rel_prod]


## now incorporate setbacks 
## ---------------------------------------------

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
  ## make well status for NA "Unknown_NL" for unknown, not listed
  mutate(well_status = ifelse(is.na(well_status), "Unknown_NL", well_status)) %>%
  ## assume NA means not in setback (note that there are about 20 wells that are na)
  mutate(within_setback = ifelse(is.na(within_setback), 0, within_setback),
         active_well = ifelse(well_status == "Plugged", 0, 1),
         prod_post_setback = ifelse(within_setback == 0, 1, 0),
         prod_post_setback_plug = ifelse(within_setback == 0 & active_well == 1, 1, 0)) %>%
  group_by(setback_scenario, doc_field_code, doc_fieldname, start_year) %>% 
  ## calculate total number of wells, active wells (- plugged), active wells post setback (- plugged)
  summarise(n_wells_total = n(),
            n_active_wells = sum(active_well),
            n_not_in_setback = sum(prod_post_setback),
            n_not_setback_active = sum(prod_post_setback_plug)) %>%
  ungroup()

## clean for joining  -- adjust this depending on version
op_wells_agg2 <- op_wells_agg %>%
  select(-n_wells_total, -n_not_in_setback) 

## join with dt_pred_long, adjust production to account for setbacks and plugged wells
dt_pred_long_adj <- merge(op_wells_agg2, dt_pred_long, by = c('doc_field_code', 'doc_fieldname', 'start_year'), all.x = T)

setcolorder(dt_pred_long_adj, c("doc_field_code", "doc_fieldname", "setback_scenario", "start_year", "no_wells", 
                                "n_active_wells", "n_not_setback_active", "year", "production_bbl"))

setDT(dt_pred_long_adj)

## calculate prod per bbl, use the number of non-plugged wells
dt_pred_long_adj[, prod_per_bbl := fifelse(n_active_wells == 0, 0, production_bbl / n_active_wells)]

## account for setbacks -- multiple by number of active and non-setback wells
dt_pred_long_adj[, production_bbl_sb := prod_per_bbl * n_not_setback_active]

## use production_bbl_sb (rename to production_bbl) & n_not_setback_active (rename to adj_no_wells)
dt_pred_long_adj <- dt_pred_long_adj[, c('doc_field_code', 'doc_fieldname', 'setback_scenario', 'start_year', 'no_wells', 'n_not_setback_active', 'year', 'production_bbl_sb')]

## rename
setnames(dt_pred_long_adj, c('n_not_setback_active', 'production_bbl_sb'), c('adj_no_wells', 'production_bbl'))


## save production without exit
fwrite(dt_pred_long_adj, paste0(save_path, 'pred_prod_no_exit_2020-2045_field_start_year_revised.csv'))

# aggregate predicted production -----

# agg_prod_field = dt_pred_long[, .(production_bbl = sum(production_bbl, na.rm = T)),  by = .(setback_scenario, doc_field_code, doc_fieldname, year)]
# agg_prod_field_start_year = dt_pred_long[, .(production_bbl = sum(production_bbl, na.rm = T)),  by = .(setback_scenario, doc_field_code, doc_fieldname, start_year, year)]


