# prep data for field-yearly level decline parameters
# created: may 25, 2021
# author: meas meng

# inputs ------

  emlab_dir       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  prod_fil        = 'data/stocks-flows/processed/well_prod_m_processed.csv'
  init_fil        = 'outputs/stocks-flows/well_start_yr/well_start_prod_api10_revised.csv'
  entry_fil       = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  
# outputs ------
  
  save_dir        = 'outputs/decline-historic/data'
  peak_fy_fil     = 'field-year_peak-production_yearly.csv'
  prod_fy_fil     = 'production_field-year_yearly_entry.csv'
  well_fy_fil     = 'production_api10_yearly_start_year.csv'
  
  
# load libraries -------- 
  
  library(data.table)  
  
# read in data ------
  
  # monthly wellstar production data
    well_prod = fread(file.path(emlab_dir, prod_fil), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))
  
  # start year
    init_prod = fread(file.path(emlab_dir, init_fil), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))
    
  # entry df
    entry_df = fread(file.path(emlab_dir, entry_fil), header = T, colClasses = c('doc_field_code' = 'character'))
    
  # well status
    wells <- sf::st_read(file.path(emlab_dir, "data/GIS/raw/allwells_gis/Wells_All.shp")) %>% 
      dplyr::select(API, WellStatus) %>%
      unique() %>%
      as_tibble() %>%
      dplyr::select(api_ten_digit = API, well_status = WellStatus)
  
# get field code + field name matches -----

  field_code = unique(well_prod[, c('doc_field_code', 'doc_fieldname')])

# unique well-start dates ------

  init_prod[, start_year := as.numeric(substr(start_date, 1, 4))]
  ## find unique start date for wells that "start" in two different fields, same date
  ## remove entry with prod_bbl is NA or == 0
  ## should leave one well with two entries
  ## api-ten 0403702049 is the only one with two entries in init prod, two different fields
  init_prod = init_prod[month_year == start_date & prod_bbl > 0 & !is.na(prod_bbl)]  
  init_prod = unique(init_prod[, c('api_ten_digit', 'start_date', 'start_year')])

# remove Any Field and fields that do not produce oil in historic period -----
  
  pos_fields = well_prod[, .(total_bbl = sum(OilorCondensateProduced, na.rm = T)), by = doc_field_code]
  pos_fields = pos_fields[total_bbl > 0]
  
  well_prod = well_prod[!doc_field_code == '000']
  well_prod = well_prod[doc_field_code %chin% pos_fields[, doc_field_code]]

# add start year column -----
  
  entry_df[, start_year := year]
  
# match production data with well data for wells and their start dates ------
  
  well_prod2 = well_prod[init_prod, on = c('api_ten_digit'), nomatch = 0]
  
# keep only fields in entry data -----
  
  well_prod3 = well_prod2[doc_field_code %in% unique(entry_df[, doc_field_code])]
  ## 154 dropped
  
# aggregate oil production to the monthly level -------  
  
  well_prod3 = well_prod3[!is.na(OilorCondensateProduced)]
  well_prod3 = well_prod3[OilorCondensateProduced > 0]
  
  well_prod3 = well_prod3[, .(oil_prod = sum(OilorCondensateProduced, na.rm = T)), 
                          by = .(api_ten_digit, start_date, start_year, doc_field_code, doc_fieldname, ProductionReportDate, DaysProducing)]
  
  ## 0403702049 production gets duplicated, divide production by 2
  well_prod3 = well_prod3[, oil_prod := fifelse(api_ten_digit == "0403702049", oil_prod / 2, oil_prod)]

# add months to each API number -----
  
  # function to calculate number of months between dates 
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  well_prod3[, month_no := elapsed_months(ProductionReportDate, start_date), by = c('api_ten_digit', 'start_date', 'start_year', 'doc_fieldname', 'doc_field_code') ]
  
  well_prod3 = well_prod3[month_no > 0]
  well_prod3 = well_prod3[order(api_ten_digit, month_no)]
  well_prod3[, total_months := max(month_no, na.rm = T), by = 'api_ten_digit']
  
# cut number of months into number of year in production for each field-start year -------
  
  well_prod3[, year_no := cut(month_no, breaks = seq(0,516,12), labels = c(1:43), right = T)]
  
# count the number of unique wells in each field-vintage -----
  
  count_fv_wells = well_prod3[, .(no_wells = uniqueN(api_ten_digit)), by = .(doc_field_code, doc_fieldname, start_year)]
  count_fv_wells = setorderv(count_fv_wells, c('doc_field_code', 'start_year'))
  
# aggregate production by: field-vintage-year since start date ----  
  
  prod_fv_year = well_prod3[, .(tot_oil_prod = sum(oil_prod, na.rm = T)),  by = .(doc_fieldname, doc_field_code, start_year, year_no)]
  
  prod_fv_year = prod_fv_year[count_fv_wells, on = c('doc_fieldname', 'doc_field_code', 'start_year')]
  prod_fv_year[, well_prod := tot_oil_prod/no_wells ] # avg production per well for each year
  
  prod_fv_year = prod_fv_year[order(doc_fieldname, start_year, year_no)]
  prod_fv_year[, prod_rate := well_prod/365, by = c('doc_fieldname', 'doc_field_code', 'start_year')] # bbls/day
  prod_fv_year[, decline_rate := (prod_rate - shift(prod_rate))/prod_rate, by = c('doc_fieldname', 'doc_field_code', 'start_year')]
  
# get peak production year for each field-vintage -----
  
  # for start year = 1977, choose month no = 1 as peak month
    peak_fv_year_pre = prod_fv_year[start_year == 1977, .SD[which.min(year_no)], by = .(doc_fieldname, doc_field_code, start_year)]
  
  # for post-1977, choose peak production rate as peak year
    peak_fv_year_post = prod_fv_year[start_year > 1977, .SD[which.max(prod_rate)], by = .(doc_fieldname, doc_field_code, start_year)]
  
  # combine peak months for pre and post 1978
    peak_fv_year = rbindlist(list(peak_fv_year_pre, peak_fv_year_post))
    peak_fv_year = peak_fv_year[, 'decline_rate' := NULL ]
    
    colnames(peak_fv_year)[c(4:5,7:8)] = c('peak_prod_year', 'peak_tot_prod', 'peak_avg_well_prod', 'peak_well_prod_rate')

# check for instances where field-years in the entry dataset don't have matching peak prod data -----
    
  nonmatch = entry_df[!peak_fv_year, on = c('doc_field_code', 'start_year')]
  nonmatch = unique(nonmatch[, c('doc_field_code', 'start_year', 'n_new_wells')])
    
# merge yearly production data with peak production month info ----
  
  prod_fv_year_2 = prod_fv_year[peak_fv_year[,-'no_wells'], on = c('doc_fieldname', 'doc_field_code', 'start_year')]
  prod_fv_year_2[, peak_year_diff := as.numeric(year_no) - as.numeric(peak_prod_year)]

# calculate the percentage of production from plugged wells in peak year for each field vintage
  peak_prod_plugged <- unique(well_prod3[, .(api_ten_digit, start_year, doc_field_code)])
  
  peak_prod_plugged <- merge(peak_prod_plugged, peak_fv_year,
                             by = c("doc_field_code", "start_year"),
                             all.x = T)
  
  peak_prod_plugged <- peak_prod_plugged[, .(api_ten_digit, doc_field_code, doc_fieldname, start_year, peak_prod_year, peak_tot_prod)]
  
  peak_prod_plugged[, year_no := peak_prod_year]
  
  
  ## annual well production
  annual_well_prod <- well_prod3[, .(prod_bbls = sum(oil_prod, na.rm = T)), by = .(api_ten_digit, doc_field_code, start_year, year_no)]
  
  ## merge
  peak_prod_plugged <- merge(peak_prod_plugged, annual_well_prod,
                             by = c("api_ten_digit", "doc_field_code", "start_year", "year_no"),
                             all.x = T)
  
  ## check that sums are the same 
  # peak_prod_plugged[, peak_tot_prod_2 := sum(prod_bbls, na.rm = T), by = .(doc_field_code, doc_fieldname, start_year, year_no)]
  # View(peak_prod_plugged %>% mutate(diff = peak_tot_prod - peak_tot_prod_2) %>% filter(diff > 0))
  # ## they are, remove peak_tot_prod_2
  # peak_prod_plugged[, peak_tot_prod_2 := NULL]
  
  ## add status
  peak_prod_plugged <- merge(peak_prod_plugged, wells,
                             by = "api_ten_digit",
                             all.x = T)
  ## replace NA values
  peak_prod_plugged[, ":=" (prod_bbls = fifelse(is.na(prod_bbls), 0, prod_bbls),
                            well_status = fifelse(is.na(well_status), "Active", well_status))]
  
  peak_prod_plugged[, plugged_status := fifelse(well_status == "Plugged", "Plugged", "Other")]
  
  ## calculate percentage of prod from plugged wells for each field vintage
  peak_prod_plugged_perc <- peak_prod_plugged[, .(prod_bbls = sum(prod_bbls),
                                                  peak_tot_prod = unique(peak_tot_prod)), by = .(doc_field_code, doc_fieldname, start_year, plugged_status)]
  
  peak_prod_plugged_perc[, rel_prod := prod_bbls / peak_tot_prod]

  ## make version with zeros
  peak_prod_plugged_perc <- peak_prod_plugged_perc[, .(doc_field_code, start_year, plugged_status, rel_prod)]
  peak_prod_plugged_perc[, id := paste(doc_field_code, start_year, sep = "-")]
  
  ## all ids and statuses
  status_df <- expand.grid(id = unique(peak_prod_plugged_perc$id),
                           plugged_status = unique(peak_prod_plugged_perc$plugged_status))
  
  setDT(status_df)
  
  peak_prod_plugged_perc_all <- merge(status_df, peak_prod_plugged_perc,
                                      by = c("id", "plugged_status"),
                                      all.x = T)
  
  peak_prod_plugged_perc_all[, ":=" (doc_field_code = substr(id, 1, 3),
                                     start_year = stringr::str_sub(id, -4, -1),
                                     rel_prod = fifelse(is.na(rel_prod), 0, rel_prod))]
  
  plugged_fig <- ggplot(peak_prod_plugged_perc %>% filter(plugged_status == "Plugged"), aes(x = rel_prod)) +
    geom_histogram(binwidth = 0.01) +
    labs(y = "count",
         x = "relative peak production (plugged wells)")

  
  peak_prod_adj_val <- peak_prod_plugged_perc_all[plugged_status == "Other", .(doc_field_code, start_year, rel_prod)]
  setnames(peak_prod_adj_val, "rel_prod", "non_plug_rel_prod")
    
# save outputs ------
  
  fwrite(peak_fv_year, file.path(emlab_dir, save_dir, peak_fy_fil))
  fwrite(prod_fv_year_2, file.path(emlab_dir, save_dir, prod_fy_fil))
  fwrite(well_prod3, file.path(emlab_dir, save_dir, well_fy_fil))
  fwrite(peak_prod_adj_val, file.path(emlab_dir, save_dir, "adj_val_field-year_pred_prod.csv"))
  