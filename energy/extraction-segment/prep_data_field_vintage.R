# prep data for field-vintage level decline parameters
# created: august 31, 2020
# author: meas meng

# inputs ------

  data_directory  = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/'
  prod_file       = 'well_prod_m.rds'
  well_file       = 'wells_19.csv'
  init_file       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10_x_field.csv'
  entry_file      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df_final.csv'
  save_dir        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/decline-historic/data/'

# load libraries -------- 

  library(data.table)  
  library(lubridate)
  library(zoo)
  library(openxlsx)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)

# read in data ------

  well_prod = readRDS(paste0(data_directory, prod_file)) # monthly wellstar production data
  well_prod = setDT(well_prod)
  
  well_info = fread(paste0(data_directory, well_file), colClasses = c(rep('character',2), rep(NA, 21))) # info on wells, operators, location, etc
  
  init_prod = fread(init_file, colClasses = c(rep('character',2), rep(NA, 9)))
  
  entry_df = fread(entry_file, header = T)

# get 10 digit API numbers -------

  well_prod[, api_ten_digit := substr(APINumber, 1, 10)]
  well_info[, api_ten_digit := substr(API, 1, 10)]

# pad field code with leading zeroes -----
  
  well_prod[, FieldCode := sprintf("%03s", FieldCode)]
  init_prod[, FieldCode := sprintf("%03s", FieldCode)]
  entry_df[, doc_field_code := sprintf("%03s", doc_field_code)]
  
# get field code + field name matches -----
  
  field_code = unique(well_info[, c('FieldCode', 'FieldName')])

# unique well-field-start dates ------
  
  init_prod[, start_year := as.numeric(substr(start_date,1,4))]
  init_prod = unique(init_prod[, c('api_ten_digit', 'FieldCode', 'n_field', 'start_date', 'start_year')])
  
# calculate percentage of wells that belong to more than one field ----
  # there are conflicting results across datasets for this
  # there are some 10 digit apis that are categorized as n_field = 1 in the init_prod dataset but have more than one field associated with it in the well_info dataset. 
  # so, i'm going to use the production data to get API digits and fields
  
  wells_mult_field = unique(well_prod[, c('api_ten_digit', 'FieldCode')])
  wells_mult_field[, no_field := uniqueN(FieldCode), by = api_ten_digit]
  uniqueN(wells_mult_field[no_field > 1])/uniqueN(wells_mult_field[no_field == 1])
  # View(well_info[ api_ten_digit %in% wells_mult_field[ n_field > 1, api_ten_digit]][, c('api_ten_digit', 'FieldCode', 'FieldName')])
  
# fix some field names -----
  
  # change Old Wilmington to new Wilmington
    well_prod[ FieldCode == '848', FieldCode := '849' ]
  
# remove Any Field -----
  
  well_prod = well_prod[!FieldCode == '000']

# for each well, get field with earliest production date --------
    
  # wells_start_field = unique(well_prod[, c('api_ten_digit', 'ProductionReportDate', 'FieldCode')])[ , .SD[which.min(ProductionReportDate)], by = api_ten_digit]
  # colnames(wells_start_field)[2] = 'start_date'
  # wells_start_field[, start_year := as.numeric(substr(start_date,1,4))]
  # wells_start_field[, vintage:= cut(start_year, c(-Inf, 1977, 1982, 1987, 1992, 1997, 2002, 2007, 2012, 2019),
  #                                   c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))]
    
# get first field for each well -----
  
  wells_start_field = unique(init_prod[, c('api_ten_digit', 'FieldCode', 'start_date', 'start_year')])[ , .SD[which.min(start_year)], by = api_ten_digit]

# get start years and vintages -----
    
  wells_start_field[, vintage:= cut(start_year, c(-Inf, 1977, 1982, 1987, 1992, 1997, 2002, 2007, 2012, 2019),
                                    c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))]
  
  entry_df[, vintage := cut(year, c(-Inf, 1977, 1982, 1987, 1992, 1997, 2002, 2007, 2012, 2019),
                            c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))]
    
# merge with field names -----
  
  wells_start_field = wells_start_field[field_code, on = 'FieldCode', nomatch = 0] 
  
# remove 'Any Field' (FieldCode == 000) ------
  
  wells_start_field = wells_start_field[! FieldCode == '000']
  
# get wells that start after 1978 -----
  
  # init_aft_1978 = wells_start_field[ start_year >= 1978 ]

# match production data with well data for wells and their start dates ------

  well_prod2 = well_prod[wells_start_field, on = c('api_ten_digit', 'FieldCode'), nomatch = 0]

# keep only fields in entry data -----

  well_prod3 = well_prod2[ FieldCode %in% unique(entry_df[, doc_field_code]) ]

# aggregate oil production to the monthly level -------  

  well_prod3 = well_prod3[!is.na(OilorCondensateProduced), .(oil_prod = sum(OilorCondensateProduced, na.rm = T)), 
                          by = .(api_ten_digit, start_date, start_year, vintage, FieldName, FieldCode, ProductionReportDate, DaysProducing)]
  
# add months to each API number -----

  # function to calculate number of months between dates 
    elapsed_months <- function(end_date, start_date) {
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      12 * (ed$year - sd$year) + (ed$mon - sd$mon)
    }
  
  well_prod3[, month_no := elapsed_months(ProductionReportDate, start_date), by = c('api_ten_digit', 'start_date', 'start_year', 'vintage', 'FieldName', 'FieldCode') ]
  
  well_prod3 = well_prod3[ month_no > 0]
  well_prod3 = well_prod3[order(api_ten_digit,month_no)]
  well_prod3[, total_months := max(month_no, na.rm = T), by = 'api_ten_digit']
  
# cut number of months into number of year in production for each field vintage -------
  
  well_prod3[, year_no := cut(month_no, breaks = seq(0,516,12), labels = c(1:43), right = T)]
  
# count the number of unique wells in each field-vintage -----
  
  count_fv_wells = well_prod3[, .(no_wells = uniqueN(api_ten_digit)), by = .(FieldCode, FieldName, vintage)]
  count_fv_wells = setorderv(count_fv_wells, c('FieldCode', 'vintage'))
  

# aggregate production by: field-vintage-month since start date ----  

  prod_fv_month = well_prod3[, .(tot_oil_prod = sum(oil_prod, na.rm = T)),  by = .(FieldName, FieldCode, vintage, month_no)]
  
  prod_fv_month = prod_fv_month[count_fv_wells, on = c('FieldName', 'FieldCode', 'vintage')]
  
  prod_fv_month[, well_prod := tot_oil_prod/no_wells ] # avg production per well for each month
  
  prod_fv_month = prod_fv_month[order(FieldName, vintage, month_no)]
  prod_fv_month[, prod_rate := well_prod/30.5, by = c('FieldName', 'FieldCode', 'vintage')] # bbls/day for each well
  prod_fv_month[, decline_rate := (prod_rate - shift(prod_rate))/prod_rate, by = c('FieldName', 'FieldCode', 'vintage')]
  
  # ggplot(prod_fv_month, aes(x = month_no, y = prod_rate/1000, group = vintage, color = vintage)) + geom_line() +
  #   facet_wrap(. ~ FieldName, nrow = 4, scales = "free_y") +
  #   labs(title = 'Production curves for each field-vintage at the monthly level',         
  #        subtitle = 'Production aggregated to total produced during each month relative to well start date',
  #        x = 'Number of months since start date',
  #        y = 'Average production rate (thousand bbls/day)') +
  #   scale_color_manual(values = rev(brewer.pal(9, 'YlOrRd'))) +
  #   theme_line + 
  #   theme(legend.position = 'bottom')

# aggregate production by: field-vintage-year since start date ----  
  
  prod_fv_year = well_prod3[, .(tot_oil_prod = sum(oil_prod, na.rm = T)),  by = .(FieldName, FieldCode, vintage, year_no)]
  
  prod_fv_year = prod_fv_year[count_fv_wells, on = c('FieldName', 'FieldCode', 'vintage')]
  prod_fv_year[, well_prod := tot_oil_prod/no_wells ] # avg production per well for each year
  
  prod_fv_year = prod_fv_year[order(FieldName, vintage, year_no)]
  prod_fv_year[, prod_rate := well_prod/365, by = c('FieldName', 'FieldCode', 'vintage')] # bbls/day
  prod_fv_year[, decline_rate := (prod_rate - shift(prod_rate))/prod_rate, by = c('FieldName', 'FieldCode', 'vintage')]
  
# get peak production month for each field-vintage -----
  
  # for pre-1978, choose month no = 1 as peak month
    peak_fv_month_pre = prod_fv_month[vintage == 'pre 1978', .SD[which.min(month_no)], by = .(FieldName, FieldCode, vintage)]
  
  # for pre-1978, choose peak production rate as peak month
    # peak_fv_month_pre = prod_fv_month[vintage == 'pre 1978', .SD[which.max(prod_rate)], by = .(FieldName, FieldCode, vintage)]
  
  # for post-1978, choose peak production rate as peak month
    peak_fv_month_post = prod_fv_month[! vintage == 'pre 1978', .SD[which.max(prod_rate)], by = .(FieldName, FieldCode, vintage)]
  
  # combine peak months for pre and post 1978
    peak_fv_month = rbindlist(list(peak_fv_month_pre,peak_fv_month_post))
    peak_fv_month = peak_fv_month[, 'decline_rate' := NULL ]
    
    colnames(peak_fv_month)[c(4:5,7:8)] = c('peak_prod_month', 'peak_tot_prod', 'peak_avg_well_prod', 'peak_well_prod_rate')
    

  # ggplot(prod_fv_month_2[ peak_month_diff >= 0 ], aes(x = peak_month_diff, y = prod_rate/1000, group = vintage, color = vintage)) + geom_line() +
  #   facet_wrap(. ~ FieldName, nrow = 4, scales = "free_y") +
  #   labs(title = 'Production curves for each field-vintage at the monthly level',
  #        subtitle = 'Production aggregated to total produced during each month relative to well start date',
  #        x = 'Number of months since peak month',
  #        y = 'Average production rate (thousand bbls/day)') +
  #   scale_color_manual(values = rev(brewer.pal(9, 'YlOrRd'))) +
  #   theme_line +
  #   theme(legend.position = 'bottom')

  
  # ggplot(prod_fv_year, aes(x = as.numeric(year_no), y = prod_rate/1000, group = vintage, color = vintage)) + geom_line() +
  #   facet_wrap(. ~ FieldName, nrow = 4, scales = "free_y") +
  #   labs(title = 'Production curves for each field-vintage at the yearly level',         
  #        subtitle = 'Production aggregated to total produced during each month relative to well start date',
  #        x = 'Number of years since start date',
  #        y = 'Average production rate (thousand bbls/day)') +
  #   scale_color_manual(values = rev(brewer.pal(9, 'YlOrRd'))) +
  #   theme_line + 
  #   theme(legend.position = 'bottom')

# get peak production year for each field-vintage -----

  # for pre-1978, choose month no = 1 as peak month
    peak_fv_year_pre = prod_fv_year[vintage == 'pre 1978', .SD[which.min(year_no)], by = .(FieldName, FieldCode, vintage)]
  
  # for pre-1978, choose peak production rate as peak month
    # peak_fv_year_pre = prod_fv_year[vintage == 'pre 1978', .SD[which.max(prod_rate)], by = .(FieldName, FieldCode, vintage)]
    
  # for post-1978, choose peak production rate as peak month
    peak_fv_year_post = prod_fv_year[! vintage == 'pre 1978', .SD[which.max(prod_rate)], by = .(FieldName, FieldCode, vintage)]
    
  # combine peak months for pre and post 1978
    peak_fv_year = rbindlist(list(peak_fv_year_pre,peak_fv_year_post))
    peak_fv_year = peak_fv_year[, 'decline_rate' := NULL ]
    
    colnames(peak_fv_year)[c(4:5,7:8)] = c('peak_prod_year', 'peak_tot_prod', 'peak_avg_well_prod', 'peak_well_prod_rate')
    
    
    
# calculae medians of peak production by field and by vintage (yearly) -----
    
  med_field = peak_fv_year[, .(peak_prod_year = round(median(as.numeric(peak_prod_year), na.rm = T),0),
                               peak_tot_prod = median(as.numeric(peak_tot_prod), na.rm = T),
                               no_wells = NA,
                               peak_avg_well_prod = median(peak_avg_well_prod, na.rm = T),
                               peak_well_prod_rate = median(peak_well_prod_rate, na.rm = T)), by = .(FieldCode)]
  
  med_vintage = peak_fv_year[, .(peak_prod_year = round(median(as.numeric(peak_prod_year), na.rm = T),0),
                                 peak_tot_prod = median(as.numeric(peak_tot_prod), na.rm = T),
                                 no_wells = NA,
                                 peak_avg_well_prod = median(peak_avg_well_prod, na.rm = T),
                                 peak_well_prod_rate = median(peak_well_prod_rate, na.rm = T)), by = .(vintage)]
  
# fix instances where field-vintages in the entry dataset don't have matching peak prod data -----
    
  nonmatch = entry_df[!peak_fv_year, on = c('doc_field_code' = 'FieldCode', 'vintage')]
  nonmatch = unique(nonmatch[, c('doc_field_code', 'vintage')])
  colnames(nonmatch)[1] = 'FieldCode'
  
  list_fixes = list()
    for (i in 1:nrow(nonmatch)) {
      
      fv = nonmatch[i]
      checkexists = nrow(med_field[FieldCode %in% fv[, FieldCode]])
      
      if (checkexists > 0) {
        match = med_field[FieldCode == fv[, FieldCode]]
        fv2 = fv[match, on = c('FieldCode')]
      } else {
        match = med_vintage[vintage == fv[, vintage]]
        fv2 = fv[match, on = c('vintage')]
      }
      
      list_fixes[[i]] = fv2
      
      rm(fv,checkexists,fv2)
      
    }
  
  dt_fixes = rbindlist(list_fixes)
  dt_fixes = dt_fixes[field_code, on = 'FieldCode', nomatch = 0]
  setcolorder(dt_fixes, colnames(peak_fv_year))
  
# combine with peak production dataset -----
  
  peak_fv_year = rbindlist(list(peak_fv_year, dt_fixes))
  peak_fv_year = peak_fv_year[order(FieldCode,vintage)]

# merge monthly production data with peak production month info ----
  
  # prod_fv_month_2 = prod_fv_month[peak_fv_month[,-'no_wells'], on = c('FieldName', 'FieldCode', 'vintage')]
  # prod_fv_month_2[, peak_month_diff := month_no - peak_prod_month ]
    
# merge monthly production data with peak production month info ----
    
  prod_fv_year_2 = prod_fv_year[peak_fv_year[,-'no_wells'], on = c('FieldName', 'FieldCode', 'vintage')]
  prod_fv_year_2[, peak_year_diff := as.numeric(year_no) - as.numeric(peak_prod_year) ]

  # ggplot(prod_fv_year_2[ peak_year_diff >= 0 ], aes(x = peak_year_diff, y = prod_rate/1000, group = vintage, color = vintage)) + geom_line() +
  #   facet_wrap(. ~ FieldName, nrow = 4, scales = "free_y") +
  #   labs(title = 'Production curves for each field-vintage at the yearly level',         
  #        subtitle = 'Production aggregated to total produced during each year relative to well start date',
  #        x = 'Number of years since peak month',
  #        y = 'Average production rate (thousand bbls/day)') +
  #   scale_color_manual(values = rev(brewer.pal(9, 'YlOrRd'))) +
  #   theme_line + 
  #   theme(legend.position = 'bottom')


# export data -----
  
  fwrite(peak_fv_year, paste0(save_dir, 'field-vintage_peak-production_yearly.csv'), row.names = F)
  fwrite(prod_fv_year_2, paste0(save_dir, 'production_field-vintange_yearly_entry.csv'), row.names = F)
  
  fwrite(well_prod3, paste0(save_dir, 'production_api10_monthly.csv'), row.names = F)
  
  # fwrite(peak_fv_month, paste0(save_dir, 'field-vintage_peak-production_monthly.csv'), row.names = F)
  # fwrite(prod_fv_month_2, paste0(save_dir, 'production_field-vintange_monthly_entry.csv'), row.names = F)
  
