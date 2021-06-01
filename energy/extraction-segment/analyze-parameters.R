## meas meng
## august 31, 2020
## analysis and plots of decline curve parameters

# inputs ------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  # para_dir      = "outputs/decline-historic/parameters/"
  # yr_fil        = "fitted-parameters_field-vintage_yearly_entry_modeled_only.csv"
  yr_file         = "outputs/decline-historic/parameters/fitted-parameters_field-start-year_yearly_entry.csv"
  prod_file       = "outputs/stocks-flows/crude_prod_x_field_revised.csv"
  plot_path       = "outputs/decline-historic/figures/"
  
# outputs -------
  
  save_path       = "outputs/decline-historic/parameters"
  save_file       = "forecasted_decline_parameters_2020_2050_revised.csv"

# load libraries -------- 

  library(data.table)  
  library(lubridate)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(RColorBrewer)
  
# source items ------
  
  # items = list.files(here::here("src"))
  # sapply(here::here("src", items), source)
  
# read in data ------
  
  para_year = fread(file.path(emlab_path, yr_file))
  # para_year[, no_wells.x := NULL]
  # setnames(para_year, 'no_wells.y', 'no_wells')
  
  prod_2019 = fread(file.path(emlab_path, prod_file), header = T)
  prod_2019 = prod_2019[year == 2019]
  
# rename field code columns -----
  
  # setnames(para_year, 'FieldCode', 'doc_field_code')
  # setnames(prod_2019, 'FieldCode', 'doc_field_code')
  
# rename field name columns -----
  
  # setnames(para_year, 'FieldName', 'doc_fieldname')
  # setnames(prod_2019, 'FieldName', 'doc_fieldname')

# pad field code with leading zeroes -----
  
  para_year[, doc_field_code := sprintf("%03d", doc_field_code)]
  prod_2019[, doc_field_code := sprintf("%03d", doc_field_code)]
  
# get final b value ------
  
  # para_year[, b := ifelse(is.na(b2), b1, b2)]
  
# get first year of each vintage ------
  
  # para_year[! vintage == "pre 1978", first_year := as.numeric(substr(vintage,1,4))]
  
# remove pre 1978 -----
  
  # para_year_2 = para_year[start_year > 1977]
  
# list top 10 fields ------
  
  setorder(prod_2019, -'total_bbls')
  prod_2019[, field_rank := rank(-total_bbls)]
  top_fields_2019 = prod_2019[field_rank %in% c(1:10)]
  top10_fields = top_fields_2019[, doc_field_code]
  # top10_fields = c('Belridge  South', 'Midway-Sunset', 'Kern River', 'Cymric', 'Wilmington', 'Lost Hills', 'San Ardo', 'Elk Hills', 'Coalinga', 'Huntington Beach')

# count unique vintages for each field -----
  
  para_year[, un_start_year := uniqueN(start_year), by = .(doc_field_code)]
  # para_year_2[, un_start_year := uniqueN(start_year), by = .(doc_field_code)]
  
  # para_year[, .(count = uniqueN(doc_field_code)), by = un_vintage][order(un_vintage)]
  # para_year_2[, .(count = uniqueN(doc_field_code)), by = un_vintage][order(un_vintage)]

# years to predict -----
  
  pred_years = data.table(start_year = c(seq(2019, 2050, 1)))
  
# remove outliers outside of 1st and 99th percentile ------
  
  para_clean = para_year[b > quantile(b, 0.01, na.rm = T) & b < quantile(b, 0.99, na.rm = T)]
  para_clean = para_clean[d > quantile(d, 0.01, na.rm = T) & d < quantile(d, 0.99, na.rm = T)]
  
# regress parameters for top 10 fields ------

  para_top10 = para_clean[doc_field_code %in% top10_fields]
  para_top10 = para_top10[!is.na(b)]

  pred_top10 = para_top10[, .(q_i = predict(lm(q_i ~ start_year),  pred_years),
                              D = predict(lm(D ~ start_year),  pred_years),
                              b = predict(lm(b ~ start_year),  pred_years),
                              d = predict(lm(d ~ start_year),  pred_years),
                              int_year = predict(lm(int_yr ~ start_year), pred_years)), by = .(doc_field_code, doc_fieldname)]
  pred_top10[, start_year := rep(pred_years[, start_year], uniqueN(pred_top10[, doc_field_code]))]
  
  # top_fields_2019[!doc_field_code %in% pred_top10[, doc_field_code]]

  
# take median of parameters across vintages -------

  median_params = para_clean[!doc_field_code %in% pred_top10[, doc_field_code], .(median_q_i = median(q_i, na.rm = T),
                                                                                  median_D = median(D, na.rm = T),
                                                                                  median_b = median(b, na.rm = T),
                                                                                  median_d = median(d, na.rm = T),
                                                                                  median_int = median(int_yr, na.rm = T)), by = .(start_year)]
  setorder(median_params, "start_year")
  
# regress median parameters for all other fields ------
  
  pred_other = median_params[, .(q_i = predict(lm(median_q_i ~ start_year),  pred_years),
                                 D = predict(lm(median_D ~ start_year),  pred_years),
                                 b = predict(lm(median_b ~ start_year),  pred_years),
                                 d = predict(lm(median_d ~ start_year),  pred_years),
                                 int_year = predict(lm(median_int ~ start_year),  pred_years))]
  pred_other[, start_year := pred_years[, start_year]]
  
# for negative values of predicted intercept years, replace with median of 1990-20
  
  pred_other[int_year < 0, int_year := median(median_params[start_year %in% 1990:2010, median_int])]
  
# add columns for "other" fields (all non-top 10 fields) -------
  
  pred_other[, doc_field_code := 'XYZ']
  pred_other[, doc_fieldname := 'other']
  
# combine top 10 and non top 10 results together -----
  
  pred_all = rbindlist(list(pred_top10, pred_other), use.names = T, fill = T)
  setcolorder(pred_all, c('doc_field_code', 'doc_fieldname', 'start_year', 'q_i', 'D', 'b', 'd', 'int_year'))
  setnames(pred_all, 'start_year', 'year')
  
# remove 2019 predictions -----
  
  pred_all = pred_all[year > 2019]
  
# add end year of vintage ----
  
  # pred_all[, end_year := start_year + 4]

# generate vintages -----
  
  # pred_all[, vintage := paste(start_year, end_year, sep = "-")]

# duplicate rows for each year of vintage ----
  
  # pred_all_full = pred_all[ , list(freq = rep(1,5)), by = c("q_i", "D", "b", "d", "int_year", "start_year")]
  # pred_all_full[, year := 2020:2049]
  # pred_all_full[, freq := NULL]
  # setcolorder(pred_all_full, c("year", "vintage", "first_year", "end_year", "q_i", "D", "b2", "d2", "int_year"))
  
# # regress parameters for top 10 fields ------
# 
#   para_top10 = para_year_2[doc_field_code %in% top10_fields]
#   para_top10 = para_top10[!is.na(b2)]
# 
#   pred_top10 = para_top10[, .(q_i = predict(lm(q_i ~ first_year),  pred_years),
#                               D = predict(lm(D ~ first_year),  pred_years),
#                               b2 = predict(lm(b2 ~ first_year),  pred_years),
#                               d2 = predict(lm(d2 ~ first_year),  pred_years)), by = .(doc_field_code, doc_fieldname)]
#   pred_top10[, first_year := rep(pred_years[, first_year], 10)]
# 
# # regress parameters for all other fields -----
# 
#   para_other = para_year_2[! FieldCode %in% top10_fields]
# 
#   para_other = para_other[ b2 > quantile(b2, 0.01, na.rm = T) & b2 < quantile(b2, 0.99, na.rm = T)]
#   para_other = para_other[ d > quantile(d, 0.01, na.rm = T) & d < quantile(d, 0.99, na.rm = T)]
# 
#   median_params = para_other[, .(median_q_i = median(q_i, na.rm = T),
#                                  median_D = median(D, na.rm = T),
#                                  median_b2 = median(b2, na.rm = T),
#                                  median_d = median(d, na.rm = T),
#                                  median_int = median(int_yr, na.rm = T)), by = .(first_year, vintage)]
#   setorder(median_params, "first_year")
# 
#   pred_other = median_params[, .(q_i = predict(lm(median_q_i ~ first_year),  pred_years),
#                                  D = predict(lm(median_D ~ first_year),  pred_years),
#                                  b2 = predict(lm(median_b2 ~ first_year),  pred_years),
#                                  d = predict(lm(median_d ~ first_year),  pred_years),
#                                  int_year = predict(lm(median_int ~ first_year),  pred_years))]
#   pred_other[, first_year := pred_years[, first_year]]
# 
# # calculate medians of predicted parameters ------
# 
#   median_pred_top10 = merge(pred_top10[q_i > 0, .(med_q_i = median(q_i, na.rm = T)), by = .(FieldCode, FieldName)],
#                             pred_top10[D > 0, .(med_D = median(D, na.rm = T)), by = .(FieldCode, FieldName)],
#                             by = c("FieldCode", "FieldName"))
# 
#   median_pred_top10 = merge(median_pred_top10,
#                             pred_top10[b1 > 0, .(med_b1 = median(b1, na.rm = T)), by = .(FieldCode, FieldName)],
#                             by = c("FieldCode", "FieldName"))
# 
# # for instances in top 10 fields, replace negative with median of non-negative values ----
# 
#   pred_top10 = pred_top10[median_pred_top10, on = c("FieldName", "FieldCode")]
#   pred_top10[q_i < 0, q_i := med_q_i]
#   pred_top10[D < 0, D := med_D]
#   pred_top10[b1 < 0, b1 := med_b1]
# 
#   pred_top10[, med_q_i := NULL]
#   pred_top10[, med_D := NULL]
#   pred_top10[, med_b1 := NULL]
# 
# # replace predict intercept years with median intercept year of last 2 years -----
# 
#   repl = median_params[first_year %in% c(2003,2008), lapply(.SD, median, na.rm = TRUE), .SDcols = c("median_q_i", "median_D", "median_b2", "median_d", "median_int")]
# 
#   pred_other[, median_int := repl[, median_int]]
#   pred_other[, int_year := median_int]
#   pred_other[, median_int := NULL]
# 
# # remove 2013 predictions -----
# 
#   pred_top10 = pred_top10[! first_year == 2013]
#   pred_other = pred_other[! first_year == 2013]
# 
# # add end year of vintage ----
# 
#   pred_top10[, end_year := first_year + 4]
#   pred_other[, end_year := first_year + 4]
# 
# # generate vintages -----
# 
#   pred_top10[, vintage := paste(first_year, end_year, sep = "-")]
#   pred_other[, vintage := paste(first_year, end_year, sep = "-")]
# 
# # duplicate rows for each year of vintage ----
# 
#   pred_top10_all = pred_top10[ , list(freq = rep(1,5)), by = c("FieldCode", "FieldName", "q_i", "D", "b1", "first_year", "end_year", "vintage")]
#   pred_top10_all[, year := rep(2020:2049, 10)]
#   pred_top10_all[, freq := NULL]
#   setcolorder(pred_top10_all, c("FieldCode", "FieldName", "year", "vintage", "first_year", "end_year", "q_i", "D", "b1"))
# 
#   pred_other_all = pred_other[ , list(freq = rep(1,5)), by = c("q_i", "D", "b2", "d", "int_year", "first_year", "end_year", "vintage")]
#   pred_other_all[, year := 2020:2049]
#   pred_other_all[, freq := NULL]
#   pred_other_all[, FieldName := "other"]
#   pred_other_all[, FieldCode := NA]
#   setcolorder(pred_other_all, c("FieldCode", "FieldName", "year", "vintage", "first_year", "end_year", "q_i", "D", "b2", "d", "int_year"))
# 
# # combine into single data table -----
# 
#   pred_all = rbindlist(list(pred_top10_all, pred_other_all), use.names = T, fill = T)
  
# export to csv -----
  
  # fwrite(pred_all, paste0(save_dir, save_fil), row.names = F)
  fwrite(pred_all, file.path(emlab_path, save_path, save_file), row.names = F)
  