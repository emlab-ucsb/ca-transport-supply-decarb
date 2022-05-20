## meas meng
## august 31, 2020
## analysis and plots of decline curve parameters

# inputs ------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  # para_dir      = "outputs/decline-historic/parameters/"
  # yr_fil        = "fitted-parameters_field-vintage_yearly_entry_modeled_only.csv"
  yr_file         = "outputs/decline-historic/parameters/fitted-parameters_field-start-year_yearly_entry.csv"
  prod_file       = "outputs/stocks-flows/crude_prod_x_field_revised.csv"
  entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  plot_path       = "outputs/decline-historic/figures/"
  
# outputs -------
  
  save_path       = "outputs/decline-historic/parameters"
  save_file       = "forecasted_decline_parameters_2020_2045.csv"

# load libraries -------- 

  library(data.table)  
  library(lubridate)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(RColorBrewer)
  
# read in data ------
  
  para_year = fread(file.path(emlab_path, yr_file))

  prod_2019 = fread(file.path(emlab_path, prod_file), header = T)
  prod_2019 = prod_2019[year == 2019]
  
  df_entry = fread(file.path(emlab_path, entry_file), header = T)
  
# pad field code with leading zeroes -----
  
  para_year[, doc_field_code := sprintf("%03d", doc_field_code)]
  prod_2019[, doc_field_code := sprintf("%03d", doc_field_code)]
  df_entry[, doc_field_code := sprintf("%03d", doc_field_code)]
  
# get unique number of fields in entry model -----
  
  un_fields = unique(df_entry[, .(doc_field_code, doc_fieldname)])
  
# list top 10 fields ------
  
  setorder(prod_2019, -'total_bbls')
  prod_2019[, field_rank := rank(-total_bbls)]
  top_fields_2019 = prod_2019[field_rank %in% c(1:10)]
  top10_fields = top_fields_2019[, doc_field_code]

# count unique vintages for each field -----
  
  para_year[, un_start_year := uniqueN(start_year), by = .(doc_field_code)]

# years to predict -----
  
  pred_years = data.table(start_year = c(seq(2019, 2045, 1)))
  
# remove outliers outside of 1st and 99th percentile ------
  
  para_clean = para_year[b > quantile(b, 0.01, na.rm = T) & b < quantile(b, 0.99, na.rm = T)]
  para_clean = para_clean[d > quantile(d, 0.01, na.rm = T) & d < quantile(d, 0.99, na.rm = T)]
  
# regress parameters for all fields (that data exist for) -------
  
  pred_clean = para_clean[, .(q_i = predict(lm(q_i ~ start_year),  pred_years),
                              D = predict(lm(D ~ start_year),  pred_years),
                              b = predict(lm(b ~ start_year),  pred_years),
                              d = predict(lm(d ~ start_year),  pred_years),
                              int_year = predict(lm(int_yr ~ start_year), pred_years)), by = .(doc_field_code, doc_fieldname)]
  pred_clean[, start_year := rep(pred_years[, start_year], uniqueN(pred_clean[, doc_field_code]))]
  
# for negative values of predicted intercept years, replace with median -------
  
  rank_years_pred = pred_clean[int_year > 0, .SD[start_year %in% tail(sort(unique(start_year)), 5)], by = .(doc_field_code, doc_fieldname)]
  median_int_yr_pred = rank_years_pred[, .(med_int_year = median(int_year, na.rm = T)), by = .(doc_field_code, doc_fieldname)]
  
  rank_years_hist = para_clean[, .SD[start_year %in% tail(sort(unique(start_year)), 5)], by = .(doc_field_code, doc_fieldname)]
  median_int_yr_hist = rank_years_hist[, .(med_int_year = median(int_yr, na.rm = T)), by = .(doc_field_code, doc_fieldname)]
  
  pred_clean = merge(pred_clean, median_int_yr_pred, by = c('doc_field_code', 'doc_fieldname'), all.x = TRUE)
  pred_clean[int_year < 0, int_year := med_int_year]
  pred_clean[, med_int_year := NULL]
  
  pred_clean = merge(pred_clean, median_int_yr_hist, by = c('doc_field_code', 'doc_fieldname'), all.x = TRUE)
  pred_clean[is.na(int_year), int_year := med_int_year]
  pred_clean[, med_int_year := NULL]
  
# remove fields that have negative parameters -------
  
  fields_bad = unique(pred_clean[(q_i < 0) | (D < 0) | (b < 0) | (d < 0), .(doc_field_code, doc_fieldname)])
  
  pred_clean_2 = pred_clean[!fields_bad, on = .(doc_field_code, doc_fieldname)]
  
# check which fields are missing -------
  
  missing_fields = un_fields[!pred_clean_2, on = .(doc_field_code, doc_fieldname)]
  
# take median of parameters across vintages -------
  
  median_params = para_clean[, .(median_q_i = median(q_i, na.rm = T),
                                 median_D = median(D, na.rm = T),
                                 median_b = median(b, na.rm = T),
                                 median_d = median(d, na.rm = T),
                                 median_int = median(int_yr, na.rm = T)), by = .(start_year)]
  setorder(median_params, "start_year")
  
# regress median parameters for all other fields ------
  
  pred_vintage = median_params[, .(q_i = predict(lm(median_q_i ~ start_year),  pred_years),
                                   D = predict(lm(median_D ~ start_year),  pred_years),
                                   b = predict(lm(median_b ~ start_year),  pred_years),
                                   d = predict(lm(median_d ~ start_year),  pred_years),
                                   int_year = predict(lm(median_int ~ start_year),  pred_years))]
  pred_vintage[, start_year := pred_years[, start_year]]
  
# calculate percentage change in median forecasted parameters --------
  
  # setkey(pred_vintage, start_year)
  # pred_vintage[, perc_q_i := (q_i - shift(q_i, type = 'lag'))/shift(q_i, type = 'lag')]
  # pred_vintage[, perc_D := D - shift(D, type = 'lag')]
  # pred_vintage[, perc_b := b - shift(b, type = 'lag')]
  
# use median parameter regression for all missing fields -------
  
  l_fill_in = list()
  for (i in 1:nrow(missing_fields)){
    
    temp = copy(pred_vintage)
    temp[, doc_field_code := missing_fields[i, doc_field_code]]
    temp[, doc_fieldname := missing_fields[i, doc_fieldname]]
    
    l_fill_in[[i]] = temp
    
  }
  
  dt_fill_in = rbindlist(l_fill_in)
  
# combine predicted and filled in fields ------
  
  pred_all = rbindlist(list(pred_clean_2, dt_fill_in), use.names = T, fill = T)
  setkey(pred_all, doc_field_code)
  
# remove 2019 predictions -----
  
  setcolorder(pred_all, c('doc_field_code', 'doc_fieldname', 'start_year', 'q_i', 'D', 'b', 'd', 'int_year'))
  setnames(pred_all, 'start_year', 'year')
  pred_all = pred_all[year > 2019]
  
# export to csv -----
  
  # fwrite(pred_all, paste0(save_dir, save_fil), row.names = F)
  fwrite(pred_all, file.path(emlab_path, save_path, save_file), row.names = F)
  