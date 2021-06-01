# parameterize decline at field-start year level
# created: may 27, 2021
# author: meas meng

# inputs ------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  fv_year_file    = 'outputs/decline-historic/data/production_field-year_yearly_entry.csv'
  peak_file       = 'outputs/decline-historic/data/field-year_peak-production_yearly.csv'
  entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  plot_dir        = 'outputs/figures/interim-report-figures/drafts/fuels-model/decline_figs/'
  
# outputs -------
  
  save_path       = 'outputs/decline-historic/parameters/'
  save_file       = 'fitted-parameters_field-start-year_yearly_entry.csv'

# load libraries -------- 
  
  library(data.table) 
  library(zoo)
  
# read in data ------
  
  prod_fv_year = fread(file.path(emlab_path, fv_year_file), colClasses = c(rep('character',2), rep(NA,12)))
  peak_prod = fread(file.path(emlab_path, peak_file), header = T)
  dt_entry = fread(file.path(emlab_path, entry_file), header = T)
  
# pad field code with leading zeroes -----
  
  prod_fv_year[, doc_field_code := sprintf("%03s", doc_field_code)]
  peak_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_entry[, doc_field_code := sprintf("%03s", doc_field_code)]
  
# get list of unique field-vintages -----
  
  un_fy = unique(prod_fv_year[, c('doc_fieldname', 'doc_field_code', 'start_year', 'no_wells')])
  un_fy = un_fy[order(doc_field_code, start_year)]
  
# create start year column for entry df -----
  
  dt_entry[, start_year := year]
  
# get field-vintage combos in the entry df that are not in the production dataset -----  
  
  nonmatch = dt_entry[!un_fy, on = c('doc_field_code', 'start_year')]
  
# functions ------
  
  hypfunc = function(b,t) {  q_i/((1 + b*D_h*t)^(1/b)) } # write hyperbolic function
  expfunc = function(d,t) {   q_i*exp(-d*t)      } # write exponential function
  
# hyperbolic and exponential regression for each field-vintage at the yearly level --------
  
  l_nls_mult = list()
  
  for (i in 1:nrow(un_fy)) {
    
    temp = prod_fv_year[un_fy[i], on = c('doc_field_code', 'doc_fieldname', 'start_year'), nomatch = 0] # get production for specified field-start year
    temp2 = temp[peak_year_diff >= 0] # only keep years from peak year onwards
    # temp2 = temp2[!prod_rate %in% boxplot(temp2[, prod_rate])$out]
    # temp2 = temp2[prod_rate < boxplot(temp2[, prod_rate])$out] # remove outliers

    if (nrow(temp2) >= 5) { # CHECK 1: if the number of rows for yearly production is less than 10, skip
      
      temp2[, t := 1:nrow(temp2)] # add sequence of years
      temp2[, moving_decline_rate := rollmean(decline_rate, 5, align = 'left', na.pad = T)] # calculate a moving decline rate by taking a 5-year left-aligned rolling mean
      exp_year = ifelse(is.na(temp2[moving_decline_rate < 0 & abs(moving_decline_rate) <= 10/100 & t >= quantile(t, 0.3) ][1, t]),
                        max(temp2[, t] + 1),
                        temp2[moving_decline_rate < 0 & abs(moving_decline_rate) <= 10/100 & t >= quantile(t, 0.3) ][1, t]) # condition to calculate estimate year when curve changes from hyperbolic to exponential
      
      if (exp_year < max(temp2[, t])) { # CHECK 2: if the estimated exponential year is greater than the total number of years, skip
        
        temp2[t < exp_year, type := 'hyperbolic'] # assign years before exp_year as hyperbolic decline
        temp2[t >= exp_year, type := 'exponential'] # assign years after exp_year as exponential decline
        
        q_i = max(temp2[, prod_rate], na.rm = T) # calculate q_i (peak production rate)
        D_h = max(abs(temp2[!is.infinite(decline_rate) & !is.na(decline_rate) & decline_rate < 0, decline_rate][1:5]), na.rm = T) # calculate D_h (initial decline rate) by taking max of first five decline rates
        
        # perform hyperbolic + exponential regression
        
        b_seq = seq(0.001, 5, 0.001) # sequence of b values to start at
        
        for (b_num in 1:length(b_seq)) { # loop through the b values until a hyperbolic regression fit can be applied (if applicable)
          
          catch_hyp = tryCatch({ # check if hyperbolic years can be properly fitted using nls
            mult <- nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2[type == 'hyperbolic'], start = list(b = b_seq[b_num]))
            ('yes')
          }, error = function(e) {
            ('no')
          })
          
          if (catch_hyp == 'yes') { # if hyperbolic fit exists at current value, break loop
            break
          } else { # if hyperbolic fit does not exist at current b value, try next value
            b_num = b_num + 1
          }
        }
        
        if (b_num <= length(b_seq)) { # CHECK 3: if hyperbolic fit cannot be applied to estimated hyperbolic portion, skip
          
          nlsmod = nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2[type == 'hyperbolic'], start = list(b = b_seq[b_num]))
          # nlsmod = nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2[type == 'hyperbolic'], start = list(b = ifelse(D_h > 10, 2,
          #                                                                                                               ifelse(D_h >= 1 & D_h <= 10, 1,
          #                                                                                                                      0.001))))
          bval = coef(nlsmod)[1]
          
          # q_ie = hypfunc(bval2,exp_year)
          
          expmod = nls(prod_rate ~ q_i*exp(-d*t), data = temp2[type == 'exponential'], start = list(d = -0.008)) # fit second portion as exponential
          # temp2[type == 'hyperbolic' , fitmult := fitted(nlsmod) ]
          # temp2[type == 'exponential' , fitmult := fitted(expmod) ]
          dval = coef(expmod)[1]
          
          fullfunc  = function(t) {q_i/((1 + bval*D_h*t)^(1/bval)) - q_i*exp(-dval*t)} # full function of hyperbolic + exponential
          
          intercept_yr = NA
          try(intercept_yr <- uniroot(f = fullfunc, interval = c(1, 100))$root, silent=T) # calculate root of full function to find intercept year
          if(is.na(intercept_yr)) intercept_yr = 1
          intercept_yr = round(intercept_yr,0)
          
          # if intercept year of full function is greater than full number of years of data, use estimated exp_year as intercept year instead
          intercept_yr = ifelse(intercept_yr <= max(temp2[, t]), intercept_yr, exp_year)
          
          maxpredt = max(intercept_yr, nrow(temp2)) # max number of predicted years
          
          hypfit = hypfunc(bval,(1:maxpredt)) # hyperbolic fitted line
          expfit = expfunc(dval,(1:maxpredt)) # exponential fitted line
          
          # save outputs into data.table
          # dt = data.table(i_num = i,
          #                 field_name = un_fy[i, doc_fieldname],
          #                 field_code = un_fy[i, doc_field_code],
          #                 vintage = un_fy[i, vintage],
          #                 D_h = D_h)
          l_nls_mult[[i]] = data.table(un_fy[i], 
                                       q_i = q_i, 
                                       D = D_h, 
                                       b = bval,
                                       d = dval,
                                       int_yr = intercept_yr)
          
          
          # l_nls_mult[[i]] = dt
          
          rm(q_i,D_h,b_seq,b_num,nlsmod,expmod,bval,dval,intercept_yr,maxpredt,hypfit,expfit,mult,exp_year,catch_hyp)
          
          
        } else { # skip for CHECK 3
          i = i + 1
        }
        
        
      } else { # skip for CHECK 2
        i = i + 1
      }
      
      rm(exp_year)
      
      
    } else { # skip for CHECK 1
      i = i + 1
    }
    
    rm(temp,temp2)
    
  }
  
  res_mult_fit = rbindlist(l_nls_mult)
  res_mult_fit[, type := 'hyperbolic and exponential coefs (curve-fitted)']
  
# get field-start years that were unable to have hyperbolic + exponential fit ------
  
  non_mult_fit = un_fy[!res_mult_fit, on = .(doc_field_code, start_year)]
  
# get fields that were not able to fit at all ------
  
  non_fields = non_mult_fit[!doc_field_code %in% unique(res_mult_fit[, doc_field_code])]
  non_mult_fit = non_mult_fit[doc_field_code %in% unique(res_mult_fit[, doc_field_code])]
  
# match fields that could not be fit to hyperbolic + exponential curves -----
  
  l_stat_mult = list()
  
  for (i in 1:nrow(non_mult_fit)) {
    
    temp = non_mult_fit[i]
    temp[, missing_year := start_year]
    # setnames(temp, 'start_year', 'missing_year')
    
    # get regression results for all start years in the same field 
    res = res_mult_fit[doc_field_code == temp[, doc_field_code]]
    
    # match field together 
    res = res[temp[, .(doc_field_code, missing_year)], on = .(doc_field_code)]
    
    # calculate the difference between the years that do have curve fits and the missing year
    res[, year_diff := abs(start_year - missing_year)]
    
    # sort res by year difference
    setorder(res, year_diff)
    
    # select the top five year_diff (therefore the closest 5 start years to the missing year)
    sel = res[1:5]
    
    # take the medians of the results
    stat = sel[, lapply(.SD, median, na.rm = T), .SDcols = c('q_i', 'D', 'b', 'd', 'int_yr')]
    
    # remove missing year column
    temp[, missing_year := NULL]
    
    # save results 
    l_stat_mult[[i]] = data.table(temp, 
                                  q_i = stat[, q_i], 
                                  D = stat[, D], 
                                  b = stat[, b],
                                  d = stat[, d],
                                  int_yr = stat[, int_yr])

    rm(temp, res, sel, stat, i)
    
    
  }
  
  res_stat_fit = rbindlist(l_stat_mult)
  res_stat_fit[, type := 'hyperbolic and exponential coefs (averaged)']

# for remaining field-start years, perform exponential decline ------
  
  l_nls_exp = list()
  
  for (i in 1:nrow(non_fields)) {
    
    temp = prod_fv_year[non_fields[i], on = c('doc_field_code', 'doc_fieldname', 'start_year'), nomatch = 0] # get production for specified field-start year
    temp2 = temp[peak_year_diff >= 0] # only keep years from peak year onwards
    temp2 = temp2[!prod_rate %in% boxplot(temp2[, prod_rate])$out]
    
    total_prod = sum(temp2[, tot_oil_prod])
    
    if (total_prod > 0) { # CHECK 1: if no production at all, skip
      
      if (nrow(temp2) >= 5) {
        
        temp2[, t := 1:nrow(temp2)] # add sequence of years
        q_i = max(temp2[, prod_rate], na.rm = T) # calculate q_i (peak production rate)
        D_h = max(abs(temp2[!is.infinite(decline_rate) & !is.na(decline_rate) & decline_rate < 0, decline_rate][1:5]), na.rm = T) # calculate D_h (initial decline rate) by taking max of first five decline rates
        
        expmod = nls(prod_rate ~ q_i*exp(-d*t), data = temp2, start = list(d = -0.008)) # fit exponential curve
        dval = coef(expmod)[1]
        
        l_nls_exp[[i]] = data.table(non_fields[i], 
                                    q_i = q_i, 
                                    D = D_h, 
                                    d = dval)
        
      } else {
        i = i + 1
      }
      
    } else {
      i = i + 1
    }
    
    rm(i, temp, temp2, total_prod, q_i, D_h, expmod, dval)

    # ggplot(temp2, aes(t, prod_rate)) + geom_point()

  }
  
  res_exp_fit = rbindlist(l_nls_exp)
  res_exp_fit[, type := 'exponential coef (curve-fitted)']
  
# count fields that can be matched to averaged exponential fits -----
  
  last_fields = non_fields[!res_exp_fit, on = .(doc_field_code, start_year)]
  last_fields = last_fields[doc_field_code %in% unique(res_exp_fit[, doc_field_code])]
  
# match fields that could not be fit to exponential curves -----
  
  l_stat_exp = list()
  
  for (i in 1:nrow(last_fields)) {
    
    temp = last_fields[i]
    temp[, missing_year := start_year]
    
    # get regression results for all start years in the same field 
    res = res_exp_fit[doc_field_code == temp[, doc_field_code]]
    
    # match field together 
    res = res[temp[, .(doc_field_code, missing_year)], on = .(doc_field_code)]
    
    # calculate the difference between the years that do have curve fits and the missing year
    res[, year_diff := abs(start_year - missing_year)]
    
    # sort res by year difference
    setorder(res, year_diff)
    
    # select the top five year_diff (therefore the closest 5 start years to the missing year)
    sel = res[1:5]
    
    # take the medians of the results
    stat = sel[, lapply(.SD, median, na.rm = T), .SDcols = c('q_i', 'D', 'd')]
    
    # remove missing year column
    temp[, missing_year := NULL]
    
    # save results 
    l_stat_exp[[i]] = data.table(temp, 
                                 q_i = stat[, q_i], 
                                 D = stat[, D], 
                                 d = stat[, d])
    
    rm(temp, res, sel, stat, i)
    
    
  }
  
  res_stat_exp = rbindlist(l_stat_exp)
  res_stat_exp[, type := 'exponential coef (averaged)']

# final list of fields that don't have any coefficients -------
  
  final_fields = non_fields[!doc_field_code %in% unique(res_exp_fit[, doc_field_code])]
  final_fields[, type := 'no coefs (no longer producing)']
  
# combine all results -------
  
  res_all = rbindlist(list(res_mult_fit, res_stat_fit, res_exp_fit, res_stat_exp, final_fields), use.names = T, fill = T)
  setorder(res_all, doc_field_code, start_year)
  
# combine parameters for field-vintages with peak production and well info -----
  
  res_info = merge(res_all, peak_prod, by = c('doc_fieldname', 'doc_field_code', 'start_year', 'no_wells'))
  
  setcolorder(res_info, c('doc_fieldname', 'doc_field_code', 'start_year', 'no_wells', 'q_i', 'D', 'b', 'd', 'int_yr', 
                          'peak_prod_year', 'peak_tot_prod', 'peak_avg_well_prod', 'peak_well_prod_rate', 'type'))

# save to csv ------
  
  fwrite(res_info, file.path(emlab_path, save_path, save_file), row.names = F)
  