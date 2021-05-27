# parameterize decline at field-vintage level (updated)
# created: may 25, 2021
# author: meas meng

# inputs ------

  emlab_dir       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  data_dir        = 'outputs/decline-historic/data'
  fv_year_fil     = 'production_field-vintange_yearly_entry_revised.csv'
  peak_fil        = 'field-vintage_peak-production_yearly_revised.csv'
  entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  plot_dir        = 'outputs/figures/interim-report-figures/drafts/fuels-model/decline_figs/'
  save_dir        = 'outputs/decline-historic/parameters/'

# load libraries -------- 

  library(data.table)  
  library(lubridate)
  library(zoo)
  library(stringr)
  library(segmented)
  library(openxlsx)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(RColorBrewer)

# read in data ------

  prod_fv_year = fread(file.path(emlab_dir, data_dir, fv_year_fil), colClasses = c(rep('character',2), rep(NA,12)))
  peak_prod = fread(file.path(emlab_dir, data_dir, peak_fil), header = T)
  dt_entry = fread(file.path(emlab_dir, entry_file), header = T)

# pad field code with leading zeroes -----

  prod_fv_year[, doc_field_code := sprintf("%03s", doc_field_code)]
  peak_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_entry[, doc_field_code := sprintf("%03s", doc_field_code)]

# get vintages in entry file ------

  dt_entry[, vintage:= cut(year, c(-Inf, 1977, 1982, 1987, 1992, 1997, 2002, 2007, 2012, 2019), 
                           c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))]

# get list of unique field-vintages -----

  un_fv = unique(prod_fv_year[, c('doc_fieldname', 'doc_field_code', 'vintage', 'no_wells')])
  un_fv = un_fv[order(doc_field_code, vintage)]

# get field-vintage combos in the entry df that are not in the production dataset -----  

  nonmatch = dt_entry[!un_fv, on = c('doc_field_code', 'vintage')]

# hyperbolic and exponential regression for each field-vintage at the yearly level --------
  
  hypfunc = function(b,t) {  q_i/((1 + b*D_h*t)^(1/b)) } # write hyperbolic function
  expfunc = function(d,t) {   q_i*exp(-d*t)      } # write exponential function

  l_nls_mult = list()
  for (i in 1:nrow(un_fv)) {
    
    temp = prod_fv_year[un_fv[i], on = c('doc_field_code', 'doc_fieldname', 'vintage'), nomatch = 0] # get production for specified field-vintage
    temp2 = temp[peak_year_diff >= 0] # only keep years from peak year onwards
    
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

        b_seq = seq(0.001, 2, 0.001) # sequence of b values to start at
        
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
          #                 field_name = un_fv[i, doc_fieldname],
          #                 field_code = un_fv[i, doc_field_code],
          #                 vintage = un_fv[i, vintage],
          #                 D_h = D_h)
          l_nls_mult[[i]] = data.table(un_fv[i], 
                                       q_i = q_i, 
                                       D = D_h, 
                                       b = bval,
                                       d = dval,
                                       int_yr = intercept_yr)
          
          
          # l_nls_mult[[i]] = dt
          
          rm(q_i,D_h,b_seq,b_num,nlsmod,expmod,bval,dval,intercept_yr,maxpredt,hypfit,expfit)
          
          
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

  
# get field-vintages that were unable to have hyperbolic + exponential fit ------
  
  non_mult_fit = un_fv[!res_mult_fit, on = .(doc_field_code, vintage)]

# for all field-vintages that couldn't fit hyperbolic + exponential, do hyperbolic fit ------
  
  l_nls_hyp = list()
  for (i in 1:nrow(non_mult_fit)) {
    
    # print(i)
    
    temp = prod_fv_year[non_mult_fit[i], on = c('doc_field_code', 'doc_fieldname', 'vintage'), nomatch = 0] # get production for specified field-vintage
    temp2 = temp[peak_year_diff >= 0] # only keep years from peak year onwards
    temp2[, t := 1:nrow(temp2)] # add sequence of years
    
    if (nrow(temp2) > 10) { # CHECK 1: if the number of rows for yearly production is less than 10, skip
      
      q_i = max(temp2[, prod_rate], na.rm = T) # calculate q_i (peak production rate)
      D_h = max(abs(temp2[!is.infinite(decline_rate) & !is.na(decline_rate) & decline_rate < 0, decline_rate][1:5]), na.rm = T) # calculate D_h (initial decline rate) by taking max of first five decline rates
      
      if (!is.infinite(D_h)) { # CHECK 2: if no initial decline rate can be found, skip
        
        b_seq = seq(0.001, 2, 0.001) # sequence of b values to start at
        
        for (b_num in 1:length(b_seq)) { # loop through the b values until a hyperbolic regression fit can be applied (if applicable)
          
          catch_hyp = tryCatch({ # check if hyperbolic years can be properly fitted using nls
            hyptest <- nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2, start = list(b = b_seq[b_num]))
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
        
        if (b_num <= length(b_seq)) { # CHECK 3: if no hyperbolic fit can be applied, skip
          
          hyponly = nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2, start = list(b = b_seq[b_num]))
          bval = coef(hyponly)[1]
          
          l_nls_hyp[[i]] = data.table(non_mult_fit[i], 
                                      q_i = q_i, 
                                      D = D_h, 
                                      b = bval)
          
          
        } else { # skip for check 3
          
          i = i + 1
          
        }
        
        
      } else { # skip for check 2
        i = i + 1
      }
      
    } else { # skip for check 1
      i = i + 1
    }
    
    rm(temp, temp2, q_i, D_h, b_seq, catch_hyp, hyponly, bval)
    
  }
  
  res_hyp_fit = rbindlist(l_nls_hyp)
  
# for all field-vintages that couldn't fit hyperbolic + exponential, do exponential fit ------
  
  l_nls_exp = list()
  for (i in 1:nrow(non_mult_fit)) {
    
    # print(i)
    
    temp = prod_fv_year[non_mult_fit[i], on = c('doc_field_code', 'doc_fieldname', 'vintage'), nomatch = 0] # get production for specified field-vintage
    temp2 = temp[peak_year_diff >= 0] # only keep years from peak year onwards
    temp2[, t := 1:nrow(temp2)] # add sequence of years
    
    if (nrow(temp2) > 10) { # CHECK 1: if the number of rows for yearly production is less than 10, skip
      
      q_i = max(temp2[, prod_rate], na.rm = T) # calculate q_i (peak production rate)
      D_h = max(abs(temp2[!is.infinite(decline_rate) & !is.na(decline_rate) & decline_rate < 0, decline_rate][1:5]), na.rm = T) # calculate D_h (initial decline rate) by taking max of first five decline rates
      
      catch_exp = tryCatch({ # check if exponential regression can be properly fitted using nls
        exptest <- nls(prod_rate ~ q_i*exp(-d*t), data = temp2, start = list(d = -0.008)) 
        ('yes')
      }, error = function(e) {
        ('no')
      })
      
        if (catch_exp == 'yes') { # CHECK 2: if exponential function cannot be fitted, skip
          
          expmod = nls(prod_rate ~ q_i*exp(-d*t), data = temp2, start = list(d = -0.008)) # fit second portion as exponential
          dval = coef(expmod)[1]
          
          l_nls_exp[[i]] = data.table(non_mult_fit[i], 
                                      q_i = q_i, 
                                      D = D_h, 
                                      d = dval)

        } else { # skip for check 2
          i = i + 1
        }
      
    } else { # skip for check 1
      i = i + 1
    }
    
    rm(temp, temp2, q_i, D_h, catch_exp, expmod, dval)
    
  }
  
  res_exp_fit = rbindlist(l_nls_exp)
  