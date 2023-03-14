# parameterize decline at field-vintage level
# created: august 12, 2020
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
  
  l_nls_year = list()
  for (i in 1:nrow(un_fv)) {
    
    temp = prod_fv_year[un_fv[i], on = c('doc_field_code', 'doc_fieldname', 'vintage'), nomatch = 0] # get production for specified field-vintage
    temp2 = temp[peak_year_diff >= 0] # only keep years from peak year onwards
    temp2[, t := 1:nrow(temp2)] # add sequence of years
    temp2[, moving_decline_rate := rollmean(decline_rate, 5, align = 'left', na.pad = T)] # calculate a moving decline rate by taking a 5-year left-aligned rolling mean
    exp_year = ifelse(is.na(temp2[moving_decline_rate < 0 & abs(moving_decline_rate) <= 10/100 & t >= quantile(t, 0.3) ][1, t]),
                      max(temp2[, t] + 1),
                      temp2[moving_decline_rate < 0 & abs(moving_decline_rate) <= 10/100 & t >= quantile(t, 0.3) ][1, t]) # condition to calculate estimate year when curve changes from hyperbolic to exponential
    temp2[t < exp_year, type := 'hyperbolic'] # assign years before exp_year as hyperbolic decline
    temp2[t >= exp_year, type := 'exponential'] # assign years after exp_year as exponential decline

    q_i = max(temp2[, prod_rate][1:3], na.rm = T) # calculate q_i (peak production rate)
    D_h = max(abs(temp2[, decline_rate][1:5]), na.rm = T) # calculate D_h (initial decline rate) by taking max of first five decline rates
    # D_e = mean(temp2[ type == 'exponential', decline_rate], na.rm = T)
    
    hypfunc = function(b,t) {  q_i/((1 + b*D_h*t)^(1/b)) } # write hyperbolic function
    expfunc = function(d,t) {   q_i*exp(-d*t)      } # write exponential function
    
    error = try(res <- nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2, start = list(b = 1))) # check if a field-vintage's production curve is able to be fitted as a hyperbolic decline
    
    if(class(error)!="try-error"){ # if the error check above does not given an error, then do the following:
      
      exponly = nls(prod_rate ~ q_i*exp(-d*t), data = temp2, start = list(d = -0.008)) # first, fit the entire production curve as an exponential decline
      temp2[, fitone := fitted(exponly)] 
      dval = coef(exponly)[1] # exponential decline coefficient
      
      # nlsonly = nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2, start = list(b = 1)) # fit the entire production curve as a hyperbolic decline
      # temp2[, fitone := fitted(nlsonly) ]
      # bval = coef(nlsonly)[1]
      # temp2[, hypfit := hypfunc(bval,(1:nrow(temp2)))]
      
        catchmult = tryCatch({ # check if hyperbolic years can be properly fitted using nls
          mult <- nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2[type == 'hyperbolic'], start = list(b = 1))
          ('yes')
        }, error = function(e) {
          ('no')
        })
      

        if ( catchmult == 'yes' & exp_year < max(temp2[, t]) ) { # if yes (can be fitted as hyperbolic), then fit the first portion as hyperbolic
          nlsmod = nls(prod_rate ~ q_i/((1+b*D_h*t)^(1/b)), data = temp2[type == 'hyperbolic'], start = list(b = 1))
          bval2 = coef(nlsmod)[1]
          
          # q_ie = hypfunc(bval2,exp_year)
          
          expmod = nls(prod_rate ~ q_i*exp(-d*t), data = temp2[type == 'exponential'], start = list(d = -0.008)) # fit second portion as exponential
          # temp2[type == 'hyperbolic' , fitmult := fitted(nlsmod) ]
          # temp2[type == 'exponential' , fitmult := fitted(expmod) ]
          dval2 = coef(expmod)[1]

          fullfunc  = function(t) {  q_i/((1 + bval2*D_h*t)^(1/bval2)) - q_i*exp(-dval2*t)  } # full function of hyperbolic + exponential
          
          intercept_yr = NA
          try(intercept_yr <- uniroot(f = fullfunc, interval = c(1, 100))$root, silent=T) # calculate root of full function to find intercept year
          if(is.na(intercept_yr)) intercept_yr = 1
          intercept_yr = round(intercept_yr,0)
          
          maxpredt = max(intercept_yr, nrow(temp2)) # max number of predicted years
          
          hypfit2 = hypfunc(bval2,(1:maxpredt)) # hyperbolic fitted line
          expfit = expfunc(dval2,(1:maxpredt)) # exponential fitted line
          
          # fullfunc  = function(t) {  q_i/((1 + bval2*D_h*t)^(1/bval2)) - q_i*exp(-dval*t)  }
          # intercept_yr = uniroot(f = fullfunc, interval = c(1, 50))$root
          # intercept_yr = round(intercept_yr,0)
          # 
          # temp2[, hypfit2 := hypfunc(bval2,(1:nrow(temp2)))]
          # temp2[, expfit := expfunc(dval,(1:nrow(temp2)))]

        } 
      

      l_nls_year[[i]] = data.table(un_fv[i], 
                                   q_i = q_i, 
                                   D = D_h, 
                                   d1 = dval,
                                   b2 = ifelse(catchmult == 'yes' & exp_year < max(temp2[, t]), bval2, NA),
                                   d2 = ifelse(catchmult == 'yes' & exp_year < max(temp2[, t]), dval2, NA),
                                   int_yr = ifelse(catchmult == 'yes' & exp_year < max(temp2[, t]), intercept_yr, NA))
      
      ptitle = paste0(un_fv[i, doc_field_code], ' - ', un_fv[i, doc_fieldname], ' - Vintage: ', un_fv[i, vintage], ' (', un_fv[i, no_wells], ' wells)')

      p = ggplot(temp2) + geom_line(aes( x =  t, y = prod_rate)) +
        geom_line(aes(x = t, y = fitone, color = 'exponly', lty = 'exponly'), size = 0.9) +
        # geom_line(aes(x = t, y = fitone, color = 'exponly'), color = 'orangered2', size = 0.9) +
        labs(title = NULL, # ifelse(catchmult == 'yes' & exp_year < max(temp2[, t]),
                           # paste0(ptitle, ' (d1 = ', signif(dval, 4), ')', '\n', '(b2 = ', signif(bval2, 4), ', d2 = ', signif(dval2, 4), ')'),
                           # paste0(ptitle, ' (d1 = ', signif(dval, 4), ')')),
             subtitle = NULL, #'Production aggregated to total produced during each year (12 months) relative to well start date',
             x = 'Number of years since peak year',
             y = 'Average production rate for each well (bbls/day)') +
        scale_color_manual(name = NULL, 
                           labels = c('exponly' = 'Exponential fit only'),
                           values = c('exponly' = 'orangered2')) +
        scale_linetype_manual(name = NULL, 
                              labels = c('exponly' = 'Exponential fit only'),
                              values = c('exponly' = 1)) +
        theme_ipsum(base_family = 'Arial',
                    grid = 'Y',
                    plot_title_size = 10, 
                    subtitle_size = 9,
                    axis_title_just = 'center',
                    axis_title_size = 9, 
                    axis_text_size = 9,
                    strip_text_size = 9) +
        theme(plot.title = element_text(hjust = 0, face = 'bold'),
              plot.title.position = 'plot',
              plot.subtitle = element_text(hjust = 0),
              plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
              axis.line.x = element_line(color = 'black'),
              axis.ticks.x = element_line(color = 'black'),
              axis.ticks.length.x = unit(0.2, 'cm'),
              axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
              axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
              legend.title = element_text(size = 8, vjust = 0.5),
              legend.text = element_text(size = 8, vjust = 0.5),
              legend.position = 'bottom',
              strip.text = element_text(hjust = 0.5, face = 'bold'),
              plot.margin = unit(c(1,3,1,1), 'lines'))

      if (catchmult == 'yes' & exp_year < max(temp2[, t])) {
        p = p + 
          # geom_line(data = data.table(t = 1:maxpredt, hypfit2 = hypfit2),
          #                 aes(x = t, y = hypfit2), color = 'steelblue2', size = 0.9, linetype = 2) +
          # geom_line(data = data.table(t = 1:maxpredt, expfit = expfit),
          #           aes(x = 1:maxpredt, y = expfit), color = 'palegreen4', size = 0.9, linetype = 2) +
          geom_line(data = data.table(t = 1:maxpredt, hypfit2 = hypfit2),
                    aes(x = t, y = hypfit2, color = 'hypfit', lty = 'hypfit'), size = 0.9) +
          geom_line(data = data.table(t = 1:maxpredt, expfit = expfit),
                    aes(x = 1:maxpredt, y = expfit, color = 'expfit', lty = 'expfit'), size = 0.9) +
          scale_color_manual(name = NULL, 
                             labels = c('exponly' = 'Exponential fit only',
                                        'hypfit' = 'Hyperbolic decay component',
                                        'expfit' = 'Exponential decay component'),
                             values = c('exponly' = 'orangered2',
                                        'hypfit' = 'steelblue2',
                                        'expfit' = 'palegreen4'),
                             guide = guide_legend(nrow = 1),
                             breaks = c('exponly', 'hypfit', 'expfit')) +
          scale_linetype_manual(name = NULL, 
                                labels = c('exponly' = 'Exponential fit only',
                                           'hypfit' = 'Hyperbolic decay component',
                                           'expfit' = 'Exponential decay component'),
                                values = c('exponly' = 1,
                                           'hypfit' = 2,
                                           'expfit' = 2),
                                guide = guide_legend(nrow = 1),
                                breaks = c('exponly', 'hypfit', 'expfit')) +
          geom_vline(xintercept = intercept_yr, color = 'tan2', lty = 2, size = 0.9)
      }

      # pname = paste0(plot_dir, 'field-vintage-year/', un_fv[i, doc_field_code], '_', un_fv[i, doc_fieldname], '_', un_fv[i, vintage], '_yearly_hyperbolic_exponential.pdf')
      fname = paste0(un_fv[i, doc_fieldname], '_', un_fv[i, vintage], '_yearly_hyperbolic_exponential.png')
      fname = gsub(' ', '_', fname)
      fname = gsub('-', '_', fname)
      
      pname = paste0(plot_dir, 'field-vintage-year/', fname)
      
      ggsave(p,
             filename = pname,
             width = 6.5,
             height = 4,
             dpi = 400, 
             units = 'in', 
             device = 'png')

      # ggsave(p,
      #        filename = pname,
      #        width = 10,
      #        height = 6.25)
      # 
      # embed_fonts(pname,
      #             outfile = pname)
    
    } else {
      i = i+1
    }
    
    rm(temp,temp2,nlsmod,expmod,p,q_i,D_h,bval,ptitle,exp_year,hypfunc,expfunc,fullfunc,bval2,dval,expfit,hypfit2,error,mult,res,nlsonly)

  }
  
  nls_fv_year =  rbindlist(l_nls_year)
  
# get medians for each field -------
  
  med_field_yr = nls_fv_year[, .(q_i = median(q_i, na.rm = T),
                                 D = median(D, na.rm = T),
                                 d1 = median(d1, na.rm = T),
                                 b2 = median(b2, na.rm = T),
                                 d2 = median(d2, na.rm = T),
                                 int_yr = median(int_yr, na.rm = T)), by = .(FieldName, FieldCode)]
  
# get medians for each vintage ------
  
  med_vintage_yr = nls_fv_year[, .(q_i = median(q_i, na.rm = T),
                                   D = median(D, na.rm = T),
                                   d1 = median(d1, na.rm = T),
                                   b2 = median(b2, na.rm = T),
                                   d2 = median(d2, na.rm = T),
                                   int_yr = median(int_yr, na.rm = T)), by = .(vintage)]
  
# get field-vintages that exist in entry model but not production data or could not be matched -----
  
  non_fv = dt_entry[!nls_fv_year, on = c('doc_field_code' = 'FieldCode', 'vintage')]
  non_fv = unique(non_fv[, c('doc_field_code', 'doc_fieldname', 'vintage') ])
  setnames(non_fv, 'doc_field_code', 'FieldCode')
  setnames(non_fv, 'doc_fieldname', 'FieldName')
  
# loop to match missing field-vintages to parameters ------
  
  list_fixes = list()
    for (i in 1:nrow(non_fv)) {
      
      fv = non_fv[i]
      checkexists = nrow(nls_fv_year[FieldCode %in% fv[, FieldCode]])
      nls_fv_year[FieldCode %in% fv[, FieldCode]]
      # fv[, no_wells := NA]
      
        if (checkexists > 0) {
          match = med_field_yr[FieldCode == fv[, FieldCode]]
          fv2 = fv[match, on = c('FieldCode', 'FieldName')]
        } else {
          match = med_vintage_yr[vintage == fv[, vintage]]
          fv2 = fv[match, on = c('vintage')]
        }
      
      list_fixes[[i]] = fv2
      
      rm(fv,checkexists,fv2)
      
    }
  
  dt_fixes = rbindlist(list_fixes, use.names = T, fill = T)
  
# combine NLS results with fixes ------
  
  nls_fv_year_2 = rbindlist(list(nls_fv_year, dt_fixes), use.names = T, fill = T)
  nls_fv_year_2[, vintage := factor(vintage, levels = c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))]
  setorderv(nls_fv_year_2, c('FieldCode', 'vintage'))
  
# combine parameters for field-vintages with peak production and well info -----
  
  nls_fv_year_info = merge(nls_fv_year, peak_prod, by = c('FieldCode', 'FieldName', 'vintage'))
  
  fv_info = merge(nls_fv_year_2, peak_prod, by = c('FieldCode', 'FieldName', 'vintage'))
  fv_info[, vintage := factor(vintage, levels = c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))]
  setorderv(fv_info, c('FieldCode', 'vintage'))
  
# check -----
  
  # dt = fread(paste0(data_dir, 'production_api12_monthly.csv'), header = T)
  # dt[, FieldCode := str_pad(FieldCode, 3, pad = '0') ]
  # dtnon = dt[non_fv, on = c('FieldCode' = 'doc_field_code', 'vintage'), nomatch = 0]
  # unnon = unique(dtnon[, c('FieldCode', 'vintage')])
  # unnon[!un_fv, on =  c('FieldCode', 'vintage')]
  
# export fitted parameters for all field-vintage combinations ------
  
  # fwrite(nls_fv_month, paste0(save_dir, 'fitted-parameters_field-vintage_monthly_entry.csv'), row.names = F)
  fwrite(nls_fv_year_info, paste0(save_dir, 'exponential_fitted-parameters_field-vintage_yearly_entry_modeled_only.csv'), row.names = F)
  fwrite(fv_info, paste0(save_dir, 'exponential_fitted-parameters_field-vintage_yearly_entry.csv'), row.names = F)
