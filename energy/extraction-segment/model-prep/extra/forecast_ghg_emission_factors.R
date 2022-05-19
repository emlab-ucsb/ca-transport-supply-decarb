# forecast ghg emission factors
# created: june 23, 2021
# author: @measrainsey

# inputs ------------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  res_path        = 'data/OPGEE'
  prod_file       = 'outputs/stocks-flows/crude_prod_x_field_revised.csv'
  entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  inj_file        = 'data/stocks-flows/processed/injection-by-well-type-per-field-per-year_1977-2018_revised.csv'
  
# outputs -----------
  
  save_path       = 'outputs/ghg-emissions/opgee-results'
  out_path        = 'outputs/stocks-flows'
  out_file        = 'ghg_emissions_x_field_2018-2045.csv'
  hist_file       = 'ghg_emissions_x_field_historic.csv'

# load packages ------
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# read in results files -----
  
  files_res = list.files(path = file.path(emlab_path, res_path), pattern = 'opgee_doc_results')
  list_res = lapply(file.path(emlab_path, res_path, files_res), fread)
  dt_res = rbindlist(list_res)
  
# read in entry file to get full list of fields ------
  
  dt_entry = fread(file.path(emlab_path, entry_file), header = T)
  
# load field-level production ------
  
  dt_prod = fread(file.path(emlab_path, prod_file), header = T)
  
# load injection data ------
  
  dt_inj = fread(file.path(emlab_path, inj_file), header = T)
  
# pad field code with leading zeroes -----
  
  dt_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_entry[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_inj[, doc_field_code := sprintf("%03s", doc_field_code)]
  
# get unique set of field codes ------
  
  un_fields = unique(dt_prod[, .(doc_field_code, doc_fieldname)])

# calculate upstream emissions ------
  
  dt_res[, upstream_gCO2e_MJ := exploration_gCO2e_MJ + drilling_gCO2e_MJ + crude_production_gCO2e_MJ + 
           surface_processing_gCO2e_MJ + maintenance_gCO2e_MJ + waste_gCO2e_MJ + other_gCO2e_MJ]
  dt_res[, upstream_kgCO2e_bbl := upstream_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  
# save historic values ------
  
  historic_vals <- dt_res[, .(field_name, year, upstream_kgCO2e_bbl)]
  
  # fix field names to match DOC naming conventions -----
  
  historic_vals[, doc_fieldname := field_name]
  historic_vals[, doc_fieldname := gsub(',', '', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'North$', doc_fieldname := gsub('North', ' North', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'South$', doc_fieldname := gsub('South', ' South', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'East$', doc_fieldname := gsub('East', ' East', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'West$', doc_fieldname := gsub('West', ' West', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'N$', doc_fieldname := gsub('N', ' North', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'S$', doc_fieldname := gsub('S', ' South', doc_fieldname)]
  historic_vals[doc_fieldname %like% 'Northwest$', doc_fieldname := gsub('Northwest', ' Northwest', doc_fieldname)]
  historic_vals[doc_fieldname == 'Elwood S. Offshore', doc_fieldname := 'Elwood  South  Offshore']
  
  historic_vals[, field_name := NULL]
  
  fwrite(historic_vals, file.path(emlab_path, out_path, hist_file), row.names = F)
  
# get all fields that ever have sor > 0 ------
  
  fields_sor = unique(dt_res[sor_bbl_bbl > 0, field_name])
  
# get top 10 producing fields in 2019 ------
  
  dt_prod = fread(file.path(emlab_path, prod_file))
  dt_prod[, order := frank(-total_bbls), by = .(year)]
  fields_top10 = dt_prod[year == max(year) & order %in% 1:10]
  
  fields_top10[, field_name := fifelse(doc_fieldname == 'Belridge  South', 'Belridge, South', doc_fieldname)]
  
# get mean emission factors of fields that use steam ------
  
  emfac_steam = dt_res[field_name %in% fields_sor & year < 2019]
  mean_steam = emfac_steam[, .(upstream_kgCO2e_bbl = mean(upstream_kgCO2e_bbl, na.rm = T)), by = .(year)]
  
# get mean emission factos of fields that do not use steam ------
  
  emfac_nonsteam = dt_res[!field_name %in% fields_sor & year < 2019]
  mean_nonsteam = emfac_nonsteam[, .(upstream_kgCO2e_bbl = mean(upstream_kgCO2e_bbl, na.rm = T)), by = .(year)]
  
# linearly regress emission factors for steam fields --------
  
  pred_steam = data.table(year = seq(2018, 2045, 1))
  lm_steam = lm(upstream_kgCO2e_bbl ~ year, mean_steam)
  pred_steam[, upstream_kgCO2e_bbl := predict(lm_steam, pred_steam)]
  pred_steam[, perc_change := (upstream_kgCO2e_bbl - shift(upstream_kgCO2e_bbl, type = 'lag'))/shift(upstream_kgCO2e_bbl, type = 'lag')]
  pred_steam[, type := 'steam']
  
# linearly regress emission factors for non-steam fields --------
  
  pred_nonsteam = data.table(year = seq(2018, 2045, 1))
  lm_nonsteam = lm(upstream_kgCO2e_bbl ~ year, mean_nonsteam)
  pred_nonsteam[, upstream_kgCO2e_bbl := predict(lm_nonsteam, pred_steam)]
  pred_nonsteam[, perc_change := (upstream_kgCO2e_bbl - shift(upstream_kgCO2e_bbl, type = 'lag'))/shift(upstream_kgCO2e_bbl, type = 'lag')]
  pred_nonsteam[, type := 'non-steam']
  
# combine all predicted emission factors -------
  
  pred_emfac = rbindlist(list(pred_steam, pred_nonsteam), use.names = T)
  
# get 2018 emission factors ------
  
  emfac_2019 = dt_res[year == 2018]
  
# fix field names to match DOC naming conventions -----
  
  emfac_2019[, doc_fieldname := field_name]
  emfac_2019[, doc_fieldname := gsub(',', '', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'North$', doc_fieldname := gsub('North', ' North', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'South$', doc_fieldname := gsub('South', ' South', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'East$', doc_fieldname := gsub('East', ' East', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'West$', doc_fieldname := gsub('West', ' West', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'N$', doc_fieldname := gsub('N', ' North', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'S$', doc_fieldname := gsub('S', ' South', doc_fieldname)]
  emfac_2019[doc_fieldname %like% 'Northwest$', doc_fieldname := gsub('Northwest', ' Northwest', doc_fieldname)]
  emfac_2019[doc_fieldname == 'Elwood S. Offshore', doc_fieldname := 'Elwood  South  Offshore']
  
# match field codes to emission factors -----
  
  emfac_2019 = un_fields[emfac_2019, on = .(doc_fieldname)]
  
# separate fields that don't use steam injection with fields that do -----
  
  dt_inj[well_type_name %in% c('Cyclic Steam', 'Steam Flood'), type := 'steam']
  dt_inj[!well_type_name %in% c('Cyclic Steam', 'Steam Flood'), type := 'other']
  
  field_inj_agg = dt_inj[, .(type_inj = sum(type_inj, na.rm = T)), by = .(doc_field_code, doc_fieldname, year, type)]
  
  steam_fields = field_inj_agg[doc_field_code %in% field_inj_agg[year == 2018 & type == 'steam' & type_inj > 0, doc_field_code]]
  nonsteam_fields = field_inj_agg[!doc_field_code %in% field_inj_agg[type == 'steam' & type_inj > 0, doc_field_code]]
  
# separate emission factors by steam and non steam fields -----
  
  steam_ghg = emfac_2019[doc_field_code %in% steam_fields[, doc_field_code]]
  nonsteam_ghg = emfac_2019[doc_field_code %in% nonsteam_fields[, doc_field_code]]
  
# get mean for steam and non-steam emission factors
  
  steam_ghg_mean = steam_ghg[, lapply(.SD, mean, na.rm = T), .SDcols = c('upstream_kgCO2e_bbl') ] 
  nonsteam_ghg_mean = nonsteam_ghg[, lapply(.SD, mean, na.rm = T), .SDcols = c('upstream_kgCO2e_bbl') ] 
  
# get emissions for all fields -------
  
  all_fields = emfac_2019[un_fields, on = .(doc_field_code, doc_fieldname)]
  all_fields = all_fields[, .(doc_field_code, doc_fieldname, year, upstream_kgCO2e_bbl)]
  all_fields[, year := 2018]
  
# for NA fields, decide to match with either median of steam or non-steam fields -----
  
  all_fields[is.na(upstream_kgCO2e_bbl), upstream_kgCO2e_bbl := ifelse(doc_field_code %in% steam_fields[, doc_field_code],
                                                                       steam_ghg_mean[, upstream_kgCO2e_bbl],
                                                                       nonsteam_ghg_mean[, upstream_kgCO2e_bbl])]

# get full span of fields and predict years -------  

  un_fields[, k := 1]
  pred_years = data.table(year = seq(2018, 2045, 1))
  pred_years[, k := 1]
  
  pred_all = un_fields[pred_years, on = .(k), allow.cartesian = T]
  pred_all[, k := NULL]
  un_fields[, k := 1]
  
  match_pred_all = all_fields[pred_all, on = .(doc_field_code, doc_fieldname, year)]
  setnames(match_pred_all, 'upstream_kgCO2e_bbl', 'calc_upstream_kgCO2e_bbl')
  
# match full span of fields and predict years with percent change --------
  
  match_pred_all[, type := fifelse(doc_field_code %in% steam_fields[, doc_field_code], 'steam', 'non-steam')]
  match_pred_all = match_pred_all[pred_emfac, on = .(year, type)]
  setnames(match_pred_all, 'upstream_kgCO2e_bbl', 'pred_upstream_kgCO2e_bbl')
  
# calculated future emission factors ------
  
  setkey(match_pred_all, doc_field_code, year)
  # for 2018, make final emissions factor = 2018 modeled emissions factor
  match_pred_all[year == 2018, upstream_kgCO2e_bbl := calc_upstream_kgCO2e_bbl]
  
  # for all following years, use percent growth 
  
  l_fin_pred = list()
  
  next_years = seq(2018, 2045, 1)
  
  for (i in seq_along(next_years)) {
    
    if (next_years[i] == 2018) {
      l_fin_pred[[i]] = copy(match_pred_all[year == 2018])
    } else {
      
      cur = match_pred_all[year == next_years[i]]
      prev = copy(l_fin_pred[[i-1]])
      
      setnames(prev, 'upstream_kgCO2e_bbl', 'prev_upstream_kgCO2e_bbl')
      
      prev = prev[, .(doc_field_code, doc_fieldname, prev_upstream_kgCO2e_bbl)]
      temp = cur[prev, on = .(doc_field_code, doc_fieldname)]
      temp[, upstream_kgCO2e_bbl := (1+perc_change) * prev_upstream_kgCO2e_bbl]
      
      temp[, prev_upstream_kgCO2e_bbl := NULL]
      
      l_fin_pred[[i]] = temp
      
      rm(cur, prev, temp)
      
    }
  }
  
  final_pred_all = rbindlist(l_fin_pred, use.names = T)
  
# add tag for steam field ------
  
  final_pred_all[, steam_field := fifelse(type == 'steam', 'yes', 'no')]

# save select columns -------
  
  final_pred_all_sel = final_pred_all[, .(doc_field_code, doc_fieldname, year, steam_field, upstream_kgCO2e_bbl)]
  
# export file to csv ------
  
  fwrite(final_pred_all_sel, file.path(emlab_path, out_path, out_file), row.names = F)

# ------------------------------- plot ---------------------------
  
  # theme ---------
  
  theme_line = theme_ipsum(base_family = 'Secca Soft',
                           grid = 'Y', 
                           plot_title_size = 20, 
                           subtitle_size = 18,
                           axis_title_just = 'center',
                           axis_title_size = 18, 
                           axis_text_size = 16,
                           strip_text_size = 16)  +
    theme(plot.title = element_text(hjust = 0, face = 'bold'),
          plot.title.position = 'plot',
          plot.subtitle = element_text(hjust = 0),
          plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
          axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
          axis.line.x = element_line(color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          axis.ticks.length.x = unit(0.25, "cm"),
          axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
          plot.margin = unit(c(1,2,1,1), "lines"),
          legend.text = element_text(size = 16),
          legend.position = 'bottom')
  
  
  
  # plot linearly fit upstream emission factors of sor fields ---------
  
    fig_line_sor = ggplot(dt_res[field_name %in% fields_sor & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_line() +
      # geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Emission factors for fields that use steam injection (2000-2018)',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,400,100), limits = c(0,400), expand = c(0,0)) +
      theme_line
  fig_line_sor
  
  ggsave(fig_line_sor,
         filename = file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.png'),
         width = 12,
         height = 8,
         dpi = 600, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_line_sor,
         filename = file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.pdf'),
         width = 12,
         height = 8,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.pdf'),
              outfile = file.path(emlab_path, save_path, 'line_emission_factors_steam_fields.pdf'))
  
  
  # plot linearly fit upstream emission factors of sor fields ---------
    
    fig_lm_sor = ggplot(dt_res[field_name %in% fields_sor & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Linearly regressed emission factors for fields that use steam injection',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
    scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,400,100), limits = c(0,400), expand = c(0,0)) +
      theme_line
    fig_lm_sor
    
    ggsave(fig_lm_sor,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.png'),
           width = 12,
           height = 8,
           dpi = 600, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_lm_sor,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.pdf'),
           width = 12,
           height = 8,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.pdf'),
                outfile = file.path(emlab_path, save_path, 'fit_emission_factors_steam_fields.pdf'))
    
    
  # plot emissions factors for top 10 fields ------
  
    fig_line_top10 = ggplot(dt_res[field_name %in% fields_top10[, field_name] & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_line() +
      # geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Emission factors for top 10 producing fields (2000-2018)',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,200,50), limits = c(0,200), expand = c(0,0)) +
      theme_line
    fig_line_top10
    
    ggsave(fig_line_top10,
           filename = file.path(emlab_path, save_path, 'line_emission_factors_top_fields.png'),
           width = 12,
           height = 8,
           dpi = 600, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_line_top10,
           filename = file.path(emlab_path, save_path, 'line_emission_factors_top_fields.pdf'),
           width = 12,
           height = 8,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(emlab_path, save_path, 'line_emission_factors_top_fields.pdf'),
                outfile = file.path(emlab_path, save_path, 'line_emission_factors_top_fields.pdf'))

  # plot linearly fit emissions factors for top 10 fields ------
    
    fig_lm_top10 = ggplot(dt_res[field_name %in% fields_top10[, field_name] & year < 2019], aes(x = year, y = upstream_kgCO2e_bbl, group = field_name, color = field_name)) + 
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      labs(title = 'Linearly regressed emission factors for top 10 producing fields',
           subtitle = 'Upstream emission factor (kg CO2e/bbl)',
           x = NULL,
           y = NULL,
           color = NULL) + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,200,50), limits = c(0,200), expand = c(0,0)) +
      theme_line
    fig_lm_top10
    
    ggsave(fig_lm_top10,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.png'),
           width = 12,
           height = 8,
           dpi = 600, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_lm_top10,
           filename = file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.pdf'),
           width = 12,
           height = 8,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.pdf'),
                outfile = file.path(emlab_path, save_path, 'fit_emission_factors_top_fields.pdf'))
    