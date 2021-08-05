# compare historic ghg emissions using ghg factors vs ghg inventory
# created: august 5, 2021
# author: @measrainsey

# inputs -------------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  res_path        = 'data/OPGEE'
  prod_file       = 'outputs/stocks-flows/crude_prod_x_field_revised.csv'
  entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  inj_file        = 'data/stocks-flows/processed/injection-by-well-type-per-field-per-year_1977-2018_revised.csv'
  inv_file        = 'data/stocks-flows/processed/historic_ghg_emissions_og_ng_adjusted.csv'
  
# outputs -----------
  
  save_path       = 'outputs/ghg-emissions/opgee-results'

# load packages ------
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
  
# read in opgee results files -----
  
  files_res = list.files(path = file.path(emlab_path, res_path), pattern = 'opgee_doc_results')
  list_res = lapply(file.path(emlab_path, res_path, files_res), fread)
  dt_res = rbindlist(list_res)
  
# read in entry file to get full list of fields ------
  
  dt_entry = fread(file.path(emlab_path, entry_file), header = T)
  
# load field-level production ------
  
  dt_prod = fread(file.path(emlab_path, prod_file), header = T)
  
# load injection data ------
  
  dt_inj = fread(file.path(emlab_path, inj_file), header = T)
  
# read in historic ghg inventory data ------
  
  dt_inv = fread(file.path(emlab_path, inv_file), header = T)
  
# pad field code with leading zeroes -----
  
  dt_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_entry[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_inj[, doc_field_code := sprintf("%03s", doc_field_code)]
  
# get unique set of field codes ------
  
  un_fields = unique(dt_entry[, .(doc_field_code, doc_fieldname)])
  
# calculate upstream emissions ------
  
  dt_res[, upstream_gCO2e_MJ := exploration_gCO2e_MJ + drilling_gCO2e_MJ + crude_production_gCO2e_MJ + 
           surface_processing_gCO2e_MJ + maintenance_gCO2e_MJ + waste_gCO2e_MJ + other_gCO2e_MJ]
  dt_res[, upstream_kgCO2e_bbl := upstream_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  
# get all fields that ever have sor > 0 ------
  
  fields_sor = unique(dt_res[sor_bbl_bbl > 0, field_name])
  
# # get top 10 producing fields in 2019 ------
#   
#   dt_prod[, order := frank(-total_bbls), by = .(year)]
#   fields_top10 = dt_prod[year == max(year) & order %in% 1:10]
#   
#   fields_top10[, field_name := fifelse(doc_fieldname == 'Belridge  South', 'Belridge, South', doc_fieldname)]
  
# get mean emission factors of fields that use steam ------
  
  emfac_steam = dt_res[field_name %in% fields_sor & year < 2019]
  mean_steam = emfac_steam[, .(upstream_kgCO2e_bbl = mean(upstream_kgCO2e_bbl, na.rm = T)), by = .(year)]
  
# get mean emission factos of fields that do not use steam ------
  
  emfac_nonsteam = dt_res[!field_name %in% fields_sor & year < 2019]
  mean_nonsteam = emfac_nonsteam[, .(upstream_kgCO2e_bbl = mean(upstream_kgCO2e_bbl, na.rm = T)), by = .(year)]
  
# linearly regress emission factors for steam fields --------
  
  pred_steam = data.table(year = seq(2000, 2019, 1))
  lm_steam = lm(upstream_kgCO2e_bbl ~ year, mean_steam)
  pred_steam[, upstream_kgCO2e_bbl := predict(lm_steam, pred_steam)]
  pred_steam[, perc_change := (upstream_kgCO2e_bbl - shift(upstream_kgCO2e_bbl, type = 'lag'))/shift(upstream_kgCO2e_bbl, type = 'lag')]
  pred_steam[, type := 'steam']
  
# linearly regress emission factors for non-steam fields --------
  
  pred_nonsteam = data.table(year = seq(2000, 2019, 1))
  lm_nonsteam = lm(upstream_kgCO2e_bbl ~ year, mean_nonsteam)
  pred_nonsteam[, upstream_kgCO2e_bbl := predict(lm_nonsteam, pred_steam)]
  pred_nonsteam[, perc_change := (upstream_kgCO2e_bbl - shift(upstream_kgCO2e_bbl, type = 'lag'))/shift(upstream_kgCO2e_bbl, type = 'lag')]
  pred_nonsteam[, type := 'non-steam']
  
# combine all predicted emission factors -------
  
  pred_emfac = rbindlist(list(pred_steam, pred_nonsteam), use.names = T)
  
# # get 2018 emission factors ------
#   
#   emfac_2000 = dt_res[year == 2000]
  
# fix field names to match DOC naming conventions -----
  
  dt_res[, doc_fieldname := field_name]
  dt_res[, doc_fieldname := gsub(',', '', doc_fieldname)]
  dt_res[doc_fieldname %like% 'North$', doc_fieldname := gsub('North', ' North', doc_fieldname)]
  dt_res[doc_fieldname %like% 'South$', doc_fieldname := gsub('South', ' South', doc_fieldname)]
  dt_res[doc_fieldname %like% 'East$', doc_fieldname := gsub('East', ' East', doc_fieldname)]
  dt_res[doc_fieldname %like% 'West$', doc_fieldname := gsub('West', ' West', doc_fieldname)]
  dt_res[doc_fieldname %like% 'N$', doc_fieldname := gsub('N', ' North', doc_fieldname)]
  dt_res[doc_fieldname %like% 'S$', doc_fieldname := gsub('S', ' South', doc_fieldname)]
  dt_res[doc_fieldname %like% 'Northwest$', doc_fieldname := gsub('Northwest', ' Northwest', doc_fieldname)]
  dt_res[doc_fieldname == 'Elwood S. Offshore', doc_fieldname := 'Elwood  South  Offshore']
  
# match field codes to emission factors -----
  
  dt_res = un_fields[dt_res, on = .(doc_fieldname)]
  
# separate fields that don't use steam injection with fields that do -----

  dt_inj[well_type_name %in% c('Cyclic Steam', 'Steam Flood'), type := 'steam']
  dt_inj[!well_type_name %in% c('Cyclic Steam', 'Steam Flood'), type := 'other']

  field_inj_agg = dt_inj[year %in% seq(2000, 2018, 1), .(type_inj = sum(type_inj, na.rm = T)), by = .(doc_field_code, doc_fieldname, type)]

  steam_fields = field_inj_agg[doc_field_code %in% field_inj_agg[type == 'steam' & type_inj > 0, doc_field_code]]
  nonsteam_fields = field_inj_agg[!doc_field_code %in% field_inj_agg[type == 'steam' & type_inj > 0, doc_field_code]]

# # separate emission factors by steam and non steam fields -----
#   
#   steam_ghg = dt_res[doc_field_code %in% steam_fields[, doc_field_code]]
#   nonsteam_ghg = dt_res[doc_field_code %in% nonsteam_fields[, doc_field_code]]
#   
# # get mean for steam and non-steam emission factors
#   
#   steam_ghg_mean = steam_ghg[, lapply(.SD, mean, na.rm = T), .SDcols = c('upstream_kgCO2e_bbl'), by = .(year)] 
#   nonsteam_ghg_mean = nonsteam_ghg[, lapply(.SD, mean, na.rm = T), .SDcols = c('upstream_kgCO2e_bbl'), by = .(year) ] 
  
# get emissions for all fields -------
  
  un_fields[, k := 1]
  pred_years = data.table(year = seq(1977, 2019, 1))
  pred_years[, k := 1]
  
  pred_all = un_fields[pred_years, on = .(k), allow.cartesian = T]
  pred_all[, k := NULL]
  un_fields[, k := NULL]
  pred_years[, k := NULL]
  
# loop: calculate ghg factors over time for all fields ------------
  
  l_ghg_pred = list() 
  
  for (i in seq_along(pred_years[, year])) {
    
    t = pred_years[i, year]
    
    df = pred_all[year == t]
    
    if (t <= 2000) {
      
      # first match with opgee results
      df2 = dt_res[year == 2000, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)][df, on = .(doc_field_code, doc_fieldname)]
      
      # for NA ghg factors, match with steam or non-steam average
      df2[is.na(upstream_kgCO2e_bbl), upstream_kgCO2e_bbl := ifelse(doc_field_code %in% steam_fields[, doc_field_code],
                                                                    mean_steam[year == 2000, upstream_kgCO2e_bbl],
                                                                    mean_nonsteam[year == 2000, upstream_kgCO2e_bbl])]
      
    } else {
      
      prev = copy(l_ghg_pred[[i-1]])
      setnames(prev, 'upstream_kgCO2e_bbl', 'prev_upstream_kgCO2e_bbl')
      
      prev = prev[, .(doc_field_code, doc_fieldname, prev_upstream_kgCO2e_bbl)]
      
      df[, type := fifelse(doc_field_code %in% steam_fields[, doc_field_code], 'steam', 'non-steam')]
      df2 = pred_emfac[df, on = .(type, year)]

      df2 = df2[prev, on = .(doc_field_code, doc_fieldname)]
      df2[, upstream_kgCO2e_bbl := (1+perc_change) * prev_upstream_kgCO2e_bbl]
      
      df2 = df2[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl, year)]
        
    }
    
    l_ghg_pred[[i]] = df2
    
    
  }
  
  dt_emfac = rbindlist(l_ghg_pred)
  
# calculate modeled ghg emissions -------
  
  dt_prod_un = dt_prod[doc_field_code %in% un_fields[, doc_field_code]]
  dt_prod_ghg = dt_prod_un[dt_emfac, on = .(doc_field_code, doc_fieldname, year)]
  
  dt_prod_ghg[, upstream_kgCO2e := total_bbls * upstream_kgCO2e_bbl]
  
  prod_ghg_agg = dt_prod_ghg[, .(upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T)), by = .(year)]
  prod_ghg_agg[, mtco2e := upstream_kgCO2e/1e9]
  prod_ghg_agg[, emissions_type := 'modeled']
  
  prod_ghg_agg = prod_ghg_agg[, .(year, emissions_type, mtco2e)] 
  
  prod_agg = dt_prod[, .(prod_bbl = sum(total_bbls, na.rm = T)), by = .(year)]
  
  
# combine all emission types -----
  
  dt_inv[, emissions_type := gsub('mtco2e', 'inventory', emissions_type)]
  
  ghg_all = rbind(dt_inv, prod_ghg_agg)
  ghg_all[, emissions_type := factor(emissions_type, levels = c('inventory', 'adj_inventory', 'modeled'))]
  
  
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
  
  # figure: ghg emissions over time ---------
  
    fig_ghg = ggplot(ghg_all[year >= 2000 & year <= 2018], aes(year, mtco2e, type = emissions_type, color = emissions_type)) + 
      geom_line(size = 1) + 
      labs(title = 'State-wide GHG emissions by type (2000-2018)',
           subtitle = 'Upstream emissions (MT CO2e)',
           x = NULL,
           y = NULL,
           color = NULL,
           caption = 'Modeled emissions only include fields modeled in entry (263 fields total)') + 
      scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000,2018), expand = c(0,0)) +
      # scale_y_continuous(breaks = seq(0,400,100), limits = c(0,400), expand = c(0,0)) +
      scale_color_ipsum() +
      theme_line
  fig_ghg
  
  ggsave(fig_ghg,
         filename = file.path(emlab_path, save_path, 'line_historic_ghg_emissions_comparison.png'),
         width = 12,
         height = 8,
         dpi = 600, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_ghg,
         filename = file.path(emlab_path, save_path, 'line_historic_ghg_emissions_comparison.pdf'),
         width = 12,
         height = 8,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(emlab_path, save_path, 'line_historic_ghg_emissions_comparison.pdf'),
              outfile = file.path(emlab_path, save_path, 'line_historic_ghg_emissions_comparison.pdf'))
  
  
  # figure: production over time ---------
  
    # ggplot(prod_agg[year >= 2000 & year <= 2018], aes(year, prod_bbl)) + geom_line()

  # diagnostics -------
  
    check2017 = dt_prod_ghg[year == 2017]
    setnames(check2017, c('total_bbls', 'upstream_kgCO2e_bbl', 'upstream_kgCO2e'), c('total_bbls_2017', 'upstream_kgCO2e_bbl_2017', 'upstream_kgCO2e_2017'))
    check2018 = dt_prod_ghg[year == 2018]
    check = check2017[check2018, on = .(doc_field_code, doc_fieldname)]
    check[, delta_prod := total_bbls - total_bbls_2017]
    check[, delta_ghg := upstream_kgCO2e - upstream_kgCO2e_2017]
    