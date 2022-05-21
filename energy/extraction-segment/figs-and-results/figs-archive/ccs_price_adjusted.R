# plot ccs prices by field and by refinery
# created: november 2, 2020
# author: meas meng

# inputs ------------

  proj_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
  ext_file        = 'input_variables_ccs_and_carbon_only.csv'
  refin_file      = 'input_variables_scenarios_refining.csv'
  refcap_file     = 'refinery_loc_cap.csv'
  
# selection ------  
  
  a               = 4

# outputs -------
  
  save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/ccs_price_adjusted'
  fig_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model'
  
# libraries ------

  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)

# source items ------

  # source ccs emissions mean b calculation script
    source(here::here('scripts', 'model', 'ghg-emissions', 'ccs_parameterization.R'))
    items = list.files(here::here('src'))
    sapply(here::here('src', items), source)
  
# load data --------
  
  # load ccs and carbon price scenarios for extraction
    dt_ext = fread(file.path(proj_path, 'project-materials/scenario-inputs', ext_file), header = T)
  
  # load ccs and carbon price scenarios for refining
    dt_ref = fread(file.path(proj_path, 'project-materials/scenario-inputs', refin_file), header = T)
    dt_ref = unique(dt_ref[, .(year, ccs_scenario, ccs_price_usd_per_kg)])
    
  # read in refinery capacities
    dt_refcap = fread(file.path(proj_path, 'data/stocks-flows/processed', refcap_file), header = T)
    dt_refcap = dt_refcap[, c('site_id', 'refinery_name', 'barrels_per_day', 'location', 'county', 'cluster')]
    setnames(dt_refcap, 'cluster', 'region')
    
# pad field codes with leading zeroes -----
  
  dt_ext[, doc_field_code := sprintf("%03d", doc_field_code)]
    
# get unique sets of refining scenarios ------
    
  ref_scens = CJ(year = 2020:2045, refinery_name = unique(dt_refcap[, refinery_name]))
  ref_scens = dt_ref[ref_scens, on = .(year), allow.cartesian = T]

# total cost function -------
  
  solve_tc <- function(a, b, q) {
    f <- (q*(a*b - a*(q^(1/a)) + b))/(a + 1)
    return(f)
  }
  
# get unique prices ------
  
  un_ext = unique(dt_ext[, .(year, ccs_scenario, ccs_price_usd_per_kg)])
  un_ref = unique(dt_ref[, .(year, ccs_scenario, ccs_price_usd_per_kg)])
  
# solve for ccs costs for fields -----

  ext_scens = unique(dt_ext[, .(year, doc_field_code, doc_fieldname, ccs_scenario, ccs_price_usd_per_kg)])
  ext_scens = ext_scens[unique(ghg_all[, .(doc_field_code, co2e_tonnes)]), on = .(doc_field_code), nomatch = 0]
  ext_scens[, co2e_kg := co2e_tonnes*1e3]
  ext_scens[, mean_b := solve_mean_b(a, ccs_price_usd_per_kg*1e3, 'extraction'), 
            by = .(ccs_scenario, year)]
  ext_scens[, total_cost := solve_tc(a, mean_b, co2e_tonnes)]
  ext_scens[, ccs_adj_usd_per_mt := total_cost/co2e_tonnes]
  ext_scens[, ccs_adj_usd_per_kg := total_cost/co2e_kg]
  # ext_scens[is.na(ccs_adj_usd_per_kg), ccs_adj_usd_per_kg := ccs_price_usd_per_kg] # if no a value (zero emissions), use non-field adjusted ccs price
  
# solve for ccs costs for refineries -----
  
  ref_scens = merge(ref_scens,
                    unique(ghg_all[!is.na(refinery_name), .(refinery_name, co2e_tonnes)]),
                    by = 'refinery_name',
                    all.x = T)
  ref_scens[, co2e_kg := co2e_tonnes*1e3]
  ref_scens[, mean_b := solve_mean_b(a, ccs_price_usd_per_kg*1e3, 'refining'), 
            by = .(ccs_scenario, year)]
  ref_scens[, total_cost := solve_tc(a, mean_b, co2e_tonnes)]
  ref_scens[, ccs_adj_usd_per_mt := total_cost/co2e_tonnes]
  ref_scens[, ccs_adj_usd_per_kg := total_cost/co2e_kg]
  # ref_scens[is.na(ccs_adj_usd_per_kg), ccs_adj_usd_per_kg := ccs_price_usd_per_kg] # if no a value (zero emissions), use non-field adjusted ccs price
  
# reorder levels -------
  
  un_ext[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
  ext_scens[, ccs_scenario := factor(ccs_scenario, levels = c('low CCS cost', 'medium CCS cost', 'high CCS cost'))]
  un_ref[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
  ref_scens[, ccs_scenario := factor(ccs_scenario, levels = c('low CCS cost', 'medium CCS cost', 'high CCS cost'))]
  
# order rows -----
  
  setorder(ext_scens, ccs_scenario, year, doc_field_code)
  setorder(ref_scens, ccs_scenario, year, refinery_name)
  
# export to csv files -------
  
  fwrite(ext_scens, file.path(save_path, 'adjusted_ccs_prices_by_field.csv'), row.names = F)
  fwrite(ref_scens, file.path(save_path, 'adjusted_ccs_prices_by_refinery.csv'), row.names = F)
  
# ------------------------- plots ------------------------- 

  # plot theme & palettes ------
  
    theme_line = theme_ipsum(base_family = 'Arial',
                             grid = 'Y', 
                             plot_title_size = 10, 
                             subtitle_size = 9,
                             axis_title_just = 'center',
                             axis_title_size = 9, 
                             axis_text_size = 9,
                             strip_text_size = 9)  +
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
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
  
  # plot: field ccs prices -------
  
    fig_ccs_field = ggplot() +
      geom_line(data = ext_scens[co2e_tonnes > 0 & !is.na(ccs_adj_usd_per_mt)], 
                aes(x = year, y = ccs_adj_usd_per_mt, color = co2e_tonnes/1e6, group = doc_field_code)) +
      geom_line(data = un_ext, aes(x = year, y = ccs_price_usd_per_kg * 1e3), size = 0.7, color = '#252525', linetype = 2) +
      facet_wrap(vars(ccs_scenario), labeller = labeller(ccs_scenario = c('high CCS cost' = 'High CCS Cost', 
                                                                          'medium CCS cost' = 'Central CCS Cost', 
                                                                          'low CCS cost' = 'Low CCS Cost'))) +
      labs(title = NULL,
           subtitle = NULL, 
           caption = 'Dashed black line is non-adjusted CCS price',
           x = NULL,
           y = 'CCS cost by field (USD per metric tonne)',
           color = expression(paste("Million tonnes of ", CO[2], "e emitted in 2018"))) +
           # color = 'Million tonnes of CO2e emitted in 2018') +
      scale_color_gradient(low = blue_intensity[2], high = blue_intensity[1]) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0, 250), expand = c(0,0)) +
      theme_line +
      theme(legend.position = "top")
    # fig_ccs_field
  
    ggsave(fig_ccs_field,
           filename = file.path(fig_path, 'adjusted_ccs_prices_by_field.png'),
           width = 6.5,
           height = 4,
           dpi = 400, 
           units = 'in', 
           device='png')

    # ggsave(fig_ccs_field,
    #        filename = file.path(save_path, 'adjusted_ccs_prices_by_field.pdf'),
    #        width = 12,
    #        height = 8)
    # 
    # embed_fonts(file.path(save_path, 'adjusted_ccs_prices_by_field.pdf'),
    #             outfile = file.path(save_path, 'adjusted_ccs_prices_by_field.pdf'))
    
  # plot: refinery ccs prices -------
    
    fig_ccs_refinery = ggplot() +
      geom_line(data = ref_scens[co2e_tonnes > 0 & !is.na(ccs_adj_usd_per_mt)], 
                aes(x = year, y = ccs_adj_usd_per_mt, color = co2e_tonnes/1e6, group = refinery_name)) +
      geom_line(data = un_ref, aes(x = year, y = ccs_price_usd_per_kg * 1e3), size = 0.7, color = '#252525', linetype = 2) +
      facet_wrap(vars(ccs_scenario), labeller = labeller(ccs_scenario = c('high CCS cost' = 'High CCS Cost', 
                                                                          'medium CCS cost' = 'Central CCS Cost', 
                                                                          'low CCS cost' = 'Low CCS Cost'))) +
      labs(title = NULL,
           subtitle = NULL, 
           caption = 'Dashed black line is non-adjusted CCS price',
           x = NULL,
           y = 'CCS cost by refinery (USD per metric tonne)',
           color = expression(paste("Million tonnes of ", CO[2], "e emitted in 2018"))) +
           # color = 'Million tonnes of CO2e emitted in 2018') +
      scale_color_gradient(low = blue_intensity[2], high = blue_intensity[1]) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 250, 50), limits = c(0, 250), expand = c(0,0)) +
      theme_line +
      theme(legend.position = "top")
    # fig_ccs_refinery
    
    ggsave(fig_ccs_refinery,
           filename = file.path(fig_path, 'adjusted_ccs_prices_by_refinery.png'),
           width = 6.5,
           height = 4,
           dpi = 400, 
           units = "in", 
           device='png')
    
    # ggsave(fig_ccs_refinery,
    #        filename = file.path(save_path, 'adjusted_ccs_prices_by_refinery.pdf'),
    #        width = 12,
    #        height = 8)
    # 
    # embed_fonts(file.path(save_path, 'adjusted_ccs_prices_by_refinery.pdf'),
    #             outfile = file.path(save_path, 'adjusted_ccs_prices_by_refinery.pdf'))
