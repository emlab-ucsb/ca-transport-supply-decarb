# executive summary figure of extraction and refining ghg emissions
# created: december 28, 2020
# author: meas meng

# ------------------ inputs ------------------
  
  ext_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios'
  ref_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  base_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/baseline-2019'
  ext_file        = 'extraction_prod_ghg_outputs.csv'
  ref_file        = 'refining_scenario_outputs_state_net_exports.csv'
  base_file       = 'refining_emissions_state_2019.csv'

# ------------------ outputs ------------------
  
  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
  
# libraries --------
  
  library(data.table)
  library(feather)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(cowplot)
  
# source items ------
  
  # source ccs emissions mean b calculation script
    items = list.files(here::here('src'))
    sapply(here::here('src', items), source)

# load data --------
    
  res_ext = fread(file.path(ext_path, ext_file), header = T)
  res_ref = fread(file.path(ref_path, ref_file), header = T)
  base_ref = fread(file.path(ref_path, base_file), header = T)

# replace extraction scenario names -------
  
  res_ext[, scenario := gsub('BAU', 'E-BAU', scenario)]
  res_ext[, scenario := gsub('E1', 'LCE1', scenario)]
  res_ext[, scenario := gsub('E2', 'LCE2', scenario)]
  
# keep BAU macro conditions for refining results ------
  
  res_ref = res_ref[innovation_scenario == 'low innovation' & 
                      carbon_price_scenario == 'price floor' & 
                      ccs_scenario == 'medium CCS cost']
  
# get selected refining scenarios --------
  
  res_ref[demand_scenario == 'BAU' & refining_scenario == 'historic exports', scenario := 'R-BAU']
  res_ref[demand_scenario == 'LC1' & refining_scenario == 'historic exports', scenario := 'LCR1']
  res_ref[demand_scenario == 'LC1' & refining_scenario == 'low exports', scenario := 'LCR2']
  
  res_ref = res_ref[!is.na(scenario) & fuel == 'crude' & source == 'total' & boundary == 'complete' & type == 'ghg']
  
# get ghg in 2019 ------
  
  ghg_2019 = base_ref[boundary == 'complete']
  ghg_2019[, j := 1]
  
# create unique scenarios for 2019 -----
  
  un_scens = data.table(scenario = c('R-BAU', 'LCR1', 'LCR2'), j = 1)
  ghg_2019 = ghg_2019[un_scens, on = .(j)]
  ghg_2019[, j := NULL]
  
# combine refining ghg results with baseline 2019 refining ghg ------
  
  res_ref_2 = res_ref[, .(scenario, year, fuel, source, boundary, type, units, value)]
  ghg_2019 = ghg_2019[, .(scenario, year, fuel, source, boundary, type, units, value)]
  
  res_ref_complete = rbindlist(list(res_ref_2, ghg_2019), use.names = T, fill = T)
  res_ref_complete[, ghg_mmt_co2e := value/1e9]
  
# join extraction and refining scenarios -------
  
  ref_all = res_ref_complete[, .(scenario, year, ghg_mmt_co2e)]
  ref_all[, segment := 'Refining']
  
  ext_all = res_ext[, .(scenario, year, ghg_mmt_co2e)]
  ext_all[, segment := 'Extraction']

  final_res = rbindlist(list(ext_all, ref_all), use.names = T)
  
# rename scenario for plot -------
  
  final_res[scenario == 'E-BAU', scenario_name := 'E-BAU - Extraction Business-as-Usual']
  final_res[scenario == 'LCE1', scenario_name := 'LCE1 - Production Quota']
  final_res[scenario == 'LCE2', scenario_name := 'LCE2 - Production Quota + Setbacks']
  
  final_res[scenario == 'R-BAU', scenario_name := 'R-BAU - Refining Business-as-Usual']
  final_res[scenario == 'LCR1', scenario_name := 'LCR1 - Low Carbon Demand + Historical Exports']
  final_res[scenario == 'LCR2', scenario_name := 'LCR2 - Low Carbon Demand + Low Exports']
  
# reorder factor levels ------
  
  final_res[, segment := factor(segment, levels = rev(c('Extraction', 'Refining')))]
  final_res[, scenario := factor(scenario, levels = c('E-BAU', 'LCE1', 'LCE2', 'R-BAU', 'LCR1', 'LCR2'))]
  final_res[, scenario_name := factor(scenario_name, levels = c('E-BAU - Extraction Business-as-Usual', 
                                                           'LCE1 - Production Quota', 
                                                           'LCE2 - Production Quota + Setbacks', 
                                                           'R-BAU - Refining Business-as-Usual', 
                                                           'LCR1 - Low Carbon Demand + Historical Exports', 
                                                           'LCR2 - Low Carbon Demand + Low Exports'))]
  
  setorder(final_res, scenario, year, segment)
  
# save to csv ------
  
  fwrite(final_res, file.path(fig_path, 'exec_summary_extraction_and_refining_ghg.csv'), row.names = F)
  
# ------------------------- plots ------------------------- 
  
  # plot theme & palettes ------
  
    pal_scenarios = c('E-BAU' = '#FFD200',
                      'R-BAU' = '#FFD200',
                      'LCE1' = '#72CDF4',
                      'LCR1' = '#72CDF4',
                      'LCE2' = '#005581',
                      'LCR2' = '#005581',
                      'E-BAU - Extraction Business-as-Usual' = '#FFD200',
                      'R-BAU - Refining Business-as-Usual' = '#FFD200',
                      'LCE1 - Production Quota' = '#72CDF4',
                      'LCR1 - Low Carbon Demand + Historical Exports' = '#72CDF4',
                      'LCE2 - Production Quota + Setbacks' = '#005581',
                      'LCR2 - Low Carbon Demand + Low Exports' = '#005581')
  
    theme_line = theme_ipsum(base_family = 'Arial',
                             grid = 'Y', 
                             plot_title_size = 9, 
                             subtitle_size = 9,
                             axis_title_just = 'center',
                             axis_title_size = 9, 
                             axis_text_size = 9,
                             strip_text_size = 9)  +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold', margin = margin(t = 0, b = 0.1, unit = 'cm')),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.2, 'cm'),
            axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
            axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
            legend.title = element_text(size = 8, vjust = 0.5),
            legend.text = element_text(size = 7.2, vjust = 0.5),
            legend.position = 'top',
            legend.spacing = unit(0, 'cm'),
            legend.key.size = unit(0.38, "cm"),
            legend.key.width = unit(0.3, 'cm'),   
            legend.background = element_rect(color = 'white'),
            legend.box.margin = margin(0, 0, 0, 0, "cm"), 
            legend.box.spacing = unit(0, "cm"),
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
  
  # plot: extraction ghg ------------
    
    fig_extraction = ggplot(final_res[segment == 'Extraction'], aes(x = year, y = ghg_mmt_co2e, color = scenario_name)) +
      geom_line(size = 0.7) +
      labs(title = 'Extraction',
           subtitle = NULL, 
           x = NULL,
           y = 'Annual CO2e emissions (million metric tons)',
           color = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios, guide = guide_legend(nrow = 3)) +
      theme_line +
      theme(legend.position = c(0.46, 0.82))
    # fig_extraction
    
  # plot: refining ghg ------------
    
    fig_refining = ggplot(final_res[segment == 'Refining'], aes(x = year, y = ghg_mmt_co2e, color = scenario_name)) +
      geom_line(size = 0.7) +
      labs(title = 'Refining',
           subtitle = NULL, 
           x = NULL,
           y = 'Annual CO2e emissions (million metric tons)',
           color = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30), expand = c(0,0)) +
      scale_color_manual(values = pal_scenarios, guide = guide_legend(nrow = 3)) +
      theme_line + 
      theme(legend.position = c(0.54, 0.18))
    # fig_refining
    
    
  # combine plots ---------
    
    fig_combine = plot_grid(fig_extraction, fig_refining, 
                            nrow = 1, align = "h", rel_widths = c(0.5, 0.5))
    # fig_combine
    
    ggsave(fig_combine,
           filename = file.path(fig_path, 'exec_summary_extraction_and_refining_ghg.png'),
           width = 6.5,
           height = 3.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_combine,
           filename = file.path(fig_path, 'exec_summary_extraction_and_refining_ghg.pdf'),
           width = 6.5,
           height = 3.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'exec_summary_extraction_and_refining_ghg.pdf'),
                outfile = file.path(fig_path, 'exec_summary_extraction_and_refining_ghg.pdf'))
    
    