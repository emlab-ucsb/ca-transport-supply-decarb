# comparison of ghg emissions from extraction and refining modules under all combinations of selected scenarios
# created: november 30, 2020
# author: meas meng

# ------------------ inputs ------------------
  
  ext_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios'
  ref_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  base_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/baseline-2019'
  ext_file        = 'extraction_prod_ghg_outputs_revised.csv'
  ref_file        = 'refining_scenario_outputs_state_net_exports_revised.csv'
  base_file       = 'refining_emissions_state_2019_revised.csv'

# ------------------ outputs ------------------
  
  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
  
# libraries --------
  
  library(data.table)
  library(feather)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
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
  setnames(ref_all, 'scenario', 'refining_scenario')
  setnames(ref_all, 'ghg_mmt_co2e', 'refining_ghg_mmt')
  ref_all[, j := 1]
  
  ext_all = res_ext[, .(scenario, year, ghg_mmt_co2e)]
  setnames(ext_all, 'scenario', 'extraction_scenario')
  setnames(ext_all, 'ghg_mmt_co2e', 'extraction_ghg_mmt')
  ext_all[, j := 1]
  
  res_all = ref_all[ext_all, on = .(j, year), allow.cartesian = T]
  res_all[, j := NULL]
  setcolorder(res_all, c('refining_scenario', 'extraction_scenario', 'year', 'refining_ghg_mmt', 'extraction_ghg_mmt'))
  
# convert combined results to long format ------
  
  final_res = melt(res_all, 
                   id.vars = c('refining_scenario', 'extraction_scenario', 'year'),
                   measure.vars = c('refining_ghg_mmt', 'extraction_ghg_mmt'),
                   variable.name = 'segment',
                   value.name = 'ghg_mmt_co2e')
  final_res[, segment := gsub('_ghg_mmt', '', segment)]
  final_res[segment == 'refining', segment := 'Refining']
  final_res[segment == 'extraction', segment := 'Extraction']

# reorder factor levels ------
  
  final_res[, segment := factor(segment, levels = rev(c('Extraction', 'Refining')))]
  final_res[, refining_scenario := factor(refining_scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]
  final_res[, extraction_scenario := factor(extraction_scenario, levels = c('E-BAU', 'LCE1', 'LCE2'))]
  
  setorder(final_res, refining_scenario, extraction_scenario, year, segment)
  
# save to CSV -------
  
  # fwrite(final_res, file.path(fig_path, 'selected_scenarios_extraction_and_refining_ghg.csv'), row.names = F)

# ------------------------- plots ------------------------- 
  
  # plot theme & palettes ------
  
    pal_source = c('Extraction' = '#1295D8',
                   'Refining' = '#FFB511')
  
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
            legend.margin = margin(t = 0, b = 0, unit = 'cm'),
            legend.position = 'top',
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
    
  # plot: area charts of crude production, crude imports, and renewable feedstock ---------
    
    fig_comparison = ggplot(final_res, aes(x = year, y = ghg_mmt_co2e, fill = segment)) +
      geom_area() +
      facet_wrap(refining_scenario ~ extraction_scenario, ncol = 3, 
                 labeller = labeller(refining_scenario = c('R-BAU' = 'Refining scenario = R-BAU',
                                                           'LCR1' = 'Refining scenario = LCR1',
                                                           'LCR2' = 'Refining scenario = LCR2'),
                                     extraction_scenario = c('E-BAU' = 'Extraction scenario = E-BAU',
                                                             'LCE1' = 'Extraction scenario = LCE1',
                                                             'LCE2' = 'Extraction scenario = LCE2'))) +
    labs(title = NULL,
         subtitle = NULL, 
         x = NULL,
         y = 'CO2e emitted (million metric tons)',
         fill = NULL) +
    scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0, 50, 5), limits = c(0, 50), expand = c(0,0)) +
    scale_fill_manual(values = pal_source) +
    theme_line
  fig_comparison
  
  ggsave(fig_comparison,
         filename = file.path(fig_path, 'selected_scenarios_extraction_and_refining_ghg_revised.png'),
         width = 6.5,
         height = 8,
         dpi = 400, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_comparison,
         filename = file.path(fig_path, 'selected_scenarios_extraction_and_refining_ghg_revised.pdf'),
         width = 6.5,
         height = 8,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(fig_path, 'selected_scenarios_extraction_and_refining_ghg_revised.pdf'),
              outfile = file.path(fig_path, 'selected_scenarios_extraction_and_refining_ghg_revised.pdf'))
  