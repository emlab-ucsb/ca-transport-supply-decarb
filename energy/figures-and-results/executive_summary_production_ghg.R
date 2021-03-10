# figure of production and GHG under BAU and most stringent scenarios 
# created: december 22, 2020
# author: meas meng

# ------------------ outputs ------------------

  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'

# libraries --------

  library(data.table)
  library(feather)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(cowplot)

#  --------------------- PART 1: load crude production/consumption results -----------------------

# ------------------ inputs ------------------

  ext_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios'
  ref_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  base_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/baseline-2019'
  ext_file        = 'extraction_prod_ghg_outputs.csv'
  ref_file        = 'refining_scenario_outputs_state_net_exports.csv'
  base_file       = 'baseline_region_level_crude_equiv.csv'

# source items ------

  # source ccs emissions mean b calculation script
  items = list.files(here::here('src'))
  sapply(here::here('src', items), source)

# load data --------

  res_ext = fread(file.path(ext_path, ext_file), header = T)
  res_ref = fread(file.path(ref_path, ref_file), header = T)
  base_ref = fread(file.path(base_path, base_file), header = T)

# replace extraction scenario names -------

  res_ext[, scenario := gsub('BAU', 'E-BAU', scenario)]
  res_ext[, scenario := gsub('E1', 'LCE1', scenario)]
  res_ext[, scenario := gsub('E2', 'LCE2', scenario)]

# only keep crude in results -------

  res_ref = res_ref[fuel == 'crude' & type == 'consumption' & source != 'total']
  res_ref = unique(res_ref[, .(demand_scenario, refining_scenario, year, fuel, source, boundary, type, units, value)])

# get selected scenarios --------

  res_ref[demand_scenario == 'BAU' & refining_scenario == 'historic exports', scenario := 'R-BAU']
  res_ref[demand_scenario == 'LC1' & refining_scenario == 'historic exports', scenario := 'LCR1']
  res_ref[demand_scenario == 'LC1' & refining_scenario == 'low exports', scenario := 'LCR2']
  
  res_ref = res_ref[!is.na(scenario)]

# rename source -------

  res_ref[source %in% c('main renewable', 'residual renewable'), source := 'renewable feedstock']
  res_ref[source == 'traditional', source := 'crude consumption']
  
  res_ref = res_ref[, .(value = sum(value)), by = .(scenario, demand_scenario, refining_scenario, year, fuel, source, type, units)]

# aggregate 2019 crude ------

  base_ref[, fuel := 'crude']
  base_ref[category == 'Renewable Diesel', source := 'renewable feedstock']
  base_ref[!category == 'Renewable Diesel', source := 'crude consumption']
  base_ref[, boundary := 'complete']
  base_ref[, type := 'consumption']
  base_ref[, units := 'bbl']
  base_ref[, value := crude_equiv_bbls]
  
  base_ref = base_ref[, .(value = sum(value)), by = .(year, fuel, source, type, units)]
  base_ref[, j := 1]

# create unique scenarios for 2019 -----

  un_scens = data.table(scenario = c('R-BAU', 'LCR1', 'LCR2'), j = 1)
  base_ref = base_ref[un_scens, on = .(j), allow.cartesian = T]
  base_ref[, j := NULL]
  
  base_ref = base_ref[, .(scenario, year, fuel, source, type, units, value)]

# combine 2019 and projected refining results -------

  ref_all = rbindlist(list(base_ref, res_ref), use.names = T, fill = T)

# join extraction and refining scenarios -------

  ref_all = ref_all[, .(scenario, year, fuel, source, value)]
  setnames(ref_all, 'scenario', 'refining_scenario')
  setnames(ref_all, 'value', 'consumption_bbl')
  ref_all[, j := 1]
  
  ext_all = res_ext[, .(scenario, year, production_bbl)]
  setnames(ext_all, 'scenario', 'extraction_scenario')
  ext_all[, j := 1]
  
  res_all = ref_all[ext_all, on = .(j, year), allow.cartesian = T]
  res_all[, j := NULL]
  setcolorder(res_all, c('refining_scenario', 'extraction_scenario', 'year', 'fuel', 'source', 'consumption_bbl', 'production_bbl'))

# calculate imports ------

  res_exports = res_all[source == 'crude consumption']
  res_exports[, imports_bbl := consumption_bbl - production_bbl]
  
  res_exports_long = melt(res_exports, 
                          id.vars = c('refining_scenario', 'extraction_scenario', 'year'),
                          measure.vars = c('consumption_bbl', 'production_bbl', 'imports_bbl'),
                          variable.name = 'source',
                          value.name = 'value')
  res_exports_long[, source := gsub('_bbl', '', source)]

# get renewable feedstocks -----

  res_renewable = res_all[source == 'renewable feedstock']
  res_renewable[, value := consumption_bbl]
  res_renewable[, consumption_bbl := NULL]
  res_renewable[, production_bbl := NULL]
  res_renewable[, fuel := NULL]

# combine crude production, imports, and renewable feedstocks ------

  final_prod_res = rbindlist(list(res_exports_long, res_renewable), use.names = T, fill = T)

# rename sources ------

  final_prod_res[source == 'imports', source := 'Crude imports']
  final_prod_res[source == 'production', source := 'In-state crude production']
  final_prod_res[source == 'renewable feedstock', source := 'Renewable feedstock']

# reorder factor levels ------

  final_prod_res[, source := factor(source, levels = rev(c('In-state crude production', 'Crude imports', 'Renewable feedstock', 'consumption')))]
  final_prod_res[, refining_scenario := factor(refining_scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]
  final_prod_res[, extraction_scenario := factor(extraction_scenario, levels = c('E-BAU', 'LCE1', 'LCE2'))]
  
# remove 'consumption' source ------

  final_prod_res = final_prod_res[! source == 'consumption']

# add units ------

  final_prod_res[, units := 'bbl']
  
# remove all objects except production results ------
  
  rm(list = ls()[!ls() %in% c('final_prod_res', 'fig_path')])
  
#  --------------------- PART 2: load  ghg results -----------------------
  
# ------------------ inputs ------------------
  
  ext_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios'
  ref_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/refinery-outputs'
  base_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/baseline-2019'
  ext_file        = 'extraction_prod_ghg_outputs.csv'
  ref_file        = 'refining_scenario_outputs_state_net_exports.csv'
  base_file       = 'refining_emissions_state_2019.csv'
  
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
  
  final_ghg_res = melt(res_all, 
                   id.vars = c('refining_scenario', 'extraction_scenario', 'year'),
                   measure.vars = c('refining_ghg_mmt', 'extraction_ghg_mmt'),
                   variable.name = 'segment',
                   value.name = 'ghg_mmt_co2e')
  final_ghg_res[, segment := gsub('_ghg_mmt', '', segment)]
  final_ghg_res[segment == 'refining', segment := 'Refining']
  final_ghg_res[segment == 'extraction', segment := 'Extraction']
  
# reorder factor levels ------
  
  final_ghg_res[, segment := factor(segment, levels = rev(c('Extraction', 'Refining')))]
  final_ghg_res[, refining_scenario := factor(refining_scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]
  final_ghg_res[, extraction_scenario := factor(extraction_scenario, levels = c('E-BAU', 'LCE1', 'LCE2'))]
  
  setorder(final_ghg_res, refining_scenario, extraction_scenario, year, segment)
  
# remove all objects except final results ------
  
  rm(list = ls()[!ls() %in% c('final_prod_res', 'final_ghg_res', 'fig_path')])
  
# ------------------------- part 3: plots ------------------------- 
  
  # plot theme & palettes ------
  
    pal_source = c('Crude imports' = '#72CDF4',
                   'In-state crude production' = '#005581',
                   'Renewable feedstock' = '#FFB511',
                   'Extraction' = '#1295D8',
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
  
    fig_crude = ggplot(final_prod_res[(!source == 'consumption') & 
                                        ((extraction_scenario == 'E-BAU' & refining_scenario == 'R-BAU') | (extraction_scenario == 'LCE2' & refining_scenario == 'LCR2'))], 
                       aes(x = year, y = value/1e6, fill = source)) +
      geom_area() +
      facet_wrap(refining_scenario ~ extraction_scenario, ncol = 2, 
                 labeller = labeller(refining_scenario = c('R-BAU' = 'Refining scenario = R-BAU',
                                                           'LCR1' = 'Refining scenario = LCR1',
                                                           'LCR2' = 'Refining scenario = LCR2'),
                                     extraction_scenario = c('E-BAU' = 'Extraction scenario = E-BAU',
                                                             'LCE1' = 'Extraction scenario = LCE1',
                                                             'LCE2' = 'Extraction scenario = LCE2'))) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'Crude equivalent (million barrels)',
           fill = NULL) +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 600, 100), limits = c(0, 600), expand = c(0,0)) +
      scale_fill_manual(values = pal_source) +
      theme_line
    fig_crude
    
  # plot: area charts of ghg ---------
    
    fig_ghg = ggplot(final_ghg_res[((extraction_scenario == 'E-BAU' & refining_scenario == 'R-BAU') | (extraction_scenario == 'LCE2' & refining_scenario == 'LCR2'))],
                     aes(x = year, y = ghg_mmt_co2e, fill = segment)) +
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
      scale_y_continuous(breaks = seq(0, 45, 5), limits = c(0, 45), expand = c(0,0)) +
      scale_fill_manual(values = pal_source) +
      theme_line
    fig_ghg
    
    
  # combine plots ------
    
    fig_combine = plot_grid(fig_crude, fig_ghg, 
                            nrow = 2, align = "v", rel_heights = c(0.5, 0.5))
  
    fig_combine

    ggsave(fig_combine,
           filename = file.path(fig_path, 'exec_summary_production_and_ghg.png'),
           width = 6.5,
           height = 7,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_combine,
           filename = file.path(fig_path, 'exec_summary_production_and_ghg.pdf'),
           width = 6.5,
           height = 7,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'exec_summary_production_and_ghg.pdf'),
                outfile = file.path(fig_path, 'exec_summary_production_and_ghg.pdf'))
    