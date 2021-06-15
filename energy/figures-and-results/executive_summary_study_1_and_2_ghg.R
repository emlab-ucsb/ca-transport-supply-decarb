# executive summary figure combining emissions from studies 1 and 2
# created: february 26, 2021
# author: meas meng (@measrainsey)

# ------------------ inputs ------------------

  study1_path   = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw'
  study1_file   = 'Tailpipe emissions Only-v2.xlsx'
  study2_path   = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
  study2_file   = 'selected_scenarios_extraction_and_refining_ghg.csv'
  
# ------------------ outputs ------------------
  
  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
  
# libraries --------
  
  library(data.table)
  library(openxlsx)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
  # font_import(paths = '/Users/meas/Documents/fonts', recursive = T)
  # loadfonts()
  
# load study 2 data ------
  
  s2_data = fread(file.path(study2_path, study2_file), header = T)
  
# load study 1 data ------
  
  s1_bau = setDT(read.xlsx(file.path(study1_path, study1_file), sheet = 'Emissions', rows = c(1, 27), cols = 2:37))
  setnames(s1_bau, 'Fuel', 'scenario')
  setnames(s1_bau, 'Units', 'units')
  s1_bau[, scenario := 'T-BAU']
  
  s1_lc1 = setDT(read.xlsx(file.path(study1_path, study1_file), sheet = 'Emissions', rows = c(1, 51), cols = 2:37))
  setnames(s1_lc1, 'Fuel', 'scenario')
  setnames(s1_lc1, 'Units', 'units')
  s1_lc1[, scenario := 'LCT1']

# melt study 1 data -------
  
  s1_bau_long = melt(s1_bau, id.vars = c('scenario', 'units'), measure.vars = as.character(c(2017:2050)),
                     variable.name = 'year', value.name = 'ghg_mmt_co2e')
  s1_bau_long[, units := NULL]
  
  s1_lc1_long = melt(s1_lc1, id.vars = c('scenario', 'units'), measure.vars = as.character(c(2017:2050)),
                     variable.name = 'year', value.name = 'ghg_mmt_co2e')
  s1_lc1_long[, units := NULL]
  
# combine study 1 data ------
  
  s1_data = rbind(s1_bau_long, s1_lc1_long)
  setnames(s1_data, 'scenario', 'tailpipe_scenario')
  # s1_data[, segment := 'Tailpipe']
  s1_data[, year := as.integer(as.character(year))]
  setcolorder(s1_data, c('tailpipe_scenario', 'year', 'ghg_mmt_co2e'))
  setnames(s1_data, 'ghg_mmt_co2e', 'tailpipe_ghg_mmt')
  
# dcast study 2 data -------
  
  s2_data_wide = dcast(s2_data, refining_scenario + extraction_scenario + year ~ segment, value.var = 'ghg_mmt_co2e')
  setnames(s2_data_wide, 'Extraction', 'extraction_ghg_mmt')
  setnames(s2_data_wide, 'Refining', 'refining_ghg_mmt')
  
# combine tailpipe scenarios with extraction + refining scenarios ------
  
  s1_data[, k := 1]
  s2_data_wide[, k := 1]
  
  data_all = s1_data[s2_data_wide, on = .(k, year), allow.cartesian = T]
  data_all[, k := NULL]

# convert combined results to long format ------
  
  final_res = melt(data_all, 
                   id.vars = c('refining_scenario', 'extraction_scenario', 'tailpipe_scenario', 'year'),
                   measure.vars = c('refining_ghg_mmt', 'extraction_ghg_mmt', 'tailpipe_ghg_mmt'),
                   variable.name = 'segment',
                   value.name = 'ghg_mmt_co2e')
  final_res[, segment := gsub('_ghg_mmt', '', segment)]
  final_res[segment == 'refining', segment := 'Refining']
  final_res[segment == 'extraction', segment := 'Extraction']
  final_res[segment == 'tailpipe', segment := 'Tailpipe']
  
# reorder factor levels ------
  
  final_res[, segment := factor(segment, levels = rev(c('Extraction', 'Refining', 'Tailpipe')))]
  final_res[, refining_scenario := factor(refining_scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]
  final_res[, extraction_scenario := factor(extraction_scenario, levels = c('E-BAU', 'LCE1', 'LCE2'))]
  final_res[, tailpipe_scenario := factor(tailpipe_scenario, levels = c('T-BAU', 'LCT1'))]
  
  setorder(final_res, refining_scenario, extraction_scenario, tailpipe_scenario, year, segment)
  
# only plot selected scenarios -------
  
  final_sel = final_res[(refining_scenario == 'R-BAU' & extraction_scenario == 'E-BAU' & tailpipe_scenario == 'T-BAU') | 
                          (refining_scenario == 'LCR2' & extraction_scenario == 'LCE2' & tailpipe_scenario == 'LCT1')  ]
  
# rename scenarios -------
  
  final_sel[(refining_scenario == 'R-BAU' & extraction_scenario == 'E-BAU' & tailpipe_scenario == 'T-BAU'), scenario_name := 'Business As Usual']
  final_sel[(refining_scenario == 'LCR2' & extraction_scenario == 'LCE2' & tailpipe_scenario == 'LCT1'), scenario_name := 'Low Carbon Scenarios']
  
# ------------------------- plots ------------------------- 
  
  # plot theme & palettes ------
  
  pal_source = c('Extraction' = '#005581',
                 'Refining' = '#1295D8',
                 'Tailpipe' = '#FFB511')
  
  theme_line = theme_ipsum(base_family = 'Arial',
                           grid = 'Y', 
                           plot_title_size = 10, 
                           subtitle_size = 9,
                           axis_title_just = 'center',
                           axis_title_size = 9, 
                           axis_text_size = 9,
                           strip_text_size = 11)  +
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
  
  # plot: area charts of studies 1 and 2 ghg emissions ---------
  
  fig_comparison = ggplot(final_sel, aes(x = year, y = ghg_mmt_co2e, fill = segment)) +
    geom_area() +
    facet_wrap(tailpipe_scenario + refining_scenario ~ extraction_scenario, ncol = 2, 
               labeller = labeller(refining_scenario = c('R-BAU' = 'Refining = R-BAU',
                                                         'LCR1' = 'Refining = LCR1',
                                                         'LCR2' = 'Refining = LCR2'),
                                   extraction_scenario = c('E-BAU' = 'Extraction = E-BAU',
                                                           'LCE1' = 'Extraction = LCE1',
                                                           'LCE2' = 'Extraction = LCE2'),
                                   tailpipe_scenario = c('T-BAU' = 'Tailpipe = BAU',
                                                         'LCT1' = 'Tailpipe = LC1'))) +
    labs(title = NULL,
         subtitle = NULL, 
         x = NULL,
         y = 'CO2e emitted (million metric tons)',
         fill = NULL) +
    scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0, 200, 25), limits = c(0, 200), expand = c(0,0)) +
    scale_fill_manual(values = pal_source) +
    theme_line
  fig_comparison
  
  ggsave(fig_comparison,
         filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg.png'),
         width = 6.5,
         height = 4.5,
         dpi = 400, 
         units = 'in', 
         device = 'png')
  
  ggsave(fig_comparison,
         filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg.pdf'),
         width = 6.5,
         height = 4.5,
         units = 'in', 
         device = 'pdf')
  
  embed_fonts(file.path(fig_path, 'executive_summary_study_1_and_2_ghg.pdf'),
              outfile = file.path(fig_path, 'executive_summary_study_1_and_2_ghg.pdf'), 
              options="-sFONTPATH=/System/Library/Fonts/Supplemental/Arial.ttf")
  
  
  
  # plot: labeled version (no legend) ---------
  
    plot_labels = unique(final_sel[year == 2020, .(refining_scenario, extraction_scenario, tailpipe_scenario, year, segment, ghg_mmt_co2e)])
    
    plot_labels[, label := fcase(segment == 'Refining', as.character(refining_scenario),
                                 segment == 'Extraction', as.character(extraction_scenario),
                                 segment == 'Tailpipe', as.character(tailpipe_scenario))]
    plot_labels[label == 'T-BAU', label := 'BAU']
    plot_labels[label == 'LCT1', label := 'LC1']
    
    plot_labels[1, ghg_mmt_co2e := 75]
    # plot_labels[1, year := 2022]
    
    plot_labels[2, ghg_mmt_co2e := 23]
    # plot_labels[2, year := 2022]
    
    plot_labels[3, ghg_mmt_co2e := 5.1]
    # plot_labels[3, year := 2022]
    
    plot_labels[4, ghg_mmt_co2e := 75]
    # plot_labels[4, year := 2022]
    
    plot_labels[5, ghg_mmt_co2e := 23]
    # plot_labels[5, year := 2022]
    
    plot_labels[6, ghg_mmt_co2e := 5.1]
    # plot_labels[6, year := 2022]
    
    # final_sel[, seg_col := fifelse(segment == 'Tailpipe', 'black', 'white')]
    
    fig_label = ggplot(final_sel, aes(x = year, y = ghg_mmt_co2e, fill = segment)) +
      geom_area() +
      facet_wrap(tailpipe_scenario + refining_scenario ~ extraction_scenario, ncol = 2, 
                 labeller = labeller(refining_scenario = c('R-BAU' = 'Refining = R-BAU',
                                                           'LCR1' = 'Refining = LCR1',
                                                           'LCR2' = 'Refining = LCR2'),
                                     extraction_scenario = c('E-BAU' = 'Extraction = E-BAU',
                                                             'LCE1' = 'Extraction = LCE1',
                                                             'LCE2' = 'Extraction = LCE2'),
                                     tailpipe_scenario = c('T-BAU' = 'Tailpipe = BAU',
                                                           'LCT1' = 'Tailpipe = LC1'))) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'CO2e emitted (million metric tons)',
           fill = NULL) +
      geom_text(data = plot_labels, aes(label = label), family = 'Arial', color = 'white', size = 3.6, hjust = 0, fontface = 'bold') +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 200, 25), limits = c(0, 200), expand = c(0,0)) +
      scale_fill_manual(values = pal_source) +
      theme_line +
      theme(strip.background = element_blank(),
            strip.text = element_blank())
    
    # fig_label
    
    ggsave(fig_label,
           filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_label,
           filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled.pdf'),
                outfile = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled.pdf'))
  
  # plot: labeled version v2 (no legend) ---------
  
    plot_labels = unique(final_sel[year == 2020, .(refining_scenario, extraction_scenario, tailpipe_scenario, year, segment, ghg_mmt_co2e)])
    
    plot_labels[, label := fcase(segment == 'Refining', as.character(refining_scenario),
                                 segment == 'Extraction', as.character(extraction_scenario),
                                 segment == 'Tailpipe', as.character(tailpipe_scenario))]
    plot_labels[label == 'T-BAU', label := 'BAU']
    plot_labels[label == 'LCT1', label := 'LC1']
    
    plot_labels[1, ghg_mmt_co2e := 75]
    # plot_labels[1, year := 2022]
    
    plot_labels[2, ghg_mmt_co2e := 23]
    # plot_labels[2, year := 2022]
    
    plot_labels[3, ghg_mmt_co2e := 5.1]
    # plot_labels[3, year := 2022]
    
    plot_labels[4, ghg_mmt_co2e := 75]
    # plot_labels[4, year := 2022]
    
    plot_labels[5, ghg_mmt_co2e := 23]
    # plot_labels[5, year := 2022]
    
    plot_labels[6, ghg_mmt_co2e := 5.1]
    # plot_labels[6, year := 2022]
    
    # final_sel[, seg_col := fifelse(segment == 'Tailpipe', 'black', 'white')]

    fig_label = ggplot(final_sel, aes(x = year, y = ghg_mmt_co2e, fill = segment)) +
      geom_area() +
      facet_wrap(. ~ scenario_name, ncol = 2) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'CO2e emitted (million metric tons)',
           fill = NULL) +
      guides(fill = FALSE) +
      geom_text(data = plot_labels, aes(label = segment), family = 'Arial', color = 'white', size = 3.6, hjust = 0, fontface = 'bold') +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 200, 25), limits = c(0, 200), expand = c(0,0)) +
      scale_fill_manual(values = pal_source) +
      theme_line +
      theme(strip.background = element_blank(),
            strip.text = element_text(face = 'bold'))
    # fig_label
    
    ggsave(fig_label,
           filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_v2.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_label,
           filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_v2.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_v2.pdf'),
                outfile = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_v2.pdf'))
    

  # plot: labeled version (no legend) & borders ---------
    
    fig_borders = ggplot(final_sel, aes(x = year, y = ghg_mmt_co2e, fill = segment)) +
      geom_area() +
      geom_line(aes(color = segment), position = 'stack', lwd = 0.3) +
      facet_wrap(tailpipe_scenario + refining_scenario ~ extraction_scenario, ncol = 2, 
                 labeller = labeller(refining_scenario = c('R-BAU' = 'Refining = R-BAU',
                                                           'LCR1' = 'Refining = LCR1',
                                                           'LCR2' = 'Refining = LCR2'),
                                     extraction_scenario = c('E-BAU' = 'Extraction = E-BAU',
                                                             'LCE1' = 'Extraction = LCE1',
                                                             'LCE2' = 'Extraction = LCE2'),
                                     tailpipe_scenario = c('T-BAU' = 'Tailpipe = BAU',
                                                           'LCT1' = 'Tailpipe = LC1'))) +
      labs(title = NULL,
           subtitle = NULL, 
           x = NULL,
           y = 'CO2e emitted (million metric tons)',
           fill = NULL) +
      guides(color = FALSE) + 
      geom_text(data = plot_labels, aes(label = label), family = 'Arial', color = 'white', size = 3.6, hjust = 0, fontface = 'bold') +
      scale_x_continuous(breaks = c(2019, seq(2025, 2045, 5)), limits = c(2019, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 200, 25), limits = c(0, 200), expand = c(0,0)) +
      scale_fill_manual(values = pal_source) +
      scale_color_manual(values = c('Tailpipe' = 'black',
                                    'Refining' = 'white',
                                    'Extraction' = 'white')) +
      theme_line +
      theme(strip.background = element_blank(),
            strip.text = element_blank())
    
    # fig_borders
    
    ggsave(fig_borders,
           filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_borders.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    ggsave(fig_borders,
           filename = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_borders.pdf'),
           width = 6.5,
           height = 4.5,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_borders.pdf'),
                outfile = file.path(fig_path, 'executive_summary_study_1_and_2_ghg_labeled_borders.pdf'))
    
    