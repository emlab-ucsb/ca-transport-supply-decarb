# extraction ghg emissions plots for interim report
# created: november 23, 2020
# author: meas meng

# ------------------ inputs ------------------
  
  res_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_20_all_scens'
  res_file     = 'summary_outputs_2020-2045.csv'
  
# ------------------ outputs ------------------
  
  fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model'
  
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

# load results --------
  
  dt_res = as.data.table(read_feather(file.path(res_path, res_file)))
  
# only keep no tax excise results ------
  
  dt_res = dt_res[excise_tax_scenario == 'no tax']
  
# keep BAU macro conditions ------
  
  dt_res = dt_res[oil_price_scenario == 'iea oil price' & 
                    innovation_scenario == 'low innovation' & 
                    carbon_price_scenario == 'price floor' & 
                    ccs_scenario == 'medium CCS cost']
  
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
    
  # plot: bau ghg emissions -------- 
    
    fig_ghg_ext_bau = ggplot(dt_res[setback_scenario == 'no_setback' & prod_quota_scenario == 'no quota'], aes(x = year, y = ghg_mmt_inno_ccs_adj)) +
      geom_area(fill = ucsb_pal_prim2[1]) +
      labs(title = 'Extraction BAU scenario',
           subtitle = 'Million metric tonnes of CO2e', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 15, 1), limits = c(0, 11), expand = c(0,0)) +
      theme_line
    fig_ghg_ext_bau
    
    ggsave(fig_ghg_ext_bau,
           filename = file.path(fig_path, 'ghg_extraction_bau.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
  # plot: e1 ghg emissions -------- 
    
    fig_ghg_ext_e1 = ggplot(dt_res[setback_scenario == 'no_setback' & prod_quota_scenario == 'quota_20'], aes(x = year, y = ghg_mmt_inno_ccs_adj)) +
      geom_area(fill = ucsb_pal_prim2[1]) +
      labs(title = 'Extraction E1 (80% reduction quota + no setbacks) scenario',
           subtitle = 'Million metric tonnes of CO2e', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 15, 1), limits = c(0, 11), expand = c(0,0)) +
      theme_line
    fig_ghg_ext_e1
    
    ggsave(fig_ghg_ext_e1,
           filename = file.path(fig_path, 'ghg_extraction_e1.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
  # plot: e2 ghg emissions -------- 
    
    fig_ghg_ext_e2 = ggplot(dt_res[setback_scenario == 'setback_2500ft' & prod_quota_scenario == 'quota_20'], aes(x = year, y = ghg_mmt_inno_ccs_adj)) +
      geom_area(fill = ucsb_pal_prim2[1]) +
      labs(title = 'Extraction E2 (80% reduction quota + 2,500 feet setbacks) scenario',
           subtitle = 'Million metric tonnes of CO2e', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(2020, 2045, 5), limits = c(2020, 2045), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, 15, 1), limits = c(0, 11), expand = c(0,0)) +
      theme_line
    fig_ghg_ext_e2
    
    ggsave(fig_ghg_ext_e2,
           filename = file.path(fig_path, 'ghg_extraction_e2.png'),
           width = 6.5,
           height = 4.5,
           dpi = 400, 
           units = 'in', 
           device = 'png')
