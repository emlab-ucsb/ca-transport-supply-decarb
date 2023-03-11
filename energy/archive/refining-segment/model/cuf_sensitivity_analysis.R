# create diagnostic figures of refinery module under different CUFs
# created: august 12, 2021
# author: @measrainsey

# inputs ------------

  proj_path       = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
  res_path        = 'outputs/predict-production/refining_2021-08-12'
  ei_crude        = 5.698               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_3.pdf
  ei_gasoline     = 5.052               # mmbtu/bbl; source: https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_4.pdf
  
  
# outputs -----------
  
  
# libraries --------
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  
# sequence of CUF values ------
  
  l_cuf = seq(0.2, 0.8, 0.1)
  
# load refinery count and capacity data ---------
  
  l_crude_ref = list()
  l_ren_ref = list()
  
  for (i in seq_along(l_cuf)) {
    
    c = l_cuf[i]
    fname = paste0('CUF', c)
    
    crude_dt = fread(file.path(proj_path, res_path, fname, 'crude_refinery_capacity_and_count.csv'))
    ren_dt = fread(file.path(proj_path, res_path, fname, 'renewable_refinery_capacity_and_count.csv'))
    
    crude_dt[, cuf_scenario := c]
    ren_dt[, cuf_scenario := c]
    
    l_crude_ref[[i]] = crude_dt
    l_ren_ref[[i]] = ren_dt
    
    rm(c, fname, crude_dt, ren_dt)
    
  }
  
  dt_crude_ref = rbindlist(l_crude_ref, fill = TRUE)
  dt_ren_ref = rbindlist(l_ren_ref, fill = TRUE)

# get aggregate capacity by scenario ------
  
  agg_crude_cap = dt_crude_ref[, .(crude_barrels_per_day = sum(barrels_per_day, na.rm = T),
                                   no_crude_refineries = uniqueN(site_id)), 
                               by = .(demand_scenario, refining_scenario, cuf_scenario, year)]
  
  # agg_crude_cap[, cuf_scenario := as.character(cuf_scenario)]
  
  
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

  # fig: crude capacity ------
    
    fig_crude_capacity = ggplot(agg_crude_cap, aes(year, (crude_barrels_per_day*365*(ei_crude/ei_gasoline))/1e6, color = cuf_scenario, group = cuf_scenario)) +
      geom_line() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Annual capacity (million barrels of gasoline equivalent per year)',
           x = NULL,
           y = NULL,
           color = 'CUF',
           linetype = NULL) +
      scale_color_gradient(low = '#eff3ff', high = '#084594') +
      theme_line +
      theme(legend.key.width = unit(1.5, 'cm'))
  
    ggsave(fig_crude_capacity, 
           filename = file.path(proj_path, res_path, 'compare_cuf_crude_refinery_capacity.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(proj_path, res_path, 'compare_cuf_crude_refinery_capacity.pdf'),
                outfile = file.path(proj_path, res_path, 'compare_cuf_crude_refinery_capacity.pdf'))
    
    ggsave(fig_crude_capacity,
           filename = file.path(proj_path, res_path, 'compare_cuf_crude_refinery_capacity.png'),
           width = 14,
           height = 12,
           dpi = 400, 
           units = 'in', 
           device = 'png')
  
    
  # fig: crude refinery count ------
    
    
    fig_refinery_count = ggplot(agg_crude_cap, aes(x = year, y = no_crude_refineries, group = cuf_scenario, color = cuf_scenario)) +
      geom_line() +
      facet_wrap(demand_scenario ~ refining_scenario, 
                 labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                           'historic production' = 'Historic Production',
                                                           'low exports' = 'Low Exports'))) +
      labs(title = 'Number of refineries in each year',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(2020,2040,5), limits = c(2020, 2045), expand = c(0,0)) +
      # scale_y_continuous(expand = c(0,0), breaks = seq(0, 10, 1), limits = c(0, 8)) + 
      scale_color_gradient(low = '#eff3ff', high = '#084594') +
      theme_line +
      theme(legend.key.width = unit(1.5, 'cm'))
    
    ggsave(fig_refinery_count, 
           filename = file.path(proj_path, res_path, 'compare_cuf_crude_refinery_count.pdf'), 
           width = 14, 
           height = 12)
    
    embed_fonts(file.path(proj_path, res_path, 'compare_cuf_crude_refinery_count.pdf'),
                outfile = file.path(proj_path, res_path, 'compare_cuf_crude_refinery_count.pdf'))
    
    ggsave(fig_refinery_count,
           filename = file.path(proj_path, res_path, 'compare_cuf_crude_refinery_count.png'),
           width = 14,
           height = 12,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    
  
  