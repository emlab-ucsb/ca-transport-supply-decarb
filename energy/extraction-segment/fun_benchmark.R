benchmark_outputs <- function(oil_price_selection, output_extraction) {
  
  print('Now creating benchmarks...')
  
  # create objects for list items -------
  
  state_out = output_extraction[[3]]
  
  field_out = output_extraction[[2]]
  
  ## all scenario combinations
  scen_combos <- unique(state_out[, c('oil_price_scenario', 'innovation_scenario', "carbon_price_scenario", "ccs_scenario", 
                                      "setback_scenario", "prod_quota_scenario", "excise_tax_scenario")])
  
  
  ## melt production
  state_out_prod <- state_out[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                              "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", 
                              "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl")]
  
  state_out_prod <- melt(state_out_prod,
                         measure.vars = c("existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"),
                         variable.name = "type",
                         value.name = "production_bbls")
  
  state_out_prod[, type := str_extract(type, "[^_]+")]
  
  ## melt ghg
  state_out_ghg <- state_out[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                  "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", 
                                 "new_wells", "existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e")]
  
  state_out_ghg <- melt(state_out_ghg,
                         measure.vars = c("existing_ghg_kgCO2e", "new_ghg_kgCO2e", "total_ghg_kgCO2e"),
                         variable.name = "type",
                         value.name = "ghg_kgCO2e")
  
  state_out_ghg[, type := str_extract(type, "[^_]+")]
  
  ## merge
  state_all <- merge(state_out_prod, state_out_ghg)
  
  ## MtCO2e
  state_all <- state_all[, ghg_MtCO2e := ghg_kgCO2e / 1e9]
  
  ## avg emissions
  state_all <- state_all[, mean_emis_factor := ghg_kgCO2e / production_bbls]
  
  ## scenarios
  oil_px_scens <- scen_combos[(innovation_scenario == 'low innovation' & 
                            carbon_price_scenario == 'price floor' & 
                            ccs_scenario == 'medium CCS cost' &
                            excise_tax_scenario == 'no tax' &
                            setback_scenario == 'no_setback' &
                            prod_quota_scenario == 'no quota')] 
  oil_px_scens[, scenario := 'oil price scenarios']
  
  innovation_scens <- scen_combos[(oil_price_scenario == 'iea oil price' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota')]
  innovation_scens[, scenario := 'innovation scenarios']
  
  carbon_px_scens <- scen_combos[(oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota')]
  carbon_px_scens[, scenario := 'carbon price scenarios']
  
  ccs_cost_scens <- scen_combos[(oil_price_scenario == 'iea oil price' & 
                                    innovation_scenario == 'low innovation' & 
                                    carbon_price_scenario == 'price floor' & 
                                    excise_tax_scenario == 'no tax' &
                                    setback_scenario == 'no_setback' &
                                    prod_quota_scenario == 'no quota')]
  ccs_cost_scens[, scenario := 'CCS cost scenarios']
  
  tax_scens <- scen_combos[(oil_price_scenario == 'iea oil price' & 
                                 innovation_scenario == 'low innovation' & 
                                 carbon_price_scenario == 'price floor' & 
                                 ccs_scenario == 'medium CCS cost' & 
                                 setback_scenario == 'no_setback' &
                                 prod_quota_scenario == 'no quota')]
  tax_scens[, scenario := 'tax scenarios']
  
  setback_scens <- scen_combos[(oil_price_scenario == 'iea oil price' & 
                              innovation_scenario == 'low innovation' & 
                              carbon_price_scenario == 'price floor' & 
                              ccs_scenario == 'medium CCS cost' & 
                              excise_tax_scenario == 'no tax' & 
                              prod_quota_scenario == 'no quota')]
  setback_scens[, scenario := 'setback scenarios']
  
  quota_scens <- scen_combos[(oil_price_scenario == 'iea oil price' & 
                                  innovation_scenario == 'low innovation' & 
                                  carbon_price_scenario == 'price floor' & 
                                  ccs_scenario == 'medium CCS cost' & 
                                  excise_tax_scenario == 'no tax' & 
                                  setback_scenario == 'no_setback')]
  quota_scens[, scenario := 'quota scenarios']
  
  ## all scenarios
  scenarios_df <- rbind(oil_px_scens, innovation_scens, carbon_px_scens, ccs_cost_scens, setback_scens, quota_scens, tax_scens) %>%
    select(scenario, oil_price_scenario:excise_tax_scenario) %>%
    left_join(state_all) %>%
    mutate(oil_price_scenario = as.character(oil_price_scenario),
           ccs_scenario = as.character(ccs_scenario)) %>%
    mutate(option = ifelse(scenario == 'oil price scenarios', oil_price_scenario,
                           ifelse(scenario == 'innovation scenarios', innovation_scenario,
                                  ifelse(scenario == 'carbon price scenarios', carbon_price_scenario,
                                         ifelse(scenario == 'CCS cost scenarios', ccs_scenario,
                                                ifelse(scenario == 'setback scenarios', setback_scenario,
                                                       ifelse(scenario == 'quota scenarios', prod_quota_scenario, excise_tax_scenario)))))))


  # plot theme --------------------
  
  theme_line =  
    theme_ipsum(base_family = 'Arial',
                grid = 'Y',
                plot_title_size = 12,
                subtitle_size = 11,
                axis_title_just = 'center',
                axis_title_size = 12,
                axis_text_size = 12,
                strip_text_size = 12)  +
    theme(plot.title = element_text(hjust = 0, face = 'bold'),
          plot.title.position = 'plot',
          plot.subtitle = element_text(hjust = 0),
          plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
          # axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), angle = 90),
          axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
          axis.line.x = element_line(color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          axis.ticks.length.x = unit(0.25, "cm"),
          axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
          plot.margin = unit(c(1,2,1,1), "lines"),
          strip.text = element_text(hjust = 0.5),
          legend.text = element_text(size = 11),
          legend.position = 'bottom',
    )
  
  scen_options <- unique(scenarios_df$scenario)
  
  
  plot_scen <- function(scen_choice) {
  
    if(scen_choice == 1) {name <- scen_options[1]} 
    else if(scen_choice == 2) {name <- scen_options[2]}
    else if(scen_choice == 3) {name <- scen_options[3]}
    else if(scen_choice == 4) {name <- scen_options[4]}
    else if(scen_choice == 5) {name <- scen_options[5]}
    else if(scen_choice == 6) {name <- scen_options[6]}
    else {name <- scen_options[7]}
    
  # new well entry
  new_wells_fig = ggplot(scenarios_df %>% filter(scenario == name,
                                                 type == 'total'), 
                         aes(x = year, y = new_wells, color = option)) + 
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'State-level new wells',
         # subtitle = 'number of new wells', 
         x = NULL,
         y = "# new wells",
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line 
  
  # oil production
  oil_prod_fig = ggplot(scenarios_df %>% filter(scenario == name,
                                                type == 'total'), 
                         aes(x = year, y = production_bbls / 1e6, color = option)) + 
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'State-level oil production',
         # subtitle = 'million barrels', 
         x = NULL,
         y = 'million bbls',
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  # ghg
  ghg_fig = ggplot(scenarios_df %>% filter(scenario == name,
                                          type == 'total'), 
                        aes(x = year, y = ghg_MtCO2e, color = option)) + 
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'State-level GHG emissions',
         # subtitle = 'MtCO2e', 
         x = NULL,
         y = 'MtCO2e',
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  # mean ghg emissions
  mean_ghg_fig = ggplot(scenarios_df %>% filter(scenario == name,
                                           type == 'total'), 
                   aes(x = year, y = mean_emis_factor, color = option)) + 
    geom_line(alpha = 0.7, size = 1) +
    labs(title = 'Average GHG emissions factor',
         # subtitle = 'kgCO2e per barrel', 
         x = NULL,
         y = 'kgCO2e per barrel',
         color = '') +
    expand_limits(y = 0) +
    # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
    theme_line
  
  out <- cowplot::plot_grid(new_wells_fig + theme(legend.position = 'none'),
                            oil_prod_fig + theme(legend.position = 'none'), 
                            ghg_fig + theme(legend.position = 'none'), 
                            mean_ghg_fig + theme(legend.position = 'none'),
                            nrow = 1)
  
  legend <- cowplot::get_legend(new_wells_fig)
  
  scen_plot <- cowplot::plot_grid(out, legend, ncol = 1, rel_heights = c(1, .1))
  
  ## title
  
  if(scen_choice == 4) {
    
    title <- ggdraw() + 
      draw_label(
        paste0(name, ': carbon price = ceiling'),
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
    
    final_out <- plot_grid(
      title, scen_plot,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    )  
    
  } else {
  
  title <- ggdraw() + 
    draw_label(
      name,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  final_out <- plot_grid(
    title, scen_plot,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  
    }
  
  }
  
p1 <- plot_scen(1)
p2 <- plot_scen(2)
p3 <- plot_scen(3)
p4 <- plot_scen(4)
p5 <- plot_scen(5)
p6 <- plot_scen(6)
p7 <- plot_scen(7)

macro_plot <- plot_grid(p1, p2, p3, p4, ncol = 1)
policy_plot <- plot_grid(p5, p6, p7, ncol = 1)

  # save figures  -----
  
  # save info file
  save_info_path = file.path(save_path, run_type, "benchmark-figs")
  dir.create(save_info_path, showWarnings = FALSE)
  print(paste0("Saving benchmark figures to ", save_info_path))
  
  # save figures ----
  macro_fname = paste0(oil_price_selection, '_macro_fig.pdf')
  ggsave(macro_plot, 
         filename = file.path(save_info_path, macro_fname), 
         width = 25, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, macro_fname),
              outfile = file.path(save_info_path, macro_fname))
  print(paste0('Saved benchmark figures to ', macro_fname))
  
  
  policy_fname = paste0(oil_price_selection, '_policy_fig.pdf')
  ggsave(policy_plot, 
         filename = file.path(save_info_path, policy_fname), 
         width = 25, 
         height = 20)
  
  embed_fonts(file.path(save_info_path, policy_fname),
              outfile = file.path(save_info_path, policy_fname))
  print(paste0('Saved diagnostic figures to ', policy_fname))

  
  
} 