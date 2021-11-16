## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)

## figure themes
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
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
refining_folder_path <- 'outputs/academic-out/refining/refining_2021-11-15/'
state_save_path     = paste0(main_path, refining_folder_path)

## create a folder to store outputs
cur_date              = Sys.Date()
save_info_path        = paste0(main_path, 'outputs/academic-out/refining/exploratory-figs/', paste0("figs_", cur_date))
dir.create(save_info_path, showWarnings = FALSE)  
# dir.create(paste0(save_info_path, "/cumulative_v1"), showWarnings = FALSE) 
# dir.create(paste0(save_info_path, "/cumulative_v2"), showWarnings = FALSE) 
dir.create(paste0(save_info_path, "/labor_v_mortality"), showWarnings = FALSE) 
dir.create(paste0(save_info_path, "/pathway"), showWarnings = FALSE) 

## read inputs
state_out <- fread(paste0(state_save_path, "/subset_state_results.csv"))

## site out for ghg 
site_out <- fread(paste0(main_path, refining_folder_path, "site_refining_outputs.csv"))


## filter for subset
## ---------------------------------------


## state scens
state_scens <- state_out[(oil_price_scenario == "reference case" &
                          carbon_price_scenario %in% c("price floor") &
                          ccs_scenario %in% c("medium CCS cost", "no ccs") &
                          demand_scenario == "BAU" &
                          refining_scenario == "historic production") |
                         (oil_price_scenario == "reference case" &
                          ccs_scenario %in% c("medium CCS cost", "no ccs") &
                          demand_scenario == "BAU" &
                          refining_scenario == "historic production") |
                         (oil_price_scenario == "reference case" &
                          carbon_price_scenario %in% c("price floor") &
                          ccs_scenario  %in% c("medium CCS cost", "no ccs") & 
                          demand_scenario == "LC1" &
                          refining_scenario %in% c("historic exports", "low exports"))]

state_labor_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                      ccs_scenario, demand_scenario,  refining_scenario, year, total_emp, total_comp)]

## melt
state_labor_levels <- melt(state_labor_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                           'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                           measure.vars = c("total_emp", "total_comp"),
                           variable.name = "metric",
                           value.name = "value")

## emissions and consumption
## ------------------------------
state_energy_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                        ccs_scenario, demand_scenario, refining_scenario, year, bbls_consumed, ghg_kg)]

state_energy_levels[, total_state_ghg_MtCO2 := ghg_kg / (1000 * 1e6)]
state_energy_levels[, ghg_kg := NULL]

state_energy_levels <- melt(state_energy_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                               'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario',  'year'),
                             measure.vars = c("bbls_consumed", "total_state_ghg_MtCO2"),
                             variable.name = "metric",
                             value.name = "value")

## health
## ------------------------------
state_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, demand_scenario,  refining_scenario, year, mean_total_pm25, mean_delta_total_pm25, mortality_level, mortality_delta, cost_2019_PV, cost_PV)]

state_health_levels <- melt(state_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                             'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                            measure.vars = c("mean_total_pm25", "mean_delta_total_pm25", "mortality_level", "mortality_delta", "cost_2019_PV", "cost_PV"),
                            variable.name = "metric",
                            value.name = "value")

## combine
##----------------------------------

state_levels <- rbind(state_energy_levels, state_labor_levels, state_health_levels)

state_levels[, policy_intervention := paste0(refining_scenario, " - ", demand_scenario, " demand")]

state_levels[, policy_intervention := fifelse(policy_intervention == "historic production - BAU demand" &
                                                carbon_price_scenario == "price floor", "BAU", policy_intervention)]



## targets
target1000 <- c("BAU historic production low innovation carbon_setback_1000ft medium CCS cost reference case",
                "BAU historic production low innovation carbon_setback_1000ft no ccs reference case")

target2500 <- c("BAU historic production low innovation central SCC medium CCS cost reference case",
                "BAU historic production low innovation central SCC no ccs reference case")

target5280 <- c("BAU historic production low innovation carbon_setback_5280ft medium CCS cost reference case",
                "BAU historic production low innovation carbon_setback_5280ft no ccs reference case")

target90 <- c("BAU historic production low innovation carbon_90_perc_reduction no ccs reference case",
              "BAU historic production low innovation carbon_90_perc_reduction medium CCS cost reference case")

state_levels[, target := fifelse(scen_id %in% target1000, "1000ft setback",
                                 fifelse(scen_id %in% target2500, "2500ft setback",
                                         fifelse(scen_id %in% target5280, "5280ft setback",
                                                 fifelse(scen_id %in% target90, "90% reduction", "no target"))))]

state_levels[, ccs_option := fifelse(ccs_scenario == "no ccs", "no ccs", "ccs")]

## V1 -- numbers
## make a series of plots 2019 - 2045
## (bbls equiv, emissions, labor employment, labor compensation, mortality_level)

consumed_fig <- ggplot(state_levels %>% filter(metric == "state_crude_eq_consumed_bbl",
                                              ccs_option == "ccs"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = target, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Refinery consumption",
       x = NULL,
       y = "Bbls crude equivalent (million bbls)",
       color = NULL) +
  # facet_wrap(~ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(consumed_fig, 
       filename = file.path(save_info_path, 'pathway/consumption_x_time_fig.png'), 
       width = 8, 
       height = 5)

# embed_fonts(file.path(save_info_path, 'pathway/prod_x_time_fig.png'),
#             outfile = file.path(save_info_path, 'pathway/prod_x_time_fig.png'))


## ccs options
consumed_fig_ccs <- ggplot(state_levels %>% filter(metric == "state_crude_eq_consumed_bbl"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = target, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Refinery consumption",
       x = NULL,
       y = "Bbls crude equivalent (million bbls)",
       color = NULL) +
  facet_wrap(~ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(consumed_fig_ccs, 
       filename = file.path(save_info_path, 'pathway/consumption_x_time_ccs_fig.png'), 
       width = 8, 
       height = 5)


## employment
emp_pw_fig <- ggplot(state_levels %>% filter(metric == "total_emp",
                                             ccs_option == "ccs"),  aes(x = year, y = value, color = policy_intervention, lty = target, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Labor: Total employment",
       x = NULL,
       y = "Total employment, FTE job-years",
       color = NULL) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(emp_pw_fig, 
       filename = file.path(save_info_path, 'pathway/labor_empl_x_time_fig.png'), 
       width = 8, 
       height = 5)
# 
# embed_fonts(file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'),
#             outfile = file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'))


## compensation
comp_pw_fig <- ggplot(state_levels %>% filter(metric == "total_comp",
                                              ccs_option == "ccs"),  aes(x = year, y = value / 1e9, color = policy_intervention, lty = target, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Labor: Total compensation",
       x = NULL,
       y = "Total compensation (USD billion)",
       color = NULL) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(comp_pw_fig, 
       filename = file.path(save_info_path, 'pathway/labor_comp_x_time_fig.png'), 
       width = 8, 
       height = 5)

# embed_fonts(file.path(save_info_path, 'pathway/labor_comp_x_time_fig.pdf'),
#             outfile = file.path(save_info_path, 'pathway/labor_comp_x_time_fig.pdf'))


## pm25
pm25_pw_fig <- ggplot(state_levels %>% filter(metric == "mean_total_pm25",
                                              ccs_option == "ccs"), aes(x = year, y = value, color = policy_intervention, lty = target, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Mean total pm2.5 exposure",
       x = NULL,
       y = "pm2.5 exposure (ug/m3)",
       color = NULL) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(pm25_pw_fig, 
       filename = file.path(save_info_path, 'pathway/health_pm25_x_time_fig.png'), 
       width = 8, 
       height = 5)

# embed_fonts(file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'),
#             outfile = file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'))

## mortality
mortality_pw_fig <- ggplot(state_levels %>% filter(metric == "mortality_level",
                                                   ccs_option == "ccs"), aes(x = year, y = value, color = policy_intervention, lty = target, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Mortality level",
       x = NULL,
       y = "# premature deaths",
       color = NULL) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(mortality_pw_fig, 
       filename = file.path(save_info_path, 'pathway/health_mortality_x_time_fig.png'), 
       width = 8, 
       height = 5)






