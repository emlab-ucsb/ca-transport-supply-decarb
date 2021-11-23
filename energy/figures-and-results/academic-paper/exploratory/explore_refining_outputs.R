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
refining_folder_path <- 'outputs/academic-out/refining/refining_2021-11-22/'
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


## 2019 emissions
refin_file <- 'ref_scenario_id_list.csv'
ghg_2019_file <- 'refining_emissions_state_2019_revised.csv'
ghg_2019 <- fread(paste0(main_path, "/model-development/scenario-plot/refinery-outputs/", ghg_2019_file))

ghg_2019 <- ghg_2019[boundary == "complete", .(year, value)]
ghg_2019[, mtco2e := value / 1e9]
ghg_2019[, value := NULL]
ghg_2019_val <- ghg_2019$mtco2e[1]


## carbon target results
target_results <- state_out[year == 2045, .(scen_id, year, carbon_price_scenario, ghg_kg)]
target_results[, total_state_ghg_MtCO2 := ghg_kg / (1000 * 1e6)]
target_results[, rel_reduction := (total_state_ghg_MtCO2 - ghg_2019_val) / ghg_2019_val]
target_results <- target_results[, .(scen_id, rel_reduction)]


## filter for subset
## ---------------------------------------

# 
# ## state scens
# state_scens <- state_out[## bau, three CCS scenarios
#                         (oil_price_scenario == "reference case" &
#                           carbon_price_scenario %in% c("price floor") &
#                           # ccs_scenario %in% c("medium CCS cost", "no ccs") &
#                           demand_scenario == "BAU" &
#                           refining_scenario == "historic production") |
#                           ## the three CCS scenarios interacting with carbon taxes
#                          (oil_price_scenario == "reference case" &
#                           # ccs_scenario %in% c("medium CCS cost", "no ccs") &
#                           demand_scenario == "BAU" &
#                           refining_scenario == "historic production") |
#                          (oil_price_scenario == "reference case" &
#                           carbon_price_scenario %in% c("price floor") &
#                           # ccs_scenario  %in% c("medium CCS cost", "no ccs") &
#                           demand_scenario == "LC1" &
#                           refining_scenario %in% c("historic exports", "low exports"))]


## state_scens
state_scens <- state_out[oil_price_scenario == "reference case"]

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

# state_levels[, policy_intervention := paste0(refining_scenario, " - ", demand_scenario, " demand")]
# 
# state_levels[, policy_intervention := fifelse(policy_intervention == "historic production - BAU demand" &
#                                                 carbon_price_scenario == "price floor", "BAU", policy_intervention)]

state_levels[, target := fifelse(str_detect(scen_id, '90_perc') == TRUE, "90% reduction", 
                                 fifelse(str_detect(scen_id, '1000ft') == TRUE, "1000ft setback",
                                         fifelse(str_detect(scen_id, '2500ft') == TRUE, "2500ft setback",
                                                 fifelse(str_detect(scen_id, '5280ft') == TRUE, "5280ft setback", "no target"))))]

state_levels[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", ccs_scenario)]

state_levels$ccs_option <- factor(state_levels$ccs_option, levels = c('no CCS', "high CCS cost", "medium CCS cost"))
state_levels$refining_scenario <- factor(state_levels$refining_scenario, levels = c('historic production', "historic exports", "low exports"))

## add scneario names
state_levels[, scenario := paste0(refining_scenario, " - ", demand_scenario, " demand")]

## add emission reduction
state_levels <- merge(state_levels, target_results,
                      by = "scen_id",
                      all.x = T)

state_levels[, target_name := paste0(round(rel_reduction, 2) * 100, "% GHG reduction")]


## V1 -- numbers
## make a series of plots 2020- 2045
## (bbls equiv, emissions, labor employment, labor compensation, mortality_level)

consumed_fig <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                               metric == "bbls_consumed",
                                               oil_price_scenario == "reference case",
                                               carbon_price_scenario == "price floor",
                                               year > 2019), aes(x = year, y = value / 1e6, color = refining_scenario, lty = demand_scenario, group = scenario)) +
  geom_line(size = 0.75, alpha = 0.7) +
  labs(title = "Refinery consumption",
       x = NULL,
       y = "Bbls crude equivalent (million bbls)",
       color = "Refinery scenario",
       lty = "Demand scenario") +
  guides(colour = guide_legend(order = 1), 
         lty = guide_legend(order = 2)) +
  # facet_grid(demand_scenario ~ ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 

ggsave(consumed_fig, 
       filename = file.path(save_info_path, 'pathway/consumption_x_time_fig.png'), 
       width = 8, 
       height = 6)

# embed_fonts(file.path(save_info_path, 'pathway/prod_x_time_fig.png'),
#             outfile = file.path(save_info_path, 'pathway/prod_x_time_fig.png'))

fig_ghg <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                          metric == "total_state_ghg_MtCO2",
                                          oil_price_scenario == "reference case",
                                          carbon_price_scenario == "price floor",
                                          year > 2019), aes(x = year, y = value, color = refining_scenario, lty = demand_scenario, group = scenario)) +
  geom_line(size = 0.75, alpha = 0.7) +
  labs(title = "Refinery segment emissions",
       subtitle = "carbon price scenario = price floor",
       x = NULL,
       y = "GHG (MtCO2e)",
       color = "Refinery scenario",
       lty = "Demand scenario") +
  # facet_wrap(~carbon_price_scenario, ncol = 3) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 

ggsave(fig_ghg, 
       filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.png'), 
       width = 8, 
       height = 6)

## carbon price scenario values
carbon_scens <- c("carbon_setback_1000ft-no ccs", "carbon_setback_2500ft-no ccs", "carbon_setback_5280ft-no ccs", "carbon_90_perc_reduction-no ccs")

## figs x carbon price scenario
fig_ghg_carbon_scens <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                          metric == "total_state_ghg_MtCO2",
                                          oil_price_scenario == "reference case",
                                          carbon_price_scenario %in% carbon_scens,
                                          year > 2019), aes(x = year, y = value, color = refining_scenario, lty = demand_scenario, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Refinery segment emissions",
       x = NULL,
       y = "GHG (MtCO2e)",
       color = "Emissions target\n(extraction)",
       lty = "Refining scenario") +
  facet_wrap(~ target_name) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 

ggsave(fig_ghg, 
       filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.png'), 
       width = 10, 
       height = 10)



## employment
emp_pw_fig <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                             metric == "total_emp",
                                             oil_price_scenario == "reference case",
                                             carbon_price_scenario == "price floor",
                                             year > 2019), aes(x = year, y = value / 1000, color = refining_scenario, lty = demand_scenario, group = scenario)) +
  geom_line(size = 0.75, alpha = 0.6) +
  labs(title = "Labor: Employment, FTE job-years",
       x = NULL,
       y = "FTE job-years (thousdand)",
       color = "Refinery scenario",
       lty = "Demand scenario") +
  # facet_grid(demand_scenario ~ ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 


ggsave(emp_pw_fig, 
       filename = file.path(save_info_path, 'pathway/labor_empl_x_time_fig.png'), 
       width = 8, 
       height = 6)
# 
# embed_fonts(file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'),
#             outfile = file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'))


## compensation
comp_pw_fig <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                              metric == "total_comp",
                                              oil_price_scenario == "reference case",
                                              carbon_price_scenario == "price floor",
                                              year > 2019), aes(x = year, y = value / 1e9, color = refining_scenario, lty = demand_scenario, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Labor: Total compensation",
       x = NULL,
       y = "Total compensation (USD billion)",
       color = "Refinery scenario",
       lty = "Demand scenario") +
  # facet_grid(demand_scenario ~ ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 


ggsave(comp_pw_fig, 
       filename = file.path(save_info_path, 'pathway/labor_comp_x_time_fig.png'), 
       width = 8, 
       height = 6)




## pm25
pm25_pw_fig <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                              metric == "mean_total_pm25",
                                              oil_price_scenario == "reference case",
                                              carbon_price_scenario == "price floor",
                                              year > 2019), aes(x = year, y = value, color = refining_scenario, lty = demand_scenario, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Mean total pm2.5 exposure",
       x = NULL,
       y = "pm2.5 exposure (ug/m3)",
       color = "Refinery scenario",
       lty = "Demand scenario") +
  # facet_grid(demand_scenario ~ ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 

ggsave(pm25_pw_fig, 
       filename = file.path(save_info_path, 'pathway/health_pm25_x_time_fig.png'), 
       width = 8, 
       height = 6)

# embed_fonts(file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'),
#             outfile = file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'))

## mortality
mortality_pw_fig <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                                   metric == "mortality_level",
                                                   oil_price_scenario == "reference case",
                                                   carbon_price_scenario == "price floor",
                                                   year > 2019), aes(x = year, y = value, color = refining_scenario, lty = demand_scenario, group = scen_id)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Health: Mortality level",
       x = NULL,
       y = "# premature deaths",
       color = "Refinery scenario",
       lty = "Demand scenario") +
  # facet_grid(demand_scenario ~ ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 


ggsave(mortality_pw_fig, 
       filename = file.path(save_info_path, 'pathway/health_mort_x_time_fig.png'), 
       width = 8, 
       height = 6)


## ----------------------------------------------------------------------










