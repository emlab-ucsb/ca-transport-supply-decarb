## Tracey Mangin

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
extraction_folder_path <- 'outputs/academic-out/extraction/extraction_2021-10-12/'
save_info_path <- paste0(main_path, 'outputs/academic-out/extraction/exploratory-figs')


state_save_path     = paste0(main_path, extraction_folder_path, 'state-results/')

state_out <- fread(paste0(state_save_path, "subset_state_results.csv"))

## filter for BAU macro (ref oil price, price floor, low innovation, and medium CCS cost)
## keep all setback scenarios (no tax, carbon price floor)
## keep all four excise tax scenarios (no setback, carbon price floor)
## keep all carbon taxes match the setback scenarios (no setback, no excise tax)

state_scens <- state_out[(oil_price_scenario == "reference case" &
                            carbon_price_scenario %in% c("carbon_setback_1000ft", "carbon_setback_5280ft",
                                                         "carbon_90_perc_reduction", "central SCC") &
                            ccs_scenario == "medium CCS cost" &
                            setback_scenario == "no_setback" &
                            excise_tax_scenario == "no tax") |
                         (oil_price_scenario == "reference case" &
                         carbon_price_scenario == "price floor" &
                         ccs_scenario == "medium CCS cost") |
                         (oil_price_scenario == "reference case" &
                          carbon_price_scenario == "price floor" &
                          ccs_scenario == "medium CCS cost" &
                          setback_scenario == "no_setback" &
                          excise_tax_scenario == "no tax")]

state_labor_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                      ccs_scenario, excise_tax_scenario,  setback_scenario, year, c.dire_emp, c.indi_emp, c.indu_emp,
                                      c.dire_comp, c.indi_comp, c.indu_comp)]
## calculate totals
state_labor_levels[, total_emp := c.dire_emp + c.indi_emp + c.indu_emp]
state_labor_levels[, total_comp := c.dire_comp + c.indi_comp + c.indu_comp]

## select columns
state_labor_levels <- state_labor_levels[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                             ccs_scenario, setback_scenario, excise_tax_scenario, year, total_emp, total_comp)]

## select columns
state_labor_levels <- state_labor_levels[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                             ccs_scenario, setback_scenario, excise_tax_scenario, year, total_emp, total_comp)]

state_labor_levels <- melt(state_labor_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                           'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario', 'year'),
                           measure.vars = c("total_emp", "total_comp"),
                           variable.name = "metric",
                           value.name = "value")

## emissions and extraction
## ------------------------------
state_extract_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                        ccs_scenario, setback_scenario, excise_tax_scenario, year, total_state_bbl, total_state_ghg_kgCO2)]

state_extract_levels[, total_state_ghg_MtCO2 := total_state_ghg_kgCO2 / (1000 * 1e6)]
state_extract_levels[, total_state_ghg_kgCO2 := NULL]

state_extract_levels <- melt(state_extract_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                           'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario',  'year'),
                           measure.vars = c("total_state_bbl", "total_state_ghg_MtCO2"),
                           variable.name = "metric",
                           value.name = "value")

## health
## ------------------------------
state_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                        ccs_scenario, setback_scenario, excise_tax_scenario, year, total_pm25, mortality_level, cost_2019_PV, cost_PV)]

state_health_levels <- melt(state_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                               'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario', 'year'),
                             measure.vars = c("total_pm25", "mortality_level", "cost_2019_PV", "cost_PV"),
                             variable.name = "metric",
                             value.name = "value")

## combine
##----------------------------------

state_levels <- rbind(state_extract_levels, state_labor_levels, state_health_levels)

state_levels[, policy_intervention := fifelse(carbon_price_scenario != "price floor", "carbon tax",
                                              fifelse(setback_scenario != "no_setback", "setback",
                                                      fifelse(excise_tax_scenario != "no tax", "excise tax", "BAU")))]

## targets
target1000 <- c("reference case_no_setback_no quota_carbon_setback_1000ft_medium CCS cost_low innovation_no tax",
                "reference case_setback_1000ft_no quota_price floor_medium CCS cost_low innovation_no tax",
                "reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_setback_1000ft")

target2500 <- c("reference case_no_setback_no quota_central SCC_medium CCS cost_low innovation_no tax",
                "reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_setback_2500ft",
                "reference case_setback_2500ft_no quota_price floor_medium CCS cost_low innovation_no tax")

target5280 <- c("reference case_no_setback_no quota_carbon_setback_5280ft_medium CCS cost_low innovation_no tax",
                "reference case_setback_5280ft_no quota_price floor_medium CCS cost_low innovation_no tax",
                "reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_setback_5280ft")

target90 <- c("reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_tax_90_perc_reduction",
              "reference case_no_setback_no quota_carbon_90_perc_reduction_medium CCS cost_low innovation_no tax")

state_levels[, target := fifelse(scen_id %in% target1000, "1000ft setback",
                                              fifelse(scen_id %in% target2500, "2500ft setback",
                                                      fifelse(scen_id %in% target5280, "5280ft setback",
                                                              fifelse(scen_id %in% target90, "90% reduction", "BAU"))))]

## pathways
##--------------------

## V1 -- numbers
## make a series of plots 2019 - 2045
## (bbls, emissions, labor employment, labor compensation, mortality_level)

prod_pw_fig <- ggplot(state_levels %>% filter(metric == "total_state_bbl"), aes(x = year, y = value / 1e6, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Oil production",
       x = NULL,
       y = "Production (million bbls)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(prod_pw_fig, 
       filename = file.path(save_info_path, 'pathway/prod_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway/prod_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway/prod_x_time_fig.pdf'))

  
## ghg
ghg_pw_fig <- ggplot(state_levels %>% filter(metric == "total_state_ghg_MtCO2"), aes(x = year, y = value , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "GHG emissions",
       x = NULL,
       y = "GHG emissions (MtCO2e)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(ghg_pw_fig, 
       filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway/ghg_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway/ghg_x_time_fig.pdf'))


## employment
emp_pw_fig <- ggplot(state_levels %>% filter(metric == "total_emp"), aes(x = year, y = value , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Labor: Total employment",
       x = NULL,
       y = "Total employment",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(emp_pw_fig, 
       filename = file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway/labor_empl_x_time_fig.pdf'))


## compensation
comp_pw_fig <- ggplot(state_levels %>% filter(metric == "total_comp"), aes(x = year, y = value / 1e9 , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Labor: Total compensation",
       x = NULL,
       y = "Total compensation (USD billion)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(comp_pw_fig, 
       filename = file.path(save_info_path, 'pathway/labor_comp_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway/labor_comp_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway/labor_comp_x_time_fig.pdf'))


## pm25
pm25_pw_fig <- ggplot(state_levels %>% filter(metric == "total_pm25"), aes(x = year, y = value, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Total pm2.5 exposure",
       x = NULL,
       y = "pm2.5 exposure (ug/m3)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(pm25_pw_fig, 
       filename = file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway/health_pm25_x_time_fig.pdf'))

## mortality
mortality_pw_fig <- ggplot(state_levels %>% filter(metric == "mortality_level"), aes(x = year, y = value, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Mortality level",
       x = NULL,
       y = "# premature deaths",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(mortality_pw_fig, 
       filename = file.path(save_info_path, 'pathway/health_mortality_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway/health_mortality_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway/health_mortality_x_time_fig.pdf'))





## V2 -- relative to BAU
## ------------------------------------

## select health outputs

rel_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, setback_scenario, excise_tax_scenario, year, delta_total_pm25, mortality_delta, cost_2019, cost, cost_2019_PV, cost_PV)]

rel_health_levels <- melt(rel_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                             'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'excise_tax_scenario', 'year'),
                            measure.vars = c("delta_total_pm25", "mortality_delta", "cost_2019", "cost", "cost_2019_PV", "cost_PV"),
                            variable.name = "metric",
                            value.name = "value")


rel_health_levels[, target := fifelse(scen_id %in% target1000, "1000ft setback",
                                 fifelse(scen_id %in% target2500, "2500ft setback",
                                         fifelse(scen_id %in% target5280, "5280ft setback",
                                                 fifelse(scen_id %in% target90, "90% reduction", "BAU"))))]

rel_health_levels[, policy_intervention := fifelse(carbon_price_scenario != "price floor", "carbon tax",
                                              fifelse(setback_scenario != "no_setback", "setback",
                                                      fifelse(excise_tax_scenario != "no tax", "excise tax", "BAU")))]

rel_health_levels <- rel_health_levels[, .(scen_id, year, metric, policy_intervention, target, value)]

setnames(rel_health_levels, "value", "diff_bau")


## BAU outputs for labor and energy
bau_out <- state_levels[target == "BAU" & policy_intervention == "BAU" & metric %in% c("total_state_bbl",
                                                                                       "total_state_ghg_MtCO2",
                                                                                       "total_emp",
                                                                                       "total_comp")]

setnames(bau_out, "value", "bau_value")
bau_out <- bau_out[, .(year, metric, bau_value)]

## combine bau with scenario outputs
rel_vals <- state_levels[metric %in% c("total_state_bbl",
                                       "total_state_ghg_MtCO2",
                                       "total_emp",
                                       "total_comp")]

rel_vals <- merge(rel_vals, bau_out,
                  by = c("year", "metric"),
                  all.x = T)

rel_vals[, diff_bau := value - bau_value]
rel_vals <- rel_vals[, .(scen_id, year, metric, policy_intervention, target, diff_bau)]

## bind
state_rel_vals <- rbind(rel_vals, rel_health_levels)

## figs

## cost
cost_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "cost_PV"), aes(x = year, y = diff_bau / 1e6, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Cost of premature deaths relative to BAU",
       x = NULL,
       y = "Difference (USD million (present value)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(cost_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_health_cost_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_health_cost_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_health_cost_x_time_fig.pdf'))


## prod
prod_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "total_state_bbl"), aes(x = year, y = diff_bau / 1e6, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Oil production relative to BAU",
       x = NULL,
       y = "Difference in production (million bbls)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(prod_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_prod_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_prod_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_prod_x_time_fig.pdf'))


## ghg
ghg_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "total_state_ghg_MtCO2"), aes(x = year, y = diff_bau , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "GHG emissions relative to BAU",
       x = NULL,
       y = "Difference in GHG emissions (MtCO2e)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(ghg_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_ghg_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_ghg_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_ghg_x_time_fig.pdf'))


## employment
emp_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "total_emp"), aes(x = year, y = diff_bau , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Labor: Total employment relative to BAU",
       x = NULL,
       y = "Difference in total employment",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(emp_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_labor_empl_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_labor_empl_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_labor_empl_x_time_fig.pdf'))


## compensation
comp_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "total_comp"), aes(x = year, y = diff_bau / 1e9 , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Labor: Total compensation relative to BAU",
       x = NULL,
       y = "Difference in total compensation (USD billion)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(comp_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_labor_comp_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_labor_comp_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_labor_comp_x_time_fig.pdf'))


## pm25
pm25_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "delta_total_pm25"), aes(x = year, y = diff_bau, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Total pm2.5 exposure relative to BAU",
       x = NULL,
       y = "Difference in pm2.5 exposure (ug/m3)",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(pm25_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_health_pm25_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_health_pm25_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_health_pm25_x_time_fig.pdf'))

## mortality
mortality_rel_fig <- ggplot(state_rel_vals %>% filter(metric == "mortality_delta"), aes(x = year, y = diff_bau, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Mortality level",
       x = NULL,
       y = "# premature deaths",
       color = NULL) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm')) 

ggsave(mortality_rel_fig, 
       filename = file.path(save_info_path, 'pathway_rel_bau/rel_health_mortality_x_time_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'pathway_rel_bau/rel_health_mortality_x_time_fig.pdf'),
            outfile = file.path(save_info_path, 'pathway_rel_bau/rel_health_mortality_x_time_fig.pdf'))



## cumulative
##--------------------

## labor compensation, labor job years, emissions, bbls, mortality: 

## 2045 emissions
ghg_2019 <- unique(state_levels[metric == "total_state_ghg_MtCO2" &
                           year == 2019, .(year, value)])

ghg_2045 <- state_levels[metric == "total_state_ghg_MtCO2" &
                         year == 2045, .(scen_id, policy_intervention, target, year, value)]

setnames(ghg_2045, "value", "ghg_2045")

ghg_2045[, ghg_2045_perc := (ghg_2045 - ghg_2019$value[1]) / ghg_2019$value[1]]

ghg_2045[, ghg_2045 := NULL]
ghg_2045[, year := NULL]
ghg_2045[, policy_intervention := NULL]
ghg_2045[, target := NULL]

## 2019 values
vals_2019 <- unique(state_levels[year == 2019, .(metric, value)])
setnames(vals_2019, "value", "value_2019")

## calculate relative values
cumul_df <- merge(state_levels, vals_2019,
                  by = c("metric"),
                  all.x = T)

cumul_df[, diff_2019 := value - value_2019]

cumul_df <- cumul_df[, .(sum_metric = sum(diff_2019)), by = .(scen_id, policy_intervention, target, metric)]

cumul_df <- merge(cumul_df, ghg_2045,
                  by = c("scen_id"),
                  all.x = T)

cumul_df[, scen_name := paste(policy_intervention, target, sep = " - ")]


## V1a: take the difference between 2019 and year t, add all years for cumulative impact, x axis == scenario name
## -----------------------------------------------------------------------

## cost
cost_cum1_fig <- ggplot(cumul_df %>% filter(metric == "cost_PV"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative cost of premature deaths relative to 2019 ",
       x = NULL,
       y = "USD million (present value)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(cost_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_health_cost_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_health_cost_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_health_cost_v1_fig.pdf'))


## prod
prod_cum1_fig <- ggplot(cumul_df %>% filter(metric == "total_state_bbl"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative oil production",
       x = NULL,
       y = "Production (million of bbls)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(prod_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_prod_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_prod_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_prod_v1_fig.pdf'))


## ghg
ghg_cum1_fig <- ggplot(cumul_df %>% filter(metric == "total_state_ghg_MtCO2"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions relative to 2019",
       x = NULL,
       y = "GHG emissions (MtCO2e)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(ghg_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_ghg_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_ghg_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_ghg_v1_fig.pdf'))


## employment
emp_cum1_fig <- ggplot(cumul_df %>% filter(metric == "total_emp"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total employment relative to 2019",
       x = NULL,
       y = "Total employment)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(emp_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_empl_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_empl_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_empl_v1_fig.pdf'))


## compensation
comp_cum1_fig <- ggplot(cumul_df %>% filter(metric == "total_comp"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric / 1e9, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total compensation relative to 2019",
       x = NULL,
       y = "Compensation",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(comp_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_compensationl_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_compensationl_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_compensationl_v1_fig.pdf'))



## pm25

pm25_cum1_fig <- ggplot(cumul_df %>% filter(metric == "total_pm25"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative pm2.5 exposure relative to 2019",
       x = NULL,
       y = "pm2.5 exposure (ug/m3)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(pm25_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_pm25_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_pm25_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_pm25_v1_fig.pdf'))


## mortality

mortality_cum1_fig <- ggplot(cumul_df %>% filter(metric == "mortality_level"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative mortality level relative to 2019",
       x = NULL,
       y = "mortality",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(mortality_cum1_fig, 
       filename = file.path(save_info_path, 'cumulative_v1/cumulative_mortality_v1_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v1/cumulative_mortality_v1_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v1/cumulative_mortality_v1_fig.pdf'))



## V1b: same as above, x axis == 2045 emissions as a percentage of 2019 emissions
## cost
cost_cum2_fig <- ggplot(cumul_df %>% filter(metric == "cost_PV"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative cost of premature deaths relative to 2019 ",
       x = "GHG emissions (% of 2019)",
       y = "USD million (present value)",
       color = NULL) +
  scale_x_continuous(limits = c(NA, 0)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(cost_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_health_cost_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_health_cost_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_health_cost_v2_fig.pdf'))


## prods
prod_cum2_fig <- ggplot(cumul_df %>% filter(metric == "total_state_bbl"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative oil production",
       x = "GHG emissions (% of 2019)",
       y = "Production (million of bbls)",
       color = NULL) +
  scale_x_continuous(limits = c(NA, 0)) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(prod_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_prod_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_prod_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_prod_v2_fig.pdf'))


## ghg
ghg_cum2_fig <- ggplot(cumul_df %>% filter(metric == "total_state_ghg_MtCO2"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions relative to 2019",
       x = "GHG emissions (% of 2019)",
       y = "GHG emissions (MtCO2e)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(ghg_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_ghg_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_ghg_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_ghg_v2_fig.pdf'))


## employment
emp_cum2_fig <- ggplot(cumul_df %>% filter(metric == "total_emp"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total employment relative to 2019",
       x = "GHG emissions (% of 2019)",
       y = "Total employment)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(emp_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_empl_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_empl_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_empl_v2_fig.pdf'))


## compensation
comp_cum2_fig <- ggplot(cumul_df %>% filter(metric == "total_comp"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric / 1e9, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total compensation relative to 2019",
       x = "GHG emissions (% of 2019)",
       y = "Compensation",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(comp_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_compensationl_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_compensationl_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_compensationl_v2_fig.pdf'))



## pm25

pm25_cum2_fig <- ggplot(cumul_df %>% filter(metric == "total_pm25"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative pm2.5 exposure relative to 2019",
       x = "GHG emissions (% of 2019)",
       y = "pm2.5 exposure (ug/m3)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(pm25_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_pm25_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_pm25_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_pm25_v2_fig.pdf'))


## mortality

mortality_cum2_fig <- ggplot(cumul_df %>% filter(metric == "mortality_level"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative mortality level relative to 2019",
       x = "GHG emissions (% of 2019)",
       y = "mortality",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(mortality_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_mortality_v2_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'cumulative_v2/cumulative_mortality_v2_fig.pdf'),
            outfile = file.path(save_info_path, 'cumulative_v2/cumulative_mortality_v2_fig.pdf'))



## V2a: take the difference between BAU year t and scenario year t, add all years for cumulative impact, x axis == scenario name
bau_cumulative_df <- state_rel_vals[, .(sum_diff_bau = sum(diff_bau)), by = .(scen_id, metric, policy_intervention, target)]

bau_cumulative_df <- merge(bau_cumulative_df, ghg_2045,
                           by = c("scen_id"),
                           all.x = T)

bau_cumulative_df[, scen_name := paste(policy_intervention, target, sep = " - ")]

setnames(bau_cumulative_df, "sum_diff_bau", "sum_metric")

## v1 bau
## cost
cost_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "cost_PV"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative cost of premature deaths relative to BAU ",
       x = NULL,
       y = "USD million (present value)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(cost_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_health_cost_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_health_cost_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_health_cost_v1b_fig.pdf'))


## prod
prod_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_state_bbl"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative oil production relative to BAU",
       x = NULL,
       y = "Production (million of bbls)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(prod_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_prod_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_prod_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_prod_v1b_fig.pdf'))


## ghg
ghg_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_state_ghg_MtCO2"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions relative to BAU",
       x = NULL,
       y = "GHG emissions (MtCO2e)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(ghg_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_ghg_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_ghg_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_ghg_v1b_fig.pdf'))


## employment
emp_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_emp"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total employment relative to BAU",
       x = NULL,
       y = "Total employment)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(emp_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_empl_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_empl_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_empl_v1b_fig.pdf'))


## compensation
comp_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_comp"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric / 1e9, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total compensation relative to BAU",
       x = NULL,
       y = "Compensation",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(comp_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_compensationl_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_compensationl_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_compensationl_v1b_fig.pdf'))



## pm25

pm25_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "delta_total_pm25"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative pm2.5 exposure relative to BAU",
       x = NULL,
       y = "pm2.5 exposure (ug/m3)",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(pm25_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_pm25_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_pm25_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_pm25_v1b_fig.pdf'))


## mortality

mortality_cum1b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "mortality_delta"), aes(x = reorder(scen_name, -sum_metric), y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative mortality level relative to BAU",
       x = NULL,
       y = "mortality",
       color = NULL) +
  theme_line +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(mortality_cum1b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v1/cumulative_mortality_v1b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v1/cumulative_mortality_v1b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v1/cumulative_mortality_v1b_fig.pdf'))


## V2b: same as above, x axis == 2045 emissions as a percentage of 2019 emissions
## cost
# 
# mortality_cum2_fig <- ggplot(cumul_df %>% filter(metric == "mortality_level"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(title = "Health: Cumulative mortality level relative to 2019",
#        x = "GHG emissions (% of 2019)",
#        y = "mortality",
#        color = NULL) +
#   theme_line +
#   scale_x_continuous(limits = c(NA, 0)) +
#   theme(legend.position = "right",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


cost_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "cost_PV"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative cost of premature deaths relative to BAU ",
       x = "GHG emissions (% of 2019)",
       y = "USD million (present value)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(cost_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_health_cost_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_health_cost_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_health_cost_v2b_fig.pdf'))


## prod
prod_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_state_bbl"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric / 1e6, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative oil production relative to BAU",
       x = "GHG emissions (% of 2019)",
       y = "Production (million of bbls)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(prod_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_prod_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_prod_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_prod_v2b_fig.pdf'))


## ghg
ghg_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_state_ghg_MtCO2"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions relative to BAU",
       x = "GHG emissions (% of 2019)",
       y = "GHG emissions (MtCO2e)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(ghg_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_ghg_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_ghg_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_ghg_v2b_fig.pdf'))


## employment
emp_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_emp"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total employment relative to BAU",
       x = "GHG emissions (% of 2019)",
       y = "Total employment)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(emp_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_empl_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_empl_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_empl_v2b_fig.pdf'))


## compensation
comp_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "total_comp"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric / 1e9, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative total compensation relative to BAU",
       x = "GHG emissions (% of 2019)",
       y = "Compensation",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(comp_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_compensationl_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_compensationl_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_compensationl_v2b_fig.pdf'))



## pm25

pm25_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "delta_total_pm25"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative pm2.5 exposure relative to BAU",
       x = "GHG emissions (% of 2019)",
       y = "pm2.5 exposure (ug/m3)",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(pm25_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_pm25_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_pm25_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_pm25_v2b_fig.pdf'))


## mortality

mortality_cum2b_fig <- ggplot(bau_cumulative_df %>% filter(metric == "mortality_delta"), aes(x = round(ghg_2045_perc, digits = 2) * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative mortality level relative to BAU",
       x = "GHG emissions (% of 2019)",
       y = "mortality",
       color = NULL) +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(mortality_cum2b_fig, 
       filename = file.path(save_info_path, 'bau_cumulative_v2/cumulative_mortality_v2b_fig.pdf'), 
       width = 11, 
       height = 8)

embed_fonts(file.path(save_info_path, 'bau_cumulative_v2/cumulative_mortality_v2b_fig.pdf'),
            outfile = file.path(save_info_path, 'bau_cumulative_v2/cumulative_mortality_v2b_fig.pdf'))

## labor vs mortality

labor_health_df <- bau_cumulative_df[metric %in% c('cost_PV', 'total_comp', 'total_emp')]

labor_health_df <- dcast(labor_health_df, scen_id + policy_intervention + target ~ metric, value.var = "sum_metric")

## labor compensation vs. mortality ($) (relative to BAU for both)
labor_health_fig1 <- ggplot(labor_health_df, aes(x = total_comp /1e9, y = cost_PV / 1e9, color = target, shape = policy_intervention, group = scen_id)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor compensation vs. health mortality cost (relative to BAU)",
       x = "Cumulative compensation relative to BAU (billion)",
       y = "Cumulative mortality cost relative to BAU (present value) (billion)",
       color = NULL) +
  theme_line +
  # scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(labor_health_fig1, 
       filename = file.path(save_info_path, 'labor_v_mortality/compensation_v_mortality_cost.pdf'), 
       width = 8, 
       height = 8)

embed_fonts(file.path(save_info_path, 'labor_v_mortality/compensation_v_mortality_cost.pdf'),
            outfile = file.path(save_info_path, 'labor_v_mortality/compensation_v_mortality_cost.pdf'))


## labor job years vs. mortality ($) (relative to BAU for both)
labor_health_fig2 <- ggplot(labor_health_df, aes(x = total_emp / 1000, y = cost_PV / 1e9, color = target, shape = policy_intervention, group = scen_id)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor employment vs. health mortality cost (relative to BAU)",
       x = "Cumulative employment relative to BAU (thousand)",
       y = "Cumulative mortality cost relative to BAU (present value) (billion)",
       color = NULL) +
  theme_line +
  # scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(labor_health_fig2, 
       filename = file.path(save_info_path, 'labor_v_mortality/employment_v_mortality_cost.pdf'), 
       width = 8, 
       height = 8)

embed_fonts(file.path(save_info_path, 'labor_v_mortality/employment_v_mortality_cost.pdf'),
            outfile = file.path(save_info_path, 'labor_v_mortality/employment_v_mortality_cost.pdf'))


## color lines based on emissions reduction categories, shape for policy intervention





