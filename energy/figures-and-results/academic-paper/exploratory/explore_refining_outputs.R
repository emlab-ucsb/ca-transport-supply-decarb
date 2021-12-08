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
dir.create(paste0(save_info_path, "/cumulative"), showWarnings = FALSE)
dir.create(paste0(save_info_path, "/labor_v_mortality"), showWarnings = FALSE) 
dir.create(paste0(save_info_path, "/pathway"), showWarnings = FALSE) 


## files
carbon_px_file <- 'carbon_price_scenarios_revised.xlsx'
scc_file <- 'social_cost_carbon.csv'

## read inputs
state_out <- fread(paste0(state_save_path, "/subset_state_results.csv"))
county_out <- fread(paste0(main_path, refining_folder_path, '/county_refining_outputs.csv'))
site_out <- fread(paste0(main_path, refining_folder_path, "site_refining_outputs.csv"))


## CPI values
cpi_df <- setDT(read.xlsx(paste0(main_path, 'data/stocks-flows/processed/', carbon_px_file), sheet = 'BLS Data Series', startRow = 12))

cpi_df <- cpi_df[Year %in% c(2019, 2020), .(Year, Annual)]

setnames(cpi_df, c("Year", "Annual"), c("year", "annual"))

cpi2020 <- cpi_df %>%
  filter(year == 2020) %>%
  select(annual) %>%
  as.numeric()

cpi2019 <- cpi_df %>%
  filter(year == 2019) %>%
  select(annual) %>%
  as.numeric()

## discount rate
discount_rate <- 0.03

## social cost of carbon
scc_df <- fread(paste0(main_path, 'data/stocks-flows/processed/', scc_file))

## filter for 3 percent
scc_df_filt <- scc_df[discount_rate == 'three_perc_avg', .(year, social_cost_co2)]



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


## state scens

## carbon price scenario values
carbon_scens <- c("price floor", "central SCC", "price ceiling")


state_scens <- state_out[(oil_price_scenario == "reference case" &
                          carbon_price_scenario %in% carbon_scens)]

## state_scens
state_labor_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                      ccs_scenario, demand_scenario,  refining_scenario, year, total_emp, total_comp)]

## calc PV
state_labor_levels[, total_comp_PV := total_comp / ((1 + discount_rate) ^ (year - 2019))]


## melt
state_labor_levels <- melt(state_labor_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                           'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                           measure.vars = c("total_emp", "total_comp", "total_comp_PV"),
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

## convert to 2020 USD
state_health_levels[, cost_PV_20 := cost_PV / cpi2019 * cpi2020]


state_health_levels <- melt(state_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                             'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                            measure.vars = c("mean_total_pm25", "mean_delta_total_pm25", "mortality_level", "mortality_delta", "cost_2019_PV", "cost_PV", "cost_PV_20"),
                            variable.name = "metric",
                            value.name = "value")

## combine
##----------------------------------

state_levels <- rbind(state_energy_levels, state_labor_levels, state_health_levels)

# state_levels[, policy_intervention := paste0(refining_scenario, " - ", demand_scenario, " demand")]
# 
# state_levels[, policy_intervention := fifelse(policy_intervention == "historic production - BAU demand" &
#                                                 carbon_price_scenario == "price floor", "BAU", policy_intervention)]

# state_levels[, target := fifelse(str_detect(scen_id, '90_perc') == TRUE, "90% reduction", 
#                                  fifelse(str_detect(scen_id, '1000ft') == TRUE, "1000ft setback",
#                                          fifelse(str_detect(scen_id, '2500ft') == TRUE, "2500ft setback",
#                                                  fifelse(str_detect(scen_id, '5280ft') == TRUE, "5280ft setback", "no target"))))]
# 
# state_levels[, ccs_option := fifelse(ccs_scenario == "no ccs", "no CCS", ccs_scenario)]
# 
# state_levels$ccs_option <- factor(state_levels$ccs_option, levels = c('no CCS', "high CCS cost", "medium CCS cost"))
# state_levels$refining_scenario <- factor(state_levels$refining_scenario, levels = c('historic production', "historic exports", "low exports"))

## add scneario names
state_levels[, scenario := paste0(refining_scenario, " - ", demand_scenario, " demand")]

## figs x carbon price scenario
state_levels$carbon_price_scenario <- factor(state_levels$carbon_price_scenario,
                                             levels = c('price floor', 'central SCC', 'price ceiling'))


state_levels$ccs_scenario <- factor(state_levels$ccs_scenario,
                                    levels = c('no ccs', 'medium CCS cost', 'high CCS cost'))



## add emission reduction
# state_levels <- merge(state_levels, target_results,
#                       by = "scen_id",
#                       all.x = T)

# state_levels[, target_name := paste0(round(rel_reduction, 2) * 100, "% GHG reduction")]


## V1 -- numbers
## make a series of plots 2020- 2045
## (bbls equiv, emissions, labor employment, labor compensation, mortality_level)

consumed_fig <- ggplot(state_levels %>% filter(ccs_scenario %in% c("medium CCS cost"),
                                               metric == "bbls_consumed",
                                               oil_price_scenario == "reference case",
                                               carbon_price_scenario == "price floor",
                                               year > 2019), aes(x = year, y = value / 1e6, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.7) +
  labs(title = "Refinery consumption",
       x = NULL,
       y = "Bbls crude equivalent (million bbls)",
       color = "Refinery-demand scenario",
       lty = "CCS scenario") +
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

fig_ghg <- ggplot(state_levels %>% filter(ccs_scenario %in% c("medium CCS cost"),
                                          metric == "total_state_ghg_MtCO2",
                                          oil_price_scenario == "reference case",
                                          carbon_price_scenario == "price floor",
                                          year > 2019), aes(x = year, y = value, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.7) +
  labs(title = "Refinery segment emissions",
       subtitle = "carbon price scenario = price floor",
       x = NULL,
       y = "GHG (MtCO2e)",
       color = "Refinery-demand scenario") +
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



fig_ghg_carbon_scens <- ggplot(state_levels %>% filter(ccs_scenario %in% c("high CCS cost", "no ccs"),
                                          metric == "total_state_ghg_MtCO2",
                                          oil_price_scenario == "reference case",
                                          year > 2019), aes(x = year, y = value, color = scenario, lty = ccs_scenario)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Refinery segment emissions",
       x = NULL,
       y = "GHG (MtCO2e)",
       color = "Refinery-demand scenario", 
       lty = "CCS scenario") +
  facet_wrap(~ carbon_price_scenario) +
  scale_linetype_manual(values = c("no ccs" = "solid", "medium CCS cost" = "dotted", "high CCS cost" = "dotdash")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  guides(colour = guide_legend(order = 1), 
         lty = guide_legend(order = 2)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm')) 

ggsave(fig_ghg_carbon_scens, 
       filename = file.path(save_info_path, 'pathway/ghg_x_time_carbon_scens_high.png'), 
       width = 10, 
       height = 8)



## employment
emp_pw_fig <- ggplot(state_levels %>% filter(ccs_scenario == "medium CCS cost",
                                             metric == "total_emp",
                                             oil_price_scenario == "reference case",
                                             carbon_price_scenario == "price floor",
                                             year > 2019), aes(x = year, y = value / 1000, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Labor: Employment, FTE job-years",
       x = NULL,
       y = "FTE job-years (thousdand)",
       color = "Refinery-demand scenario") +
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
                                              year > 2019), aes(x = year, y = value / 1e9, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Labor: Total compensation",
       x = NULL,
       y = "Total compensation (USD billion)",
       color = "Refinery-demand scenario") +
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
                                              year > 2019), aes(x = year, y = value, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Health: Mean total pm2.5 exposure",
       x = NULL,
       y = "pm2.5 exposure (ug/m3)",
       color = "Refinery-demand scenario",
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
                                                   year > 2019), aes(x = year, y = value, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.75) +
  labs(title = "Health: Mortality level",
       x = NULL,
       y = "# premature deaths",
       color = "Refinery-demand scenario") +
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
## relative to BAU
## ----------------------------------------------------------------------

## select health outputs

rel_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                     ccs_scenario, demand_scenario, refining_scenario, year,
                                     mean_delta_total_pm25, mortality_delta, cost_2019, cost, cost_2019_PV, cost_PV)]


rel_health_levels[, cost_PV_20 := cost_PV / cpi2019 * cpi2020]


rel_health_levels <- melt(rel_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario',
                                                         'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                          measure.vars = c("mean_delta_total_pm25", "mortality_delta", "cost_2019", "cost", "cost_2019_PV", "cost_PV", "cost_PV_20"),
                          variable.name = "metric",
                          value.name = "value")

setnames(rel_health_levels, "value", "diff_bau")


rel_health_levels[, scenario := paste0(refining_scenario, " - ", demand_scenario, " demand")]

## add levels
rel_health_levels$carbon_price_scenario <- factor(rel_health_levels$carbon_price_scenario,
                                             levels = c('price floor', 'central SCC', 'price ceiling'))

## add levels
rel_health_levels$ccs_scenario <- factor(rel_health_levels$ccs_scenario,
                                    levels = c('no ccs', 'medium CCS cost', 'high CCS cost'))


## BAU outputs for labor and energy
bau_out <- state_levels[carbon_price_scenario == "price floor" & ccs_scenario == "no ccs" &
                          demand_scenario == "BAU" & refining_scenario == "historic production" &
                          metric %in% c("bbls_consumed", "total_state_ghg_MtCO2", "total_emp",
                                        "total_comp", "total_comp_PV")]

setnames(bau_out, "value", "bau_value")
bau_out <- bau_out[, .(year, metric, bau_value)]

## combine bau with scenario outputs
rel_vals <- state_levels[metric %in% c("bbls_consumed",
                                       "total_state_ghg_MtCO2",
                                       "total_emp",
                                       "total_comp",
                                       "total_comp_PV")]

rel_vals <- merge(rel_vals, bau_out,
                  by = c("year", "metric"),
                  all.x = T)

rel_vals[, diff_bau := value - bau_value]

rel_vals <- rel_vals[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                         ccs_scenario, demand_scenario, refining_scenario, year, metric, 
                         diff_bau, scenario)]

## bind
state_rel_vals <- rbind(rel_vals, rel_health_levels)


## cumulative ---------------------------------------------------------

bau_cumulative_df <- state_rel_vals[year > 2019, .(sum_diff_bau = sum(diff_bau)), by = .(scen_id, oil_price_scenario, innovation_scenario,
                                                                                         carbon_price_scenario, ccs_scenario, demand_scenario,
                                                                                         refining_scenario, scenario, metric)]

bau_cumulative_df <- merge(bau_cumulative_df, target_results,
                           by = c("scen_id"),
                           all.x = T)


setnames(bau_cumulative_df, "sum_diff_bau", "sum_metric")




## labor vs mortality
## --------------------------------------------------------------------

labor_health_df <- bau_cumulative_df[metric %in% c('cost_PV_20', 'mortality_delta', 'total_comp_PV', 'total_emp')]

labor_health_df <- dcast(labor_health_df, scen_id + oil_price_scenario + innovation_scenario +
                        carbon_price_scenario + ccs_scenario + demand_scenario + refining_scenario +
                        scenario ~ metric, value.var = "sum_metric")

## labor job years vs. mortality ($) (relative to BAU for both)
labor_health_fig <- ggplot(labor_health_df %>% filter(carbon_price_scenario == "price floor",
                                                      ccs_scenario == "no ccs"), 
                           aes(x = total_emp, y = mortality_delta * -1, color = scenario, group = scen_id)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor employment vs. health mortality # (relative to BAU)",
       x = "Cumulative employment job-years relative to BAU",
       y = "Cumulative avoided mortality relative to BAU",
       color = "Refining-demand scenario") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(labor_health_fig, 
       filename = file.path(save_info_path, 'labor_v_mortality/employment_v_mortality_num.png'), 
       width = 6, 
       height = 5)

## cumulative reduction
## ---------------------------------------------------------------------

fig_bbls_ghg <- ggplot(bau_cumulative_df %>% filter(carbon_price_scenario == "price floor",
                                                    ccs_scenario == "no ccs",
                                                    metric == "bbls_consumed"), 
                           aes(x = rel_reduction * -100, y = sum_metric / 1e9, color = scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Cumulative crude equivalent consumption relative to BAU",
       x = "Reduction in GHG emissions:\n2045 compared to 2019",
       y = "Relative cumulative crude consumption\n(billion bbls crude equivalent)",
       color = "Refining-demand scenario") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_bbls_ghg, 
       filename = file.path(save_info_path, 'cumulative/cumulative_crude_ghg.png'), 
       width = 6, 
       height = 5)

## ghgs

fig_ghg_ghg <- ggplot(bau_cumulative_df %>% filter(carbon_price_scenario == "price floor",
                                                    ccs_scenario == "no ccs",
                                                    metric == "total_state_ghg_MtCO2"), 
                       aes(x = rel_reduction * -100, y = sum_metric, color = scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Cumulative GHG emissions relative to BAU",
       subtitle = "no CCS, carbon price = price floor",
       x = "Reduction in GHG emissions:\n2045 compared to 2019",
       y = "Relative cumulative GHG emissions (MtCO2e)",
       color = "Refining-demand scenario",
       shape = "CCS scenarios") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_ghg_ghg, 
       filename = file.path(save_info_path, 'cumulative/cumulative_ghg_ghg.png'), 
       width = 6, 
       height = 5)


fig_ghg_ghg2 <- ggplot(bau_cumulative_df %>% filter(metric == "total_state_ghg_MtCO2"), 
                      aes(x = rel_reduction * -100, y = sum_metric, color = scenario, shape = ccs_scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Cumulative GHG emissions relative to BAU",
       x = "Reduction in GHG emissions: 2045 compared to 2019",
       y = "Relative cumulative GHG emissions (MtCO2e)",
       color = "Refining-demand scenario",
       shape = "CCS scenarios") +
  theme_line +
  facet_wrap(~carbon_price_scenario) +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_ghg_ghg2, 
       filename = file.path(save_info_path, 'cumulative/cumulative_ghg_ghg_all.png'), 
       width = 9, 
       height = 6)

## labor
fig_labor_ghg <- ggplot(bau_cumulative_df %>% filter(carbon_price_scenario == "price floor",
                                                    ccs_scenario == "no ccs",
                                                    metric == "total_emp"), 
                       aes(x = rel_reduction * -100, y = sum_metric / 1000, color = scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Labor: cumulative FTE job-years relative to BAU",
       x = "Reduction in GHG emissions:\n2045 compared to 2019",
       y = "FTE job-years (thousand)",
       color = "Refining-demand scenario") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_labor_ghg, 
       filename = file.path(save_info_path, 'cumulative/cumulative_emp_ghg.png'), 
       width = 6, 
       height = 5)

## labor
fig_labor_comp_ghg <- ggplot(bau_cumulative_df %>% filter(carbon_price_scenario == "price floor",
                                                     ccs_scenario == "no ccs",
                                                     metric == "total_comp_PV"), 
                        aes(x = rel_reduction * -100, y = sum_metric / 1e9, color = scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Labor: cumulative compensation relative to BAU",
       x = "Reduction in GHG emissions:\n2045 compared to 2019",
       y = "Compensation (USD billion, present value)",
       color = "Refining-demand scenario") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_labor_comp_ghg, 
       filename = file.path(save_info_path, 'cumulative/cumulative_compensation_ghg.png'), 
       width = 6, 
       height = 5)

## health
fig_mortality_ghg <- ggplot(bau_cumulative_df %>% filter(carbon_price_scenario == "price floor",
                                                     ccs_scenario == "no ccs",
                                                     metric == "mortality_delta"), 
                        aes(x = rel_reduction * -100, y = sum_metric * -1, color = scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Health: cumulative avoided mortality relative to BAU",
       x = "Reduction in GHG emissions:\n2045 compared to 2019",
       y = "Avoided mortality",
       color = "Refining-demand scenario") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_y_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_mortality_ghg, 
       filename = file.path(save_info_path, 'cumulative/cumulative_mortality_ghg.png'), 
       width = 6, 
       height = 5)


## health
fig_mort_usd_ghg <- ggplot(bau_cumulative_df %>% filter(carbon_price_scenario == "price floor",
                                                         ccs_scenario == "no ccs",
                                                         metric == "cost_PV_20"), 
                            aes(x = rel_reduction * -100, y = sum_metric / -1e9, color = scenario)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(title = "Health: cumulative avoided mortality relative to BAU (USD)",
       x = "Reduction in GHG emissions:\n2045 compared to 2019",
       y = "Avoided mortality (USD billion, present value)",
       color = "Refining-demand scenario") +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  scale_y_continuous(label = comma) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_mort_usd_ghg, 
       filename = file.path(save_info_path, 'cumulative/cumulative_mort_usd_ghg.png'), 
       width = 6, 
       height = 5)

## -----------------------------------------------------------------------
## NPV
## -----------------------------------------------------------------------

## 1) cumul benefit = cumul health benefit -compensation loss + carbon mitigation
## 2) cumul benefit / cumulative GHG emisisons

## social cost of carbon
scc_value <- state_rel_vals[metric == 'total_state_ghg_MtCO2']

## join with social cost of carbon
scc_value <- merge(scc_value, scc_df_filt,
                   by = 'year',
                   all.x = T)

scc_value[, scc_avoided_ghg := diff_bau * -1e6 * social_cost_co2]
scc_value <- scc_value[year > 2019]

## summarise
cumul_scc_value <- scc_value[, .(scc_avoided_ghg = sum(scc_avoided_ghg, na.rm = T)), by = .(scen_id, oil_price_scenario,
                                                                                            innovation_scenario, carbon_price_scenario,
                                                                                            ccs_scenario, demand_scenario, refining_scenario, scenario)]


cumul_rel_vals_bau <- state_rel_vals[, .(diff_bau = sum(diff_bau)), by = .(scen_id, oil_price_scenario, innovation_scenario,
                                                                           carbon_price_scenario, ccs_scenario, demand_scenario,
                                                                           refining_scenario, metric)]

cumul_rel_vals_bau <- cumul_rel_vals_bau[metric %in% c( "total_comp", "total_comp_PV", "cost_2019",
                                                        "cost", "cost_2019_PV", "cost_PV", "cost_PV_20")]

cumul_rel_vals_bau <- dcast(cumul_rel_vals_bau, scen_id + oil_price_scenario + innovation_scenario + carbon_price_scenario +
                              ccs_scenario + demand_scenario + refining_scenario ~ metric, value.var = "diff_bau")

## join witih scc
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, cumul_scc_value,
                            by = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario',
                                   'ccs_scenario', 'demand_scenario', 'refining_scenario'),
                            all.x = T)

## total ghg emissions
ghg_total <- state_levels[metric == "total_state_ghg_MtCO2"]
ghg_total <- ghg_total[, .(cumul_ghg = sum(value)), by = .(scen_id)]

## cumul ghg
avoided_ghg <- state_rel_vals[metric == 'total_state_ghg_MtCO2']
avoided_ghg <- avoided_ghg[, .(cumul_diff_ghg = sum(diff_bau)), by = .(scen_id)]


## join
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, ghg_total,
                            by = "scen_id",
                            all.x = T)

## join with avoided ghg
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, avoided_ghg,
                            by = "scen_id",
                            all.x = T)



## calc benefit
cumul_rel_vals_bau[, benefit := (cost_PV_20 * -1) + total_comp_PV + scc_avoided_ghg]
cumul_rel_vals_bau[, benefit_per_ghg := benefit / (cumul_diff_ghg * -1)]

cumul_rel_vals_bau2 <- melt(cumul_rel_vals_bau, id.vars = c('scen_id', 'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 
                                                            'refining_scenario', 'scenario'),
                            measure.vars = c("benefit", "benefit_per_ghg"),
                            variable.name = "metric",
                            value.name = "value")

cumul_rel_vals_bau2[, metric := fifelse(metric == "benefit", "Net benefit (USD)", "Net benefit / MtCO2e (USD per MtCO2e)")]

cumul_rel_vals_bau2 <- merge(cumul_rel_vals_bau2, target_results,
                             by = "scen_id",
                             all.x = T)

cumul_rel_vals_bau2[, value := fifelse(is.na(value), 0, value)]

cumul_rel_vals_bau2[, adj_scaler := fifelse(metric == "Net benefit (USD)", 1e9, 1e6)]
cumul_rel_vals_bau2[, adj_value := value / adj_scaler]
cumul_rel_vals_bau2[, adj_metric := fifelse(metric == "Net benefit (USD)", "Net benefit (billion USD)", "Net benefit /\nMtCO2e (million USD per MtCO2e)")]


## figure
fig_benefit <- ggplot(cumul_rel_vals_bau2 %>% filter(ccs_scenario == "no ccs", 
                                                     carbon_price_scenario == 'price floor',
                                                     scenario != "historic production - LC1 demand"), 
                      aes(x = rel_reduction * -100, y = adj_value, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Net cumulative benefit (USD)",
       subtitle = "no CCS",
       x = "Reduction in GHG emissions: 2045 compared to 2019",
       y = "Benefit (avoided mortality - labor compensation + scc)",
       color = NULL,
       shape = "ccs_scenario") +
  facet_wrap(~adj_metric, scales = "free_y") +
  theme_line +
  # scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

ggsave(fig_benefit,
       filename = file.path(save_info_path, 'cumulative/net_cumul_benefit_fig.png'),
       width = 8,
       height = 6)


## benefit x metric
## -----------------------------------------

npv_x_metric <- melt(cumul_rel_vals_bau, id.vars = c('scen_id', 'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'cumul_ghg', 'cumul_diff_ghg'),
                     measure.vars = c("total_comp_PV", "cost_PV_20", "scc_avoided_ghg"),
                     variable.name = "metric",
                     value.name = "value")

npv_x_metric[, value := fifelse(metric == 'cost_PV_20', value * -1, value)]
npv_x_metric[, value_per_ghg :=  value / cumul_diff_ghg]
npv_x_metric[, value_billion := value / 1e9]
npv_x_metric[, value_per_ghg_million := value_per_ghg / 1e6]
npv_x_metric[, title := fifelse(metric == "total_comp_PV", "Labor: Compensation",
                                fifelse(metric == "cost_PV_20", "Health: Avoided mortality", "Abated GHG"))]

npv_x_metric <- merge(npv_x_metric, target_results,
                      by = "scen_id",
                      all.x = T)

npv_x_metric[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]

npv_x_metric[, value_per_ghg_million := fifelse(is.na(value_per_ghg_million), 0, value_per_ghg_million)]

## fig
fig_benefit_x_metric <- ggplot(npv_x_metric %>% filter(ccs_scenario == "no ccs",
                                                       carbon_price_scenario == "price floor",
                                                       scenario != "historic production - LC1 demand"), 
                               aes(x = rel_reduction * -100, y = value_billion, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "NPV metrics",
       subtitle = "no CCS",
       x = 'Reduction in GHG emissions: 2045 compared to 2019',
       y = "NPV (2020 USD billion)",
       color = NULL) +
  facet_wrap(~title) +
  theme_line +
  # scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_benefit_x_metric, 
       filename = file.path(save_info_path, 'cumulative/benefit_x_metric.png'), 
       width = 7, 
       height = 5)


## fig -- scale by avoided ghg
fig_benefit_x_metric_ghg <- ggplot(npv_x_metric %>% filter(ccs_scenario == "no ccs",
                                                           carbon_price_scenario == "price floor",
                                                           scenario != "historic production - LC1 demand"), 
                                   aes(x = rel_reduction * -100, y = value_per_ghg_million, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "NPV metrics per avoided GHG",
       subtitle = "no CCS",
       x = 'Reduction in GHG emissions: 2045 compared to 2019',
       y = "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)",
       color = NULL) +
  facet_wrap(~title) +
  theme_line +
  # scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

ggsave(fig_benefit_x_metric_ghg, 
       filename = file.path(save_info_path, 'cumulative/benefit_x_metric_ghg.png'), 
       width = 8, 
       height = 6)




## ----------------------------------------------------------------------
## cumulative impacts / avoided GHG emissions (total, DAC, DAC share)
## ----------------------------------------------------------------------


## labor, county
labor_scens <- county_out[(oil_price_scenario == "reference case" &
                            carbon_price_scenario %in% carbon_scens &
                            innovation_scenario == "low innovation" &
                            ccs_scenario %in% c('no ccs', 'medium CCS cost', 'high CCS cost'))]

labor_scens[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]

## bau
bau_emp <- labor_scens[BAU_scen == 1, .(county, year, total_emp)]
setnames(bau_emp, "total_emp", "bau_emp")

## join
labor_scens <-  merge(labor_scens, bau_emp,
                      by = c("county", "year"),
                      all.x = T)

labor_scens[, diff := total_emp - bau_emp]
labor_scens[, dac_emp := diff * dac_share]


## calculate dac share of jobs 
abs_labor <- labor_scens[, .(scen_id, ccs_scenario, carbon_price_scenario, refining_scenario,
                             demand_scenario, scenario, county, year, dac_share, diff, dac_emp)]

# abs_labor[, dac_emp := total_emp * dac_share]

abs_labor <- abs_labor[, .(dac_labor = sum(dac_emp),
                           total_labor = sum(diff)), by = .(scen_id, ccs_scenario, carbon_price_scenario, refining_scenario,
                                                            demand_scenario, scenario, year)]

# abs_labor[, dac_share := dac_labor / total_labor]


## calculate dac share, labor FTE
labor_dac <- labor_scens[, .(cumul_dac_emp_loss = sum(dac_emp), 
                             cumul_total_emp_loss = sum(diff)), by = .(scen_id, oil_price_scenario, ccs_scenario, 
                                                                       carbon_price_scenario, refining_scenario,
                                                                       demand_scenario, scenario, county, median_hh_income)]


labor_dac_state <- labor_dac[, .(cumul_dac_emp_loss = sum(cumul_dac_emp_loss), 
                                 cumul_total_emp_loss = sum(cumul_total_emp_loss)), by = .(scen_id, oil_price_scenario, ccs_scenario, 
                                                                                           carbon_price_scenario, refining_scenario,
                                                                                           demand_scenario, scenario)]

labor_dac_state[, dac_share_emp := fifelse(cumul_dac_emp_loss == 0 & cumul_total_emp_loss == 0, 0, cumul_dac_emp_loss / cumul_total_emp_loss)]

labor_dac_state <- merge(labor_dac_state, target_results,
                         by = "scen_id",
                         all.x = T)

labor_dac_state <- merge(labor_dac_state, ghg_total,
                         by = "scen_id",
                         all.x = T)

## cumulative job loss rel bau / cumulative ghg savings rel bau
## ---------------------------------------------------------------

## add cumulative ghg savings relative to bau
cumul_ghg_df <- bau_cumulative_df[metric == "total_state_ghg_MtCO2", .(scen_id, sum_metric)]
setnames(cumul_ghg_df, "sum_metric", "cumul_ghg_savings")

# labor ghg
labor_ghg_df <- merge(labor_dac_state, cumul_ghg_df,
                      by = "scen_id",
                      all.x = T)

labor_ghg_df[, dac_loss_ghg := fifelse(cumul_dac_emp_loss == 0 & cumul_ghg_savings == 0, 0, cumul_dac_emp_loss / (cumul_ghg_savings * -1))]
labor_ghg_df[, total_loss_ghg := fifelse(cumul_total_emp_loss == 0 & cumul_ghg_savings == 0, 0, cumul_total_emp_loss / (cumul_ghg_savings * -1))]
labor_ghg_df[, dac_share_loss_ghg := fifelse(dac_loss_ghg == 0 & total_loss_ghg == 0, 0, dac_loss_ghg / total_loss_ghg)]

labor_ghg_df <- melt(labor_ghg_df, id.vars = c("scen_id", "oil_price_scenario", "ccs_scenario", "carbon_price_scenario",
                                               "refining_scenario", "demand_scenario", "scenario", "cumul_ghg_savings", "rel_reduction"),
                     measure.vars = c("dac_loss_ghg", "total_loss_ghg", "dac_share_loss_ghg"),
                     variable.name = "type", value.name = "emp_loss_ghg")

labor_ghg_df[, type := fifelse(type == "dac_loss_ghg", "DAC population",
                               fifelse(type == "total_loss_ghg", "Total population", "DAC share"))]

labor_ghg_df$type <- factor(labor_ghg_df$type, levels = c("Total population", "DAC population", "DAC share"))

labor_ghg_df <- labor_ghg_df %>%
  mutate(emp_loss_ghg_adj = ifelse(scenario == "historic production - BAU demand", NA, emp_loss_ghg))

## figure
fig_emp_ghg <- ggplot(labor_ghg_df %>% filter(ccs_scenario == "no ccs",
                                              carbon_price_scenario == "price floor",
                                              scenario != "historic production - LC1 demand"), aes(x = rel_reduction * -100, y = emp_loss_ghg_adj, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Labor: Cumulative job-years relative to BAU per\ncumulative avoided GHG MtCO2e (FTE job-year loss/MtCO2e)",
       subtitle = "no CCS",
       x = NULL,
       y = "FTE job-years /MtCO2e",
       color = NULL) +
  facet_wrap(~type, ncol = 3, scales = "free_y") +
  theme_line +
  scale_y_continuous(limits = c(NA, 1)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(fig_emp_ghg, 
       filename = file.path(save_info_path, 'cumulative/labor_ghg_fig.png'), 
       width = 8, 
       height = 6)




## --------------------------------------------------------------------------
## health DAC
## --------------------------------------------------------------------------

## compile health outputs
files_to_process <- paste0(unique(state_scens$scen_id), "_ct_results.rds")

health_out <- list()

for (i in 1:length(files_to_process)) {
  
  file_name <- files_to_process[i]
  
  scen_out <- readRDS(paste0(state_save_path, 'subset-census-tract-results/', file_name))
  
  health_out[[i]] <- scen_out
  
}

health_scens <- rbindlist(health_out)
  

## calculate dac share, mortality
health_scens[, dac_multiplier := fifelse(disadvantaged == "Yes", 1, 0)]

health_scens[, dac_share_mortality := dac_multiplier * mortality_delta]

health_dac_state <- health_scens[, .(cumul_dac_mortality = sum(dac_share_mortality, na.rm = T), 
                               cumul_total_mortality = sum(mortality_delta, na.rm = T)), by = .(scen_id)]



health_dac_state[, dac_share_health := cumul_dac_mortality / cumul_total_mortality]

health_dac_state[, dac_share_health := fifelse(is.na(dac_share_health), 0, dac_share_health)]

## merge with scenario info
scenario_info <- unique(state_levels[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                  ccs_scenario, demand_scenario, refining_scenario, scenario)])


health_dac_state <- merge(health_dac_state, scenario_info,
                          by = "scen_id",
                          all.x = T)

## merge with ghg info
health_dac_state <- merge(health_dac_state, target_results,
                         by = "scen_id",
                         all.x = T)

health_dac_state <- merge(health_dac_state, ghg_total,
                         by = "scen_id",
                         all.x = T)

## cumulative job loss rel bau / cumulative ghg savings rel bau
## ---------------------------------------------------------------

# labor ghg
health_ghg_df <- merge(health_dac_state, cumul_ghg_df,
                      by = "scen_id",
                      all.x = T)

health_ghg_df[, dac_avoided_mort_ghg := fifelse(cumul_dac_mortality == 0 & cumul_ghg_savings == 0, 0, cumul_dac_mortality / cumul_ghg_savings)]
health_ghg_df[, total_avoided_mort_ghg := fifelse(cumul_total_mortality == 0 & cumul_ghg_savings == 0, 0, cumul_total_mortality / cumul_ghg_savings)]
health_ghg_df[, dac_share_loss_ghg := fifelse(dac_avoided_mort_ghg == 0 & total_avoided_mort_ghg == 0, 0, dac_avoided_mort_ghg / total_avoided_mort_ghg)]

health_ghg_df <- melt(health_ghg_df, id.vars = c("scen_id", "oil_price_scenario", "ccs_scenario", "carbon_price_scenario",
                                               "refining_scenario", "demand_scenario", "scenario", "cumul_ghg_savings", "rel_reduction"),
                     measure.vars = c("dac_avoided_mort_ghg", "total_avoided_mort_ghg", "dac_share_loss_ghg"),
                     variable.name = "type", value.name = "avoided_mort_ghg")

health_ghg_df[, type := fifelse(type == "dac_avoided_mort_ghg", "DAC population",
                               fifelse(type == "total_avoided_mort_ghg", "Total population", "DAC share"))]

health_ghg_df$type <- factor(health_ghg_df$type, levels = c("Total population", "DAC population", "DAC share"))

health_ghg_df <- health_ghg_df %>%
  mutate(avoided_mort_ghg_adj = ifelse(scenario == "historic production - BAU demand", NA, avoided_mort_ghg))


## figure
fig_health_ghg <- ggplot(health_ghg_df %>% filter(ccs_scenario == "no ccs",
                                              carbon_price_scenario == "price floor",
                                              scenario != "historic production - LC1 demand"), aes(x = rel_reduction * -100, y = avoided_mort_ghg_adj, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Health: Cumulative avoided mortality relative to BAU per\ncumulative avoided GHG MtCO2e (FTE job-year loss/MtCO2e)",
       subtitle = "no CCS",
       x = NULL,
       y = "Avoided mortalities (#)",
       color = NULL) +
  facet_wrap(~type, ncol = 3, scales = "free_y") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(fig_health_ghg, 
       filename = file.path(save_info_path, 'cumulative/health_ghg_fig.png'), 
       width = 8, 
       height = 6)






