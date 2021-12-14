## Tracey Mangin
## December 6, 2021
## Academic paper figure 3 - refining

## libraries
library(data.table)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)
library(rebus)
library(tidyverse)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/refining/figures/'

## csv names

npv_file <- 'npv_x_metric_refining.csv'
dac_file <- 'dac_health_labor_refining.csv'


## read in data
npv_dt <- fread(paste0(main_path, fig_path, npv_file))
dac_dt <- fread(paste0(main_path, fig_path, dac_file))

## update and factor title
npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: Abated GHG emissions', title)]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: Avoided mortality', 'Labor: Compensation', 'Climate: Abated GHG emissions'))

## update unit
npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2020 USD billion)", "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e")]

## add scenario
npv_dt[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]
npv_dt <- npv_dt[!scenario %in% c("historic production - BAU demand", "historic production - LC1 demand")]
npv_dt[, scenario := paste0(demand_scenario, " demand", ' - ', refining_scenario)]
npv_dt[, scenario := str_replace(scenario, "LC1", "Low carbon")]
npv_dt[, short_scen := str_replace(scenario, "Low carbon", "Low C.")]
npv_dt[, short_scen := str_replace(short_scen, "historic", "hist.")]

## factor
npv_dt$scenario <- factor(npv_dt$scenario, levels = c('BAU demand - historic exports', 
                                                      'BAU demand - low exports',
                                                      'Low carbon demand - historic exports',
                                                       'Low carbon demand - low exports'))

## factor
npv_dt$short_scen <- factor(npv_dt$short_scen, levels = c('BAU demand - hist. exports', 
                                                          'BAU demand - low exports',
                                                          'Low C. demand - hist. exports',
                                                          'Low C. demand - low exports'))



## fig
fig_benefit_x_metric <- ggplot(npv_dt %>% filter(ccs_scenario == "no ccs",
                                                 carbon_price_scenario == "price floor"), 
                               aes(x = rel_reduction * -100, y = value, color = short_scen)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(color = NULL,
       y = NULL,
       x = 'GHG emissions reduction in 2045 (% of 2019)') +
  facet_grid(measure~title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('BAU demand - hist. exports' = "#ff5e5b",
                                'BAU demand - low exports' = '#fcb97d',
                                # 'BAU demand - hist. production' = "#A84268",
                                'Low C. demand - hist. exports' = '#4a6c6f',
                                'Low C. demand - low exports' = "#9DBF9E")) +
  # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


ggsave(fig_benefit_x_metric,
       filename = file.path(main_path, fig_path, 'figure3a_refining.png'),
       width = 9.5,
       height = 5,
       units = "in")



## Equity portion
## ------------------------------------------------------

dac_dt$type <- factor(dac_dt$type, levels = c("Total population", "DAC population", "DAC share"))

dac_dt <- dac_dt[ccs_scenario == "no ccs" &
                  carbon_price_scenario == "price floor" &
                 !scenario %in% c("historic production - LC1 demand",
                                  "historic production - BAU demand")]


dac_dt[, scenario := paste0(demand_scenario, " demand", ' - ', refining_scenario)]
dac_dt <- dac_dt[, scenario := str_replace(scenario, "LC1", "Low carbon")]




## figure
fig_equity_labor <- ggplot(dac_dt %>% filter(metric == "Employment loss per avoided GHG"), 
                           aes(x = rel_reduction * -100, y = value, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = 'GHG emissions reduction in 2045 (% of 2019)',
       y = 'FTE job-years / avoided MtCO2e',
       color = NULL) +
  facet_wrap(~type, scales = "free_y") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('BAU demand - historic exports' = "#ff5e5b",
                                'BAU demand - low exports' = '#fcb97d',
                                # 'historic production - BAU demand' = "#A84268",
                                'Low carbon demand - historic exports' = '#4a6c6f',
                                'Low carbon demand - low exports' = "#9DBF9E")) +
  theme_line +
  # scale_y_continuous(limits = c(NA, 0.6)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## health
fig_equity_health <- ggplot(dac_dt %>% filter(metric != "Employment loss per avoided GHG"), 
                            aes(x = rel_reduction * -100, y = value, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = '',
       y = 'Avoided mortalities / avoided MtCO2e',
       color = NULL) +
  facet_wrap(~type, scales = "free_y") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('historic exports - BAU demand' = "#9DBF9E",
                                'historic exports - LC1 demand' = '#ff5e5b',
                                # 'historic production - BAU demand' = "#A84268",
                                'low exports - BAU demand' = '#4a6c6f',
                                'low exports - LC1 demand' = "#fcb97d")) +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


## dac share only

dac_shares <- ggplot(dac_dt %>% filter(type == "DAC share"), aes(x = rel_reduction * -100, y = value, color = scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = NULL,
       y = 'DAC share',
       color = NULL) +
  facet_wrap(~metric, ncol = 1) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('BAU demand - historic exports' = "#ff5e5b",
                                'BAU demand - low exports' = '#fcb97d',
                                # 'historic production - BAU demand' = "#A84268",
                                'Low carbon demand - historic exports' = '#4a6c6f',
                                'Low carbon demand - low exports' = "#9DBF9E")) +
  theme_line +
  scale_y_continuous(limits = c(0.35, 0.5)) +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



# ## legend on left plot for slides
# 
# left_panel <- ggpubr::ggarrange(fig_equity_health, fig_equity_labor, # list of plots
#                                 labels = "AUTO", # labels
#                                 common.legend = T, # COMMON LEGEND
#                                 legend = "left", # legend position
#                                 align = "hv", # Align them both, horizontal and vertical
#                                 nrow = 2)  # number of row

ggsave(dac_shares,
       filename = file.path(main_path, fig_path, 'figure3c_dac_refining.png'),
       width = 3,
       height = 5,
       units = "in")

