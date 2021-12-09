## Tracey Mangin
## December 6, 2021
## Academic paper figure 2

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)
library(rebus)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/'

## csv names
levels_file <- 'state_levels_subset.csv'
npv_file <- 'npv_x_metric.csv'
dac_file <- 'dac_health_labor.csv'


## read in data
npv_dt <- fread(paste0(main_path, fig_path, npv_file))
dac_dt <- fread(paste0(main_path, fig_path, dac_file))

## cumulative
npv_dt$target <- factor(npv_dt$target, levels = c('BAU', '1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                                '90% GHG reduction'))

npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: Abated GHG emissions', title)]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: Avoided mortality', 'Labor: Compensation', 'Climate: Abated GHG emissions'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'ccs_option', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction'),
                     measure.vars = c("value_billion", "value_per_ghg_million"),
                     variable.name = "unit",
                     value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2020 USD billion)", "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e")]

npv_dt <- npv_dt[target != 'BAU']

npv_dt$target <- factor(npv_dt$target, levels = c('1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                        '90% GHG reduction'))
## remove medium CCS
npv_dt <- npv_dt[ccs_option != "medium CCS cost"]

## fig
fig_benefit_x_metric <- ggplot(npv_dt %>% filter(target != 'BAU',
                                                 policy_intervention != 'carbon tax & setback'), aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(color = "GHG emission target",
       shape = "Policy intervention",
       y = NULL,
       x = 'GHG emissions reduction in 2045 (% of 2019)') +
  facet_grid(measure~title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
                                )) +
  # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"

ggsave(fig_benefit_x_metric,
       filename = file.path(main_path, fig_path, 'figure3a.png'),
       width = 9.5,
       height = 5,
       units = "in")


## Equity portion
## ------------------------------------------------------

dac_dt$pop_type <- factor(dac_dt$pop_type, levels = c("Total population", "DAC population", "DAC share"))

dac_dt <- dac_dt[ccs_option == "no CCS" &
                   target != "BAU"]


## figure
fig_equity_labor <- ggplot(dac_dt %>% filter(metric == "Employment loss per avoided GHG",
                                             policy_intervention != "carbon tax & setback"), aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = 'GHG emissions reduction in 2045 (% of 2019)',
       y = 'FTE job-years / avoided MtCO2e',
       color = "GHG emission target",
       shape = "Policy intervention") +
  facet_wrap(~pop_type, scales = "free_y") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
  )) +
  theme_line +
  scale_y_continuous(limits = c(NA, 0.5)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


## health
fig_equity_health <- ggplot(dac_dt %>% filter(metric != "Employment loss per avoided GHG",
                                              policy_intervention != "carbon tax & setback"), aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = '',
       y = 'Avoided mortalities / avoided MtCO2e',
       color = "GHG emission target",
       shape = "Policy intervention") +
  facet_wrap(~pop_type, scales = "free_y") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
  )) +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## extract the legend
legend <- get_legend(
  fig_equity_health 
  
)

## combine the figures
## ---------------------------------

fig3b_combine <- plot_grid(
  fig_equity_health + theme(legend.position="none"),
  fig_equity_labor + theme(legend.position="none"),
  legend,
  nrow = 3,
  rel_heights = c(1, 1, 0.3)
)


ggsave(fig3b_combine,
       filename = file.path(main_path, fig_path, 'figure3b.png'),
       width = 8,
       height = 8,
       units = "in")

## legend on left plot for slides

left_panel <- ggpubr::ggarrange(fig_equity_health, fig_equity_labor, # list of plots
                  labels = "AUTO", # labels
                  common.legend = T, # COMMON LEGEND
                  legend = "left", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  nrow = 2)  # number of row

ggsave(left_panel,
       filename = file.path(main_path, fig_path, 'figure3b_left.png'),
       width = 9.5,
       height = 6,
       units = "in")

## DAC only

dac_shares <- ggplot(dac_dt %>% filter(pop_type == "DAC share",
                                       policy_intervention != "carbon tax & setback"), 
                     aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = NULL,
       y = 'DAC share',
       color = NULL) +
  facet_wrap(~metric, ncol = 1) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
  )) +
  theme_line +
  scale_y_continuous(limits = c(0.25, 0.5)) +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



ggsave(dac_shares,
       filename = file.path(main_path, fig_path, 'figure3c_dac.png'),
       width = 3,
       height = 6,
       units = "in")









## remake figures with carbon + sb policy
## -----------------------------------------------------

npv_90 <- npv_dt[target == "90% GHG reduction"]
npv_90[, setback_dist := str_extract(scen_id, pattern = 'setback_' %R% one_or_more(DIGIT) %R% 'ft')]

npv_90 <- npv_90[, scen_name := fifelse(policy_intervention == 'carbon tax & setback', paste0('carbon tax + ', str_extract(setback_dist, one_or_more(DIGIT)), 'ft setback'), policy_intervention)]

npv_90$scen_name <- factor(npv_90$scen_name, levels = c("excise tax", "carbon tax", "carbon tax + 1000ft setback", "carbon tax + 2500ft setback", "carbon tax + 5280ft setback"))


## fig
fig_benefit_x_metric2 <- ggplot(npv_90 %>% filter(target != 'BAU'), aes(x = scen_name, y = value, color = target)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(color = NULL,
       y = NULL,
       x = NULL) +
  facet_grid(measure~title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
  )) +
  # # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



ggsave(fig_benefit_x_metric2,
       filename = file.path(main_path, fig_path, 'figure5a.png'),
       width = 8,
       height = 8,
       units = "in")



## DAC for carbon + sb
## -------------------------------

dac_90 <- dac_dt[target == "90% GHG reduction"]
dac_90[, setback_dist := str_extract(scen_id, pattern = 'setback_' %R% one_or_more(DIGIT) %R% 'ft')]

dac_90 <- dac_90[, scen_name := fifelse(policy_intervention == 'carbon tax & setback', paste0('carbon tax + ', str_extract(setback_dist, one_or_more(DIGIT)), 'ft setback'), policy_intervention)]

dac_90$scen_name <- factor(dac_90$scen_name, levels = c("excise tax", "carbon tax", "carbon tax + 1000ft setback", "carbon tax + 2500ft setback", "carbon tax + 5280ft setback"))





## figure
fig_equity_labor2 <- ggplot(dac_90 %>% filter(metric == "Employment loss per avoided GHG"), aes(x = scen_name, y = value, color = target)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = NULL,
       y = 'FTE job-years / avoided MtCO2e') +
  facet_wrap(~pop_type, scales = "free_y") +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
  )) +
  theme_line +
  scale_y_continuous(limits = c(NA, 0.5)) +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


## health
fig_equity_health2 <- ggplot(dac_90 %>% filter(metric != "Employment loss per avoided GHG"), aes(x = scen_name, y = value, color = target)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(x = '',
       y = 'Avoided mortalities / avoided MtCO2e') +
  facet_wrap(~pop_type, scales = "free_y") +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
  )) +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 




## combine the figures
## ---------------------------------

fig5b_combine <- plot_grid(
  fig_equity_health2,
  fig_equity_labor2,
  nrow = 2,
  rel_heights = c(1, 1)
)


ggsave(fig5b_combine,
       filename = file.path(main_path, fig_path, 'figure5b.png'),
       width = 8,
       height = 10,
       units = "in")


