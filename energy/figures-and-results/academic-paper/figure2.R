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

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/'

## csv names
levels_name <- 'state_levels_subset.csv'

## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

## filter out carbon + setback
levels_dt <- levels_dt[policy_intervention != 'carbon tax & setback' & ccs_scenario == "no ccs"]


## horizontal panel, A) production; B) GHG emissions; C) Cumulative GHG emissions x 2045 emissions reduction

prod_fig <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                           year > 2019), aes(x = year, y = value / 1e6, color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "Oil production",
       x = NULL,
       y = "Oil production (million bbls)",
       color = "2045 GHG emission target",
       lty = "Policy intervention") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed", "carbon tax & setback" = "longdash")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(prod_fig, 
       filename = file.path(save_info_path, 'pathway/prod_x_time_fig.png'), 
       width = 8, 
       height = 5)



## ghg
ghg_pw_fig <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                             year > 2019), aes(x = year, y = value , color = target, lty = policy_intervention)) +
  geom_line(size = 0.75, alpha = 0.8) +
  labs(title = "GHG emissions",
       x = NULL,
       y = "GHG emissions (MtCO2e)",
       color = "2045 GHG emission target",
       lty = "Policy intervention") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(ghg_pw_fig, 
       filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.png'), 
       width = 8, 
       height = 5)


## part C: cumulative GHG x 2045 reduction

## ghg
ghg_cum2_fig <- ggplot(cumul_df %>% filter(metric == "total_state_ghg_MtCO2",
                                           ccs_option == "no CCS"), aes(x = ghg_2045_perc * 100, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions relative to 2019",
       subtitle = "no CCS",
       x = "GHG emissions (% of 2019)",
       y = "GHG emissions (MtCO2e)",
       color = "GHG emission target",
       shape = "Policy intervention") +
  theme_line +
  scale_x_continuous(limits = c(NA, 0)) +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(ghg_cum2_fig, 
       filename = file.path(save_info_path, 'cumulative_v2/cumulative_ghg_v2_fig.png'), 
       width = 6, 
       height = 5)
