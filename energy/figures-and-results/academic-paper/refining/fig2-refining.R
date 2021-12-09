## Tracey Mangin
## December 6, 2021
## Academic paper figure 2 - refining

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/refining/figures/'

## csv names
levels_file <- 'state_levels_subset_refining.csv'
cumulative_file <- 'state_cumulative_subset_refining.csv'

## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_file))
cumulative_dt <- fread(paste0(main_path, fig_path, cumulative_file))


# And the last plot I need is the refining pathways. I will be adding only the refinery oil consumption plot instead of the three panel figure similar to Figure 2 for extraction.
# You have that figure on page 50. To make it consistent with the slide text and the extraction figure, I will state the changes here.
# Move the legend to the left of the plot.
# Change the y axis label to “Oil refined (millions bbls crude oil equivalent)”
# Title: “Refinery oil and renewable feedstock consumption”
# Legend title: “Refinery demand”


## add scenario
levels_dt[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]
levels_dt <- levels_dt[!scenario %in% c("historic production - LC1 demand")]
levels_dt[, scenario := paste0(demand_scenario, " demand", ' - ', refining_scenario)]
levels_dt <- levels_dt[, scenario := str_replace(scenario, "LC1", "Low carbon")]

## factor
levels_dt$scenario <- factor(levels_dt$scenario, levels = c('BAU demand - historic production', 
                                                            'BAU demand - historic exports', 
                                                            'BAU demand - low exports',
                                                            'Low carbon demand - historic exports',
                                                            'Low carbon demand - low exports'))



consumed_fig <- ggplot(levels_dt %>% filter(ccs_scenario %in% c("no ccs"),
                                               metric == "bbls_consumed",
                                               oil_price_scenario == "reference case",
                                               carbon_price_scenario == "price floor",
                                               year > 2019), aes(x = year, y = value / 1e6, color = scenario)) +
  geom_line(size = 0.75, alpha = 0.7) +
  labs(title = "Refinery oil and renewable feedstock consumption",
       x = NULL,
       y = "Oil refined (millions bbls crude oil equivalent)",
       color = "Refinery demand") +
  guides(colour = guide_legend(order = 1), 
         lty = guide_legend(order = 2)) +
  scale_color_manual(values = c('BAU demand - historic exports' = "#ff5e5b",
                                'BAU demand - low exports' = '#fcb97d',
                                'BAU demand - historic production' = "#A84268",
                                'Low carbon demand - historic exports' = '#4a6c6f',
                                'Low carbon demand - low exports' = "#9DBF9E")) +
  # facet_grid(demand_scenario ~ ccs_option) +
  # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        plot.title = element_text(hjust = 1)) 

ggsave(consumed_fig, 
       filename = paste0(main_path, fig_path, 'figure2-refining.png'), 
       width = 8, 
       height = 6)

# embed_fonts(file.path(save_info_path, 'pathway/prod_x_time_fig.png'),
#             outfile = file.path(save_info_path, 'pathway/prod_x_time_fig.png'))



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
  theme_bw() +
  theme_line +
  theme(legend.position = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        legend.justification = "left")

## extract the legend
legend_pathways <- get_legend(
  prod_fig

)
# 
# 
# 
# # ggsave(prod_fig, 
# #        filename = file.path(save_info_path, 'pathway/prod_x_time_fig.png'), 
# #        width = 8, 
# #        height = 5)
# 
# 
# 
# ## ghg
# ghg_pw_fig <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
#                                           year > 2019), aes(x = year, y = value , color = target, lty = policy_intervention)) +
#   geom_line(size = 0.75, alpha = 0.8) +
#   labs(title = "GHG emissions",
#        x = NULL,
#        y = "GHG emissions (MtCO2e)",
#        color = "2045 GHG emission target",
#        lty = "Policy intervention") +
#   # facet_wrap(~ccs_option) +
#   scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   theme_line +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical") 
# 
# # ggsave(ghg_pw_fig, 
# #        filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.png'), 
# #        width = 8, 
# #        height = 5)
# 
# 
# ## part C: cumulative GHG x 2045 reduction
# 
# ## ghg
# ghg_cumul_fig <- ggplot(cumulative_dt %>% filter(metric == "total_state_ghg_MtCO2"), 
#                         aes(x = ghg_2045_perc_reduction, y = sum_metric, color = target, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(title = "Cumulative GHG emissions relative to 2019",
#        x = "GHG emissions reduction in 2045 (% of 2019)",
#        y = "GHG emissions (MtCO2e)",
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   theme_line +
#   scale_x_continuous(limits = c(0, NA)) +
#   scale_y_continuous(limits = c(NA, 0)) +
#   theme(legend.position = c(0.25, 0.5),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.background = element_rect(fill = "white", color = "grey")) 
# 
# 
# ## combine the figures
# ## ---------------------------------
# 
# fig2_combine <- plot_grid(
#   legend_pathways,
#   prod_fig + theme(legend.position="none"),
#   ghg_pw_fig + theme(legend.position="none"),
#   ghg_cumul_fig,
#   align = 'vh',
#   # labels = c("A", "B", "C"),
#   hjust = -1,
#   nrow = 1,
#   rel_widths = c(0.4, 1, 1, 1)
# )
# 
# 
# ggsave(fig2_combine,
#        filename = file.path(main_path, fig_path, 'figure2.png'),
#        width = 13,
#        height = 4,
#        units = "in")