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
# cumulative_dt <- fread(paste0(main_path, fig_path, cumulative_file))

## add scenario
levels_dt[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]
levels_dt <- levels_dt[!scenario %in% c("historic production - LC1 demand")]
levels_dt[, scenario := paste0(demand_scenario, " demand", ' - ', refining_scenario)]
levels_dt[, scenario := str_replace(scenario, "LC1", "Low C.")]
levels_dt[, short_scen := str_replace(scenario, "LC1", "Low C.")]
levels_dt[, short_scen := str_replace(short_scen, "historic", "hist.")]

## factor
levels_dt$scenario <- factor(levels_dt$scenario, levels = c('BAU demand - historic production', 
                                                            'BAU demand - historic exports', 
                                                            'BAU demand - low exports',
                                                            'Low C. demand - historic exports',
                                                            'Low C. demand - low exports'))

## factor
levels_dt$short_scen <- factor(levels_dt$short_scen, levels = c('BAU demand - hist. production', 
                                                            'BAU demand - hist. exports', 
                                                            'BAU demand - low exports',
                                                            'Low C. demand - hist. exports',
                                                            'Low C. demand - low exports'))
## target
levels_dt[, target_label := paste0(round(ghg_2045_perc_reduction), "%")]


## figure 2, reference case 
## -----------------------------------------------

## ref case, crude consumption
crude_ref <- ggplot(levels_dt %>% filter(metric == "bbls_consumed",
                                         year > 2019,
                                         oil_price_scenario == "reference case",
                                         carbon_price_scenario == "price floor",
                                         ccs_scenario == "no ccs",
                                         refining_scenario != 'historic production'), aes(x = year, y = value / 1e6, color = scenario, lty = scenario)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "A. Crude consumption",
       x = NULL,
       y = "Barrels (million)") +
  scale_color_manual(name = "Scenario",
                      labels = c("BAU demand - historic exports", 
                                 "BAU demand - low exports", 
                                 "Low C. demand - historic exports", 
                                 "Low C. demand - low exports"),
                      values = c("#33658A", "#33658A", "#F6AE2D", "#F6AE2D")) +   
  scale_linetype_manual(name = "Scenario",
                     labels = c("BAU demand - historic exports", 
                                "BAU demand - low exports", 
                                "Low C. demand - historic exports", 
                                "Low C. demand - low exports"),
                     values = c("solid", "dashed", "solid", "dashed")) +
  geom_line(data = levels_dt %>% filter(metric == "bbls_consumed",
                                        year > 2019,
                                        oil_price_scenario == "reference case",
                                        carbon_price_scenario == "price floor",
                                        ccs_scenario == "no ccs",
                                        refining_scenario == 'historic production'), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2037, y = 580, label = "BAU demand & historic production", size = 2) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 625)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 


## legend figure
prod_fig_legend <- ggplot(levels_dt %>% filter(metric == "bbls_consumed",
                                               year > 2019,
                                               oil_price_scenario == "reference case",
                                               carbon_price_scenario == "price floor",
                                               ccs_scenario == "no ccs",
                                               refining_scenario != 'historic production'), aes(x = year, y = value / 1e6, color = scenario, lty = scenario)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "A. Crude consumption",
       x = NULL,
       y = "Barrels (million)") +
  scale_color_manual(name = "Scenario",
                     labels = c("BAU demand - historic exports", 
                                "BAU demand - low exports", 
                                "Low C. demand - historic exports", 
                                "Low C. demand - low exports"),
                     values = c("#33658A", "#33658A", "#F6AE2D", "#F6AE2D")) +   
  scale_linetype_manual(name = "Scenario",
                        labels = c("BAU demand - historic exports", 
                                   "BAU demand - low exports", 
                                   "Low C. demand - historic exports", 
                                   "Low C. demand - low exports"),
                        values = c("solid", "dashed", "solid", "dashed")) +
  # facet_wrap(~ccs_option) +
  # scale_linetype_manual(values = c("60%" = "solid", "75%" = "dashed", "90%" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "bbls_consumed",
                                        year > 2019,
                                        oil_price_scenario == "reference case",
                                        carbon_price_scenario == "price floor",
                                        ccs_scenario == "no ccs",
                                        refining_scenario == 'historic production'), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2037, y = 580, label = "BAU demand & historic production", size = 2) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 625)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  theme(legend.position = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 



## GHG
## ----------------------------------------------------------------

## v2 ghg
ghg_ref <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                             year > 2019,
                                             oil_price_scenario == "reference case",
                                             carbon_price_scenario == "price floor",
                                             ccs_scenario == "no ccs",
                                             refining_scenario != 'historic production'), aes(x = year, y = value, color = scenario, lty = scenario)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "B. GHG emissions",
       x = NULL,
       y = "Barrels (million)") +
  scale_color_manual(name = "Scenario",
                     labels = c("BAU demand - historic exports", 
                                "BAU demand - low exports", 
                                "Low C. demand - historic exports", 
                                "Low C. demand - low exports"),
                     values = c("#33658A", "#33658A", "#F6AE2D", "#F6AE2D")) +   
  scale_linetype_manual(name = "Scenario",
                        labels = c("BAU demand - historic exports", 
                                   "BAU demand - low exports", 
                                   "Low C. demand - historic exports", 
                                   "Low C. demand - low exports"),
                        values = c("solid", "dashed", "solid", "dashed")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                        year > 2019,
                                        oil_price_scenario == "reference case",
                                        carbon_price_scenario == "price floor",
                                        ccs_scenario == "no ccs",
                                        refining_scenario == 'historic production'), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2037, y = 32, label = "BAU demand & historic production", size = 2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36)) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 
  

## part C: cumulative GHG x 2045 reductions
## ---------------------------------------------------------

cumul_ghg <- levels_dt[metric == "total_state_ghg_MtCO2" & year > 2019, .(cumul_ghg = sum(value)), by = .(scen_id, oil_price_scenario, policy_intervention,
                                                                                                          ghg_2045_perc, target_label)]


cumul_ghg$target_label <- factor(cumul_ghg$target_label, levels = c("BAU", "55%",
                                                                    "60%", "75%",
                                                                    "90%"))

cumul_ghg$policy_intervention <- factor(cumul_ghg$policy_intervention, levels = c("BAU", "excise tax", "carbon tax", 
                                                                                  "setback"))

## ghg
# ghg_cumul_fig <- ggplot(cumul_ghg %>% filter(target_label != "55%"), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = target_label, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(title = "Cumulative GHG emissions",
#        x = "GHG emissions reduction target (%, 2045 vs 2019)",
#        y = "MtCO2e",
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   theme_line +
#   scale_color_manual(values = c(target_colors)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   # scale_y_continuous(limits = c(NA, 0)) +
#   theme(legend.position = c(0.25, 0.3),
#         # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.background = element_rect(fill = "white", color = "grey")) 


ghg_cumul_fig_v2 <- ggplot(cumul_ghg %>% filter(oil_price_scenario == "reference case"), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "C. Cumulative GHG emissions",
       x = "GHG emissions reduction target (%, 2045 vs 2019)",
       y = bquote(MtCO[2]~e),
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line_n +
  annotate("text", x = 55.5, y = 268, label = "BAU", size = 2) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  # scale_x_continuous(limits = c(0, NA)) +
  # scale_y_continuous(limits = c(NA, 0)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey"),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 



# ghg_cumul_fig <- ggplot(cumul_ghg, aes(x = target_label, y = cumul_ghg, color = target_label, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(title = "Cumulative GHG emissions",
#        x = "GHG emissions reduction target (%, 2045 vs 2019)",
#        y = "GHG emissions (MtCO2e)",
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   theme_line +
#   scale_color_manual(values = c(target_colors, "BAU" = "black")) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   # scale_y_continuous(limits = c(NA, 0)) +
#   theme(legend.position = c(0.25, 0.3),
#         # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.background = element_rect(fill = "white", color = "grey")) 


## combine the figures
## ---------------------------------

# fig2_combine <- plot_grid(
#   prod_fig + theme(legend.position="none"),
#   ghg_pw_fig + theme(legend.position="none"),
#   align = 'vh',
#   # labels = c("A", "B", "C"),
#   hjust = -1,
#   nrow = 1,
#   rel_widths = c(1, 1)
# )

# legend_pathways <- get_legend(
#   prod_fig + 
#     theme(legend.position = "left",
#           legend.title = element_text(size = 10),
#           legend.text = element_text(size = 8))
#   
# )


legend_pathways_v2 <- get_legend(
  prod_fig_legend + 
    theme(legend.title = element_text(size = 7),
          legend.text = element_text(size = 7))
  
)


## part C: cumulative GHG x 2045 reduction
# 
# cumulative_dt <- fread(paste0(main_path, fig_path, cumulative_file))
# 
# ## cumulative
# cumulative_dt <- cumulative_dt[policy_intervention != 'carbon tax & setback' & ccs_option != "medium CCS cost"]
# cumulative_dt$target <- factor(cumulative_dt$target, levels = c('BAU', '1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
#                                                                 '90% GHG reduction'))
# 
# 
# 
# ## ghg
# ghg_cumul_fig <- ggplot(cumulative_dt %>% filter(metric == "total_state_ghg_MtCO2"), 
#                         aes(x = ghg_2045_perc_reduction, y = sum_metric, color = target, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.9) +
#   labs(title = "Cumulative GHG emissions relative to 2019",
#        x = "GHG emissions reduction in 2045 (% of 2019)",
#        y = "GHG emissions (MtCO2e)",
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   theme_line +
#   scale_color_manual(values = c(target_colors, "BAU" = "black"),
#                      guide = "none") +
#   scale_shape_manual(values = policy_symbols) +
#   scale_x_continuous(limits = c(0, NA)) +
#   # scale_y_continuous(limits = c(NA, 0)) +
#   theme(legend.position = c(0.25, 0.6),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.background = element_rect(fill = "white", color = "grey")) 
# 

## combine the figures
## ---------------------------------
# 
# fig2_combine <- plot_grid(
#   prod_fig + theme(legend.position="none"),
#   ghg_pw_fig + theme(legend.position="none"),
#   legend_pathways,
#   ghg_cumul_fig + guides(color = "none"),
#   align = 'vh',
#   # labels = c("A", "B", "C"),
#   hjust = -1,
#   nrow = 2,
#   rel_widths = c(1, 1, 1, 1)
# )
# 
# 
# ggsave(fig2_combine,
#        filename = file.path(main_path, fig_path, 'figure2.png'),
#        width = 7,
#        height = 7,
#        units = "in")

## combine v2  figure
## ---------------------------------

fig2_v2_combine <- plot_grid(
  prod_fig_v2 + theme(legend.position="none"),
  ghg_pw_fig_v2 + theme(legend.position="none"),
  ghg_cumul_fig_v2 + theme(legend.position = "none"),
  legend_pathways_v2,
  align = 'vh',
  # labels = c("A", "B", "C", ""),
  # # labels = 'AUTO',
  label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1)
)


ggsave(fig2_v2_combine,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure2-ref-case.png'),
       width = 180,
       height = 185,
       units = "mm")

ggsave(fig2_v2_combine,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure2-ref-case.pdf'),
       width = 180,
       height = 185,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/figure2-ref-case.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/figure2-ref-case.pdf'))






























# consumed_fig <- ggplot(levels_dt %>% filter(ccs_scenario %in% c("no ccs"),
#                                                metric == "bbls_consumed",
#                                                oil_price_scenario == "reference case",
#                                                carbon_price_scenario == "price floor",
#                                                year > 2019), aes(x = year, y = value / 1e6, color = short_scen)) +
#   geom_line(size = 0.75, alpha = 0.7) +
#   labs(title = "Refinery oil and renewable feedstock consumption",
#        x = NULL,
#        y = "Oil refined (millions bbls crude oil equivalent)",
#        color = "Refinery demand") +
#   guides(colour = guide_legend(order = 1), 
#          lty = guide_legend(order = 2)) +
#   scale_color_manual(values = c('BAU demand - hist. production' = "#A84268",
#                                 'BAU demand - hist. exports' = "#ff5e5b",
#                                 'BAU demand - low exports' = '#fcb97d',
#                                 'Low C. demand - hist. exports' = '#4a6c6f',
#                                 'Low C. demand - low exports' = "#9DBF9E")) +
#   # facet_grid(demand_scenario ~ ccs_option) +
#   # scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   theme_line +
#   theme(legend.position = "none",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm')) 
# 
# 
# 
# # ggsave(consumed_fig, 
# #        filename = paste0(main_path, fig_path, 'figure2-refining.png'), 
# #        width = 8, 
# #        height = 6)
# 
# 
# ## extract the legend
# legend_pathways <- get_legend(
#   consumed_fig + theme(legend.position = "left")
#   
# )
# 
# fig2_combine <- plot_grid(
#   legend_pathways,
#   consumed_fig + theme(legend.position = "none"),
#   align = 'vh',
#   # labels = c("A", "B", "C"),
#   hjust = -1,
#   nrow = 1,
#   rel_widths = c(0.5, 1)
# )
# 
# 
# ggsave(fig2_combine,
#        filename = file.path(main_path, fig_path, 'figure2.png'),
#        width = 6.2,
#        height = 4,
#        units = "in")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # embed_fonts(file.path(save_info_path, 'pathway/prod_x_time_fig.png'),
# #             outfile = file.path(save_info_path, 'pathway/prod_x_time_fig.png'))
# 
# 
# 
# ## horizontal panel, A) production; B) GHG emissions; C) Cumulative GHG emissions x 2045 emissions reduction
# 
# prod_fig <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
#                                         year > 2019), aes(x = year, y = value / 1e6, color = target, lty = policy_intervention)) +
#   geom_line(size = 0.75, alpha = 0.8) +
#   labs(title = "Oil production",
#        x = NULL,
#        y = "Oil production (million bbls)",
#        color = "2045 GHG emission target",
#        lty = "Policy intervention") +
#   # facet_wrap(~ccs_option) +
#   scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed", "carbon tax & setback" = "longdash")) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
#   theme_bw() +
#   theme_line +
#   theme(legend.position = "left",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical",
#         legend.justification = "left")
# 
# ## extract the legend
# legend_pathways <- get_legend(
#   prod_fig
# 
# )
# # 
# # 
# # 
# # # ggsave(prod_fig, 
# # #        filename = file.path(save_info_path, 'pathway/prod_x_time_fig.png'), 
# # #        width = 8, 
# # #        height = 5)
# # 
# # 
# # 
# # ## ghg
# # ghg_pw_fig <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
# #                                           year > 2019), aes(x = year, y = value , color = target, lty = policy_intervention)) +
# #   geom_line(size = 0.75, alpha = 0.8) +
# #   labs(title = "GHG emissions",
# #        x = NULL,
# #        y = "GHG emissions (MtCO2e)",
# #        color = "2045 GHG emission target",
# #        lty = "Policy intervention") +
# #   # facet_wrap(~ccs_option) +
# #   scale_linetype_manual(values = c("setback" = "solid", "BAU" = "dotdash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
# #   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
# #   theme_line +
# #   theme(legend.position = "bottom",
# #         legend.key.width= unit(1, 'cm'),
# #         legend.box="vertical") 
# # 
# # # ggsave(ghg_pw_fig, 
# # #        filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.png'), 
# # #        width = 8, 
# # #        height = 5)
# # 
# # 
# # ## part C: cumulative GHG x 2045 reduction
# # 
# # ## ghg
# # ghg_cumul_fig <- ggplot(cumulative_dt %>% filter(metric == "total_state_ghg_MtCO2"), 
# #                         aes(x = ghg_2045_perc_reduction, y = sum_metric, color = target, shape = policy_intervention)) +
# #   geom_point(size = 2, alpha = 0.8) +
# #   labs(title = "Cumulative GHG emissions relative to 2019",
# #        x = "GHG emissions reduction in 2045 (% of 2019)",
# #        y = "GHG emissions (MtCO2e)",
# #        color = "2045 GHG emission target",
# #        shape = "Policy intervention") +
# #   theme_line +
# #   scale_x_continuous(limits = c(0, NA)) +
# #   scale_y_continuous(limits = c(NA, 0)) +
# #   theme(legend.position = c(0.25, 0.5),
# #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
# #         legend.background = element_rect(fill = "white", color = "grey")) 
# # 
# # 
# # ## combine the figures
# # ## ---------------------------------
# # 
# # fig2_combine <- plot_grid(
# #   legend_pathways,
# #   prod_fig + theme(legend.position="none"),
# #   ghg_pw_fig + theme(legend.position="none"),
# #   ghg_cumul_fig,
# #   align = 'vh',
# #   # labels = c("A", "B", "C"),
# #   hjust = -1,
# #   nrow = 1,
# #   rel_widths = c(0.4, 1, 1, 1)
# )
# 
# 
# ggsave(fig2_combine,
#        filename = file.path(main_path, fig_path, 'figure2.png'),
#        width = 13,
#        height = 4,
#        units = "in")