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
library(extrafont)

## define if you are using zenodo repo for inputs 
# input_loc <- "zenodo"
input_loc <- "emlab"

## if using zenodo, define location to save outputs
zenodo_save_path


## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## set paths
if(input_loc == "zenodo") {
  
  ## input path
  main_path <- 'ca-transport-supply-decarb-files/outputs/fig-and-results-out/'
  ## save path
  fig_path <- zenodo_save_path
  
} else {

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/final/'

}

## csv names
levels_name <- 'state_levels_all_oil.csv'

## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

## filter out carbon + setback
levels_dt <- levels_dt[!policy_intervention %in% c('carbon tax & setback', 'excise tax & setback')]
levels_dt <- levels_dt[, target_label := fifelse(target_label == "no_target", "BAU", target_label)]

## update setback policy intervention (if setback_existing == 1, all wells)
levels_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                              setback_existing == 1), "setback (all wells)",
                                           policy_intervention)]

levels_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                              setback_existing == 0), "setback (new wells)",
                                           policy_intervention)]

# ## remove duplicate of 90% reduction and BAU
# levels_dt <- levels_dt[!(target == "90perc_reduction" & setback_existing == 0)]
# levels_dt <- levels_dt[!(target == "no_target" & setback_existing == 0)]

## horizontal panel, A) production; B) GHG emissions; C) Cumulative GHG emissions x 2045 emissions reduction

# prod_fig <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
#                                         year > 2019,
#                                         policy_intervention != "BAU",
#                                         target_label != "55%"), aes(x = year, y = value / 1e6, color = target_label, lty = policy_intervention)) +
#   geom_line(size = 0.65, alpha = 0.9) +
#   geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
#                                  year > 2019,
#                                  policy_intervention == "BAU"), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
#   annotate("text", x = 2044, y = 85, label = "BAU") +
#   labs(title = "Oil production",
#        x = NULL,
#        y = "Oil production (million bbls)",
#        color = "2045 GHG emission target",
#        lty = "Policy intervention") +
#   # facet_wrap(~ccs_option) +
#   scale_linetype_manual(values = c("setback" = "twodash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
#   scale_color_manual(values = target_colors) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
#   theme_line +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical") 

## version 2, categorical colors for policy
prod_fig_v2 <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        oil_price_scenario == "reference case",
                                        target != "setback_1000ft",
                                        setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = target_label)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "2045 GHG emission target") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c(
                                   "65%" = "solid",
                                   # "66%" = "solid",
                                   "72%" = "dashed",
                                   # "81%" = "dashed", 
                                   "90%" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 74, label = "BAU", size = 2) +
  scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n_l +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

# ## other version: setbacks only apply to new wells
# prod_fig_v2_sb <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
#                                            year > 2019,
#                                            oil_price_scenario == "reference case",
#                                            target_label != "62%",
#                                            setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = target_label)) +
#   geom_line(size = 0.65, alpha = 0.9) +
#   labs(title = "A. Oil production",
#        x = NULL,
#        y = "Barrels (million)",
#        color = "Policy",
#        lty = "2045 GHG emission target") +
#   # facet_wrap(~ccs_option) +
#   scale_linetype_manual(values = c("65%" = "solid", "72%" = "dashed", "90%" = "dotted")) +
#   geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
#                                         year > 2019,
#                                         policy_intervention == "BAU",
#                                         oil_price_scenario == "reference case",
#                                         setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
#   
#   annotate("text", x = 2044, y = 74, label = "BAU", size = 2) +
#   scale_color_manual(values = policy_colors_subset) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
#   # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
#   theme_line_n +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical",
#         axis.ticks.length.y = unit(0.1, 'cm'),
#         axis.ticks.length.x = unit(0.1, 'cm')) 
# 

## for joint legend:
## version 2, categorical colors for policy
levels_dt_legend <- levels_dt %>%
  mutate(legend_lab = ifelse(target_label %in% c("15%", "16%", "65%", "66%", "85%", "88%"), paste0(target_label, " (= 2,500 ft setback)"),
                             ifelse(target_label %in% c("33%", "35%", "72%", "81%", "89%", "94%"), paste0(target_label, " (= 1 mile setback)"), target_label)))

## legend figure
prod_fig_legend <- ggplot(levels_dt_legend %>% filter(metric == "total_state_bbl",
                                           year > 2019,
                                           target != "setback_1000ft",
                                           oil_price_scenario == "reference case",
                                           setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = legend_lab)) +
  geom_line(size = 0.65, alpha = 0.9) +
  geom_point() +
  geom_line(data = levels_dt_legend %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2044, y = 85, label = "BAU", size = 2) +
  labs(title = "(a) Oil production",
       x = NULL,
       y = "million bbls",
       color = "Policy",
       lty = "2045 GHG emission reduction target") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c("65% (= 2,500 ft setback)" = "solid",
                                   # "66%" = "solid",
                                   "72% (= 1 mile setback)" = "dashed",
                                   # "81%" = "dashed", 
                                   "90%" = "dotted")) +
  scale_color_manual(values = c(policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n_l +
  guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2)) +
  theme(legend.position = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

# ## for joint legend:
# ## version 2, categorical colors for policy
# levels_dt_legend_sb <- levels_dt %>%
#   mutate(legend_lab = ifelse(target_label %in% c("15%", "65%", "85%"), paste0(target_label, " (= 2,500 ft setback)"),
#                              ifelse(target_label %in% c("33%", "89%", "72%"), paste0(target_label, " (= 1 mile setback)"), target_label)))
# 
# ## legend figure
# prod_fig_legend_sb <- ggplot(levels_dt_legend %>% filter(metric == "total_state_bbl",
#                                                       year > 2019,
#                                                       target != "setback_1000ft",
#                                                       oil_price_scenario == "reference case"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = legend_lab)) +
#   geom_line(size = 0.65, alpha = 0.9) +
#   geom_point() +
#   geom_line(data = levels_dt_legend %>% filter(metric == "total_state_bbl",
#                                                year > 2019,
#                                                policy_intervention == "BAU",
#                                                oil_price_scenario == "reference case",
#                                                setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
#   annotate("text", x = 2044, y = 85, label = "BAU", size = 2) +
#   labs(title = "(A) Oil production",
#        x = NULL,
#        y = "million bbls",
#        color = "Policy",
#        lty = "2045 GHG emission reduction target") +
#   # facet_wrap(~ccs_option) +
#   scale_linetype_manual(values = c("65% (= 2,500 ft setback)" = "solid", "72% (= 1 mile setback)" = "dashed", "90%" = "dotted")) +
#   scale_color_manual(values = c(policy_colors_subset)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
#   # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
#   theme_line_n +
#   guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2)) +
#   theme(legend.position = "left",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical",
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 8),
#         axis.ticks.length.y = unit(0.1, 'cm'),
#         axis.ticks.length.x = unit(0.1, 'cm')) 
# 

## GHG
## ----------------------------------------------------------------

# 
# ## ghg
# ghg_pw_fig <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
#                                           year > 2019,
#                                           policy_intervention != "BAU"), aes(x = year, y = value , color = target_label, lty = policy_intervention)) +
#   geom_line(size = 0.65, alpha = 0.8) +
#   geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
#                                         year > 2019,
#                                         policy_intervention == "BAU"), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
#   annotate("text", x = 2044, y = 9, label = "BAU") +
#   labs(title = "GHG emissions",
#        x = NULL,
#        y = "GHG emissions (MtCO2e)",
#        color = "2045 GHG emission target",
#        lty = "Policy intervention") +
#   # facet_wrap(~ccs_option) +
#   scale_color_manual(values = target_colors) +
#   scale_linetype_manual(values = c("setback" = "twodash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
#   theme_line +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical") 

## v2 ghg
ghg_pw_fig_v2 <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                          year > 2019,
                                          policy_intervention != "BAU",
                                          oil_price_scenario == "reference case",
                                          target != "setback_1000ft",
                                          setback_existing == 0), aes(x = year, y = value , color = policy_intervention, lty = target_label)) +
  geom_line(size = 0.65, alpha = 0.8) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2044, y = 7.5, label = "BAU", size = 2) +
  labs(title = "b. GHG emissions",
       x = NULL,
       # y = "MtCO2e",
       y = bquote(MtCO[2]~e),
       color = "Policy intervention",
       lty = "2045 GHG emission target") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values =  c("65%" = "solid",
                                    # "66%" = "solid",
                                    "72%" = "dashed",
                                    # "81%" = "dashed", 
                                    "90%" = "dotted")) +
  scale_color_manual(values = c(policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme_line_n_l +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

# ghg_pw_fig_v2_sb <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
#                                              year > 2019,
#                                              policy_intervention != "BAU",
#                                              oil_price_scenario == "reference case",
#                                              target_label != "63%",
#                                              setback_existing == 0
#                                              ), aes(x = year, y = value , color = policy_intervention, lty = target_label)) +
#   geom_line(size = 0.65, alpha = 0.8) +
#   geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
#                                         year > 2019,
#                                         policy_intervention == "BAU",
#                                         oil_price_scenario == "reference case",
#                                         setback_existing == 0), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
#   annotate("text", x = 2044, y = 7.5, label = "BAU", size = 2) +
#   labs(title = "B. GHG emissions",
#        x = NULL,
#        # y = "MtCO2e",
#        y = bquote(MtCO[2]~e),
#        color = "Policy intervention",
#        lty = "2045 GHG emission target") +
#   # facet_wrap(~ccs_option) +
#   scale_linetype_manual(values = c("65%" = "solid", "72%" = "dashed", "90%" = "dotted")) +
#   scale_color_manual(values = policy_colors_subset) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
#   theme_line_n +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         legend.box="vertical",
#         axis.ticks.length.y = unit(0.1, 'cm'),
#         axis.ticks.length.x = unit(0.1, 'cm')) 
# 
# 

## part C: cumulative GHG x 2045 reductions
## ---------------------------------------------------------

cumul_ghg <- levels_dt[(metric == "total_state_ghg_MtCO2" & 
                         year > 2019), .(cumul_ghg = sum(value)), by = .(scen_id, setback_existing, oil_price_scenario,  policy_intervention,
                                                                                                         ghg_2045_perc, target_label)]


cumul_ghg$target_label <- factor(cumul_ghg$target_label, levels = c("BAU", "62%",
                                                                    "63%", "65%",
                                                                    "66%", "72%", 
                                                                    "81%", "90%"))

cumul_ghg$policy_intervention <- factor(cumul_ghg$policy_intervention, levels = c("BAU", "excise tax", "carbon tax", 
                                                                    "setback (all wells)", "setback (new wells)"))

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


ghg_cumul_fig_v2 <- ggplot(cumul_ghg %>% 
                             filter(oil_price_scenario == "reference case" & setback_existing == 0), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "c. Cumulative GHG emissions",
       x = "GHG emissions reduction target (%, 2045 vs 2019)",
       y = bquote(MtCO[2]~e),
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line_n_l +
  annotate("text", x = 59, y = 255, label = "BAU", size = 2) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(150, 270)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey"),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

# ## setback version
# cumul_ghg_sb <- levels_dt[metric == "total_state_ghg_MtCO2" & 
#                          year > 2019 & setback_existing == 0, .(cumul_ghg = sum(value)), by = .(scen_id, oil_price_scenario,  policy_intervention,
#                                                                                                 ghg_2045_perc, target_label)]
# 
# 
# cumul_ghg_sb$target_label <- factor(cumul_ghg$target_label, levels = c("BAU", "62%",
#                                                                     "65%", "72%",
#                                                                     "90%"))
# 
# cumul_ghg_sb$policy_intervention <- factor(cumul_ghg$policy_intervention, levels = c("BAU", "excise tax", "carbon tax", 
#                                                                                   "setback"))
# 
# ghg_cumul_fig_v2_sb <- ggplot(cumul_ghg_sb %>% filter(oil_price_scenario == "reference case"), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(title = "C. Cumulative GHG emissions",
#        x = "GHG emissions reduction target (%, 2045 vs 2019)",
#        y = bquote(MtCO[2]~e),
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   theme_line_n +
#   annotate("text", x = 59, y = 255, label = "BAU", size = 2) +
#   scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_y_continuous(limits = c(150, 270)) +
#   theme(legend.position = "none",
#         # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.background = element_rect(fill = "white", color = "grey"),
#         axis.ticks.length.y = unit(0.1, 'cm'),
#         axis.ticks.length.x = unit(0.1, 'cm')) 


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

# legend_pathways_v2_sb <- get_legend(
#   prod_fig_legend_sb + 
#     theme(legend.title = element_text(size = 7),
#           legend.text = element_text(size = 7))
#   
# )



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
  label_size = 7,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1)
)


ggsave(fig2_v2_combine,
       filename = paste0(main_path, fig_path, 'figs/figure2-ref-case.png'),
       width = 180,
       height = 185,
       units = "mm")

ggsave(fig2_v2_combine,
       filename = paste0(main_path, fig_path, 'figs/figure2-ref-case.pdf'),
       width = 180,
       height = 185,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/figure2-ref-case.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/figure2-ref-case.pdf'))



ggsave(fig2_v2_combine,
       filename = paste0(main_path, fig_path, 'figs/figure2-ref-case-test.pdf'),
       width = 88,
       height = 95,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/figure2-ref-case-test.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/figure2-ref-case-test.pdf'))

# ##
# fig2_v2_combine_sb <- plot_grid(
#   prod_fig_v2_sb + theme(legend.position="none"),
#   ghg_pw_fig_v2_sb + theme(legend.position="none"),
#   ghg_cumul_fig_v2_sb + theme(legend.position = "none"),
#   legend_pathways_v2_sb,
#   align = 'vh',
#   # labels = c("A", "B", "C", ""),
#   # # labels = 'AUTO',
#   label_size = 10,
#   hjust = -1,
#   nrow = 2,
#   rel_widths = c(1, 1, 1, 1)
# )
# 
# 
# ggsave(fig2_v2_combine_sb,
#        filename = file.path(main_path, fig_path, 'figs/figure2-ref-case-sb.png'),
#        width = 180,
#        height = 185,
#        units = "mm")
# 
# ggsave(fig2_v2_combine_sb,
#        filename = file.path(main_path, fig_path, 'figs/figure2-ref-case-sb.pdf'),
#        width = 180,
#        height = 185,
#        units = "mm",
#        device = 'pdf')
# 
# embed_fonts(paste0(main_path, fig_path, 'figs/figure2-ref-case-sb.pdf'),
#             outfile = paste0(main_path, fig_path, 'figs/figure2-ref-case-sb.pdf'))
# 
# 

## --------------------------------------------------------------------
## all oil prices
## --------------------------------------------------------------------


prod_fig_low <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                           year > 2019,
                                           oil_price_scenario == "low oil price",
                                           target != "setback_1000ft",
                                           setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = target_label)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "2045 GHG emission target") +
  scale_linetype_manual(values = c("85%" = "solid",
                                   "89%" = "dashed",
                                   "90%" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "low oil price",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2042, y = 45, label = "BAU - low price", size = 2) +
  scale_color_manual(values = c(policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 


## v2 ghg
ghg_pw_low <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                          year > 2019,
                                          policy_intervention != "BAU",
                                          oil_price_scenario == "low oil price",
                                          target != "setback_1000ft",
                                          setback_existing == 0), aes(x = year, y = value , color = policy_intervention, lty = target_label)) +
  geom_line(size = 0.65, alpha = 0.8) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "low oil price"), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2042.5, y = 4.7, label = "BAU - low price", size = 2) +
  labs(title = "b. GHG emissions",
       x = NULL,
       y = bquote(MtCO[2]~e),
       color = "Policy intervention",
       lty = "2045 GHG emission target") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c("85%" = "solid",
                                   "89%" = "dashed",
                                   "90%" = "dotted")) +
  scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

ghg_cumul_low <- ggplot(cumul_ghg %>% 
                          filter(oil_price_scenario == "low oil price" & setback_existing == 0), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "c. Cumulative GHG emissions",
       x = "GHG emissions reduction target (%, 2045 vs 2019)",
       y = bquote(MtCO[2]~e),
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line_n +
  annotate("text", x = 83, y = 198, label = "BAU - low price", size = 2) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(100, 200)) +
  # scale_x_continuous(limits = c(0, NA)) +
  # scale_y_continuous(limits = c(NA, 0)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey"),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## label
legend_low <- ggplot(levels_dt_legend %>% filter(metric == "total_state_bbl",
                                          year > 2019,
                                          oil_price_scenario == "low oil price",
                                          setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = legend_lab)) +
  geom_line(size = 0.65, alpha = 0.9) +
  geom_point() +
  labs(title = "Oil production",
       x = NULL,
       y = "a. Barrels (million)",
       color = "Policy",
       lty = "2045 GHG emission reduction target") +
  scale_linetype_manual(values = c("85% (= 2,500 ft setback)" = "solid", "89% (= 1 mile setback)" = "dashed", "90%" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "low oil price"), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 44, label = "BAU - low price", size = 2) +
  scale_color_manual(values = c(policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2)) +
  theme(legend.position = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

legend_pathways_low <- get_legend(
  legend_low + 
    theme(legend.title = element_text(size = 7),
          legend.text = element_text(size = 7))
  
)  
  

## combine v2  figure - low oil px
## ---------------------------------

title_low <- ggdraw() + 
  draw_label(
    "Low oil price",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fig2_v2_combine_low <- plot_grid(
  prod_fig_low + theme(legend.position="none"),
  ghg_pw_low + theme(legend.position="none"),
  ghg_cumul_low + theme(legend.position = "none"),
  legend_pathways_low,
  align = 'vh',
  # labels = c("A", "B", "C", ""),
  # label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1)
)


low_px_fig <- plot_grid(
  title_low, fig2_v2_combine_low,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.025, 1)
)


ggsave(low_px_fig,
       filename = file.path(main_path, fig_path, 'figs/figure2-low.png'),
       width = 180,
       height = 185,
       units = "mm")

ggsave(low_px_fig,
       filename = file.path(main_path, fig_path, 'figs/figure2-low.pdf'),
       width = 180,
       height = 185,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/figure2-low.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/figure2-low.pdf'))



## high oil px
## ---------------------------------------------------------------


prod_fig_high <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                            year > 2019,
                                            oil_price_scenario == "high oil price",
                                            target != "setback_1000ft",
                                            setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = target_label)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "2045 GHG emission target") +
  scale_linetype_manual(values = c("15%" = "solid", 
                                   "33%" = "dashed", 
                                   "90%" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "high oil price",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2040, y = 215, label = "BAU - high price", size = 2) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 


## v2 ghg
ghg_pw_high <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                          year > 2019,
                                          policy_intervention != "BAU",
                                          oil_price_scenario == "high oil price",
                                          target != "setback_1000ft",
                                          setback_existing == 0), aes(x = year, y = value , color = policy_intervention, lty = target_label)) +
  geom_line(size = 0.65, alpha = 0.8) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "high oil price",
                                        setback_existing == 0), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2040, y = 21, label = "BAU - high price", size = 2) +
  labs(title = "GHG emissions",
       x = NULL,
       y = bquote(MtCO[2]~e),
       color = "Policy intervention",
       lty = "2045 GHG emission target") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c("15%" = "solid", 
                                   "33%" = "dashed", 
                                   "90%" = "dotted")) +
  scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

ghg_cumul_high <- ggplot(cumul_ghg %>% 
                           filter(oil_price_scenario == "high oil price" & setback_existing == 0), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions",
       x = "GHG emissions reduction target (%, 2045 vs 2019)",
       y = bquote(MtCO[2]~e),
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line_n +
  annotate("text", x = 10, y = 443, label = "BAU - high price", size = 2) +
  scale_y_continuous(expand = c(0, 0), limits = c(140, 450)) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  # scale_x_continuous(limits = c(0, NA)) +
  # scale_y_continuous(limits = c(NA, 0)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey"),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

## label
legend_high <- ggplot(levels_dt_legend %>% filter(metric == "total_state_bbl",
                                           year > 2019,
                                           oil_price_scenario == "high oil price",
                                           target != "setback_1000ft",
                                           setback_existing == 0), aes(x = year, y = value / 1e6, color = policy_intervention, lty = legend_lab)) +
  geom_line(size = 0.65, alpha = 0.9) +
  geom_point() +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "2045 GHG emission reduction target") +
  scale_linetype_manual(values = c("15% (= 2,500 ft setback)" = "solid", 
                                   "33% (= 1 mile setback)" = "dashed", 
                                   "90%" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "high oil price",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2045, y = 170, label = "BAU", size = 2) +
  scale_color_manual(values = c(policy_colors_subset)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2)) +
  theme(legend.position = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

legend_pathways_high <- get_legend(
  legend_high + 
    theme(legend.title = element_text(size = 7),
          legend.text = element_text(size = 7))
  
)  





## combine v2  figure - low oil px
## ---------------------------------

title_high <- ggdraw() + 
  draw_label(
    "High oil price",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fig2_v2_combine_high <- plot_grid(
  prod_fig_high + theme(legend.position="none"),
  ghg_pw_high + theme(legend.position="none"),
  ghg_cumul_high + theme(legend.position = "none"),
  legend_pathways_high,
  align = 'vh',
  # labels = c("A", "B", "C", ""),
  # label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1)
)


high_px_fig <- plot_grid(
  title_high, fig2_v2_combine_high,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.025, 1)
)


ggsave(high_px_fig,
       filename = file.path(main_path, fig_path, 'figs/figure2-high.png'),
       width = 180,
       height = 185,
       units = "mm")

ggsave(high_px_fig,
       filename = file.path(main_path, fig_path, 'figs/figure2-high.pdf'),
       width = 180,
       height = 185,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/figure2-high.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/figure2-high.pdf'))

