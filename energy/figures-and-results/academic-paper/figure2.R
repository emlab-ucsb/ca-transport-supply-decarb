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
npv_file <- 'npv_x_metric.csv'
cumulative_file <- 'state_cumulative_subset.csv'

## bring in GHG emissions target
npv_dt <- fread(paste0(main_path, fig_path, npv_file))

reduction_df <- unique(npv_dt[, .(scen_id, ghg_2045_perc_reduction)])


## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

## merge with reduction values
levels_dt <- merge(levels_dt, reduction_df,
                   by = "scen_id",
                   all.x = T)

## filter out carbon + setback
levels_dt <- levels_dt[policy_intervention != 'carbon tax & setback' & ccs_scenario == "no ccs"]

levels_dt[, target_name := fifelse(target == "BAU", "BAU",
                                    paste0(round(ghg_2045_perc_reduction), "% reduction"))]



## horizontal panel, A) production; B) GHG emissions; C) Cumulative GHG emissions x 2045 emissions reduction

prod_fig <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention != "BAU"), aes(x = year, y = value / 1e6, color = target, lty = policy_intervention)) +
  geom_line(size = 0.65, alpha = 0.9) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                 year > 2019,
                                 policy_intervention == "BAU"), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2044, y = 85, label = "BAU") +
  labs(title = "Oil production",
       x = NULL,
       y = "Oil production (million bbls)",
       color = "2045 GHG emission target",
       lty = "Policy intervention") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c("setback" = "twodash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_color_manual(values = target_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

# ggsave(prod_fig, 
#        filename = file.path(save_info_path, 'pathway/prod_x_time_fig.png'), 
#        width = 8, 
#        height = 5)

## facet wrap version
## ----------------------------------------------------------------


## ghg
ghg_pw_fig <- ggplot(levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                          year > 2019,
                                          policy_intervention != "BAU"), aes(x = year, y = value , color = target, lty = policy_intervention)) +
  geom_line(size = 0.65, alpha = 0.8) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_ghg_MtCO2",
                                        year > 2019,
                                        policy_intervention == "BAU"), aes(x = year, y = value), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  annotate("text", x = 2044, y = 9, label = "BAU") +
  labs(title = "GHG emissions",
       x = NULL,
       y = "GHG emissions (MtCO2e)",
       color = "2045 GHG emission target",
       lty = "Policy intervention") +
  # facet_wrap(~ccs_option) +
  scale_color_manual(values = target_colors) +
  scale_linetype_manual(values = c("setback" = "twodash", "carbon tax" = "dotted", "excise tax" = "dashed")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

# ggsave(ghg_pw_fig, 
#        filename = file.path(save_info_path, 'pathway/ghg_x_time_fig.png'), 
#        width = 8, 
#        height = 5)


## part C: cumulative GHG x 2045 reductions

## ghg
ghg_cumul_fig <- ggplot(cumulative_dt %>% filter(metric == "total_state_ghg_MtCO2"), 
                        aes(x = ghg_2045_perc_reduction, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Cumulative GHG emissions relative to 2019",
       x = "GHG emissions reduction in 2045 (% of 2019)",
       y = "GHG emissions (MtCO2e)",
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line +
  scale_color_manual(values = target_colors) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(NA, 0)) +
  theme(legend.position = c(0.25, 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey")) 


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

legend_pathways <- get_legend(
  prod_fig + 
    theme(legend.position = "left",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))
  
)



## part C: cumulative GHG x 2045 reduction

cumulative_dt <- fread(paste0(main_path, fig_path, cumulative_file))

## cumulative
cumulative_dt <- cumulative_dt[policy_intervention != 'carbon tax & setback' & ccs_option != "medium CCS cost"]
cumulative_dt$target <- factor(cumulative_dt$target, levels = c('BAU', '1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                                '90% GHG reduction'))



## ghg
ghg_cumul_fig <- ggplot(cumulative_dt %>% filter(metric == "total_state_ghg_MtCO2"), 
                        aes(x = ghg_2045_perc_reduction, y = sum_metric, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.9) +
  labs(title = "Cumulative GHG emissions relative to 2019",
       x = "GHG emissions reduction in 2045 (% of 2019)",
       y = "GHG emissions (MtCO2e)",
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line +
  scale_color_manual(values = c(target_colors, "BAU" = "black"),
                     guide = "none") +
  scale_shape_manual(values = policy_symbols) +
  scale_x_continuous(limits = c(0, NA)) +
  # scale_y_continuous(limits = c(NA, 0)) +
  theme(legend.position = c(0.25, 0.6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey")) 


## combine the figures
## ---------------------------------

fig2_combine <- plot_grid(
  prod_fig + theme(legend.position="none"),
  ghg_pw_fig + theme(legend.position="none"),
  legend_pathways,
  ghg_cumul_fig,
  align = 'vh',
  # labels = c("A", "B", "C"),
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1)
)


ggsave(fig2_combine,
       filename = file.path(main_path, fig_path, 'figure2.png'),
       width = 7,
       height = 7,
       units = "in")


