## Tracey Mangin
## December 6, 2021
## Academic paper figure 3

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
fig_path <- 'outputs/academic-out/extraction/figures/all-oil-px/'

## csv names
# levels_file <- 'state_levels_subset.csv'
npv_file <- 'npv_x_metric_all_oil.csv'
dac_file <- 'dac_health_labor_all_oil.csv' ## DAC shares not relative to BAU
dac_bau_file <- 'dac_bau_health_labor_all_oil.csv' ## DAC shares relative to BAU
dac_pop_file <- 'state_dac_ratios.csv'
# carbon_px_file <- "final_carbon_tax_scenarios.csv"


## read in data
npv_dt <- fread(paste0(main_path, fig_path, npv_file))
dac_dt <- fread(paste0(main_path, fig_path, dac_file))
dac_bau_dt <- fread(paste0(main_path, fig_path, dac_bau_file))
dac_pop_dt <- fread(paste0(main_path, fig_path, dac_pop_file))
# carbon_px <- fread(paste0("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/", carbon_px_file))

## cumulative
npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'oil_price_scenario', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction', 'target_label'),
                     measure.vars = c("value_billion", "value_per_ghg_million"),
                     variable.name = "unit",
                     value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2020 USD billion)", "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)")]

npv_dt[, measure_unit := fifelse(unit == "value_billion", "2020 USD billion", "2020 USD million / MtCO2e")]


npv_dt <- npv_dt[target != 'no_target']

npv_dt$target <- factor(npv_dt$target, levels = c('setback_1000ft', 'setback_2500ft', 'setback_5280ft',
                                                        '90perc_reduction'))



# npv_dt$target_label <- factor(npv_dt$target_label, levels = c("55%", "60%", "75%", "90%"))
# 
npv_dt$policy_intervention <- factor(npv_dt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                                  "setback", "carbon tax & setback", "excise tax & setback"))

# ## bind with dac
# npv_dt_bind <- npv_dt[, .(scen_id, ccs_option, policy_intervention, title, ghg_2045_perc_reduction,
#                           measure, value)]
# 
# dac_bau_dt_bind <- dac_bau_dt[metric %in% c("dac_share_pv", "dac_share_av_pv") &
#                                 type == "DAC share", .(scen_id, ccs_option, policy_intervention, category, ghg_2045_perc_reduction,
#                                   type, value, metric)]
# 
# dac_bau_dt_bind[, metric := NULL]
# 
# setnames(dac_bau_dt_bind, c("category", "type"), c("title", "measure"))
# 
# dac_bau_dt_bind[, title := fifelse(title == "Avoided mortalities", "Health: Avoided mortality", "Labor: Compensation")]
# 
# ## rbind
# npv_dac_dt <- rbind(npv_dt_bind, dac_bau_dt_bind)
# 
# npv_dac_dt$measure <- factor(npv_dac_dt$measure, levels = c("NPV (2020 USD billion)",
#                                                             "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)",
#                                                             "DAC share"))




# fig
fig_benefit_x_metric <- ggplot(npv_dt %>% filter(target != 'BAU',
                                                 oil_price_scenario == "reference case",
                                                 !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback')), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       y = NULL,
       x = "GHG emissions reduction target (%, 2045 vs 2019)",) +
  facet_grid(measure ~ title, scales = "free") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust = 1)) 

## revised version, make them separately
## -------------------------------------------------------------------
fig_bxm_a <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV (2020 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "Health: avoided mortality",
       y = "NPV (2020 USD billion)",
       x = NULL) +
  ylim(0, 3) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.5, hjust = 1)) 

fig_bxm_b <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Labor: forgone wages",
                                      measure == "NPV (2020 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "Labor: forgone wages",
       y = NULL,
       x = NULL) +
  ylim(-15, 0) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.5, hjust = 1)) 

fig_bxm_c <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Climate: avoided damage",
                                      measure == "NPV (2020 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "Climate: avoided climate damage",
       y = NULL,
       x = NULL) +
  ylim(0, 8) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(vjust = 0.5, hjust = 1)) 

bquotelab <- bquote(MtCO[2]~e)

fig_bxm_d <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "",
       # y = expression(NPV~(2020~USD~million)~per~avoied~GHG ~MtCO[2]~e),
       y = bquote('NPV (2020 USD million)\nper avoided GHG MtCO'[2]~e),
       # y = bquote('NPV per avoided GHG MtCO'[2]~e'\n(2020 USD million / MtCO'[2]~e')'),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 1))

fig_bxm_e <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Labor: forgone wages",
                                      measure == "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 1))

fig_bxm_f <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Climate: avoided damage",
                                      measure == "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 1))

## extract legend
legend_fig <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Labor: forgone wages",
                                      measure == "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 1))

legend_fig_3 <- get_legend(
  legend_fig + 
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))
  
)


## combine figure
## ---------------------------------

## shared x axis
xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 10)

fig3_plot_grid <- plot_grid(
  fig_bxm_a,
  fig_bxm_b,
  fig_bxm_c,
  fig_bxm_d + labs(x = NULL),
  fig_bxm_e + labs(x = NULL),
  fig_bxm_f+ labs(x = NULL),
  align = 'vh',
  labels = c("A", "B", "C", "D", "E", "F"),
  # # labels = 'AUTO',
  label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1, 1, 1)
)

fig3_plot_grid2 <- plot_grid(
  fig3_plot_grid,
  xaxis_lab,
  legend_fig_3,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.05, 0.05)
  # rel_widths = c(1, 1),
)


## save figure 3
ggsave(fig3_plot_grid2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure3-ref-case.png'),
       width = 8,
       height = 6,
       units = "in")

ggsave(fig3_plot_grid2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure3-ref-case.pdf'),
       width = 8,
       height = 6,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/figure3-ref-case.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/figure3-ref-case.pdf'))




# ## ---------------------------------------------
# ## v2 
# ## ---------------------------------------------
# 
# fig3_plot_grid_v2 <- plot_grid(
#   fig_bxm_a,
#   fig_bxm_b,
#   fig_bxm_c,
#   fig_bxm_d + labs(x = NULL),
#   fig_bxm_e + labs(x = NULL) + theme(legend.position = "none"),
#   legend_fig_3,
#   align = 'vh',
#   # labels = c("(A)", "(B)", "(C)", ""),
#   # # labels = 'AUTO',
#   # label_size = 10,
#   hjust = -1,
#   nrow = 2,
#   rel_widths = c(1, 1, 1, 1, 1, 1)
# )
# 
# fig3_plot_grid2_v2 <- plot_grid(
#   fig3_plot_grid_v2,
#   xaxis_lab,
#   align = "v",
#   # labels = c("(A)", "(B)", "(C)", ""),
#   # # labels = 'AUTO',
#   # label_size = 10,
#   # hjust = -1,
#   ncol = 1,
#   rel_heights = c(1, 0.05)
#   # rel_widths = c(1, 1),
# )
# 
# 
# ## save figure 3
# ggsave(fig3_plot_grid2_v2,
#        filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure3-ref-case-v2.png'),
#        width = 8,
#        height = 6,
#        units = "in")




## high and low
## ----------------------------------------------
fig_benefit_x_metric_low <- ggplot(npv_dt %>% filter(target != 'BAU',
                                                 oil_price_scenario == "low oil price",
                                                 !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback')), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "Low oil price",
       color = "Policy intervention",
       y = NULL,
       x = "GHG emissions reduction target (%, 2045 vs 2019)",) +
  facet_grid(measure ~ title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust = 1)) 

## save figure 3
ggsave(fig_benefit_x_metric_low,
       filename = file.path(main_path, fig_path, 'figs/figure3-low.png'),
       width = 7.5,
       height = 7.5,
       units = "in")

## high
fig_benefit_x_metric_high <- ggplot(npv_dt %>% filter(target != 'BAU',
                                                     oil_price_scenario == "high oil price",
                                                     !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback')), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "High oil price",
       color = "Policy intervention",
       y = NULL,
       x = "GHG emissions reduction target (%, 2045 vs 2019)",) +
  facet_grid(measure ~ title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust = 1)) 

## save figure 3
ggsave(fig_benefit_x_metric_high,
       filename = file.path(main_path, fig_path, 'figs/figure3-high.png'),
       width = 7.5,
       height = 7.5,
       units = "in")




## figure 4: DAC, two versions
## -----------------------------------------

## version 1: relative to BAU
fig_dac_bau_h <- ggplot(dac_bau_dt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                            oil_price_scenario == "reference case",
                                            type == "DAC share",
                                            metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                        mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                  ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Health: avoided mortalities"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       y = "DAC share",
       x = NULL) +
       # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.25, 0.35)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 

fig_dac_bau_l <- ggplot(dac_bau_dt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                            oil_price_scenario == "reference case",
                                            type == "DAC share",
                                            metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                        mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                  ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                        filter(facet_lab == "Labor: forgone wages"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       y = "DAC share",
       x = NULL) +
  facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.3, 0.45)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1))

## plot them together
## -------------------------------

fig4_plot_grid <- plot_grid(
  fig_dac_bau_h + theme(legend.position = "none"),
  fig_dac_bau_l,
  align = 'vh',
  labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 1,
  rel_widths = c(1, 1)
)

fig4_plot_grid2 <- plot_grid(
  fig4_plot_grid,
  xaxis_lab,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.025)
  # rel_widths = c(1, 1),
)

fig4_plot_grid2 <- plot_grid(
  fig4_plot_grid2,
  NULL,
  legend_fig_3,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.025, 0.05)
  # rel_widths = c(1, 1),
)


## save figure 4, v1
ggsave(fig4_plot_grid2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure4-refcase-relBAU.png'),
       width = 6,
       height = 3.5,
       units = "in")

ggsave(fig4_plot_grid2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure4-refcase-relBAU.pdf'),
       width = 6,
       height = 3.5,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/figure4-refcase-relBAU.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/figure4-refcase-relBAU.pdf'))




## High and low
## ---------------------------

## high and low
fig_dac_bau_low <- ggplot(dac_bau_dt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                            oil_price_scenario == "low oil price",
                                            type == "DAC share",
                                            metric %in% c("dac_share_pv", "dac_share_av_pv")), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy intervention",
       y = "DAC share",
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  facet_wrap(~category, ncol = 2, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 

## save figure 4, v1
ggsave(fig_dac_bau_low,
       filename = file.path(main_path, fig_path, 'figs/figure4-low-relBAU.png'),
       width = 5.5,
       height = 3,
       units = "in")

fig_dac_bau_high <- ggplot(dac_bau_dt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                                oil_price_scenario == "high oil price",
                                                type == "DAC share",
                                                metric %in% c("dac_share_pv", "dac_share_av_pv")), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy intervention",
       y = "DAC share",
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  facet_wrap(~category, ncol = 2, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 

## save figure 4, v1
ggsave(fig_dac_bau_high,
       filename = file.path(main_path, fig_path, 'figs/figure4-low-relBAU.png'),
       width = 5.5,
       height = 3,
       units = "in")



## version 2, not relative
## -----------------------------------------------------

## 2019 DAC line
dac_line <- dac_pop_dt %>% 
  filter(year == 2019) %>%
  select(pop_type, year, dac_ratio) %>%
  mutate(category = ifelse(pop_type == "ct_pop_all", "Employment", "Mortality"),
         measure = "DAC share",
         text = paste0("2019 DAC ratio = ", round(dac_ratio, 2)))

reduction_df <- tibble(ghg_2045_perc_reduction = c(100, 100, 50, 50),
                       category = c("Employment", "Mortality", "Employment", "Mortality")) %>%
  left_join(dac_line)


# dac_line_df <- npv_dac_dt %>%
#   filter(measure == "DAC share") %>%
#   mutate(value = dac_line,
#          text = paste0("2019 DAC ratio = ", round(dac_line, 2)))

dac_dt$category <- factor(dac_dt$category, levels = c("Mortality", "Employment"))
reduction_df$category <- factor(reduction_df$category, levels = c("Mortality", "Employment"))

## DAC, PV, not relative to BAU
dac_fig_v2 <- ggplot(dac_dt %>% filter(type == "DAC share",
                                       metric %in% c("dac_share_pv", "dac_comp_pv_share"),
                                       !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                       oil_price_scenario == "reference case"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(x = 'GHG emissions reduction target (%, 2045 vs 2019)',
       y = "DAC share",
       color = "Policy intervention") +
  facet_wrap(~category, scales = "free_y") +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  geom_line(data = reduction_df, aes(x = ghg_2045_perc_reduction, y = dac_ratio), inherit.aes = F, color = "darkgray", size = 0.5) +
  geom_text(data = reduction_df %>% filter(ghg_2045_perc_reduction == 50), aes(x = ghg_2045_perc_reduction + 25,
                                                                                y = dac_ratio,
                                                                                label = text), vjust = 0, size = 2, inherit.aes = F) +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## save figure 4, v2
ggsave(dac_fig_v2,
       filename = file.path(main_path, fig_path, 'figs/figure4-refcase.png'),
       width = 5.5,
       height = 3,
       units = "in")

## high and low
## -----------------------------------------------

dac_fig_v2_low <- ggplot(dac_dt %>% filter(type == "DAC share",
                                       metric %in% c("dac_share_pv", "dac_comp_pv_share"),
                                       !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                       oil_price_scenario == "low oil price"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(x = 'GHG emissions reduction target (%, 2045 vs 2019)',
       y = "DAC share",
       color = "Policy intervention") +
  facet_wrap(~category, scales = "free_y") +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  geom_line(data = reduction_df, aes(x = ghg_2045_perc_reduction, y = dac_ratio), inherit.aes = F, color = "darkgray", size = 0.5) +
  geom_text(data = reduction_df %>% filter(ghg_2045_perc_reduction == 50), aes(x = ghg_2045_perc_reduction + 25,
                                                                               y = dac_ratio,
                                                                               label = text), vjust = 0, size = 2, inherit.aes = F) +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## save figure 4, v2
ggsave(dac_fig_v2_low,
       filename = file.path(main_path, fig_path, 'figs/figure4-low.png'),
       width = 5.5,
       height = 3,
       units = "in")

dac_fig_v2_high <- ggplot(dac_dt %>% filter(type == "DAC share",
                                           metric %in% c("dac_share_pv", "dac_comp_pv_share"),
                                           !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                           oil_price_scenario == "high oil price"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(x = 'GHG emissions reduction target (%, 2045 vs 2019)',
       y = "DAC share",
       color = "Policy intervention") +
  facet_wrap(~category, scales = "free_y") +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  geom_line(data = reduction_df, aes(x = ghg_2045_perc_reduction, y = dac_ratio), inherit.aes = F, color = "darkgray", size = 0.5) +
  geom_text(data = reduction_df %>% filter(ghg_2045_perc_reduction == 50), aes(x = ghg_2045_perc_reduction + 25,
                                                                               y = dac_ratio,
                                                                               label = text), vjust = 0, size = 2, inherit.aes = F) +
  theme_line +
  # scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## save figure 4, v2
ggsave(dac_fig_v2_high,
       filename = file.path(main_path, fig_path, 'figs/figure4-high.png'),
       width = 5.5,
       height = 3,
       units = "in")



## figure 6, setback + tax
## ----------------------------------------------------------------

## x axis = avoided mortality
## y axis = employment compensation
## facet... total and per avoided mtghgco2e


# ## carbon px values 2020 
# carbon_px_vals <- carbon_px %>%
#   filter(carbon_price_scenario %in% c("carbon_90_perc_reduction-no_setback-no ccs",
#                                       "carbon_sb_90_perc_reduction-setback_1000ft-no ccs",
#                                       "carbon_sb_90_perc_reduction-setback_2500ft-no ccs",
#                                       "carbon_sb_90_perc_reduction-setback_5280ft-no ccs"),
#          year %in% c(2020, 2045)) %>%
#   mutate(sb_dist = str_extract(carbon_price_scenario, pattern = one_or_more(DGT) %R% 'ft'),
#          sb_dist = ifelse(is.na(sb_dist), "0ft", sb_dist)) %>%
#   select(sb_dist, year, carbon_price)


## filter and pivot wider x measure
csb_npv_dt <- npv_dt %>%
  filter(policy_intervention %in% c('carbon tax & setback', 'carbon tax'),
         target_label == "90%",
         title != "Climate: Abated GHG emissions") %>%
  mutate(sb_dist = str_extract(scen_id, pattern = one_or_more(DGT) %R% 'ft'),
         sb_dist = ifelse(is.na(sb_dist), "0ft", sb_dist),
         sector = ifelse(title == "Labor: Compensation", "labor", "health")) %>%
  select(oil_price_scenario, sb_dist, sector, value, measure, measure_unit) %>%
  pivot_wider(names_from = sector, values_from = value) %>%
  mutate(scenario_name = paste0("Carbon tax + ", sb_dist, " setback"))



# fig
fig_carbon_sb <- ggplot(csb_npv_dt %>% filter(oil_price_scenario == "reference case"), aes(x = labor, y = health, color = scenario_name)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(y = "Health: value of avoided mortalities",
       x = "Labor: value of compensation loss",
       color = NULL) +
  facet_wrap(~ measure, ncol = 2, scales = "free") +
  scale_color_manual(values = c("Carbon tax + 0ft setback" = "#95F9C3",
                                "Carbon tax + 1000ft setback" = "#73C9AC",
                                "Carbon tax + 2500ft setback" = "#3B7C87", 
                                "Carbon tax + 5280ft setback" = "#0B3866")) +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  # scale_color_manual(values = policy_colors_subset) +
  theme_line +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 

## save figure 3
ggsave(fig_carbon_sb,
       filename = file.path(main_path, fig_path, 'figs/figure6-refcase.png'),
       width = 6.5,
       height = 5,
       units = "in")




## high and low
fig_carbon_sb_low <- ggplot(csb_npv_dt %>% filter(oil_price_scenario == "low oil price"), aes(x = labor, y = health, color = scenario_name)) +
  geom_point(size = 2) +
  labs(title = "Low oil price",
       y = "Health: value of avoided mortalities",
       x = "Labor: value of compensation loss",
       color = NULL) +
  facet_wrap(~ measure, scales = "free") +
  scale_color_manual(values = c("Carbon tax + 0ft setback" = "#95F9C3",
                                "Carbon tax + 1000ft setback" = "#73C9AC",
                                "Carbon tax + 2500ft setback" = "#3B7C87", 
                                "Carbon tax + 5280ft setback" = "#0B3866")) +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  # scale_color_manual(values = policy_colors_subset) +
  theme_line +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 

## save figure 3
ggsave(fig_carbon_sb_low,
       filename = file.path(main_path, fig_path, 'figs/figure6-low.png'),
       width = 6.5,
       height = 5,
       units = "in")


## high and low
fig_carbon_sb_high <- ggplot(csb_npv_dt %>% filter(oil_price_scenario == "high oil price"), aes(x = labor, y = health, color = scenario_name)) +
  geom_point(size = 2) +
  labs(title = "High oil price",
       y = "Health: value of avoided mortalities",
       x = "Labor: value of compensation loss",
       color = NULL) +
  facet_wrap(~ measure, scales = "free") +
  scale_color_manual(values = c("Carbon tax + 0ft setback" = "#95F9C3",
                                "Carbon tax + 1000ft setback" = "#73C9AC",
                                "Carbon tax + 2500ft setback" = "#3B7C87", 
                                "Carbon tax + 5280ft setback" = "#0B3866")) +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  # scale_color_manual(values = policy_colors_subset) +
  theme_line +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 

## save figure 3
ggsave(fig_carbon_sb_high,
       filename = file.path(main_path, fig_path, 'figs/figure6-high.png'),
       width = 6.5,
       height = 5,
       units = "in")






# ## bar plot
# fig_benefit_x_metric_bar <- ggplot(npv_dt %>% filter(!target %in% c('BAU', '1000ft setback GHG'),
#                                                     policy_intervention != 'carbon tax & setback',
#                                                     unit != "value_billion",
#                                                     title != "Climate: Abated GHG emissions"), aes(x = target_label, y = value, fill = policy_intervention)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
#   labs(title = "NPV per avoided GHG MtCO2e",
#        fill = "Policy intervention",
#        y = "2020 USD million per MtCO2e",
#        x = NULL) +
#   facet_wrap(~title) +
#   # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_fill_manual(values = policy_colors_subset) +
#   # scale_shape_manual(values = c("carbon tax" = 17,
#   #                               "excise tax" = 15,
#   #                               "setback" = 3)) +
#   # scale_y_continuous(labels = comma) +
#   theme_line +
#   theme(legend.position = "left",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(vjust = 0.5, hjust=1)) 


## Equity portion
## ------------------------------------------------------

# dac_dt$pop_type <- factor(dac_dt$pop_type, levels = c("Total population", "DAC population", "DAC share"))

# dac_dt_filt <- dac_dt[ccs_option == "no CCS" &
#                  type == "DAC share" &
#                  policy_intervention != "carbon tax & setback"]
# 
# dac_dt_filt$category <- factor(dac_dt_filt$category, levels = c("Mortality", "Employment"))
# 
# dac_bau_dt_filt <- dac_bau_dt[ccs_option == "no CCS" &
#                            type == "DAC share" &
#                            policy_intervention != "carbon tax & setback" &
#                            target_label != "55%"]
# 
# dac_bau_dt_filt[, category := fifelse(category == "Employment", "Employment loss", "Avoided mortalities")]
# dac_bau_dt_filt$category <- factor(dac_bau_dt_filt$category, levels = c("Avoided mortalities", "Employment loss"))
# 

# ## health
# dac_fig <- ggplot(dac_dt, aes(x = ghg_2045_perc_reduction, y = value, color = target_label, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(x = 'GHG emissions reduction target (%, 2045 vs 2019)',
#        y = 'DAC share',
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   facet_wrap(~metric) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c("BAU" = "black", target_colors)) +
#   scale_shape_manual(values = policy_symbols) +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   theme(legend.position = "bottom",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# ## DAC, PV, not relative to BAU
# dac_fig_v2 <- ggplot(dac_dt %>% filter(type == "DAC share",
#                                        metric %in% c("dac_share_pv", "dac_comp_pv_share")), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.6) +
#   labs(title = 'DAC share',
#        subtitle = "NPV",
#        x = 'GHG emissions reduction target (%, 2045 vs 2019)',
#        y = NULL,
#        color = "Policy intervention") +
#   facet_wrap(~category) +
#   geom_hline(yintercept = dac_line, color = "darkgray", size = 0.5) +
#   annotate("text", x = 75, y = 0.24, label = paste0("2019 DAC ratio = ", round(dac_line, 2)), color = "darkgray", size = 3) +
#   geom_text(data = dac_dt %>% filter(target == "BAU"), aes(x = ghg_2045_perc_reduction, y = value, label = target), 
#     nudge_x = 2, nudge_y = 0.02,  check_overlap = T, size = 3) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
#   guides(alpha = "none") +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   theme(legend.position = "none",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# 
# ## DAC, PV, relative to BAU
# dac_fig_bau <- ggplot(dac_bau_dt %>% filter(type == "DAC share",
#                                        metric %in% c("dac_share_pv", "dac_share_av_pv")), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.6) +
#   labs(title = 'DAC share',
#        subtitle = 'NPV; Avoided mortalities and employment losses relative to BAU',
#        x = 'GHG emissions reduction target (%, 2045 vs 2019)',
#        y = NULL,
#        color = "Policy intervention") +
#   facet_wrap(~category) +
#   geom_hline(yintercept = dac_line, color = "darkgray", size = 0.5) +
#   annotate("text", x = 75, y = 0.24, label = paste0("2019 DAC ratio = ", round(dac_line, 2)), color = "darkgray", size = 3) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
#   guides(alpha = "none") +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   theme(legend.position = "none",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## save both versions of DAC figure for review
## combine the figures
# 
# dac_combine <- plot_grid(
#   dac_fig_v2 + theme(legend.position="none"),
#   dac_fig_bau + theme(legend.position="none"),
#   nrow = 2,
#   rel_heights = c(1, 1)
# )
# 
# ggsave(dac_combine,
#        filename = file.path(main_path, fig_path, 'dac_versions.png'),
#        width = 5.5,
#        height = 6.5,
#        units = "in")
# 


# ## figure
# fig_equity_labor <- ggplot(dac_dt %>% filter(metric == "Employment loss per avoided GHG",
#                                              policy_intervention != "carbon tax & setback"), aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(x = 'GHG emissions reduction in 2045 (% of 2019)',
#        y = 'FTE job-years / avoided MtCO2e',
#        color = "GHG emission target",
#        shape = "Policy intervention") +
#   facet_wrap(~pop_type, scales = "free_y") +
#   scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
#                                 '2500ft setback GHG' = '#00BF7D',
#                                 '5280ft setback GHG' = "#00B0F6",
#                                 '90% GHG reduction' = "#E76BF3"
#                                 # , 'BAU' = '#F8766D'
#   )) +
#   theme_line +
#   scale_y_continuous(limits = c(NA, 0.5)) +
#   theme(legend.position = "bottom",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# 
# ## health
# fig_equity_health <- ggplot(dac_dt %>% filter(metric != "Employment loss per avoided GHG",
#                                               policy_intervention != "carbon tax & setback"), aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(x = '',
#        y = 'Avoided mortalities / avoided MtCO2e',
#        color = "GHG emission target",
#        shape = "Policy intervention") +
#   facet_wrap(~pop_type, scales = "free_y") +
#   scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
#                                 '2500ft setback GHG' = '#00BF7D',
#                                 '5280ft setback GHG' = "#00B0F6",
#                                 '90% GHG reduction' = "#E76BF3"
#                                 # , 'BAU' = '#F8766D'
#   )) +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   theme(legend.position = "bottom",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## extract the legend
legend <- get_legend(
  fig_benefit_x_metric_v2 
  
)

## combine the figures
## ---------------------------------

fig3b_combine <- plot_grid(
  fig_benefit_x_metric_v2 + theme(legend.position="none"),
  dac_fig_v2 + theme(legend.position="none"),
  nrow = 2,
  rel_heights = c(1, 1)
)


ggsave(fig3b_combine,
       filename = file.path(main_path, fig_path, 'figure3.png'),
       width = 5.5,
       height = 6,
       units = "in")

# ## legend on left plot for slides
# 
# left_panel <- ggpubr::ggarrange(fig_equity_health, fig_equity_labor, # list of plots
#                   labels = "AUTO", # labels
#                   common.legend = T, # COMMON LEGEND
#                   legend = "left", # legend position
#                   align = "hv", # Align them both, horizontal and vertical
#                   nrow = 2)  # number of row
# 
# ggsave(left_panel,
#        filename = file.path(main_path, fig_path, 'figure3b_left.png'),
#        width = 9.5,
#        height = 6,
#        units = "in")
# 
# ## DAC only
# 
# dac_shares <- ggplot(dac_dt %>% filter(pop_type == "DAC share",
#                                        policy_intervention != "carbon tax & setback"), 
#                      aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(x = NULL,
#        y = 'DAC share',
#        color = NULL) +
#   facet_wrap(~metric, ncol = 1) +
#   scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
#                                 '2500ft setback GHG' = '#00BF7D',
#                                 '5280ft setback GHG' = "#00B0F6",
#                                 '90% GHG reduction' = "#E76BF3"
#                                 # , 'BAU' = '#F8766D'
#   )) +
#   theme_line +
#   scale_y_continuous(limits = c(0.25, 0.4)) +
#   theme(legend.position = "none",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# 
# 
# ggsave(dac_shares,
#        filename = file.path(main_path, fig_path, 'figure3c_dac.png'),
#        width = 3,
#        height = 5,
#        units = "in")
# 
# 
# 

# ## bar plots
# ## ----------------------------------------------------
# 
# dac_bar_dt <- dac_dt[metric %in% c("cumul_dac_pv", "cumul_total_pv", "dac_share_pv",
#                          "cumul_dac_comp_PV", "cumul_total_comp_PV", "dac_comp_pv_share") &
#                        ccs_option == "no CCS"]
# 
# dac_bar_dt[, value := fifelse(metric %in% c("cumul_dac_pv", "cumul_total_pv"), value * -1, value)]
# 
# dac_bar_dt$target_label <- factor(dac_bar_dt$target_label, levels = c("BAU", "55%", "60%", "75%", "90%"))
# 
# dac_bar_dt$category <- factor(dac_bar_dt$category, levels = c("Mortality", "Employment"))
# 
# ## bar plot
# fig_benefit_x_metric_bar <- ggplot(dac_bar_dt %>% filter(!target %in% c('1000ft setback GHG'),
#                                                      policy_intervention != 'carbon tax & setback',
#                                                      type != "DAC share",
#                                                      metric %in% c("cumul_total_pv", "cumul_total_comp_PV")), aes(x = target_label, y = value /1e9, fill = policy_intervention,)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   facet_wrap(~category, ncol = 2) +
#   # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
#   labs(title = "NPV",
#        fill = "Policy intervention",
#        y = "2020 USD billion",
#        x = NULL) +
#   # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_fill_manual(values = c("BAU" = "black", policy_colors_subset)) +
#   # scale_shape_manual(values = c("carbon tax" = 17,
#   #                               "excise tax" = 15,
#   #                               "setback" = 3)) +
#   # scale_y_continuous(labels = comma) +
#   theme_line +
#   theme(legend.position = "left",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(vjust = 0.5, hjust=1)) 
# 
# 
# ## dac bar
# dac_bar_dt2 <- dac_bar_dt[metric %in% c("dac_share_pv", "dac_comp_pv_share")]
# dac_bar_dt2[, non_dac_share := 1 - value]
# 
# 
# ## DAC
# non_dac_dt <- copy(dac_bar_dt2)
# non_dac_dt[, value := NULL]
# non_dac_dt[, metric := str_replace(metric, "dac", "non_dac")]
# non_dac_dt[, type := "Non-DAC share"]
# 
# setnames(non_dac_dt, c("non_dac_share"), c("value"))
# 
# ## bind iwth dacbar2
# dac_bar_dt2[, non_dac_share := NULL]
# 
# dac_bar_dt2 <- rbind(dac_bar_dt2, non_dac_dt)
# 
# ## dac bar
# dac_bar <- ggplot(dac_bar_dt2 %>% filter(!target %in% c('1000ft setback GHG'),
#                                         policy_intervention != 'carbon tax & setback'), aes(x = target_label, y = value, fill = policy_intervention, alpha = type)) +
#   geom_bar(position = "stack", stat = "identity") +
#   facet_wrap(~category, ncol = 2) +
#   # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
#   labs(title = "NPV",
#        fill = "Policy intervention",
#        y = "2020 USD billion",
#        x = NULL) +
#   # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_fill_manual(values = c("BAU" = "black", policy_colors_subset)) +
#   # scale_shape_manual(values = c("carbon tax" = 17,
#   #                               "excise tax" = 15,
#   #                               "setback" = 3)) +
#   # scale_y_continuous(labels = comma) +
#   theme_line +
#   theme(legend.position = "left",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(vjust = 0.5, hjust=1)) 




## remake figures with carbon + sb policy
## -----------------------------------------------------

npv_90 <- npv_dt[target == "90perc_reduction"]
npv_90[, setback_dist := str_extract(scen_id, pattern = 'setback_' %R% one_or_more(DIGIT) %R% 'ft')]

npv_90 <- npv_90[policy_intervention != 'excise tax & setback']

npv_90 <- npv_90 %>%
  mutate(scen_name = ifelse(policy_intervention == 'carbon tax & setback', paste0('Carbon tax + ', str_extract(setback_dist, one_or_more(DIGIT)), 'ft setback'), as.character(policy_intervention))) %>%
  setDT()

npv_90 <- npv_90 %>%
  mutate(scen_name = ifelse(policy_intervention == 'carbon tax', 'Carbon tax + 0ft setback', scen_name))
  
npv_90$scen_name <- factor(npv_90$scen_name, levels = c("excise tax", "Carbon tax + 0ft setback", 
                                                        "Carbon tax + 1000ft setback", 
                                                        "Carbon tax + 2500ft setback", 
                                                        "Carbon tax + 5280ft setback"))


## fig
fig_benefit_x_metric2 <- ggplot(npv_90 %>% filter(target != 'BAU',
                                                  oil_price_scenario == "reference case",
                                                  scen_name != "excise tax"), aes(x = scen_name, y = value)) +
  geom_point(size = 2, alpha = 0.8, color = "#1d577e") +
  labs(color = NULL,
       y = NULL,
       x = NULL) +
  facet_grid(measure~title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
  #                               '2500ft setback GHG' = '#00BF7D',
  #                               '5280ft setback GHG' = "#00B0F6",
  #                               '90% GHG reduction' = "#E76BF3"
  #                               # , 'BAU' = '#F8766D'
  # )) +
  # # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



ggsave(fig_benefit_x_metric2,
       filename = file.path(main_path, fig_path, 'figs/figure5a-refcase.png'),
       width = 8,
       height = 8,
       units = "in")

## high and low
## ---------------------------

fig_benefit_x_metric2_low <- ggplot(npv_90 %>% filter(target != 'BAU',
                                                  oil_price_scenario == "low oil price",
                                                  scen_name != "excise tax"), aes(x = scen_name, y = value)) +
  geom_point(size = 2, alpha = 0.8, color = "#1d577e") +
  labs(title = "Low oil price",
       color = NULL,
       y = NULL,
       x = NULL) +
  facet_grid(measure~title, scales = "free_y") +
  theme_line +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



ggsave(fig_benefit_x_metric2_low,
       filename = file.path(main_path, fig_path, 'figs/figure5a-low.png'),
       width = 8,
       height = 8,
       units = "in")

## high
fig_benefit_x_metric2_high <- ggplot(npv_90 %>% filter(target != 'BAU',
                                                      oil_price_scenario == "high oil price",
                                                      scen_name != "excise tax"), aes(x = scen_name, y = value)) +
  geom_point(size = 2, alpha = 0.8, color = "#1d577e") +
  labs(title = "High oil price",
       color = NULL,
       y = NULL,
       x = NULL) +
  facet_grid(measure~title, scales = "free_y") +
  theme_line +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



ggsave(fig_benefit_x_metric2_high,
       filename = file.path(main_path, fig_path, 'figs/figure5a-high.png'),
       width = 8,
       height = 8,
       units = "in")



# 
# ## DAC for carbon + sb
# ## -------------------------------
# 
# dac_90 <- dac_dt[target == "90perc_reduction"]
# dac_90[, setback_dist := str_extract(scen_id, pattern = 'setback_' %R% one_or_more(DIGIT) %R% 'ft')]
# 
# dac_90 <- dac_90[policy_intervention != "excise tax & setback"]
# 
# dac_90 <- dac_90[, scen_name := fifelse(policy_intervention == 'carbon tax & setback', paste0('carbon tax + ', str_extract(setback_dist, one_or_more(DIGIT)), 'ft setback'), as.character(policy_intervention))]
# 
# dac_90$scen_name <- factor(dac_90$scen_name, levels = c("excise tax", "carbon tax", "carbon tax + 1000ft setback", "carbon tax + 2500ft setback", "carbon tax + 5280ft setback"))
# 
# 
# ## figure
# fig_equity_labor2 <- ggplot(dac_90 %>% filter(metric == "Employment loss per avoided GHG",
#                                               oil_price_scenario == "reference case"), aes(x = scen_name, y = value)) +
#   geom_point(size = 2, alpha = 0.8, color = "#1d577e") +
#   labs(x = NULL,
#        y = 'FTE job-years / avoided MtCO2e') +
#   facet_wrap(~type, scales = "free_y") +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
#                                 '2500ft setback GHG' = '#00BF7D',
#                                 '5280ft setback GHG' = "#00B0F6",
#                                 '90% GHG reduction' = "#E76BF3"
#                                 # , 'BAU' = '#F8766D'
#   )) +
#   theme_line +
#   scale_y_continuous(limits = c(NA, 0.5)) +
#   theme(legend.position = "none",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# 
# ## health
# fig_equity_health2 <- ggplot(dac_90 %>% filter(metric != "Employment loss per avoided GHG"), aes(x = scen_name, y = value, color = target)) +
#   geom_point(size = 2, alpha = 0.8) +
#   labs(x = '',
#        y = 'Avoided mortalities / avoided MtCO2e') +
#   facet_wrap(~pop_type, scales = "free_y") +
#   # scale_x_continuous(limits = c(0, NA)) +
#   scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
#                                 '2500ft setback GHG' = '#00BF7D',
#                                 '5280ft setback GHG' = "#00B0F6",
#                                 '90% GHG reduction' = "#E76BF3"
#                                 # , 'BAU' = '#F8766D'
#   )) +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   theme(legend.position = "none",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# 
# 
# 
# ## combine the figures
# ## ---------------------------------
# 
# fig5b_combine <- plot_grid(
#   fig_equity_health2,
#   fig_equity_labor2,
#   nrow = 2,
#   rel_heights = c(1, 1)
# )
# 
# 
# ggsave(fig5b_combine,
#        filename = file.path(main_path, fig_path, 'figure5b.png'),
#        width = 8,
#        height = 10,
#        units = "in")


