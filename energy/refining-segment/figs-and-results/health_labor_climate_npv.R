## Tracey Mangin
## May 16, 2022
## Refining fig: Health, labor, and climate impacts of different refining scenarios

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

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## paths
# main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
main_path <- '/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn' # meas path
fig_path <- 'outputs/academic-out/refining/figures'

## csv names
npv_file <- 'npv_x_metric_refining.csv'
dac_file <- 'dac_health_labor_refining.csv'

## read in data
npv_dt <- fread(file.path(main_path, fig_path, npv_file))
dac_dt <- fread(file.path(main_path, fig_path, dac_file))

## cumulative
npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))


## add scenario
npv_dt[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]
npv_dt[, scenario := paste0(demand_scenario, " demand", ' - ', refining_scenario)]
npv_dt[, scenario := str_replace(scenario, "LC1", "Low C.")]
npv_dt[, short_scen := str_replace(scenario, "LC1", "Low C.")]
npv_dt[, short_scen := str_replace(short_scen, "historic", "hist.")]

## filter for the six scenarios
npv_dt <- npv_dt[oil_price_scenario == 'reference case' &
                 carbon_price_scenario == 'price floor' &
                 ccs_scenario == 'no ccs']


## factor
npv_dt$scenario <- factor(npv_dt$scenario, levels = c('BAU demand - historic production', 
                                                            'BAU demand - historic exports', 
                                                            'BAU demand - low exports',
                                                            'Low C. demand - historic production',
                                                            'Low C. demand - historic exports',
                                                            'Low C. demand - low exports'))

## factor
npv_dt$short_scen <- factor(npv_dt$short_scen, levels = c('BAU demand - hist. production', 
                                                                'BAU demand - hist. exports', 
                                                                'BAU demand - low exports',
                                                                'Low C. demand - hist. production',
                                                                'Low C. demand - hist. exports',
                                                                'Low C. demand - low exports'))


## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'scenario', 'short_scen', 'demand_scenario', 'refining_scenario', 'title', 'ghg_2045_perc_reduction'),
               measure.vars = c("value_billion", "value_per_ghg_million"),
               variable.name = "unit",
               value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2019 USD billion)", "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)")]

npv_dt[, measure_unit := fifelse(unit == "value_billion", "2019 USD billion", "2019 USD million / MtCO2e")]


## color for refining scenario
refin_colors <- c('historic exports' = '#2F4858', 'historic production' = '#F6AE2D', 'low exports' = '#F26419')

## filter out these BAU scenarios
bau_scens <- c('BAU historic production low innovation price floor no ccs reference case',
               'LC1 historic production low innovation price floor no ccs reference case')



# figure
fig_benefit_x_metric <- ggplot(npv_dt %>% filter(!scen_id %in% bau_scens), 
                               aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Refing scenario",
       shape = "Demand scenario",
       y = NULL,
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  facet_grid(measure ~ title, scales = "free") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = refin_colors) +
  theme_line_n +
  theme(legend.position = "bottom",
        # legend.box = "vertical",
        # legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## revised version, make them separately
## -------------------------------------------------------------------
fig_bxm_a <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                      title == "Health: avoided mortality",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value,  color = refining_scenario, shape = demand_scenario),
             size = 3, alpha = 0.8) +
  labs(color = "Refing scenario",
       shape = "Demand scenario",
       title = "A. Health: avoided mortality",
       y = "NPV (2019 USD billion)",
       x = NULL) +
  ylim(-1, 40) +
  xlim(0, 80) +
  scale_color_manual(values = refin_colors) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

fig_bxm_b <- ggplot() + 
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                      title == "Labor: forgone wages",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  labs(color = "Policy",
       title = "B. Labor: forgone wages",
       y = NULL,
       x = NULL) +
  ylim(-30, 0) +
  xlim(0, 80) +
  scale_color_manual(values = refin_colors) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

fig_bxm_c <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                      title == "Climate: avoided damage",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "C. Climate: avoided damage",
       y = NULL,
       x = NULL) +
  ylim(-1, 20) +
  xlim(0, 80) +
  scale_color_manual(values = refin_colors) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

fig_bxm_d <- ggplot() + 
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                      title == "Health: avoided mortality",
                                      measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  labs(color = "Policy",
       title = "D.",
       y = bquote('NPV (2019 USD million)\nper avoided GHG MtCO'[2]~e),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = refin_colors) +
  ylim(0, 200) +
  xlim(0, 80) +
  theme_line_n +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

fig_bxm_e <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                      title == "Labor: forgone wages",
                                      measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  labs(color = "Policy",
       title = "E.",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = refin_colors) +
  theme_line_n +
  xlim(0, 80) +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

fig_bxm_f <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                      title == "Climate: avoided damage",
                                      measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario), size = 3, alpha = 0.8) +
  labs(color = "Policy",
       title = "F.",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = refin_colors) +
  theme_line_n +
  ylim(0, 80) +
  xlim(0, 80) +
  theme(legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

## extract legend
legend_fig <- ggplot() +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  geom_point(data = npv_dt %>% filter(!scen_id %in% bau_scens,
                                       title == "Labor: forgone wages",
                                       measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = scenario, shape = scenario), size = 3, alpha = 0.8) +
  labs(title = "",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(name = "",
                     labels = c("BAU demand - historic exports", 
                                "BAU demand - low exports", 
                                "Low C. demand - historic exports", 
                                "Low C. demand - low exports"),
                     values = c("BAU demand - historic exports" = "#2F4858", 
                                "BAU demand - low exports" = "#F26419", 
                                "Low C. demand - historic exports" = "#2F4858",
                                "Low C. demand - low exports" = "#F26419")) +   
  scale_shape_manual(name = "",
                        labels = c("BAU demand - historic exports", 
                                   "BAU demand - low exports", 
                                   "Low C. demand - historic exports", 
                                   "Low C. demand - low exports"),
                        values = c(16, 16, 17, 17)) +
  theme_line_n +
  theme(legend.position = "bottom",
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) +
  guides(color = guide_legend(nrow = 1, byrow = FALSE))


legend_fig_3 <- get_legend(
  legend_fig + 
    theme(legend.title = element_text(size = 5),
          legend.text = element_text(size = 5))
  
)


## combine figure
## ---------------------------------

## shared x axis
xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 7)

fig3_plot_grid <- plot_grid(
  fig_bxm_a,
  fig_bxm_b,
  fig_bxm_c,
  fig_bxm_d + labs(x = NULL),
  fig_bxm_e + labs(x = NULL),
  fig_bxm_f+ labs(x = NULL),
  align = 'vh',
  # labels = c("A", "B", "C", "D", "E", "F"),
  # # labels = 'AUTO',
  # label_size = 10,
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
       filename = file.path(main_path, fig_path, 'health_labor_climate_impacts_fig.png'),
       width = 180,
       height = 160,
       units = "mm",)

ggsave(fig3_plot_grid2,
       filename = file.path(main_path, fig_path, 'health_labor_climate_impacts_fig.pdf'),
       width = 180,
       height = 160,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'health_labor_climate_impacts_fig.pdf'),
            outfile = paste0(main_path, fig_path, 'health_labor_climate_impacts_fig.pdf'))


## DAC figure
## ----------------------------------------

scens <- c("historic exports - BAU demand", "low exports - BAU demand", "historic exports - LC1 demand", "low exports - LC1 demand")

fig_dac_bau_h <- ggplot(dac_dt %>% filter(scenario %in% scens,
                                              oil_price_scenario == "reference case",
                                              type == "DAC share",
                                              metric %in% c("dac_share_emp", "dac_share_health"),
                                          ccs_scenario == "no ccs") %>%
                          mutate(facet_lab = ifelse(category == "Health", "Health: avoided mortalities",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Health: avoided mortalities"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "A. Health: avoided mortalities",
       color = "Refining scenario",
       shape = "Demand scenario",
       y = "DAC share",
       x = NULL) +
  scale_color_manual(values = refin_colors) +
  # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.42, 0.44)) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        plot.background = element_rect(color="white")) 

ggsave(fig_dac_bau_h,
       filename = file.path(main_path, fig_path, 'fig_dac_bau_h.png'),
       width = 5,
       height = 5.2,
       dpi = 400,
       units = "in",
       device = 'png')

ggsave(fig_dac_bau_h,
       filename = file.path(main_path, fig_path, 'fig_dac_bau_h.pdf'),
       width = 5,
       height = 5.2,
       dpi = 400,
       units = "in",
       device = 'pdf')

embed_fonts(file.path(main_path, fig_path, 'fig_dac_bau_h.pdf'),
            outfile = file.path(main_path, fig_path, 'fig_dac_bau_h.pdf'))


fig_dac_bau_l <- ggplot(dac_dt %>% filter(scenario %in% scens,
                                          oil_price_scenario == "reference case",
                                          type == "DAC share",
                                          metric %in% c("dac_share_emp", "dac_share_health"),
                                          ccs_scenario == "no ccs") %>%
                          mutate(facet_lab = ifelse(category == "Health", "Health: avoided mortalities",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Labor: forgone wages"), aes(x = ghg_2045_perc_reduction, y = value, color = refining_scenario, shape = demand_scenario)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "B. Labor: forgone wages",
       color = "Refining scenario",
       shape = "Demand scenario",
       y = "DAC share",
       x = NULL) +
  scale_color_manual(values = refin_colors) +
  # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.39, 0.49)) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        plot.background = element_rect(color="white")) 
  
ggsave(fig_dac_bau_l,
       filename = file.path(main_path, fig_path, 'fig_dac_bau_l.png'),
       width = 5,
       height = 5.2,
       dpi = 400,
       units = "in",
       device = 'png')

ggsave(fig_dac_bau_l,
       filename = file.path(main_path, fig_path, 'fig_dac_bau_l.pdf'),
       width = 5,
       height = 5.2,
       dpi = 400,
       units = "in",
       device = 'pdf')

embed_fonts(file.path(main_path, fig_path, 'fig_dac_bau_l.pdf'),
            outfile = file.path(main_path, fig_path, 'fig_dac_bau_l.pdf'))

  
## below is not updated
## ---------------------------------
 

# ## plot them together
# ## -------------------------------
# 
# fig4_plot_grid <- plot_grid(
#   fig_dac_bau_h + theme(legend.position = "none"),
#   fig_dac_bau_l,
#   align = 'vh',
#   # labels = c("A", "B"),
#   # # labels = 'AUTO',
#   # label_size = 10,
#   hjust = -1,
#   nrow = 1,
#   rel_widths = c(1, 1)
# )
# 
# fig4_plot_grid2 <- plot_grid(
#   fig4_plot_grid,
#   xaxis_lab,
#   align = "v",
#   # labels = c("(A)", "(B)", "(C)", ""),
#   # # labels = 'AUTO',
#   # label_size = 10,
#   # hjust = -1,
#   ncol = 1,
#   rel_heights = c(1, 0.025)
#   # rel_widths = c(1, 1),
# )
# 
# fig4_plot_grid2 <- plot_grid(
#   fig4_plot_grid2,
#   NULL,
#   legend_fig_3,
#   align = "v",
#   # labels = c("(A)", "(B)", "(C)", ""),
#   # # labels = 'AUTO',
#   # label_size = 10,
#   # hjust = -1,
#   ncol = 1,
#   rel_heights = c(1, 0.025, 0.05)
#   # rel_widths = c(1, 1),
# )
# 
# 
# ## save figure 4, v1
# ggsave(fig4_plot_grid2,
#        filename = file.path(main_path, fig_path, 'figs/figure4-refcase-relBAU.png'),
#        width = 100,
#        height = 70,
#        units = "mm")
# 
# ggsave(fig4_plot_grid2,
#        filename = file.path(main_path, fig_path, 'figs/figure4-refcase-relBAU.pdf'),
#        width = 100,
#        height = 70,
#        units = "mm",
#        device = 'pdf')
# 
# embed_fonts(paste0(main_path, fig_path, 'figs/figure4-refcase-relBAU.pdf'),
#             outfile = paste0(main_path, fig_path, 'figs/figure4-refcase-relBAU.pdf'))
# 
# 





