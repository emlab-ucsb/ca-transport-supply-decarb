## tracey mangin
## april 25, 2023
## nature energy policy brief figure: combo of figs 3 and 5

## libraries
library(data.table)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)
library(rebus)
library(tidyverse)
library(ggrepel)

## define if you are using zenodo repo for inputs (if yes, set to TRUE)
zenodo_repo <- FALSE

## if using zenodo, define user path and location to save outputs
if(zenodo_repo) {
  zenodo_user_path <- '~/Desktop/'
  zenodo_save_path <- ""
}

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## set paths
if(zenodo_repo) {
  
  ## input path
  main_path <- paste0(zenodo_user_path, 'ca-transportation-supply-decarb-files/outputs/fig-and-results-out/')
  fig_path <- main_path
  save_path <- zenodo_save_path
  
} else {
  
  ## paths
  main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
  main_path <- '/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/'
  fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/final/'
  save_path <- paste0(fig_path, "figs/policy-brief/")
  source_data_path <- paste0(main_path, 'nature-energy/source-data/')
  
}

## csv names
npv_file <- 'npv_x_metric_all_oil.csv'
dac_bau_file <- 'dac_bau_health_labor_all_oil.csv' ## DAC shares relative to BAU



## read in data
if(zenodo_repo) {
  npv_dt <- fread(paste0(main_path, npv_file))
  dac_bau_dt <- fread(paste0(main_path, dac_bau_file))
} else {
  npv_dt <- fread(paste0(main_path, fig_path, npv_file))
  dac_bau_dt <- fread(paste0(main_path, fig_path, dac_bau_file))
  
}


## cumulative
npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'oil_price_scenario', 'setback_existing', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction', 'target_label'),
               measure.vars = c("value_billion", "value_per_ghg_million"),
               variable.name = "unit",
               value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2019 USD billion)", "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)")]

npv_dt[, measure_unit := fifelse(unit == "value_billion", "2019 USD billion", "2019 USD million / MtCO2e")]


npv_dt <- npv_dt[target != 'no_target']

npv_dt$target <- factor(npv_dt$target, levels = c('setback_1000ft', 'setback_2500ft', 'setback_5280ft',
                                                  '90perc_reduction'))



# npv_dt$target_label <- factor(npv_dt$target_label, levels = c("55%", "60%", "75%", "90%"))
# 

## update setback policy intervention (if setback_existing == 1, all wells)
npv_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 1), "setback (all wells)",
                                        policy_intervention)]

npv_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 0), "setback (new wells)",
                                        policy_intervention)]


npv_dt$policy_intervention <- factor(npv_dt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                            "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))



# ## create source data
# sd_fig3 <- npv_dt %>%
#   filter(target != 'BAU',
#          oil_price_scenario == "reference case",
#          setback_existing == 0,
#          !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback')) %>%
#   select(oil_price_scenario, policy_intervention, target, cumul_ghg,
#          ghg_2045_perc_reduction, target_label, title, unit, value) %>%
#   mutate(unit = ifelse(unit == "value_billion", "billion_usd_2019", "million_usd_2019_per_ghg"))
# 
# fwrite(sd_fig3, paste0(source_data_path, "fig3/fig3a-f.csv"))


## revised version, make them separately
## -------------------------------------------------------------------
fig3_text_size <- 7

## create setback label dt
health_npv_df <- npv_dt %>% 
  filter(target != 'BAU',
         oil_price_scenario == "reference case",
         setback_existing == 0,
         !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
         title == "Health: avoided mortality",
         measure == "NPV (2019 USD billion)") %>%
  mutate(setback_lab = ifelse(policy_intervention == "setback (new wells)" & target == "setback_1000ft", "1,000 ft",
                              ifelse(policy_intervention == "setback (new wells)" & target == "setback_2500ft", "2,500 ft",
                                     ifelse(policy_intervention == "setback (new wells)" & target == "setback_5280ft", "1 mile",
                                            ifelse(policy_intervention == "excise tax" & target == "90perc_reduction", "90% GHG\nreduction", NA)))))



# fig_health_npv <- ggplot(health_npv_df, aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
#   labs(color = "Policy",
#        title = "Avoided mortality",
#        y = "Net Present Value 2020-2045\n(2019 USD billion)",
#        x = NULL) +
#   geom_text_repel(data = health_npv_df %>% filter(!is.na(setback_lab)), aes(label = setback_lab), size = 2, color = "black") +
#   ylim(0, 2) +
#   scale_color_manual(values = policy_colors_subset) +
#   theme_line_n +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust = 0.5, size = fig3_text_size),
#         axis.text.x = element_text(vjust = 0.5, hjust = 1, size = fig3_text_size),
#         axis.text.y = element_text(size = fig3_text_size),
#         axis.title.y = element_text(size = fig3_text_size, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)),
#         axis.ticks.length.y = unit(0.1, 'cm'),
#         axis.ticks.length.x = unit(0.1, 'cm'))
# 
# ## save figure 3
# ggsave(fig_health_npv,
#        filename = file.path(main_path, save_path, 'pb-a.png'),
#        width = 70,
#        height = 70,
#        units = "mm",)
# 
# ggsave(fig_health_npv,
#        filename = file.path(main_path, save_path, 'pb-a.pdf'),
#        width = 70,
#        height = 70,
#        units = "mm",
#        device = 'pdf')
# 
# embed_fonts(paste0(main_path, save_path, 'pb-a.pdf'),
#             outfile = paste0(main_path, save_path, 'pb-a.pdf'))


## alternative version, lines with distance
values_df <- health_npv_df %>%
  filter(!is.na(setback_lab))

fig_health_npv_v2 <- ggplot() +
  # geom_vline(xintercept = 61.86996, lty = "dashed", color = "darkgray") +
  # geom_vline(xintercept = 65.37517, lty = "dashed", color = "darkgray") +
  # geom_vline(xintercept = 72.49368, lty = "dashed", color = "darkgray") +
  # geom_vline(xintercept = 90.00000, lty = "dashed", color = "darkgray") +
  geom_point(data = health_npv_df, aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention), size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "HEALTH\n\nAvoided mortality",
       # y = (expression(paste(bold("BENEFITS AND LOSSES\n\n"), "Net Present Value 2020-2045\n(2019 USD billion)"))),
       y = "BENEFITS AND LOSSES\n\nNet Present Value 2020-2045\n(2019 USD billion)",
       x = NULL) +
  annotate("text", x = c(61.86996, 65.37517, 72.49368, 90.0), y = c(1, 1.2, 1.4, 2), label = c("1,000 ft", "2,500 ft", "1 mile", "90% GHG\nreduction"), size = 2.1, angle = 90, hjust = 0.5) +
  # geom_text_repel(data = health_npv_df %>% filter(!is.na(setback_lab)), aes(label = setback_lab), size = 2, color = "black") +
  ylim(0, 2.3) +
  xlim(60, 100) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = fig3_text_size),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, size = fig3_text_size),
        axis.text.y = element_text(size = fig3_text_size),
        axis.title.y = element_text(size = fig3_text_size,  face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))

## save figure 3
ggsave(fig_health_npv_v2,
       filename = file.path(main_path, save_path, 'pb-a-v2.png'),
       width = 70,
       height = 70,
       units = "mm",)

ggsave(fig_health_npv_v2,
       filename = file.path(main_path, save_path, 'pb-a-v2.pdf'),
       width = 70,
       height = 70,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, save_path, 'pb-a-v2.pdf'),
            outfile = paste0(main_path, save_path, 'pb-a-v2.pdf'))

## labor
fig_labor_npv <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      setback_existing == 0,
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                      title == "Labor: forgone wages",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "LABOR\n\nLost worker compensation",
       y = NULL,
       x = NULL) +
  ylim(-15, 0) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = fig3_text_size),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, size = fig3_text_size),
        axis.text.y = element_text(size = fig3_text_size),
        axis.title.y = element_text(size = fig3_text_size, face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))


## extract legend
legend_fig <- ggplot(npv_dt %>% filter(target != 'BAU',
                                       oil_price_scenario == "reference case",
                                       setback_existing == 0,
                                       !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback'),
                                       title == "Labor: forgone wages",
                                       measure == "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "",
       y = NULL,
       # y = paste("NPV per avoied GHG ", bquotelab, "(2020 USD million / ", bquotelab),
       x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.title = element_text(size = fig3_text_size),
        legend.text = element_text(size = fig3_text_size),
        plot.title = element_text(hjust = 0, size = fig3_text_size),
        axis.text.x = element_text(vjust = 0.5, hjust = 1, size = fig3_text_size),
        axis.text.y = element_text(size = fig3_text_size),
        axis.title.y = element_text(size = fig3_text_size, face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))



## figure 4: DAC, two versions
## -----------------------------------------

dac_bau_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 1), "setback (all wells)",
                                            policy_intervention)]

dac_bau_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 0), "setback (new wells)",
                                            policy_intervention)]


dac_bau_dt$policy_intervention <- factor(dac_bau_dt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                                    "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))

# ## create source data
# sd_fig5 <- dac_bau_dt %>%
#   filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
#          oil_price_scenario == "reference case",
#          setback_existing == 0,
#          type == "DAC share",
#          metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
#   mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortality",
#                             ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
#   select(oil_price_scenario, policy_intervention, target, target_policy, target_label,
#          ghg_2045_perc, ghg_2045_perc_reduction, category, value) %>%
#   rename(metric = category,
#          DAC_value = value) %>%
#   arrange(metric)
# 
# fwrite(sd_fig5, paste0(source_data_path, "fig5/fig5ab.csv"))


## version 1: relative to BAU
fig_dac_bau_h <- ggplot(dac_bau_dt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                              oil_price_scenario == "reference case",
                                              setback_existing == 0,
                                              type == "DAC share",
                                              metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                          mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortality",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Health: avoided mortality"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = NULL,
       color = "Policy",
       y = "EQUITY\n\nDisadvantaged community share",
       x = NULL) +
  # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.25, 0.35)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text = element_text(vjust = 0.5, hjust=1, size = fig3_text_size),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.title.y = element_text(size = fig3_text_size,  face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0)),
        title = element_text(size = fig3_text_size),
        legend.title = element_text(size = fig3_text_size),
        legend.text = element_text(size = fig3_text_size))

fig_dac_bau_l <- ggplot(dac_bau_dt %>% filter(!policy_intervention %in% c('BAU', 'carbon tax & setback', 'excise tax & setback'),
                                              oil_price_scenario == "reference case",
                                              setback_existing == 0,
                                              type == "DAC share",
                                              metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                          mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Labor: forgone wages"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = NULL,
       color = "Policy",
       y = NULL,
       x = NULL) +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.3, 0.40)) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        axis.text = element_text(vjust = 0.5, hjust=1, size = fig3_text_size),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.title = element_text(size = fig3_text_size),
        title = element_text(size = fig3_text_size),
        legend.title = element_text(size = fig3_text_size),
        legend.text = element_text(size = fig3_text_size))

## plot them together
## -------------------------------

pb_plot_grid <- plot_grid(
  fig_health_npv_v2 + theme(legend.position = "none",
                         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
  fig_labor_npv + theme(legend.position = "none",
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
  fig_dac_bau_h + theme(legend.position = "none",
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
  fig_dac_bau_l + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")),
  align = 'vh',
  # labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1)
)
pb_plot_grid

## smaller x axis
xaxis_lab_5 <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 7)

pb_plot_grid2 <- plot_grid(
  pb_plot_grid,
  xaxis_lab_5,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.025)
  # rel_widths = c(1, 1),
)
pb_plot_grid2

## legend fig
legend_pb_fig <- get_legend(
  legend_fig
  
)

pb_plot_grid3 <- plot_grid(
  pb_plot_grid2,
  NULL,
  legend_pb_fig,
  align = "v",
  # labels = c("(A)", "(B)", "(C)", ""),
  # # labels = 'AUTO',
  # label_size = 10,
  # hjust = -1,
  ncol = 1,
  rel_heights = c(1, 0.025, 0.05)
  # rel_widths = c(1, 1),
)

pb_plot_grid3

## save figure 4, v1
ggsave(pb_plot_grid3,
       filename = file.path(main_path, save_path, 'brief_fig.png'),
       width = 120,
       height = 100,
       units = "mm")

ggsave(pb_plot_grid3,
       filename = file.path(main_path, save_path, 'brief_fig.pdf'),
       width = 120,
       height = 100,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, save_path, 'brief_fig.pdf'),
            outfile = paste0(main_path, save_path, 'brief_fig.pdf'))


## make version without horizontal lines
pb_plot_grid_no_lines <- plot_grid(
  fig_health_npv_v2 + theme(legend.position = "none",
                            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
                            panel.grid.major.y = element_blank()),
  fig_labor_npv + theme(legend.position = "none",
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
                        panel.grid.major.y = element_blank()),
  fig_dac_bau_h + theme(legend.position = "none",
                        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
                        panel.grid.major.y = element_blank()),
  fig_dac_bau_l + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
                        panel.grid.major.y = element_blank()),
  align = 'vh',
  # labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1)
)
pb_plot_grid_no_lines








