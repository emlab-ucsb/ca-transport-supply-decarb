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
fig_path <- 'outputs/academic-out/extraction/figures/'

## csv names
# levels_file <- 'state_levels_subset.csv'
npv_file <- 'npv_x_metric.csv'
dac_file <- 'dac_health_labor.csv'
dac_bau_file <- 'dac_bau_health_labor.csv'
dac_pop_file <- 'state_dac_ratios.csv'


## read in data
npv_dt <- fread(paste0(main_path, fig_path, npv_file))
dac_dt <- fread(paste0(main_path, fig_path, dac_file))
dac_bau_dt <- fread(paste0(main_path, fig_path, dac_bau_file))
dac_pop_dt <- fread(paste0(main_path, fig_path, dac_pop_file))

## cumulative
npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: Abated GHG emissions', title)]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: Avoided mortality', 'Labor: Compensation', 'Climate: Abated GHG emissions'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'ccs_option', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction', 'target_label'),
                     measure.vars = c("value_billion", "value_per_ghg_million"),
                     variable.name = "unit",
                     value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2020 USD billion)", "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e)")]

npv_dt[, measure_unit := fifelse(unit == "value_billion", "2020 USD billion", "2020 USD million / MtCO2e")]


npv_dt <- npv_dt[target != 'BAU']

npv_dt$target <- factor(npv_dt$target, levels = c('1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                        '90% GHG reduction'))
## remove medium CCS
npv_dt <- npv_dt[ccs_option != "medium CCS cost"]


npv_dt$target_label <- factor(npv_dt$target_label, levels = c("55%", "60%", "75%", "90%"))

npv_dt$policy_intervention <- factor(npv_dt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                                  "setback"))


## fig
# fig_benefit_x_metric <- ggplot(npv_dt %>% filter(target != 'BAU',
#                                                  policy_intervention != 'carbon tax & setback',
#                                                  unit != "value_billion",
#                                                  title != "Climate: Abated GHG emissions"), aes(x = ghg_2045_perc_reduction, y = value, color = target_label, shape = policy_intervention)) +
#   geom_point(size = 2, alpha = 0.8) +
#   geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
#   labs(title = "NPV per avoided GHG MtCO2e",
#        x = "GHG emissions reduction target (%, 2045 vs 2019)",
#        y = NULL,
#        color = "2045 GHG emission target",
#        shape = "Policy intervention") +
#   theme_line +
#   facet_grid(measure_unit~title, scales = "free_y") +
#   scale_color_manual(values = c(target_colors)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   # scale_y_continuous(limits = c(NA, 0)) +
#   theme(legend.position = "left",
#         legend.box = "vertical",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  


fig_benefit_x_metric_v2 <- ggplot(npv_dt %>% filter(!target %in% c('BAU', '1000ft setback GHG'),
                                                 policy_intervention != 'carbon tax & setback',
                                                 unit != "value_billion",
                                                 title != "Climate: Abated GHG emissions"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "NPV per avoided GHG MtCO2e",
       color = "Policy intervention",
       y = "2020 USD million per MtCO2e",
       x = 'GHG emissions reduction target (%, 2045 vs 2019)') +
  facet_wrap(~title) +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = policy_colors_subset) +
  # scale_shape_manual(values = c("carbon tax" = 17,
  #                               "excise tax" = 15,
  #                               "setback" = 3)) +
  # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



## bar plot
fig_benefit_x_metric_bar <- ggplot(npv_dt %>% filter(!target %in% c('BAU', '1000ft setback GHG'),
                                                    policy_intervention != 'carbon tax & setback',
                                                    unit != "value_billion",
                                                    title != "Climate: Abated GHG emissions"), aes(x = target_label, y = value, fill = policy_intervention)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "NPV per avoided GHG MtCO2e",
       fill = "Policy intervention",
       y = "2020 USD million per MtCO2e",
       x = NULL) +
  facet_wrap(~title) +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = policy_colors_subset) +
  # scale_shape_manual(values = c("carbon tax" = 17,
  #                               "excise tax" = 15,
  #                               "setback" = 3)) +
  # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1)) 


## Equity portion
## ------------------------------------------------------

# dac_dt$pop_type <- factor(dac_dt$pop_type, levels = c("Total population", "DAC population", "DAC share"))

dac_dt_filt <- dac_dt[ccs_option == "no CCS" &
                 type == "DAC share" &
                 policy_intervention != "carbon tax & setback" &
                 target_label != "55%"]

dac_dt_filt$category <- factor(dac_dt_filt$category, levels = c("Mortality", "Employment"))

dac_bau_dt_filt <- dac_bau_dt[ccs_option == "no CCS" &
                           type == "DAC share" &
                           policy_intervention != "carbon tax & setback" &
                           target_label != "55%"]

dac_bau_dt_filt[, category := fifelse(category == "Employment", "Employment loss", "Avoided mortalities")]
dac_bau_dt_filt$category <- factor(dac_bau_dt_filt$category, levels = c("Avoided mortalities", "Employment loss"))


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

## 2019 DAC line
dac_line <- dac_pop_dt %>% 
  filter(year == 2019) %>% 
  select(dac_ratio) %>% 
  as.numeric()


## DAC, PV, not relative to BAU
dac_fig_v2 <- ggplot(dac_dt %>% filter(type == "DAC share",
                                       metric %in% c("dac_share_pv", "dac_comp_pv_share")), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = 'DAC share',
       subtitle = "NPV",
       x = 'GHG emissions reduction target (%, 2045 vs 2019)',
       y = NULL,
       color = "Policy intervention") +
  facet_wrap(~category) +
  geom_hline(yintercept = dac_line, color = "darkgray", size = 0.5) +
  annotate("text", x = 75, y = 0.24, label = paste0("2019 DAC ratio = ", round(dac_line, 2)), color = "darkgray", size = 3) +
  geom_text(data = dac_dt %>% filter(target == "BAU"), aes(x = ghg_2045_perc_reduction, y = value, label = target), 
    nudge_x = 2, nudge_y = 0.02,  check_overlap = T, size = 3) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  guides(alpha = "none") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



## DAC, PV, relative to BAU
dac_fig_bau <- ggplot(dac_bau_dt %>% filter(type == "DAC share",
                                       metric %in% c("dac_share_pv", "dac_share_av_pv")), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(title = 'DAC share',
       subtitle = 'NPV; Avoided mortalities and employment losses relative to BAU',
       x = 'GHG emissions reduction target (%, 2045 vs 2019)',
       y = NULL,
       color = "Policy intervention") +
  facet_wrap(~category) +
  geom_hline(yintercept = dac_line, color = "darkgray", size = 0.5) +
  annotate("text", x = 75, y = 0.24, label = paste0("2019 DAC ratio = ", round(dac_line, 2)), color = "darkgray", size = 3) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  guides(alpha = "none") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## save both versions of DAC figure for review
## combine the figures

dac_combine <- plot_grid(
  dac_fig_v2 + theme(legend.position="none"),
  dac_fig_bau + theme(legend.position="none"),
  nrow = 2,
  rel_heights = c(1, 1)
)

ggsave(dac_combine,
       filename = file.path(main_path, fig_path, 'dac_versions.png'),
       width = 5.5,
       height = 6.5,
       units = "in")



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


