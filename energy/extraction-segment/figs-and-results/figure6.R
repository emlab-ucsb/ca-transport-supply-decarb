## Tracey Mangin
## November 22, 2022
## Figure comparing setback scenarios

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)
library(broom)
library(cowplot)
library(extrafont)
library(rebus)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/final/'

## csv names
levels_name <- 'state_levels_all_oil.csv'
npv_file <- 'npv_x_metric_all_oil.csv'
dac_bau_file <- 'dac_bau_health_labor_all_oil.csv' ## DAC shares relative to BAU

## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

npv_dt <- fread(paste0(main_path, fig_path, npv_file))

dac_bau_dt <- fread(paste0(main_path, fig_path, dac_bau_file))

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

## filter for setback scnearios
levels_dt <- levels_dt[policy_intervention %in% c("BAU", "setback (new wells)", "setback (all wells)")]
levels_dt[, setback_name := paste0(str_extract(setback_scenario, pattern = one_or_more(DGT)), 'ft')]

levels_dt %>% filter(metric == "total_state_bbl",
                     year > 2019,
                     oil_price_scenario == "reference case",
                     policy_intervention != "BAU") %>%
  select(policy_intervention, setback_scenario, setback_existing, target_label) %>%
  unique()



prod_fig_sb <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                           year > 2019,
                                           oil_price_scenario == "reference case",
                                           !policy_intervention %in% c("BAU"),
                                           setback_name != "1000ft"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = setback_name)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "Setback distance") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c(
    # "1000ft" = "solid",
    "2500ft" = "solid",
    "5280ft" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 80, label = "BAU", size = 3) +
  scale_color_manual(values = sb_policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n_l +
  theme(legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_text(size =  8),
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'), 
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) 

## view
prod_fig_sb


## legend figure
line_legend <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                               year > 2019,
                                               oil_price_scenario == "reference case",
                                               policy_intervention != "BAU",
                                               setback_name != "1000ft"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = setback_name)) +
  geom_line(size = 0.65, alpha = 0.9) +
  # geom_point(data = levels_dt %>% filter(metric == "total_state_bbl",
  #                                        year > 2019,
  #                                        oil_price_scenario == "reference case",
  #                                        policy_intervention != "BAU"), aes(x = year, y = value / 1e6, shape = setback_name, color = policy_intervention)) +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy: ",
       lty = "Setback distance: ") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c(
    # "1000ft" = "solid",
    "2500ft" = "solid",
    "5280ft" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 74, label = "BAU", size = 3) +
  scale_color_manual(values = sb_policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n_l +
  guides(color = guide_legend(nrow = 2, override.aes = list(shape = NA)),
         lty = "none") +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        legend.margin = margin(-0.5,0,0,0, unit="cm")) 

## View
line_legend


## legend figure
lty_legend <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                          year > 2019,
                                          oil_price_scenario == "reference case",
                                          policy_intervention != "BAU",
                                          setback_name != "1000ft"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = setback_name)) +
  geom_line(size = 0.65, alpha = 0.9) +
  # geom_point(data = levels_dt %>% filter(metric == "total_state_bbl",
  #                                        year > 2019,
  #                                        oil_price_scenario == "reference case",
  #                                        policy_intervention != "BAU"), aes(x = year, y = value / 1e6, shape = setback_name, color = policy_intervention)) +
  labs(title = "a. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy: ",
       lty = "Setback distance: ") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c(
    # "1000ft" = "solid",
    "2500ft" = "solid",
    "5280ft" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 74, label = "BAU", size = 3) +
  scale_color_manual(values = sb_policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n_l +
  guides(color = "none") +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        legend.margin = margin(-0.5,0,0,0, unit="cm")) 

## View
lty_legend



## cumulative GHG x 2045 reductions
## ---------------------------------------------------------

cumul_ghg <- levels_dt[(metric == "total_state_ghg_MtCO2" & 
                          year > 2019), .(cumul_ghg = sum(value)), by = .(scen_id, setback_existing, oil_price_scenario,  policy_intervention,
                                                                          setback_name, ghg_2045_perc, target_label)]


cumul_ghg$policy_intervention <- factor(cumul_ghg$policy_intervention, levels = c("BAU", "setback (all wells)", "setback (new wells)"))


ghg_cumul_fig_v2 <- ggplot(cumul_ghg %>% 
                             filter(oil_price_scenario == "reference case",
                                    policy_intervention != "BAU"), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention, shape = setback_name)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "b. Cumulative GHG emissions",
       x = "GHG emissions reduction target (%, 2045 vs 2019)",
       # y = "",
       y = bquote(MtCO[2]~e),
       color = "Policy intervention: ",
       shape = "Setback distance: ") +
  geom_point(data = cumul_ghg %>% 
               filter(oil_price_scenario == "reference case",
                      policy_intervention == "BAU"), aes(x = ghg_2045_perc * -100, y = cumul_ghg), color = "black", shape = 3, inherit.aes = FALSE) +
  theme_line_n_l +
  annotate("text", x = 61, y = 260, label = "BAU", size = 3) +
  scale_color_manual(values = c(sb_policy_colors_subset)
                     # ,guide = "none"
                     ) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(150, 270)) +
  theme(legend.position = "bottom",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # legend.background = element_rect(fill = "white", color = "grey"),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        legend.box="vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) 
## View
ghg_cumul_fig_v2

# Combine A and B
fig6ab <- plot_grid(
  prod_fig_sb,
  ghg_cumul_fig_v2 + theme(legend.position = "none"),
  align = 'vh',
  # labels = c("A", "B", "C", "D", "E", "F"),
  # # labels = 'AUTO',
  # label_size = 10,
  nrow = 1,
  rel_widths = c(1, 1)
)

## get legend
line_legend_fig6 <- get_legend(
  line_legend + 
    theme(legend.title = element_text(size = 9),
          legend.text = element_text(size = 9))
  
)

## View
plot(line_legend_fig6)

## get legend
lty_legend_fig6 <- get_legend(
  lty_legend + 
    theme(legend.title = element_text(size = 9),
          legend.text = element_text(size = 9))
  
)

## View
plot(lty_legend_fig6)



# ## plot together
# fig6abl <- plot_grid(
#   top_legend_fig6,
#   fig6ab,
#   align = "v",
#   ncol = 1,
#   rel_heights = c(0.25, 1)
#   # rel_widths = c(1, 1),
# )




## panels c and d
## ----------------------------------------

## setback name
merge_df <- unique(levels_dt[, .(scen_id, setback_name)])

npv_dt <- left_join(npv_dt, merge_df)  %>%
  as.data.table()

npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))

## filter out no setback
npv_dt <- npv_dt[!is.na(setback_name)]


## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'oil_price_scenario', 'setback_existing', 
                                   'setback_name', 'policy_intervention', 'target', 'cumul_ghg',  
                                   'title', 'ghg_2045_perc_reduction', 'target_label'),
               measure.vars = c("value_billion", "value_per_ghg_million"),
               variable.name = "unit",
               value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2019 USD billion)", "NPV per avoided GHG MtCO2e\n(2019 USD million / MtCO2e)")]

npv_dt[, measure_unit := fifelse(unit == "value_billion", "2019 USD billion", "2019 USD million / MtCO2e")]


npv_dt <- npv_dt[target != 'no_target']

npv_dt$target <- factor(npv_dt$target, levels = c('setback_1000ft', 'setback_2500ft', 'setback_5280ft',
                                                  '90perc_reduction'))


## update setback policy intervention (if setback_existing == 1, all wells)
npv_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 1), "setback (all wells)",
                                        policy_intervention)]

npv_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                           setback_existing == 0), "setback (new wells)",
                                        policy_intervention)]


npv_dt$policy_intervention <- factor(npv_dt$policy_intervention, levels = c("carbon tax", "excise tax",
                                                                            "setback (new wells)", "setback (all wells)", "carbon tax & setback", "excise tax & setback"))


fig_bxm_c <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback', 'excise tax', 'carbon tax'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention, shape = setback_name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "c. Health: avoided mortality",
       y = "NPV (2019 USD billion)",
       x = NULL) +
  ylim(0, 3) +
  scale_color_manual(values = sb_policy_colors_subset) +
  theme_line_n_l +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) 

## View
fig_bxm_c


fig_bxm_d <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback', 'excise tax', 'carbon tax'),
                                      title == "Labor: forgone wages",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention, shape = setback_name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "d. Labor: lost worker compensation",
       y = NULL,
       x = NULL) +
  ylim(-15, 0) +
  scale_color_manual(values = sb_policy_colors_subset) +
  theme_line_n_l +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

## View
fig_bxm_d

## shared x axis
xaxis_lab <- ggdraw() + draw_label("GHG emissions reduction target (%, 2045 vs 2019)", size = 9)

## Combine c and d
fig6cd_plot_grid <- plot_grid(
  fig_bxm_c,
  fig_bxm_d,
  align = 'vh',
  # labels = c("A", "B"),
  # # labels = 'AUTO',
  # label_size = 10,
  hjust = -1,
  nrow = 1,
  rel_widths = c(1, 1)
)

fig6cd_plot_grid2 <- plot_grid(
  fig6cd_plot_grid,
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



## DAC
## ---------------------------------------------------------------

## setback name

dac_bau_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 1), "setback (all wells)",
                                            policy_intervention)]

dac_bau_dt[, policy_intervention := fifelse((policy_intervention == "setback" &
                                               setback_existing == 0), "setback (new wells)",
                                            policy_intervention)]

dac_bau_dt <- dac_bau_dt[policy_intervention %in% c("setback (new wells)", "setback (all wells)")]
dac_bau_dt <- dac_bau_dt[oil_price_scenario %in% c("reference case")]

dac_bau_dt <- left_join(dac_bau_dt, merge_df)  %>%
  as.data.table()

dac_bau_dt$policy_intervention <- factor(dac_bau_dt$policy_intervention, levels = c("setback (new wells)", "setback (all wells)"))

## version 1: relative to BAU
fig_dac_bau_h <- ggplot(dac_bau_dt %>% filter(type == "DAC share",
                                              metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                          mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Health: avoided mortalities"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention, shape = setback_name)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "e. DAC share health: avoided mortality",
       color = NULL,
       y = "DAC share",
       x = NULL) +
  # x = "GHG emissions reduction target (%, 2045 vs 2019)") +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.3, 0.35)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = sb_policy_colors_subset) +
  theme_line_n_l +
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), "cm")) 

## View
fig_dac_bau_h

fig_dac_bau_l <- ggplot(dac_bau_dt %>% filter(type == "DAC share",
                                              metric %in% c("dac_share_pv", "dac_share_av_pv")) %>%
                          mutate(facet_lab = ifelse(category == "Avoided mortalities", "Health: avoided mortalities",
                                                    ifelse(category == "Employment", "Labor: forgone wages", category))) %>%
                          filter(facet_lab == "Labor: forgone wages"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention, shape = setback_name)) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(title = "f. DAC share labor: lost worker compensation",
       color = "Policy",
       y = NULL,
       x = NULL) +
  # facet_wrap(~facet_lab, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.3, 0.35)) +
  scale_color_manual(values = sb_policy_colors_subset) +
  theme_line_n_l +
  theme(legend.position = "none",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(vjust = 0.5, hjust=1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'),
        axis.title = element_text(size = 7),
        plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), "cm"))

## View
fig_dac_bau_l

## plot them together
## -------------------------------

fig6ef_plot_grid <- plot_grid(
  fig_dac_bau_h,
  fig_dac_bau_l,
  align = 'vh',
  hjust = -1,
  nrow = 1,
  rel_widths = c(1, 1)
)

fig6ef_plot_grid2 <- plot_grid(
  fig6ef_plot_grid,
  xaxis_lab,
  align = "v",
  ncol = 1,
  rel_heights = c(1, 0.025)
  # rel_widths = c(1, 1),
)

## View
fig6ef_plot_grid2

## get legend
bottom_legend_fig6 <- get_legend(
  ghg_cumul_fig_v2 + 
    guides(color = "none") +
    theme(legend.title = element_blank(),
          legend.justification = "left",
          legend.text = element_text(size = 9))
  
)

## View
plot(bottom_legend_fig6)

## plot setback legends together
sb_legends <- plot_grid(
  lty_legend_fig6,
  bottom_legend_fig6,
  align = "v",
  ncol = 1,
  rel_heights = c(1, 1)
)

legends <- plot_grid(
  line_legend_fig6,
  sb_legends,
  align = "v",
  ncol = 2
)


## plot together
fig6 <- plot_grid(
  fig6ab,
  fig6cd_plot_grid2,
  fig6ef_plot_grid,
  align = "v",
  ncol = 1,
  rel_heights = c(1, 1, 1)
  # rel_widths = c(1, 1),
)

## save figure 6
ggsave(fig6,
       filename = file.path(main_path, fig_path, 'figs/figure6-ref-case.png'),
       width = 180,
       height = 210,
       units = "mm",)

ggsave(fig6,
       filename = file.path(main_path, fig_path, 'figs/figure6-ref-case.pdf'),
       width = 180,
       height = 210,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/figure6-ref-case.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/figure6-ref-case.pdf'))


## save fig 6 legend
ggsave(legends,
       filename = file.path(main_path, fig_path, 'figs/figure6-ref-case-l.pdf'),
       width = 150,
       height = 25,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/figure6-ref-case-l.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/figure6-ref-case-l.pdf'))




