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

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/setback-revision/'

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
                                           policy_intervention != "BAU"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = setback_name)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "A. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "Setback distance") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c(
    "1000ft" = "solid",
    "2500ft" = "dashed",
    "5280ft" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 74, label = "BAU", size = 2) +
  scale_color_manual(values = sb_policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  theme(legend.position = "right",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## legend figure
shared_legend <- ggplot(levels_dt %>% filter(metric == "total_state_bbl",
                                               year > 2019,
                                               oil_price_scenario == "reference case",
                                               policy_intervention != "BAU"), aes(x = year, y = value / 1e6, color = policy_intervention, lty = setback_name)) +
  geom_line(size = 0.65, alpha = 0.9) +
  labs(title = "A. Oil production",
       x = NULL,
       y = "Barrels (million)",
       color = "Policy",
       lty = "Setback distance") +
  # facet_wrap(~ccs_option) +
  scale_linetype_manual(values = c(
    "1000ft" = "solid",
    "2500ft" = "dashed",
    "5280ft" = "dotted")) +
  geom_line(data = levels_dt %>% filter(metric == "total_state_bbl",
                                        year > 2019,
                                        policy_intervention == "BAU",
                                        oil_price_scenario == "reference case",
                                        setback_existing == 0), aes(x = year, y = value / 1e6), size = 1.2, alpha = 0.9, color = "black", inherit.aes = F) +
  
  annotate("text", x = 2044, y = 74, label = "BAU", size = 2) +
  scale_color_manual(values = sb_policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line_n +
  guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2)) +
  theme(legend.position = "left",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## cumulative GHG x 2045 reductions
## ---------------------------------------------------------

cumul_ghg <- levels_dt[(metric == "total_state_ghg_MtCO2" & 
                          year > 2019), .(cumul_ghg = sum(value)), by = .(scen_id, setback_existing, oil_price_scenario,  policy_intervention,
                                                                          setback_name, ghg_2045_perc, target_label)]


cumul_ghg$policy_intervention <- factor(cumul_ghg$policy_intervention, levels = c("BAU", "setback (all wells)", "setback (new wells)"))


ghg_cumul_fig_v2 <- ggplot(cumul_ghg %>% 
                             filter(oil_price_scenario == "reference case"), aes(x = ghg_2045_perc * -100, y = cumul_ghg, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "C. Cumulative GHG emissions",
       x = "GHG emissions reduction target (%, 2045 vs 2019)",
       y = bquote(MtCO[2]~e),
       color = "2045 GHG emission target",
       shape = "Policy intervention") +
  theme_line_n +
  annotate("text", x = 59, y = 255, label = "BAU", size = 2) +
  scale_color_manual(values = c("BAU" = "black", sb_policy_colors_subset)) +
  # scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(150, 270)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.background = element_rect(fill = "white", color = "grey"),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

## panels c and d
## ----------------------------------------

## setback name
merge_df <- unique(levels_dt[, .(scen_id, setback_name)])

npv_dt <- left_join(npv_dt, merge_df)  %>%
  as.data.table()

npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: avoided damage',
                                    fifelse(title == "Labor: Compensation", "Labor: forgone wages", "Health: avoided mortality"))]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: avoided mortality', 'Labor: forgone wages', 'Climate: avoided damage'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'oil_price_scenario', 'setback_existing', 'setback_name', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction', 'target_label'),
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


fig_bxm_a <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback', 'excise tax', 'carbon tax'),
                                      title == "Health: avoided mortality",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention, shape = setback_name)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "A. Health: avoided mortality",
       y = "NPV (2019 USD billion)",
       x = NULL) +
  ylim(0, 5) +
  scale_color_manual(values = sb_policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm')) 

fig_bxm_b <- ggplot(npv_dt %>% filter(target != 'BAU',
                                      oil_price_scenario == "reference case",
                                      !policy_intervention %in% c('carbon tax & setback', 'excise tax & setback', 'excise tax', 'carbon tax'),
                                      title == "Labor: forgone wages",
                                      measure == "NPV (2019 USD billion)"), aes(x = ghg_2045_perc_reduction, y = value, color = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "darkgray", size = 0.5) +
  labs(color = "Policy",
       title = "B. Labor: forgone wages",
       y = NULL,
       x = NULL) +
  ylim(-15, 0) +
  scale_color_manual(values = policy_colors_subset) +
  theme_line_n +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.ticks.length.y = unit(0.1, 'cm'),
        axis.ticks.length.x = unit(0.1, 'cm'))


