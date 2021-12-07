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

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/'

## csv names
levels_file <- 'state_levels_subset.csv'
npv_file <- 'npv_x_metric.csv'


## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_file))
npv_dt <- fread(paste0(main_path, fig_path, npv_file))

## filter out carbon + setback
levels_dt <- levels_dt[policy_intervention != 'carbon tax & setback' & ccs_scenario == "no ccs"]
levels_dt$target <- factor(levels_dt$target, levels = c('BAU', '1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                        '90% GHG reduction'))

## cumulative
npv_dt <- npv_dt[policy_intervention != 'carbon tax & setback' & ccs_option != "medium CCS cost"]
npv_dt$target <- factor(npv_dt$target, levels = c('BAU', '1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                                '90% GHG reduction'))

npv_dt <- npv_dt[, title := fifelse(title == 'Abated GHG', 'Climate: Abated GHG emissions', title)]

npv_dt$title <- factor(npv_dt$title, levels = c('Health: Avoided mortality', 'Labor: Compensation', 'Climate: Abated GHG emissions'))

## pivot longer
npv_dt <- melt(npv_dt, id.vars = c('scen_id', 'ccs_option', 'policy_intervention', 'target', 'cumul_ghg',  'title', 'ghg_2045_perc_reduction'),
                     measure.vars = c("value_billion", "value_per_ghg_million"),
                     variable.name = "unit",
                     value.name = "value")

npv_dt[, measure := fifelse(unit == "value_billion", "NPV (2020 USD billion)", "NPV per avoided GHG MtCO2e\n(2020 USD million / MtCO2e")]

npv_dt <- npv_dt[target != 'BAU']

npv_dt$target <- factor(npv_dt$target, levels = c('1000ft setback GHG', '2500ft setback GHG', '5280ft setback GHG',
                                                        '90% GHG reduction'))

## fig
fig_benefit_x_metric <- ggplot(npv_dt %>% filter(target != 'BAU'), aes(x = ghg_2045_perc_reduction, y = value, color = target, shape = policy_intervention)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(color = "GHG emission target",
       shape = "Policy intervention",
       y = NULL,
       x = 'GHG emissions reduction in 2045 (% of 2019)') +
  facet_grid(measure~title, scales = "free_y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(-15, 10)) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c('1000ft setback GHG' = "#A3A500",
                                '2500ft setback GHG' = '#00BF7D',
                                '5280ft setback GHG' = "#00B0F6",
                                '90% GHG reduction' = "#E76BF3"
                                # , 'BAU' = '#F8766D'
                                )) +
  # scale_y_continuous(labels = comma) +
  theme_line +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"

ggsave(fig_benefit_x_metric,
       filename = file.path(main_path, fig_path, 'figure3a.png'),
       width = 8,
       height = 8,
       units = "in")

