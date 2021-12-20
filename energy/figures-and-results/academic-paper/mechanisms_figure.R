## Tracey Mangin
## December 17, 2021
## Academic paper mechanisms fig

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
levels_file <- 'state_levels_subset.csv'
health_file <- 'field_dt_health.csv'

## read in data
levels_file <- fread(paste0(main_path, fig_path, levels_file))
health_dt <- fread(paste0(main_path, fig_path, health_file))

## health fig, affected pop / total production, annual

health_cluster_dt <- health_dt[, .(total_prod_bbl = sum(total_prod_bbl)), by = .(scen_id, carbon_price_scenario,
                                                                                 setback_scenario, excise_tax_scenario,
                                                                                 id, year, affected_pop, share_dac_weighted)]

health_cluster_dt[, dac_affected_pop := affected_pop * share_dac_weighted]

# ## if production is zero, affected prod == 0
# health_cluster_dt[, affected_cluster := fifelse(total_prod_bbl > 0, 1, 0)]
# health_cluster_dt[, adj_affected_pop := affected_pop * affected_cluster]
# health_cluster_dt[, adj_dac_affected_pop := dac_affected_pop * affected_cluster]


## sum of affected * prod over total production
health_cluster_dt[, affected_pop_prod := affected_pop * total_prod_bbl]
health_cluster_dt[, dac_affected_pop_prod := dac_affected_pop * total_prod_bbl]

## sum by year, divide by total prod
health_annual_dt <- health_cluster_dt[, lapply(.SD, sum, na.rm = T), .SDcols = c("affected_pop_prod", "dac_affected_pop_prod", "total_prod_bbl"), 
                                      by = .(scen_id, carbon_price_scenario, setback_scenario, excise_tax_scenario, year)]


health_annual_dt[, pop_per_bbl := affected_pop_prod / total_prod_bbl]
health_annual_dt[, dac_pop_per_bbl := dac_affected_pop_prod / total_prod_bbl]

## make long for plotting
ppl_per_bbl_dt <- melt(health_annual_dt, id.vars = c('scen_id', 'year'),
                    measure.vars = c("pop_per_bbl", "dac_pop_per_bbl"),
                    variable.name = "population",
                    value.name = "ppl_per_bbl")

ppl_per_bbl_dt[, pop_lab := fifelse(population == "pop_per_bbl", "Total", "DAC")]

ppl_per_bbl_dt$pop_lab <- factor(ppl_per_bbl_dt$pop_lab, levels = c("Total", "DAC"))

ppl_per_bbl_dt[, scen_lab := fifelse(scen_id == "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax", "BAU",
                                     fifelse(scen_id == "reference case_setback_5280ft_no quota_price floor_no ccs_low innovation_no tax", "setback",
                                             fifelse(scen_id == "reference case_no_setback_no quota_carbon_setback_5280ft-no_setback-no ccs_no ccs_low innovation_no tax", "carbon tax", "excise tax")))]


## plot
mechanism_fig1 <- ggplot(ppl_per_bbl_dt, aes(x = year, y = ppl_per_bbl, color = scen_lab)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(title = "Production weighted affected population per bbl produced",
       subtitle = "75% GHG reduction",
       x = NULL,
       y = NULL,
       color = "Policy intervention") +
  facet_wrap(~pop_lab, nrow = 1) +
  scale_color_manual(values = c("BAU" = "black", policy_colors_subset)) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(mechanism_fig1,
       filename = file.path(main_path, fig_path, 'figure_mechanisms.png'),
       width = 6,
       height = 4,
       units = "in")



