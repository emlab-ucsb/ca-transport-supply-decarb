## Tracey Mangin
## February 16, 2022
## SI - modeled outputs

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
fig_path <- 'outputs/academic-out/extraction/figures/all-oil-px/'
save_path <- 'outputs/academic-out/extraction/figures/si-figs/'

## csv names
levels_name <- 'state_levels_all_oil.csv'

## read in data
levels_dt <- fread(paste0(main_path, fig_path, levels_name))

## filter for BAU, oil price scens
levels_dt_oilpx <- levels_dt[policy_intervention == 'BAU']
levels_dt_oilpx[, oil_name := paste0('EIA ', oil_price_scenario)]

## factor
levels_dt_oilpx$oil_name <- factor(levels_dt_oilpx$oil_name , levels = c("EIA low oil price", "EIA reference case", "EIA high oil price"))


## plot production
prod_oil_fig <- ggplot(levels_dt_oilpx %>% filter(metric == "total_state_bbl",
                                           year > 2019), aes(x = year, y = value / 1e6, color = oil_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = "Oil production (million bbls)",
       color = NULL) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(prod_oil_fig,
       filename = file.path(main_path, save_path, 'si-oil-prod-oilpx.png'),
       width = 5,
       height = 4,
       units = "in")

## GHG
## ------------------------------------------

## plot ghg
ghg_oil_fig <- ggplot(levels_dt_oilpx %>% filter(metric == "total_state_ghg_MtCO2",
                                            year > 2019), aes(x = year, y = value, color = oil_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = expression(paste("Million metric tonnes of ", CO[2], "e")),
       color = NULL) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
   theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(ghg_oil_fig,
       filename = file.path(main_path, save_path, 'si-ghg-oilpx.png'),
       width = 5,
       height = 4,
       units = "in")

## carbon price pathway outputs
## filter for BAU, carbon price scenarios

# levels_dt_carbon <- levels_dt[oil_price_scenario == 'reference case' &
#                               carbon_price_scenario %in% c("")]



levels_dt_carbon[, oil_name := paste0('EIA ', oil_price_scenario)]

## factor
levels_dt$oil_name <- factor(levels_dt$oil_name , levels = c("EIA low oil price", "EIA reference case", "EIA high oil price"))


## setback distances
## filter for BAU, setback distances

levels_dt_sb <- levels_dt[oil_price_scenario == 'reference case' &
                          carbon_price_scenario %in% c("price floor") &
                          excise_tax_scenario == "no tax"]

levels_dt_sb[, sb_name := fifelse(setback_scenario == 'no_setback', '0ft setback',
                                  fifelse(setback_scenario == 'setback_1000ft', '1000ft setback',
                                          fifelse(setback_scenario == 'setback_2500ft', '2500ft setback', '5280ft setback')))]


## factor
levels_dt_sb$sb_name <- factor(levels_dt_sb$sb_name, 
                               levels = c("0ft setback", "1000ft setback", "2500ft setback", "5280ft setback"))


## plot ghg
setback_fig <- ggplot(levels_dt_sb %>% filter(metric == "total_state_bbl",
                                                 year > 2019), aes(x = year, y = value / 1e6, color = sb_name)) +
  geom_line(size = 0.8, alpha = 0.9) +
  labs(x = NULL,
       y = 'Oil production (million bbls)',
       color = NULL) +
  # scale_color_manual(values = policy_colors_subset) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  # scale_x_continuous(breaks = c(1977, seq(1980, 2045, by = 5))) +
  theme_line +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        legend.box="vertical") 

ggsave(setback_fig,
       filename = file.path(main_path, save_path, 'si-prod-setback.png'),
       width = 5,
       height = 4,
       units = "in")

