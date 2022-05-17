## Tracey Mangin
## May 17, 2022
## Refined products over time figure, adapted from refining_module_net.R

## libraries
library(tidyverse)
library(data.table)

## paths
main_path   <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
input_path  <- 'outputs/predict-production/refining_2021-11-22/CUF0.6/'
fig_path    <- paste0(main_path, 'outputs/academic-out/refining/figures/')

## source figure themes
items <- "figure_themes.R"
walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

## inputs
prod_file   <- 'state_GJD_and_reGJD_production_all_refineries.csv'

## read in prod file
prod_df <- fread(paste0(main_path, input_path, prod_file))

## change demand scenario name
prod_df[, demand_scenario_adj := ifelse(demand_scenario == 'BAU', 'BAU Demand', 'LC Demand')]

## add fuel after 'jet'
prod_df[, fuel_adj := ifelse(fuel == 'jet', 'jet fuel', fuel)]

## factor
prod_df$refining_scenario <- factor(prod_df$refining_scenario, levels = c('historic production', 
                                                                          'historic exports', 
                                                                          'low exports'))

## capitalize first letter
prod_df[, fuel_adj := str_to_sentence(fuel_adj)]

## factor
prod_df$fuel_adj <- factor(prod_df$fuel_adj, levels = rev(c('Gasoline',
                                                'Drop-in gasoline',
                                                'Diesel',
                                                'Renewable diesel',
                                                'Jet fuel',
                                                'Sustainable aviation fuel',
                                                'Exports')))

## figure
fig_fuel_demand_tot = ggplot(prod_df, aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)) +
  geom_area() +
  facet_wrap(demand_scenario_adj ~ refining_scenario, ncol = 3,
             labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                       'historic production' = 'Historic Production',
                                                       'low exports' = 'Low Exports'))) +
  labs(title = 'Annual production at all refineries by fuel (2014-2045)',
       subtitle = 'Million barrels of gasoline equivalent',
       x = NULL,
       y = NULL,
       fill = NULL,
       linetype = NULL) +
  scale_x_continuous(breaks = seq(2015,2040,5), limits = c(2014, 2045), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) + 
  scale_fill_manual(values = pal_fuel_title,
                    guide = guide_legend(reverse = TRUE, nrow = 1)) + 
  geom_segment(x = 2019, xend = 2019, y = 0, yend = 750, color = 'black', linetype = 2)  +
  theme_line +
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 14, vjust = 0.5),
        legend.text = element_text(size = 14, vjust = 0.5),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) 

ggsave(fig_fuel_demand_tot, 
       filename = file.path(fig_path, 'state_GJD_and_reGJD_production_all_refineries.png'), 
       width = 20, 
       height = 12)

ggsave(fig_fuel_demand_tot, 
       filename = file.path(fig_path, 'state_GJD_and_reGJD_production_all_refineries.pdf'), 
       width = 20, 
       height = 12)

embed_fonts(file.path(fig_path, 'state_GJD_and_reGJD_production_all_refineries.pdf'),
            outfile = file.path(fig_path, 'state_GJD_and_reGJD_production_all_refineries.pdf'))


