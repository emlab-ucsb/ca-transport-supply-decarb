# plot of dual axes (refined product, ghg emissions) over time for refining
# meas meng and tracey mangin
# created: 2023-02-16
# taken a lot from fig_refined_prod_x_time.R

## paths ------
# main_path   = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
main_path     = '/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn' # meas path
fig_path      = file.path(main_path, 'outputs/academic-out/refining/figures/2022-12-update')
prod_path     = 'outputs/predict-production/refining_2021-11-22/CUF0.6/'
prod_file     = 'state_GJD_and_reGJD_production_all_refineries.csv'
ghg_path      = 'outputs/academic-out/refining/figures'
ghg_file      = 'state_levels_subset_refining.csv'

## libraries -----
library(data.table)

## source figure themes -----
items <- "figure_themes.R"
walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## read in production file ------
prod_df = fread(file.path(main_path, prod_path, prod_file))

## read in ghg emissions file ------
ghg_df = fread(file.path(main_path, ghg_path, ghg_file))
ghg_df = ghg_df[metric == "total_state_ghg_MtCO2"]
ghg_df = ghg_df[oil_price_scenario == 'reference case' & carbon_price_scenario == 'price floor' & ccs_scenario == 'no ccs']
ghg_df = ghg_df[, .(demand_scenario, refining_scenario, year, metric, value)]
ghg_df[, label := 'GHG emissions']

## change demand scenario name -----
prod_df[, demand_scenario_adj := ifelse(demand_scenario == 'BAU', 'Reference Demand', 'Low Carbon Demand')]
ghg_df[, demand_scenario_adj := ifelse(demand_scenario == 'BAU', 'Reference Demand', 'Low Carbon Demand')]

## add fuel after 'jet' ------
prod_df[, fuel_adj := ifelse(fuel == 'jet', 'jet fuel', fuel)]

## factor -------

## rename fuel ----
prod_df[fuel_adj == 'drop-in gasoline', fuel_adj := 'renewable gasoline']

## capitalize first letter of fuel ----
prod_df[, fuel_adj := str_to_title(fuel_adj)]

## factor fuel -----
prod_df[, fuel_adj := factor(fuel_adj, levels = rev(c('Gasoline',
                                                      'Renewable Gasoline',
                                                      'Diesel',
                                                      'Renewable Diesel',
                                                      'Jet Fuel',
                                                      'Sustainable Aviation Fuel',
                                                      'Exports')))]

## refactor refining scenario -----
prod_df[, refining_scenario := factor(refining_scenario, levels = c('historic production', 
                                                                    'historic exports', 
                                                                    'low exports'))]
ghg_df[, refining_scenario := factor(refining_scenario, levels = c('historic production', 
                                                                   'historic exports', 
                                                                   'low exports'))]

## refactor demand scenario -----
prod_df[, demand_scenario_adj := factor(demand_scenario_adj, levels = c('Reference Demand', 'Low Carbon Demand'))]
ghg_df[, demand_scenario_adj := factor(demand_scenario_adj, levels = c('Reference Demand', 'Low Carbon Demand'))]

## theme ------
theme_line = theme_ipsum(base_family = 'Arial',
                         grid = '', 
                         plot_title_size = 18, 
                         subtitle_size = 16,
                         axis_title_just = 'center',
                         axis_title_size = 16, 
                         axis_text_size = 16,
                         strip_text_size = 16)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 14, color = '#5c5c5c', face = 'plain'),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length.y = unit(0.2, 'cm'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 16, vjust = 0.5),
        legend.text = element_text(size = 16, vjust = 0.5),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))

pal_fuel_title = c('Sustainable Aviation Fuel'	=	'#D3756B',
                   'Jet Fuel'	=	'#FFC3A1',
                   'Renewable Diesel'	=	'#366BA1',
                   'Diesel'	=	'#ABC7E3',
                   'Renewable Gasoline'	=	'#3C6255',
                   'Gasoline'	=	'#A6BB8D',
                   'Exports' = '#dadbdf')
pal_label = c('GHG emissions' = '#000000')

## figure ------

coef = 19

fig_fuel_demand_tot = ggplot() +
  geom_area(data = prod_df, aes(x = year, y = consumption_bge / 1e6, fill = fuel_adj)) +
  geom_line(data = ghg_df, aes(x = year, y = value * coef, color = label), linewidth = 1.3) + 
  # geom_segment(x = 2019, xend = 2019, y = 0, yend = 750, color = 'black', linetype = 2)  +
  facet_wrap(demand_scenario_adj ~ refining_scenario, ncol = 3,
             labeller = labeller(refining_scenario = c('historic exports' = 'Historic Exports',
                                                       'historic production' = 'Historic Production',
                                                       'low exports' = 'Low Exports'))) +
  labs(title = NULL,
       x = NULL,
       y = 'Million barrels of gasoline equivalent',
       fill = NULL,
       linetype = NULL,
       color = NULL) +
  scale_x_continuous(breaks = seq(2015,2040,5), limits = c(2014, 2045), expand = c(0,0)) +
  scale_y_continuous(name = 'Fuel production (Million barrels of gasoline equivalent)',
                     sec.axis = sec_axis(~./coef, name = bquote(GHG~emissions~(MtCO[2]))), 
                     expand = c(0,0), 
                     breaks = seq(0, 700, 100)) + 
  scale_fill_manual(values = pal_fuel_title,
                    guide = guide_legend(reverse = TRUE, nrow = 2)) + 
  scale_color_manual(values = pal_label) +
  theme_line +
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 25, vjust = 0.5),
        legend.text = element_text(size = 25, vjust = 0.5),
        axis.title.y = element_text(size = 26),
        axis.title.y.right = element_text(size = 26),
        strip.text = element_text(size = 25),
        plot.subtitle = element_text(size = 25),
        plot.title = element_text(size = 25),
        plot.caption = element_text(size = 25),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) 
fig_fuel_demand_tot

ggsave(fig_fuel_demand_tot, 
       filename = file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.png'), 
       width = 20, 
       height = 12,
       dpi = 600)

ggsave(fig_fuel_demand_tot, 
       filename = file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.pdf'), 
       width = 20, 
       height = 12)

embed_fonts(file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.pdf'),
            outfile = file.path(fig_path, 'state_GJD_and_reGJD_production_and_ghg_emissions.pdf'))
