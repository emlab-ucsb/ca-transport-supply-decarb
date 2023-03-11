# comparing ITS fuel demand and refined fuel produced (2023 update)
# created: jan 6, 2023
# author: tracey mangin and meas meng

# ------------------ paths -------------------
ref_results_date <- '2022-12-01'

# main_path            <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
main_path             = '/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn' # meas path
refining_folder_path  = file.path(main_path, paste0('outputs/predict-production/refining_', ref_results_date, '/CUF0.6/outputs/'))
data_path             = file.path(main_path, 'outputs/fuel-demand/prelim-results')

# ------------------ inputs ------------------
res_file        = 'refining_scenario_outputs_refinery_net_exports_revised.csv'
its_file        = 'its_demand_bau_and_lc1_2020_2045.csv'
jet_file        = 'its_demand_intrastate_jet_2020_2045.csv'
inter_file      = 'cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv'

# ------------------ outputs ------------------

# fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model'
# fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
fig_path     = file.path(main_path, 'outputs/academic-out/refining/figures/2022-12-update')

# libraries --------

library(data.table)
library(stringr) 
library(ggplot2)
library(hrbrthemes)
library(extrafont)

# load results --------
dt_its = fread(file.path(data_path, its_file), header = T)
dt_jet = fread(file.path(data_path, jet_file), header = T)
dt_inter = fread(file.path(data_path, inter_file), header = T)

# calculate bge ------
dt_inter[, consumption_bge := total_jet_fuel_demand_gge/42]
dt_its[, consumption_bge := consumption_gge/42]
dt_jet[, consumption_bge := consumption_gge/42]
dt_jet[, j := 1]

# unique set of scenarios -------

dt_inter = dt_inter[scenario == 'Mid Case']
un_scens = CJ(year = 2017:2050,
              scenario = c('Reference Demand', 'Low Carbon Demand'),
              j = 1)

# create full set of intrastate jet fuel demand 

dt_intra = dt_jet[un_scens, on = .(j, year), allow.cartesian = T]
dt_intra[, j := NULL]
dt_intra = dt_intra[year >= 2019]

dt_intra_2 = dt_intra[, .(scenario, year, consumption_bge)]
setnames(dt_intra_2, 'consumption_bge', 'intra_consumption_bge')
setorder(dt_intra_2, scenario, year)

# seperate interstate from intrastate demand ------

dt_inter = dt_inter[, .(year, consumption_bge)]
dt_inter[, fuel := 'jet (total)']
dt_inter[, j := 1]

dt_inter = dt_inter[un_scens, on = .(j, year), allow.cartesian = T]
dt_inter[, j := NULL]
dt_inter = dt_inter[year >= 2019 & year <= 2045]

dt_inter_2 = dt_inter[, .(scenario, year, consumption_bge)]
setnames(dt_inter_2, 'consumption_bge', 'total_jet_consumption_bge')
setorder(dt_inter_2, scenario, year)

dt_jet_2 = dt_intra_2[dt_inter_2, on = .(scenario, year)]
dt_jet_2[, inter_consumption_bge := total_jet_consumption_bge - intra_consumption_bge]

dt_jet_all = melt(dt_jet_2, 
                  id.vars = c('scenario', 'year'),
                  measure.vars = c('intra_consumption_bge', 'inter_consumption_bge'),
                  variable.name = 'fuel',
                  value.name = 'consumption_bge')
dt_jet_all[fuel == 'intra_consumption_bge', fuel := 'jet fuel (intrastate)']
dt_jet_all[fuel == 'inter_consumption_bge', fuel := 'jet fuel (interstate + military)']

# combine demand ------

dt_demand = rbindlist(list(dt_its[, .(scenario, year, fuel, consumption_bge)],
                           dt_jet_all[, .(scenario, year, fuel, consumption_bge)]))
setorder(dt_demand, scenario, year, fuel)

# rename fuel ------

dt_demand[fuel == 'drop-in gasoline', fuel := 'renewable gasoline']
dt_demand[, fuel := str_to_title(fuel)]
dt_demand[, fuel := gsub('Ldv', 'LDV', fuel)]
dt_demand[, fuel := gsub('Hdv', 'HDV', fuel)]

# rename scenario -------
dt_demand[scenario == 'BAU', scenario := 'Reference Demand']
dt_demand[scenario == 'LC1', scenario := 'Low Carbon Demand']

# reorder factor levels ------

dt_demand[, fuel := factor(fuel, levels = rev(c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                                'Jet Fuel (Intrastate)', 'Sustainable Aviation Fuel', 'Jet Fuel (Interstate + Military)',
                                                'Ethanol', 'Biodiesel', 'Renewable Natural Gas', 'LDV Hydrogen', 'HDV Hydrogen', 'LDV Electricity', 'HDV Electricity')))]


# get line of Total intrastate transportation liquid fuels demand included -------

inc_its = dt_demand[fuel %in% c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                'Jet Fuel (Intrastate)', 'Sustainable Aviation Fuel')]
inc_its = inc_its[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(scenario, year)]
inc_its[scenario == 'BAU', scenario := 'Reference Demand']
inc_its[scenario == 'LC1', scenario := 'Low Carbon Demand']

# get line of total fuels included (not exports) ---------

inc_full = dt_demand[fuel %in% c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                'Jet Fuel (Intrastate)', 'Sustainable Aviation Fuel', 'Jet Fuel (Interstate + Military)')]
inc_full = inc_full[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(scenario, year)]
inc_full[scenario == 'BAU', scenario := 'Reference Demand']
inc_full[scenario == 'LC1', scenario := 'Low Carbon Demand']

# refactor scenario -------

dt_demand[, scenario := factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))]
inc_its[, scenario := factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))]
inc_full[, scenario := factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))]

# ------------------------- plots ------------------------- 

# plot theme & palettes ------

# pal_fuel = c('HDV Hydrogen'	=	'#fcc3bb',
#              'LDV Hydrogen'	=	'#fb8c7e',
#              'HDV Electricity'	=	'#e58ca3',
#              'LDV Electricity'	=	'#d6476b',
#              'Renewable Natural Gas'	=	'#ad647c',
#              'Biodiesel'	=	'#7a1134',
#              'Ethanol'	=	'#dadbdf',
#              'Jet Fuel (Interstate + Military)' = '#61c8c1',
#              'Sustainable Aviation Fuel'	=	'#169486',
#              'Jet Fuel (Intrastate)'	=	'#106c66',
#              'Renewable Diesel'	=	'#599aa8',
#              'Diesel'	=	'#0f5e74',
#              'Renewable Gasoline'	=	'#4bbbe6',
#              'Gasoline'	=	'#0f7ac3')

# pal_fuel = c('HDV Hydrogen'	=	'#fbb4ae',
#              'LDV Hydrogen'	=	'#b3cde3',
#              'HDV Electricity'	=	'#ccebc5',
#              'LDV Electricity'	=	'#decbe4',
#              'Renewable Natural Gas'	=	'#fed9a6',
#              'Biodiesel'	=	'#ffffcc',
#              'Ethanol'	=	'#e5d8bd',
#              'Jet Fuel (Interstate + Military)' = '#eea320',
#              'Sustainable Aviation Fuel'	=	'#6dbbbf',
#              'Jet Fuel (Intrastate)'	=	'#8da0ab',
#              'Renewable Diesel'	=	'#eb8f73',
#              'Diesel'	=	'#008a84',
#              'Renewable Gasoline'	=	'#f15a40',
#              'Gasoline'	=	'#00526d')

pal_fuel = c('HDV Hydrogen'	=	'#fbb4ae',
             'LDV Hydrogen'	=	'#b3cde3',
             'HDV Electricity'	=	'#ccebc5',
             'LDV Electricity'	=	'#decbe4',
             'Renewable Natural Gas'	=	'#fed9a6',
             'Biodiesel'	=	'#ffffcc',
             'Ethanol'	=	'#e5d8bd',
             'Jet Fuel (Interstate + Military)' = '#A75D5D',
             'Sustainable Aviation Fuel'	=	'#D3756B',
             'Jet Fuel (Intrastate)'	=	'#FFC3A1',
             'Renewable Diesel'	=	'#366BA1',
             'Diesel'	=	'#ABC7E3',
             'Renewable Gasoline'	=	'#3C6255',
             'Gasoline'	=	'#A6BB8D')

theme_line = theme_ipsum(base_family = 'Arial',
                         grid = 'Y', 
                         plot_title_size = 10, 
                         subtitle_size = 9,
                         axis_title_just = 'center',
                         axis_title_size = 9, 
                         axis_text_size = 9,
                         strip_text_size = 9)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 8, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5),
        legend.position = 'bottom',
        strip.text = element_text(hjust = 0.5, face = 'bold'),
        plot.margin = unit(c(1,3,1,1), 'lines'),
        plot.background = element_rect(color = 'white'))



# plot: area chart of fuel demand ---------

fig_demand = ggplot() +
  geom_area(data = dt_demand, aes(x = year, y = consumption_bge/1e6, fill = fuel, group = fuel)) +
  geom_line(data = inc_its, aes(x = year, y = consumption_bge/1e6, lty = 'its'), linewidth = 1, color = 'black') +
  geom_line(data = inc_full, aes(x = year, y = consumption_bge/1e6, lty = 'all'), linewidth = 1, color = 'black') +
  facet_wrap(~ scenario, nrow = 2) +
  # facet_wrap(vars(scenario)) +
  # facet_grid(~factor(scenario, levels = c('Reference Demand', 'Low Carbon Demand'))) +
  labs(title = NULL,
       subtitle = NULL,
       x = 'Year',
       y = 'Million barrels of gasoline equivalent demanded',
       fill = NULL) +
  scale_x_continuous(breaks = c(2020, seq(2025, 2045, 5)), limits = c(2020, 2045), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) +
  scale_fill_manual(values = pal_fuel, guide = guide_legend(nrow = 7)) + 
  scale_linetype_manual(name = NULL, 
                        labels = c('its' = 'Total intrastate transportation\nliquid fuels demand supplied by oil refineries', 
                                   'all' = 'Total transportation liquid fuels\ndemand including interstate and military aviation'),
                        values = c('its' = 3,
                                   'all' = 2),
                        guide = guide_legend(nrow = 2)) +
  theme_line 
fig_demand

ggsave(fig_demand,
       filename = file.path(fig_path, 'its_demand_and_production_2023.png'),
       width = 6.5,
       height = 8,
       dpi = 400, 
       units = 'in', 
       device = 'png')

ggsave(fig_demand,
       filename = file.path(fig_path, 'its_demand_and_production_2023.pdf'),
       width = 6.5,
       height = 8,
       dpi = 400, 
       units = 'in', 
       device = 'pdf')

embed_fonts(file.path(fig_path, 'its_demand_and_production_2023.pdf'),
            outfile = file.path(fig_path, 'its_demand_and_production_2023.pdf'))
