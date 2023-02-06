# comparing ITS fuel demand and refined fuel produced
# created: may 16, 2022
# author: tracey mangin and meas meng

# ------------------ paths -------------------
ref_results_date <- '2021-11-22'

main_path            <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
refining_folder_path <- paste0(main_path, 'outputs/predict-production/refining_', ref_results_date, '/CUF0.6/outputs/')
data_path            <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/fuel-demand/prelim-results'

# ------------------ inputs ------------------
res_file        = 'refining_scenario_outputs_refinery_net_exports_revised.csv'
its_file        = 'its_demand_bau_and_lc1_2020_2045.csv'
jet_file        = 'its_demand_intrastate_jet_2020_2045.csv'
inter_file      = 'cec_jet_fuel_demand_incl_military_forecasted_2020_2045.csv'

# ------------------ outputs ------------------

# fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model'
# fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model'
fig_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/figures'

# libraries --------

library(data.table)
library(stringr) 
library(ggplot2)
library(hrbrthemes)
library(extrafont)

# source items ------

# source ccs emissions mean b calculation script
items = list.files(here::here('src'))
sapply(here::here('src', items), source)

# # source fig themes
# items <- "figure_themes.R"
# 
# walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items


# load results --------
dt_res <- fread(paste0(refining_folder_path, res_file))
# dt_res = fread(file.path(res_path, res_file), header = T)
dt_its = fread(file.path(data_path, its_file), header = T)
dt_jet = fread(file.path(data_path, jet_file), header = T)
dt_inter = fread(file.path(data_path, inter_file), header = T)

# get selected scenarios --------

dt_res[demand_scenario == 'BAU' & 
         refining_scenario == 'historic production' &
         innovation_scenario == 'low innovation' &
         ccs_scenario == 'no ccs' &
         carbon_price_scenario == 'price floor', scenario := 'BAU Demand']
dt_res[demand_scenario == 'LC1' & 
         refining_scenario == 'historic production' &
         innovation_scenario == 'low innovation' &
         ccs_scenario == 'no ccs' &
         carbon_price_scenario == 'price floor', scenario := 'LC Demand']
dt_res = dt_res[!is.na(scenario)]
# dt_res = dt_res[!fuel == 'exports']

## filter for production?
# dt_res = dt_res[type == 'consumption']
dt_res = dt_res[type == 'production']

dt_res = unique(dt_res[, .(demand_scenario, year, fuel, source, type, units, value)])

dt_inter = dt_inter[scenario == 'Mid Case']

# calculate bge ------

dt_inter[, consumption_bge := total_jet_fuel_demand_gge/42]
dt_its[, consumption_bge := consumption_gge/42]
dt_jet[, consumption_bge := consumption_gge/42]
dt_jet[, j := 1]

# unique set of scenarios -------

un_scens = CJ(year = 2017:2050,
              scenario = c('BAU Demand', 'LC Demand'),
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
dt_jet_all[fuel == 'intra_consumption_bge', fuel := 'jet (intrastate)']
dt_jet_all[fuel == 'inter_consumption_bge', fuel := 'jet (interstate + military)']

# combine demand ------

dt_demand = rbindlist(list(dt_its[, .(scenario, year, fuel, consumption_bge)],
                           dt_jet_all[, .(scenario, year, fuel, consumption_bge)]))
setorder(dt_demand, scenario, year, fuel)

# rename fuel ------

dt_demand[fuel == 'drop-in gasoline', fuel := 'renewable gasoline']
dt_demand[, fuel := str_to_title(fuel)]
dt_demand[, fuel := gsub('Ldv', 'LDV', fuel)]
dt_demand[, fuel := gsub('Hdv', 'HDV', fuel)]

# reorder factor levels ------

# dt_res[, scenario := factor(scenario, levels = c('R-BAU', 'LCR1', 'LCR2'))]
dt_res[, fuel := factor(fuel, levels = rev(c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel', 'exports')))]
setnames(dt_res, 'demand_scenario', 'scenario')
dt_demand[, fuel := factor(fuel, levels = rev(c('Gasoline', 'Renewable Gasoline', 'Diesel', 'Renewable Diesel', 
                                                'Jet (Intrastate)', 'Sustainable Aviation Fuel', 'Jet (Interstate + Military)',
                                                'Ethanol', 'Biodiesel', 'Renewable Natural Gas', 'LDV Hydrogen', 'HDV Hydrogen', 'LDV Electricity', 'HDV Electricity')))]

# get line of ITS fuels included -------

inc_its = rbindlist(list(dt_res[fuel %in% c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'sustainable aviation fuel')],
                         dt_demand[fuel == 'Jet (Intrastate)']),
                    use.names = T, fill = T)
inc_its = inc_its[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(scenario, year)]

# get line of total fuels included (not exports) ---------

inc_full = dt_res[fuel %in% c('gasoline', 'drop-in gasoline', 'diesel', 'renewable diesel', 'jet', 'sustainable aviation fuel')]
inc_full = inc_full[, .(consumption_bge = sum(consumption_bge, na.rm = T)), by = .(scenario, year)]

# ------------------------- plots ------------------------- 

# plot theme & palettes ------

# pal_fuel = c('HDV Hydrogen'	=	'#fcc3bb',
#              'LDV Hydrogen'	=	'#fb8c7e',
#              'HDV Electricity'	=	'#e58ca3',
#              'LDV Electricity'	=	'#d6476b',
#              'Renewable Natural Gas'	=	'#ad647c',
#              'Biodiesel'	=	'#7a1134',
#              'Ethanol'	=	'#dadbdf',
#              'Jet (Interstate)' = '#61c8c1',
#              'Sustainable Aviation Fuel'	=	'#169486',
#              'Jet (Intrastate)'	=	'#106c66',
#              'Renewable Diesel'	=	'#599aa8',
#              'Diesel'	=	'#0f5e74',
#              'Drop-In Gasoline'	=	'#4bbbe6',
#              'Gasoline'	=	'#0f7ac3')
pal_fuel = c('Gasoline' = '#FF6E1B', 
             'Renewable Gasoline' = '#ffb286', 
             'Diesel' = '#E44C9A', 
             'Renewable Diesel' = '#e59cc2', 
             'Jet (Intrastate)' = '#00778B', 
             'Sustainable Aviation Fuel' = '#00A3AD',
             'Jet (Interstate + Military)' = '#8cbac4',
             'HDV Hydrogen'	=	'#BEB6AF',
             'LDV Hydrogen'	=	'#DBD5CD',
             'HDV Electricity'	=	'#7C7E7F',
             'LDV Electricity'	=	'#8F8884',
             'Renewable Natural Gas'	=	'#B4975A',
             'Biodiesel'	=	'#005581',
             'Ethanol'	=	'#FFD200')

labs_fuel = c('Gasoline' = 'Gasoline', 
              'Renewable Gasoline' = 'Renewable gasoline', 
              'Diesel' = 'Diesel', 
              'Renewable Diesel' = 'Renewable diesel', 
              'Jet (Intrastate)' = 'Jet (intrastate)', 
              'Sustainable Aviation Fuel' = 'Sustainable aviation fuel',
              'Jet (Interstate + Military)' = 'Jet (interstate + military)',
              'HDV Hydrogen'	=	'HDV hydrogen',
              'LDV Hydrogen'	=	'LDV hydrogen',
              'HDV Electricity'	=	'HDV electricity',
              'LDV Electricity'	=	'LDV electricity',
              'Renewable Natural Gas'	=	'Renewable natural gas',
              'Biodiesel'	=	'Biodiesel',
              'Ethanol'	=	'Ethanol')

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
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5, face = 'bold'),
        plot.margin = unit(c(1,3,1,1), 'lines'))

# plot: area chart of fuel demand ---------

fig_demand = ggplot() +
  geom_area(data = dt_demand, aes(x = year, y = consumption_bge/1e6, fill = fuel, group = fuel)) +
  geom_line(data = inc_its, aes(x = year, y = consumption_bge/1e6, lty = 'its'), size = 1, color = 'black') +
  geom_line(data = inc_full, aes(x = year, y = consumption_bge/1e6, lty = 'all'), size = 1, color = 'black') +
  facet_wrap(. ~ scenario, ncol = 1) +
  labs(title = NULL,
       subtitle = NULL,
       x = 'Year',
       y = 'Million barrels of gasoline equivalent demanded',
       fill = NULL) +
  scale_x_continuous(breaks = c(2020, seq(2025, 2045, 5)), limits = c(2020, 2045), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 700, 100)) +
  scale_fill_manual(values = pal_fuel, labels = labs_fuel, guide = guide_legend(nrow = 7)) + 
  scale_linetype_manual(name = NULL, 
                        labels = c('its' = 'Study 1 demand included in model', 
                                   'all' = 'Total demand (excluding exports) included in model'),
                        values = c('its' = 3,
                                   'all' = 2),
                        guide = guide_legend(nrow = 2)) +
  theme_line 
fig_demand

ggsave(fig_demand,
       filename = file.path(fig_path, 'its_demand_and_production.png'),
       width = 6.5,
       height = 8,
       dpi = 400, 
       units = 'in', 
       device = 'png')

ggsave(fig_demand,
       filename = file.path(fig_path, 'its_demand_and_production.pdf'),
       width = 6.5,
       height = 8,
       dpi = 400, 
       units = 'in', 
       device = 'pdf')

embed_fonts(file.path(fig_path, 'its_demand_and_production.pdf'),
            outfile = file.path(fig_path, 'its_demand_and_production.pdf'))


