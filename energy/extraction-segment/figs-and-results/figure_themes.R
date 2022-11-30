## Tracey Mangin
## September 16, 2021

library(hrbrthemes)

## figure themes
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
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length.y = unit(0.2, 'cm'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 8, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))

## figure themes
theme_line_n = theme_ipsum(base_family = 'Arial',
                         grid = 'Y', 
                         plot_title_size = 7, 
                         subtitle_size = 7,
                         axis_title_just = 'center',
                         axis_title_size = 7, 
                         axis_text_size = 7,
                         strip_text_size = 7)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 7, color = '#5c5c5c', face = 'plain'),
        axis.line.y = element_line(color = 'black'),
        axis.ticks.y = element_line(color = 'black'),
        axis.ticks.length.y = unit(0.7, 'cm'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.7, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 7, vjust = 0.5),
        legend.text = element_text(size = 7, vjust = 0.5),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))


## target colors
target_colors <- c("55%" = "#133a54",
                   "60%" = "#1d577e",
                   "75%" = "#6390ae",
                   "90%" = "#a6bfd1")

policy_colors_all <-  c('BAU' = "black",
                        'carbon tax' = "#fcb97d",
                        'excise tax' = '#ff5e5b',
                        'setback' = '#4a6c6f',
                        'carbon tax & setback' = "#9DBF9E")

policy_colors_subset <-  c('setback (new wells)' = '#4a6c6f',
                           'excise tax' = '#ff5e5b',
                           'carbon tax' = "#fcb97d")

sb_policy_colors_subset <-  c('setback (new wells)' = '#4a6c6f',
                           'setback (all wells)' = '#b5446e')


policy_colors_subset_2sb <- c('setback (new wells)' = '#2a9d8f',
                              'setback (all wells)' = '#4a6c6f',
                              'excise tax' = '#ff5e5b',
                              'carbon tax' = "#fcb97d")



policy_symbols <- c("BAU" = 16,
                    "carbon tax" = 17,
                    "excise tax" = 15,
                    "setback" = 3)

macro_pal <- c("#233d4d", "#fe7f2d", "#fcca46", "#a1c181", "#619b8a")

## pals

## distinct ucsb
ucsb_distinct <- c("#1295D8", "#FFB511", "#FF6E1B", "#E44C9A", "#00778B")

## refining fig colors
# plot theme & palettes ------

pal_scenarios = c('BAU' = '#576b81',
                  'Mid Case' = '#576b81',
                  'LC1' = '#50a727',
                  'Low Case' = '#50a727',
                  'High Case' = '#f05c0b',
                  'crude' = '#f6ac0e',
                  'crude (energy intensity equation)' = '#dd1e24',
                  'crude (regression)' = '#305ff0')

pal_region = c('North' = '#3baeba',
               'South' = '#05426f',
               'Renewables' = '#ffa440')

pal_fuel = c('gasoline' = '#4bbbe6', 
             'drop-in gasoline' = '#c2eaf7', 
             'diesel' = '#fda89c', 
             'renewable diesel' = '#fddad5', 
             'jet' = '#169486', 
             'sustainable aviation fuel' = '#61c8c1',
             'exports' = '#dadbdf')

pal_fuel_title = c('Gasoline' = '#4bbbe6', 
                   'Drop-in gasoline' = '#c2eaf7', 
                   'Diesel' = '#fda89c', 
                   'Renewable diesel' = '#fddad5', 
                   'Jet fuel' = '#169486', 
                   'Sustainable aviation fuel' = '#61c8c1',
                   'Exports' = '#dadbdf')

pal_crude = c('crude (traditional)' = '#096497',
              'crude (residual renewable)' = '#29b9cd',
              'crude (main renewable)' = '#99c1d5',
              'traditional' = '#096497',
              'residual renewable' = '#29b9cd',
              'main renewable' = '#99c1d5')

# pal_refinery = c(unikn::seecol(pal_unikn_pair)[1:16], '#ebd74e')
# names(pal_refinery) = unique(c(dt_refcap[, refinery_name], dt_renref[, refinery_name], 'AltAir Paramount'))




