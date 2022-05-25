## Tracey Mangin
## carbon price paths

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)

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

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
save_path <- paste0(main_path, 'outputs/academic-out/misc/figs')
scen_path <- paste0(main_path, 'project-materials/scenario-inputs/')

## files
carbon_paths <- 'final_carbon_tax_scenarios.csv'

## read in carbon scenarios
carbon_df <- fread(paste0(scen_path, carbon_paths))
carbon_df[, ccs_scenario := sub(".*-", "", carbon_price_scenario)]
carbon_df[, ccs_scenario := fifelse(ccs_scenario %in% c('price floor', 'price ceiling', 'central SCC'), 'no ccs', ccs_scenario)]
carbon_df[, target := fifelse(carbon_price_scenario %in% c('carbon_90_perc_reduction-medium CCS cost',
                                                           'carbon_90_perc_reduction-no ccs'), '90% reduction',
                              fifelse(carbon_price_scenario %in% c('carbon_setback_1000ft-medium CCS cost',
                                                                   'carbon_setback_1000ft-no ccs'), '1000ft setback',
                                      fifelse(carbon_price_scenario %in% c('carbon_setback_2500ft-medium CCS cost',
                                                                           'carbon_setback_2500ft-no ccs'), '2500ft setback',
                                              fifelse(carbon_price_scenario %in% c('carbon_setback_5280ft-medium CCS cost',
                                                                                   'carbon_setback_5280ft-no ccs'), '1 mile setback', carbon_price_scenario))))]




## ggplot
carbon_path_fig <- ggplot(carbon_df, aes(x = year, y = carbon_price, color = target, lty = ccs_scenario)) +
  geom_line(size = 0.75, alpha = 0.8) +
  theme_line +
  # facet_wrap(~ccs_scenario) +
  labs(title = "Carbon price paths",
       # subtitle = "no CCS",
       x = NULL,
       y = "Carbon price (USD per mtCO2)",
       color = "Carbon price scenario") +
  scale_linetype_manual(values = c(3, 1)) +
  theme(legend.position = "right")


ggsave(carbon_path_fig, 
       filename = file.path(save_path, 'carbon_price_paths_fig.png'), 
       width = 8, 
       height = 8)



