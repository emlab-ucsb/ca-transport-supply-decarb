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
ccs_ext_file      = 'ccs_extraction_scenarios_revised.csv' ## revised includes ccs cost = inf
ccs_ref_file      = 'ccs_refining_scenarios.csv'


## read in CCS scenarios
ccs_ext_df <- fread(paste0(scen_path, ccs_ext_file))
ccs_ext_df[, segment := "extraction"]

ccs_ref_df <- fread(paste0(scen_path, ccs_ref_file))
ccs_ref_df[, segment := "refining"]

ccs_prices <- rbind(ccs_ext_df, ccs_ref_df)

labs <- ccs_prices[ccs_scenario != "no ccs" & year == 2020]

## ggplot
ccs_path_fig <- ggplot(ccs_prices %>% filter(ccs_scenario != "no ccs"), aes(x = year, y = ccs_price, color = ccs_scenario)) +
  geom_line(size = 0.75, alpha = 0.8) +
  facet_wrap(~segment) +
  theme_line +
  geom_text(aes(label=ifelse(year == 2020,paste0('$',ccs_price),''), hjust = 0, vjust = 1)) +
  labs(title = "CCS paths",
       # subtitle = "no CCS",
       x = NULL,
       y = "CCS price (USD per mtCO2)",
       color = "CCS scenario") +
  scale_linetype_manual(values = c(3, 1)) +
  theme(legend.position = "right")


ggsave(ccs_path_fig, 
       filename = file.path(save_path, 'ccs_price_paths_fig.png'), 
       width = 8, 
       height = 8)

