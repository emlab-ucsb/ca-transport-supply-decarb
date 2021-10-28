## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)

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
refining_folder_path <- 'outputs/academic-out/refining/refining_2021-10-27/'
state_save_path     = paste0(main_path, refining_folder_path, 'state-results/')

## create a folder to store outputs
cur_date              = Sys.Date()
save_info_path        = paste0(main_path, 'outputs/academic-out/refining/refining-figs/', paste0("figs_", cur_date))
dir.create(save_info_path, showWarnings = FALSE)  
dir.create(paste0(save_info_path, "/cumulative_v1"), showWarnings = FALSE) 
dir.create(paste0(save_info_path, "/cumulative_v2"), showWarnings = FALSE) 
dir.create(paste0(save_info_path, "/labor_v_mortality"), showWarnings = FALSE) 
dir.create(paste0(save_info_path, "/pathway"), showWarnings = FALSE) 

## read inputs
state_out <- fread(paste0(state_save_path, "subset_state_results.csv"))