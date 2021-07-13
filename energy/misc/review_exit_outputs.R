## Tracey Mangin
## July 12, 2021
## Review exit results

library(tidyverse)
library(data.table)

## set paths and file names
model_out_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
run_folder          = 'extraction_2021-07-12/revised-incorporate-exit-v2'
exit_fname          = 'diagnostic-exit-results.csv'
save_info_path      = paste0(model_out_path, "/", run_folder, '/diagnostic-figs/exit-figs-x-field')

## read in outputs
exit_out <- fread(file.path(model_out_path, run_folder, exit_fname), header = T, colClasses = c('doc_field_code' = 'character'))

exit_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                               fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

exit_out2 <- exit_out %>%
  mutate(well_type = ifelse(vintage == "new", "new", "existing")) %>%
  group_by(scen_name, doc_field_code, doc_fieldname, well_type, year, n_well_exit) %>%
  summarise(num_wells_start = sum(adj_no_wells),
            num_wells_after_exit = sum(no_wells_after_exit)) %>%
  ungroup() %>%
  arrange(scen_name, doc_field_code, well_type, year) 

exit_all <- exit_out %>%
  mutate(well_type = "all_wells") %>%
  group_by(scen_name, doc_field_code, doc_fieldname, well_type, year, n_well_exit) %>%
  summarise(num_wells_start = sum(adj_no_wells),
            num_wells_after_exit = sum(no_wells_after_exit)) %>%
  ungroup() %>%
  arrange(scen_name, doc_field_code, well_type, year) %>%
  rbind(exit_out2) %>%
  select(-num_wells_start) %>%
  mutate(well_type = ifelse(well_type == "all_wells", "All wells",
                            ifelse(well_type == "existing", "Existing wells", "New wells")))


## theme
theme_line =  
  theme_ipsum(base_family = 'Arial',
              grid = 'Y',
              plot_title_size = 12,
              subtitle_size = 10,
              axis_title_just = 'center',
              axis_title_size = 10,
              axis_text_size = 10,
              strip_text_size = 10)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
        axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.25, "cm"),
        axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
        plot.margin = unit(c(1,2,1,1), "lines"),
        strip.text = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.position = 'bottom',
  )


## function
plot_wells <- function(field) {
  
  fieldn <- field
  
  field_exit_df <- exit_all %>% filter(doc_fieldname == fieldn,
                                      well_type == "All wells")

  exit_fig <- 
    ggplot(field_exit_df, aes(x = year, y = num_wells_after_exit, group = scen_name, color = scen_name, lty = scen_name)) +
    geom_line(alpha = 0.9) +
    labs(x = NULL,
         y = NULL,
         title = fieldn,
         subtitle = "# of wells") +
    scale_color_manual(values = c("#FF6E1B", "#FFD200", "#005581")) +
    theme_bw() +
    theme_line +
    theme(legend.title = element_blank(),
          legend.position = "bottom")

  # save figures ----
  well_fname = paste0(fieldn, '_well_exits.pdf')
  ggsave(exit_fig,
         filename = file.path(save_info_path, well_fname),
         width = 6,
         height = 4)
  
  embed_fonts(file.path(save_info_path, well_fname),
              outfile = file.path(save_info_path, well_fname))

}

field_names <- unique(exit_all$doc_fieldname)

purrr::map(field_names, plot_wells)
  
  