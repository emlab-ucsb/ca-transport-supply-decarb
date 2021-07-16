## Tracey Mangin
## July 12, 2021
## Review exit results

library(tidyverse)
library(data.table)
library(hrbrthemes)
library(scales)
library(extrafont)
library(ggrepel)

## set paths and file names
model_out_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/'
run_folder          = 'extraction_2021-07-15/revised-edit-exit-model/'
exit_fname          = 'diagnostic-exit-results.csv'
field_fname         = "diagnostic-field-level-results.csv"
save_exit_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/exit/'
exit_file           = 'well_exits_under_rule_forecast.csv'
save_info_path      = paste0(model_out_path, "/", run_folder, 'diagnostic-figs/exit-figs-x-field')


## new well entry by field
field_out <- fread(paste0(model_out_path, run_folder, field_fname), header = T, colClasses = c('doc_field_code' = 'character'))
field_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                 fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

new_wells <- field_out[, .(scen_name, doc_field_code, doc_fieldname, year, new_wells)]

## previous exit
prev_exits <- fread(paste0(save_exit_path, exit_file), header = T, colClasses = c('doc_field_code' = 'character'))

## all fields and exit years
prev_exits2 <- expand.grid(doc_field_code = unique(prev_exits$doc_field_code),
                          setback_scenario = unique(prev_exits$setback_scenario),
                          exit_year = seq(from = 2020, to = 2045, by = 1)) %>%
  left_join(prev_exits) %>%
  select(-doc_fieldname) %>%
  mutate(no_exits_field = ifelse(is.na(no_exits_field), 0, no_exits_field))

## state level
prev_exits_state <- prev_exits2 %>%
  group_by(setback_scenario, year = exit_year) %>%
  summarise(sum_exits = sum(no_exits_field)) %>%
  ungroup() %>%
  mutate(scen_name = NA,
         version = "exit-rule") %>%
  select(scen_name, setback_scenario, year, n_exits = sum_exits, version)


## read in outputs
exit_out <- fread(file.path(model_out_path, run_folder, exit_fname), header = T, colClasses = c('doc_field_code' = 'character'))

exit_out <- exit_out[setback_scenario %in% c("no_setback", "setback_1000ft", "setback_2500ft")]

exit_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                               fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

# ## compare 2020 predicted well exit
# pred_2020 <- exit_out[year == 2020, .(scen_name, doc_field_code, orig_year, orig_year, vintage, start_year, n_well_exit)]
# 
# pred_2020_2 <- pred_2020 %>%
#   pivot_wider(names_from = scen_name, values_from = n_well_exit) %>%
#   mutate(same = ifelse(BAU == LCE1 & BAU == LCE2, 1, 0))
# ## prediction is the same
# 
# ## no wells after exit... same?
# nafter <- pred_2020 <- exit_out[year == 2020, .(scen_name, doc_field_code, orig_year, vintage, start_year, no_wells_after_exit)]
# 
# nafter2 <- nafter %>%
#   pivot_wider(names_from = scen_name, values_from = no_wells_after_exit) %>%
#   mutate(bau_lce1 = ifelse(BAU == LCE1, 1, 0))

# ###
# exit_out2 <- exit_out %>%
#   mutate(well_type = ifelse(vintage == "new", "new", "existing")) %>%
#   group_by(scen_name, doc_field_code, doc_fieldname, well_type, year, n_well_exit) %>%
#   summarise(num_wells_start = sum(adj_no_wells),
#             num_wells_after_exit = sum(no_wells_after_exit)) %>%
#   ungroup() %>%
#   arrange(scen_name, doc_field_code, well_type, year) 

exit_all <- exit_out %>%
  select(scen_name, doc_field_code, doc_fieldname, setback_scenario, year, orig_year, vintage, start_year, adj_no_wells, no_wells_after_exit) %>%
  group_by(scen_name, doc_field_code, doc_fieldname, setback_scenario, vintage, start_year) %>%
  mutate(prev_n_wells = ifelse(start_year >= 2020 & start_year == orig_year, adj_no_wells, 
                               ifelse(start_year < 2020 & orig_year == 2020, adj_no_wells, lag(no_wells_after_exit))),
         n_exits = prev_n_wells - no_wells_after_exit) %>%
  ungroup() %>%
  group_by(scen_name, doc_field_code, doc_fieldname, setback_scenario, orig_year) %>%
  summarise(prev_n_wells = sum(prev_n_wells),
            n_exits = sum(n_exits)) %>%
  ungroup() %>%
  arrange(scen_name, doc_field_code, orig_year) 

proj_exit_state <- exit_all %>%
  group_by(scen_name, setback_scenario, orig_year) %>%
  summarise(n_exits = sum(n_exits)) %>%
  ungroup() %>%
  mutate(version = "exit-model") %>%
  rename(year = orig_year) %>%
  rbind(prev_exits_state) %>%
  mutate(setback_scenario = ifelse(setback_scenario %in% c("no_setback", "setback_2500ft"), setback_scenario, paste0(setback_scenario, "ft")),
         scen_name = ifelse(is.na(scen_name), "no-scen", scen_name),
         id = paste0(scen_name, "-", setback_scenario))

## look at number of exits in year 2020 across scenarios
scen_comp <- exit_out[, .(scen_name, doc_field_code, doc_fieldname, year, orig_year, vintage, start_year, adj_no_wells, no_wells_after_exit)]

scen_comp[, prev_n_wells := fifelse(orig_year == 2020, adj_no_wells, lag(no_wells_after_exit))]
scen_comp[, n_exits := prev_n_wells - no_wells_after_exit]
scen_comp[, c("prev_n_wells", "no_wells_after_exit", "adj_no_wells") := NULL]

# scen_comp <- scen_comp %>%
#   select(-orig_year) %>%
#   group_by(scen_name, doc_field_code, doc_fieldname, year, vintage, start_year) %>%
#   pivot_wider(names_from = scen_name, values_from = n_exits) %>%
#   mutate(BAU = ifelse(is.na(BAU), 0, BAU),
#          LCE1 = ifelse(is.na(LCE1), 0, LCE1),
#          LCE2 = ifelse(is.na(LCE2), 0, LCE2)) %>%
#   mutate(diff1 = BAU - LCE1,
#          diff2 = BAU - LCE2,
#          diff3 = LCE1 - LCE2)

## new wells exit
exit_new <- exit_out %>%
  select(scen_name, doc_field_code, doc_fieldname, setback_scenario, year, orig_year, vintage, start_year, adj_no_wells, no_wells_after_exit) %>%
  group_by(scen_name, doc_field_code, doc_fieldname, setback_scenario, vintage, start_year) %>%
  mutate(prev_n_wells = ifelse(start_year >= 2020 & start_year == orig_year, adj_no_wells, 
                               ifelse(start_year < 2020 & orig_year == 2020, adj_no_wells, lag(no_wells_after_exit))),
         n_exits = prev_n_wells - no_wells_after_exit) %>%
  ungroup() %>%
  filter(start_year >= 2020) %>%
  select(scen_name, doc_field_code, doc_fieldname, orig_year, vintage, start_year, n_exits) %>%
  mutate(vintage_name = paste0(vintage, "-", start_year))




## new wells and well exits
field_entry_exit <- merge(exit_all2[, c('scen_name', 'doc_field_code', 'doc_fieldname', 'year', 'n_exits')], new_wells,
                          by = c("scen_name", "doc_field_code", "doc_fieldname", "year"))


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


## plot state level
proj_exit_state_plot_df <- proj_exit_state %>% filter(!setback_scenario %in% c("setback_1000ft", "setback_5280ft")) %>% 
  mutate(scen_name = ifelse(scen_name == 'no-scen', 'exit-rule', scen_name))

proj_exit_state_plot_df$scen_name <- factor(proj_exit_state_plot_df$scen_name, 
                                               levels = c("exit-rule", "BAU", "LCE1", "LCE2"))


state_comp <- ggplot(proj_exit_state_plot_df, 
                     aes(x = year, y = n_exits, group = id, color = scen_name)) +
  geom_line(alpha = 0.75) +
  labs(x = NULL,
       y = "number of wells that exit") +
  facet_wrap(~setback_scenario) +
  scale_y_continuous(labels = comma) +
  # geom_text_repel(data = proj_exit_state_plot_df %>% filter(year == 2021), aes(year, n_exits, label = scen_name)) +
  theme_bw() +
  theme_line +
  theme(legend.title = element_blank())

ggsave(state_comp,
       filename = file.path(model_out_path, run_folder, "diagnostic-figs/state_exit_comparison.pdf"),
       width = 6,
       height = 6)

embed_fonts(file.path(model_out_path, run_folder, "diagnostic-figs/state_exit_comparison.pdf"),
            outfile = file.path(model_out_path, run_folder, "diagnostic-figs/state_exit_comparison.pdf"))  


## make figure that looks at well entry and well exit
## ---------------------------------------------------

## exit by well type
exit_new_existing <- exit_out %>%
  select(scen_name, doc_field_code, doc_fieldname, setback_scenario, year, orig_year, vintage, start_year, adj_no_wells, no_wells_after_exit) %>%
  group_by(scen_name, doc_field_code, doc_fieldname, setback_scenario, vintage, start_year) %>%
  mutate(prev_n_wells = ifelse(start_year >= 2020 & start_year == orig_year, adj_no_wells, 
                               ifelse(start_year < 2020 & orig_year == 2020, adj_no_wells, lag(no_wells_after_exit))),
         n_exits = prev_n_wells - no_wells_after_exit) %>%
  ungroup() %>%
  mutate(vintage = ifelse(start_year < 2020, "existing", "new")) %>%
  group_by(scen_name, doc_field_code, doc_fieldname, setback_scenario, orig_year, vintage) %>%
  summarise(prev_n_wells = sum(prev_n_wells),
            n_exits = sum(n_exits)) %>%
  ungroup() %>%
  arrange(scen_name, doc_field_code, orig_year) %>%
  rename(year = orig_year) %>%
  pivot_wider(names_from = vintage, values_from = c(prev_n_wells, n_exits)) %>%
  select(scen_name:prev_n_wells_existing, n_exits_existing, prev_n_wells_new, n_exits_new) %>%
  left_join(new_wells) %>%
  ## shift new wells down
  group_by(scen_name, doc_field_code, doc_fieldname) %>%
  mutate(new_wells_lag = lag(new_wells)) %>%
  ungroup() %>%
  select(-setback_scenario) %>%
  select(scen_name:n_exits_existing, prev_n_wells_new, new_wells, new_wells_lag, n_exits_new)

## try to isolate instances of entry and immediate exit
exit_new_existing2 <- exit_new_existing %>%
  mutate(diff = new_wells_lag - n_exits_new) %>%
  filter(n_exits_new > 0 & new_wells_lag > 0 & diff == 0)


# new wells
new_wells_fig = ggplot(state_all_long %>% filter(type == "new"), 
                       aes(x = year, y = new_wells, color = version)) + 
  geom_line(alpha = 0.7, size = 1) +
  facet_grid(~ scen_name) +
  geom_point(data = state_all_long %>% filter(version == paste0("adj-", run_type), year %in% seq(2020, 2045, by = 5)),
             aes(x = year, y = new_wells, color = version), shape = 3) +
  geom_line(data = report_wells_out_all %>% mutate(version = ifelse(year <= 2019, "historic", "calepa-report")), aes(x = year, y = new_wells, color = version), alpha = 0.7, size = 1) +
  geom_vline(xintercept = 2019, color = "darkgrey", size = 0.3, lty = "dashed") +
  scale_color_manual(values = c("#FF6E1B", "#FFD200", "#005581", "black")) +
  labs(title = 'State-level new wells',
       subtitle = 'number of new wells', 
       x = 'Year',
       y = NULL,
       color = '') +
  # scale_color_gradient(low = "#f7fbff", high = "#08306b") +
  theme_line




  