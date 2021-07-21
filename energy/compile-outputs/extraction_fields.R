## Tracey Mangin
## July 21, 2021
## Make list of fields included in the analysis

## libraries
library(data.table)
library(tidyverse)

## set paths and file names
model_out_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/'
run_folder          = 'extraction_2021-07-21/revised-new-wells-setback/'
field_fname         = "diagnostic-field-level-results.csv"

# save_exit_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/exit/'
# exit_file           = 'well_exits_under_rule_forecast.csv'
# save_info_path      = paste0(model_out_path, "/", run_folder, 'diagnostic-figs/exit-figs-x-field')

## read in output file
field_out <- fread(paste0(model_out_path, run_folder, field_fname), header = T, colClasses = c('doc_field_code' = 'character'))

field_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                 fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]


length(unique(field_out[, doc_field_code]))

pos_field <- field_out %>%
  group_by(scen_name, doc_field_code, doc_fieldname) %>%
  summarise(sum_prod = sum(total_prod_bbl)) %>%
  ungroup() %>%
  filter(sum_prod > 0)

