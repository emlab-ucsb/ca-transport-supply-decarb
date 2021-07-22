## Tracey Mangin
## July 21, 2021
## Make list of fields included in the analysis

## libraries
library(data.table)
library(tidyverse)
library(sf)

## set paths and file names
home                = "/Volumes/GoogleDrive/Shared drives"
model_out_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/'
run_folder          = 'extraction_2021-07-22/revised-ccs-correct-new-setback/'
field_fname         = "diagnostic-field-level-results.csv"

## save path
save_path           = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/'

## field boundaries
boundaries <- st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)


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

field_out_zero <- field_out %>%
  filter(!doc_field_code %in% pos_field$doc_field_code)

field_boundaries <- boundaries %>%
  filter(FIELD_CODE %in% pos_field$doc_field_code) %>%
  dplyr::select(NAME, doc_field_code = FIELD_CODE)

## save for health/labor team
st_write(field_boundaries, dsn = paste0(save_path, "extraction_fields.shp"))

