
library(tidyverse)
library(data.table)
library(sf)



base_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
proj_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"

test1 = fread(file.path(base_path, 'extraction_2021-06-04', 'revised-setbacks', 'diagnostic-field-level-results.csv'), 
              header = T, colClasses = c('doc_field_code' = 'character'))

test1_doc <- test1 %>% select(doc_field_code) %>% unique()

test2 = fread(file.path(base_path, 'extraction_2021-06-04', 'revised-remove-plugged', 'diagnostic-field-level-results.csv'), 
              header = T, colClasses = c('doc_field_code' = 'character'))

test2_doc <- test2 %>% select(doc_field_code) %>% unique()

## missing fields
missing_fields <- anti_join(test1_doc, test2_doc)

## setback
well_setback_out <- fread(paste0(proj_dir, "outputs/setback/model-inputs/wells_in_setbacks_revised.csv"), colClasses = c('api_ten_digit' = 'character'))

## wells
wells <- sf::st_read(file.path(proj_dir, "data/GIS/raw/allwells_gis/Wells_All.shp")) %>%
  select(api_ten_digit = API, NAME = FieldName) %>%
  st_drop_geometry()

## boundaries
boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)

## group by field, count
wells2 <- wells %>%
  mutate(NAME = ifelse(NAME == "Goleta", "Goleta (ABD)",
                       ifelse(NAME == "Jerry Slough (ABD)", "Jerry Slough",
                              ifelse(NAME == "Newgate", "Newgate (ABD)", 
                                     ifelse(NAME == "Wilson Creek Gas", "Wilson Creek Gas (ABD)", 
                                            ifelse(NAME == "Collegeville, East, Gas", "Collegeville, East, Gas  Gas", NAME)))))) %>%
  left_join(boundaries) %>%
  select(api_ten_digit, NAME, doc_field_code = FIELD_CODE) 

## join
wells_setback <- well_setback_out %>%
  left_join(wells2) 

wells_missing_field <- wells_setback %>% filter(doc_field_code %in% missing_fields$doc_field_code)

View(wells_missing_field %>% filter(well_status != "Plugged"))

missing_wells <- wells_missing_field %>% filter(well_status != "Plugged")

unique(missing_wells$doc_field_code)

## how many fields in pred prod?
pred_prod %>%
  select(doc_field_code) %>%
  unique() 

##
prod_file    <- "well_prod_m_processed.csv"

## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))

well_prod_all <- well_prod[, .(sum_prod = sum(OilorCondensateProduced, na.rm = T)), by = api_ten_digit]

well_prod_missing <- well_prod_all[api_ten_digit %in% missing_wells[, api_ten_digit]]

