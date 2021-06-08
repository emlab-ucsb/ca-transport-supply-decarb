## Tracey Mangin
## May 27, 2021
## Well density

library(tidyverse)
library(sf)
library(data.table)

# comment out and add your own machine's file path
proj_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"

prod_file    <- "well_prod_m_processed.csv"


# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

# transform to NAD83(NSRS2007) / California Albers as well for wells and field boundaries
wells <- sf::st_read(file.path(proj_dir, "data/GIS/raw/allwells_gis/Wells_All.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(API, WellStatus, doc_fieldname = FieldName) %>%
  unique() %>%
  st_drop_geometry()

## boundaries
boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)
  
## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))


pos_fields <- well_prod %>%
  group_by(doc_field_code) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

field_codes <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique() %>%
  filter(doc_field_code %in% pos_fields$doc_field_code)

## area
field_area <- boundaries %>%
  rename(doc_field_code = FIELD_CODE) %>%
  dplyr::select(NAME, doc_field_code, AREA_SQ_MI) %>%
  st_drop_geometry()

## wells, filter, join
setDT(wells)
wells[, gas_field := str_detect(doc_fieldname, "Gas")][, gas_field := ifelse(gas_field == TRUE, 1, 0)]
wells <- wells[gas_field == 0]
wells[, gas_field := NULL]

## check production by well status
wells_merge <- wells[, c("API", "WellStatus")]
setnames(wells_merge, "API", "api_ten_digit")

well_prod_status <- well_prod %>%
  left_join(wells_merge) %>%
  group_by(api_ten_digit, year, WellStatus) %>%
  summarise(prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()
## positive production from 'canceled' wells.

## calc percentage of prod from canceled wells by year
well_prod_status2 <- well_prod_status %>%
  group_by(WellStatus, year) %>%
  summarise(prod_bbl = sum(prod_bbl)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_bbl = sum(prod_bbl)) %>%
  ungroup() %>%
  mutate(rel_prod = prod_bbl / total_bbl)

well_prod_status3 <-well_prod %>%
  left_join(wells_merge) %>%
  group_by(doc_field_code, WellStatus, year) %>%
  summarise(prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, year) %>%
  mutate(total_bbl = sum(prod_bbl)) %>%
  ungroup() %>%
  mutate(rel_prod = prod_bbl / total_bbl)


wells2 <- as.tibble(wells) %>%
  filter(!WellStatus %in% c("Abeyance"))

## get ready to rename
anti_join(wells2 %>% dplyr::select(doc_fieldname) %>% unique(), field_codes %>% dplyr::select(doc_fieldname) %>% unique())
anti_join(field_codes %>% dplyr::select(doc_fieldname) %>% unique(), wells2 %>% dplyr::select(doc_fieldname) %>% unique())
anti_join(wells2 %>% dplyr::select(doc_fieldname) %>% unique(), boundaries %>% dplyr::select(NAME) %>% rename(doc_fieldname = NAME) %>% unique())


# recode(column_to_fix, "thing thats wrong"="thing thats right",

## group by field, count
n_wells_field <- wells2 %>%
  rename(NAME = doc_fieldname) %>%
  mutate(NAME = ifelse(NAME == "Goleta", "Goleta (ABD)",
                       ifelse(NAME == "Jerry Slough (ABD)", "Jerry Slough",
                              ifelse(NAME == "Newgate", "Newgate (ABD)", NAME)))) %>%
  group_by(NAME) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(field_area) %>%
  mutate(density = n / AREA_SQ_MI) %>%
  mutate(prod_field = ifelse(doc_field_code %in% pos_fields$doc_field_code, 1, 0)) %>%
  filter(prod_field == 1)
  
density_fig <- ggplot(n_wells_field, aes(y = density, x = AREA_SQ_MI)) +
  geom_point(alpha = 0.7) +
  ylab("density (wells/sq mi)") +
  xlab("field area (sq mi)") +
  theme_bw()

ggsave(density_fig, 
       filename = file.path(proj_dir, 'model-development/density/density_fig.png'), 
       width = 6, 
       height = 7)
  
  







  