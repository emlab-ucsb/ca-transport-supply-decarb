## Tracey Mangin
## May 27, 2021
## Well density

library(tidyverse)
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
  mutate
  dplyr::select(API, WellStatus, doc_fieldname = FieldName) %>%
  unique() %>%
  st_drop_geometry()

## boundaries
boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)
  
## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

field_codes <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique()

## area
field_area <- boundaries %>%
  rename(doc_field_code = FIELD_CODE) %>%
  dplyr::select(doc_field_code, AREA_SQ_MI) %>%
  st_drop_geometry()

## wells, filter, join
setDT(wells)
wells[, gas_field := str_detect(FieldName, "Gas")][, gas_field := ifelse(gas_field == TRUE, 1, 0)]
wells <- wells[gas_field == 0]
wells[, gas_field := NULL]


wells2 <- as.tibble(wells) %>%
  filter(!WellStatus %in% c("Canceled"))

## get ready to rename
anti_join(wells2 %>% dplyr::select(FieldName) %>% unique(), field_codes %>% dplyr::select(doc_fieldname) %>% unique())



## group by field, count
n_wells_field <- wells2 %>%
  rename(doc_fieldname = FieldName) %>%
  mutate(doc_fieldname = ifelse(doc_fieldname == "Antelope Hills, North", "Antelope Hills  North",
                          ifelse(doc_fieldname == "Bellevue, West", "Bellevue  West",
                            ifelse(doc_fieldname == "Belridge, North", "Belridge  North",
                              ifelse(doc_fieldname == "Belridge, South", "Belridge  South",
                                ifelse(doc_fieldname == "Coalinga, East, Extension", "Coalinga  East  Extension",
                                  ifelse(doc_fieldname == "Coles Levee, North", "Coles Levee  North",
                                    ifelse(doc_fieldname == "Coles Levee, South", "Coles Levee  South",
                                      ifelse(doc_fieldname == "Edison, Northeast", "Edison  Northeast",
                                        ifelse(doc_fieldname == "Lost Hills, Northwest", "Lost Hills  Northwest",
                                          ifelse(doc_fieldname == "Montalvo, West", "Montalvo  West",
                                            ifelse(doc_fieldname == "Newport, West", "Newport  West",
                                              ifelse(doc_fieldname == "Salt Lake, South", "Salt Lake  South",
                                                ifelse(doc_fieldname == "Shafter, North", "Shafter  North",
                                                  ifelse(doc_fieldname == "Tapo Canyon, South", "Tapo Canyon  South",
                                                    ifelse(doc_fieldname == "Tejon, North", "Tejon  North", doc_fieldname)))))))))))))))) %>%
  left_join(field_codes) %>%
  group_by(doc_fieldname, doc_field_code) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(field_area) %>%
  mutate(density = n / AREA_SQ_MI)
  

  
  







  