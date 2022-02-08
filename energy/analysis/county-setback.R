## Tracey Mangin
## February 7, 2022
## Percentage of county covered by setback

# load packages
library(sf)
library(tidyverse)
library(purrr)
# library(rgdal)
library(data.table)
# library(gdalUtilities)
library(maps)
library(mapview)

## paths 
main_path       <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn"
buffer_path     <- "data/GIS/processed/fracktracker-sr"
data_directory  <- "data/stocks-flows/processed/"

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## quick CA
ca <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME) 

## field boundaries
boundaries <- st_read(file.path(main_path, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(ca_crs)


################################# READ DATA AND TRANSFORM

buff1000 <- sf::st_read(file.path(main_path, buffer_path, "buffer_1000ft.shp"))

buff2500 <- sf::st_read(file.path(main_path, buffer_path, "buffer_2500ft.shp"))

buff5280 <- sf::st_read(file.path(main_path, buffer_path, "buffer_5280ft.shp"))

## production

## files
prod_file    <- "well_prod_m_processed.csv"

## monthly well production
well_prod <- fread(paste0(main_path, "/", data_directory, "/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## fields that produce oil in time horizon
pos_fields <- well_prod[, (prod = sum(OilorCondensateProduced)), by = .(doc_field_code)]

field_boundaries <- boundaries %>%
  filter(FIELD_CODE %in% pos_fields$doc_field_code) %>%
  dplyr::select(doc_field_code = FIELD_CODE)

anti_join(pos_fields %>% dplyr::select(doc_field_code) %>% unique(), 
          field_boundaries %>% dplyr::select(doc_field_code) %>% unique())

## find ratio of county area covered by buffer for three buffer scenarios
## -----------------------------

county_fields_df <-  county_boundaries %>% 
  st_intersection(field_boundaries) 

county_fields_df <- county_fields_df %>%
  mutate(area = st_area(county_fields_df))

county_field_area <- county_fields_df %>%
  group_by(adj_county_name) %>%
  summarise(county_field_area = sum(area)) %>%
  ungroup()

county_coverage_df_1000 <- county_field_area %>%
  st_intersection(buff1000) %>%
  mutate(setback_area = st_area(geometry),
         county_field_coverage = setback_area / county_field_area,
         setback_scenario = "setback_1000ft") %>%
  select(adj_county_name, setback_scenario, county_field_area, setback_area, county_field_coverage) %>%
  st_drop_geometry()

county_coverage_df_2500 <- county_field_area %>%
  st_intersection(buff2500) %>%
  mutate(setback_area = st_area(geometry),
         county_field_coverage = setback_area / county_field_area,
         setback_scenario = "setback_2500ft") %>%
  select(adj_county_name, setback_scenario, county_field_area, setback_area, county_field_coverage) %>%
  st_drop_geometry()

county_coverage_df_5280 <- county_field_area %>%
  st_intersection(buff5280) %>%
  mutate(setback_area = st_area(geometry),
         county_field_coverage = setback_area / county_field_area,
         setback_scenario = "setback_5280ft") %>%
  select(adj_county_name, setback_scenario, county_field_area, setback_area, county_field_coverage) %>%
  st_drop_geometry()


## combine all three setbacks
county_setbacks <- rbind(county_coverage_df_1000, county_coverage_df_2500, county_coverage_df_5280) %>%
  units::drop_units() 
  

# save output

write_csv(county_setbacks, file.path(main_path, "outputs/setback/county-level/county_level_setback_coverage.csv"))


## map 

cc_5280 <- county_field_area %>%
  st_intersection(buff5280) %>%
  units::drop_units()

mapview(county_boundaries, layer.name = "county boundary", label = 'adj_county_name', col.regions = "yellow", legend = FALSE) +
  # mapview(buff1000, layer.name = "1000ft", col.regions = "blue", legend = FALSE) +
  # mapview(buff2500, layer.name = "2500ft", col.regions = "grey", legend = FALSE) +
  mapview(buff5280, layer.name = "5280ft", col.regions = "red", legend = FALSE) +
  mapview(field_boundaries, layer.name = "field boundaries", label = 'doc_field_code', col.regions = "green") +
  mapview(county_field_area %>% units::drop_units(), layer.name = "county fields", label = 'adj_county_name', col.regions = "orange") +
  mapview(cc_5280, layer.name = "5200ft coverage", col.regions = "purple") +
  mapview(county_coverage_df_2500, layer.name = "2500ft coverage", col.regions = "purple", legend = FALSE) +
  mapview(county_coverage_df_5280, layer.name = "5280ft coverage", col.regions = "orange", legend = FALSE) 
