## Tracey Mangin
## May 4, 2021
## Match assets to to fields - step 1: match assets to fields
## version 3 -- combination of 1 & 2

# ------------------------------------------- INPUTS -----------------------------------
data_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path        <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/"
sp_dir             <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/"
match_out_path     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/"
save_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## files
rystad_file  <- "field_rystad_match_apis_revised.csv"
prod_file    <- "well_prod_m_processed.csv"
nn_field_file <- "field_x_field_match_revised.csv"
nn_asset_file <- "fieldsAssets_adj_revised.csv"
asset_loc_file <- "asset_latlon_adj.csv"
field_b_file <- "DOGGR_Admin_Boundaries_Master.shp"


## -------------------------- library
library(tidyverse)
library(data.table)
library(mapview)
library(sf)

## --------------------------- read inputs

## field location                     
fields_loc <- st_read(paste0(sp_dir, field_b_file))

## asset location
asset_loc <- read.csv(paste0(rystad_path, asset_loc_file)) %>%
  select(-X)

asset_loc <- st_as_sf(asset_loc, coords = c("Longitude", "Latitude"), 
                      crs = st_crs(fields_loc))



## nearest assets to field
nn_asset <- fread(paste0(rystad_path, nn_asset_file)) %>%
  select(-V1)

## nearest fields with asset info to fields
nn_field <- fread(paste0(match_out_path, nn_field_file), colClasses = c('doc_field_code' = 'character',
                                                                        'nn_field_code' = "character"))


## asset to field match using well APIs
field_asset_match <- fread(paste0(rystad_path, rystad_file), colClasses = c('doc_field_code' = 'character'))
field_asset_match[, c("doc_fieldname", "bbl_prod", "n_wells_field", "field_prod", "rel_field", "rel_prod") := NULL]


## monthly well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))
## fieldcodes 
fieldcodes <- unique(well_prod[, c("doc_field_code", "doc_fieldname")])

## compute productive fields
productive_fields <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]
productive_fields <- productive_fields[total_prod > 0]

## fields that get matched on asset
well_match_df <- productive_fields %>%
  select(doc_field_code) %>%
  left_join(field_asset_match) 

well_match_api_df <- well_match_df %>%
  filter(!is.na(original_asset_name))

## distance api matches assets (field to asset)

## field loc
fields_loc2 <- fields_loc %>%
  select(doc_field_code = FIELD_CODE, geometry)

well_match_api_df2 <- well_match_api_df %>%
  filter(!doc_field_code %in% c("000", "154")) %>%
  left_join(fields_loc2) %>% 
  select(-n_wells_asset) %>%
  unnest(c(doc_field_code, original_asset_name)) %>%
  st_as_sf(crs = st_crs(fields_loc))

calc_dist <- function(i) {
  
  tmp_field <- well_match_api_df2[i, ]
  
  tmp_asset_name <- as.character(tmp_field$original_asset_name[1])
  
  tmp_asset_loc <- asset_loc %>% filter(Asset == tmp_asset_name)
  
  tmp_asset_loc_geom <- st_geometry(tmp_asset_loc)
  
  dist <- st_distance(tmp_field, tmp_asset_loc_geom)
  
  dist_df <- tmp_field %>%
    mutate(dist_m = as.numeric(dist))
  
}

## i


## run the function to find nearest asset(s)
dist_df <- purrr::map(1:nrow(well_match_api_df2), calc_dist) %>%
  bind_rows() %>%
  st_drop_geometry() %>%
  left_join(fieldcodes) %>%
  select(doc_field_code, doc_fieldname, original_asset_name, dist_m)


write_csv(dist_df, file = paste0(match_out_path, "dist_field_asset_match_by_wells.csv"))


mapview(fields_loc, layer.name = "fields") +
  mapview(asset_loc %>% filter(Asset == "San Joaquin_CA_Other Partner(s), US"), fill = "yellow", layer.name = "asset", cex = 0.25)


## asset match dist

nn_asset2 <- nn_asset %>%
  filter(!doc_field_code %in% dist_df$doc_field_code) %>%
  filter(doc_field_code %in% productive_fields$doc_field_code) %>%
  select(doc_field_code, dist) %>%
  mutate(type = "nearest asset")

nn_field2 <- nn_field %>%
  filter(!doc_field_code %in% dist_df$doc_field_code) %>%
  select(doc_field_code, dist) %>%
  mutate(type = "nearest field")

dist_df %>%
  select(doc_field_code, dist = dist_m) %>%
  mutate(type = "match by well location") %>%
  rbind(nn_asset2) %>%
  rbind(nn_field2) %>%
  # filter(dist < 2e5) %>%
  ggplot(aes(x = dist, by = type, fill = type)) +
  geom_histogram(alpha = 0.6, binwidth = 1000) +
  theme_bw()

dist_df %>%
  select(doc_field_code, dist = dist_m) %>%
  mutate(type = "match by well location") %>%
  rbind(nn_asset2) %>%
  rbind(nn_field2) %>%
  filter(dist < 2e5) %>%
  ggplot(aes(x = dist, by = type, fill = type)) +
  geom_histogram(alpha = 0.6, binwidth =  1000)




