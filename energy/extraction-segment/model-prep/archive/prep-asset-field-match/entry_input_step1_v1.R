## Tracey Mangin
## May 4, 2021
## Create v1 entry model input df

# ------------------------------------------- INPUTS -----------------------------------
data_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path        <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/"
sp_dir             <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/"
save_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## files
rystad_file  <- "field_rystad_match_apis_revised.csv"
prod_file    <- "well_prod_m_processed.csv"
field_b_file <- "DOGGR_Admin_Boundaries_Master.shp"
asset_loc_file <- "asset_latlon_adj.csv"

## -------------------------- library
library(tidyverse)
library(data.table)
library(mapview)
library(sf)
library(rgeos) #nearest poly to each point
library(nngeo) #nearest point to each poly


## make df of nearest neighbor field to asset
## ----------------------------------------------------------------

## asset location
asset_loc <- read.csv(paste0(rystad_path, asset_loc_file))

## field location                     
fields_loc <- st_read(paste0(sp_dir, field_b_file))

asset_loc_sf <- st_as_sf(asset_loc, coords = c("Longitude", "Latitude"), 
                       crs = st_crs(fields_loc))

st_crs(fields_loc) == st_crs(asset_loc_sf)

plot(st_geometry(fields_loc))
plot(asset_loc_sf, add = TRUE)

## nearest point to each polygon 
find_n_asset <- function(fcode) {
  
  field_code <- fcode
  
  field_sp <- fields_loc %>%
    filter(FIELD_CODE == field_code)
  
  nf <- st_nn(field_sp, asset_loc_sf, k = 10, returnDist = T)
  
  match_tmp <- asset_loc_sf[nf$nn[[1]], ]
  
  match_tmp_dist <- nf$dist[[1]]
  
  match_tmp$dist <- match_tmp_dist
  
  match_tmp2 <- match_tmp %>%
    filter(dist == min(dist))
  
  final_a <- match_tmp2 %>%
    as_tibble() %>%
    select(-X) %>%
    select(nn_asset = Asset,
           dist) 
  
  orig_field_df <- tibble(doc_field_code = rep(field_code, nrow(final_a)))
  
  final <- cbind(orig_field_df, final_a)
  
}

## fields
field_vec <- unique(fields_loc$FIELD_CODE)

## run the function to find nearest asset(s)
nn_assets <- purrr::map(field_vec, find_n_asset) %>%
  bind_rows()

## how many assets per field?
nn_assets_n <- nn_assets %>%
  group_by(doc_field_code) %>%
  mutate(n = n()) %>%
  ungroup()

# ## mapview
# mapview(fields_loc %>% filter(FIELD_CODE == "464"), layer.name = "Fields", label = fields_loc$NAME) +
#   mapview(asset_loc_sf, col.regions = "yellow", layer.name = "Asset",  label = asset_loc_sf$Asset)

write.csv(nn_assets, paste0(rystad_path, "fieldsAssets_adj_revised.csv"))



## make v1 field asset matches
## ----------------------------------------------------------------

## asset to field match using well APIs
field_asset_match <- read_csv(paste0(rystad_path, rystad_file)) %>%
  select(doc_field_code, original_asset_name, n_wells_asset)

## nearest asset (spatial)
# field_assets <- read_csv(paste0(rystad_path, "processed/fieldsAssets_adj.csv")) ## use version that removes assets with 0 Rystad prod
## use nn_assets

## well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## compute productive fields
productive_fields <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]
productive_fields <- productive_fields[total_prod > 0]

well_match_df <- productive_fields %>%
  select(doc_field_code) %>%
  left_join(field_asset_match) 

fieldcodes <- well_prod %>%
  select(doc_field_code, doc_fieldname) %>%
  unique() %>%
  filter(doc_field_code %in% productive_fields$doc_field_code)


## fields that get matched with assets
field_asset_well_match <- well_match_df %>%
  select(doc_field_code, original_asset_name) %>%
  unique() %>%
  filter(!is.na(original_asset_name))

## save file
# write_csv(field_asset_well_match, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/well_doc_asset_match.csv")
write_csv(field_asset_well_match, file = paste0(save_directory, "stocks-flows/entry-model-input/well_doc_asset_match_revised.csv")) 


## are there multipe assets in a field?
n_assets_x_field <- well_match_df %>%
  select(doc_field_code, original_asset_name) %>%
  unique() %>%
  filter(!is.na(original_asset_name)) %>%
  group_by(doc_field_code) %>%
  summarise(n = n()) %>%
  ungroup()

ggplot(n_assets_x_field, aes(x = n)) +
  geom_histogram(binwidth = 1)
## yes

## are there any doc fields without assets?
na_asset <- well_match_df %>%
  select(doc_field_code, original_asset_name) %>%
  unique() %>%
  filter(is.na(original_asset_name))
nrow(na_asset)

## filter for the fields without an asset
na_asset2 <- na_asset %>%
  select(doc_field_code) %>%
  unique()

## filter field assets for missing assets and doc fields
sp_matches <- nn_assets %>%
  select(doc_field_code,
         original_asset_name = nn_asset,
         dist) %>%
  filter(doc_field_code %in% na_asset2$doc_field_code) %>%
  mutate(n_wells_asset = 1,
         match_method = "spatial") %>%
  select(doc_field_code, original_asset_name, n_wells_asset, match_method, dist)

anti_join(na_asset2 %>% select(doc_field_code) %>% unique(), sp_matches %>% select(doc_field_code) %>% unique())
# 154 - Coal Oil Point Offshore (ABD) -- this field is not in the field boundary spatial data

## all combos
all_combos <- well_match_df %>%
  filter(!is.na(doc_field_code),
         !is.na(original_asset_name)) %>%
  mutate(match_method = "well_match",
         dist = NA) %>%
  # rbind(name_match) %>%
  rbind(sp_matches) %>%
  filter(doc_field_code != "000") %>%
  left_join(fieldcodes) %>%
  select(doc_field_code, doc_fieldname, original_asset_name, n_wells_asset, match_method, dist_m = dist)

## save file
write_csv(all_combos, file = paste0(save_directory, "stocks-flows/entry-model-input/field_asset_matches_v1_revised.csv"))



## investigate method 2 -- match to nearest field with opex/capex
## ---------------------------------------------------------------
# 
# m2_field_assets <- field_assets_m2 %>%
#   select(OBJECTID, NAME, FIELD_CODE, nn, dist, sequence) %>%
#   mutate(FieldCode2 = paste0("00", nn),
#                 field_code_matched = str_sub(FieldCode2, start= -3)) %>%
#            select(-nn, -FieldCode2) %>%
#   left_join(well_match_df, c("FIELD_CODE" = "FieldCode")) %>%
#   mutate(match = ifelse(n_wells_asset > 0, 1, 0))
# 
# no_match <- m2_field_assets %>%
#   group_by(NAME, FIELD_CODE) %>%
#   summarise(nw_val = sum(match)) %>%
#   ungroup()

