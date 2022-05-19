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


## -------------------------- library
library(tidyverse)
library(data.table)
# library(mapview)
# library(sf)

## --------------------------- read inputs

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

## not matched
no_match <- well_match_df %>%
  filter(is.na(original_asset_name))

## for nearest field df, join with nn_field_code asset
match_df <- well_match_api_df %>%
  rename(nn_field_code = doc_field_code) %>% 
  select(-n_wells_asset) %>%
  mutate(match_method = "nearest_field_w_asset_match")

nn_field2 <- nn_field %>%
  filter(doc_field_code %in% no_match$doc_field_code) %>%
  left_join(match_df) %>%
  select(doc_field_code, original_asset_name, dist, match_method)

## for nearest asset df, match column names above
nn_asset2 <- nn_asset %>%
  filter(doc_field_code %in% no_match$doc_field_code) %>%
  rename(original_asset_name = nn_asset) %>%
  mutate(match_method = "nearest_asset")

## combine 
no_match_assets <- rbind(nn_field2, nn_asset2) %>%
  group_by(doc_field_code) %>%
  mutate(dist_rank = rank(dist),
         min_rank = min(dist_rank)) %>%
  ungroup() %>%
  filter(dist_rank == min_rank) %>%
  ungroup() %>%
  select(-dist_rank, -min_rank) %>%
  mutate(n_wells_asset = NA) %>%
  left_join(fieldcodes) %>%
  select(doc_field_code, doc_fieldname, original_asset_name, dist, n_wells_asset, match_method)

all_matches <- well_match_api_df %>%
  left_join(fieldcodes) %>%
  mutate(dist = NA,
         match_method = "well_match") %>%
  select(doc_field_code, doc_fieldname, original_asset_name, dist, n_wells_asset, match_method) %>%
  rbind(no_match_assets) %>%
  filter(doc_field_code != "000") %>%
  rename(dist_m = dist) %>%
  unique()

## save file
write_csv(all_matches, file = paste0(match_out_path, "field_asset_matches_v3_revised.csv"))

