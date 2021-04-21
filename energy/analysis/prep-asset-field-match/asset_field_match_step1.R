## Tracey Mangin
## April 20, 2021
## Match assets to to fields - step 1: match assets to fields

# ------------------------------------------- INPUTS -----------------------------------
data_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path        <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/"
sp_dir             <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/"
save_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## files
rystad_file  <- "field_rystad_match_apis.csv"
prod_file    <- "well_prod_m_processed.csv"
field_b_file <- "DOGGR_Admin_Boundaries_Master.shp"

## -------------------------- ibrary
library(tidyverse)
library(data.table)
library(sf)
library(rgeos) #nearest poly to each point
library(nngeo) #nearest point to each poly

# ## source items
# items <- list.files(here::here("src"))
# 
# walk(items, ~ here::here("src", .x) %>% source()) # load local items

## --------------------------- read inputs

## asset to field match using well APIs
field_asset_match <- fread(paste0(rystad_path, rystad_file), colClasses = c('FieldCode' = 'character'))
field_asset_match[, c("FieldName", "bbl_prod", "n_wells_field", "field_prod", "rel_field", "rel_prod") := NULL]
setnames(field_asset_match, "FieldCode", "doc_field_code")

## monthly well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))
## field boundaries
fields_loc <- st_read(paste0(sp_dir, field_b_file)) %>%
  st_transform(CRS("+init=epsg:3310"))

## ---------------------------- start match

## compute productive fields
productive_fields <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]
productive_fields <- productive_fields[total_prod > 0]

## full join production with field asset match, see what is going on
# productive_fields[, total_prod := NULL]

well_match_df <- merge(productive_fields, field_asset_match, all = TRUE)
well_match_df <- well_match_df[!is.na(original_asset_name) & !is.na(total_prod)]


## fields that get matched with assets
matches <- unique(well_match_df[, c("total_prod", "n_wells_asset") := NULL])
 
# ## save file
# write_csv(field_asset_well_match, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/well_doc_asset_match_revised.csv")

##########################################################
##########################################################
##########################################################


library(mapview)
library(leaflet)
library(maps)

## reassign 848 to 849 because Old Wilmington does not have a spatial boundary, but we know it is in the
## same spot at 849

matches[, doc_field_code := fifelse(doc_field_code == "848", "849", doc_field_code)]

## field options
field_options <- fields_loc %>%
  st_transform(CRS("+init=epsg:3310")) %>%
  filter(FIELD_CODE %in% matches$doc_field_code) 

## function - find nearest neighbor

find_nfield <- function(fcode) {
  
  field_code <- fcode
  
  field_sp <- fields_loc %>%
    filter(FIELD_CODE == field_code)
  
  other_fields <- field_options %>%
    filter(FIELD_CODE != field_code)
  
  nf <- st_nn(field_sp, other_fields, k = 10, returnDist = T)
  
  match_tmp <- other_fields[nf$nn[[1]], ]
  
  match_tmp_dist <- nf$dist[[1]]
  
  match_tmp$dist <- match_tmp_dist
  
  match_tmp2 <- match_tmp %>%
    filter(dist == min(dist))
  
  # mfn <- match_tmp$NAME
  # mfc <- match_tmp$FIELD_CODE
  # dist <- nf$dist[[1]][1]
  # 
  # final_tib <- tibble(doc_field_code = field_code,
  #                     nn_field_code = mfc,
  #                     nn_field_name = mfn,
  #                     dist = dist)
  
  final_a <- match_tmp2 %>%
    as_tibble() %>%
    select(nn_field_code = FIELD_CODE,
           nn_field_name = NAME,
           dist) 
  
  orig_field_df <- tibble(doc_field_code = rep(field_code, nrow(final_a)))
  
  final <- cbind(orig_field_df, final_a)
  
}

## field vec

# field_vec <- unique(fields_loc$FIELD_CODE)
valid_fields <- fields_loc %>% 
  filter(FIELD_CODE %in% productive_fields$doc_field_code)

field_vec <- unique(valid_fields$FIELD_CODE)

nn_fields <- purrr::map(field_vec, find_nfield) %>%
  bind_rows()

nn_fields_n <- nn_fields %>%
  group_by(doc_field_code) %>%
  mutate(n = n()) %>%
  ungroup()


write_csv(nn_fields, file = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/field_x_field_match_revised.csv")


# ## figure
# fields_sf2 <- fields_loc %>%
#   mutate(cost_info = ifelse(NAME %in% field_options$NAME, "yes", "no"))
# 
# 
# ## counties
# counties <- read_sf(dsn = paste0(sp_dir, "raw/CA_Counties/", layer = "CA_Counties_TIGER2016.shp")) %>%
#   st_transform(CRS("+init=epsg:3310"))
# 
# ## mapview 
# mapview(fields_sf2, zcol = "cost_info", layer.name = "Fields", label = fields_sf2$NAME) + 
#   mapview(counties, layer.name = "County boundaries", col.regions = "grey", alpha = 0.1, label = counties$NAME) 




