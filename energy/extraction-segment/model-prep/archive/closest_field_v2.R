## Tracey Mangin
## September 22, 2020
## spatial match, v2

## libraries
library(tidyverse)
library(sf) #read
library(rgeos) #nearest poly to each point
library(nngeo) #nearest point to each poly
library(mapview)
library(leaflet)
library(maps)

## read in data
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
sp_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/"

fields_loc <- st_read("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp") %>%
  st_transform(CRS("+init=epsg:3310"))

matches <- read_csv("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/well_doc_asset_match.csv") 

## well production
well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2)

## wells
wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10))

## make table for matching field id with field name
fieldcodes <- wells_19 %>%
  select(FieldCode, FieldName) %>%
  unique()

## productive fields
productive_fields <- well_prod %>%
  group_by(FieldCode) %>%
  summarise(total_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(total_prod > 0) %>%
  left_join(fieldcodes)

## field options
field_options <- fields_loc %>%
  st_transform(CRS("+init=epsg:3310")) %>%
  filter(FIELD_CODE %in% matches$FieldCode) 

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

field_vec <- unique(fields_loc$FIELD_CODE)

nn_fields <- purrr::map(field_vec, find_nfield) %>%
  bind_rows()

nn_fields_n <- nn_fields %>%
  group_by(doc_field_code) %>%
  mutate(n = n()) %>%
  ungroup()


write_csv(nn_fields, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/field_x_field_match.csv")


## figure
fields_sf2 <- fields_loc %>%
  mutate(cost_info = ifelse(NAME %in% field_options$NAME, "yes", "no"))


## counties
counties <- read_sf(dsn = paste0(sp_dir, "raw/CA_Counties/", layer = "CA_Counties_TIGER2016.shp")) %>%
  st_transform(CRS("+init=epsg:3310"))

## mapview 
mapview(fields_sf2, zcol = "cost_info", layer.name = "Fields", label = fields_sf2$NAME) + 
  mapview(counties, layer.name = "County boundaries", col.regions = "grey", alpha = 0.1, label = counties$NAME) 

