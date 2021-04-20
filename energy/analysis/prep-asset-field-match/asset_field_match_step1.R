## Tracey Mangin
## April 20, 2021
## Match assets to to fields - step 1

# ------------------------------------------- INPUTS -----------------------------------
data_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path        <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/"
save_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## files
rystad_file  <- "field_rystad_match_apis.csv"
prod_file    <- "well_prod_m_processed.csv"

## -------------------------- ibrary
# library(tidyverse)
library(data.table)

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

## compute productive fields
productive_fields <- well_prod[, .(total_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]
productive_fields <- productive_fields[total_prod > 0]

## full join production with field asset match, see what is going on
# productive_fields[, total_prod := NULL]

well_match_df <- merge(productive_fields, field_asset_match, all = TRUE)
well_match_df <- well_match_df[!is.na(original_asset_name) & !is.na(total_prod)]


## fields that get matched with assets
field_asset_well_match <- unique(well_match_df[, c("total_prod", "n_wells_asset") := NULL])
 
# ## save file
# write_csv(field_asset_well_match, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/well_doc_asset_match_revised.csv")





