## September 15, 2020
## Create entry model input df
## Step 1: Match fields to assets

library(tidyverse)

## source items
items <- list.files(here::here("src"))
walk(items, ~ here::here("src", .x) %>% source()) # load local items

## define data folders
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
rystad_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## asset to field match using well APIs
field_asset_match <- read_csv(paste0(rystad_path, "processed/field_rystad_match_apis.csv")) %>%
  select(FieldCode, original_asset_name, n_wells_asset)

## nearest asset (spatial)
field_assets <- read_csv(paste0(rystad_path, "processed/fieldsAssets_adj.csv")) ## use version that removes assets with 0 Rystad prod

# field_assets <- read_csv(paste0(rystad_path, "processed/fieldsAssets.csv"))
# field_assets_m2 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/intermediary/outlier_fields_nn.csv")

## well production
well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  select(FieldCode) %>%
  full_join(field_asset_match) 

## fields that get matched with assets
field_asset_well_match <- well_match_df %>%
  select(FieldCode, original_asset_name) %>%
  unique() %>%
  filter(!is.na(original_asset_name))

## save file
write_csv(field_asset_well_match, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/well_doc_asset_match.csv")

## are there multipe assets in a field?
n_assets_x_field <- well_match_df %>%
  select(FieldCode, original_asset_name) %>%
  unique() %>%
  filter(!is.na(original_asset_name)) %>%
  group_by(FieldCode) %>%
  summarise(n = n()) %>%
  ungroup()

ggplot(n_assets_x_field, aes(x = n)) +
  geom_histogram(binwidth = 1)
  ## yes
   ## are there any doc fields without assets?

na_asset <- well_match_df %>%
  select(FieldCode, original_asset_name) %>%
  unique() %>%
  filter(is.na(original_asset_name))
  nrow(na_asset)
  
  ## filter for the fields without an asset
  na_asset2 <- na_asset %>%
    select(FieldCode) %>%
    unique()
  # filter field assets for missing assets and doc fields
  sp_matches <- field_assets %>%
    select(FieldName = NAME,
           FieldCode = FIELD_CODE,
           original_asset_name = Asset,
           dist) %>%
    mutate(FieldName = str_remove_all(FieldName, pattern = ",")) %>%
    filter(FieldCode %in% na_asset2$FieldCode) %>%
    mutate(n_wells_asset = 1,
           match_method = "spatial") %>%
    select(FieldCode, original_asset_name, n_wells_asset, match_method, dist)
  anti_join(na_asset2 %>% select(FieldCode) %>% unique(), sp_matches %>% select(FieldCode) %>% unique())
  # 154 - Coal Oil Point Offshore (ABD) -- this field is not in the field boundary spatial data
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
  ## all combos
  all_combos <- well_match_df %>%
    filter(!is.na(FieldCode),
           !is.na(original_asset_name)) %>%
    mutate(match_method = "well_match",
           dist = NA) %>%
    # rbind(name_match) %>%
    rbind(sp_matches) %>%
    filter(FieldCode != "000") %>%
    left_join(fieldcodes) %>%
    select(FieldCode, FieldName, original_asset_name, n_wells_asset, match_method, dist_m = dist)
 
  ## save file
  write_csv(all_combos, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-model-input/field_asset_matches.csv")
          
  
