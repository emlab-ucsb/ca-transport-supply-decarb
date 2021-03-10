## Tracey Mangin
## August 19, 2020
## Well status and type

library(tidyverse)
library(readxl)
library(openxlsx)
library(rebus)
library(scales)
library(fuzzyjoin)


## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## read in data

well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10))  %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
  FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2) %>%
  mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode))

# well_inj <- read_rds(paste0(data_directory, "well_inject_m.rds")) %>%
#   mutate(api_ten_digit = substr(APINumber, 1, 10))

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10)) 

all_wells <- read_xlsx("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/All_wells_20200417.xlsx") %>%
  mutate(spud_date = convertToDate(SpudDate))

well_type_df <- read_csv(paste0(data_directory, "well_type_df.csv")) 

# ## entry
# entry <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry_df.csv")



## how many active wells in 2019? two ways, 1) indicator, 2) production
## ----------------------------------------------

## all wells duplicates?
all_wells_dup <- all_wells %>%
  group_by(API) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

## test, is it just the sup date creating duplicates?
all_wells_filt <- all_wells %>%
  select(-SpudDate, - spud_date) %>%
  unique() %>%
  group_by(API) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

## unique wells
n_wells <- all_wells %>%
  select(-SpudDate, - spud_date) %>%
  unique()

nrow(n_wells)

## wells 19 duplicates?

wells_19_dup <- wells_19 %>%
  group_by(API) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

## test, is it just the spud date creating duplicates?
all_wells_filt <- all_wells %>%
  select(-SpudDate, - spud_date) %>%
  unique() %>%
  group_by(API) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)


fieldcodes <- wells_19 %>%
  select(FieldCode, FieldName) %>%
  unique() %>%
  mutate(FieldName = str_replace_all(FieldName, pattern = "  ", " "))

well_type_field <- all_wells %>%
  select(-SpudDate, - spud_date) %>%
  rename(WellTypeCode = WellType) %>%
  unique() %>%
  filter(WellStatus != "Canceled") %>%
  mutate(FieldName = trimws(FieldName)) %>%
  group_by(FieldName, WellStatus, WellTypeCode) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(well_type_df) %>%
  mutate(FieldName = str_remove_all(FieldName, ","),
         FieldName = ifelse(FieldName == "Old Wilmington (ABD)", "Wilmington", FieldName)) 
# %>%
#   mutate(FieldName = ifelse(FieldName == "Newgate", "Newgate (ABD)", FieldName))

View(anti_join(well_type_field %>% select(FieldName) %>% unique, fieldcodes %>% select(FieldName)))
View(anti_join(fieldcodes %>% select(FieldName), well_type_field %>% select(FieldName) %>% unique))
## View(anti_join(entry %>% rename(FieldName = doc_fieldname) %>% select(FieldName) %>% unique, well_type_field %>% select(FieldName) %>% unique))

well_type_field1 <- well_type_field %>%
  group_by(FieldName, well_type_name) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  group_by(FieldName) %>%
  mutate(total_wells_field = sum(n, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_welltype = n / total_wells_field) %>%
  left_join(fieldcodes) %>%
  select(FieldCode, FieldName, well_type_name:rel_welltype)


## save file
write_csv(well_type_field1, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_type_x_field_all.csv")

####
well_type_field2 <- well_type_field %>%
  filter(!WellStatus %in% c("Canceled", "PluggedOnly", "Plugged")) %>%
  group_by(FieldName, well_type_name) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  group_by(FieldName) %>%
  mutate(total_wells_field = sum(n, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_welltype = n / total_wells_field) %>%
  left_join(fieldcodes) %>%
  select(FieldCode, well_type_name:rel_welltype)


## save file
write_csv(well_type_field2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_type_x_field_no_plugged.csv")

