## Tracey Mangin
## August 27, 2020
## API of oil


library(tidyverse)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10)) 
## for initial year production
init_yr_prod <- read.csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10_x_field.csv") %>%
  mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode)) 


## wellstar production

well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2) %>%
  mutate(FieldCode = ifelse(FieldCode == "848", "849", FieldCode))

# field codes
fieldcodes <- wells_19 %>%
  select(FieldCode, FieldName) %>%
  unique()

## top ten fields
top_fields <- well_prod %>%
  group_by(FieldCode, year) %>%
  summarise(yr_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(yr_rank = rank(-yr_prod)) %>%
  ungroup() %>%
  group_by(FieldCode) %>%
  mutate(total_prod = sum(yr_prod)) %>%
  ungroup() %>%
  left_join(fieldcodes) %>%
  group_by(year) 


top_fields_all_time <- well_prod %>%
  group_by(FieldCode) %>%
  summarise(total_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(fieldcodes) %>%
  mutate(rank = rank(-total_prod))

top_fields_all_time10 <- top_fields_all_time %>%
  filter(rank <= 10)

## api over time, weighted mean, by field
api_time <- well_prod %>%
  # filter(FieldCode %in% top_fields_all_time10$FieldCode) %>%
  filter(APIGravityofOil <= 50,
         OilorCondensateProduced > 0) %>%
  group_by(FieldCode, year) %>%
  summarise(wm_api = weighted.mean(APIGravityofOil, OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(fieldcodes)

## save 
write_csv(api_time, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/annual_wm_api_x_field.csv")


# ggplot(api_time, aes(x = year, y = wm_api, color = FieldName)) +
#   geom_line(size = 1, alpha = 0.7) +
#   ylab("Mean API gravity of oil\n(weighted by oil production") +
#   base_theme 

## mean api of new wells weighted by production


