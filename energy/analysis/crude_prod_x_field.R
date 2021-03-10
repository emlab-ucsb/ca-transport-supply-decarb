## Tracey Mangin
## June 9, 2020
## well vintage and productin

library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(readxl)
library(openxlsx)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10))

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10))


## well prod
well_prod_field <- well_prod %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode) %>%
  rename(FieldCode = FieldCode3) %>%
  group_by(FieldCode, year) %>%
  summarise(total_bbls = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

well_prod_field2 <- expand.grid(FieldCode = unique(well_prod_field$FieldCode),
                                year = unique(well_prod_field$year)) %>%
  left_join(well_prod_field) %>%
  mutate(total_bbls = ifelse(is.na(total_bbls), 0, total_bbls)) %>%
  arrange(FieldCode, year)
  

write_csv(well_prod_field2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/crude_prod_x_field.csv")




