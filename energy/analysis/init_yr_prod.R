## Tracey Mangin
## August 7, 2020
## rename init df


library(tidyverse)
library(lubridate)
library(zoo)
library(sf)
# library(scales)
library(rebus)
library(readxl)
library(openxlsx)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"


## load the data
well_prod <- read_rds(paste0(data_directory, "well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  mutate(FieldCode2 = paste0("00", FieldCode),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = FieldCode,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2) %>%
  mutate(month_year =  as.Date(as.yearmon(paste(year, month, sep = "-"))))

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10))

all_wells <- read_xlsx("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/All_wells_20200417.xlsx") %>%
  mutate(spud_date = convertToDate(SpudDate))

##
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
  left_join(fieldcodes)

## how much prodcution from any field
rel_prod <- well_prod %>%
  group_by(FieldCode, year) %>%
  summarise(yr_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_prod = sum(yr_prod)) %>%
  ungroup() %>%
  mutate(rel_prod = yr_prod / total_prod)

## Make dataframes with production and start date for each well-field combination
## -------------------------------------------------------------------------------

## which wells produce oil? 10-digit API
pos_crude_prod <- well_prod %>%
  group_by(api_ten_digit, FieldCode) %>%
  summarise(sum_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(sum_prod > 0) %>%
  mutate(api_field = paste0(api_ten_digit, "-", FieldCode))

## which wells produce oil? 12-digit API
pos_crude_prod_api12 <- well_prod %>%
  group_by(APINumber, api_ten_digit, FieldCode) %>%
  summarise(sum_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(sum_prod > 0) %>%
  mutate(api_field = paste0(APINumber, "-", FieldCode)) 

# anti_join(pos_crude_prod_api12 %>% select(-sum_prod), pos_crude_prod)


## first, api-ten digit level (api10-field code)
## -----------------------------------------------------------------

init_yr_prod_api10 <- well_prod %>%
  mutate(api_field = paste0(api_ten_digit, "-", FieldCode)) %>%
  filter(api_field %in% pos_crude_prod$api_field) %>%
  select(api_ten_digit, FieldCode, api_field, ProductionReportDate, month, year, month_year, DaysProducing, ProductionStatus,  APIGravityofOil, OilorCondensateProduced) %>%
  mutate(productive = ifelse(OilorCondensateProduced > 0, 1, 0)) %>%
  arrange(api_ten_digit, FieldCode, api_field, ProductionReportDate) %>%
  group_by(api_ten_digit, FieldCode, api_field) %>%
  mutate(cumsum = cumsum(productive)) %>%
  ungroup() %>%
  filter(cumsum == 1) %>%
  group_by(api_ten_digit, FieldCode, api_field) %>%
  slice(which.min(ProductionReportDate)) %>%
  ungroup() %>%
  rename(start_date = month_year) %>%
  select(api_ten_digit, FieldCode, api_field, start_date)

## how many field starts per well > 1?
init_yr_prod_api10_n <- init_yr_prod_api10 %>%
  group_by(api_ten_digit) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

## production implication... 
prod_mult_starts <- well_prod %>%
  mutate(api_field = paste0(api_ten_digit, "-", FieldCode)) %>%
  filter(api_ten_digit %in% init_yr_prod_api10_n$api_ten_digit) %>%
  group_by(FieldCode, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(top_fields) %>%
  mutate(div = prod / total_prod,
         perc = div * 100)
## 0-27%

## how many go from 000 to a field?
init_yr_prod_api10b <- init_yr_prod_api10 %>%
  filter(api_ten_digit %in% init_yr_prod_api10_n$api_ten_digit) %>%
  arrange(api_ten_digit, start_date) %>%
  mutate(any000 = ifelse(FieldCode == "000", 1, NA)) %>%
  arrange(api_ten_digit, FieldCode) %>%
  group_by(api_ten_digit) %>%
  fill(any000) %>%
  ungroup() %>%
  arrange(api_ten_digit, start_date)
## most with a 000 go from a field to 000, although some go from 000 to a defined field.
## all of the changes happen in 2018 with 000

init_yr_prod_api10b_s2 <- init_yr_prod_api10b %>%
  group_by(api_ten_digit) %>%
  slice(which.max(start_date)) 
## different well does not always start in 2018

## 
# init_yr_prod_api10c <- init_yr_prod_api10 %>%  
#   group_by(api_ten_digit, FieldCode) %>%
#   slice(which.min(start_date)) %>%
#   ungroup() %>%
#   select(api_ten_digit, FieldCode, start_date)


## get all combos
well_year_combos <- expand.grid(api_field = as.character(unique(pos_crude_prod$api_field)),
                                month_year = unique(well_prod$month_year))

## next add to all years of produciton

# test_df <- pos_crude_prod[1:100, ]
  

init_yr_prod_api10_all <- well_prod %>%
  mutate(api_field = paste0(api_ten_digit, "-", FieldCode)) %>%
  filter(api_field %in% pos_crude_prod$api_field) %>%
  group_by(api_ten_digit, FieldCode, api_field, ProductionReportDate, month_year) %>%
  summarise(api_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  full_join(well_year_combos) %>%
  mutate(api_prod = ifelse(is.na(api_prod), 0, api_prod)) %>%
  arrange(api_field, month_year) %>%
  mutate(api_ten_digit = ifelse(is.na(api_ten_digit), substr(api_field, start = 1, stop = 10), api_ten_digit),
         FieldCode = ifelse(is.na(FieldCode), substr(api_field, start = 12, stop = 14), FieldCode)) %>%
  left_join(init_yr_prod_api10) 

## n_field
n_field <- init_yr_prod_api10 %>%
  arrange(api_ten_digit, start_date) %>%
  mutate(n_field_temp = 1) %>%
  group_by(api_ten_digit) %>%
  mutate(n_field = cumsum(n_field_temp)) %>%
  ungroup() %>%
  select(api_field, start_date, n_field)


init_yr_prod_api10_all2 <- init_yr_prod_api10_all %>%
  left_join(n_field) %>%
  mutate(well_age = month_year - start_date) %>%
  mutate(month = month(month_year),
         year = year(month_year)) %>%
  filter(well_age >= 0) %>%
  select(api_ten_digit, FieldCode, n_field, api_field, start_date, ProductionReportDate, month, year, month_year, well_age, api_prod)


write_csv(init_yr_prod_api10_all2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10_x_field.csv")


## repeat, but just API ten, not with field (1 start date per api 10)
## -------------------------------------------------------------------------------

init_yr_prod_api10_m2 <- well_prod %>%
  mutate(api_field = paste0(api_ten_digit, "-", FieldCode)) %>%
  filter(api_ten_digit %in% pos_crude_prod$api_ten_digit) %>%
  select(api_ten_digit, FieldCode, api_field, ProductionReportDate, month, year, month_year, DaysProducing, ProductionStatus,  APIGravityofOil, OilorCondensateProduced) %>%
  mutate(productive = ifelse(OilorCondensateProduced > 0, 1, 0)) %>%
  arrange(api_ten_digit, ProductionReportDate) %>%
  group_by(api_ten_digit) %>%
  mutate(cumsum = cumsum(productive)) %>%
  ungroup() %>%
  filter(cumsum == 1) %>%
  group_by(api_ten_digit) %>%
  slice(which.min(ProductionReportDate)) %>%
  ungroup() %>%
  rename(start_date = month_year) %>%
  select(api_ten_digit, start_date)

## how many field starts per well > 1?
init_yr_prod_api10_m2_n <- init_yr_prod_api10_m2 %>%
  group_by(api_ten_digit) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
## 0


## next add to all years of produciton

# test_df <- pos_crude_prod[1:100, ]
# 
# 0401300009

## pos api 10
pos_10 <- well_prod %>%
  group_by(api_ten_digit) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(prod > 0)

# well_year_combos2 <- expand.grid(api_ten_digit = as.character(unique(pos_10$api_ten_digit)),
#                                  month_year = unique(well_prod$month_year))

init_yr_prod_api10_m2_all <- well_prod %>%
  mutate(api_field = paste0(APINumber, "-", FieldCode)) %>%
  filter(api_ten_digit %in% pos_10$api_ten_digit) %>%
  # group_by(api_ten_digit, ProductionReportDate, month_year) %>%
  # mutate(api_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  # ungroup() %>%
  arrange(month_year) %>%
  left_join(init_yr_prod_api10_m2) 
  
init_yr_prod_api10_m2_all2 <- init_yr_prod_api10_m2_all %>%
  select(api_ten_digit, FieldCode, api_field, month_year, month, year, OilorCondensateProduced, start_date) %>%
  full_join(well_year_combos) 



# init_yr_prod_api10_m2_all2 <- init_yr_prod_api10_m2_all %>%
#   group_by(api_ten_digit) %>%
#   filter(month_year >= start_date) %>%
#   ungroup()

# ## zero_production
# last_year_prod <- init_yr_prod_api10_m2_all2 %>%
#   filter(api_prod > 0) %>%
#   group_by(api_ten_digit) %>%
#   filter(month_year == max(month_year)) %>%
#   ungroup() %>%
#   select(api_ten_digit, last_prod_date = month_year) %>%
#   unique()
# 
## check wells in two fields
n_fields <- well_prod %>%
  group_by(api_ten_digit, FieldCode) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(prod > 0) %>%
  unique() %>%
  group_by(api_ten_digit) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
# 
# 
# ## how big is this multiple field problem?
# well_field_prod <- init_yr_prod_api10_m2_all2 %>%
#   left_join(last_year_prod) %>%
#   filter(api_ten_digit %in% n_fields$api_ten_digit,
#          month_year <= last_prod_date) %>%
#   group_by(api_ten_digit, FieldCode) %>%
#   summarise(total_production = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup() %>%
#   group_by(api_ten_digit) %>%
#   mutate(total_well_prod = sum(total_production)) %>%
#   ungroup() %>%
#   mutate(rel_prod = total_production / total_well_prod,
#          diff = total_well_prod - total_production)
# 
# ## do any have production in two fields, final year?
# final_prod <- init_yr_prod_api10_m2_all %>%
#   left_join(last_year_prod) %>%
#   filter(month_year <= last_prod_date) %>%
#   mutate(count = 1) %>%
#   group_by(api_ten_digit) %>%
#   mutate(cumsum = cumsum(count),
#          max_count = max(cumsum),
#          max_sub = max_count - 12) %>%
#   filter(cumsum > max_sub) %>%
#   ungroup() %>%
#   filter(api_ten_digit %in% n_fields$api_ten_digit,
#          month_year == last_prod_date) %>%
#   group_by(api_ten_digit, FieldCode, year) %>%
#   summarise(total_production = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup() %>%
#   left_join(fieldcodes) %>%
#   group_by(api_ten_digit) %>%
#   mutate(total_well_prod = sum(total_production)) %>%
#   ungroup() %>%
#   mutate(rel_prod = total_production / total_well_prod,
#          diff = total_well_prod - total_production)



init_yr_prod_api10_all2 <- init_yr_prod_api10_m2_all2 %>%
  arrange(api_ten_digit, month_year) %>%
  group_by(api_ten_digit) %>%
  fill(start_date) %>%
  ungroup()



# ## n_field
# n_field <- init_yr_prod_api10 %>%
#   arrange(api_ten_digit, start_date) %>%
#   mutate(n_field_temp = 1) %>%
#   group_by(api_ten_digit) %>%
#   mutate(n_field = cumsum(n_field_temp)) %>%
#   ungroup() %>%
#   select(api_field, start_date, n_field)


init_yr_prod_api10_all3 <- init_yr_prod_api10_all2 %>%
  rename(prod = OilorCondensateProduced) %>%
  mutate(well_age = month_year - start_date) %>%
  filter(well_age >= 0) 

write_csv(init_yr_prod_api10_all3, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10.csv")

init_prod_a <- init_yr_prod_api10_all3 %>%
  group_by(year) %>%
  summarise(total_prod = sum(prod, na.rm = T)) %>%
  ungroup()

total_prod <- well_prod %>%
  group_by(year) %>%
  summarise(prod2 = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(init_prod_a) %>%
  mutate(diff = total_prod - prod2)
  

## do this again at the 12 digit api
## -------------------------------------------------------------

init_yr_prod_api12 <- well_prod %>%
  mutate(api_field = paste0(APINumber, "-", FieldCode)) %>%
  filter(api_field %in% pos_crude_prod_api12$api_field) %>%
  select(APINumber, FieldCode, api_field, ProductionReportDate, month, year, month_year, DaysProducing, ProductionStatus,  APIGravityofOil, OilorCondensateProduced) %>%
  mutate(productive = ifelse(OilorCondensateProduced > 0, 1, 0)) %>%
  arrange(APINumber, FieldCode, api_field, ProductionReportDate) %>%
  group_by(APINumber, FieldCode, api_field) %>%
  mutate(cumsum = cumsum(productive)) %>%
  ungroup() %>%
  filter(cumsum == 1) %>%
  group_by(APINumber, FieldCode, api_field) %>%
  slice(which.min(ProductionReportDate)) %>%
  ungroup() %>%
  rename(start_date = month_year) %>%
  select(APINumber, FieldCode, api_field, start_date)

## how many field starts per well > 1?
init_yr_prod_api12_n <- init_yr_prod_api12 %>%
  group_by(APINumber) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

## production implication... 
prod_mult_starts12 <- well_prod %>%
  mutate(api_field = paste0(APINumber, "-", FieldCode)) %>%
  filter(APINumber %in% init_yr_prod_api12_n$APINumber) %>%
  group_by(FieldCode, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(top_fields) %>%
  mutate(div = prod / total_prod,
         perc = div * 100)
## 0-27%

## how many go from 000 to a field?
init_yr_prod_api12b <- init_yr_prod_api12 %>%
  filter(APINumber %in% init_yr_prod_api12_n$APINumber) %>%
  arrange(APINumber, start_date) %>%
  mutate(any000 = ifelse(FieldCode == "000", 1, NA)) %>%
  arrange(APINumber, FieldCode) %>%
  group_by(APINumber) %>%
  fill(any000) %>%
  ungroup() %>%
  arrange(APINumber, start_date)
## most with a 000 go from a field to 000, although some go from 000 to a defined field.
## all of the changes happen in 2018 with 000

init_yr_prod_api12b_s2 <- init_yr_prod_api12b %>%
  group_by(APINumber) %>%
  slice(which.max(start_date)) 
## different well does not always start in 2018

## 
# init_yr_prod_api10c <- init_yr_prod_api10 %>%  
#   group_by(api_ten_digit, FieldCode) %>%
#   slice(which.min(start_date)) %>%
#   ungroup() %>%
#   select(api_ten_digit, FieldCode, start_date)


## get all combos
well_year_combos12 <- expand.grid(api_field = as.character(unique(pos_crude_prod_api12$api_field)),
                                month_year = unique(well_prod$month_year))

## next add to all years of produciton

# test_df <- pos_crude_prod_api12[1:100, ]


init_yr_prod_api12_all <- well_prod %>%
  mutate(api_field = paste0(APINumber, "-", FieldCode)) %>%
  filter(api_field %in% pos_crude_prod_api12$api_field) %>%
  # filter(api_field %in% test_df$api_field) %>%
  group_by(APINumber, FieldCode, api_field, ProductionReportDate, month_year) %>%
  summarise(api_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  full_join(well_year_combos12) %>%
  mutate(api_prod = ifelse(is.na(api_prod), 0, api_prod)) %>%
  arrange(api_field, month_year) %>%
  mutate(APINumber = ifelse(is.na(APINumber), substr(api_field, start = 1, stop = 12), APINumber),
         FieldCode = ifelse(is.na(FieldCode), substr(api_field, start = 14, stop = 16), FieldCode)) %>%
  left_join(init_yr_prod_api12) 

## n_field
n_field12 <- init_yr_prod_api12 %>%
  arrange(APINumber, start_date) %>%
  mutate(n_field_temp = 1) %>%
  group_by(APINumber) %>%
  mutate(n_field = cumsum(n_field_temp)) %>%
  ungroup() %>%
  select(api_field, start_date, n_field)


init_yr_prod_api12_all2 <- init_yr_prod_api12_all %>%
  left_join(n_field12) %>%
  mutate(well_age = month_year - start_date) %>%
  mutate(month = month(month_year),
         year = year(month_year)) %>%
  filter(well_age >= 0) %>%
  select(APINumber, FieldCode, n_field, api_field, start_date, ProductionReportDate, month, year, month_year, well_age, api_prod)




write_csv(init_yr_prod_api12_all2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api12_x_field.csv")

## repeat, but just API 12, not with field (1 start date per api 12)
## -------------------------------------------------------------------------------

## pos api 12
pos_12 <- well_prod %>%
  group_by(APINumber) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(prod > 0)

## get all combos
well_year_combos12_m2 <- expand.grid(APInumber = as.character(unique(pos_12$APINumber)),
                                  month_year = unique(well_prod$month_year))

init_yr_prod_api12_m2 <- well_prod %>%
  filter(APINumber %in% pos_12$APINumber) %>%
  select(APINumber, FieldCode, ProductionReportDate, month, year, month_year, DaysProducing, ProductionStatus,  APIGravityofOil, OilorCondensateProduced) %>%
  mutate(productive = ifelse(OilorCondensateProduced > 0, 1, 0)) %>%
  arrange(APINumber, FieldCode, ProductionReportDate) %>%
  group_by(APINumber) %>%
  mutate(cumsum = cumsum(productive)) %>%
  ungroup() %>%
  filter(cumsum == 1) %>%
  group_by(APINumber) %>%
  slice(which.min(ProductionReportDate)) %>%
  ungroup() %>%
  rename(start_date = month_year) %>%
  select(APINumber, start_date)

## how many field starts per well > 1?
init_yr_prod_api12_m2_n <- init_yr_prod_api12_m2 %>%
  group_by(APINumber) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)


## next add to all years of produciton

# test_df <- pos_crude_prod_api12[1:100, ]


init_yr_prod_api12_m2_all <- well_prod %>%
  filter(APINumber %in% pos_12$APINumber) %>%
  group_by(APINumber, ProductionReportDate, month_year) %>%
  summarise(api_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  full_join(well_year_combos12_m2) %>%
  mutate(api_prod = ifelse(is.na(api_prod), 0, api_prod)) %>%
  arrange(APINumber, month_year) %>%
  left_join(init_yr_prod_api12_m2) 


init_yr_prod_api12_m2_all2 <- init_yr_prod_api12_m2_all %>%
  arrange(APINumber, month_year) %>%
  group_by(APINumber) %>%
  fill(start_date) %>%
  ungroup()


init_yr_prod_api12_m2_all3 <- init_yr_prod_api12_m2_all2 %>%
  mutate(well_age = month_year - start_date) %>%
  mutate(month = month(month_year),
         year = year(month_year)) %>%
  filter(well_age >= 0) 

write_csv(init_yr_prod_api12_m2_all3, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api12.csv")






## --------------------------------------------------------------
# 
# init_yr_prod <- well_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   mutate(api_field = paste0(api_ten_digit, "-", FieldCode)) %>%
#   filter(api_field %in% pos_crude_prod$api_field) %>%
#   select(api_ten_digit, FieldCode, api_field, ProductionReportDate, month, year, DaysProducing, ProductionStatus,  APIGravityofOil, OilorCondensateProduced) %>%
#   mutate(productive = ifelse(OilorCondensateProduced > 0, 1, 0)) %>%
#   arrange(api_ten_digit, FieldCode, api_field, ProductionReportDate) %>%
#   group_by(api_ten_digit, FieldCode, api_field) %>%
#   mutate(cumsum = cumsum(productive)) %>%
#   ungroup() %>%
#   filter(cumsum == 1) %>%
#   mutate(month_year = paste(year, month, sep = "-"),
#          month_year =  as.Date(as.yearmon(month_year))) %>%
#   group_by(api_ten_digit, FieldCode, api_field) %>%
#   slice(which.min(ProductionReportDate)) %>%
#   ungroup() %>%
#   rename(start_date = month_year) %>%
#   select(api_ten_digit, FieldCode, api_field, start_date)
# 
# 
# ## 
# init_yr_prod2 <- well_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   mutate(api_field = paste0(api_ten_digit, "-", FieldCode)) %>%
#   filter(api_field %in% pos_crude_prod$api_field) %>%
#   group_by(api_ten_digit, FieldCode, ProductionReportDate, year, month) %>%
#   summarise(api_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(month_year = paste(year, month, sep = "-"),
#          month_year =  as.Date(as.yearmon(month_year))) %>%
#   left_join(init_yr_prod) 
# 
# init_yr_prod3 <- init_yr_prod2 %>%
#   mutate(well_age = month_year - start_date) %>%
#   select(api_ten_digit, ProductionReportDate, year, month, month_year, FieldCode, start_date, year, api_prod)
# 
# 
# write_csv(init_yr_prod3, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/intermediary/well_start_prod.csv")


## ------------------------------------
## new prod field
## ------------------------------------


init_field_prod <- well_prod %>%
  select(FieldCode, ProductionReportDate, month_year, month, year, DaysProducing, ProductionStatus,  APIGravityofOil, OilorCondensateProduced) %>%
  mutate(productive = ifelse(OilorCondensateProduced > 0, 1, 0)) %>%
  arrange(FieldCode, ProductionReportDate) %>%
  group_by(FieldCode) %>%
  mutate(cumsum = cumsum(productive)) %>%
  ungroup() %>%
  filter(cumsum == 1) %>%
  group_by(FieldCode) %>%
  slice(which.min(ProductionReportDate)) %>%
  ungroup() %>%
  rename(start_date = month_year) %>%
  select(FieldCode, start_date, month, year) %>%
  left_join(fieldcodes) %>%
  select(FieldCode, FieldName, start_date, month, year)

write_csv(init_field_prod, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/init_field_prod_yr.csv")


# init_field_prod2 <- well_prod %>%
#   group_by(FieldCode, ProductionReportDate, year, month) %>%
#   summarise(field_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(month_year = paste(year, month, sep = "-"),
#          month_year =  as.Date(as.yearmon(month_year))) %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   left_join(init_field_prod) 
# 
# init_field_prod3 <- init_field_prod2 %>%
#   mutate(field_age = month_year - start_date) %>%
#   select(ProductionReportDate, year, month, month_year, FieldCode, start_date, year, field_prod)
# 
# 
# 
# ## load spatial data
# ## -----------------------------------------------------------------
# proj <- "+proj=longlat +datum=WGS84"
# 
# ## facility boundaries
# field_df <- read_sf(dsn = paste0(sp_dir, "raw/field-boundaries/", layer = "DOGGR_Admin_Boundaries_Master.shp")) %>%
#   st_transform(proj) %>%
#   select(FieldCode = FIELD_CODE, NAME)
# 
# 
# ## productive fields
# 
# field_prod <- well_prod %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode) %>%
#   rename(FieldCode = FieldCode3) %>%
#   group_by(FieldCode) %>%
#   summarise(sum_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup()
# 
# 
# ## match
# no_prod_df <- field_df %>%
#   left_join(field_prod) %>%
#   mutate(gas = str_detect(NAME, pattern = "Gas"),
#          abd = str_detect(NAME, pattern = "ABD"),
#          sum_prod = ifelse(is.na(sum_prod), 0, sum_prod))
# 
# 
# 
