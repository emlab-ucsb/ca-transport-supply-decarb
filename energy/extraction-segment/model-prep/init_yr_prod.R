## Tracey Mangin
## August 7, 2020
## create initial year of production file

## libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(rebus)

## directories
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## files
prod_file       <- "well_prod_m_processed.csv"

## read in files
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## top ten fields
top_fields <- well_prod[, .(yr_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code, year)]
top_fields[, yr_rank := rank(-yr_prod), by = .(year)]
top_fields[, total_prod := sum(yr_prod), by = .(doc_field_code)]

## relative production by field
rel_prod <- well_prod[, .(yr_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code, year)]
rel_prod[, total_prod := sum(yr_prod), by = .(year)]
rel_prod[, rel_prod := yr_prod / total_prod]

## which wells produce oil in the historic period? API 10 digit
pos_crude_prod <- well_prod[, .(sum_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(api_ten_digit)]
pos_crude_prod <- pos_crude_prod[sum_prod > 0]
pos_crude_prod[, sum_prod := NULL]

## Find initial year of production for API ten digit
## -------------------------------------------------------------------------------
init_yr_prod_api10_m2 <- copy(well_prod)
init_yr_prod_api10_m2[, api_field := paste0(api_ten_digit, "-", doc_field_code)]
init_yr_prod_api10_m2 <- merge(pos_crude_prod, init_yr_prod_api10_m2)
init_yr_prod_api10_m2 <- init_yr_prod_api10_m2[, c("api_ten_digit", "doc_field_code", "api_field", "ProductionReportDate",
                                                   "month", "year", "month_year", "DaysProducing", "ProductionStatus", 
                                                   "APIGravityofOil", "OilorCondensateProduced")]

init_yr_prod_api10_m2[, productive := fifelse(OilorCondensateProduced > 0, 1, 0)]

setorder(init_yr_prod_api10_m2, api_ten_digit, ProductionReportDate)

## cumulative sum of positive production dates, select first date of production
init_yr_prod_api10_m2[, cumsum := cumsum(productive), by = .(api_ten_digit)]

init_yr_prod_api10_m2 <- init_yr_prod_api10_m2[cumsum == 1]
init_yr_prod_api10_m2 <- init_yr_prod_api10_m2[productive == 1]
setnames(init_yr_prod_api10_m2, "month_year", "start_date")
init_yr_prod_api10_m2 <- init_yr_prod_api10_m2[, c("api_ten_digit", "start_date")]

## how many field starts per well > 1?
init_yr_prod_api10_m2_n <- init_yr_prod_api10_m2[, .N, by = api_ten_digit]
init_yr_prod_api10_m2_n[N > 1]
## 0

## create df of well-year combos (balance the data set) -- has to be at field level as well
pos_crude_prod_f <- well_prod[, .(sum_prod = sum(OilorCondensateProduced, na.rm = T)), by = .(api_ten_digit, doc_field_code)]
pos_crude_prod_f[, api_field := paste0(api_ten_digit, "-", doc_field_code)]
pos_crude_prod_f <- pos_crude_prod_f[sum_prod > 0]
pos_crude_prod_f[, sum_prod := NULL]
pos_crude_prod_f_id <- pos_crude_prod_f[, c(api_field)]

## all well, field, date combos
well_year_combos <- expand.grid(api_field = as.character(unique(pos_crude_prod_f$api_field)),
                                month_year = unique(well_prod$month_year))

well_year_combos <- as.data.table(well_year_combos)
setorder(well_year_combos, api_field, month_year)

## next add to all years of production
well_prod_bal <- well_prod[, c("api_ten_digit", "doc_field_code", "month_year", "OilorCondensateProduced")]
well_prod_bal[, api_field := paste0(api_ten_digit, "-", doc_field_code)]

## summarise (multiple production entries for each field x well combo because of different pools)
well_prod_bal <- well_prod_bal[, .(prod_bbl = sum(OilorCondensateProduced, na.rm = T)), 
                                   by = .(api_ten_digit, doc_field_code, api_field, month_year)]

well_prod_bal <- well_prod_bal[api_field %chin% pos_crude_prod_f_id]

## join with all combos
well_prod_bal_all <- full_join(well_year_combos, well_prod_bal)

## fill in NA gaps
well_prod_bal_all[, api_ten_digit := fifelse(is.na(api_ten_digit), as.character(substr(api_field, 1, 10)), api_ten_digit)]
well_prod_bal_all[, doc_field_code := fifelse(is.na(doc_field_code), as.character(str_sub(api_field, -3, -1)), doc_field_code)]

## set order
setorder(well_prod_bal_all, api_field, month_year)

## add start date
well_prod_api10_sd <- left_join(well_prod_bal_all, init_yr_prod_api10_m2) %>%
  mutate(month_year = as.Date(month_year, format = "%y/%m/%d"),
         start_date = as.Date(start_date, format = "%y/%m/%d"),
         well_age = month_year - start_date) %>%
  filter(well_age >= 0)

## save output
write_csv(well_prod_api10_sd, path = paste0(save_directory, "stocks-flows/well_start_yr/well_start_prod_api10_revised.csv"))


## check annual production
init_prod_a <- well_prod_api10_sd %>%
  mutate(year = year(month_year)) %>%
  group_by(year) %>%
  summarise(total_prod = sum(prod_bbl, na.rm = T)) %>%
  ungroup()

total_prod <- well_prod %>%
  group_by(year) %>%
  summarise(prod2 = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(init_prod_a) %>%
  mutate(diff = total_prod - prod2)






