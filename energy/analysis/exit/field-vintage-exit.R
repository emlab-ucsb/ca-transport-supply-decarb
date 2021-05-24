## May 24, 2021
## well exit

library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)

## set directory
proj_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
raw_dir            <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/"
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
output_dir <- "outputs/exit/"

## files
prod_file       <- "well_prod_m_processed.csv"
well_file    <- "AllWells_table/AllWells_20210427.csv"

## read in files
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## all wells
# all_wells <- read_xlsx("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/All_wells_20200417.xlsx") %>%
#   mutate(spud_date = convertToDate(SpudDate)) 

all_wells <- fread(paste0(raw_dir, well_file))

## wells status
status <- all_wells %>%
  mutate(api_ten_digit = paste0("0", API)) %>%
  select(api_ten_digit, well_status = WellStatus)

## find wells that produce at some point over time period
pos_well_api_prod <- well_prod %>%
  group_by(api_ten_digit) %>%
  summarise(oil_total = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(pos_pro = ifelse(oil_total > 0, 1, 0)) %>%
  filter(pos_pro == 1)

pos_api_vec <- pos_well_api_prod$api_ten_digit

## well add column for month_year, select certain columns
well_prod2 <- well_prod[, .(api_ten_digit, doc_field_code, doc_fieldname, month_year, OilorCondensateProduced)]

setnames(well_prod2, c("OilorCondensateProduced"),
         c("oil_prod"))


## filter for wells with positive production 
prod_dt <- well_prod2[api_ten_digit %chin% pos_api_vec]

prod_dt <- prod_dt[, lapply(.SD, sum, na.rm = T), .SDcols = c("oil_prod"), by = .(api_ten_digit, doc_field_code, doc_fieldname, month_year)]


##
well_year_combos <- expand.grid(api_ten_digit = unique(prod_dt$api_ten_digit),
                                month_year = unique(prod_dt$month_year)) 

setDT(well_year_combos)

prod_dt <- merge(well_year_combos, prod_dt, all = TRUE)

prod_dt[is.na(prod_dt)] <- 0

## add month and year columns
prod_dt[, `:=` (month = month(month_year),
                year = year(month_year))]

setorder(prod_dt, api_ten_digit, month_year)

## add vintage

## find final year of positive production by 

## calculate relevant values


