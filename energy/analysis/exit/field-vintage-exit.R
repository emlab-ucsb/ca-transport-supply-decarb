## Tracey Mangin and Ruiwen Lee
## May 24, 2021
## Create well exit variable

library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(zoo)

## set directory
proj_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
raw_dir            <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/"
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
output_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/exit/"

## files
prod_file               <- "well_prod_m_processed.csv"
well_file               <- "AllWells_table/AllWells_20210427.csv"
well_start_file         <- "well_start_prod_api10_revised.csv"
no_prod_file            <- "no_prod_wells_out.csv"

## read in files
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## for initial year production
init_yr_prod <- fread(paste0(proj_dir, "outputs/stocks-flows/well_start_yr/", well_start_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                      'doc_field_code' = 'character',
                                                                                                      'api_field' = 'character')) 
## get start year for each well
init_start_yr <- init_yr_prod %>%
  select(api_ten_digit, start_date) %>%
  unique() %>%
  mutate(start_year = year(start_date)) %>%
  select(-start_date)

## read in file of wells with zero production after 5y or 10y
no_prod_wells <- fread(paste0(output_dir, no_prod_file), colClasses = c('api_ten_digit' = 'character'))

no_prod_5 <- no_prod_wells %>% filter(year_cut_off == 5) %>% select(api_ten_digit) %>% unique()
no_prod_5_vec <- no_prod_5$api_ten_digit

no_prod_10 <- no_prod_wells %>% filter(year_cut_off == 10) %>% select(api_ten_digit) %>% unique()
no_prod_10_vec <- no_prod_10$api_ten_digit

## all wells
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

## wells that are now plugged
plugged_api <- status %>%
  filter(well_status == "Plugged")
plugged_api_vec <- plugged_api$api_ten_digit

## wells that produced at some point and are now plugged
pos_plugged_api_vec <- intersect(plugged_api_vec, pos_api_vec)

## well add column for month_year, select certain columns
well_prod2 <- well_prod[, .(api_ten_digit, doc_field_code, doc_fieldname, month_year, OilorCondensateProduced)]

setnames(well_prod2, c("OilorCondensateProduced"),
         c("oil_prod"))


## filter for wells with positive production 
prod_dt <- well_prod2[api_ten_digit %chin% pos_api_vec]

prod_dt <- prod_dt[, lapply(.SD, sum, na.rm = T), .SDcols = c("oil_prod"), by = .(api_ten_digit, doc_field_code, doc_fieldname, month_year)]


##
prod_dt[, api_doc_field_code := paste0(api_ten_digit, "-", doc_field_code)]

well_year_combos <- expand.grid(api_doc_field_code = unique(prod_dt$api_doc_field_code),
                                month_year = unique(prod_dt$month_year))

well_year_combos <- well_year_combos %>%
  mutate(api_ten_digit = substr(api_doc_field_code, 1, 10),
         doc_field_code = str_sub(api_doc_field_code, - 3, - 1))  

## field code - field name combos to join
field_code <- unique(well_prod[, c('doc_field_code', 'doc_fieldname')])

well_year_combos <- well_year_combos %>%
  left_join(field_code)

setDT(well_year_combos)

prod_dt <- prod_dt[, c('api_ten_digit', 'doc_field_code', 'month_year', 'oil_prod')]

prod_dt <- merge(well_year_combos, prod_dt, all = TRUE)

prod_dt[is.na(prod_dt)] <- 0

## add month and year columns
prod_dt[, `:=` (month = month(month_year),
                year = year(month_year))]

setorder(prod_dt, api_ten_digit, month_year)

prod_dt[, api_doc_field_code := NULL]

## find final year of positive production by 
prod_dt <- prod_dt %>%
  group_by(api_ten_digit) %>%
  mutate(pos_year = ifelse(oil_prod > 0, year, 0)) %>%
  ungroup()

prod_dt <- prod_dt %>%
  group_by(api_ten_digit) %>%
  mutate(
    last_pos_year = max(pos_year)
  )  %>%
  ungroup()

## add start year
prod_dt <- prod_dt %>%
  left_join(init_start_yr)

## calculate relevant values
well_exit_dt <- prod_dt %>%
  select(api_ten_digit, doc_field_code, doc_fieldname, start_year, last_pos_year)

# Keep unique rows
well_exit_dt <- distinct(well_exit_dt)

# Exit year is first year with zero production
well_exit_dt <- well_exit_dt %>%
  mutate(
    exit_year = last_pos_year+1
  )

## note: some wells have multiple entries
View(well_exit_dt %>% group_by(api_ten_digit) %>% mutate(n = n()) %>% ungroup() %>% filter(n > 1))


## start the function
## -------------------------------------

calc_exits <- function(scen) {

  if (scen == 1) {
    
    filt_vec <- plugged_api_vec
    
  } else if(scen == 2) {
    
    filt_vec <- c(plugged_api_vec, no_prod_5_vec)
    
  } else if(scen == 3) {filt_vec  <- c(plugged_api_vec, no_prod_10_vec)
  
  } else if(scen == 4) {filt_vec <- no_prod_5_vec
  
  } else if(scen == 5) {filt_vec  <- no_prod_10_vec}

well_exit_dt <- well_exit_dt %>%
  filter(api_ten_digit %in% filt_vec) %>%
  group_by(doc_field_code) %>%
  add_count(exit_year)

field_exit_dt <- well_exit_dt %>%
  select(doc_field_code, doc_fieldname, start_year, exit_year, n)
field_exit_dt <- distinct(field_exit_dt)

field_year_combos <- expand.grid(doc_field_code = unique(field_exit_dt$doc_field_code),
                                 start_year = unique(field_exit_dt$start_year),
                                 exit_year = unique(field_exit_dt$exit_year)) 

field_exit_dt2 <- field_year_combos %>%
  left_join(field_exit_dt, by=c("doc_field_code", "start_year", "exit_year"))

field_exit_dt2 <- field_exit_dt2 %>%
  select(doc_field_code, start_year, exit_year, n)
field_exit_dt2[is.na(field_exit_dt2)] <- 0
field_exit_dt2 <- rename(field_exit_dt2, well_exits=n)

field_exit_dt3 <- field_exit_dt2 %>%
  mutate(exit_scen = ifelse(scen == 1, "plugged_wells",
                            ifelse(scen == 2, "plugged_and_5y", "plugged_and_10y")))

}

scen_vec <- c(1:5)

field_exit_out <- purrr::map(as.list(scen_vec), calc_exits) %>%
  bind_rows()


## Save field-year-level well exit data



write_csv(field_exit_out, file = paste0(output_dir, "well_exits.csv"))
