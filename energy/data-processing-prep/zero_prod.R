## Tracey Mangin
## April 22, 2021
## zero production investigation

library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)
library(readxl)
library(openxlsx)

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"

## files
prod_file       <- "well_prod_m_processed.csv"

## read in files
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## all wells
all_wells <- read_xlsx("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/All_wells_20200417.xlsx") %>%
  mutate(spud_date = convertToDate(SpudDate)) 

## wells status
status <- all_wells %>%
  select(-SpudDate, -spud_date) %>%
  unique() %>%
  # filter(WellStatus %in% c("Plugged", "PluggedOnly")) %>%
  rename(api_ten_digit = API) %>%
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
well_prod2 <- well_prod[, .(api_ten_digit, month_year, OilorCondensateProduced, GasProduced, WaterProduced)]

setnames(well_prod2, c("OilorCondensateProduced", "GasProduced", "WaterProduced"),
         c("oil_prod", "gas_prod", "water_prod"))


## start making stuff for histogram
prod_dt <- well_prod2[api_ten_digit %chin% pos_api_vec]

prod_dt <- prod_dt[, lapply(.SD, sum, na.rm = T), .SDcols = c("oil_prod", "gas_prod", "water_prod"), by = .(api_ten_digit, month_year)]


##
well_year_combos <- expand.grid(api_ten_digit = unique(prod_dt$api_ten_digit),
                                month_year = unique(prod_dt$month_year)) 

setDT(well_year_combos)

prod_dt <- merge(well_year_combos, prod_dt, all = TRUE)

prod_dt[is.na(prod_dt)] <- 0

setorder(prod_dt, api_ten_digit, month_year)

# prod_dt[, oil_or_gas_prod := fifelse(oil_prod > 0 | gas_prod > 0, 1, 0)]
# 
# prod_dt[, `:=` (zero_prod = fifelse(oil_or_gas_prod > 0, 0, 1),
#                 pos_prod = fifelse(oil_or_gas_prod == 1, 1, 0))]

prod_dt[, `:=` (zero_prod = fifelse(oil_prod > 0, 0, 1),
                pos_prod = fifelse(oil_prod > 0, 1, 0))]

prod_dt[, cumul_zero_prod := hutilscpp::cumsum_reset(as.logical(zero_prod), zero_prod), api_ten_digit]

prod_dt[, max_of_grp := shift(cumul_zero_prod, type = "lead"), by = api_ten_digit]

prod_dt[, max_of_grp := fifelse(is.na(max_of_grp), 0, max_of_grp)]

prod_dt[, max_of_grp2 := fifelse(cumul_zero_prod > 0 & max_of_grp == 0, cumul_zero_prod, 0)]

prod_dt[, zero_prod_end := fifelse(month_year == max(month_year) & max_of_grp2 > 0, 1, 0)]

prod_dt[, cumul_prod := cumsum(pos_prod), api_ten_digit]

prod_dt[, cumul_prod_next := shift(cumul_prod, type = "lead"), api_ten_digit]

## use this to remove zero production before first production date in historic data set
prod_dt[, zero_prod_start := fifelse(max_of_grp2 > 0 & cumul_prod_next == 1, 1, 0)]

## smaller df
zero_prod_dt <- prod_dt[max_of_grp2 > 0]

zero_prod_dt[, c("gas_prod", "water_prod", "cumul_zero_prod", "max_of_grp", "cumul_prod", "cumul_prod_next") := NULL]

zero_prod_dt[is.na(zero_prod_dt)] <- 0

zero_prod_dt_filt <- zero_prod_dt[zero_prod_start != 1]

setDT(status)

zero_prod_dt_filt2 <- setDT(left_join(zero_prod_dt_filt, status, by = "api_ten_digit"))

## remove tail of dataset.... 
zero_prod_dt_filt2[, remove_tail_all := fifelse(zero_prod_end == 1, 1, 0)]

# zero_prod_dt_filt2[, remove_tail_threeyr := fifelse(zero_prod_end == 1 & max_of_grp2 > 36, 1, 0)]

# zero_prod_dt_filt3 <- zero_prod_dt_filt2[remove_tail == 0]

setnames(zero_prod_dt_filt2, "max_of_grp2", "zero_prod_months")

## make histogram

zero_prod_hist <- ggplot(zero_prod_dt_filt2 %>% filter(remove_tail_all == 0), aes(x = zero_prod_months)) +
  geom_histogram(binwidth = 1)

## What proportion of wells do non-abandoned exited wells represent? 
## of the wells in our data that had ever stopped producing for X months consecutively, 
## how many were never reactivated, i.e. never produced again. Try 6, 12, 24 months.





