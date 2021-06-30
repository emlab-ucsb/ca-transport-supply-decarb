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
  group_by(api_ten_digit, doc_field_code) %>%
  summarise(oil_total = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(pos_pro = ifelse(oil_total > 0, 1, 0)) %>%
  filter(pos_pro == 1)

pos_api_vec <- pos_well_api_prod$api_ten_digit

## repeat, well-field level
pos_api_field_prod <- well_prod %>%
  mutate(api_field_code = paste0(api_ten_digit, "-", doc_field_code)) %>%
  group_by(api_field_code) %>%
  summarise(oil_total = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(pos_pro = ifelse(oil_total > 0, 1, 0)) %>%
  filter(pos_pro == 1)

pos_api_field_vec <- pos_api_field_prod$api_field_code

## well add column for month_year, select certain columns
well_prod2 <- well_prod[, .(api_ten_digit, doc_field_code, month_year, OilorCondensateProduced, GasProduced, WaterProduced)]

well_prod2[, api_field_code := paste0(api_ten_digit, "-", doc_field_code)]

setnames(well_prod2, c("OilorCondensateProduced", "GasProduced", "WaterProduced"),
         c("oil_prod", "gas_prod", "water_prod"))


## start making stuff for histogram
prod_dt <- well_prod2[api_field_code %chin% pos_api_field_vec]

prod_dt <- prod_dt[, lapply(.SD, sum, na.rm = T), .SDcols = c("oil_prod", "gas_prod", "water_prod"), by = .(api_field_code, month_year)]


##
well_year_combos <- expand.grid(api_field_code = unique(prod_dt$api_field_code),
                                month_year = unique(prod_dt$month_year)) 

setDT(well_year_combos)

prod_dt <- merge(well_year_combos, prod_dt, all = TRUE)

prod_dt[is.na(prod_dt)] <- 0

setorder(prod_dt, api_field_code, month_year)

# prod_dt[, oil_or_gas_prod := fifelse(oil_prod > 0 | gas_prod > 0, 1, 0)]
# 
# prod_dt[, `:=` (zero_prod = fifelse(oil_or_gas_prod > 0, 0, 1),
#                 pos_prod = fifelse(oil_or_gas_prod == 1, 1, 0))]

prod_dt[, `:=` (zero_prod = fifelse(oil_prod > 0, 0, 1),
                pos_prod = fifelse(oil_prod > 0, 1, 0))]

prod_dt[, cumul_zero_prod := hutilscpp::cumsum_reset(as.logical(zero_prod), zero_prod), api_field_code]

prod_dt[, max_of_grp := shift(cumul_zero_prod, type = "lead"), by = api_field_code]

prod_dt[, max_of_grp := fifelse(is.na(max_of_grp), 0, max_of_grp)]

prod_dt[, max_of_grp2 := fifelse(cumul_zero_prod > 0 & max_of_grp == 0, cumul_zero_prod, 0)]

prod_dt[, zero_prod_end := fifelse(month_year == max(month_year) & max_of_grp2 > 0, 1, 0)]

prod_dt[, cumul_prod := cumsum(pos_prod), api_field_code]

prod_dt[, cumul_prod_next := shift(cumul_prod, type = "lead"), api_field_code]

## use this mark removal of zero production before first production date in historic data set
prod_dt[, zero_prod_start := fifelse(max_of_grp2 > 0 & cumul_prod_next == 1, 1, 0)]

## smaller df, filter for rows with the "breaks"
zero_prod_dt <- prod_dt[max_of_grp2 > 0]

zero_prod_dt[, c("gas_prod", "water_prod", "cumul_zero_prod", "max_of_grp", "cumul_prod", "cumul_prod_next") := NULL]

zero_prod_dt[is.na(zero_prod_dt)] <- 0

## remove zero production break before first poroduction period
## newer wells will not have been "idle" befor that; older wells, maybe
zero_prod_dt_filt <- zero_prod_dt[zero_prod_start != 1]

setDT(status)

zero_prod_dt_filt[, api_ten_digit := substr(api_field_code, 1, 10)]

zero_prod_dt_filt2 <- setDT(left_join(zero_prod_dt_filt, status, by = "api_ten_digit"))

## mark the tail of dataset.... use this for tracking for now
zero_prod_dt_filt2[, remove_tail_all := fifelse(zero_prod_end == 1, 1, 0)]

# zero_prod_dt_filt2[, remove_tail_threeyr := fifelse(zero_prod_end == 1 & max_of_grp2 > 36, 1, 0)]

# zero_prod_dt_filt3 <- zero_prod_dt_filt2[remove_tail == 0]

setnames(zero_prod_dt_filt2, "max_of_grp2", "zero_prod_months")

## add doc field code
zero_prod_dt_filt2[, doc_field_code := stringr::str_sub(api_field_code, - 3, - 1)]

## make histogram

zero_prod_hist <- ggplot(zero_prod_dt_filt2 %>% filter(remove_tail_all == 0), aes(x = zero_prod_months)) +
  geom_histogram(binwidth = 1)

## What proportion of wells do non-abandoned exited wells represent? 
## of the wells in our data that had ever stopped producing for X months consecutively, 
## how many were never reactivated, i.e. never produced again. Try 6, 12, 24 months.

calc_zero_prod <- function(n_month_val) {
  
  ## filter for number of months
  tmp_all <- zero_prod_dt_filt2[zero_prod_months >= n_month_val]

  ## prod stop
  tmp_zero_prod <- tmp_all[, n := .N, by = .(api_field_code)]
  
  ## those that stop after n months
  tmp_stop_prod <- tmp_zero_prod[zero_prod_end == 1 & n == 1]
  
  tmp_n_stop_n <- length(unique(tmp_stop_prod[, api_field_code]))
  
  ## n pause and stop
  tmp_break_stop <- tmp_zero_prod[zero_prod_end == 1 & n > 1]
  
  tmp_n_break_stop_n <- length(unique(tmp_break_stop[, api_field_code]))
  
  ## n plugged in stop 
  tmp_stop_status <- tmp_stop_prod[well_status == "Plugged", c(n = .N), by = .(well_status)]
  tmp_stop_break_status <- tmp_break_stop[well_status == "Plugged", c(n = .N), by = .(well_status)]
  
  plugged_df <- rbind(tmp_stop_status, tmp_stop_break_status)
  
  tmp_plugged_val <- as.numeric(sum( plugged_df[, V1]))
  
  ## break and then produce 
  tmp_break <- tmp_zero_prod[zero_prod_end != 1]
  tmp_break <- tmp_break[!(api_field_code %in% c(tmp_stop_prod[, api_field_code], tmp_break_stop[, api_field_code]))]
  
  tmp_n_break_vec <- length(unique(tmp_break[, api_field_code]))
  
  ## total wells in threshold
  tmp_all <- zero_prod_dt_filt2[zero_prod_months >= n_month_val]
  tmp_n_total <- length(unique(tmp_all[, api_field_code]))

  ## make data table
  out_df <- data.table(zero_prod_length = n_month_val,
                       n_stop_only = tmp_n_stop_n,
                       n_break_stop = tmp_n_break_stop_n, 
                       n_stop_plugged = tmp_plugged_val,
                       rel_plugged = tmp_plugged_val / (tmp_n_stop_n + tmp_n_break_stop_n),
                       n_break_only = tmp_n_break_vec,
                       n_stop_break = tmp_n_total,
                       rel_stop = tmp_n_stop_n / tmp_n_total)
  
}

month_vec <- c(1:10) * 12

zero_prod_out <- purrr::map(as.list(month_vec), calc_zero_prod) %>%
  bind_rows()

fwrite(zero_prod_out, paste0(proj_dir, output_dir, 'zero_prod_breaks_stops.csv'), row.names = F)


## function that filters for wells that are inactive for X years and never produce again

filt_zero_prod <- function(n_month_val) {
  
  ## filter for number of months
  tmp_zero_prod <- zero_prod_dt_filt2[zero_prod_months >= n_month_val]
  
  ## keep only those that stop production
  tmp_zero_prod <- tmp_zero_prod[zero_prod_end == 1]
  
  ## make data table
  out_df <- tmp_zero_prod %>% 
    select(api_field_code, api_ten_digit, doc_field_code, zero_prod_months, well_status) %>%
    mutate(year_cut_off = n_month_val / 12)
  
}


cat_vec <- c(5, 10) * 12

no_prod_wells_out <- purrr::map(as.list(cat_vec), filt_zero_prod) %>%
  bind_rows()

fwrite(no_prod_wells_out, paste0(proj_dir, output_dir, 'no_prod_wells_out.csv'), row.names = F)




