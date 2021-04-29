## Tracey Mangin
## April 29, 2021
## process well injection, save for later use through out

# ------------------------------------------- INPUTS -----------------------------------

data_dir     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
minj_fil    <- "well_inject_m.rds"
wells_19_fil <- "wells_19.csv"

# ------------------------------------------- MAIN -----------------------------------

# load libraries -------- 

library(data.table)  
library(tidyverse)
library(zoo)

# load data ------

wells_19 <- fread(paste0(data_dir, wells_19_fil))
wells_19[, api_ten_digit := substr(API, 1, 10)]
wells_19[, FieldCode := paste0("00", FieldCode)][, FieldCode := str_sub(FieldCode, start= -3)]

well_inj <- read_rds(paste0(data_dir, minj_fil))

setDT(well_inj)

well_inj[, ':=' (api_ten_digit = substr(APINumber, 1, 10),
                  FieldCode = paste0("00", FieldCode))][, FieldCode := str_sub(FieldCode, start= -3)]

well_inj[, month_year :=  as.Date(as.yearmon(paste(year, month, sep = "-")))]

## make df of field codes and field names to join with well prod
fieldcodes <- unique(wells_19[, .(FieldCode, FieldName)])

## join to well prod
well_inj <- merge(well_inj, fieldcodes)
# well_prod[, ':=' (FieldCode = fifelse(FieldCode == "848", "849", FieldCode),
#                   FieldName = fifelse(FieldName == "Old Wilmington (ABD)", "Wilmington", FieldName))]

setnames(well_inj, c("FieldCode", "FieldName"), c('doc_field_code', "doc_fieldname"))

## remove gas fields
well_inj[, gas_field := str_detect(doc_fieldname, "Gas")][, gas_field := ifelse(gas_field == TRUE, 1, 0)]

## check
field_types <- unique(well_inj[, .(doc_fieldname, doc_field_code, gas_field)])

## filter out gas fields
well_inj <- well_inj[gas_field == 0]
well_inj[, gas_field := NULL]

setcolorder(well_inj, c("ReportType", "APINumber", "api_ten_digit", "doc_field_code", "doc_fieldname", "county", 
                         "county_name", "AreaCode", "PoolCode", "WellTypeCode", "well_type_name", "InjectionDate", 
                         "year", "month", "month_year", "InjectionStatus", "GasAirInjected", "SteamWaterInjected",
                         "DaysInjecting", "SurfaceInjectionPressure", "CasingInjectionPressure", "WaterSource", 
                         "WaterKind", "ReportedOrEstimated"))

fwrite(well_inj, paste0(data_dir, 'well_inj_m_processed.csv'))


