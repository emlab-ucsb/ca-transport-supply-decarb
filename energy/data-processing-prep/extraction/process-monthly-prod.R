## Tracey Mangin
## March 10, 2021
## process well production, save for later use through out
## revised: feb 14, 2024 -haejin

# ------------------------------------------- INPUTS -----------------------------------

data_dir     <- "/capstone/freshcair/meds-freshcair-capstone/data/processed/"
mprod_fil    <- "well_prod_m.rds"
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

well_prod <- read_rds(paste0(data_dir, "well_prod_m.rds"))

setDT(well_prod)

well_prod[, ':=' (api_ten_digit = substr(APINumber, 1, 10),
                  FieldCode = paste0("00", FieldCode))][, FieldCode := str_sub(FieldCode, start= -3)]

well_prod[, month_year :=  as.Date(as.yearmon(paste(year, month, sep = "-")))]

## make df of field codes and field names to join with well prod
fieldcodes <- unique(wells_19[, .(FieldCode, FieldName)])
  
## join to well prod
well_prod <- merge(well_prod, fieldcodes)
# well_prod[, ':=' (FieldCode = fifelse(FieldCode == "848", "849", FieldCode),
#                   FieldName = fifelse(FieldName == "Old Wilmington (ABD)", "Wilmington", FieldName))]

setnames(well_prod, c("FieldCode", "FieldName"), c('doc_field_code', "doc_fieldname"))

## remove gas fields
well_prod[, gas_field := str_detect(doc_fieldname, "Gas")][, gas_field := ifelse(gas_field == TRUE, 1, 0)]

## check
field_types <- unique(well_prod[, .(doc_fieldname, doc_field_code, gas_field)])

## filter out gas fields
well_prod <- well_prod[gas_field == 0]
well_prod[, gas_field := NULL]

setcolorder(well_prod, c("ReportType", "APINumber", "api_ten_digit", "doc_field_code", "doc_fieldname", "county", 
                         "county_name", "AreaCode", "PoolCode", "WellTypeCode", "well_type_name", "ProductionReportDate", 
                         "year", "month", "month_year", "ProductionStatus", "CasingPressure", "TubingPressure", 
                         "BTUofGasProduced", "MethodOfOperation", "APIGravityofOil", "WaterDisposition", "OilorCondensateProduced", "DaysProducing",          
                         "GasProduced", "WaterProduced", "ReportedOrEstimated"))

fwrite(well_prod, paste0(data_dir, 'well_prod_m_processed.csv'))

## doc field codes, field names
field_info <- unique(well_prod[, c("doc_field_code", "doc_fieldname")])

fwrite(field_info, paste0(data_dir, 'field_info.csv'))

