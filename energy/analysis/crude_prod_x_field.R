## Tracey Mangin
## June 9, 2020
## well vintage and productin

library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(readxl)
library(openxlsx)
library(data.table)

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

prod_file    <- "well_prod_m_processed.csv"

## monthly well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))
## prod by field
field_prod <- well_prod[, .(total_bbls = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code, doc_fieldname, year)]

field_prod2 <- expand.grid(doc_field_code = unique(field_prod$doc_field_code),
                           year = unique(field_prod$year)) %>%
  left_join(field_prod) %>%
  mutate(total_bbls = ifelse(is.na(total_bbls), 0, total_bbls)) %>%
  arrange(doc_field_code, year) %>%
  select(doc_field_code, doc_fieldname, year, total_bbls)
  

write_csv(field_prod2, path = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/crude_prod_x_field_revised.csv")




