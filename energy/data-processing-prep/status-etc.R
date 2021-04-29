# ------------------------------------------- INPUTS -----------------------------------
data_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
raw_dir            <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/"
rystad_path        <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/"
sp_dir             <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/AllWells_gis/"
save_directory     <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/exit/"


library(sf)
library(tidyverse)
library(data.table)
library(plotly)
library(readxl)
library(zoo)


prod_file    <- "well_prod_m_processed.csv"
inj_file     <- "well_inj_m_processed.csv"
well_file    <- "AllWells_table/AllWells_20210427.csv"
well_shp     <- "Wells_All.shp"
field_b_file <- "DOGGR_Admin_Boundaries_Master.shp"
ws_permits_file <- "Historic Permit WellStar.xlsx"
cw_permits_file <- "historic_permit_calwims.csv"


## permits
permits_ws <- read_xlsx(paste0(raw_dir, ws_permits_file))

permits_cw <- fread(paste0(data_directory, cw_permits_file))

## 
well_df <- fread(paste0(raw_dir, well_file))

well_shp_df <- st_read(dsn = file.path(sp_dir, well_shp))

## monthly well injection
well_inj <- fread(paste0(data_directory, inj_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## monthly well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

field_prod <- well_prod[, .(prod = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_fieldname, doc_field_code, county_name, year)]


field_prod[, offshore := str_detect(doc_fieldname, "Offshore")][, offshore := ifelse(offshore == TRUE, 1, 0)]

offshore_df <- field_prod[offshore == 1]

View(offshore_df %>% group_by(doc_field_code, doc_fieldname, year) %>% mutate(n = n()) %>% ungroup())

## field boundaries
fields_loc <- st_read(paste0(sp_dir, field_b_file)) %>%
  st_transform(CRS("+init=epsg:3310"))


ggplot(offshore_df %>% filter(county_name == "Santa Barbara Offshore"), aes(x = year, y = prod, color = doc_fieldname)) +
  geom_line()

### well status
## -------------------

## find wells that produce at some point over time period
pos_well_api_prod <- well_prod %>%
  group_by(api_ten_digit) %>%
  summarise(oil_total = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(pos_pro = ifelse(oil_total > 0, 1, 0)) %>%
  filter(pos_pro == 1)

pos_vec <- unique(pos_well_api_prod$api_ten_digit)

prod_well_status <- well_prod[api_ten_digit %chin% pos_vec]

## status
status_df <- well_df %>%
  select(api_ten_digit = API, status = WellStatus, spud_date = SpudDate) %>%
  mutate(api_ten_digit = paste0("0", api_ten_digit))

# status_shp <- well_shp_df %>%
#   select(api_ten_digit = API, status_shp = WellStatus)
# 
# st_geometry(status_shp) <- NULL

well_info1 <- prod_well_status[, c("api_ten_digit", "month_year", "OilorCondensateProduced")]

well_inj[, injecting := fifelse(GasAirInjected > 0 | SteamWaterInjected > 0 |
                                 DaysInjecting > 0, 1, 0)]

inj_info <- well_inj[, c("api_ten_digit", "month_year", "injecting")]

well_info2 <- full_join(well_info1, inj_info)


## merge
well_status_df <- well_info2 %>%
  left_join(status_df) %>%
  rename(prod = OilorCondensateProduced) %>%
  mutate(month_year = as.Date(month_year),
         spud_date = as.Date(spud_date, format="%m/%d/%Y"),
         spud_month = month(spud_date),
         spud_year = year(spud_date),
         spud_my = as.Date(as.yearmon(paste(spud_year, spud_month, sep = "-"))),
         injecting = ifelse(is.na(injecting), 0, injecting),
         prod = ifelse(is.na(prod), 0, prod),
         activity = ifelse(prod > 0 | injecting == 1, 1, 0),
         activity_before_spud = ifelse(activity == 1 & month_year < spud_my, 1, 0))

## how many wells have production before spud date?
activity_before_spud_df <- well_status_df %>%
  filter(activity_before_spud == 1) %>%
  select(api_ten_digit) %>%
  unique()

## min
min_df <- well_status_df %>% 
  filter(activity_before_spud == 1) %>%
  group_by(api_ten_digit) %>%
  slice(which.min(spud_my)) %>%
  ungroup()
  


# left_join(status_shp) %>%
  # mutate(same_status = ifelse(status_shp == status, 1, 0)) %>%
  filter(status %in% c("Plugged", "PluggedOnly")) %>%
  group_by(api_ten_digit, year, status) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()


test <- ggplot(well_status_df, aes(x = year, y = prod, color = status, group = api_ten_digit)) +
  geom_line(size = 0.1, alpha = 0.2)

ggplotly(test)

## add permit data
permits_ws2 <- janitor::clean_names(permits_ws) %>%
  select(api_ten_digit = well_api_number, permit_well_status = well_status, application_status, notice_type, notice_received_date, notice_approval_date)

## merge
status_permit_df <- well_status_df %>%
  left_join(permits_ws2)

fwrite(status_permit_df, paste0(save_directory, 'plugged_prod_permit.csv'), row.names = F)





