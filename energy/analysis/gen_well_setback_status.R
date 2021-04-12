##################################################################
# This script generates the well attributes within_Xft of SR for X = {1,000ft, 2,500ft, 1 mille, and 10,000ft}
# Sandy Sum
# sandysum@ucsb.edu
# written: 4/9/2021 | modified
##################################################################

# comment out and add your own machine's file path
home <- "/Volumes/GoogleDrive/Shared drives"

# load packages
library(sf)
library(tidyverse)
library(purrr)
library(rgdal)

# read data in 

layers <- sf::st_layers(dsn = file.path(home, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"))

sr <- sf::st_read(dsn = file.path(home, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"), layer = "SetbackOutlines_SR_Dwellings_082220")
sr <- sr  %>% st_transform(3488) 
# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
wells <- sf::st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells/Wells_All.shp")) %>% st_transform(3488)

boundaries <- st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>%  st_transform(3488)

######### Define function
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

gen_within_vars <- function(sr_df, wells_df, ft) {
  for (i in 1:length(ft)){
    # browser()
    d = ft[i]
    m = d*0.3048
    buf <- st_buffer(st_geometry(sr_df), dist = m)
    wells_df <- wells_df %>% 
      mutate(!!(paste0("within", d)) := (sf::st_within(wells_df, buf) %>% lengths() > 0 %>% as.numeric()))
  }
  wells_df
}

# convert the multisurface to multipolygon

sr_1 <- ensure_multipolygons(sr[1,])
sr[1,] <- sr_1

# drop the MULTISURFACE
sr <- sr[-1,]

out <- gen_within_vars(sr, wells, ft = c(1000, 2500, 5280, 10000))

out_df <- out %>%
  as_tibble() %>%
  dplyr::select(
    api_ten_digit = API,
    setback_2500ft = within2500,
    setback_5280ft = within5280,
    setback_1000ft = within1000,
    setback_10000ft = within10000
  ) %>% 
  gather("setback_scenario", "in_setback", -api_ten_digit) %>% 
  mutate(in_setback = as.numeric(in_setback))

# check number in setback compared to tracey's output

out_df %>% group_by(setback_scenario) %>% 
  summarize(n_in = sum(in_setback))

# save output
write_csv(out_df, file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_test_R.csv"))
# setback_scenario   n_in
# <chr>             <dbl>
#   1 setback_10000ft  138783
# 2 setback_1000ft    19529
# 3 setback_2500ft    41446
# 4 setback_5280ft    82256

tracey_wells <- read_csv(file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_test.csv"))

tracey_wells %>% group_by(setback_scenario) %>% 
  summarize(n_in = sum(in_setback))

# # A tibble: 4 x 2
# setback_scenario  n_in
# <chr>            <dbl>
#   1 no_setback           0
# 2 setback_1000ft    8055
# 3 setback_2500ft   16806
# 4 setback_5280ft   35274

##################### to find buffer #####################

gen_field_setback_coverage <- function(field_index, sr, ft = c(1000, 2500, 5280, 10000)) {
  field <- boundaries[field_index,]
  for (i in 1:length(ft)){
    # browser()
    d = ft[i]
    m = d*0.3048
    buf <- st_buffer(st_geometry(sr), dist = m) %>% st_union()
    int <- as_tibble(st_intersection(buf, field))
    if (nrow(int)==0){
      area = 0
    } else {
      area = st_area(int$geometry)/st_area(field)
    }
    field <- field %>% 
      mutate(!!(paste0("percent_within_", d)) := area)
  }
  field %>% as_tibble()
}

field_out <- map(1:nrow(boundaries), gen_field_setback_coverage, sr=sr, ft = c(1000, 2500, 5280, 10000))
field_df <- field_out %>% purrr::map(~(.x %>% mutate_at(vars(contains('percent')), as.double))) %>% 
  bind_rows()

# read in tracey's output

tracey_coverage <- read_csv(file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/setback_coverage.csv"))

tracey_wells %>% group_by(setback_scenario) %>% 
  summarize(n_in = sum(in_setback))

field_df <- field_df %>%
  as_tibble() %>%
  dplyr::select(
    FieldCode = FIELD_CODE,
    setback_2500ft = percent_within_2500,
    setback_5280ft = percent_within_5280,
    setback_1000ft = percent_within_1000,
    setback_10000ft = percent_within_10000
  ) %>% 
  gather("setback_scenario", "percent_coverage", -FieldCode)

# save output

write_csv(field_df, file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/setback_coverage_R.csv"))
