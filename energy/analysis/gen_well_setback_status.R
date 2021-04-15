##################################################################
# This script generates the well attributes within_Xft of SR for X = {1,000ft, 2,500ft, 1 mille, and 10,000ft}
# Sandy Sum
# sandysum@ucsb.edu
# written: 4/9/2021 | modified: 4/12/2021
##################################################################

# comment out and add your own machine's file path
home <- "/Volumes/GoogleDrive/Shared drives"

# load packages
library(sf)
library(tidyverse)
library(purrr)
library(rgdal)
library(maps)

################################# READ DATA AND TRANSFORM

# to get the names of layers in the shapefile
layers <- sf::st_layers(dsn = file.path(home, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"))

sr <- sf::st_read(dsn = file.path(home, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"), layer = "SetbackOutlines_SR_Dwellings_082220")
sr <- sr  %>% st_transform(3488) 

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters

# transform to NAD83(NSRS2007) / California Albers as well for wells and field boundaries
wells <- sf::st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells/Wells_All.shp")) %>% 
  st_transform(3488) %>%
  dplyr::select(API, WellStatus) %>%
  unique()

boundaries <- st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)

################################# DEFINE FUNCTIONS 

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

# convert the multisurface to multipolygon

sr_1 <- ensure_multipolygons(sr[1,])
sr[1,] <- sr_1

# drop the MULTISURFACE as it seems to be the union of all the other MULTIPOLYGONS in the rest of the file 
# see Sandy's notes in https://docs.google.com/document/d/1j_5GoAH2Mpqon4sHqZt29Qh31yAP5kKgCgGddfOQ4Nw/edit#

################################# GENERATE WELL SETBACK SCENARIO ATTRIBUTES (IN OR OUT FOR d = c(1000, 2500, 5280, 10000))
sr <- sr[-1,]

out <- gen_within_vars(sr, wells, ft = c(1000, 2500, 5280, 10000))

out_df <- out %>%
  as_tibble() %>%
  dplyr::select(
    api_ten_digit = API,
    well_status = WellStatus,
    setback_2500ft = within2500,
    setback_5280ft = within5280,
    setback_1000ft = within1000,
    setback_10000ft = within10000
  ) %>% 
  gather("setback_scenario", "in_setback", -c(api_ten_digit, well_status)) %>% 
  mutate(in_setback = as.numeric(in_setback))

# check number in setback compared to tracey's output

out_df %>% group_by(setback_scenario) %>% 
  summarize(n_in = sum(in_setback))

tracey_wells <- read_csv(file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_test.csv")) %>%
  mutate(api_ten_digit = as.character(paste0("0", api_ten_digit))) %>%
  rename(in_setback_orig = in_setback) %>%
  filter(setback_scenario != "no_setback")


## get wells that produce oil
well_prod <- read_rds(paste0(home, "/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds")) %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  dplyr::select(api_ten_digit, OilorCondensateProduced) %>%
  group_by(api_ten_digit) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  filter(prod > 0)

compare_df <- full_join(out_df, tracey_wells) %>%
  filter(api_ten_digit %in% well_prod$api_ten_digit)

tracey_wells %>% group_by(setback_scenario) %>% 
  summarize(n_in = sum(in_setback))

# save output
write_csv(out_df, file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_test_R.csv"))

## make plot
## map

ca <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california")

## buffers
buf1000 <- st_buffer(st_geometry(sr), dist = 1000 * 0.3048)
buf10000 <- st_buffer(st_geometry(sr), dist = 10000 * 0.3048)  


map_figure <- ggplot(data = ca) +
  geom_sf() +
  geom_sf(data = out, aes(color = within10000), size = 0.2, alpha = 0.4) +
  geom_sf(data = buf10000, color = "black", fill = NA) +
  # scale_fill_viridis_c(option = "plasma") +
  # # scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  # facet_wrap(~ year) +
  theme_minimal() +
  labs(
    x = "",
    y = "") +
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_line(colour = "white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size=15),
    legend.direction = "vertical"
  ) 





################################# GENERATE FIELD COVERAGE ATTRIBUTES (IN OR OUT FOR d = c(1000, 2500, 5280, 10000))

field_out <- map(1:nrow(boundaries), gen_field_setback_coverage, sr=sr, ft = c(1000, 2500, 5280, 10000))
field_df <- field_out %>% purrr::map(~(.x %>% mutate_at(vars(contains('percent')), as.double))) %>% 
  bind_rows()

# read in tracey's output

tracey_coverage <- read_csv(file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/setback_coverage.csv"))

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
