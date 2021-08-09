## Tracey Mangin
## August 9, 2021
## Refinery locations

## libraries
library(data.table)  
library(tidyverse)

## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
outputs_path <- 'model-development/scenario-plot/refinery-outputs'

## files
refining_file <- 'refining_scenario_outputs_refinery_net_exports_revised.csv'


## refining outputs
refining_out <- fread(file.path(main_path, outputs_path, refining_file))

## site ids 
site_id <- fread(paste0(main_path, "/data/stocks-flows/processed/refinery_loc_cap_manual.csv"), colClasses = c("site_id" = "character")) 

## unique refineries in study
## ---------------------------------
refineries_out <- refining_out %>%
  select(site_id, refinery_name, region, cluster) %>%
  unique()

## add locations
site_locs <- site_id %>%
  select(site_id, county, geometry)

refineries_out <- refineries_out %>%
  left_join(site_locs) %>%
  mutate(county = ifelse(site_id == "342-2", "Contra Costa",
                         ifelse(site_id == "99999", "Kern",
                                ifelse(site_id == "t-800", "Los Angeles", county))))

## manually add lat/longs for refineries without info

ph66_sm <- 'c(35.040050501373045, -120.5896056369663)'
altair <- 'c(33.90000142538856, -118.15028819686157)'
ph66_su <- 'c(-122.255632, 38.0434740000001)' ## same location as 342
glblce <- 'c(35.380165,-119.072434)'

refineries_out <- refineries_out %>%
  mutate(geometry = ifelse(site_id == "3422", ph66_sm,
                           ifelse(site_id == "t-800", altair,
                                  ifelse(site_id == "342-2", ph66_su,
                                         ifelse(site_id == "99999", glblce, geometry)))))

fwrite(refineries_out, file.path(main_path, "/data/stocks-flows/processed/refinery_lat_long.csv"), row.names = F)


