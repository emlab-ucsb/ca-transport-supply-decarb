## Tracey Mangin
## May 7, 2021
## prep FrackTracker data for analysis

# comment out and add your own machine's file path
home <- "/Volumes/GoogleDrive/Shared drives"
ft_path <- "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"

# load packages
library(sf)
library(tidyverse)
library(purrr)
library(rgdal)
library(gdalUtilities)
library(maps)
library(mapview)

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## quick CA
ca <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california")

################################# READ DATA AND TRANSFORM

# to get the names of layers in the shapefile
layers <- sf::st_layers(dsn = file.path(home, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"))

## read in the SR layers
layer_vec <- c("SetbackOutlines_SR_Dwellings_082220", "PlaygroundsinCities", "DayCareCenters", "reselderlyCare",
               "CHHS_adultdayhealthcare_csv_Events", "CHHS_altbirthing_csv_Events", "CHHS_Dialysis_csv_Events", 
               "CHHS_healthcare_facility_locations_csv_Events", "CHHS_intermedcarefac_csv_Events", 
               "CHHS_PrimaryCareClinic_csv_Events", "CHHS_psychclinics_csv_Events",
               "CHHS_rehabclinic_csv_Events", "CHHS_skillednursingfacs_csv_Events", "CHHS_surgicalclinic_csv_Events",
               "CHHS_acutecarehospital_csv_Events_1", "CAAcuteCAreHostpitalslatlon_1", "PrivSchoolsCA_1", "SchoolPropCA_1",
               "SchoolsCA_Sabins_1")


## dwellings
sr_dwellings <- sf::st_read(dsn = file.path(home, ft_path), layer = "SetbackOutlines_SR_Dwellings_082220")
sr_dwellings <- sr_dwellings  %>% st_transform(ca_crs) 
sr_dwellings <- sf::st_cast(sr_dwellings, "MULTIPOLYGON")
sr_dwellings <- st_union(sr_dwellings)

## playgrounds
sr_pg <- sf::st_read(dsn = file.path(home, ft_path), layer = "PlaygroundsinCities") %>%
  st_transform(ca_crs) %>%
  dplyr::select(fac_type = FAC_TYPE)

## day care centers
sr_dc <- sf::st_read(dsn = file.path(home, ft_path), layer = "DayCareCenters") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "child day care center") %>%
  dplyr::select(fac_type)

## res elderly care
sr_ec <- sf::st_read(dsn = file.path(home, ft_path), layer = "reselderlyCare") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "retirement community") %>%
  dplyr::select(fac_type)

## chhs adult day health care
sr_adhc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_adultdayhealthcare_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "adult day health care") %>%
  dplyr::select(fac_type)

## alt birthing
sr_ab <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_altbirthing_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "alternative birthing center") %>%
  dplyr::select(fac_type)

## dialysis
sr_d <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_Dialysis_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "dialysis clinic") %>%
  dplyr::select(fac_type)

## health care
sr_hc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_healthcare_facility_locations_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "skilled nursing facility") %>%
  dplyr::select(fac_type)


## health care
sr_pcc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_PrimaryCareClinic_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "primary care clinic") %>%
  dplyr::select(fac_type)


## med care
sr_imc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_intermedcarefac_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "hospice") %>%
  dplyr::select(fac_type)

## psych clinics
sr_pc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_psychclinics_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "psychology clinic") %>%
  dplyr::select(fac_type)

## rehab clinics
sr_rc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_rehabclinic_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "rehab") %>%
  dplyr::select(fac_type)

## skilled nursing facs
sr_snf <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_skillednursingfacs_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "skilled nursing facility") %>%
  dplyr::select(fac_type)

## surgical clinic
sr_sc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_surgicalclinic_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "surgical clinic") %>%
  dplyr::select(fac_type)

## acute care hospital
sr_ach <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_acutecarehospital_csv_Events_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "acute care hospital") %>%
  dplyr::select(fac_type)

## ca acute care hospital
sr_caach <- sf::st_read(dsn = file.path(home, ft_path), layer = "CAAcuteCAreHostpitalslatlon_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "general acute care hospital") %>%
  dplyr::select(fac_type)

## private schools
sr_ps <- sf::st_read(dsn = file.path(home, ft_path), layer = "PrivSchoolsCA_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "private school") %>%
  dplyr::select(fac_type)

## schools (polygons)
sr_s <- sf::st_read(dsn = file.path(home, ft_path), layer = "SchoolPropCA_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "school") %>%
  dplyr::select(fac_type) 

sr_s <- sf::st_cast(sr_s, "MULTIPOLYGON")
sr_s <- st_union(sr_s)

## SchoolsCA_Sabins_1
sr_sca <- sf::st_read(dsn = file.path(home, ft_path), layer = "SchoolsCA_Sabins_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "school") %>%
  dplyr::select(fac_type)

## combine points, union
sr_pts <- rbind(sr_pg,
                sr_dc,
                sr_ec,
                sr_adhc,
                sr_ab,
                sr_d,
                sr_hc,
                sr_imc,
                sr_pc,
                sr_rc,
                sr_snf,
                sr_sc,
                sr_ach,
                sr_caach,
                sr_ps,
                sr_sca,
                sr_pcc)

sr_pts <- st_union(sr_pts)

## simplify dwellings
simp_sr_dwell <- rmapshaper::ms_simplify(sr_dwellings, keep = 0.3, keep_shapes = TRUE, explode = TRUE)

## mapview
mapview(simp_sr_dwell, layer.name = "dwellings") 


## create an sf object for each buffer
## ----------------------------------------

buffer_dist_ft <- c(1000, 2500, 5280)
ft_meter_val <- 0.3048

buff_dist_m <- buffer_dist_ft * ft_meter_val


create_buffer <- function(dist_m) {
  
  pt_buff_tmp <- sr_pts %>%
    st_buffer(dist = dist_m) %>%
    st_union() 
  
  schl_buff_tmp <- sr_s %>%
    st_buffer(dist = dist_m) %>%
    st_union()
  
  dwelling_buff_tmp <- simp_sr_dwell %>%
    st_buffer(dist = dist_m) %>%
    st_union()
  
  out_tmp1 <- st_union(pt_buff_tmp, schl_buff_tmp)
  
  out_tmp2 <- st_union(dwelling_buff_tmp, out_tmp1)
  
}




