## Tracey Mangin
## May 7, 2021
## prep FrackTracker data for analysis
##revised : Feb 14, 2024 by Haejin 

# comment out and add your own machine's file path
home <- "/capstone/freshcair/meds-freshcair-capstone"
ft_path <- "/data/inputs/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb" #--- missing file ----
save_path <- paste0(home, "/emlab/projects/current-projects/calepa-cn/data/GIS/processed/fracktracker-sr/")

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
layers <- sf::st_layers(dsn = file.path(home, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb")) #### ----- missing

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

# sandy's checks
# ensure that everything is read in: looks ok 19 SR objects
length(layer_vec)
ls(pattern = "sr") %>% length()

# put everything into a list
all <- lapply(ls(pattern= "sr"), get)

all %>% purrr::map(~( .x %>% summary()))

# sr_r and ar_dwellings are multipolygons

# all are points except for 7 and 16 --> multipolygons

## combine points, union
# everything above except the multipolygons
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

# st_union()'s help file
# Unioning a set of overlapping polygons has the effect of merging the areas (i.e. the same effect as iteratively unioning all individual polygons together). Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions of line segments will be reduced to a single line segment in the output. Unioning a set of Points has the effect of merging all identical points (producing a set with no duplicates).

sr_pts <- st_union(sr_pts)

## simplify dwellings

simp_sr_dwell <- rmapshaper::ms_simplify(sr_dwellings, keep = 0.3, keep_shapes = TRUE, explode = TRUE)
length(st_geometry(simp_sr_dwell))

# sandy's checks
# the reduction in size by 20% could probably go further since look so similar
(object.size(sr_dwellings)-object.size(simp_sr_dwell))/object.size(sr_dwellings)

# plot close up to see what is lost
# looks very similar

# define random bounding box to check
xcheck <- c(200000, 230000)
ycheck <- c(-500000,-480000)

par(mfrow = c(1, 2))

plot(sr_dwellings,
     xlim = xcheck ,
     ylim = ycheck,
     border = 1,
     axes = TRUE)

plot(simp_sr_dwell,
     xlim = xcheck ,
     ylim = ycheck,
     border = 1,
     axes = TRUE)

par(mfrow = c(1, 1))
## mapview
# mapviewOptions(fgb = FALSE) -- if map not rendering, run this
mapview(sr_dwellings, layer.name = "dwellings") 
# sandy: this function is not running for me

## save simplified version to view in QGIS and compare
# st_write(simp_sr_dwell, dsn = paste0(save_path, "simplified_dwellings.shp"))


## create an sf object for each buffer
## ----------------------------------------

buffer_dist_ft <- c(1000, 2500, 5280)
ft_meter_val <- 0.3048

create_buffer <- function(dist_ft) {
  
  buff_dist_ft_name <- paste0(dist_ft, "ft")
  
  dist_m <- dist_ft * ft_meter_val
  
  pt_buff_tmp <- sr_pts %>%
    st_buffer(dist = dist_m) %>%
    st_union() 
  
  # looking good!!!
  plot(pt_buff_tmp, xlim = xcheck, ylim = ycheck)
  plot(sr_pts, xlim = xcheck, ylim = ycheck, add = TRUE, pch = 16, cex = .5)
  
  schl_buff_tmp <- sr_s %>%
    st_buffer(dist = dist_m) %>%
    st_union()
  
  dwelling_buff_tmp <- simp_sr_dwell %>%
    st_buffer(dist = dist_m) %>%
    st_union()
  
  out_tmp1 <- st_union(pt_buff_tmp, schl_buff_tmp)
  
  out_tmp2 <- st_union(dwelling_buff_tmp, out_tmp1)
  
  # uncomment to check
  # plot(out_tmp2, xlim = xcheck, ylim = ycheck)
  # plot(sr_pts, xlim = xcheck, ylim = ycheck, add = TRUE, pch = 16, cex = .5)
  # plot(simp_sr_dwell, xlim = xcheck, ylim = ycheck, add = TRUE, col = "red")
  # plot(sr_s, xlim = xcheck, ylim = ycheck, add = TRUE, col = "blue")
  
  ## save output
  st_write(out_tmp2, dsn = paste0(save_path, paste0("buffer_", buff_dist_ft_name, ".shp")))
  
}

purrr::map(buffer_dist_ft, create_buffer)


