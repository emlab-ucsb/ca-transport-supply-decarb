##################################################################
# This script generates the well attributes within_Xft of SR for X = {1,000ft, 2,500ft, 1 mille, and 10,000ft}
# Sandy Sum
# sandysum@ucsb.edu
# written: 4/9/2021 | modified
##################################################################

# comment out and add your own machine's file path
sandy <- "/Volumes/GoogleDrive/Shared drives"

# load packages
library(sf)
library(tidyverse)
library(purrr)

# read data in 

layers <- sf::st_layers(dsn = file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"))

sr <- sf::st_read(dsn = file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb", layer = "SetbackOutlines_SR_Dwellings_082220"))

wells <- sf::st_read(file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells/Wells_All.shp"))

boundaries <- st_read(file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp"))

## explore data

# it seems like there is a kind of "MULTISURFACE" geom type in there that might be hard to deal with 

geom_type <- map(1:nrow(sr),~(st_geometry_type(sr[.x,]))) %>% unlist()

# 1 MULTISURFACE and 3807 MULTIPOLYGON
table(geom_type)
