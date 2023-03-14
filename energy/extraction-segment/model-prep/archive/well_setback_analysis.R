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
library(rgdal)
library(gdalUtilities)
# read data in 

layers <- sf::st_layers(dsn = file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"))

sr <- sf::st_read(dsn = file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/FracTracker/FracTrackerSetbackgdb-newest/FracTrackerSetbackgdb/FracTrackerSetbackdata.gdb"), layer = "SetbackOutlines_SR_Dwellings_082220")
sr <- sr  %>% st_transform(3488) 
# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
wells <- sf::st_read(file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells/Wells_All.shp")) %>% st_transform(3488)

boundaries <- st_read(file.path(sandy, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)

# read in Tracey's input 

tracey_wells <- read_csv(file.path(sandy, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_test.csv"))

## explore data

# it seems like there is a kind of "MULTISURFACE" geom type in there that might be hard to deal with 

geom_type <- map(1:nrow(sr),~(st_geometry_type(sr[.x,]))) %>% unlist()

# 1 MULTISURFACE and 3807 MULTIPOLYGON
table(geom_type)
object.size(sr[1,])

# try to convert the MULTISURFACE TO MULTIPOLYGON
# do this later
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

## Try it on your data
sr_1 <- ensure_multipolygons(sr[1,])
sr[1,] <- sr_1

quartz()
plot(st_geometry(sr_1))
# filtering out a small subset of data to test
# (1,000ft, 2,500ft, 1 mile, and 10,000ft)

d = 304.8 #1000ft

# test on subset geom # 1000-1005
sub <- st_geometry(sr[995:1005,])

# expand the bbox to see more
b <- st_bbox(sub) 
b$xmin <- b$xmin %>% as.double()-500
b$xmax <- b$xmax+500
b$ymin <- b$ymin-500
b$ymax <- b$ymax+500

b <- b %>% unlist() %>% sf::st_bbox(crs = st_crs(3488)) %>% 
  sf::st_as_sfc(.)

id_wells <- sf::st_within(wells, b)
sub_wells <- wells[which(lengths(id_wells) != 0), ]

####################### create test function on a subset of data first ####################

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

buf <- st_buffer(st_geometry(sub), dist = d)
plot(st_geometry(sub_wells), axes = TRUE)
plot(st_geometry(sub), add = TRUE)
plot(buf, border = 'red', add = TRUE)

test <- gen_within_vars(sub, sub_wells, ft = c(1000, 2500))

ggplot(data = sub) +
  geom_sf() +
  geom_sf(data = buf, fill = NA, color = "red") +
  geom_sf_label(data = test, aes(label = API), 
                fill = NA, size = 2, nudge_y = 100) +
  geom_sf(data = test, aes(color = factor(within1000)), fill = NA, size = 2) +
  theme_bw()

####################### field analysis ####################

d = 2000

# test on subset geom # 1000-1005
sub <- sr[-1,]

# approach for this differently, do for each field
# for field i
i=50
plot(st_geometry(boundaries[i,]), axes = TRUE)

# expand the bbox to see more
b <- st_bbox(boundaries[i,]) 
b$xmin <- b$xmin %>% as.double()-1000
b$xmax <- b$xmax+1000
b$ymin <- b$ymin-1000
b$ymax <- b$ymax+1000

b <- b %>% unlist() %>% sf::st_bbox(crs = st_crs(3488)) %>% 
  sf::st_as_sfc(.)

id_sr <- sf::st_intersects(sub, b, sparse = TRUE)
sub_sr <- sr[which(lengths(id_sr) != 0), ]


buf <- st_buffer(st_geometry(sub_sr), dist = d) %>% st_union()
int <- as_tibble(st_intersection(buf, boundaries[i,]))
st_area(int$geometry)
st_area(boundaries[i,])
plot(buf, border = 'red', )
plot(st_geometry(boundaries[i,]), axes = TRUE, add = TRUE, col = "gray")
plot(int, add=TRUE, border = "green", col =" green")
plot(st_geometry(sub_sr), add = TRUE)
plot(buf, border = 'red', add = TRUE)

# ok this works

# use a for loop instead of function
sr <- sr[-1,]
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

chk <- gen_field_setback_coverage(100, sr, ft = c(1000, 2500, 5280, 10000))
