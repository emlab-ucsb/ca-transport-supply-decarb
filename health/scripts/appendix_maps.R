#Danae Hernandez-Cortes hernandezcortes@ucsb.edu
#INFRASTRUCTURE TO SEND TO THE MODELING TEAM
#Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(maptools)
library(raster)
library(rgeos)
library(plyr)
library(reshape2)
library(purrr)
library(foreign)
library(haven)
library(readr)
library(dplyr)
rm(list=ls())


#DANAE'S MACHINE
outputFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/academic_output"
sourceFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix"
inmapExFiles  <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix/inmap_processed_srm/extraction"
inmapReFiles  <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix/inmap_processed_srm/refining"



CA<-st_read("H:/CALEPA/GIS/state/california2016.shp")

CA_counties<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/CA_counties/CA_Counties/CA_Counties_TIGER2016.shp")

extraction_clusters<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/fields/academic_paper/extraction_fields_clusters_10km.shp")


g<-ggplot (data = CA_counties)  +
  geom_sf(data = extraction_clusters, fill="darkslategray3", color="darkslategray3") + theme_void() +
  geom_sf(data = CA_counties, fill="transparent", color="gray75")
plot(g)

refinery_data <- read.csv(file = "D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/raw/refineries_ceidars_match_mod.csv")
ref1 <- st_as_sf(refinery_data,coords = c("lon_refinery","lat_refinery"), crs=4269, remove=F) 

additional_data <-read.csv(file = "D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer//CALEPA/raw/lat_long_refineries_decarb.csv")
ref2 <- st_as_sf(additional_data,coords = c("lon","lat"), crs=4269, remove=F) 

j<-ggplot (data = CA_counties)  +
  geom_sf(data = ref1, color="darkslategray3",size=1) + theme_void() +
  geom_sf(data = ref2, color="darkslategray3",size=1) +
  geom_sf(data = CA_counties, fill="transparent", color="gray75")
plot(j)