#######################################################################################
## Author: Anagha Uppal
## Date: Aug 5, 2020
## Purpose: Assign nearest neighbor point (oil assets) to each polygon (oil field) 
## and nearest neighbor polygon to each point.
## Project: CalEPA - estimating production of oil fields
## Packages: rgeos, nngeo and sf
## Inputs: vector (.shp) or CSV
## Outputs: CSV (.csv)
#######################################################################################

library(sf) #read
library(rgeos) #nearest poly to each point
library(nngeo) #nearest point to each poly

#library(rgdal) #shouldn't need these two anymore, but just in case
#library(tidyverse)

# data input
# assetLoc <- read.csv("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/raw/asset_latlon.csv")
## use adjusted version that excludes fields with zero production (Rystad production)
assetLoc <- read.csv("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/asset_latlon_adj.csv")
fieldsLoc <- st_read("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")
assetLocSF <- st_as_sf(assetLoc, coords = c("Longitude", "Latitude"), 
                       crs = st_crs("+init=epsg:4269"))

############################
#Nearest point to each polygon 
fieldsAssets <- st_nn(fieldsLoc, assetLocSF, sparse = TRUE, k = 1,
  maxdist = Inf, returnDist = TRUE, progress = TRUE, parallel = 1)
fieldsAssetsDF <- data.frame(OBJECTID = fieldsLoc$OBJECTID,
                             NAME = fieldsLoc$NAME,
                             FIELD_CODE = fieldsLoc$FIELD_CODE,
                             nn = unlist(fieldsAssets$nn),
                             dist = unlist(fieldsAssets$dist))
assetLoc$ID <- seq.int(nrow(assetLoc))
fieldsAssetsDF <- merge(fieldsAssetsDF, assetLoc, by.x = "nn", by.y = "ID")

##########################
# Nearest polygon to each point
#rgeos needs UTM coords
assetLocSF <- st_transform(assetLocSF, CRS("+proj=utm +datum=NAD83 +no_defs"))
fieldsLoc <- st_transform(fieldsLoc, CRS("+proj=utm +datum=NAD83 +no_defs"))
#rgeos uses sp objects
assetLocSF <- as(assetLocSF, Class = "Spatial")
fieldsLoc <- as(fieldsLoc, Class = "Spatial")

## Set up containers for results
n <- length(assetLocSF)
nearestField <- character(n)
distToNearestField <- numeric(n)

## For each point, find name of nearest polygon
for (i in seq_along(nearestField)) {
  gDists <- gDistance(assetLocSF[i,], fieldsLoc, byid=TRUE)
  nearestField[i] <- fieldsLoc$NAME[which.min(gDists)]
  distToNearestField[i] <- min(gDists)
}
assetsFields <- cbind(assetLoc, nearestField, distToNearestField)

# Output
write.csv(assetsFields, "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/assetsFields_adj.csv")
write.csv(fieldsAssetsDF, "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/Rystad/data/processed/fieldsAssets_adj.csv")
