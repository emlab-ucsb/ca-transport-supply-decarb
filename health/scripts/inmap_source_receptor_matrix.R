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
library(dplyr)
library(spData)
library(purrr)
rm(list=ls())

#2019


######################REFINING##########################################
#0. Bring production data (provided by the modeling team): refining
production_data <-read.csv(file = "H:/CALEPA/calepa-scenarios/inputs/refining/baseline_site_level_refining.csv")
production_data_2019 <-subset(production_data, (year==2019))
production_data_2019 <-subset(production_data, (type=="consumption"))

refinery_ids<-read.csv(file = "H:/CALEPA/raw/refineries_name_site_id.csv")

production_data_2019<-merge(production_data_2019,refinery_ids,by.x=c("site_id"),by.y=c("site_id"), all.x = TRUE, all.y = TRUE)
stack_info_ceidars <-read.csv(file = "H:/CALEPA/processed/data_stack_ceidars.csv")
#1. Bring refinery information
refinery_data <- read.csv(file = "H:/CALEPA/raw/refineries_ceidars_match_mod.csv")

#2. Merge refinery information
#refinery_info <-merge(refinery_data,production_data_2021, by.x=c("cluster"), by.y=c("cluster"), all.x = FALSE, all.y = TRUE)
#total_barrels_cluster <-refinery_info%>%group_by(cluster)%>%summarize(tot_barrels=sum(barrels_per_day))
#refinery_info <-merge(refinery_info,total_barrels_cluster, by.x=c("cluster"), by.y=c("cluster"), all.x = FALSE, all.y = TRUE)

#refinery_info$weighted_capacity=refinery_info$barrels_per_day/refinery_info$tot_barrels
#refinery_info$production=refinery_info$crude_bbls_refined*refinery_info$weighted_capacity

#4. Merge stack information
refinery_info<-merge(production_data_2019, refinery_data, by.x=c("site_id"),by.y=c("site_id"), all.x = TRUE, all.y=TRUE)

refinery_ceidars<-merge(refinery_data,stack_info_ceidars,by.x=c("ceidars_id"), by.y=c("CEIDARS_ID"))

avg_stack_info<-refinery_ceidars%>%group_by(cluster)%>%summarize(avg_temp=mean(GT,na.rm=TRUE), avg_velocity=mean(GV,na.rm=TRUE), avg_diam=mean(STKDIAM,na.rm=TRUE), avg_height=mean(STKHT,na.rm=TRUE))%>% dplyr::select(avg_temp, avg_velocity, avg_diam, avg_height, cluster)%>% rename(temp = avg_temp, velocity = avg_velocity, diam = avg_diam, height = avg_height)

#5. Assign average technology for missing values
refinery_info <- merge(refinery_info, avg_stack_info, by.x=c("cluster"), by.y=c("cluster"))


refinery_production_inmap<-refinery_info %>% dplyr::select("lat_refinery", "lon_refinery", "value_bbls", "diam", "temp", "velocity","height", "cluster", "site_id", "company")%>%rename(lat=lat_refinery, lon=lon_refinery, production =value_bbls)
refinery_production_inmap <- refinery_production_inmap[!duplicated(refinery_production_inmap$site_id),]
refinery_production_inmap<-refinery_production_inmap%>%filter(production!="NA")

#5.5 Assume that all refineries have the same production = 100,000,000
refinery_production_inmap$PM2_5=1000
refinery_production_inmap$NH3=1000
refinery_production_inmap$VOC=1000
refinery_production_inmap$NOx=1000
refinery_production_inmap$SOx=1000



#7. Create shapefile per refinery LAPPLY HERE

refinery_production_site <-refinery_production_inmap %>% dplyr::select("lat", "lon", "diam", "temp", "velocity","height", "PM2_5", "NH3", "VOC", "NOx", "SOx")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(refinery_production_site$site_id), function(x) 
  st_write(refinery_production_site %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/refining_site_",x,".shp", 
                                                                   driver="ESRI Shapefile", append=FALSE)))
## previous code

refinery_production_site<-refinery_production_inmap %>% dplyr::filter(site_id==343)
refinery_production_site<-refinery_production_site %>% dplyr::select("lat", "lon", "diam", "temp", "velocity","height", "PM2_5", "NH3", "VOC", "NOx", "SOx")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 
st_write(data_inmap_map_refinery, "H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/refining_site_343.shp", driver="ESRI Shapefile", append=FALSE) 



