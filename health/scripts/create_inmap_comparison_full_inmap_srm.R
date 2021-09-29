#Danae Hernandez-Cortes hernandezcortes@ucsb.edu
#PREPARE INMAP DATA TO COMPARE TO REPORT 
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


productionFiles  <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/academic_output/09_21_2021"
spatialFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS"
outputFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/academic_output"
sourceFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix"

######################REFINING##########################################
#0. Bring production data (provided by the modeling team): refining
production_data <-read_csv(file = "H:/CALEPA/calepa-scenarios/inputs/refining/baseline_site_level_refining.csv")
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


#PM2.5
refinery_production_site <-refinery_production_inmap %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height", "PM2_5")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_pm25_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))

#NH3
refinery_production_site <-refinery_production_inmap %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height","NH3")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_nh3_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))
#VOC
refinery_production_site <-refinery_production_inmap %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height","VOC")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_voc_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))
#NOx
refinery_production_site <-refinery_production_inmap %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height","NOx")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_nox_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))
#SOx
refinery_production_site <-refinery_production_inmap %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height","SOx")
data_inmap_map_refinery <- st_as_sf(refinery_production_site,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_sox_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))


######################ADD ADDITIONAL 4 REFINERIES IN DATASET
additional_data <-read_csv(file = "H:/CALEPA/raw/lat_long_refineries_decarb.csv")

additional_data$PM2_5=1000
additional_data$NH3=1000
additional_data$VOC=1000
additional_data$NOx=1000
additional_data$SOx=1000

#PM2.5
additional_data_inmap <-additional_data %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height", "PM2_5")
data_inmap_map_refinery <- st_as_sf(additional_data_inmap,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_pm25_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))

#NH3
additional_data_inmap <-additional_data %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height", "NH3")
data_inmap_map_refinery <- st_as_sf(additional_data_inmap,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_nh3_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))

#NOx
additional_data_inmap <-additional_data %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height", "NOx")
data_inmap_map_refinery <- st_as_sf(additional_data_inmap,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_nox_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))

#SOx
additional_data_inmap <-additional_data %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height", "SOx")
data_inmap_map_refinery <- st_as_sf(additional_data_inmap,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_sox_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))

#VOCs
additional_data_inmap <-additional_data %>% dplyr::select("site_id","lat", "lon", "diam", "temp", "velocity","height", "VOC")
data_inmap_map_refinery <- st_as_sf(additional_data_inmap,coords = c("lon","lat"), crs=4269, remove=F) 

lapply(unique(data_inmap_map_refinery$site_id), function(x) 
  st_write(data_inmap_map_refinery %>% filter(site_id==x),paste0("H:/CALEPA/calepa-scenarios/outputs/AWS_input/Refining/source_receptor_matrix/new/refining_voc_site_",x,".shp"), driver="ESRI Shapefile", append=FALSE))

######################EXTRACTION##########################################
extraction_field_clusters_10km<-read_csv(paste0(sourceFiles,"/extraction_fields_clusters_10km.csv",sep=""))%>%
  dplyr::select(OUTPUT_FID,INPUT_FID)
extraction_field_clusters_10km<-dplyr::rename(extraction_field_clusters_10km,id=OUTPUT_FID,input_fid=INPUT_FID)

extraction_fields_xwalk<-read.dbf(paste0(sourceFiles,"/extraction_fields_xwalk_id.dbf",sep=""))
extraction_fields_xwalk<-dplyr::rename(extraction_fields_xwalk,input_fid=id,doc_field_code=dc_fld_)

extraction_xwalk<-left_join(extraction_field_clusters_10km,extraction_fields_xwalk,by=c("input_fid"))
extraction_xwalk$doc_field_code=as.numeric(as.character(extraction_xwalk$doc_field_code))

#LOAD EXTRACTION OUTPUTS
extraction_outputs<-read_csv(paste(outputFiles, "/09_21_2021/subset_field_results.csv", sep = ""))
extraction_outputs$doc_field_code<-as.double(extraction_outputs$doc_field_code)

#MERGE EXTRACTION OUTPUTS AT THE FIELD LEVEL AND X-WALK
fields_clusters<-left_join(extraction_outputs, extraction_xwalk,by="doc_field_code")

#COLLAPSE: TOTAL PRODUCTION PER CLUSTER
total_clusters<-fields_clusters%>%
  dplyr::group_by(id, year, scen_id)%>%
  dplyr::summarize(total_prod_bbl=sum(total_prod_bbl))


#MULTIPLY BY EMISSIONS FACTORS
total_clusters$NH3=total_clusters$total_prod_bbl*0.00061
total_clusters$NOx=total_clusters$total_prod_bbl*0.04611
total_clusters$PM2_5=total_clusters$total_prod_bbl*0.00165
total_clusters$SOx=total_clusters$total_prod_bbl*0.01344
total_clusters$VOC=total_clusters$total_prod_bbl*0.02614

total_clusters$scen_id_number <- as.numeric(as.factor(total_clusters$scen_id))
total_clusters_2045<-total_clusters%>%dplyr::filter(year==2045)

extraction_0<-subset(total_clusters_2045,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))
extraction_1<-subset(total_clusters_2045,(scen_id_number==1))
extraction_2<-subset(total_clusters_2045,(scen_id_number==2))
extraction_3<-subset(total_clusters_2045,(scen_id_number==3))
extraction_4<-subset(total_clusters_2045,(scen_id_number==4))

extraction_shape<-st_read(paste0(spatialFiles,"/fields/academic_paper/extraction_fields_clusters_10km.shp",sep=""))
extraction_shape<-extraction_shape%>%dplyr::rename(id=OBJECTID)

extraction_shape0<-left_join(extraction_shape,extraction_0,by=c("id"))%>%dplyr::select("PM2_5", "NH3", "NOx", "VOC", "SOx", "geometry")
st_write(extraction_shape0,"D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/calepa-scenarios/outputs/AWS_input/Extraction/comparison_srm_inmap/ext_scenario_0.shp")

extraction_shape1<-left_join(extraction_shape,extraction_1,by=c("id"))%>%dplyr::select("PM2_5", "NH3", "NOx", "VOC", "SOx", "geometry")
st_write(extraction_shape1,"D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/calepa-scenarios/outputs/AWS_input/Extraction/comparison_srm_inmap/ext_scenario_1.shp")

extraction_shape2<-left_join(extraction_shape,extraction_2,by=c("id"))%>%dplyr::select("PM2_5", "NH3", "NOx", "VOC", "SOx", "geometry")
st_write(extraction_shape2,"D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/calepa-scenarios/outputs/AWS_input/Extraction/comparison_srm_inmap/ext_scenario_2.shp")

extraction_shape3<-left_join(extraction_shape,extraction_3,by=c("id"))%>%dplyr::select("PM2_5", "NH3", "NOx", "VOC", "SOx", "geometry")
st_write(extraction_shape3,"D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/calepa-scenarios/outputs/AWS_input/Extraction/comparison_srm_inmap/ext_scenario_3.shp")

extraction_shape4<-left_join(extraction_shape,extraction_4,by=c("id"))%>%dplyr::select("PM2_5", "NH3", "NOx", "VOC", "SOx", "geometry")
st_write(extraction_shape4,"D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/calepa-scenarios/outputs/AWS_input/Extraction/comparison_srm_inmap/ext_scenario_4.shp")
