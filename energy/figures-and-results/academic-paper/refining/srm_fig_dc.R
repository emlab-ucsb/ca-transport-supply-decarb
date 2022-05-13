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

#EMLAB'S DRIVE
#outputFiles <- "emlab/projects/current-projects/calepa-cn/outputs/academic-out"
#sourceFiles <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix"
#inmapExFiles  <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/extraction"
#inmapReFiles  <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/refining"

#############################################
# PREPARE FILES FROM INMAP OUTPUT: REFINING
#############################################

#Census tracts
CA_ct<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/census_tracts/census-tract/tl_2019_06_tract.shp")

#Torrance refinery
sites_vector <- c(226)
read_refining <- function(buff_site){
  
  bsite <- buff_site
  
  nh3<-read_csv(paste0(inmapReFiles,"/nh3/srm_nh3_site",bsite,".csv",sep=""))%>%mutate(poll="nh3")
  nox<-read_csv(paste0(inmapReFiles,"/nox/srm_nox_site",bsite,".csv",sep=""))%>%mutate(poll="nox")
  pm25<-read_csv(paste0(inmapReFiles,"/pm25/srm_pm25_site",bsite,".csv",sep=""))%>%mutate(poll="pm25")
  sox<-read_csv(paste0(inmapReFiles,"/sox/srm_sox_site",bsite,".csv",sep=""))%>%mutate(poll="sox")
  voc<-read_csv(paste0(inmapReFiles,"/voc/srm_voc_site",bsite,".csv",sep=""))%>%mutate(poll="voc")
  
  all_polls<-rbind(nh3,nox,pm25,sox,voc)
  
  all_polls$site=bsite
  
  tmp<-as.data.frame(all_polls) 
  
  return(tmp)
  
}

#DO THE FUNCTION
refining_srm <-map_df(sites_vector, read_refining) %>% bind_rows()
refining_srm <-dplyr::rename(refining_srm,weighted_totalpm25=totalpm25_aw)
refining_srm_reshape<-dcast(refining_srm,site+GEOID~poll,value.var="weighted_totalpm25")
srm_all_pollutants_refining<-dplyr::rename(refining_srm_reshape,weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,site_id=site)
srm_all_pollutants_refining$total_pm25=srm_all_pollutants_refining$weighted_totalpm25nh3+srm_all_pollutants_refining$weighted_totalpm25nox+srm_all_pollutants_refining$weighted_totalpm25pm25+srm_all_pollutants_refining$weighted_totalpm25sox+srm_all_pollutants_refining$weighted_totalpm25voc
map<-left_join(CA_ct,srm_all_pollutants_refining,by=c("GEOID"))

##DACS
dac_population <- read.csv("D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/raw/ces3results_part.csv", stringsAsFactors = FALSE)%>%
  subset(sb535_dac=="Yes")%>%
  dplyr::rename(GEOID=census_tract)

CA_ct$GEOID=as.double(CA_ct$GEOID)

dac_map<-left_join(CA_ct,dac_population, by=c("GEOID"))

dac_map<-dac_map%>%dplyr::filter(sb535_dac=="Yes" & COUNTYFP=="037")

CA_counties<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/CA_counties/CA_Counties_TIGER2016_noislands.shp")

LAcontour<-CA_counties%>%subset(COUNTYFP=='037')

SFcontour<-CA_counties%>%subset(COUNTYFP=='013'|COUNTYFP=='001'|COUNTYFP=="081"| COUNTY=="075")

LA_contour_cropped<-st_crop(LAcontour, xmin=-119.4, xmax=117.46, ymin=32, ymax=34.84)

torrance<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/buffers/torrance_refinery.shp")

LA_contour_cropped<-st_crop(LAcontour, xmin=-118.9, xmax=-118.46, ymin=32, ymax=34.84)

LA_metro<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/OnlyLA_Urban_Areas/U.S._Census_Urbanized_Areas_%E2%80%93_SCAG_Region.shp")

LA_metro<-LA_metro%>%dplyr::filter(NAME10=="Los Angeles--Long Beach--Anaheim, CA")

LA <- map%>%subset(COUNTYFP=='037')%>%dplyr::filter(AWATER<ALAND)

total_pm25<-ggplot (data = LA) +
  geom_sf(data = LA, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")")))  +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  #geom_sf(data = LA_contour_cropped, fill="transparent", color="gray65") +
  geom_sf(data = LA_metro, fill="transparent", color="gray65")+
  geom_point(data = torrance, aes(x=lon, y=lat), size = 1, shape = 23, fill="black") 

pdf("C:/Users/Danae/Documents/GitHub/CA_EJ/LA_refining_zoom.pdf")
print(total_pm25)
dev.off() 
plot(total_pm25)

total_pm25<-ggplot (data = LA) +
  geom_sf(data = LA, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")"))) +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  #geom_sf(data = LA_contour_cropped, fill="transparent", color="gray65") +
  geom_sf(data = LA_metro, fill="transparent", color="gray65")+
  geom_sf(data = dac_map, fill="transparent", color="gray65",   size = 0.005)+
  geom_point(data = torrance, aes(x=lon, y=lat), size = 1, shape = 23, fill="black") 

pdf("C:/Users/Danae/Documents/GitHub/CA_EJ/LA_refining_population.pdf")
print(total_pm25)
dev.off() 
plot(total_pm25)

LA$GEOID<-as.double(LA$GEOID)

torrance_dacs<-left_join(LA,dac_population,by="GEOID")%>%dplyr::select("GEOID","total_pm25","sb535_dac")

torrance_dacs<-torrance_dacs%>%dplyr::filter(total_pm25>0.00001)

st_write(torrance_dacs,"C:/Users/Danae/Documents/GitHub/CA_EJ/torrance.csv")
st_distance(willmington,torrance)