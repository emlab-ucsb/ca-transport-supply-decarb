#Danae Hernandez-Cortes hernandezcortes@ucsb.edu
#EXTRACTION MAP
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
library(maps)
rm(list=ls())

ca_crs <- 3488

#DANAE'S MACHINE
outputFiles <- "C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/data/academic_output"
sourceFiles <- "C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix"
inmapExFiles  <- "C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix/inmap_processed_srm/extraction"
inmapReFiles  <- "C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix/inmap_processed_srm/refining"
#EMLAB'S DRIVE
#outputFiles <- "emlab/projects/current-projects/calepa-cn/outputs/academic-out"
#sourceFiles <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix"
#inmapExFiles  <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/extraction"
#inmapReFiles  <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/refining"

#############################################
# PREPARE FILES FROM INMAP OUTPUT: EXTRACTION
#############################################

#Census tracts
CA_ct<-st_read("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/data/census_tracts/census-tract/tl_2019_06_tract.shp")
 
#EXTRACTION MAP CLUSTER 1
sites_vector <- c(1)

read_extraction <- function(buff_site){
  
  bsite <- buff_site
  
  nh3<-read_csv(paste0(inmapExFiles,"/nh3/srm_nh3_field",bsite,".csv",sep=""))%>%mutate(poll="nh3")
  nox<-read_csv(paste0(inmapExFiles,"/nox/srm_nox_field",bsite,".csv",sep=""))%>%mutate(poll="nox")
  pm25<-read_csv(paste0(inmapExFiles,"/pm25/srm_pm25_field",bsite,".csv",sep=""))%>%mutate(poll="pm25")
  sox<-read_csv(paste0(inmapExFiles,"/sox/srm_sox_field",bsite,".csv",sep=""))%>%mutate(poll="sox")
  voc<-read_csv(paste0(inmapExFiles,"/voc/srm_voc_field",bsite,".csv",sep=""))%>%mutate(poll="voc")
  
  all_polls<-rbind(nh3,nox,pm25,sox,voc)
  
  all_polls$site=bsite
  
  tmp<-as.data.frame(all_polls) 
  
  return(tmp)
  
}
#DO THE FUNCTION
extraction_srm <-map_df(sites_vector, read_extraction) %>% bind_rows()
extraction_srm <-dplyr::rename(extraction_srm,weighted_totalpm25=totalpm25_aw)

extraction_srm_reshape<-dcast(extraction_srm,site+GEOID~poll,value.var="weighted_totalpm25")
srm_all_pollutants_extraction<-dplyr::rename(extraction_srm_reshape,weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,site_id=site)

srm_all_pollutants_extraction$total_pm25=srm_all_pollutants_extraction$weighted_totalpm25nh3+srm_all_pollutants_extraction$weighted_totalpm25nox+srm_all_pollutants_extraction$weighted_totalpm25pm25+srm_all_pollutants_extraction$weighted_totalpm25sox+srm_all_pollutants_extraction$weighted_totalpm25voc

map<-left_join(CA_ct,srm_all_pollutants_extraction,by=c("GEOID"))

##DACS

dac_population <- read.csv("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/raw/ces3results_part.csv", stringsAsFactors = FALSE)%>%
  subset(sb535_dac=="Yes")%>%
  dplyr::rename(GEOID=census_tract)

CA_ct$GEOID=as.double(CA_ct$GEOID)

dac_map<-left_join(CA_ct,dac_population, by=c("GEOID"))
dac_map<-dac_map%>%dplyr::filter(sb535_dac=="Yes" & COUNTYFP=="037")
CA_counties<-st_read("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/CA_counties/CA_Counties_TIGER2016_noislands.shp")
LAcontour<-CA_counties%>%subset(COUNTYFP=='037')
SFcontour<-CA_counties%>%subset(COUNTYFP=='013'|COUNTYFP=='001'|COUNTYFP=="081"| COUNTY=="075")

LA_contour_cropped<-st_crop(LAcontour, xmin=-119.4, xmax=117.46, ymin=32, ymax=34.84)


extraction_clusters<-st_read("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/fields/academic_paper/extraction_fields_clusters_10km.shp")
field1<-extraction_clusters%>%dplyr::filter(OBJECTID==1)
torrance<-st_read("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/buffers/torrance_refinery.shp")

LA_contour_cropped<-st_crop(LAcontour, xmin=-118.9, xmax=-118.46, ymin=32, ymax=34.84)

LA_metro<-st_read("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/CALEPA_bren_computer/CALEPA/GIS/OnlyLA_Urban_Areas/U.S._Census_Urbanized_Areas_%E2%80%93_SCAG_Region.shp")
LA_metro<-LA_metro%>%dplyr::filter(NAME10=="Los Angeles--Long Beach--Anaheim, CA")

field1wgs84<-st_transform(field1,4326)

field1_intersection<-st_intersection(LA_metro, field1wgs84)




LA <- map%>%subset(COUNTYFP=='037')%>%dplyr::filter(AWATER<ALAND)
LA1<-st_transform(LA,4326)
LA2<-st_intersection(LA_metro,LA1)

map2<-st_transform(map,ca_crs)
all_SC<-st_intersection(california,map2)


total_pm25<-ggplot (data = LA) +
  geom_sf(data = LA2, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")")))  +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  #geom_sf(data = LA_contour_cropped, fill="transparent", color="gray65") +
  geom_sf(data = LA_metro, fill="transparent", color="gray65")+
  geom_sf(data = field1_intersection, fill="transparent", color="black") 
pdf("C:/Users/dhern125/Documents/GitHub/ca-transport-supply-decarb/health/outputs/LA_field_cluster_1.pdf")
print(total_pm25)
dev.off() 
plot(total_pm25)

total_pm25<-ggplot (data = LA) +
  geom_sf(data = LA, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")"))) +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  #geom_sf(data = LAcontour, fill="transparent", color="gray65") +
  #geom_sf(data = LA_contour_cropped, fill="transparent", color="gray65") +
  geom_sf(data = LA_metro, fill="transparent", color="gray65")+
  geom_sf(data = dac_map, fill="transparent", color="gray65",   size = 0.005)+
  geom_sf(data = field1_intersection, fill="transparent", color="black") 
pdf("C:/Users/dhern125/Documents/GitHub/ca-transport-supply-decarb/health/outputs/LA_field_cluster_1pop.pdf")
print(total_pm25)
dev.off() 
plot(total_pm25)




total_pm25<-ggplot (data = LA) +
  geom_sf(data = LA2, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")"))) +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  #geom_sf(data = LAcontour, fill="transparent", color="gray65") +
  geom_sf(data = LA_metro, fill="transparent", color="gray65")+
  geom_sf(data = dac_map, fill="transparent", color="gray65",   size = 0.005)+
  geom_sf(data = field1_intersection, fill="transparent", color="black") 
pdf("C:/Users/dhern125/Documents/GitHub/ca-transport-supply-decarb/health/outputs/LA_field_cluster_1pop.pdf")
print(total_pm25)
dev.off() 
plot(total_pm25)



#FORMAT AS TRACEY

## califonia
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(4269)

## counties boundaries
county_boundaries <- st_read("C:/Users/dhern125/Dropbox/UCSB-PhD/emLab/CALEPA/data/CA_Counties/CA_Counties/CA_Counties_TIGER2016.shp") %>% 
  st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)



## crop area
disp_win2_wgs84 <- st_sfc(st_point(c(-123, 33)), st_point(c(-115, 39)),
                          crs = 4326)

disp_win2_trans <- st_transform(disp_win2_wgs84, crs = ca_crs)

disp_win2_coord <- st_coordinates(disp_win2_trans)

mapRangeLim<-c(-123,-115,33,39)
map2<-st_transform(map,ca_crs)
field1_intersection2<-st_transform(field1_intersection,ca_crs)

mapRangeLim<-c(-114,-123,32.5,39)

total_pm25<-ggplot () +
  geom_sf(data = map, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")"))) +
  scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", 
                      limits = c(min(map$total_pm25), max(map$total_pm25))) +
  geom_sf(data = field1_intersection, fill="transparent", color="black") +
  geom_sf(data = california, fill="transparent", color="gray65")+
  coord_sf(xlim = mapRangeLim[c(1:2)], ylim = mapRangeLim[c(3:4)])+
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.10, 0.15),
    legend.title = element_text(size = 9)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))
pdf("C:/Users/dhern125/Documents/GitHub/ca-transport-supply-decarb/health/outputs/LA_field_cluster_1pop.pdf")
print(total_pm25)
dev.off() 
plot(total_pm25)



