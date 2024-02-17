#Danae Hernandez-Cortes hernandezcortes@ucsb.edu
#vthiviergge@ucsb.edu
#last change: 10/08/2021
# modified : 02/13/2024 - Haejin 

#WEIGHTED POLLUTION EXPOSURE
#Libraries
library("viridis")  
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
library(RColorBrewer)

#EXTRACTION
rm(list=ls())

sourceFiles <- "./data/inputs"


######################VINCENT: PLEASE ADD THE MODIFIED CODE THAT OBTAINS EXTRACTION
ref_data <- deltas_refining%>%
  filter(scen_id %in% "R-BAU" & year %in% c(2019,2045))

ext_data <- deltas_extraction%>%
  filter(scen_id %in% "reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax" & year %in% c(2019,2045))

#CONVERT TO NICE IDS NOT TEXT
extraction_scenarios$id <- as.numeric(as.factor(extraction_scenarios$scen_id))
#OBTAIN BAU
extraction_BAU<-subset(extraction_scenarios,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))
extraction_BAU<-extraction_BAU%>%
  dplyr::rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25, bau_avg_total_pm25=avg_total_pm25)
extraction_BAU_2019<-extraction_BAU%>%dplyr::filter(year==2019)

###################POPULATION##########################
#0. Bring disadvantaged communities data from CES
ces_data <- read_csv(paste0(sourceFiles,"health/ces3results_part.csv",sep=""), stringsAsFactors = FALSE) %>%
  dplyr::select("census_tract","total_population")

#CA<-st_read(paste0(sourceFiles,"GIS/raw/state/california2016.shp",sep="")) # missing data ------

CA_counties<-st_read(paste0(sourceFiles,"gis/CA_Counties/CA_Counties_TIGER2016.shp",sep=""))

CA_ct<-st_read(paste0(sourceFiles,"gis/census-tract/tl_2019_06_tract.shp",sep=""))

#Merge refining

temp_data_ref <- ref_data%>%
  dplyr::select(-prim_pm25:-delta_prim_pm25)%>%
  spread(year, total_pm25)%>%
  mutate(change_pm25 = `2045`-`2019`)

map_ref <- left_join(CA_ct, temp_data_ref,by=c("GEOID"))

#State wide

ggplot(data = map_ref)+
  geom_sf(data = map_ref, aes(fill=change_pm25), color=NA) + theme_void() + labs(fill="PM2.5") +
  scale_fill_gradient(high = "#CCDDE5", low = "#005581", space = "Lab", na.value = "gray50",
                      limits = c(min(map_ref$change_pm25), max(map_ref$change_pm25))) + 
  ggtitle("California, change refining PM2.5 BAU between 2045 and 2019")

#LA county
LAcontour<-CA_counties%>%subset(COUNTYFP=='037'|COUNTYFP=='111'|COUNTYFP=='059')
LA_contour_cropped<-st_crop(LAcontour, xmin=-119.4, xmax=117.46, ymin=33.34, ymax=34.84)

LA <- map_ref%>%subset(COUNTYFP=='037'|COUNTYFP=='111'|COUNTYFP=='059')
ggplot (data = LA) + ggtitle("LA, change refining PM2.5 BAU between 2045 and 2019") +
  geom_sf(data = LA, aes(fill=change_pm25), color=NA) + theme_void() + labs(fill="PM2.5") +
  scale_fill_gradient(high = "#CCDDE5", low = "#005581", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$change_pm25), max(LA$change_pm25))) +
  geom_sf(data = LAcontour, fill="transparent", color="gray65")



#Contra Costa county
CCcontour<-map_data%>%subset(COUNTYFP=='013'|COUNTYFP=='095'|COUNTYFP=='077'|COUNTYFP=='001')
CC <- map_data%>%subset(COUNTYFP=='013'|COUNTYFP=='095'|COUNTYFP=='077'|COUNTYFP=='001')
total_pm25<-ggplot (data = LA) + ggtitle("Los Angeles County, total PM2.5 BAU 2019") +
  geom_sf(data = LA, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill="PM2.5") +
  scale_fill_gradient(high = "#CCDDE5", low = "#005581", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  geom_sf(data = LAcontour, fill="transparent", color="gray65")
plot(total_pm25)


## extraction

temp_data_ext <- ext_data%>%
  dplyr::select(-prim_pm25:-delta_prim_pm25)%>%
  spread(year, total_pm25)%>%
  mutate(change_pm25 = `2045`-`2019`)

map_ext <- left_join(CA_ct, temp_data_ext,by=c("GEOID"))

#State wide

ggplot(data = map_ext)+
  geom_sf(data = map_ext, aes(fill=change_pm25), color=NA) + theme_void() + labs(fill="PM2.5") +
  scale_fill_gradient(high = "#CCDDE5", low = "#005581", space = "Lab", na.value = "gray50",
                      limits = c(min(map_ext$change_pm25), max(map_ext$change_pm25))) + 
  ggtitle("California, change extraction PM2.5 BAU between 2045 and 2019")
