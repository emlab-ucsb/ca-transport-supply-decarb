#Danae Hernandez-Cortes hernandezcortes@ucsb.edu
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



######################VINCENT: PLEASE ADD THE MODIFIED CODE THAT OBTAINS EXTRACTION
#DO THE FUNCTION
extraction_scenarios <-map_df(years_vector, prepare_extraction) %>% 
  bind_rows()
#CONVERT TO NICE IDS NOT TEXT
extraction_scenarios$id <- as.numeric(as.factor(extraction_scenarios$scen_id))
#OBTAIN BAU
extraction_BAU<-subset(extraction_scenarios,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))
extraction_BAU<-extraction_BAU%>%
  dplyr::rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25, bau_avg_total_pm25=avg_total_pm25)
extraction_BAU_2019<-extraction_BAU%>%dplyr::filter(year==2019)

###################POPULATION##########################
#0. Bring disadvantaged communities data from CES
 ces_data <- read.csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/health/raw/ces3results_part.csv", stringsAsFactors = FALSE) %>%
   dplyr::select("census_tract","total_population")


CA<-st_read("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/state/california2016.shp")

CA_counties<-st_read("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")
#CA_counties<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/CA_Counties/CA_Counties/CA_Counties_TIGER2016.shp")

CA_ct<-st_read("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/raw/census-tract/tl_2019_06_tract.shp")
#CA_ct<-st_read("D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/census_tracts/census-tract/tl_2019_06_tract.shp")

#Merge extraction
map_data<-left_join(CA_ct,extraction_BAU_2019,by=c("GEOID"))

#LA county
LAcontour<-CA_counties%>%subset(COUNTYFP=='037'|COUNTYFP=='111'|COUNTYFP=='059')
LA_contour_cropped<-st_crop(LAcontour, xmin=-119.4, xmax=117.46, ymin=33.34, ymax=34.84)

LA <- map_data%>%subset(COUNTYFP=='037'|COUNTYFP=='111'|COUNTYFP=='059')
total_pm25<-ggplot (data = LA) + ggtitle("Los Angeles County, total PM2.5 BAU 2019") +
  geom_sf(data = LA, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill="PM2.5") +
  scale_fill_gradient(high = "#CCDDE5", low = "#005581", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  geom_sf(data = LAcontour, fill="transparent", color="gray65")
plot(total_pm25)



#Contra Costa county
CCcontour<-map_data%>%subset(COUNTYFP=='013'|COUNTYFP=='095'|COUNTYFP=='077'|COUNTYFP=='001')
CC <- map_data%>%subset(COUNTYFP=='013'|COUNTYFP=='095'|COUNTYFP=='077'|COUNTYFP=='001')
total_pm25<-ggplot (data = LA) + ggtitle("Los Angeles County, total PM2.5 BAU 2019") +
  geom_sf(data = LA, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill="PM2.5") +
  scale_fill_gradient(high = "#CCDDE5", low = "#005581", space = "Lab", na.value = "gray50",
                      limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
  geom_sf(data = LAcontour, fill="transparent", color="gray65")
plot(total_pm25)
