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

#############################################
# PREPARE FILES FROM INMAP OUTPUT: EXTRACTION
#############################################
bau_scenario<-read_csv(paste0(sourceFiles,"/inmap_comparison_srm/comparison_field0.csv",sep=""))%>%dplyr::rename(bau_totalpm25=totalpm25, bau_totalpm25_aw=totalpm25_aw)

scenario1<-read_csv(paste0(sourceFiles,"/inmap_comparison_srm/comparison_field1.csv",sep=""))%>%mutate(scenario="scenario 1")
scenario2<-read_csv(paste0(sourceFiles,"/inmap_comparison_srm/comparison_field2.csv",sep=""))%>%mutate(scenario="scenario 2")
scenario3<-read_csv(paste0(sourceFiles,"/inmap_comparison_srm/comparison_field3.csv",sep=""))%>%mutate(scenario="scenario 3")
scenario4<-read_csv(paste0(sourceFiles,"/inmap_comparison_srm/comparison_field4.csv",sep=""))%>%mutate(scenario="scenario 4")

scenarios<-rbind(scenario1, scenario2, scenario3, scenario4)

deltas_extraction<-left_join(scenarios,bau_scenario,by=c("GEOID"))
deltas_extraction$delta_total_pm25aw=deltas_extraction$totalpm25_aw-deltas_extraction$bau_totalpm25_aw

write_csv(deltas_extraction,paste0(sourceFiles,"/inmap_comparison_srm/comparison_deltas.csv",sep=""))

summary(deltas_extraction$totalpm25_aw)
summary(deltas_extraction$delta_total_pm25aw)