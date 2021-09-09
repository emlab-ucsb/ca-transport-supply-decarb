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
library(foreign)
library(haven)
rm(list=ls())

outputFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/academic_output"
sourceFiles <- "D:/Dropbox/UCSB-PhD/emLab/CALEPA/data/source_receptor_matrix"


#X-WALK
#EXTRACTION
extraction_outputs<-read_csv(paste(outputFiles, "/site_extraction_outputs.csv", sep = ""))
extraction_outputs$doc_field_code<-as.double(extraction_outputs$doc_field_code)

extraction_xwalk<-read_dta(paste(sourceFiles, "/extraction_field_clusters_xxwalk.dta", sep=""))

srm_all_pollutants<-read_dta(paste(sourceFiles,"/inmap_processed_srm/srm_extraction_all_pollutants.dta", sep=""))

fields_clusters<-left_join(extraction_outputs, extraction_xwalk,by="doc_field_code")

fields_clusters$nh3=fields_clusters$total_prod_bbl*0.0061
fields_clusters$nox=fields_clusters$total_prod_bbl*0.04611
fields_clusters$pm25=fields_clusters$total_prod_bbl*0.00165
fields_clusters$sox=fields_clusters$total_prod_bbl*0.01344
fields_clusters$voc=fields_clusters$total_prod_bbl*0.02614

fields_matrix<-left_join(fields_clusters,srm_all_pollutants,by="id")

