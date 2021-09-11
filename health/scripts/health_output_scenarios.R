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


############################################
#EXTRACTION
############################################
extraction_outputs<-read_csv(paste(outputFiles, "/09_07_2021/subset_field_results.csv", sep = ""))
extraction_outputs$doc_field_code<-as.double(extraction_outputs$doc_field_code)

extraction_xwalk<-read_dta(paste(sourceFiles, "/extraction_field_clusters_xxwalk.dta", sep=""))

srm_all_pollutants<-read_dta(paste(sourceFiles,"/inmap_processed_srm/srm_extraction_all_pollutants.dta", sep=""))

fields_clusters<-left_join(extraction_outputs, extraction_xwalk,by="doc_field_code")

#COLLAPSE: TOTAL PRODUCTION PER CLUSTER
total_clusters<-fields_clusters%>%group_by(id, year, scen_id)%>%summarize(total_prod_bbl=sum(total_prod_bbl))

#MULTIPLY BY EMISSIONS FACTORS
total_clusters$nh3=total_clusters$total_prod_bbl*0.0061
total_clusters$nox=total_clusters$total_prod_bbl*0.04611
total_clusters$pm25=total_clusters$total_prod_bbl*0.00165
total_clusters$sox=total_clusters$total_prod_bbl*0.01344
total_clusters$voc=total_clusters$total_prod_bbl*0.02614

#MERGE WITH SOURCE RECEPTOR MATRIX AND OBTAIN AVERAGE POLLUTION EXPOSURE

years_vector <- c(2020, 2021, 2022, 2023, 2024,	2025,	2026,	2027,	2028,	2029,	2030,	2031,	2032,	2033,	2034,	2035,	2036,	2037,	2038,	2039,	2040,	2041,	2042,	2043,	2044,	2045)

prepare_extraction <- function(buff_year){
  
  byear <- buff_year

  total_clusters <-subset(total_clusters, (year==byear))
  total_clusters_merge<-left_join(srm_all_pollutants,total_clusters,by=c("id"))

  #OBTAIN POLLUTION
  total_clusters_merge$tot_nh3=total_clusters_merge$weighted_totalpm25nh3*total_clusters_merge$nh3
  total_clusters_merge$tot_nox=total_clusters_merge$weighted_totalpm25nox*total_clusters_merge$nox
  total_clusters_merge$tot_sox=total_clusters_merge$weighted_totalpm25sox*total_clusters_merge$sox
  total_clusters_merge$tot_pm25=total_clusters_merge$weighted_totalpm25pm25*total_clusters_merge$pm25
  total_clusters_merge$tot_voc=total_clusters_merge$weighted_totalpm25voc*total_clusters_merge$voc
  
  total_clusters_merge$total_pm25=total_clusters_merge$tot_nh3+total_clusters_merge$tot_nox+total_clusters_merge$tot_pm25+total_clusters_merge$tot_sox+total_clusters_merge$tot_voc
  total_clusters_merge$prim_pm25=total_clusters_merge$tot_pm25
  
  mean_exposure<-total_clusters_merge%>%group_by(GEOID, year, scen_id)%>%summarize(total_pm25=mean(total_pm25), prim_pm25=mean(prim_pm25))

  tmp<-as.data.frame(mean_exposure) 
  
  return(tmp)
  
}
#DO THE FUNCTION
extraction_scenarios <-map_df(years_vector, prepare_extraction) %>% bind_rows()
#CONVERT TO NICE IDS NOT TEXT
extraction_scenarios$id <- as.numeric(as.factor(extraction_scenarios$scen_id))
#OBTAIN BAU
extraction_BAU<-subset(extraction_scenarios,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))
extraction_BAU<-extraction_BAU%>%rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

deltas_extraction<-left_join(extraction_scenarios,extraction_BAU,by=c("GEOID", "year"))
#OBTAIN THE DIFFERENCE IN EXPOSURE
deltas_extraction$delta_total_pm25=deltas_extraction$bau_total_pm25-deltas_extraction$total_pm25
deltas_extraction$delta_prim_pm25=deltas_extraction$bau_prim_pm25-deltas_extraction$prim_pm25
