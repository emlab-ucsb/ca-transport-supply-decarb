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
outputFiles <- "emlab/projects/current-projects/calepa-cn/outputs/academic-out"
sourceFiles <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix"
inmapExFiles  <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/extraction"
inmapReFiles  <- "emlab/projects/current-projects/calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/refining"

#############################################
# PREPARE FILES FROM INMAP OUTPUT: EXTRACTION
#############################################

fields_vector <- c(1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23,	24,	25,	26)

read_extraction <- function(buff_field){
  
  bfield <- buff_field
  
  nh3<-read_csv(paste0(inmapExFiles,"/nh3/srm_nh3_field",bfield,".csv",sep=""))%>%mutate(poll="nh3")
  nox<-read_csv(paste0(inmapExFiles,"/nox/srm_nox_field",bfield,".csv",sep=""))%>%mutate(poll="nox")
  pm25<-read_csv(paste0(inmapExFiles,"/pm25/srm_pm25_field",bfield,".csv",sep=""))%>%mutate(poll="pm25")
  sox<-read_csv(paste0(inmapExFiles,"/sox/srm_sox_field",bfield,".csv",sep=""))%>%mutate(poll="sox")
  voc<-read_csv(paste0(inmapExFiles,"/voc/srm_voc_field",bfield,".csv",sep=""))%>%mutate(poll="voc")
  
  all_polls<-rbind(nh3,nox,pm25,sox,voc)

  all_polls$field=bfield
    
  tmp<-as.data.frame(all_polls) 
  
  return(tmp)
  
}
#DO THE FUNCTION
extraction_srm <-map_df(fields_vector, read_extraction) %>% bind_rows()
extraction_srm <-dplyr::rename(extraction_srm,weighted_totalpm25=totalpm25_aw)

extractions_srm_reshape<-dcast(extraction_srm,field+GEOID~poll,value.var="weighted_totalpm25")
srm_all_pollutants_extraction<-dplyr::rename(extractions_srm_reshape,weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,id=field)

#############################################
# PREPARE FILES FROM INMAP OUTPUT: REFINING
#############################################

#Excluding 164
#sites_vector <- c(97, 119, 164, 202, 209, 226, 271, 279, 332, 342, 343, 800, 3422, 34222, 99999)
#Including 164 and 99999 (164 misses NOx, SOx, and PM2.5, 99999 misses VOC)
sites_vector <- c(97, 119, 202, 209, 226, 271, 279, 332, 342, 343, 800, 3422, 34222, 99999)

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


############################################
#EXTRACTION
############################################

#LOAD AND PROCESS X-WALK BETWEEN FIELDS AND CLUSTERS
extraction_field_clusters_10km<-read_csv(paste0(sourceFiles,"/extraction_fields_clusters_10km.csv",sep=""))%>%
  dplyr::select(OUTPUT_FID,INPUT_FID)
extraction_field_clusters_10km<-dplyr::rename(extraction_field_clusters_10km,id=OUTPUT_FID,input_fid=INPUT_FID)

extraction_fields_xwalk<-read.dbf(paste0(sourceFiles,"/extraction_fields_xwalk_id.dbf",sep=""))
extraction_fields_xwalk<-dplyr::rename(extraction_fields_xwalk,input_fid=id,doc_field_code=dc_fld_)

extraction_xwalk<-left_join(extraction_field_clusters_10km,extraction_fields_xwalk,by=c("input_fid"))
extraction_xwalk$doc_field_code=as.numeric(as.character(extraction_xwalk$doc_field_code))

#LOAD EXTRACTION OUTPUTS
#extraction_outputs<-read_csv(paste(outputFiles, "/09_07_2021/subset_field_results.csv", sep = ""))
extraction_outputs<-read_csv(paste(outputFiles, "/extraction_2021-09-07/field-results/subset/subset_field_results.csv", sep = ""))
extraction_outputs$doc_field_code<-as.double(extraction_outputs$doc_field_code)

#MERGE EXTRACTION OUTPUTS AT THE FIELD LEVEL AND X-WALK
fields_clusters<-left_join(extraction_outputs, extraction_xwalk,by="doc_field_code")

#COLLAPSE: TOTAL PRODUCTION PER CLUSTER
total_clusters<-fields_clusters%>%
  dplyr::group_by(id, year, scen_id)%>%
  dplyr::summarize(total_prod_bbl=sum(total_prod_bbl))

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
  total_clusters_merge<-left_join(srm_all_pollutants_extraction,total_clusters,by=c("id"))

  #OBTAIN POLLUTION
  total_clusters_merge$tot_nh3=total_clusters_merge$weighted_totalpm25nh3*total_clusters_merge$nh3
  total_clusters_merge$tot_nox=total_clusters_merge$weighted_totalpm25nox*total_clusters_merge$nox
  total_clusters_merge$tot_sox=total_clusters_merge$weighted_totalpm25sox*total_clusters_merge$sox
  total_clusters_merge$tot_pm25=total_clusters_merge$weighted_totalpm25pm25*total_clusters_merge$pm25
  total_clusters_merge$tot_voc=total_clusters_merge$weighted_totalpm25voc*total_clusters_merge$voc
  
  total_clusters_merge$total_pm25=total_clusters_merge$tot_nh3+total_clusters_merge$tot_nox+total_clusters_merge$tot_pm25+total_clusters_merge$tot_sox+total_clusters_merge$tot_voc
  total_clusters_merge$prim_pm25=total_clusters_merge$tot_pm25
  
  mean_exposure<-total_clusters_merge%>%
    dplyr::group_by(GEOID, year, scen_id)%>%
    dplyr::summarize(total_pm25=mean(total_pm25), prim_pm25=mean(prim_pm25))

  tmp<-as.data.frame(mean_exposure) 
  
  return(tmp)
  
}
#DO THE FUNCTION
extraction_scenarios <-map_df(years_vector, prepare_extraction) %>% 
  bind_rows()
#CONVERT TO NICE IDS NOT TEXT
extraction_scenarios$id <- as.numeric(as.factor(extraction_scenarios$scen_id))
#OBTAIN BAU
extraction_BAU<-subset(extraction_scenarios,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))
extraction_BAU<-extraction_BAU%>%
  dplyr::rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

deltas_extraction<-left_join(extraction_scenarios,extraction_BAU,by=c("GEOID", "year"))
#OBTAIN THE DIFFERENCE IN EXPOSURE
deltas_extraction$delta_total_pm25=deltas_extraction$total_pm25-deltas_extraction$bau_total_pm25
deltas_extraction$delta_prim_pm25=deltas_extraction$prim_pm25-deltas_extraction$bau_prim_pm25


##################################
#REFINING
##################################
refining_outputs<-read_csv(paste(outputFiles, "/refining_2021-09-07/site_refining_outputs.csv", sep = ""))
#Danae's
#refining_outputs<-read_csv(paste(outputFiles, "/09_07_2021/site_refining_outputs.csv", sep = ""))
refining_outputs$site_id<-ifelse(refining_outputs$site_id=="t-800","800",refining_outputs$site_id)
refining_outputs$site_id<-ifelse(refining_outputs$site_id=="342-2","34222",refining_outputs$site_id)

refining_outputs$nh3=refining_outputs$value*0.00056
refining_outputs$nox=refining_outputs$value*0.01495
refining_outputs$pm25=refining_outputs$value*0.00402
refining_outputs$sox=refining_outputs$value*0.00851
refining_outputs$voc=refining_outputs$value*0.01247

refining_outputs$site_id=as.numeric(refining_outputs$site_id)
#MERGE WITH SOURCE RECEPTOR MATRIX AND OBTAIN AVERAGE POLLUTION EXPOSURE

years_vector <- c(2020, 2021, 2022, 2023, 2024,	2025,	2026,	2027,	2028,	2029,	2030,	2031,	2032,	2033,	2034,	2035,	2036,	2037,	2038,	2039,	2040,	2041,	2042,	2043,	2044,	2045)

prepare_refining <- function(buff_year){
  
  byear <- buff_year
  
  refining_outputs <-subset(refining_outputs, (year==byear))
  refining_outputs_merge<-left_join(srm_all_pollutants_refining,refining_outputs,by=c("site_id"))
  
  #OBTAIN POLLUTION
  refining_outputs_merge$tot_nh3=refining_outputs_merge$weighted_totalpm25nh3*refining_outputs_merge$nh3
  refining_outputs_merge$tot_nox=refining_outputs_merge$weighted_totalpm25nox*refining_outputs_merge$nox
  refining_outputs_merge$tot_sox=refining_outputs_merge$weighted_totalpm25sox*refining_outputs_merge$sox
  refining_outputs_merge$tot_pm25=refining_outputs_merge$weighted_totalpm25pm25*refining_outputs_merge$pm25
  refining_outputs_merge$tot_voc=refining_outputs_merge$weighted_totalpm25voc*refining_outputs_merge$voc
  
  refining_outputs_merge$total_pm25=refining_outputs_merge$tot_nh3+refining_outputs_merge$tot_nox+refining_outputs_merge$tot_pm25+refining_outputs_merge$tot_sox+refining_outputs_merge$tot_voc
  refining_outputs_merge$prim_pm25=refining_outputs_merge$tot_pm25
  
  mean_exposure<-refining_outputs_merge%>%
    dplyr::group_by(GEOID, year, scen_id)%>%
    dplyr::summarize(total_pm25=mean(total_pm25), prim_pm25=mean(prim_pm25))
  
  tmp<-as.data.frame(mean_exposure) 
  
  return(tmp)
  
}
#DO THE FUNCTION
refining_scenarios <-map_df(years_vector, prepare_refining) %>% bind_rows()

#OBTAIN BAU
refining_BAU<-subset(refining_scenarios,(scen_id=="R-BAU"))
refining_BAU<-refining_BAU%>%rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

deltas_refining<-left_join(refining_scenarios,refining_BAU,by=c("GEOID", "year"))
#OBTAIN THE DIFFERENCE IN EXPOSURE
deltas_refining$delta_total_pm25=deltas_refining$total_pm25-deltas_refining$bau_total_pm25
deltas_refining$delta_prim_pm25=deltas_refining$prim_pm25-deltas_refining$bau_prim_pm25
