# CA transportation decarb: Local emissions and mortality impacts
# hernandezcortes@ucsb.edu; vthivierge@ucsb.edu
# created: 09/13/2021
# updated: 09/16/2021

# set up environment ########################################

rm(list=ls())

#Packages

packages=c("xlsx", "gdata", "dplyr","tidyr", "stringr", "fuzzyjoin", "stringr", "tictoc","maptools",
           "ggplot2", "stargazer", "plm", "cowplot", "sf", "lwgeom","data.table", "foreign", "purrr",
           "future", "furrr", "tidyverse")

lapply(1:length(packages), function(x) 
  ifelse((require(packages[x],character.only=TRUE)==FALSE),install.packages(packages[x]),
         require(packages[x],character.only=TRUE)))

#Set-up cores and memory

no_cores <- availableCores()/2 - 1
plan(multisession, workers = no_cores)
#memory.limit(size = 100000) #Set to 100GB (Only works on Windows...)

#Set directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../..') #Goes back to home project directory
getwd()

outputFiles <- "./calepa-cn/outputs/academic-out"
sourceFiles <- "./calepa-cn/data/health/source_receptor_matrix"
inmapExFiles  <- "./calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/extraction"
inmapReFiles  <- "./calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/refining"

# (1) From production to emissions #################################

# (1.1) Load source receptor matrix #######

#Extraction

fields_vector <- c(1:26)

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
extraction_srm <-future_map_dfr(fields_vector, read_extraction) %>% 
  bind_rows()%>%
  rename(weighted_totalpm25=totalpm25_aw)

srm_all_pollutants_extraction<-dcast(extraction_srm,field+GEOID~poll,value.var="weighted_totalpm25")%>%
  rename(weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,id=field)

# Refining

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
refining_srm <-future_map_dfr(sites_vector, read_refining) %>% 
  bind_rows()%>%
  rename(weighted_totalpm25=totalpm25_aw)

srm_all_pollutants_refining<-dcast(refining_srm,site+GEOID~poll,value.var="weighted_totalpm25")%>%
  rename(weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,site_id=site)

# (1.2) Calculate census tract ambient emissions for extraction  #######

#LOAD AND PROCESS X-WALK BETWEEN FIELDS AND CLUSTERS
extraction_field_clusters_10km<-read_csv(paste0(sourceFiles,"/extraction_fields_clusters_10km.csv",sep=""))%>%
  select(OUTPUT_FID,INPUT_FID)%>%
  rename(id=OUTPUT_FID,input_fid=INPUT_FID)

extraction_fields_xwalk<-foreign::read.dbf(paste0(sourceFiles,"/extraction_fields_xwalk_id.dbf",sep=""))%>%
  rename(input_fid=id,doc_field_code=dc_fld_)

extraction_xwalk<- extraction_field_clusters_10km%>%
  left_join(extraction_fields_xwalk,by=c("input_fid"))%>%
  mutate(doc_field_code = as.numeric(as.character(doc_field_code)))

#LOAD EXTRACTION OUTPUTS
extraction_outputs<-read_csv(paste(outputFiles, "/extraction/extraction_2021-09-07/field-results/subset/subset_field_results.csv", sep = ""))%>%
  mutate(doc_field_code = as.double(doc_field_code))

#MERGE EXTRACTION OUTPUTS AT THE FIELD LEVEL AND X-WALK
fields_clusters<-left_join(extraction_outputs, extraction_xwalk,by="doc_field_code")

#COLLAPSE: TOTAL PRODUCTION PER CLUSTER
total_clusters<-fields_clusters%>%
  group_by(id, year, scen_id)%>%
  summarize(total_prod_bbl=sum(total_prod_bbl))

#MULTIPLY BY EMISSIONS FACTORS
total_clusters <- total_clusters%>%
  mutate(nh3=total_prod_bbl*0.0061,
         nox=total_prod_bbl*0.04611,
         pm25=total_prod_bbl*0.00165,
         sox=total_prod_bbl*0.01344,
         voc=total_prod_bbl*0.02614)

#MERGE WITH SOURCE RECEPTOR MATRIX AND OBTAIN AVERAGE POLLUTION EXPOSURE

years_vector <- c(2019:2045)

prepare_extraction <- function(buff_year){
  
  byear <- buff_year
  
  total_clusters <-subset(total_clusters, (year==byear))%>%
  right_join(srm_all_pollutants_extraction,by=c("id"))%>%
    mutate(tot_nh3=weighted_totalpm25nh3*nh3,
           tot_nox=weighted_totalpm25nox*nox,
           tot_sox=weighted_totalpm25sox*sox,
           tot_pm25=weighted_totalpm25pm25*pm25,
           tot_voc=weighted_totalpm25voc*voc,
           total_pm25=tot_nh3+tot_nox+tot_pm25+tot_sox+tot_voc,
           prim_pm25=tot_pm25)

  mean_exposure<-total_clusters%>%
    group_by(GEOID, year, scen_id)%>%
    summarize(total_pm25=mean(total_pm25), prim_pm25=mean(prim_pm25))
  
  tmp<-as.data.frame(mean_exposure) 
  
  return(tmp)
  
}

#DO THE FUNCTION
extraction_scenarios <-future_map_dfr(years_vector, prepare_extraction) %>% 
  bind_rows()%>%
  mutate(id = as.numeric(as.factor(scen_id)))

#OBTAIN BAU
extraction_BAU<-subset(extraction_scenarios,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))%>%
  rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

#OBTAIN THE DIFFERENCE IN EXPOSURE
deltas_extraction <- extraction_scenarios %>%
  left_join(extraction_BAU %>% select(-id,-scen_id),by=c("GEOID", "year"))%>%
  mutate(delta_total_pm25 = total_pm25- bau_total_pm25,
         delta_prim_pm25 =prim_pm25 -bau_prim_pm25,
         sector = "extraction")

# (1.3) Calculate census tract ambient emissions for refining  #######

refining_outputs<-read_csv(paste(outputFiles, "/refining/refining_2021-09-07/site_refining_outputs.csv", sep = ""))

refining_outputs <- refining_outputs%>%
  mutate(site_id = ifelse(site_id=="t-800","800",site_id),
         site_id = ifelse(site_id=="342-2","34222",site_id),
         site_id = as.numeric(site_id))%>%
  mutate(nh3=value*0.00056,
         nox=value*0.01495,
         pm25=value*0.00402,
         sox=value*0.00851,
         voc=value*0.01247)
  
#MERGE WITH SOURCE RECEPTOR MATRIX AND OBTAIN AVERAGE POLLUTION EXPOSURE
  
years_vector <- c(2019:2045)

prepare_refining <- function(buff_year){
  
  byear <- buff_year
  
  refining_outputs <-subset(refining_outputs, (year==byear))%>%
    right_join(srm_all_pollutants_refining, by=c("site_id"))%>%
    mutate(tot_nh3 = weighted_totalpm25nh3*nh3,
           tot_nox = weighted_totalpm25nox*nox,
           tot_sox = weighted_totalpm25sox*sox,
           tot_pm25 = weighted_totalpm25pm25*pm25,
           tot_voc = weighted_totalpm25voc*voc,
           total_pm25 = tot_nh3+tot_nox+tot_pm25+tot_sox+tot_voc,
           prim_pm25 = tot_pm25)
  
  mean_exposure<-refining_outputs_merge%>%
    dplyr::group_by(GEOID, year, scen_id)%>%
    dplyr::summarize(total_pm25=mean(total_pm25), prim_pm25=mean(prim_pm25))
  
  tmp<-as.data.frame(mean_exposure) 
  
  return(tmp)
  
}

#DO THE FUNCTION
tic()
refining_scenarios <-future_map_dfr(years_vector, prepare_refining)%>% 
  bind_rows()
toc()

#OBTAIN BAU
refining_BAU<-subset(refining_scenarios,(scen_id=="R-BAU"))%>%
  rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

#OBTAIN THE DIFFERENCE IN EXPOSURE
deltas_refining<- refining_scenarios%>%
  left_join(refining_BAU,by=c("GEOID", "year"))%>%
  mutate(delta_total_pm25=total_pm25-bau_total_pm25,
         delta_prim_pm25=prim_pm25-bau_prim_pm25,
         sector = "refining")

# (1.4) Append extraction and refinig deltas #############

deltas <- deltas_extraction%>% 
  select(GEOID, year,scen_id,id,sector,delta_total_pm25,total_pm25)%>%
  bind_rows(deltas_refining %>% select(GEOID, year,scen_id,id,sector,delta_total_pm25,total_pm25))

# (2) Health impact #####################################

## (2.1) Load demographic data

# Disadvantaged community definition

ces3 <- read.csv("./calepa-cn/data/health/processed/ces3_data.csv", stringsAsFactors = FALSE)%>%
  select(census_tract,disadvantaged)%>%
  mutate(census_tract = paste0("0",census_tract, sep=""))

# Population and incidence

ct_inc_pop_45 <- fread("./calepa-cn/data/benmap/processed/ct_inc_45.csv", stringsAsFactors  = FALSE)%>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin,2,3),
                                   stringr::str_sub(gisjoin,5,7),
                                   stringr::str_sub(gisjoin,9,14)))%>%
  select(ct_id,lower_age, upper_age, year, pop, incidence_2015); ct_inc_pop_45

#census-tract level population-weighted incidence rate (for age>29)

ct_inc_pop_45_weighted <-ct_inc_pop_45 %>%
  filter(lower_age>29)%>%
  group_by(ct_id, year)%>%
  mutate(ct_pop = sum(pop, na.rm = T),
         share = pop/ct_pop,
         weighted_incidence = sum(share*incidence_2015, na.rm = T))%>%
  summarize(weighted_incidence = unique(weighted_incidence),
            pop = unique(ct_pop))%>%
  ungroup()

## (2.2) Merge demographic data to pollution scenarios

ct_incidence_ca_poll <- deltas %>%
  mutate(GEOID = ifelse(GEOID %in% "06037137000", "06037930401", GEOID))%>% #Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012 http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
  left_join(ces3, by = c("GEOID"="census_tract"))%>%
  right_join(ct_inc_pop_45_weighted, by = c("GEOID"="ct_id", "year"="year"))%>%
  drop_na(id);ct_incidence_ca_poll # remove census tracts that are water area only tracts (no population)

# (3) Calculate mortality impact ######################################

#Coefficients from Krewski et al (2009)
beta <- 0.00582
se <- 0.0009628

#Mortality impact fold adults (>=29 years old)
ct_health <- ct_incidence_ca_poll %>%
  #filter(lower_age>29)%>%
  mutate(mortality_delta = (1-(1/exp(beta*delta_total_pm25)))*weighted_incidence*pop,
         mortality_level = (1-(1/exp(beta*total_pm25)))*weighted_incidence*pop)


# (4) Analysis ##############################################

state_health <- ct_health%>%
  group_by(year,id)%>%
  summarise(mortality_delta = sum(mortality_delta, na.rm = T),
            mortality_level = sum(mortality_level, na.rm = T))%>%
  ungroup()

state_health%>%
  gather(type,mortality,mortality_delta:mortality_level)%>%
  ggplot(aes(y=mortality, x=year, group = id))+
  geom_line()+
  facet_grid(~type)+
  theme(legend.position = "none")+
  theme_cowplot()

