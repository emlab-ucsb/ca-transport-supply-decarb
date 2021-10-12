# CA transportation decarb: Local pm2.5 and mortality impacts
# hernandezcortes@ucsb.edu; vthivierge@ucsb.edu
# created: 09/13/2021
# updated: 10/12/2021

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

#no_cores <- availableCores()/2 - 1
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores, gc = TRUE)

#future:::ClusterRegistry("stop") ## To stop clusters

#Set directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../..') #Goes back to home project directory
getwd()

outputFiles <- "./calepa-cn/outputs/academic-out"
sourceFiles <- "./calepa-cn/data/health/source_receptor_matrix"
inmapExFiles  <- "./calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/extraction"
inmapReFiles  <- "./calepa-cn/data/health/source_receptor_matrix/inmap_processed_srm/refining"

# (1) From production to emissions #################################

# (1.1) Load source receptor matrix (srm) #######

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

#build extraction srm
srm_all_pollutants_extraction <-future_map_dfr(fields_vector, read_extraction) %>% 
  bind_rows()%>%
  rename(weighted_totalpm25=totalpm25_aw)%>%
  select(-totalpm25)%>%
  spread(poll, weighted_totalpm25)%>%
  rename(weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,id=field)

# Refining

sites_vector <- c(97, 119, 164, 202, 209, 226, 271, 279, 332, 342, 343, 800, 3422, 34222, 99999)

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

#build refining srm
srm_all_pollutants_refining <-future_map_dfr(sites_vector, read_refining) %>% 
  bind_rows()%>%
  rename(weighted_totalpm25=totalpm25_aw)%>%
  select(-totalpm25)%>%
  spread(poll, weighted_totalpm25)%>%
  rename(weighted_totalpm25nh3=nh3,weighted_totalpm25nox=nox,weighted_totalpm25pm25=pm25,weighted_totalpm25sox=sox,weighted_totalpm25voc=voc,site_id=site)

# (1.2) Calculate census tract ambient emissions for extraction  #######

#load and process cross-walk between fields and clusters 
extraction_field_clusters_10km<-read_csv(paste0(sourceFiles,"/extraction_fields_clusters_10km.csv",sep=""))%>%
  select(OUTPUT_FID,INPUT_FID)%>%
  rename(id=OUTPUT_FID,input_fid=INPUT_FID)

extraction_fields_xwalk<-foreign::read.dbf(paste0(sourceFiles,"/extraction_fields_xwalk_id.dbf",sep=""))%>%
  rename(input_fid=id,doc_field_code=dc_fld_)

extraction_xwalk<- extraction_field_clusters_10km%>%
  left_join(extraction_fields_xwalk,by=c("input_fid"))%>%
  mutate(doc_field_code = as.numeric(as.character(doc_field_code)))

#load extraction production scenarios 
extraction_outputs<-read_csv(paste(outputFiles, "/extraction/extraction_2021-09-21/field-results/subset_field_results.csv", sep = ""))%>%
  mutate(doc_field_code = as.double(doc_field_code))

#merge extraction production scenarios with extraction cluster 
fields_clusters<-left_join(extraction_outputs, extraction_xwalk,by="doc_field_code")

#summarize extraction production per cluster
total_clusters<-fields_clusters%>%
  group_by(id, year, scen_id)%>%
  summarize(total_prod_bbl=sum(total_prod_bbl))

#calculate air pollution using emission factors
total_clusters <- total_clusters%>%
  mutate(nh3=total_prod_bbl*0.00061/1000,
         nox=total_prod_bbl*0.04611/1000,
         pm25=total_prod_bbl*0.00165/1000,
         sox=total_prod_bbl*0.01344/1000,
         voc=total_prod_bbl*0.02614/1000)

# Merge cluster pollution with extraction srm to get census tract pollution exposure 

scenarios <- unique(total_clusters$scen_id)

prepare_extraction <- function(scenarios){
  
  scenarios_temp <- scenarios
  
  total_clusters_temp <-subset(total_clusters, (scen_id==scenarios_temp))%>%
  right_join(srm_all_pollutants_extraction,by=c("id"))%>%
    mutate(tot_nh3=weighted_totalpm25nh3*nh3,
           tot_nox=weighted_totalpm25nox*nox,
           tot_sox=weighted_totalpm25sox*sox,
           tot_pm25=weighted_totalpm25pm25*pm25,
           tot_voc=weighted_totalpm25voc*voc,
           total_pm25=tot_nh3+tot_nox+tot_pm25+tot_sox+tot_voc,
           prim_pm25=tot_pm25)

  ct_exposure<-total_clusters_temp%>%
    group_by(GEOID, year, scen_id)%>%
    summarize(total_pm25=sum(total_pm25, na.rm = T), prim_pm25=sum(prim_pm25, na.rm = T))
  
  tmp<-as.data.frame(ct_exposure) 
  
  return(tmp)
  
}

#build census tract extraction pm2.5 exposure
gc()
tic()
extraction_scenarios <-future_map_dfr(scenarios, prepare_extraction) %>% 
  bind_rows()%>%
  mutate(id = as.numeric(as.factor(scen_id)))
toc()

#extraction pm25 BAU
extraction_BAU<-subset(extraction_scenarios,(scen_id=="reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax"))%>%
  rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

#extraction pm25 differences
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
  mutate(nh3=value*0.00056/1000,
         nox=value*0.01495/1000,
         pm25=value*0.00402/1000,
         sox=value*0.00851/1000,
         voc=value*0.01247/1000)
  
# Merge refining srm to obtain census tract pm25 exposure

#Loop over scenarios  
scenarios <- unique(refining_outputs$scen_id)

prepare_refining <- function(scenarios){
  
  scenarios_temp <- scenarios
  
  refining_outputs_temp <-subset(refining_outputs, (scen_id==scenarios_temp))%>%
    right_join(srm_all_pollutants_refining, by=c("site_id"))%>%
    mutate(tot_nh3 = weighted_totalpm25nh3*nh3,
           tot_nox = weighted_totalpm25nox*nox,
           tot_sox = weighted_totalpm25sox*sox,
           tot_pm25 = weighted_totalpm25pm25*pm25,
           tot_voc = weighted_totalpm25voc*voc,
           total_pm25 = tot_nh3+tot_nox+tot_pm25+tot_sox+tot_voc,
           prim_pm25 = tot_pm25)
  
  ct_exposure<-refining_outputs_temp%>%
    group_by(GEOID, year, scen_id)%>%
    summarize(total_pm25=sum(total_pm25, na.rm = T), prim_pm25=sum(prim_pm25, na.rm = T))
  
  tmp<-as.data.frame(ct_exposure) 
  
  return(tmp)
  
}

#build refining census tract pm25 exposure
gc()
tic()
refining_scenarios <-future_map_dfr(scenarios, prepare_refining)%>% 
  bind_rows()
toc()

#refining pm25 BAU
refining_BAU<-subset(refining_scenarios,(scen_id=="R-BAU"))%>%
  rename(bau_total_pm25=total_pm25, bau_prim_pm25=prim_pm25)

#refining pm25 difference
deltas_refining<- refining_scenarios%>%
  left_join(refining_BAU %>% select(-scen_id),by=c("GEOID", "year"))%>%
  mutate(delta_total_pm25=total_pm25-bau_total_pm25,
         delta_prim_pm25=prim_pm25-bau_prim_pm25,
         sector = "refining")

future:::ClusterRegistry("stop") ## To stop clusters

# (1.4) Append extraction and refinig deltas #############

deltas <- deltas_extraction%>%
  mutate(scen_id = paste0("E-",id, sep=""))%>%
  select(GEOID, year,scen_id,sector,delta_total_pm25,total_pm25)%>%
  bind_rows(deltas_refining %>% select(GEOID, year,scen_id,sector,delta_total_pm25,total_pm25))

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
  drop_na(scen_id);ct_incidence_ca_poll # remove census tracts that are water area only tracts (no population)

# (3) Calculate mortality impact ######################################

#Coefficients from Krewski et al (2009)
beta <- 0.00582
se <- 0.0009628

#Mortality impact fold adults (>=29 years old)
ct_health <- ct_incidence_ca_poll %>%
  mutate(mortality_delta = ((exp(beta*delta_total_pm25)-1))*weighted_incidence*pop,
         mortality_level = ((exp(beta*total_pm25)-1))*weighted_incidence*pop)

#Output census tract level mortality 

#write.csv(ct_health, paste(outputFiles, "/health/census_tract_mortality.csv", sep = ""), row.names = F)
#ct_health <- fread(paste(outputFiles, "/health/census_tract_mortality.csv", sep = ""), stringsAsFactors = F)

# (4) Monetizing mortality ############################################

#Load data 

growth_rates <- read.csv("./calepa-cn/data/benmap/processed/growth_rates.csv", stringsAsFactors = FALSE)%>%
  filter(year > 2018)%>%
  mutate(growth = ifelse(year==2019,0,growth_2030),
         cum_growth = cumprod(1+growth))%>%
  select(-growth_2030,-growth);growth_rates

#Parameters

VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015*107.8645906/100 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
income_elasticity_mort <- 0.4
discount_rate = 0.03

future_WTP <- function(elasticity,growth_rate,WTP){
  return(elasticity*growth_rate*WTP + WTP) 
}

#Calculate the cost per premature mortality

ct_mort_cost <- ct_health %>%
  mutate(VSL_2019 = VSL_2019)%>%
  left_join(growth_rates, by = c("year"="year"))%>%
  mutate(VSL = future_WTP(income_elasticity_mort, 
                          (cum_growth-1),
                          VSL_2019),
         cost_2019 = mortality_level*VSL_2019,
         cost = mortality_level*VSL)%>%
  group_by(year)%>%
  mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
         cost_PV = cost/((1+discount_rate)^(year-2019)))

#Output census tract level monetized mortality 

#write.csv(ct_mort_cost,paste(outputFiles, "/health/ct_mort_cost.csv", sep = ""), row.names = FALSE)

# (5) TEMP ANALYSIS OF RESULTS  ##############################################

#Pollution exposure

state_ref <- deltas_refining%>%
  group_by(year,scen_id)%>%
  summarise(delta_total_pm25 = mean(delta_total_pm25, na.rm = T),
            total_pm25 = mean(total_pm25, na.rm = T),
            sector = unique(sector))%>%
  ungroup()

state_ref%>%
  gather(type,pm25,delta_total_pm25:total_pm25)%>%
  ggplot(aes(y=pm25, x=year, group = scen_id))+
  geom_line()+
  facet_grid(sector~type)+
  theme_cowplot()+
  theme(legend.position = "none")

#Mortality 

state_health <- ct_health%>%
  group_by(year,scen_id)%>%
  summarise(mortality_delta = sum(mortality_delta, na.rm = T),
            mortality_level = sum(mortality_level, na.rm = T),
            sector = unique(sector))%>%
  ungroup()

state_health%>%
  filter(sector %in% "extraction")%>%
  gather(type,mortality,mortality_delta:mortality_level)%>%
  ggplot(aes(y=mortality, x=year, group = scen_id))+
  geom_line()+
  facet_grid(sector~type, free)+
  theme_cowplot()+
  theme(legend.position = "none")

#Monetized mortality

state_health <- ct_mort_cost%>%
  group_by(year,scen_id)%>%
  summarise(cost_2019 = sum(cost_2019, na.rm = T),
            cost = sum(cost, na.rm = T),
            sector = unique(sector))%>%
  ungroup()

state_health%>%
  #filter(sector %in% "extraction")%>%
  gather(type,cost,cost_2019:cost)%>%
  ggplot(aes(y=cost, x=year, group = scen_id))+
  geom_line()+
  facet_grid(sector~type)+
  theme_cowplot()+
  theme(legend.position = "none")
