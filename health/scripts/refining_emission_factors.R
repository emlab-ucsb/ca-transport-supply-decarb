# CalEPA: Homemade refining emission factors from NEI and XXX data
# vthivierge@ucsb.edu
# created: 04/05/2023
# updated: 06/26/2023

# set up environment

rm(list=ls())
`%notin%` <- Negate(`%in%`)

options(scipen=999)

## Packages

packages=c("data.table", "dplyr", "janitor","stringr","ggplot2", "cowplot",
           "forcats", "readxl")

for (i in packages) {
  if (require(i,character.only=TRUE)==FALSE) {
    install.packages(i,repos='http://cran.us.r-project.org')
  }
  else {
    require(i,character.only=TRUE)
  }
}

## Directory

# wd <- c("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/health/processed/DataFiles_EmissionsFactors") #Vincent's WD
# setwd(wd)
# getwd()

#temporary working direction

wd <- c("G:/My Drive/UCSB/research/current/efficiency/data") #Vincent's WD
setwd(wd)

## 2017 NEI

# poll_sub_ghg <- c(poll_sub, "CH4", "CO2","N2O","SF6")
# poll_ghg <- c("CH4", "CO2","N2O","SF6")

nei_2017_raw <- fread(paste0(wd, "/raw/NEI/emis_sum_fac_15420.csv", sep=""))%>%
  clean_names()%>%
  mutate(reporting_year = 2017)%>%
  #filter(pollutant_code %in% poll_sub_ghg)%>%
  filter(pollutant_type_s %in% "CAP")

## 2014 NEI

nei_2014_raw <- fread(paste0(wd, "/raw/NEI/2014v2facilities.csv", sep=""))%>%
  clean_names()

nei_2014 <- nei_2014_raw %>%
  filter(st_usps_cd %in% "CA")%>%
  mutate(naics6 = str_sub(naics_cd,1,6),
         reporting_year = 2014)%>%
  filter(naics6 %in% c("324110"))%>%
  filter(pollutant_cd %in% c("NOX","SO2","NH3","PM25-PRI","VOC"))%>% 
  select(eis_facility_site_id,reporting_year,naics6,facility_site_name,county_name,total_emissions,pollutant_cd)%>%
  distinct()%>% 
  filter(eis_facility_site_id %notin% c("15859711","4789411","10296811", "2534511"))%>%
  rename("eis_facility_id"="eis_facility_site_id","site_name"="facility_site_name","county"="county_name","pollutant_code"="pollutant_cd")
  
# Load refining-level data

ref_analysis <- fread("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/refinery_loc_cap_manual.csv")%>%
  clean_names();ref_analysis

ref_analysis %>% select(cluster,county) %>% table()

ref_renewable <- read_excel("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/renewable_refinery_capacity.xlsx")%>%
  clean_names();ref_renewable

ref_alt <- read_excel("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/altair_refinery_capacity.xlsx")%>%
  clean_names();ref_alt

# Load cluster level production data

ref_prod <- fread("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/fuel_watch_data.csv")%>%
  clean_names()%>%
  filter(stock_flow == "Refinery Input")%>%
  #filter(stock_flow == "Refinery Production")%>%
  group_by(year,region)%>%
  summarise(thous_barrels = sum(thous_barrels, na.rm=T))%>%
  ungroup()%>%
  filter(year %in% c(2014,2017))

# Process NEI data: restrict to refining & california

nei_ca_ref <- nei_2017_raw %>%
  filter(state %in% "CA")%>%
  mutate(naics6 = str_sub(naics_code,1,6),
         naics4 = str_sub(naics_code,1,4))%>%
  #filter(naics6 %in% c("324110","325193"))%>% #Do not include ethanol refineries
  filter(naics6 %in% c("324110"))%>%
  #filter(naics4 %in% c("3241"))%>%
  filter(pollutant_code %in% c("NOX","SO2","NH3","PM25-PRI","VOC"))%>%
  select(eis_facility_id,reporting_year,naics6,company_name,site_name,address,city,county,total_emissions,pollutant_code)%>%
  bind_rows(nei_2014)
  
# Debugging the matching of refineries #####################################

ref_analysis %>% select(site_id,company,corp,refinery_name,county) %>% arrange(county)
nei_ca_ref %>% select(eis_facility_id,naics6,company_name,site_name,county,total_emissions,pollutant_code) %>% filter(pollutant_code %in% "NOX") %>% distinct() %>% arrange(county)
nei_ca_ref %>% select(eis_facility_id,naics6,company_name,site_name,county) %>% distinct() %>% arrange(county)

ref_analysis %>% select(site_id,company,corp,refinery_name,county) %>% arrange(county) %>% filter(county %in% "Los Angeles")%>%
  arrange(refinery_name)#%>% write.csv("C://Users/User/Desktop/ref.csv", row.names = F)

nei_ca_ref %>% 
  select(-reporting_year,-total_emissions)%>%distinct()%>%
  filter(pollutant_code %in% "NOX") %>% distinct() %>% arrange(county) %>% filter(county %in% "Los Angeles")%>%
  arrange(site_name)#%>% write.csv("C://Users/User/Desktop/nei.csv", row.names = F)

# Drop renewable and asphalt refineries ############################

#From capacity data

ref_analysis_clean <- ref_analysis%>%
  filter(site_id %notin% c("489","550", "191")) #remove asphalt refineires

#From NEI data

nei_ref_clean <- nei_ca_ref %>% 
  filter(eis_facility_id %notin% c("15859711","4789411","10296811", "2534511", "5683611", "13703511", "365011"))%>%  #drop renewable, dehydration, and asphalt refineries
  mutate(county = ifelse(county %in% "Solano", "Solano County", county))
  
# Emission factors at the cluster level #################################################

#capacity at cluster level 
cluster_cap_2019 <- ref_analysis_clean%>%
  group_by(cluster)%>%
  summarise(barrels_per_day = sum(barrels_per_day, na.rm=T))%>%
  ungroup()

cluster_factors <- nei_ref_clean %>% 
  left_join(ref_analysis %>% select(cluster,county) %>% distinct(), by = c("county"))%>%
  group_by(cluster,pollutant_code,reporting_year)%>%
  summarise(total_emissions = sum(total_emissions, na.rm=TRUE))%>%
  ungroup()%>%
  left_join(cluster_cap_2019, by = c("cluster"))%>%
  left_join(ref_prod,  by = c("cluster"="region", "reporting_year"="year"))%>%
  mutate(prod_per_day = (thous_barrels*1000)/365,
         capacity_util = prod_per_day/barrels_per_day)%>%
  mutate(bbl_year = thous_barrels*1000,
         emission_kg = total_emissions*1000, #Ton to kg
         kg_bbl = emission_kg/bbl_year,
         kg_bbl_cap = emission_kg/(barrels_per_day*365))

## Plot emission factors compared to Jaramillo and Muller (2016) #############################

JM <- fread("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/health/processed/DataFiles_EmissionsFactors/emission_factors_final.csv")

#poll <-c("pm25","nox","sox","voc","nh3")
#names(poll) <- c("PM25-PRI","NOX","SO2","VOC","NH3")
poll <- c("PM25-PRI","NOX","SO2","VOC","NH3")
names(poll) <-c("pm25","nox","sox","voc","nh3")


JM_ref <- JM%>%
  mutate(pollutant = str_replace_all(pollutant, pattern = poll),
         cluster = "JM (2016)",
         quantity_kg_bbl = quantity_ton_bbl*1000)%>%
  filter(process %in% "refining")
  

cluster_factors %>%
  mutate(ton_bbl = kg_bbl/1000)%>%
  left_join(JM_ref %>% select(-cluster), by = c("pollutant_code"="pollutant"))%>%
  ggplot(aes(fct_reorder(pollutant_code, -emission_kg), y = kg_bbl))+
  geom_col()+
  geom_col(data = JM_ref, aes(x=pollutant ,y=quantity_kg_bbl))+
  facet_grid(reporting_year~cluster)+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x = "Criteria pollutant", y = "Emission factor (kg/bbl)",
       title = "Figure 1: Refining emission factors")

cluster_factors %>%
  ggplot(aes(x=reporting_year, y = kg_bbl, group=cluster,color=cluster))+
  geom_line()+
  geom_point()+
  facet_wrap(~pollutant_code)+
  theme_cowplot()+
  labs(x = "Year", y = "Emission factor (kg/bbl)",
       title = "Figure 2: Refining emission factors by year",
       color = "Cluster")+
  scale_x_continuous(breaks = c(2014,2017))
  
## Output emission factors ########################

cluster_factors %>%
  filter(reporting_year %in% 2017)%>%
  select(cluster,pollutant_code,kg_bbl)%>% 
  write.csv("C://git/ca-transport-supply-decarb/health/data/ref_emission_factor.csv", row.names = F)


