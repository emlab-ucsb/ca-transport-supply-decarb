# CalEPA: Homemade refining emission factors from NEI and XXX data
# vthivierge@ucsb.edu
# created: 04/05/2023
# updated: 04/12/2023


# set up environment

rm(list=ls())
`%notin%` <- Negate(`%in%`)

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

# Load production/capactiy/cluster refining data

ref_analysis <- fread("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/refinery_loc_cap_manual.csv")%>%
  clean_names();ref_analysis

ref_analysis %>% select(cluster,county) %>% table()

ref_renewable <- read_excel("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/renewable_refinery_capacity.xlsx")%>%
  clean_names();ref_renewable

ref_alt <- read_excel("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/altair_refinery_capacity.xlsx")%>%
  clean_names();ref_alt

# Process NEI data: restrict to refining & california

nei_ca_ref <- nei_2017_raw %>%
  filter(state %in% "CA")%>%
  mutate(naics6 = str_sub(naics_code,1,6),
         naics4 = str_sub(naics_code,1,4))%>%
  filter(naics6 %in% c("324110","325193"))%>%
  filter(pollutant_code %in% c("NOX","SO2","NH3","PM25-PRI","VOC"))
  #filter(naics4 %in% c("3241","3251"))

#graph
nei_ca_ref%>%
  mutate(company_clean = str_remove_all(company_name, c("REFINERY")))%>%
  mutate(company_clean = str_remove_all(company_clean, c("LLC")))%>%
  mutate(company_clean = str_remove_all(company_clean, c("COMPANY")))%>%
  mutate(company_clean = str_remove_all(company_clean, c("REFINING")))%>%
  mutate(company_clean = str_remove_all(company_clean, c("CO")))%>%
  #ggplot(aes(x=company_name, y = total_emissions))+
  ggplot(aes(fct_reorder(company_clean, -total_emissions), y = total_emissions))+
  geom_col()+
  facet_wrap(~pollutant_code)+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x = "Facility name", y = "2017 emissions (tons)")


# Match refineries

ref_analysis %>% select(site_id,company,corp,refinery_name,county) %>% arrange(county)
nei_ca_ref %>% select(eis_facility_id,company_name,site_name,county) %>% distinct() %>% arrange(county)

# At the cluster level

#capacity at cluster level (do we have production data?)
cluster_cap_2019 <- ref_analysis%>%
  group_by(cluster)%>%
  summarise(barrels_per_day = sum(barrels_per_day, na.rm=T))%>%
  ungroup()

cluster_factors <- nei_ca_ref %>% 
  filter(eis_facility_id %notin% c("15859711","4789411","10296811", "2534511"))%>% #drop (some of the) renewable refineries
  mutate(county = ifelse(county %in% "Solano", "Solano County", county))%>%
  left_join(ref_analysis %>% select(cluster,county) %>% distinct(), by = c("county"))%>%
  group_by(cluster,pollutant_code,reporting_year)%>%
  summarise(total_emissions = sum(total_emissions, na.rm=TRUE))%>%
  ungroup()%>%
  left_join(cluster_cap_2019, by = c("cluster"))%>%
  mutate(bbl_year = barrels_per_day*365,
         emission_kg = total_emissions*1000, #Ton to kg
         kg_bbl = emission_kg/bbl_year)

## Plot emission factors compared to Jaramillo and Muller (2016)

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
  facet_wrap(~cluster)+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x = "Criteria pollutant", y = "Emission factor (kg/bbl)",
       title = "Figure 1: Refining emission factors")

# ## Read raw emission factors from Jaramillo & Muller (2016)
# 
# ef_raw <- read.csv("./emission_factors.csv", stringsAsFactors = FALSE)%>%
#   filter(source %in% "jaramillo")%>%
#   select(-source)
# 
# ef <- ef_raw %>%
#   mutate(quantity_ton_mmillionbbl = quantity,
#          quantity_ton_bbl = quantity_ton_mmillionbbl/1000000)%>%
#   select(-unit); ef
# 
# ## Save final emission factors (kg/bbl)
# 
# write.csv(ef %>% select(-quantity:-quantity_kg_mmillionbbl),"./emission_factors_final.csv",row.names = F)
