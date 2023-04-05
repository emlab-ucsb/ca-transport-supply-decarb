# CalEPA: Homemade refining emission factors from NEI and XXX data
# vthivierge@ucsb.edu
# created: 04/05/2023
# updated: 04/05/2023


# set up environment

rm(list=ls())
`%notin%` <- Negate(`%in%`)

## Packages

packages=c("data.table", "dplyr", "janitor","stringr","ggplot2", "cowplot",
           "forcats")

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

#restrict to refining & california

nei_ca_ref <- nei_2017_raw %>%
  filter(state %in% "CA")%>%
  mutate(naics6 = str_sub(naics_code,1,6),
         naics4 = str_sub(naics_code,1,4))%>%
  filter(naics6 %in% c("324110","325193"))%>%
  filter(pollutant_code %in% c("NOX","SO2","NH3","PM25-PRI","VOC"))
  #filter(naics4 %in% c("3241","3251"))

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
