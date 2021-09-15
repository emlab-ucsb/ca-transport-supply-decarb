# CA transportation decarb: Mortality impacts
# vthivierge@ucsb.edu
# created: 08/25/2021
# updated: 09/13/2021

# set up environment ########################################

rm(list=ls())

## Packages

packages=c("xlsx", "gdata", "dplyr","tidyr", "stringr", "fuzzyjoin", "stringr", "tictoc",
           "ggplot2", "stargazer", "plm", "cowplot", "sf", "lwgeom","data.table")

lapply(1:length(packages), function(x) 
  ifelse((require(packages[x],character.only=TRUE)==FALSE),install.packages(packages[x]),
         require(packages[x],character.only=TRUE)))

#Set directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../..') #Goes back to home project directory
getwd()

# Load and merge processed data #####################################

#1 Disadvantaged community definition

ces3 <- read.csv("./calepa-cn/data/health/processed/ces3_data.csv", stringsAsFactors = FALSE)%>%
  select(census_tract,disadvantaged)

#2 Merge pollution reduction scenarios

names <- list.files("./calepa-cn/data/benmap/processed/inmap/deltas/")
scenarios <- str_remove_all(names, c(".csv|scenario_"))
sector <- str_sub(scenarios,1,1)
  
str(read.csv(paste0("./calepa-cn/data/benmap/processed/inmap/deltas/",names[1], sep=""), stringsAsFactors = FALSE)) #sample of data 

names <- names[1:10] #temp file just for extraction

gc()

tic()
tff_pm25 <- bind_rows(lapply(1:length(names), function(x) 
  cbind(read.csv(paste0("./calepa-cn/data/benmap/processed/inmap/deltas/",names[x], sep=""), stringsAsFactors = FALSE)%>% 
          select(year, geoid,delta_totalpm25), #### Only select delta of total PM25
        scenario = scenarios[x], 
        sector = sector[x])))%>%
  left_join(ces3, by = c("geoid"="census_tract")); tff_pm25
toc()

#3 Merge population and incidence

ct_inc_pop_45 <- read.csv("./calepa-cn/data/benmap/processed/ct_inc_45.csv", stringsAsFactors  = FALSE)%>%
  mutate(ct_id = as.numeric(paste0(stringr::str_sub(gisjoin,3,3),
                        stringr::str_sub(gisjoin,5,7),
                        stringr::str_sub(gisjoin,9,14))))%>%
  select(ct_id,lower_age, upper_age, year, pop, incidence_2015); ct_inc_pop_45

#Adjust mismatch of census tract ids between inmap and benmap

tff_pm25  <- tff_pm25 %>%
  ungroup()%>%
  mutate(geoid = ifelse(geoid %in% 6037137000, 6037930401, geoid)) #confirmed that census ID one that changed in 2012 http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf

#Add pollution change 

ct_incidence_ca_poll <- ct_inc_pop_45 %>%
  left_join(tff_pm25, by = c("ct_id"="geoid", "year"="year")); ct_incidence_ca_poll

# Mortality impact ######################################

#Coefficients from Krewski et al (2009)
beta <- 0.00582
se <- 0.0009628

#Mortality impact fold adults (>=29 years old)
ct_health <- ct_incidence_ca_poll %>%
  filter(lower_age>29)%>%
  mutate(mortality_change = (1-(1/exp(beta*delta_totalpm25)))*incidence_2015*pop)

## Output for graph analysis

ct_health_mort <- ct_health %>%
  group_by(ct_id, year, sector, scenario)%>%
  summarise(mortality = sum(mortality_change),
            pop = sum(pop),
            disadvantaged = unique(disadvantaged))

#write.csv(ct_health_mort,file = "./calepa-cn/data/benmap/results/ct_mortality.csv", row.names = FALSE)

## Prelim analsysis of results

##Statewide by emission source

ct_health_mort%>%
  group_by(scenario)%>%
  summarise(mortality = sum(mortality, na.rm = T))%>%
  ggplot(aes(x=scenario, y=mortality))+
  geom_bar(stat='identity')+
  theme_cowplot()

state_wide <- ct_health_mort%>%
  mutate(sce_sector = paste0(scenario,"_",sector))%>%
  group_by(year,sce_sector)%>%
  summarize(mortality = as.integer(sum(mortality_change)))%>%#,
  #mortality_2020comp = as.integer(sum(mortality_change_2020comp)),
  #mortality_2020inc = as.integer(sum(mortality_change_2020inc)));state_wide
  spread(sce_sector,mortality);state_wide


