# calepa-cn: Compute final results and create final labor figures 
# Chris Malloy (cmalloy@ucsb.edu)
# created: 08/24/2021
# updated: 08/24/2021

############################################################################################
# Set up environment 
############################################################################################

# Clearing previous 
rm(list=ls())


library("cowplot")
library("rstudioapi")
library("ggplot2")
library("dplyr")
library("tidyr")
library("magrittr")
library("readr")
library("stringr")
library("readxl")
library("quantmod")
library("lubridate")
library("writexl")
library("tigris")
library("sf")

#Set wd 

#Chris' macbook 
ica_dollar <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/academic-paper-multipliers/ica' 
impact_dollar <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/academic-paper-multipliers/impact'
statewide_processed <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/statewide/processed'
processed <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/academic-paper-multipliers/processed'
fte <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results'
energy_model_output_extraction <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/extraction_2021-08-18'
energy_model_output_refining <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/refining_2021-08-18'
population_files <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/raw/population'

############################################################################################ 

# Part A: Main results (county specific direct, indirect, induced) 

## 1. Import files with energy model output and multipliers (unit of observation: scen_id x county x year), compute direct, indirect, induced impacts 
setwd(processed)

ext_df <- read_xlsx('energy_model_output_with_multipliers.xlsx',sheet='extraction') 

ref_df <- read_xlsx('energy_model_output_with_multipliers.xlsx',sheet='refining') 


# 3. Import DAC status by census tract from CalEnviroScreen 2018 update 

setwd(health_team_files)

dac_tract <- read_xlsx("ces3results.xlsx",sheet="CES 3.0 (2018 Update)") 


setwd(population_files)

ttl_county_pop <- read_xlsx("co-est2019-annres-06.xlsx",range ="A6:B63",col_names = F)

ttl_county_pop <- dac_tract %>% 
  group_by(`California County`) %>% 
  summarize(ttl_pop = sum(`Total Population`))

dac_county_pop <- dac_tract %>% 
  filter(`SB 535 Disadvantaged Community`=="Yes") %>% 
  group_by(`California County`) %>% 
  summarize(dac_pop = sum(`Total Population`))

county_frac_dac <- left_join(ttl_county_pop,dac_county_pop,by="California County") %>% 
  mutate(dac_pop = ifelse(is.na(dac_pop)==T,0,dac_pop),
         dac_pop = as.numeric(dac_pop),frac_dac = dac_pop/ttl_pop)



## 2. Create figures 

ext_for_fig1 <- ext_df %>% 
  mutate(emp_p25 = quantile(dire_emp_mult,0.25,na.rm=T))
