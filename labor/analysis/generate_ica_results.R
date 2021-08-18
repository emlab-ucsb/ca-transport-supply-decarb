# calepa-cn: Computing employment and compensation results for each scenario 
# Chris Malloy (cmalloy@ucsb.edu)
# created: 07/28/2021
# updated: 08/18/2021

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
energy_model_output_extraction <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/extraction_2021-08-12'
energy_model_output_refining <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/refining_2021-08-12'

############################################################################################ 

# 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 

## NOTE: multipliers are per $1 million of output value

setwd(processed)

total_multipliers_ext <- read_xlsx('ica_multipliers_v2.xlsx',sheet='ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T)

total_multipliers_ref <- read_xlsx('ica_multipliers_v2.xlsx',sheet='ica_total') %>% 
  filter((county != "Statewide" & segment == "refining") | is.na(segment)==T)

# 2. Import extraction and refining output from the energy modeling team 

setwd(energy_model_output_extraction) 

extraction_revenue <- read_csv('county_extraction_outputs.csv') %>% 
  rename(county = adj_county_name)

setwd(energy_model_output_refining) 

refining_revenue <- read_csv('county_refining_outputs.csv') %>% 
  mutate(county = ifelse(county=="Solano County", "Solano",county))


# 3. Join multipliers to output from the energy modeling team by county 

ext_with_multipliers <- inner_join(extraction_revenue,total_multipliers_ext,by=c("county")) 

ref_with_multipliers <- inner_join(refining_revenue,total_multipliers_ref,by=c("county")) 


# 4. Save extraction and refining output with multipliers to 1 excel file with 2 sheets 


setwd(processed)

ica_list = list(extraction=ext_with_multipliers,refining = ref_with_multipliers)
write_xlsx(x=ica_list,"energy_model_output_with_multipliers.xlsx")






