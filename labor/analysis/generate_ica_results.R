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
health_team_files <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/health/raw'
energy_model_output_extraction <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/extraction_2021-08-18'
energy_model_output_refining <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/refining_2021-08-18'

############################################################################################ 
# Part A: file with total multipliers 

# 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 

## NOTE: multipliers are per $1 million of output value

setwd(processed)

total_multipliers_ext <- read_xlsx('ica_multipliers_v2.xlsx',sheet='ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, ip.indi_comp_mult = ip.indirect_comp, ip.indu_comp_mult = ip.induced_comp)

total_multipliers_ref <- read_xlsx('ica_multipliers_v2.xlsx',sheet='ica_total') %>% 
  filter((county != "Statewide" & segment == "refining") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, ip.indi_comp_mult = ip.indirect_comp, ip.indu_comp_mult = ip.induced_comp)

# 2. Import extraction and refining output from the energy modeling team 

setwd(energy_model_output_extraction) 

extraction_revenue <- read_csv('county_extraction_outputs.csv') %>% 
  rename(county = adj_county_name)

setwd(energy_model_output_refining) 

refining_revenue <- read_csv('county_refining_outputs.csv') %>% 
  mutate(county = ifelse(county=="Solano County", "Solano",county))


# 3. Import DAC status by census tract from CalEnviroScreen 2018 update 

setwd(health_team_files)

dac_tract <- read_xlsx("ces3results.xlsx",sheet="CES 3.0 (2018 Update)") 

ttl_county_pop <- dac_tract %>% 
  group_by(`California County`) %>% 
  summarize(ttl_pop = sum(`Total Population`))


# 3. Join multipliers to output from the energy modeling team by county 

ext_with_multipliers <- inner_join(extraction_revenue,total_multipliers_ext,by=c("county")) 

ref_with_multipliers <- inner_join(refining_revenue,total_multipliers_ref,by=c("county")) 


# 4. Save extraction and refining output with multipliers to 1 excel file with 2 sheets 


setwd(processed)

ica_list = list(extraction=ext_with_multipliers,refining = ref_with_multipliers)
write_xlsx(x=ica_list,"energy_model_output_with_multipliers.xlsx")



####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# Part B: File with statewide multipliers (inclusive of indirect and induced effects across counties)

# 1. Import processed IMPLAN multipliers for the labor analysis and keep the statewide multipliers 

## NOTE: multipliers are per $1 million of output value

setwd(processed)

state_multipliers_ext <- read_xlsx('ica_multipliers_v2.xlsx',sheet='ica_total') %>% 
  filter((county == "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, ip.indi_comp_mult = ip.indirect_comp, ip.indu_comp_mult = ip.induced_comp)

state_multipliers_ref <- read_xlsx('ica_multipliers_v2.xlsx',sheet='ica_total') %>% 
  filter((county == "Statewide" & segment == "refining") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, ip.indi_comp_mult = ip.indirect_comp, ip.indu_comp_mult = ip.induced_comp)


# 2. Collapse energy model output to the scenario-state-year level, add statewide multipliers 

extraction_revenue_state <- extraction_revenue %>% 
  group_by(scen_id,year) %>% 
  summarize(oil_price_scenario=first(oil_price_scenario),innovation_scenario=first(innovation_scenario),
            carbon_price_scenario=first(carbon_price_scenario),ccs_scenario=first(ccs_scenario),
            setback_scenario=first(setback_scenario),prod_quota_scenario=first(prod_quota_scenario),
            excise_tax_scenario=first(excise_tax_scenario),total_county_bbl=sum(total_county_bbl),
            oil_price_usd_per_bbl=first(oil_price_usd_per_bbl), revenue = sum(revenue)) %>% 
  mutate(direct_emp = state_multipliers_ext$direct_emp, indirect_emp = state_multipliers_ext$indirect_emp, induced_emp = state_multipliers_ext$induced_emp,
         direct_comp = state_multipliers_ext$direct_comp, indirect_comp = state_multipliers_ext$indirect_comp, induced_comp = state_multipliers_ext$induced_comp)


refining_revenue_state <- refining_revenue %>% 
  group_by(scen_id,year) %>% 
  summarize(revenue = sum(revenue),oil_price_scenario = first(oil_price_scenario),demand_scenario=first(demand_scenario),refining_scenario=first(refining_scenario),
            innovation_scenario=first(innovation_scenario),carbon_price_scenario=first(carbon_price_scenario),ccs_scenario=first(ccs_scenario)) %>% 
  mutate(direct_emp = state_multipliers_ref$direct_emp, indirect_emp = state_multipliers_ref$indirect_emp, induced_emp = state_multipliers_ref$induced_emp,
         direct_comp = state_multipliers_ref$direct_comp, indirect_comp = state_multipliers_ref$indirect_comp, induced_comp = state_multipliers_ref$induced_comp)

#3. Output to xlsx with 2 sheets 

setwd(processed)

ica_state_list = list(extraction=extraction_revenue_state,refining = refining_revenue_state)
write_xlsx(x=ica_state_list,"statewide_energy_model_output_with_multipliers.xlsx")


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# Part C: File with multipliers by industry 

# 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 

## NOTE: multipliers are per $1 million of output value

setwd(processed)

ind_multipliers_ext <- read_csv('ica_multipliers_by_industry_long.csv') %>% 
  filter((county != "Statewide" & segment == "extraction")) %>% 
  mutate(indirect_induced_emp = indirect_emp + induced_emp, 
         indirect_induced_comp = indirect_comp + induced_comp) %>% 
  group_by(county) %>% 
  arrange(-indirect_induced_emp) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank<=10) %>% 
  dplyr::select(-indirect_induced_emp,-indirect_induced_comp) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp)

ind_multipliers_ref <- read_csv('ica_multipliers_by_industry_long.csv') %>% 
  filter((county != "Statewide" & segment == "refining")) %>% 
  mutate(indirect_induced_emp = indirect_emp + induced_emp, 
         indirect_induced_comp = indirect_comp + induced_comp) %>% 
  group_by(county) %>% 
  arrange(-indirect_induced_emp) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank<=10) %>% 
  dplyr::select(-indirect_induced_emp,-indirect_induced_comp) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp)

# 2. Import extraction and refining output from the energy modeling team 

setwd(energy_model_output_extraction) 

extraction_revenue <- read_csv('county_extraction_outputs.csv') %>% 
  rename(county = adj_county_name)

setwd(energy_model_output_refining) 

refining_revenue <- read_csv('county_refining_outputs.csv') %>% 
  mutate(county = ifelse(county=="Solano County", "Solano",county))


# 3. Join multipliers to output from the energy modeling team by county 

ext_with_multipliers_ind <- inner_join(extraction_revenue,ind_multipliers_ext,by=c("county")) 

ref_with_multipliers_ind <- inner_join(refining_revenue,ind_multipliers_ref,by=c("county")) 


# 4. output results to 2 csv files

setwd(processed)

write_csv(ext_with_multipliers_ind,"ext_industry_energy_model_output_with_multipliers.csv")
write_csv(ref_with_multipliers_ind,"ref_industry_energy_model_output_with_multipliers.csv")


