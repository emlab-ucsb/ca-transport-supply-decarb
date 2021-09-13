# calepa-cn: Import and process the latest labor results from the energy model 
# Chris Malloy (cmalloy@ucsb.edu)
# created: 09/13/2021
# updated: 09/13/2021

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

#Chris' laptop 
ica_dollar <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/academic-paper-multipliers/ica' 
impact_dollar <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/academic-paper-multipliers/impact'
statewide_processed <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/statewide/processed'
processed <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results/academic-paper-multipliers/processed'
fte <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/processed/implan-results'
health_team_files <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/health/raw'
energy_model_output_extraction <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/extraction_2021-09-07/county-results/subset'
energy_model_output_refining <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/refining_2021-09-07'
energy_model_output_misc <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/misc'
energy_model_output_scenarios <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction'
population_files <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/raw/population'

############################################################################################ 


# 1. Import energy model and labor output separately for extraction and refining 

## 1a. Scenario list (identifies subset scenarios and BAU) 

setwd(energy_model_output_scenarios)

scen_list <- read_csv('scenario_id_list.csv')

str(scen_list)

scen_list <- scen_list %>% 
  filter(subset_scens==1 | BAU_scen==1) 

## 1a. Extraction 

setwd(energy_model_output_extraction) 

### check that the multipliers implied by the labor output matches the original multipliers (as of 9/13/21 they do)

test <- readRDS('reference case_no_setback_no quota_carbon_setback_5280ft_high CCS cost_low innovation_no tax_county_results.rds')

test_check <- test %>% 
  mutate(c.dire_emp_mult = (c.dire_emp*10^6)/revenue,
         c.indi_emp_mult = (c.indi_emp*10^6)/revenue,
         c.indu_emp_mult = (c.indu_emp*10^6)/revenue,
         c.dire_comp_mult = (c.dire_comp*10^6)/revenue,
         c.indi_comp_mult = (c.indi_comp*10^6)/revenue,
         c.indu_comp_mult = (c.indu_comp*10^6)/revenue)


### loop through extraction scenarios, import and bind (append) together 

#### Import all files sharing the filename ending "*.rds" 

myfiles = list.files(path=energy_model_output_extraction, pattern="*.rds", full.names=TRUE)

dat <- lapply(myfiles, readRDS)

# bind imported files together and create a numeric scenario indicator 
ext_output <- dat %>% 
  bind_rows() %>% 
  inner_join(scen_list,by=c("scen_id","oil_price_scenario","setback_scenario","prod_quota_scenario",
                            "carbon_price_scenario","ccs_scenario","innovation_scenario","excise_tax_scenario"))



## 1b. Refining 

setwd(energy_model_output_refining) 

ref_output <- read_csv('county_refining_outputs.csv')  

str(ref_output)






###############################################################################################################################

#2. Compute the share of county population that lives in a DAC census tract for each CA county 

## 2a. Import CalEnviroscreen 2018 update 

setwd(health_team_files) 

dac <- read_xlsx('ces3results.xlsx') 

str(dac)

### Population in each county living in a DAC census tract 

dac_pop <- dac %>% 
  filter(`SB 535 Disadvantaged Community`=="Yes") %>% 
  group_by(`California County`) %>% 
  summarize(dac_pop = sum(`Total Population`)) 

## 2b. Import 2010 CA county population 

setwd(population_files) 

pop2010 <- read_xlsx('co-est2019-annres-06.xlsx',range = "A4:B63") %>% 
  rename(county = `...1`, pop = Census) %>%
  mutate(county = str_remove_all(county," County, California"), 
         county = str_remove_all(county, "\\.")) %>% 
  filter(county != "California")

str(pop2010)


## 2c. join dac status file to 2010 county population file by county

merge_check <- anti_join(dac_pop,pop2010,by=c("California County"="county")) # should be 0 observations here 

dac_share <- left_join(pop2010,dac_pop,by=c("county"="California County")) %>% 
  mutate(dac_pop = ifelse(is.na(dac_pop)==T,0,dac_pop), 
         frac_dac = dac_pop/pop) %>% 
  dplyr::select(county,frac_dac)

summary(dac_share$frac_dac)


###############################################################################################################################

#3. Compute the share of total production and emissions from extraction in each county 

## 3a. Import historic production and emissions for each county (generated by Tracey)

setwd(energy_model_output_misc) 

prod_emissions <- read_csv('hist_extraction_prod_emis_x_county.csv')

str(prod_emissions)

### Compute share of statewide production and emissions in each county for 2015-18 

prod_emissions_state <- prod_emissions %>% 
  group_by() %>% 
  summarize(bbls_state = sum(total_bbls), MtCO2e_state = sum(total_MtCO2e))

prod_emissions_county <- prod_emissions %>% 
  group_by(county) %>% 
  summarize(bbls_county = sum(total_bbls), MtCO2e_county = sum(total_MtCO2e)) %>% 
  mutate(bbls_state = prod_emissions_state$bbls_state, MtCO2e_state = prod_emissions_state$MtCO2e_state,
         prod_share = bbls_county/bbls_state, emis_share = MtCO2e_county/MtCO2e_state) %>% 
  dplyr::select(county,prod_share,emis_share)

summary(prod_emissions_county$prod_share)
summary(prod_emissions_county$emis_share)


###############################################################################################################################

#4. Join DAC, production, and emissions shares to model output by county 

## 4a. Extraction 

merge_check1 <- anti_join(dac_share,ext_output,by="county") # okay if there are 41 unmatched counties here--extraction output only has 17 counties
merge_check2 <- anti_join(prod_emissions_county,ext_output,by="county") # 10 counties produce historically, but aren't in extraction output (all produce 0 historically)

ext_joined <- left_join(ext_output,dac_share, by="county") %>% 
  left_join(prod_emissions_county,by="county")


## 4b. Refining 

merge_check3 <- anti_join(dac_share,ref_output,by="county") # okay if there are 53 unmatched counties here--refining output only has 5 counties

ref_joined <- left_join(ref_output,dac_share, by="county")


###############################################################################################################################

#5. Output extraction and refining output to xlsx file with separate sheets for extraction and refining 

setwd(processed) 

sheet_list = list(extraction=ext_joined,refining = ref_joined)
write_xlsx(x=sheet_list,"labor_results.xlsx")


  
  
  
  
  








