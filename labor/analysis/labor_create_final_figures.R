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

############################################################################################ 

# Part A: Main results (county specific direct, indirect, induced) 

## 1. Import files with energy model output and multipliers (unit of observation: scen_id x county x year), compute direct, indirect, induced impacts 
setwd(processed)

ext_df <- read_xlsx('energy_model_output_with_multipliers.xlsx',sheet='extraction') %>% 
  mutate(dire_emp = (revenue/(10^6))*dire_emp_mult, indi_emp = (revenue/(10^6))*indi_emp_mult, indu_emp = (revenue/(10^6))*indu_emp_mult,
         dire_comp = (revenue/(10^6))*dire_comp_mult, indi_comp = (revenue/(10^6))*indi_comp_mult, indu_comp = (revenue/(10^6))*indu_comp_mult)

ref_df <- read_xlsx('energy_model_output_with_multipliers.xlsx',sheet='refining') %>% 
  mutate(dire_emp = (revenue/(10^6))*dire_emp_mult, indi_emp = (revenue/(10^6))*indi_emp_mult, indu_emp = (revenue/(10^6))*indu_emp_mult,
         dire_comp = (revenue/(10^6))*dire_comp_mult, indi_comp = (revenue/(10^6))*indi_comp_mult, indu_comp = (revenue/(10^6))*indu_comp_mult)


## 2. Create figures 

ext_for_fig1 <- ext_df %>% 
  mutate(emp_p25 = quantile(dire_emp_mult,0.25,na.rm=T))
