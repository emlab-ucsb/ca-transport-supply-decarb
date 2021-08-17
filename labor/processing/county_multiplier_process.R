# calepa-cn: Processing direct, indirect, and induced multipliers by county 
# Chris Malloy (cmalloy@ucsb.edu)
# created: 07/28/2021
# updated: 07/28/2021

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

#Chris' linux 
#ica_dollar_direct <- '~/gdrive/processed/implan-results/direct/ica/dollar' 
#ica_percent_direct <- '~/gdrive/processed/implan-results/direct/ica/percentage'
#impact_dollar_direct <- '~/gdrive/processed/implan-results/direct/impact'

############################################################################################ 

#1. Import ICA files, clean up names, and add county & segment identifiers 

##1a. Industry Contribution Analysis (ICA)

### NOTE: all multipliers are for $1 million of output value in an industry 

setwd(ica_dollar)

### Kern

#### extraction 

ica_emp_ext_kern <- read_csv('ica-emp-ext-kern.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_kern <- read_csv('ica-va-ext-kern.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_kern <- read_csv('ica-emp-drill-kern.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_kern <- read_csv('ica-va-drill-kern.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



#### refining 

ica_emp_ref_kern <- read_csv('ica-emp-ref-kern.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_kern <- read_csv('ica-va-ref-kern.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Los Angeles

#### extraction 

ica_emp_ext_la <- read_csv('ica-emp-ext-la.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_la <- read_csv('ica-va-ext-la.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_la <- read_csv('ica-emp-drill-la.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_la <- read_csv('ica-va-drill-la.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



#### refining 

ica_emp_ref_la <- read_csv('ica-emp-ref-la.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_la <- read_csv('ica-va-ref-la.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Santa Barbara

#### extraction 

ica_emp_ext_sb <- read_csv('ica-emp-ext-sb.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_sb <- read_csv('ica-va-ext-sb.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_sb <- read_csv('ica-emp-drill-sb.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_sb <- read_csv('ica-va-drill-sb.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



#### refining 

ica_emp_ref_sb <- read_csv('ica-emp-ref-sb.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_sb <- read_csv('ica-va-ref-sb.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Monterey

#### extraction 

ica_emp_ext_monterey <- read_csv('ica-emp-ext-monterey.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_monterey <- read_csv('ica-va-ext-monterey.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_monterey <- read_csv('ica-emp-drill-monterey.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_monterey <- read_csv('ica-va-drill-monterey.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



####################################################################################################################

## Ventura

#### extraction 

ica_emp_ext_ventura <- read_csv('ica-emp-ext-ventura.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_ventura <- read_csv('ica-va-ext-ventura.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_ventura <- read_csv('ica-emp-drill-ventura.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_ventura <- read_csv('ica-va-drill-ventura.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



####################################################################################################################

## Orange

#### extraction 

ica_emp_ext_orange <- read_csv('ica-emp-ext-orange.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_orange <- read_csv('ica-va-ext-orange.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_orange <- read_csv('ica-emp-drill-orange.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_orange <- read_csv('ica-va-drill-orange.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



####################################################################################################################

## Fresno

#### extraction 

ica_emp_ext_fresno <- read_csv('ica-emp-ext-fresno.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_fresno <- read_csv('ica-va-ext-fresno.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

ica_emp_drill_fresno <- read_csv('ica-emp-drill-fresno.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_fresno <- read_csv('ica-va-drill-fresno.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Contra Costa

#### refining 

ica_emp_ref_cc <- read_csv('ica-emp-ref-cc.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Contra Costa", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_cc <- read_csv('ica-va-ref-cc.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Contra Costa", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)




####################################################################################################################

## Solano

#### refining 

ica_emp_ref_solano <- read_csv('ica-emp-ref-solano.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Solano", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_solano <- read_csv('ica-va-ref-solano.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Solano", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

##1b. Impact Analysis 

### NOTE: all multipliers are for $1 million change in output value in an industry 

setwd(impact_dollar)

### Kern

#### extraction 

impact_emp_ext_kern <- read_csv('impact-emp-ext-kern.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_kern <- read_csv('impact-va-ext-kern.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_kern <- read_csv('impact-emp-drill-kern.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_kern <- read_csv('impact-va-drill-kern.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



#### refining 

impact_emp_ref_kern <- read_csv('impact-emp-ref-kern.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ref_kern <- read_csv('impact-va-ref-kern.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Los Angeles

#### extraction 

impact_emp_ext_la <- read_csv('impact-emp-ext-la.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_la <- read_csv('impact-va-ext-la.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_la <- read_csv('impact-emp-drill-la.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_la <- read_csv('impact-va-drill-la.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



#### refining 

impact_emp_ref_la <- read_csv('impact-emp-ref-la.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ref_la <- read_csv('impact-va-ref-la.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Los Angeles", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Santa Barbara

#### extraction 

impact_emp_ext_sb <- read_csv('impact-emp-ext-sb.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_sb <- read_csv('impact-va-ext-sb.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_sb <- read_csv('impact-emp-drill-sb.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_sb <- read_csv('impact-va-drill-sb.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



#### refining 

impact_emp_ref_sb <- read_csv('impact-emp-ref-sb.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ref_sb <- read_csv('impact-va-ref-sb.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Monterey

#### extraction 

impact_emp_ext_monterey <- read_csv('impact-emp-ext-monterey.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_monterey <- read_csv('impact-va-ext-monterey.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_monterey <- read_csv('impact-emp-drill-monterey.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_monterey <- read_csv('impact-va-drill-monterey.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Monterey", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



####################################################################################################################

## Ventura

#### extraction 

impact_emp_ext_ventura <- read_csv('impact-emp-ext-ventura.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_ventura <- read_csv('impact-va-ext-ventura.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_ventura <- read_csv('impact-emp-drill-ventura.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_ventura <- read_csv('impact-va-drill-ventura.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Ventura", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



####################################################################################################################

## Orange

#### extraction 

impact_emp_ext_orange <- read_csv('impact-emp-ext-orange.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_orange <- read_csv('impact-va-ext-orange.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_orange <- read_csv('impact-emp-drill-orange.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_orange <- read_csv('impact-va-drill-orange.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Orange", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)



####################################################################################################################

## Fresno

#### extraction 

impact_emp_ext_fresno <- read_csv('impact-emp-ext-fresno.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ext_fresno <- read_csv('impact-va-ext-fresno.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


#### drilling 

impact_emp_drill_fresno <- read_csv('impact-emp-drill-fresno.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_drill_fresno <- read_csv('impact-va-drill-fresno.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Fresno", segment = "drilling") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################

## Contra Costa

#### refining 

impact_emp_ref_cc <- read_csv('impact-emp-ref-cc.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Contra Costa", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ref_cc <- read_csv('impact-va-ref-cc.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Contra Costa", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)

impact_comp_ref_cc$direct_comp = as.numeric(str_remove_all(impact_comp_ref_cc$direct_comp,"[()$]")) 
impact_comp_ref_cc$indirect_comp = as.numeric(str_remove_all(impact_comp_ref_cc$indirect_comp,"[()$]")) 
impact_comp_ref_cc$induced_comp = as.numeric(str_remove_all(impact_comp_ref_cc$induced_comp,"[()$]")) 

impact_comp_ref_cc$direct_proprietor_income = as.numeric(str_remove_all(impact_comp_ref_cc$direct_proprietor_income,"[()$]")) 
impact_comp_ref_cc$indirect_proprietor_income = as.numeric(str_remove_all(impact_comp_ref_cc$indirect_proprietor_income,"[()$]")) 
impact_comp_ref_cc$induced_proprietor_income = as.numeric(str_remove_all(impact_comp_ref_cc$induced_proprietor_income,"[()$]")) 

impact_comp_ref_cc$direct_other_property_income = as.numeric(str_remove_all(impact_comp_ref_cc$direct_other_property_income,"[()$]")) 
impact_comp_ref_cc$indirect_other_property_income = as.numeric(str_remove_all(impact_comp_ref_cc$indirect_other_property_income,"[()$]")) 
impact_comp_ref_cc$induced_other_property_income = as.numeric(str_remove_all(impact_comp_ref_cc$induced_other_property_income,"[()$]")) 

impact_comp_ref_cc$direct_taxes_prod_imports = as.numeric(str_remove_all(impact_comp_ref_cc$direct_taxes_prod_imports,"[()$]")) 
impact_comp_ref_cc$indirect_taxes_prod_imports = as.numeric(str_remove_all(impact_comp_ref_cc$indirect_taxes_prod_imports,"[()$]")) 
impact_comp_ref_cc$induced_taxes_prod_imports = as.numeric(str_remove_all(impact_comp_ref_cc$induced_taxes_prod_imports,"[()$]")) 

impact_comp_ref_cc$direct_va = as.numeric(str_remove_all(impact_comp_ref_cc$direct_va,"[()$]")) 
impact_comp_ref_cc$indirect_va = as.numeric(str_remove_all(impact_comp_ref_cc$indirect_va,"[()$]")) 
impact_comp_ref_cc$induced_va = as.numeric(str_remove_all(impact_comp_ref_cc$induced_va,"[()$]")) 


impact_comp_ref_cc <- impact_comp_ref_cc %>% 
  mutate(direct_comp = as.numeric(direct_comp), indirect_comp = as.numeric(indirect_comp), induced_comp = as.numeric(induced_comp),
         direct_proprietor_income = as.numeric(direct_proprietor_income), indirect_proprietor_income = as.numeric(indirect_proprietor_income), induced_proprietor_income = as.numeric(induced_proprietor_income),
         direct_other_property_income = as.numeric(direct_other_property_income), indirect_other_property_income = as.numeric(indirect_other_property_income), induced_other_property_income = as.numeric(induced_other_property_income),
         direct_taxes_prod_imports = as.numeric(direct_taxes_prod_imports), indirect_taxes_prod_imports = as.numeric(indirect_taxes_prod_imports), induced_taxes_prod_imports = as.numeric(induced_taxes_prod_imports),
         direct_va = as.numeric(direct_va), indirect_va = as.numeric(indirect_va), induced_va = as.numeric(induced_va))


####################################################################################################################

## Solano

#### refining 

impact_emp_ref_solano <- read_csv('impact-emp-ref-solano.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Solano", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


impact_comp_ref_solano <- read_csv('impact-va-ref-solano.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Solano", segment = "refining") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation`, 
         direct_proprietor_income = `Proprietor Income`, direct_other_property_income = `Other Property Income`,
         direct_taxes_prod_imports = `Taxes on Production & Imports`, direct_va = `Value Added`,
         indirect_comp = `Employee Compensation_1`, 
         indirect_proprietor_income = `Proprietor Income_1`, indirect_other_property_income = `Other Property Income_1`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports_1`, indirect_va = `Value Added_1`,
         induced_comp = `Employee Compensation_2`, 
         induced_proprietor_income = `Proprietor Income_2`, induced_other_property_income = `Other Property Income_2`,
         induced_taxes_prod_imports = `Taxes on Production & Imports_2`, induced_va = `Value Added_2`) %>% 
  dplyr::select(-`Employee Compensation_3`, -`Proprietor Income_3`, -`Other Property Income_3`,
                -`Taxes on Production & Imports_3`, -`Value Added_3`,-X1)


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#2. Bind data frames together for ICA and impact 

## ica 

ica_comp <- bind_rows(ica_comp_ref_cc,ica_comp_ref_solano,ica_comp_ref_la,ica_comp_ref_kern,ica_comp_ref_sb,
                 ica_comp_drill_fresno,ica_comp_drill_kern,ica_comp_drill_la,ica_comp_drill_monterey,
                 ica_comp_drill_orange,ica_comp_drill_sb,ica_comp_drill_ventura,
                 ica_comp_ext_fresno,ica_comp_ext_kern,ica_comp_ext_la,ica_comp_ext_monterey,
                 ica_comp_ext_orange,ica_comp_ext_sb,ica_comp_ext_ventura)

ica_emp <- bind_rows(ica_emp_ref_cc,ica_emp_ref_solano,ica_emp_ref_la,ica_emp_ref_kern,ica_emp_ref_sb,
                      ica_emp_drill_fresno,ica_emp_drill_kern,ica_emp_drill_la,ica_emp_drill_monterey,
                      ica_emp_drill_orange,ica_emp_drill_sb,ica_emp_drill_ventura,
                     ica_emp_ext_fresno,ica_emp_ext_kern,ica_emp_ext_la,ica_emp_ext_monterey,
                      ica_emp_ext_orange,ica_emp_ext_sb,ica_emp_ext_ventura) %>% 
  rename(direct_emp = `1 - Direct`,
         indirect_emp = `2 - Indirect`, induced_emp = `3 - Induced`) %>% 
  mutate(direct_emp = as.numeric(direct_emp), indirect_emp = as.numeric(indirect_emp), 
         induced_emp = as.numeric(induced_emp))

ica <- inner_join(ica_emp,ica_comp,by=c("county","segment","industry")) 


## impact 

impact_comp <- bind_rows(impact_comp_ref_cc,impact_comp_ref_solano,impact_comp_ref_la,impact_comp_ref_kern,impact_comp_ref_sb,
                      impact_comp_drill_fresno,impact_comp_drill_kern,impact_comp_drill_la,
                      impact_comp_drill_orange,impact_comp_drill_sb,impact_comp_drill_ventura,
                      impact_comp_ext_fresno,impact_comp_ext_kern,impact_comp_ext_la,impact_comp_ext_monterey,
                      impact_comp_ext_orange,impact_comp_ext_sb,impact_comp_ext_ventura)

impact_emp <- bind_rows(impact_emp_ref_cc,impact_emp_ref_solano,impact_emp_ref_la,impact_emp_ref_kern,impact_emp_ref_sb,
                     impact_emp_drill_fresno,impact_emp_drill_kern,impact_emp_drill_la,impact_emp_drill_monterey,
                     impact_emp_drill_orange,impact_emp_drill_sb,impact_emp_drill_ventura,
                     impact_emp_ext_fresno,impact_emp_ext_kern,impact_emp_ext_la,impact_emp_ext_monterey,
                     impact_emp_ext_orange,impact_emp_ext_sb,impact_emp_ext_ventura) %>% 
  rename(direct_emp = `1 - Direct`,
         indirect_emp = `2 - Indirect`, induced_emp = `3 - Induced`) %>% 
  mutate(direct_emp = as.numeric(direct_emp), indirect_emp = as.numeric(indirect_emp), 
         induced_emp = as.numeric(induced_emp))

impact <- inner_join(impact_emp,impact_comp,by=c("county","segment","industry"))



####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#3. Separate the direct, indirect, and induced multipliers into different dfs, reshape each to wide format

## ICA 

ica_emp_wide <- ica_emp %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_emp","indirect_emp","induced_emp"))

ica_emp_wide_direct <- ica_emp %>% 
  dplyr::select(-indirect_emp,-induced_emp) %>% 
  filter(direct_emp != 0) %>% 
  arrange(county,segment,-direct_emp)
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_emp")) 


ica_emp_wide_indirect <- ica_emp %>% 
  dplyr::select(-direct_emp,-induced_emp) %>% 
  filter(indirect_emp != 0) %>% 
  group_by(county,segment)  %>% 
  arrange(county,segment,-indirect_emp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("indirect_emp"))


ica_emp_wide_induced <- ica_emp %>% 
  dplyr::select(-direct_emp,-indirect_emp) %>% 
  filter(induced_emp != 0) %>% 
  group_by(county,segment)  %>% 
  arrange(county,segment,-induced_emp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("induced_emp")) 


### Compaensation 

ica_comp_wide <- ica_comp %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_comp","indirect_comp","induced_comp"))

ica_comp_wide_direct <- ica_comp %>% 
  dplyr::select(county,segment,industry,direct_comp) %>% 
  filter(direct_comp != 0) %>% 
  arrange(county,segment,-direct_comp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_comp")) 

ica_comp_wide_indirect <- ica_comp %>% 
  dplyr::select(county,segment,industry,indirect_comp) %>% 
  filter(indirect_comp != 0) %>% 
  group_by(county,segment)%>% 
  arrange(county,segment,-indirect_comp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("indirect_comp")) 


ica_comp_wide_induced <- ica_comp %>% 
  dplyr::select(county,segment,industry,induced_comp) %>% 
  filter(induced_comp != 0) %>% 
  group_by(county,segment) %>% 
  arrange(county,segment,-induced_comp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("induced_comp")) 
#######################################################################################################

## Impact 

impact_emp_wide <- impact_emp %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_emp","indirect_emp","induced_emp"))

impact_emp_wide_direct <- impact_emp %>% 
  dplyr::select(-indirect_emp,-induced_emp) %>% 
  filter(direct_emp != 0) %>% 
  arrange(county,segment,-direct_emp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_emp")) 


impact_emp_wide_indirect <- impact_emp %>% 
  dplyr::select(-direct_emp,-induced_emp) %>% 
  filter(indirect_emp != 0) %>% 
  group_by(county,segment) %>% 
  arrange(county,segment,-indirect_emp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("indirect_emp")) 


impact_emp_wide_induced <- impact_emp %>% 
  dplyr::select(-direct_emp,-indirect_emp) %>% 
  filter(induced_emp != 0) %>% 
  group_by(county,segment)  %>% 
  arrange(county,segment,-induced_emp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("induced_emp"))


### Compaensation 

impact_comp_wide <- impact_comp %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_comp","indirect_comp","induced_comp"))

impact_comp_wide_direct <- impact_comp %>% 
  dplyr::select(county,segment,industry,direct_comp) %>% 
  filter(direct_comp != 0) %>% 
  arrange(county,segment,-direct_comp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("direct_comp")) 


impact_comp_wide_indirect <- impact_comp %>% 
  dplyr::select(county,segment,industry,indirect_comp) %>% 
  filter(indirect_comp != 0) %>% 
  group_by(county,segment) %>% 
  arrange(county,segment,-indirect_comp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("indirect_comp")) 


impact_comp_wide_induced <- impact_comp %>% 
  dplyr::select(county,segment,industry,induced_comp) %>% 
  filter(induced_comp != 0) %>% 
  group_by(county,segment) %>% 
  arrange(county,segment,-induced_comp) %>% 
  pivot_wider(id_cols = c("county","segment","industry"), names_from = "industry", 
              values_from = c("induced_comp")) 

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#3. Create dfs with total direct, indirect, induced multipliers by county and segment 

ica_total <- ica %>% 
  group_by(county,segment) %>% 
  summarize(direct_emp = sum(direct_emp, na.rm = T), indirect_emp = sum(indirect_emp, na.rm = T),
            induced_emp = sum(induced_emp, na.rm = T), direct_comp = sum(direct_comp, na.rm = T), indirect_comp = sum(indirect_comp, na.rm = T),
            induced_comp = sum(induced_comp, na.rm = T))

impact_total <- impact %>% 
  group_by(county,segment) %>% 
  summarize(direct_emp = sum(direct_emp, na.rm = T), indirect_emp = sum(indirect_emp, na.rm = T),
            induced_emp = sum(induced_emp, na.rm = T), direct_comp = sum(direct_comp, na.rm = T), indirect_comp = sum(indirect_comp, na.rm = T),
            induced_comp = sum(induced_comp, na.rm = T))



####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#4. Add in counties without O&G sector activity 

county_shape <- counties(year = 2010,state = "California")

county_df <- dplyr::select(county_shape,NAME10) 

st_geometry(county_df) <- NULL

ica_total_all_counties <- left_join(county_df,ica_total,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_total_all_counties[is.na(ica_total_all_counties)] <- 0 
ica_total_all_counties <- mutate(ica_total_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################

impact_total_all_counties <- left_join(county_df,impact_total,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_total_all_counties[is.na(impact_total_all_counties)] <- 0 
impact_total_all_counties <- mutate(impact_total_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################

ica_emp_direct_all_counties <- left_join(county_df,ica_emp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_emp_direct_all_counties[is.na(ica_emp_direct_all_counties)] <- 0 
ica_emp_direct_all_counties <- mutate(ica_emp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_emp_indirect_all_counties <- left_join(county_df,ica_emp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_emp_indirect_all_counties[is.na(ica_emp_indirect_all_counties)] <- 0 
ica_emp_indirect_all_counties <- mutate(ica_emp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_emp_induced_all_counties <- left_join(county_df,ica_emp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_emp_induced_all_counties[is.na(ica_emp_induced_all_counties)] <- 0 
ica_emp_induced_all_counties <- mutate(ica_emp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################

ica_comp_direct_all_counties <- left_join(county_df,ica_comp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_comp_direct_all_counties[is.na(ica_comp_direct_all_counties)] <- 0 
ica_comp_direct_all_counties <- mutate(ica_comp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_comp_indirect_all_counties <- left_join(county_df,ica_comp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_comp_indirect_all_counties[is.na(ica_comp_indirect_all_counties)] <- 0 
ica_comp_indirect_all_counties <- mutate(ica_comp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_comp_induced_all_counties <- left_join(county_df,ica_comp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

ica_comp_induced_all_counties[is.na(ica_comp_induced_all_counties)] <- 0 
ica_comp_induced_all_counties <- mutate(ica_comp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################

impact_emp_direct_all_counties <- left_join(county_df,impact_emp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_emp_direct_all_counties[is.na(impact_emp_direct_all_counties)] <- 0 
impact_emp_direct_all_counties <- mutate(impact_emp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


impact_emp_indirect_all_counties <- left_join(county_df,impact_emp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_emp_indirect_all_counties[is.na(impact_emp_indirect_all_counties)] <- 0 
impact_emp_indirect_all_counties <- mutate(impact_emp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


impact_emp_induced_all_counties <- left_join(county_df,impact_emp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_emp_induced_all_counties[is.na(impact_emp_induced_all_counties)] <- 0 
impact_emp_induced_all_counties <- mutate(impact_emp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################

impact_comp_direct_all_counties <- left_join(county_df,impact_comp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_comp_direct_all_counties[is.na(impact_comp_direct_all_counties)] <- 0 
impact_comp_direct_all_counties <- mutate(impact_comp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


impact_comp_indirect_all_counties <- left_join(county_df,impact_comp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_comp_indirect_all_counties[is.na(impact_comp_indirect_all_counties)] <- 0 
impact_comp_indirect_all_counties <- mutate(impact_comp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


impact_comp_induced_all_counties <- left_join(county_df,impact_comp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)

impact_comp_induced_all_counties[is.na(impact_comp_induced_all_counties)] <- 0 
impact_comp_induced_all_counties <- mutate(impact_comp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#5. Read in statewide multipliers and add to the files 

setwd(statewide_processed)

ica_emp_direct_statewide <- read_xlsx('employment-direct-statewide.xlsx',sheet='ca_emp_dollar') %>% 
  mutate(m1.213111 = m1.213111*7, m1.211111 = m1.211111*7, m1.324110 = m1.324110*4) %>% 
  pivot_longer(c("m1.213111","m1.211111","m1.324110"),names_to = "industry", values_to = "emp_direct") %>% 
  mutate(industry = ifelse(industry=="m1.213111","drilling",
                           ifelse(industry=="m1.211111","extraction",
                                  ifelse(industry=="m1.324110","refining",NA)))) 

ica_emp_indirect_statewide_wide <- read_xlsx('employment-indirect-statewide.xlsx',sheet='ica_dollar_emp') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "emp_indirect",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                           ifelse(segment=="211111","extraction",
                                  ifelse(segment=="324110","refining",NA))), 
         emp_indirect = ifelse(segment=="drilling",emp_indirect*7,
                               ifelse(segment=="extraction",emp_indirect*7,
                                      ifelse(segment=="refining",emp_indirect*4,NA))),
         emp_indirect = -1*emp_indirect,
         county = "Statewide") %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = emp_indirect) 


ica_emp_induced_statewide_wide <- read_xlsx('employment-induced-statewide.xlsx',sheet='ica_dollar_emp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "emp_induced",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         emp_induced = ifelse(segment=="drilling",emp_induced*7,
                               ifelse(segment=="extraction",emp_induced*7,
                                      ifelse(segment=="refining",emp_induced*4,NA))),
         emp_induced = -1*emp_induced,
         county = "Statewide") %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = emp_induced) 



ica_indirect_emp_statewide_long <- read_xlsx('employment-indirect-statewide.xlsx',sheet='ica_dollar_emp') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "indirect_emp",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         indirect_emp = ifelse(segment=="drilling",indirect_emp*7,
                               ifelse(segment=="extraction",indirect_emp*7,
                                      ifelse(segment=="refining",indirect_emp*4,NA))),
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(indirect_emp = sum(indirect_emp,na.rm = T))

ica_induced_emp_statewide_long <- read_xlsx('employment-induced-statewide.xlsx',sheet='ica_dollar_emp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "induced_emp",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         induced_emp = ifelse(segment=="drilling",induced_emp*7,
                              ifelse(segment=="extraction",induced_emp*7,
                                     ifelse(segment=="refining",induced_emp*4,NA))),
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(induced_emp = sum(induced_emp,na.rm = T))


#################################################################################################################

# ica compensation

ica_comp_direct_statewide <- read_xlsx('comp-direct-statewide.xlsx',sheet='ca_comp_dollar') %>% 
  mutate(m1.213111 = as.numeric(str_remove_all(m1.213111,"[$,]")), m1.211111 = as.numeric(str_remove_all(m1.211111,"[$,]")), m1.324110 = as.numeric(str_remove_all(m1.324110,"[$,]")),
    m1.213111 = m1.213111*7, m1.211111 = m1.211111*7, m1.324110 = m1.324110*4) %>% 
  pivot_longer(c("m1.213111","m1.211111","m1.324110"),names_to = "segment", values_to = "comp_direct") %>% 
  mutate(segment = ifelse(segment=="m1.213111","drilling",
                           ifelse(segment=="m1.211111","extraction",
                                  ifelse(segment=="m1.324110","refining",NA)))) 

  ica_comp_indirect_statewide_ext <- read_xlsx('comp-indirect-statewide.xlsx',sheet='ica_dollar_comp') %>% 
  rename(segment = industry) %>% 
  filter(segment == 211111) %>% 
  pivot_longer(cols = starts_with("compensation."),names_to = "industry", values_to = "comp_indirect",
               names_prefix = "compensation.") %>% 
  pivot_wider(id_cols = c("segment"),names_from = "industry",values_from = comp_indirect,names_prefix = "indirect.")
  
ica_comp_indirect_statewide_wide <- read_xlsx('comp-indirect-statewide.xlsx',sheet='ica_dollar_comp') %>% 
  rename(segment = industry) %>% 
  filter(segment==213111 | segment==324110) %>% 
  dplyr::select(-starts_with("compensation.")) %>%
  bind_rows(ica_comp_indirect_statewide_ext) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "comp_indirect",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         comp_indirect = ifelse(segment=="drilling",comp_indirect*7,
                               ifelse(segment=="extraction",comp_indirect*7,
                                      ifelse(segment=="refining",comp_indirect*4,NA))),
         comp_indirect = -1*comp_indirect,
         county = "Statewide") %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = comp_indirect) 


ica_comp_induced_statewide_wide <- read_xlsx('comp-induced-statewide.xlsx',sheet='ica_dollar_comp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "comp_induced",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         comp_induced = ifelse(segment=="drilling",comp_induced*7,
                              ifelse(segment=="extraction",comp_induced*7,
                                     ifelse(segment=="refining",comp_induced*4,NA))),
         comp_induced = -1*comp_induced,
         county = "Statewide") %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = comp_induced) 



ica_indirect_comp_statewide_long <- read_xlsx('comp-indirect-statewide.xlsx',sheet='ica_dollar_comp') %>% 
  rename(segment = industry) %>% 
  filter(segment==213111 | segment==324110) %>% 
  dplyr::select(-starts_with("compensation.")) %>%
  bind_rows(ica_comp_indirect_statewide_ext) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "indirect_comp",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         comp_indirect = ifelse(segment=="drilling",indirect_comp*7,
                                ifelse(segment=="extraction",indirect_comp*7,
                                       ifelse(segment=="refining",indirect_comp*4,NA))),
         comp_indirect = -1*comp_indirect,
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(indirect_comp = sum(comp_indirect,na.rm = T))

ica_induced_comp_statewide_long <- read_xlsx('comp-induced-statewide.xlsx',sheet='ica_dollar_comp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "induced_comp",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         induced_comp = ifelse(segment=="drilling",induced_comp*7,
                               ifelse(segment=="extraction",induced_comp*7,
                                      ifelse(segment=="refining",induced_comp*4,NA))),
         induced_comp = -1*induced_comp,
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(induced_comp = sum(induced_comp,na.rm = T))



##################################################################################################################
# impact statewide 

impact_emp_direct_statewide <- read_xlsx('employment-direct-statewide.xlsx',sheet='ca_emp_impace') %>% 
  mutate(m1.213111 = as.numeric(str_remove_all(m1.213111,"[()$,]")), m1.211111 = as.numeric(str_remove_all(m1.211111,"[()$,-]")), m1.324110 = as.numeric(str_remove_all(m1.324110,"[()$,-]")),
         m1.213111 = m1.213111*7, m1.211111 = m1.211111*7, m1.324110 = m1.324110*4) %>% 
  pivot_longer(c("m1.213111","m1.211111","m1.324110"),names_to = "segment", values_to = "emp_direct") %>% 
  mutate(segment = ifelse(segment=="m1.213111","drilling",
                           ifelse(segment=="m1.211111","extraction",
                                  ifelse(segment=="m1.324110","refining",NA)))) 


impact_emp_indirect_statewide_wide <- read_xlsx('employment-indirect-statewide.xlsx',sheet='impact_dollar_emp') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "emp_indirect",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         emp_indirect = ifelse(segment=="drilling",emp_indirect*7,
                               ifelse(segment=="extraction",emp_indirect*7,
                                      ifelse(segment=="refining",emp_indirect*4,NA))),
         emp_indirect = -1*emp_indirect,
         county = "Statewide") %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = emp_indirect) 


impact_emp_induced_statewide_wide <- read_xlsx('employment-induced-statewide.xlsx',sheet='impact_dollar_emp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "emp_induced",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         emp_induced = ifelse(segment=="drilling",emp_induced*7,
                              ifelse(segment=="extraction",emp_induced*7,
                                     ifelse(segment=="refining",emp_induced*4,NA))),
         emp_induced = -1*emp_induced,
         county = "Statewide") %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = emp_induced) 



impact_emp_indirect_statewide_long <- read_xlsx('employment-indirect-statewide.xlsx',sheet='impact_dollar_emp') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "indirect_emp",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         indirect_emp = ifelse(segment=="drilling",indirect_emp*7,
                               ifelse(segment=="extraction",indirect_emp*7,
                                      ifelse(segment=="refining",indirect_emp*4,NA))),
         indirect_emp = -1*indirect_emp,
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(indirect_emp = sum(indirect_emp,na.rm = T))

impact_emp_induced_statewide_long <- read_xlsx('employment-induced-statewide.xlsx',sheet='impact_dollar_emp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "induced_emp",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         induced_emp = ifelse(segment=="drilling",induced_emp*7,
                              ifelse(segment=="extraction",induced_emp*7,
                                     ifelse(segment=="refining",induced_emp*4,NA))),
         induced_emp = -1*induced_emp,
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(induced_emp = sum(induced_emp,na.rm = T))


#################################################################################################################

# ica compensation

impact_comp_direct_statewide <- read_xlsx('comp-direct-statewide.xlsx',sheet='ca_comp_dollar') %>% 
  mutate(m1.213111 = as.numeric(str_remove_all(m1.213111,"[$,]")), m1.211111 = as.numeric(str_remove_all(m1.211111,"[$,]")), m1.324110 = as.numeric(str_remove_all(m1.324110,"[$,]")),
         m1.213111 = m1.213111*7, m1.211111 = m1.211111*7, m1.324110 = m1.324110*4) %>% 
  pivot_longer(c("m1.213111","m1.211111","m1.324110"),names_to = "segment", values_to = "comp_direct") %>% 
  mutate(segment = ifelse(segment=="m1.213111","drilling",
                           ifelse(segment=="m1.211111","extraction",
                                  ifelse(segment=="m1.324110","refining",NA)))) 

impact_comp_indirect_statewide_ext <- read_xlsx('comp-indirect-statewide.xlsx',sheet='impact_dollar_comp') %>% 
  rename(segment = industry) %>% 
  filter(segment == 211111) %>% 
  pivot_longer(cols = starts_with("compensation."),names_to = "industry", values_to = "comp_indirect",
               names_prefix = "compensation.") %>% 
  pivot_wider(id_cols = c("segment"),names_from = "industry",values_from = comp_indirect,names_prefix = "indirect.")

impact_comp_indirect_statewide_wide <- read_xlsx('comp-indirect-statewide.xlsx',sheet='impact_dollar_comp') %>% 
  rename(segment = industry) %>% 
  filter(segment==213111 | segment==324110) %>% 
  dplyr::select(-starts_with("compensation.")) %>%
  bind_rows(impact_comp_indirect_statewide_ext) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "comp_indirect",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         comp_indirect = ifelse(segment=="drilling",comp_indirect*7,
                                ifelse(segment=="extraction",comp_indirect*7,
                                       ifelse(segment=="refining",comp_indirect*4,NA))),
         comp_indirect = -1*comp_indirect,
         county = "Statewide") %>% 
  arrange(county,segment,-comp_indirect) %>%
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = comp_indirect) 


impact_comp_induced_statewide_wide <- read_xlsx('comp-induced-statewide.xlsx',sheet='impact_dollar_comp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "comp_induced",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         comp_induced = ifelse(segment=="drilling",comp_induced*7,
                               ifelse(segment=="extraction",comp_induced*7,
                                      ifelse(segment=="refining",comp_induced*4,NA))),
         comp_induced = -1*comp_induced,
         county = "Statewide") %>% 
  arrange(county,segment,-comp_induced) %>% 
  pivot_wider(id_cols = c("county","segment"),names_from = "industry",values_from = comp_induced) 



impact_indirect_comp_statewide_long <- read_xlsx('comp-indirect-statewide.xlsx',sheet='impact_dollar_comp') %>% 
  rename(segment = industry) %>% 
  filter(segment==213111 | segment==324110) %>% 
  dplyr::select(-starts_with("compensation.")) %>%
  bind_rows(impact_comp_indirect_statewide_ext) %>%
  pivot_longer(cols = starts_with("indirect."),names_to = "industry", values_to = "indirect_comp",
               names_prefix = "indirect.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         comp_indirect = ifelse(segment=="drilling",indirect_comp*7,
                                ifelse(segment=="extraction",indirect_comp*7,
                                       ifelse(segment=="refining",indirect_comp*4,NA))),
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(indirect_comp = sum(comp_indirect,na.rm = T))

impact_induced_comp_statewide_long <- read_xlsx('comp-induced-statewide.xlsx',sheet='impact_dollar_comp_induced') %>% 
  rename(segment = industry) %>%
  pivot_longer(cols = starts_with("induced."),names_to = "industry", values_to = "induced_comp",
               names_prefix = "induced.") %>% 
  mutate(segment = ifelse(segment=="213111","drilling",
                          ifelse(segment=="211111","extraction",
                                 ifelse(segment=="324110","refining",NA))), 
         induced_comp = ifelse(segment=="drilling",induced_comp*7,
                               ifelse(segment=="extraction",induced_comp*7,
                                      ifelse(segment=="refining",induced_comp*4,NA))),
         county = "Statewide") %>% 
  group_by(county,segment) %>% 
  summarize(induced_comp = sum(induced_comp,na.rm = T))

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#6. add statewide multipliers to county dfs 

##6a. ICA total impact with all counties and statewide 

ica_total_state <- ica_emp_direct_statewide %>% 
  rename(segment = industry) %>% 
  inner_join(ica_indirect_emp_statewide_long,by=c("county","segment")) %>% 
  inner_join(ica_induced_emp_statewide_long,by=c("county","segment")) %>% 
  inner_join(ica_comp_direct_statewide,by=c("county","segment")) %>% 
  inner_join(ica_indirect_comp_statewide_long,by=c("county","segment"))  %>% 
  inner_join(ica_induced_comp_statewide_long,by=c("county","segment")) %>% 
  rename(direct_emp = emp_direct, direct_comp = comp_direct)

ica_total_final <- bind_rows(ica_total_all_counties,ica_total_state)


impact_total_state <- impact_emp_direct_statewide %>% 
  inner_join(impact_emp_indirect_statewide_long,by=c("county","segment")) %>% 
  inner_join(impact_emp_induced_statewide_long,by=c("county","segment")) %>% 
  inner_join(impact_comp_direct_statewide,by=c("county","segment")) %>% 
  inner_join(impact_indirect_comp_statewide_long,by=c("county","segment"))  %>% 
  inner_join(impact_induced_comp_statewide_long,by=c("county","segment")) %>% 
  rename(direct_emp = emp_direct, direct_comp = comp_direct)

impact_total_final <- bind_rows(impact_total_all_counties,impact_total_state)

##6a. ICA indirect and induced impact by industry with all counties and statewide 

ica_indirect_emp_final <- bind_rows(ica_emp_indirect_all_counties,ica_emp_indirect_statewide_wide)

ica_indirect_comp_final <- bind_rows(ica_comp_indirect_all_counties, ica_comp_indirect_all_counties,ica_comp_indirect_statewide_wide)


ica_induced_emp_final <- bind_rows(ica_emp_induced_all_counties,ica_emp_induced_statewide_wide)

ica_induced_comp_final <- bind_rows(ica_comp_induced_all_counties, ica_comp_induced_all_counties,ica_comp_induced_statewide_wide)


##6a. Impact indirect and induced impact by industry with all counties and statewide 

impact_indirect_emp_final <- bind_rows(impact_emp_indirect_all_counties,impact_emp_indirect_statewide_wide)

impact_indirect_comp_final <- bind_rows(impact_comp_indirect_all_counties, impact_comp_indirect_all_counties,impact_comp_indirect_statewide_wide)


impact_induced_emp_final <- bind_rows(impact_emp_induced_all_counties,impact_emp_induced_statewide_wide)

impact_induced_comp_final <- bind_rows(impact_comp_induced_all_counties, impact_comp_induced_all_counties,impact_comp_induced_statewide_wide)


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#7. export as xlsx 

setwd(processed)

##7a. ICA file 

ica_list = list(ica_total_final=ica_total_final,ica_indirect_emp_final = ica_indirect_emp_final,
                ica_induced_emp_final = ica_induced_emp_final, ica_indirect_comp_final = ica_indirect_comp_final,
                ica_induced_comp_final = ica_induced_comp_final)
write_xlsx(x=ica_list,"ica_multipliers.xlsx")


##7b. Impact file 

impact_list = list(impact_total_final=impact_total_final,impact_indirect_emp_final = impact_indirect_emp_final,
                impact_induced_emp_final = impact_induced_emp_final, impact_indirect_comp_final = impact_indirect_comp_final,
                impact_induced_comp_final = impact_induced_comp_final)
write_xlsx(x=impact_list,"impact_multipliers.xlsx")







