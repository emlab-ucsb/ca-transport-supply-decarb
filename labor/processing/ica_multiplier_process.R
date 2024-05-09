# calepa-cn: Processing direct, indirect, and induced multipliers by county 
# Chris Malloy (cmalloy@ucsb.edu)
# created: 07/28/2021
# updated: 08/17/2021
# updated : 02/19/2024

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
library(dplyr)

#Set wd 
setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()

############################################################################################ 

#Read in spreadsheet for FTE conversion 


# UPDATED - MG 2/19/2024
fte_convert <- read_xlsx('data/inputs/labor/fte-convert.xlsx')%>% 
  dplyr::rename(fte_per_job = FTEperTotalEmp, ind_code = Implan546Index) %>% 
  dplyr::select(ind_code,fte_per_job)


#1. Import ICA files, clean up names, and add county & segment identifiers 

##1a. Industry Contribution Analysis (ICA)

### NOTE: all multipliers are for $1 million of output value in an industry 

setwd('/capstone/freshcair/meds-freshcair-capstone/data/inputs/labor/ica')

### Kern

#### extraction 


# UPDATED - MG - 2/19/2024
# UPDATE - not sure where ICA data is located
ica_emp_ext_kern <- read_csv('ica-emp-ext-kern.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_kern <- read_csv('ica-va-ext-kern.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
         direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...15`,
         direct_taxes_prod_imports = `Taxes on Production & Imports...16`, direct_va = `Value Added...17`,
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

# UPDATED - MG - 2/19/2024
ica_comp_drill_kern <- read_csv('ica-va-drill-kern.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
         direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
         direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
         indirect_comp = `Employee Compensation...8`, 
         indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
         induced_comp = `Employee Compensation...13`, 
         induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
         induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


# UPDATED - MG - 2/19/2024

#### refining 

ica_emp_ref_kern <- read_csv('ica-emp-ref-kern.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_ref_kern <- read_csv('ica-va-ref-kern.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
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

#### extraction 

ica_emp_ext_cc <- read_csv('ica-emp-ext-cc.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Contra Costa", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_cc <- read_csv('ica-va-ext-cc.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Contra Costa", segment = "extraction") %>% 
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
## San Luis Obispo

#### extraction 

ica_emp_ext_slo <- read_csv('ica-emp-ext-slo.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_slo <- read_csv('ica-va-ext-slo.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "extraction") %>% 
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

ica_emp_ref_slo <- read_csv('ica-emp-ref-slo.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_slo <- read_csv('ica-va-ref-slo.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "refining") %>% 
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
## San Benito

#### extraction 

ica_emp_ext_sanbenito <- read_csv('ica-emp-ext-sanbenito.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Benito", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_sanbenito <- read_csv('ica-va-ext-sanbenito.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Benito", segment = "extraction") %>% 
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
## San Bernardino

#### extraction 

ica_emp_ext_sanbernardino <- read_csv('ica-emp-ext-sanbernardino.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Bernardino", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_sanbernardino <- read_csv('ica-va-ext-sanbernardino.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Bernardino", segment = "extraction") %>% 
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
## Tulare

#### extraction 

ica_emp_ext_tulare <- read_csv('ica-emp-ext-tulare.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Tulare", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_tulare <- read_csv('ica-va-ext-tulare.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Tulare", segment = "extraction") %>% 
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
## San Mateo

#### extraction 

ica_emp_ext_sanmateo <- read_csv('ica-emp-ext-sanmateo.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Mateo", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_sanmateo <- read_csv('ica-va-ext-sanmateo.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "San Mateo", segment = "extraction") %>% 
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
## Kings

#### extraction 

ica_emp_ext_kings <- read_csv('ica-emp-ext-kings.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kings", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_kings <- read_csv('ica-va-ext-kings.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Kings", segment = "extraction") %>% 
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
## Alameda

#### extraction 

ica_emp_ext_alameda <- read_csv('ica-emp-ext-alameda.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Alameda", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_alameda <- read_csv('ica-va-ext-alameda.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Alameda", segment = "extraction") %>% 
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
## Riverside

#### extraction 

ica_emp_ext_riverside <- read_csv('ica-emp-ext-riverside.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Riverside", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_riverside <- read_csv('ica-va-ext-riverside.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Riverside", segment = "extraction") %>% 
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
## Santa Clara

#### extraction 

ica_emp_ext_santaclara <- read_csv('ica-emp-ext-santaclara.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Clara", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_santaclara <- read_csv('ica-va-ext-santaclara.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Santa Clara", segment = "extraction") %>% 
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
## Statewide 

#### extraction 

ica_emp_ext_statewide <- read_csv('ica-emp-ext-statewide.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Statewide", segment = "extraction") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ext_statewide <- read_csv('ica-va-ext-statewide.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Statewide", segment = "extraction") %>% 
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

ica_emp_drill_statewide <- read_csv('ica-emp-drill-statewide.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Statewide", segment = "drilling") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_drill_statewide <- read_csv('ica-va-drill-statewide.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Statewide", segment = "drilling") %>% 
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

ica_emp_ref_statewide <- read_csv('ica-emp-ref-statewide.csv') %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Statewide", segment = "refining") %>% 
  rename(industry = Impact) %>% 
  select(-X1,-X6) 


ica_comp_ref_statewide <- read_csv('ica-va-ref-statewide.csv',skip = 1) %>% 
  filter(is.na(X1)==F) %>% 
  mutate(county = "Statewide", segment = "refining") %>% 
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



#2. Bind data frames together for ICA and impact

## ica 

ica_comp <- bind_rows(ica_comp_ref_cc,ica_comp_ref_solano,ica_comp_ref_la,ica_comp_ref_kern,ica_comp_ref_sb,ica_comp_ref_slo,ica_comp_ref_statewide,
                      ica_comp_drill_fresno,ica_comp_drill_kern,ica_comp_drill_la,ica_comp_drill_monterey,
                      ica_comp_drill_orange,ica_comp_drill_sb,ica_comp_drill_ventura,ica_comp_drill_statewide,
                      ica_comp_ext_fresno,ica_comp_ext_kern,ica_comp_ext_la,ica_comp_ext_monterey,
                      ica_comp_ext_orange,ica_comp_ext_sb,ica_comp_ext_ventura,ica_comp_ext_slo,ica_comp_ext_sanbenito,ica_comp_ext_cc,
                      ica_comp_ext_sanbernardino,ica_comp_ext_tulare,ica_comp_ext_sanmateo,ica_comp_ext_kings,ica_comp_ext_alameda,
                      ica_comp_ext_riverside,ica_comp_ext_santaclara,ica_comp_ext_statewide)


ica_emp <- bind_rows(ica_emp_ref_cc,ica_emp_ref_solano,ica_emp_ref_la,ica_emp_ref_kern,ica_emp_ref_sb,ica_emp_ref_slo,ica_emp_ref_statewide,
                     ica_emp_drill_fresno,ica_emp_drill_kern,ica_emp_drill_la,ica_emp_drill_monterey,
                     ica_emp_drill_orange,ica_emp_drill_sb,ica_emp_drill_ventura,ica_emp_drill_statewide,
                     ica_emp_ext_fresno,ica_emp_ext_kern,ica_emp_ext_la,ica_emp_ext_monterey,
                     ica_emp_ext_orange,ica_emp_ext_sb,ica_emp_ext_ventura,ica_emp_ext_slo,ica_emp_ext_sanbenito,ica_emp_ext_cc,
                     ica_emp_ext_sanbernardino,ica_emp_ext_tulare,ica_emp_ext_sanmateo,ica_emp_ext_kings,ica_emp_ext_alameda,
                     ica_emp_ext_riverside,ica_emp_ext_santaclara,ica_emp_ext_statewide) %>% 
  rename(direct_emp = `1 - Direct`,
         indirect_emp = `2 - Indirect`, induced_emp = `3 - Induced`) %>% 
  mutate(direct_emp = as.numeric(direct_emp), indirect_emp = as.numeric(indirect_emp), 
         induced_emp = as.numeric(induced_emp)) %>% 
  separate(industry, into = c("ind_code","ind"), sep=" - ",remove = FALSE) %>% 
  mutate(ind_code = as.numeric(ind_code)) %>% 
  left_join(fte_convert, by = "ind_code") %>% 
  mutate(direct_emp = direct_emp*fte_per_job, indirect_emp = indirect_emp*fte_per_job, induced_emp = induced_emp*fte_per_job) %>% 
  dplyr::select(-ind_code,-ind,-fte_per_job)


ica <- inner_join(ica_emp,ica_comp,by=c("county","segment","industry")) 



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


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#4. Add in counties without O&G sector activity 

ica_total_state <- filter(ica_total,county=="Statewide")
ica_emp_wide_direct_state <- filter(ica_emp_wide_direct,county=="Statewide")
ica_emp_wide_indirect_state <- filter(ica_emp_wide_indirect,county=="Statewide")
ica_emp_wide_induced_state <- filter(ica_emp_wide_induced,county=="Statewide")
ica_comp_wide_direct_state <- filter(ica_comp_wide_direct,county=="Statewide")
ica_comp_wide_indirect_state <- filter(ica_comp_wide_indirect,county=="Statewide")
ica_comp_wide_induced_state <- filter(ica_comp_wide_induced,county=="Statewide")

county_shape <- counties(year = 2010,state = "California")

county_df <- dplyr::select(county_shape,NAME10) 

st_geometry(county_df) <- NULL

ica_total_all_counties <- left_join(county_df,ica_total,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_total_state)

ica_total_all_counties[is.na(ica_total_all_counties)] <- 0 
ica_total_all_counties <- mutate(ica_total_all_counties, segment = ifelse(segment=="0",NA,segment))


####################################################################################################################

ica_emp_direct_all_counties <- left_join(county_df,ica_emp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)%>% 
  bind_rows(ica_emp_wide_direct_state)

ica_emp_direct_all_counties[is.na(ica_emp_direct_all_counties)] <- 0 
ica_emp_direct_all_counties <- mutate(ica_emp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_emp_indirect_all_counties <- left_join(county_df,ica_emp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_emp_wide_indirect_state)

ica_emp_indirect_all_counties[is.na(ica_emp_indirect_all_counties)] <- 0 
ica_emp_indirect_all_counties <- mutate(ica_emp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_emp_induced_all_counties <- left_join(county_df,ica_emp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_emp_wide_induced_state)

ica_emp_induced_all_counties[is.na(ica_emp_induced_all_counties)] <- 0 
ica_emp_induced_all_counties <- mutate(ica_emp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################

ica_comp_direct_all_counties <- left_join(county_df,ica_comp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_comp_wide_direct_state)

ica_comp_direct_all_counties[is.na(ica_comp_direct_all_counties)] <- 0 
ica_comp_direct_all_counties <- mutate(ica_comp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_comp_indirect_all_counties <- left_join(county_df,ica_comp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_comp_wide_indirect_state)

ica_comp_indirect_all_counties[is.na(ica_comp_indirect_all_counties)] <- 0 
ica_comp_indirect_all_counties <- mutate(ica_comp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_comp_induced_all_counties <- left_join(county_df,ica_comp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_comp_wide_induced_state)

ica_comp_induced_all_counties[is.na(ica_comp_induced_all_counties)] <- 0 
ica_comp_induced_all_counties <- mutate(ica_comp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#5. replace direct compensation multipliers with the sample average for counties where it is 0 

ica_total_positive <- ica_total_all_counties %>% 
  filter(direct_comp != 0 & indirect_comp != 0 & induced_comp != 0) %>% 
  group_by(segment) %>% 
  summarize(avg_direct_comp = mean(direct_comp),avg_indirect_comp = mean(indirect_comp),avg_induced_comp = mean(induced_comp))

ica_total_all_counties_interpolated <- ica_total_all_counties %>% 
  inner_join(ica_total_positive,by=c("segment")) %>% 
  mutate(ip.direct_comp = ifelse((direct_comp==0 & is.na(segment) == F),avg_direct_comp,direct_comp),
         ip.indirect_comp = ifelse((indirect_comp==0 & is.na(segment) == F),avg_indirect_comp,indirect_comp),
         ip.induced_comp = ifelse((induced_comp==0 & is.na(segment) == F),avg_induced_comp,induced_comp)) %>% 
  dplyr::select(-avg_direct_comp,-avg_indirect_comp,-avg_induced_comp)


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#5. export as xlsx 

setwd(processed)

## ICA file 

ica_list = list(ica_total=ica_total_all_counties_interpolated,ica_indirect_emp = ica_emp_indirect_all_counties,
                ica_induced_emp = ica_emp_induced_all_counties, ica_indirect_comp = ica_comp_indirect_all_counties,
                ica_induced_comp = ica_comp_induced_all_counties)
write_xlsx(x=ica_list,"ica_multipliers_v2.xlsx")


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#6. save ICA industry multipliers as a csv for use in compiling results by industry in long format

setwd(processed)

ica_ind_output <- ica %>% 
  dplyr::select(county,segment,industry,direct_emp,indirect_emp,induced_emp,direct_comp,indirect_comp,induced_comp)

write_csv(ica_ind_output,'ica_multipliers_by_industry_long.csv')
