# CalEPA: Emission factors from Jaramillo and Muller (2009)
# vthivierge@ucsb.edu
# created: 08/11/2020
# updated: 06/26/2023


# Updated by Mariam Garcia 2/13/2024

# set up environment

rm(list=ls())
`%notin%` <- Negate(`%in%`)

options(scipen=999)

## Packages

packages=c("sf", "dplyr", "readr","xlsx","ggplot2", "cowplot")

for (i in packages) {
  if (require(i,character.only=TRUE)==FALSE) {
    install.packages(i,repos='http://cran.us.r-project.org')
  }
  else {
    require(i,character.only=TRUE)
  }
}
## Directory

#wd <- c("H:/EJ_wells") # Danae's WD

# UPDATED - MG 
wd <- c('/capstone/freshcair/meds-freshcair-capstone') #freshCAir main directory
setwd(wd)
getwd()

## Read raw emission factors from Jaramillo & Muller (2016)

ef_raw <- read.csv("./emission_factors.csv", stringsAsFactors = FALSE)%>%
  filter(source %in% "jaramillo")%>%
  select(-source)

ef <- ef_raw %>%
  mutate(quantity_ton_mmillionbbl = quantity,
         quantity_ton_bbl = quantity_ton_mmillionbbl/1000000)%>%
  select(-unit); ef

## Save final emission factors (kg/bbl)

write.csv(ef %>% select(-quantity:-quantity_ton_mmillionbbl),"./emission_factors_final.csv",row.names = F)
  