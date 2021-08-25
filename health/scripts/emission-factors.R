# CalEPA: Emission factors from Jaramillo and Muller (2009)
# vthivierge@ucsb.edu
# created: 08/11/2020
# updated: 08/25/2020


# set up environment

rm(list=ls())
`%notin%` <- Negate(`%in%`)

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
wd <- c("G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/health/processed/DataFiles_EmissionsFactors") #Vincent's WD
setwd(wd)
getwd()

## Read raw emission factors from Jaramillo & Muller (2016)

ef_raw <- read.csv("./emission_factors.csv", stringsAsFactors = FALSE)%>%
  filter(source %in% "jaramillo")%>%
  select(-source)

ef <- ef_raw %>%
  mutate(quantity_kg_mmillionbbl = quantity * 1000,
         quantity_kg_bbl = quantity_kg_mmillionbbl/1000000)%>%
  select(-unit); ef

## Save final emission factors (kg/bbl)

write.csv(ef %>% select(-quantity:-quantity_kg_mmillionbbl),"./emission_factors_final.csv",row.names = F)
  