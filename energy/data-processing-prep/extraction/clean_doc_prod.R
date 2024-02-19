## Tracey Mangin
## April 21, 2020
## Data cleaning -- oil production and injection data
## Data from DOC
# updated: 02/09/2024

## libraries
library(tidyverse)
library(readr)
library(lubridate)
library(rebus)
library(readtext)
library(readxl)
library(here)

## set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()


## read in data
# -------------------------------------

## Monthly Year-to-Date Well Production Injection Database.pdf
## API -- The well API. This number is used to uniquely identify each well and wellbore (state, couty, well, wellbore)
## FieldCode -- the field to which the completion interval is associated.
## AreaCode -- The area to which the completion interval is associated.
## PoolCode -- The code for the pool
## WellTypeCode -- The code for the Completion type.
## OilorCondensateProduced

# UPDATED - MP
all_wells <- read_xlsx("data/inputs/extraction/All_wells_20200417.xlsx")

# UPDATE - not all of the data is in the folders, must not have been loaded somehow
# But the paths will be correct when the data is inputted properly
## well production
prod_7785 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1977_1985/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_8689 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1986_1989/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_9094 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1990_1994/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_9599 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1995_1999/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_0004 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2000_2004/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_0509 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2005_2009/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_1514 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2010_2014/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_15 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2015/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_16 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2016/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_17 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2017/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_18 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2018/CaliforniaOilAndGasWellMonthlyProduction.csv")
prod_19 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2019/CaliforniaOilAndGasWellMonthlyProduction.csv")


## bind rows
monthly_prod <- rbind(prod_7785, prod_8689, prod_9094, prod_9599, prod_0004, prod_0509, prod_1514, prod_15, prod_16, prod_17,
                      prod_18, prod_19)

## county codes - UPDATED - MP
ccodes <- read_csv("data/inputs/extraction/county_codes.csv") %>%
  rename(county_name = county,
         county = number) %>%
  select(county_name, county)

## well type code
welltype_df <- tibble(WellTypeCode = c("AI", "DG", "GD", "GS", 
                                       "LG", "LG",  "MW", "NW",
                                       "OB", "OG", "PM", "SC",
                                       "SF", "WD", "WF", "WS"),
                      well_type_name = c("Air Injector", "Dry Gas Production", "Gas Disposal Injector", "Gas Storage Injector/Producer",
                                         "Liquid Petroleum Gas Injector/Producer", "Liquid Gas", "Ground Monitoring", "New",
                                         "Observation Well", "Oil & Gas Production", "Pressure Maintenance Injector", "Cyclic Steam",
                                         "Steam Flood Injector", "Water Disposal Injector", "Water Flood Injector", "Water Source Injector"))

## production data
all_prod <- monthly_prod %>%
  mutate(county = as.numeric(str_sub(APINumber, 3, 5)),
         year = year(ProductionReportDate),
         month = month(ProductionReportDate)) %>%
  left_join(ccodes) %>%
  left_join(welltype_df) %>%
  mutate(well_type_name = ifelse(is.na(well_type_name), WellTypeCode, well_type_name))

# UPDATED - MP
saveRDS(all_prod, file = "data/processed/well_prod_m.rds")

## injection data
# UPDATE - should work once data is inputted into all the CSV folders
## ------------------------------
inj_7785 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1977_1985/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_8689 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1986_1989/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_9094 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1990_1994/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_9599 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1995_1999/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_0004 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2000_2004/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_0509 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2005_2009/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_1514 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2010_2014/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_15 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2015/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_16 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2016/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_17 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2017/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_18 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2018/CaliforniaOilAndGasWellMonthlyInjection.csv")
inj_19 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2019/CaliforniaOilAndGasWellMonthlyInjection.csv")

## bind rows
monthly_inj <- rbind(inj_7785, inj_8689, inj_9094, inj_9599, inj_0004, inj_0509, inj_1514, inj_15, inj_16, inj_17,
                      inj_18, inj_19)

## injection data
all_inject <- monthly_inj %>%
  mutate(county = as.numeric(str_sub(APINumber, 3, 5)),
         year = year(InjectionDate),
         month = month(InjectionDate)) %>%
  left_join(ccodes) %>%
  left_join(welltype_df) %>%
  mutate(well_type_name = ifelse(is.na(well_type_name), WellTypeCode, well_type_name))

saveRDS(all_inject, file = paste0(data_directory, "processed/well_inject_m.rds")) 

## well data
# UPDATE - should work once data is inputted into all the CSV folders
## -----------------------------
wells_7785 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1977_1985/CaliforniaOilAndGasWells.csv")
wells_8689 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1986_1989/CaliforniaOilAndGasWells.csv")
wells_9094 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1990_1994/CaliforniaOilAndGasWells.csv")
wells_9599 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_1995_1999/CaliforniaOilAndGasWells.csv")
wells_0004 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2000_2004/CaliforniaOilAndGasWells.csv")
wells_0509 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2005_2009/CaliforniaOilAndGasWells.csv")
wells_1014 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2010_2014/CaliforniaOilAndGasWells.csv")
wells_15 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2015/CaliforniaOilAndGasWells.csv")
wells_16 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2016/CaliforniaOilAndGasWells.csv")
wells_17 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2017/CaliforniaOilAndGasWells.csv")
wells_18 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2018/CaliforniaOilAndGasWells.csv")
wells_19 <- read_csv("data/inputs/extraction/monthly-prod-inj-wells/CSV_2019/CaliforniaOilAndGasWells.csv")


## figure this ish out
wells_19$row <- 1:nrow(wells_19)
probs <- problems(wells_19)

wells2 <- wells_19 %>%
  filter(row %in% probs$row)

# 
# 
# test <- readLines(paste0(data_directory, "raw/hist_well/CSV_1977_1985/CaliforniaOilAndGasWells.csv"))
# test2 <- str_replace_all(test, pattern = ", Inc.", " Inc.")
# test2 <- str_replace_all(test, pattern = ", Inc", " Inc")
# test2 <- str_replace_all(test2, pattern = "Grimes, West, Gas,", "Grimes West Gas,")
# test2 <- str_replace_all(test2, pattern = ", LLC", " LLC")
# test2 <- str_replace_all(test2, pattern = ", L.L.C.", " LLC")
# test2 <- str_replace_all(test2, pattern = ", INC", " INC")
# test2 <- str_replace_all(test2, pattern = ", Jr.", " Jr.")
# test2 <- str_replace_all(test2, pattern = ", et al", " et al")
# test2 <- str_replace_all(test2, pattern = "Drilling, Exploration", "Drilling Exploration")
# test2 <- str_replace_all(test2, pattern = "Sills, ", "Sills ")
# test2 <- str_replace_all(test2, pattern = "Amerada Hess Corp., Unit Operator", "Amerada Hess Corp. Unit Operator")
# test2 <- str_replace_all(test2, pattern = "Brentwood, East, Gas", "Brentwood East Gas")
# test2 <- str_replace_all(test2, pattern = "Oakley, South, Gas", "Oakley South Gas")
# test2 <- str_replace_all(test2, pattern = "Coalinga, East, Extension", "Coalinga East Extension")
# test2 <- str_replace_all(test2, pattern = "Burrel, Southeast", "Burrel Southeast")
# test2 <- str_replace_all(test2, pattern = ", Ltd.", " Ltd.")
# test2 <- str_replace_all(test2, pattern = ", LP", " LP")
# test2 <- str_replace_all(test2, pattern = "D. J. Pickrell, Operator", "D. J. Pickrell Operator")
# test2 <- str_replace_all(test2, pattern = "Ted Koble, Operator", "Ted Koble Operator")
# test2 <- str_replace_all(test2, pattern = "Ted Koble, Operator", "Ted Koble Operator")
# test2 <- str_replace_all(test2, pattern = "Strickler, Operator", "Strickler Operator")
# test2 <- str_replace_all(test2, pattern = ", Opr.", "Opr.")
# test2 <- str_replace_all(test2, pattern = "Larkin, West, Gas", "Larkin West Gas")
# test2 <- str_replace_all(test2, pattern = "Canfield Ranch,Gosford, South", "Canfield Ranch Gosford South")
# test2 <- str_replace_all(test2, pattern = ", Oper.", "Oper.")
# test2 <- str_replace_all(test2, pattern = ", L. P.", " LP")
# test2 <- str_replace_all(test2, pattern = ",Jr.", " Jr.")
# test2 <- str_replace_all(test2, pattern = "Belridge, South", " Belridge South")
# test2 <- str_replace_all(test2, pattern = "GOU 4-5,32/33", "GOU 4-5 32/33")
# test2 <- str_replace_all(test2, pattern = "Aliso, West", "Aliso West")
# test2 <- str_replace_all(test2, pattern = "Antelope Hills, North", "Antelope Hills North")
# test2 <- str_replace_all(test2, pattern = "Bellevue, West", "Bellevue West")
# test2 <- str_replace_all(test2, pattern = "Belridge, North", "Belridge North")
# test2 <- str_replace_all(test2, pattern = "Baker, Trustee", "Baker Trustee")
# test2 <- str_replace_all(test2, pattern = "2nd & 3rd, Upper", "2nd & 3rd Upper")
# test2 <- str_replace_all(test2, pattern = "D-6, 7U(E)", "D-6 7U(E)")
# test2 <- str_replace_all(test2, pattern = "Woodward, USL", "Woodward USL")
# test2 <- str_replace_all(test2, pattern = "Tar-Ranger, Main & Del Amo", "Tar-Ranger Main & Del Amo")
# test2 <- str_replace_all(test2, pattern = ", L.C.", " L.C.")
# test2 <- str_replace_all(test2, pattern = ", California LLC", " California LLC")
# test2 <- str_replace_all(test2, pattern = ", Unit Opr.", " Unit Opr.")
# test2 <- str_replace_all(test2, pattern = "Ltd., U.S.A.", "Ltd. U.S.A.")
# test2 <- str_replace_all(test2, pattern = "1st, 2nd, and 3rd", "1st 2nd and 3rd ")
# test2 <- str_replace_all(test2, pattern = "Gosford, East,", "Gosford East")
# test2 <- str_replace_all(test2, pattern = "Gosford, West,", "Gosford West")
# test2 <- str_replace_all(test2, pattern = "S9, S10", "S9 S10")
# test2 <- str_replace_all(test2, pattern = "S9, S17", "S9 S17")
# test2 <- str_replace_all(test2, pattern = "S8, S17", "S8 S17")
# test2 <- str_replace_all(test2, pattern = "Coles Levee, North", "Coles Levee North")
# test2 <- str_replace_all(test2, pattern = "Coles Levee, South", "Coles Levee South")
# test2 <- str_replace_all(test2, pattern = "Cuyama, South", "Cuyama South")
# test2 <- str_replace_all(test2, pattern = ",L.P.", "L.P")
# test2 <- str_replace_all(test2, pattern = ", U.S.A.", " U.S.A.")
# test2 <- str_replace_all(test2, pattern = "A,B,C, PE, D, & E Zones, Block B", "A B C PE D & E Zones Block B")
# test2 <- str_replace_all(test2, pattern = "Berg, Laney", "Berg Laney")
# test2 <- str_replace_all(test2, pattern = "Corning, South, Gas", "Corning South Gas")
# test2 <- str_replace_all(test2, pattern = "Coyote, East", "Coyote East")
# test2 <- str_replace_all(test2, pattern = "Coyote, West " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Coyote West ABD")
# test2 <- str_replace_all(test2, pattern = "Leda, North " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Leda North ABD")
# test2 <- str_replace_all(test2, pattern = "Greenwood, South, Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Greenwood South Gas ABD")
# test2 <- str_replace_all(test2, pattern = "Afton, South, Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Afton South Gas ABD")
# test2 <- str_replace_all(test2, pattern = "San Joaquin, Northwest, Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "San Joaquin Northwest Gas ABD")
# test2 <- str_replace_all(test2, pattern = "Transamerica,Compton Landing, S., Gas" %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Transamerica Compton Landing S. Gas ABD")
# test2 <- str_replace_all(test2, pattern = ", Trustee Of The Hunter Living Trust", " Trustee Of The Hunter Living Trust")
# test2 <- str_replace_all(test2, pattern = "Sterling, East " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Sterling East ABD")
# test2 <- str_replace_all(test2, pattern = "Porter, West " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Porter West ABD")
# test2 <- str_replace_all(test2, pattern = "Hualde, Anaheim, Miocene A, Stern,", "Hualde Anaheim Miocene A Stern,")
# test2 <- str_replace_all(test2, pattern = "Trico, Northwest, Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Trico Northwest Gas ABD")
# test2 <- str_replace_all(test2, pattern = "6th, 184 Anticline " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "6th 184 Anticline ABD")
# test2 <- str_replace_all(test2, pattern = "Emery, West", "Emery West")
# test2 <- str_replace_all(test2, pattern = "Zamora, North, Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Zamora North Gas ABD")
# test2 <- str_replace_all(test2, pattern = "Porter, West " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Porter West ABD")
# 
# test2 <- str_replace_all(test2, pattern = "Compton Landing, S., Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Compton Landing S. Gas ABD")
# test2 <- str_replace_all(test2, pattern = "Cecil,Afton, South, Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Cecil,Afton South Gas ABD")
# test2 <- str_replace_all(test2, pattern = "L.P.K.,Yorba Linda " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "L.P.K.Yorba Linda ABD")
# test2 <- str_replace_all(test2, pattern = "Sterling, East " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Sterling East ABD")
# test2 <- str_replace_all(test2, pattern = "Compton Landing, S., Gas " %R% OPEN_PAREN %R% "ABD" %R% CLOSE_PAREN, "Compton Landing S. Gas ABD")

fix_2019 <- readLines(paste0(data_directory, "raw/hist_well/CSV_2019/CaliforniaOilAndGasWells.csv"))
fix_20192<- str_replace_all(fix_2019, pattern = "8-9B INT, Sec. 32", "8-9B INT Sec. 32")



# test2 <- str_replace_all(test2, pattern = "", "")
# test2 <- str_replace_all(test2, pattern = "", "")

#040212008000
#040112009400

writeLines(fix_20192, paste0(data_directory, "processed/wells_19.csv"))

wells_2019 <- read_csv(paste0(data_directory, "processed/parseprobs/wells_19.csv"))


