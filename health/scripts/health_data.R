# CA transportation decarb: Mortality impacts data processing
# vthivierge@ucsb.edu
# created: 08/25/2021
# updated: 05/26/2022

# set up environment ########################################

rm(list=ls())
options(java.parameters = "-Xmx8000m") 


## Packages

packages=c("xlsx", "gdata", "dplyr","tidyr", "stringr", "fuzzyjoin", "stringr", 
           "ggplot2", "stargazer", "plm", "cowplot", "sf", "lwgeom","data.table")

lapply(1:length(packages), function(x) 
  ifelse((require(packages[x],character.only=TRUE)==FALSE),install.packages(packages[x]),
         require(packages[x],character.only=TRUE)))

#Set directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../..') #Goes back to home project directory
getwd()

# Load and process data #####################################

# External data

#census tract age-population

# UPDATED
ct_raw <- read.csv("/data/ca-transportation-supply-decarb-files/inputs/health/nhgis0001_ts_geog2010_tract.csv", stringsAsFactors = FALSE); str(ct_raw)
# UPDATED
ct_age_desc <- read.csv("/data/ca-transportation-supply-decarb-files/inputs/health/age_group_desc.csv", stringsAsFactors = FALSE); str(ct_age_desc)

ct_ca <- ct_raw %>%
  `colnames<-`(tolower(colnames(ct_raw)))%>%
  filter(state == "California")%>%
  gather(age_group_raw, pop, cn5aa2000:cn5aw2010)%>%
  mutate(year = str_sub(age_group_raw,-4,-1),
         age_group_str = toupper(str_sub(age_group_raw,4,5)))%>%
  filter(year==2010)%>%
  left_join(ct_age_desc, by = c("age_group_str"="age_group_str"))%>%
  mutate(lower_age = str_sub(age_group_desc,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         age_group_desc = str_remove(age_group_desc," years"),
         upper_age = str_sub(age_group_desc,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",99,upper_age)),
         year = as.integer(year))%>%
  select(-geogyear,-age_group_raw,-age_group_str); str(ct_ca)

age_group_ct <- ct_ca %>%
  mutate(upper_age = ifelse(upper_age==5,4,upper_age))%>% ## What it actually should be (under 5 year old)
  group_by(lower_age,upper_age)%>%
  summarize();age_group_ct

#CDOF demographic projections

# UPDATED
cdof_raw <- fread("/data/ca-transportation-supply-decarb-files/inputs/health/CDOF_p2_Age_1yr_Nosup.csv", stringsAsFactors = FALSE, blank.lines.skip = TRUE)%>%
  select(-Column1:-Column16331)%>%
  gather(year,pop,'2010':'2060')%>%
  mutate(pop = as.numeric(str_replace(pop,",","")),
         County = str_replace(County," County",""),
         year = as.numeric(year),
         Age = as.numeric(Age),
         Age = replace_na(Age,99)); cdof_raw

colnames(cdof_raw) <- tolower(colnames(cdof_raw)) 

## Yearly growth rate for each year county and age group

cdof_pred <- cdof_raw %>%
  filter(year<2046)%>% 
  fuzzyjoin::fuzzy_left_join(
    age_group_ct,
    by = c("age" = "lower_age",
           "age" = "upper_age"),
    match_fun = list(`>=`, `<=`))%>%
  #mutate(upper_age = ifelse(age==0,0,upper_age),
  #       lower_age = ifelse(age>0 & age<5,1,lower_age))%>% # Don't think I need to do this...
  group_by(county,year,lower_age)%>%
  summarize(upper_age = unique(upper_age),pop=sum(pop))%>%
  group_by(county,lower_age)%>%
  arrange(year)%>%
  mutate(change_pct = (pop - dplyr::lag(pop))/dplyr::lag(pop))%>%
  arrange(county,lower_age);cdof_pred

# Exported data from BenMAP

#County name to merged with BenMAP row/col county indicators

# UPDATED
ct <- read_sf("/data/ca-transportation-supply-decarb-files/inputs/health/County_def.shp")
county <- as.data.frame(cbind(ct$NAME, ct$STATE_NAME, ct$ROW, ct$COL), stringsAsFactors = F) 
colnames(county) <- c("county","state", "row", "col")

county$row <- as.integer(county$row)
county$col <- as.integer(county$col)

# Mortality incidence data (2015 baseline)

# UPDATED
incidence_ca <- read.csv("/data/ca-transportation-supply-decarb-files/inputs/health/Mortality Incidence (2015).csv", stringsAsFactors = F) %>%
  filter(Endpoint == "Mortality, All Cause") %>%
  select(-Endpoint.Group,-Race:-Ethnicity, -Type)%>%
  left_join(county, by = c("Column"="col","Row"="row"))%>%
  filter(state %in% "California"); str(incidence_ca)

colnames(incidence_ca) <- tolower(colnames(incidence_ca))

#Fuzzy join of mortality incidence and population by age-groups

gc()

##Remove extra columns to make join faster

# Mortality 

temp_incidence_ca <- incidence_ca %>%
  select(-endpoint,-column,-row,-state)%>%
  ungroup(); temp_incidence_ca

temp_ct_ca <- ct_ca%>%
  select(-state,-statea,-tracta, -countya,-age_group_desc)%>%
  mutate(county = str_remove(county, " County"))%>%
  ungroup(); temp_ct_ca

##Fuzzy join (of incidence to pop)

ct_incidence_ca <- temp_ct_ca %>%
  fuzzyjoin::fuzzy_left_join(
    temp_incidence_ca,
    by = c("county" = "county",
           "lower_age" = "start.age" ,
           "upper_age" = "end.age"),
    match_fun = list(`==`,`>=`, `<=`))%>%
  mutate(county = coalesce(county.x,county.y))%>%
  select(-county.x,-county.y)%>%
  fuzzyjoin::fuzzy_left_join(
    temp_incidence_ca,
    by = c("lower_age" = "start.age" ,
           "upper_age" = "end.age",
           "county"="county"),
    match_fun = list(`<=`, `>=`, `==`))%>%
  mutate(start.age = coalesce(start.age.x,start.age.y),
         end.age = coalesce(end.age.x,end.age.y),
         county = coalesce(county.x,county.y),
         value = coalesce(value.x,value.y))%>%
  select(-start.age.x,-start.age.y,-end.age.x, -end.age.y,
         -county.x,-county.y,-value.x,-value.y)

# 
write.csv(ct_incidence_ca,file = "./calepa-cn/data/benmap/processed/ct_incidence_ca.csv", row.names = FALSE)
ct_incidence_ca <- read.csv("./calepa-cn/data/benmap/processed/ct_incidence_ca.csv", stringsAsFactors =  FALSE)

##Projected population data and mortality incidence

cdof_pred_sp <- cdof_pred %>%
  filter(year>2010)%>%
  select(-pop)%>%
  spread(year,change_pct)

ct_inc_45_temp <- ct_incidence_ca %>%
    mutate(upper_age = ifelse(upper_age==5,4,upper_age))%>% ## What it actually should be (under 5 year old)
    left_join(cdof_pred_sp, by = c("lower_age" = "lower_age","upper_age" = "upper_age", "county"="county"))%>%
    mutate(pop_2010 = pop)

for (i in 2011:2045){ 
  
  ct_inc_45_temp <- ct_inc_45_temp%>%
    mutate(!!paste0("pop_",i,sep="") := get(paste0("pop_",i-1,sep=""))*(1+get(paste0(i,sep=""))))
}

ct_inc_45 <- ct_inc_45_temp%>%
    select(-pop,-year,-`2011`:-`2045`)%>%
    gather(year,pop,pop_2010:pop_2045)%>%
    mutate(year = as.numeric(str_remove(year,"pop_")))%>%
    filter(year>2018)%>%
    mutate(incidence_2015 = value)%>%
    select(-value)%>%
    ungroup()

## Output final population and mortality incidence data

write.csv(ct_inc_45,file = "./calepa-cn/data/benmap/processed/ct_inc_45.csv", row.names = FALSE)
#ct_inc_45 <- fread("./calepa-cn/data/benmap/processed/ct_inc_45.csv", stringsAsFactors = FALSE)

## Census tract level for labor team

ct_pop_45 <- ct_inc_45 %>%
  group_by(gisjoin, year)%>%
  summarise(county = first(county),
            pop = sum(pop))

write.csv(ct_pop_45,file = "./calepa-cn/data/benmap/processed/ct_pop_45.csv", row.names = FALSE)
