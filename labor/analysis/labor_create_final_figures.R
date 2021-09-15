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
population_files <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/labor/raw/population'

############################################################################################ 

# Part A: Main results (county specific direct, indirect, induced) 

## 1. Import files with labor results (unit of observation: scen_id x county x year)
setwd(processed)

ext <- read_xlsx('labor_results.xlsx',sheet="extraction") 

ref <- read_xlsx('labor_results.xlsx',sheet="refining")  

str(ext)
str(ref)



## 2. Compute scenario employment - BAU for each county-year-scenario 

### extract just BAU results 

ext_bau <- filter(ext,BAU_scen==1) %>% 
  rename(c.dire_emp_bau = c.dire_emp, c.indi_emp_bau = c.indi_emp, c.indu_emp_bau = c.indu_emp, 
         c.dire_comp_bau = c.dire_comp, c.indi_comp_bau = c.indi_comp, c.indu_comp_bau = c.indu_comp) %>% 
  dplyr::select(county,year,c.dire_emp_bau,c.indi_emp_bau,c.indu_emp_bau,c.dire_comp_bau,c.indi_comp_bau,c.indu_comp_bau,revenue)

ref_bau <- filter(ref,scen_id=="R-BAU") %>% 
  rename(c.dire_emp_bau = c.dire_emp, c.indi_emp_bau = c.indi_emp, c.indu_emp_bau = c.indu_emp, 
         c.dire_comp_bau = c.dire_comp, c.indi_comp_bau = c.indi_comp, c.indu_comp_bau = c.indu_comp) %>% 
  dplyr::select(county,year,c.dire_emp_bau,c.indi_emp_bau,c.indu_emp_bau,c.dire_comp_bau,c.indi_comp_bau,c.indu_comp_bau)


### extract non BAU results and join the BAU results to them by county and year 
### collapse the data to the state level (summing across counties within scenario)
### compute job impacts as employment-bau_employment 

ext_impact <- ext %>% 
  filter(BAU_scen==0) %>% 
  inner_join(ext_bau,by=c("county","year")) %>% 
  group_by(scen_id,year) %>% 
  summarize(c.dire_emp = sum(c.dire_emp), c.indi_emp = sum(c.indi_emp), c.indu_emp = sum(c.indu_emp), 
            c.dire_comp = sum(c.dire_comp), c.indi_comp = sum(c.indi_comp), c.indu_comp = sum(c.indu_comp),
            c.dire_emp_bau = sum(c.dire_emp_bau), c.indi_emp_bau = sum(c.indi_emp_bau), c.indu_emp_bau = sum(c.indu_emp_bau), 
            c.dire_comp_bau = sum(c.dire_comp_bau), c.indi_comp_bau = sum(c.indi_comp_bau), c.indu_comp_bau = sum(c.indu_comp_bau),
            oil_price_scenario = first(oil_price_scenario),innovation_scenario = first(innovation_scenario),
            carbon_price_scenario = first(carbon_price_scenario),ccs_scenario = first(ccs_scenario),
            setback_scenario = first(setback_scenario), prod_quota_scenario = first(prod_quota_scenario),
            excise_tax_scenario = first(excise_tax_scenario)) %>% 
  mutate(tot_emp = c.dire_emp+c.indi_emp+c.indu_emp, tot_comp = c.dire_comp+c.indi_comp+c.indu_comp, 
         tot_emp_bau = c.dire_emp_bau+c.indi_emp_bau+c.indu_emp_bau,tot_comp_bau = c.dire_comp_bau+c.indi_comp_bau+c.indu_comp_bau,
         tot_emp_diff = tot_emp-tot_emp_bau, tot_comp_diff = tot_comp-tot_comp_bau,
         dire_emp_diff = c.dire_emp-c.dire_emp_bau, indi_emp_diff = c.indi_emp-c.indi_emp_bau, indu_emp_diff = c.indu_emp-c.indu_emp_bau,
         dire_comp_diff = c.dire_comp-c.dire_comp_bau, indi_comp_diff = c.indi_comp-c.indi_comp_bau, indu_comp_diff = c.indu_comp-c.indu_comp_bau,
         label = NA)

### Separately compute different percentiles of employment impacts by year (takes jth percentile across scenarios within a given year)
### NOTE: I do this once for each of 4 outcomes (direct employment and compensation, total employment and compensation)
 
ext_pctile_direct <- ext_impact %>% 
  group_by(year) %>% 
  summarize(dire_emp_p95=quantile(dire_emp_diff,0.9,na.rm = T),
            dire_emp_p75=quantile(dire_emp_diff,0.75,na.rm = T),
            dire_emp_p50=quantile(dire_emp_diff,0.5,na.rm = T),
            dire_emp_p25=quantile(dire_emp_diff,0.25,na.rm = T), 
            dire_emp_p5=quantile(dire_emp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("dire_emp_p95","dire_emp_p75","dire_emp_p50","dire_emp_p25","dire_emp_p5"),names_to = "label",names_prefix = "dire_emp_",values_to="dire_emp") %>% 
  mutate(scen_id=label)

ext_pctile_total <- ext_impact %>% 
  group_by(year) %>% 
  summarize(tot_emp_p95=quantile(tot_emp_diff,0.9,na.rm = T),
            tot_emp_p75=quantile(tot_emp_diff,0.75,na.rm = T),
            tot_emp_p50=quantile(tot_emp_diff,0.5,na.rm = T),
            tot_emp_p25=quantile(tot_emp_diff,0.25,na.rm = T), 
            tot_emp_p5=quantile(tot_emp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("tot_emp_p95","tot_emp_p75","tot_emp_p50","tot_emp_p25","tot_emp_p5"),names_to = "label",names_prefix = "tot_emp_",values_to="tot_emp") %>% 
  mutate(scen_id=label)


ext_pctile_direct_comp <- ext_impact %>% 
  group_by(year) %>% 
  summarize(dire_comp_p95=quantile(dire_comp_diff,0.9,na.rm = T),
            dire_comp_p75=quantile(dire_comp_diff,0.75,na.rm = T),
            dire_comp_p50=quantile(dire_comp_diff,0.5,na.rm = T),
            dire_comp_p25=quantile(dire_comp_diff,0.25,na.rm = T), 
            dire_comp_p5=quantile(dire_comp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("dire_comp_p95","dire_comp_p75","dire_comp_p50","dire_comp_p25","dire_comp_p5"),names_to = "label",names_prefix = "dire_comp_",values_to="dire_comp") %>% 
  mutate(scen_id=label)

ext_pctile_total_comp <- ext_impact %>% 
  group_by(year) %>% 
  summarize(tot_comp_p95=quantile(tot_comp_diff,0.9,na.rm = T),
            tot_comp_p75=quantile(tot_comp_diff,0.75,na.rm = T),
            tot_comp_p50=quantile(tot_comp_diff,0.5,na.rm = T),
            tot_comp_p25=quantile(tot_comp_diff,0.25,na.rm = T), 
            tot_comp_p5=quantile(tot_comp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("tot_comp_p95","tot_comp_p75","tot_comp_p50","tot_comp_p25","tot_comp_p5"),names_to = "label",names_prefix = "tot_comp_",values_to="tot_comp") %>% 
  mutate(scen_id=label)

### re-do the same process for refining 

ref_impact <- ref %>% 
  filter(scen_id != "R-BAU") %>% 
  inner_join(ref_bau,by=c("county","year")) %>% 
  group_by(scen_id,year) %>% 
  summarize(c.dire_emp = sum(c.dire_emp), c.indi_emp = sum(c.indi_emp), c.indu_emp = sum(c.indu_emp), 
            c.dire_comp = sum(c.dire_comp), c.indi_comp = sum(c.indi_comp), c.indu_comp = sum(c.indu_comp),
            c.dire_emp_bau = sum(c.dire_emp_bau), c.indi_emp_bau = sum(c.indi_emp_bau), c.indu_emp_bau = sum(c.indu_emp_bau), 
            c.dire_comp_bau = sum(c.dire_comp_bau), c.indi_comp_bau = sum(c.indi_comp_bau), c.indu_comp_bau = sum(c.indu_comp_bau),
            oil_price_scenario = first(oil_price_scenario),innovation_scenario = first(innovation_scenario),
            carbon_price_scenario = first(carbon_price_scenario),ccs_scenario = first(ccs_scenario),
            refining_scenario = first(refining_scenario), demand_scenario = first(demand_scenario)) %>% 
  mutate(tot_emp = c.dire_emp+c.indi_emp+c.indu_emp, tot_comp = c.dire_comp+c.indi_comp+c.indu_comp, 
         tot_emp_bau = c.dire_emp_bau+c.indi_emp_bau+c.indu_emp_bau,tot_comp_bau = c.dire_comp_bau+c.indi_comp_bau+c.indu_comp_bau,
         tot_emp_diff = tot_emp-tot_emp_bau, tot_comp_diff = tot_comp-tot_comp_bau,
         dire_emp_diff = c.dire_emp-c.dire_emp_bau, indi_emp_diff = c.indi_emp-c.indi_emp_bau, indu_emp_diff = c.indu_emp-c.indu_emp_bau,
         dire_comp_diff = c.dire_comp-c.dire_comp_bau, indi_comp_diff = c.indi_comp-c.indi_comp_bau, indu_comp_diff = c.indu_comp-c.indu_comp_bau,
         label = NA)


ref_pctile_direct <- ref_impact %>% 
  group_by(year) %>% 
  summarize(dire_emp_p95=quantile(dire_emp_diff,0.9,na.rm = T),
            dire_emp_p75=quantile(dire_emp_diff,0.75,na.rm = T),
            dire_emp_p50=quantile(dire_emp_diff,0.5,na.rm = T),
            dire_emp_p25=quantile(dire_emp_diff,0.25,na.rm = T), 
            dire_emp_p5=quantile(dire_emp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("dire_emp_p95","dire_emp_p75","dire_emp_p50","dire_emp_p25","dire_emp_p5"),names_to = "label",names_prefix = "dire_emp_",values_to="dire_emp") %>% 
  mutate(scen_id=label)

ref_pctile_total <- ref_impact %>% 
  group_by(year) %>% 
  summarize(tot_emp_p95=quantile(tot_emp_diff,0.9,na.rm = T),
            tot_emp_p75=quantile(tot_emp_diff,0.75,na.rm = T),
            tot_emp_p50=quantile(tot_emp_diff,0.5,na.rm = T),
            tot_emp_p25=quantile(tot_emp_diff,0.25,na.rm = T), 
            tot_emp_p5=quantile(tot_emp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("tot_emp_p95","tot_emp_p75","tot_emp_p50","tot_emp_p25","tot_emp_p5"),names_to = "label",names_prefix = "tot_emp_",values_to="tot_emp") %>% 
  mutate(scen_id=label)


ref_pctile_direct_comp <- ref_impact %>% 
  group_by(year) %>% 
  summarize(dire_comp_p95=quantile(dire_comp_diff,0.9,na.rm = T),
            dire_comp_p75=quantile(dire_comp_diff,0.75,na.rm = T),
            dire_comp_p50=quantile(dire_comp_diff,0.5,na.rm = T),
            dire_comp_p25=quantile(dire_comp_diff,0.25,na.rm = T), 
            dire_comp_p5=quantile(dire_comp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("dire_comp_p95","dire_comp_p75","dire_comp_p50","dire_comp_p25","dire_comp_p5"),names_to = "label",names_prefix = "dire_comp_",values_to="dire_comp") %>% 
  mutate(scen_id=label)

ref_pctile_total_comp <- ref_impact %>% 
  group_by(year) %>% 
  summarize(tot_comp_p95=quantile(tot_comp_diff,0.9,na.rm = T),
            tot_comp_p75=quantile(tot_comp_diff,0.75,na.rm = T),
            tot_comp_p50=quantile(tot_comp_diff,0.5,na.rm = T),
            tot_comp_p25=quantile(tot_comp_diff,0.25,na.rm = T), 
            tot_comp_p5=quantile(tot_comp_diff,0.05,na.rm = T)) %>% 
  pivot_longer(c("tot_comp_p95","tot_comp_p75","tot_comp_p50","tot_comp_p25","tot_comp_p5"),names_to = "label",names_prefix = "tot_comp_",values_to="tot_comp") %>% 
  mutate(scen_id=label)


#############################################################################################

#Create spaghetti plots (just amounts to plotting each scenario impact in light gray with the percentiles overlaid in black)

# extraction

## direct emp 

v1 <- ggplot(ext_impact,aes(x=year,y=dire_emp_diff,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ext_pctile_direct,color='black',aes(y=dire_emp)) +
  geom_text(data=ext_pctile_direct %>% filter(year==max(year)),aes(x = year+0.3,y=dire_emp,label=label)) +
  labs(y="Difference in Direct FTE job-years", x = "",color="",linetype="") +
  theme(legend.position = 'none', 
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v1)  

## total emp 

v2 <- ggplot(ext_impact,aes(x=year,y=tot_emp_diff,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ext_pctile_total,color='black',aes(y=tot_emp)) +
  geom_text(data=ext_pctile_total %>% filter(year==max(year)),aes(x = year+0.3,y=tot_emp,label=label)) +
  labs(y="Difference in Total FTE job-years", x = "",color="",linetype="") +
  ylim(-20000,90000) + 
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v2)  


## direct comp 

v3 <- ggplot(ext_impact,aes(x=year,y=dire_comp_diff/10^9,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ext_pctile_direct_comp,color='black',aes(y=dire_comp/10^9)) +
  geom_text(data=ext_pctile_direct_comp %>% filter(year==max(year)),aes(x = year+0.3,y=dire_comp/10^9,label=label)) +
  labs(y="Difference in Total Compensation (Billions of 2020 $)", x = "",color="",linetype="") +
  ylim(-2.5,7.5) +
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v3)  

## total comp 

v4 <- ggplot(ext_impact,aes(x=year,y=tot_comp_diff/10^9,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ext_pctile_total_comp,color='black',aes(y=tot_comp/10^9)) +
  geom_text(data=ext_pctile_total_comp %>% filter(year==max(year)),aes(x = year+0.3,y=tot_comp/10^9,label=label)) +
  labs(y="Difference in Total Compensation (Billions of 2020 $)", x = "",color="",linetype="") +
  ylim(-5,10) +
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v4)  


# refining 

## direct emp 

v5 <- ggplot(ref_impact,aes(x=year,y=dire_emp_diff,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ref_pctile_direct,color='black',aes(y=dire_emp)) +
  geom_text(data=ref_pctile_direct %>% filter(year==max(year)),aes(x = year+0.3,y=dire_emp,label=label)) +
  labs(y="Difference in Direct FTE job-years", x = "",color="",linetype="") +
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v5)  

## total emp 

v6 <- ggplot(ref_impact,aes(x=year,y=tot_emp_diff,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ref_pctile_total,color='black',aes(y=tot_emp)) +
  geom_text(data=ref_pctile_total %>% filter(year==max(year)),aes(x = year+0.3,y=tot_emp,label=label)) +
  labs(y="Difference in Total FTE job-years", x = "",color="",linetype="") +
  ylim(-50000,50000) + 
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v6)  


## direct comp 

v7 <- ggplot(ref_impact,aes(x=year,y=dire_comp_diff/10^9,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ref_pctile_direct_comp,color='black',aes(y=dire_comp/10^9)) +
  geom_text(data=ref_pctile_direct_comp %>% filter(year==max(year)),aes(x = year+0.3,y=dire_comp/10^9,label=label)) +
  labs(y="Difference in Total Compensation (Billions of 2020 $)", x = "",color="",linetype="") +
  ylim(-2,2) +
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v7)  

## total comp 

v8 <- ggplot(ref_impact,aes(x=year,y=tot_comp_diff/10^9,group=factor(scen_id))) + 
  geom_line(color='lightgrey') + 
  geom_line(data=ref_pctile_total_comp,color='black',aes(y=tot_comp/10^9)) +
  geom_text(data=ref_pctile_total_comp %>% filter(year==max(year)),aes(x = year+0.3,y=tot_comp/10^9,label=label)) +
  labs(y="Difference in Total Compensation (Billions of 2020 $)", x = "",color="",linetype="") +
  ylim(-5,5) +
  theme(legend.position = 'none',
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(color = "gray",size=0.5),
        panel.background = element_blank())

plot(v8)  

setwd('~/Dropbox/calepa') 

ggsave("ext_direct_emp.png",v1,width = 7.5,height=5,units = "in",dpi=300)
ggsave("ext_total_emp.png",v2,width = 7.5,height=5,units = "in",dpi=300)
ggsave("ext_direct_comp.png",v3,width = 7.5,height=5,units = "in",dpi=300)
ggsave("ext_total_comp.png",v4,width = 7.5,height=5,units = "in",dpi=300)

ggsave("ref_direct_emp.png",v5,width = 7.5,height=5,units = "in",dpi=300)
ggsave("ref_total_emp.png",v6,width = 7.5,height=5,units = "in",dpi=300)
ggsave("ref_direct_comp.png",v7,width = 7.5,height=5,units = "in",dpi=300)
ggsave("ref_total_comp.png",v8,width = 7.5,height=5,units = "in",dpi=300)





