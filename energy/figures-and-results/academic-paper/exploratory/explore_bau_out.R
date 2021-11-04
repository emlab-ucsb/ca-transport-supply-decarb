## tracey mangin
## november 4, 2021
## 2037 kinks exploration

## libraries
library(data.table)
library(tidyverse)
library(readxl)
library(openxlsx)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path  <-'data/stocks-flows/processed/'
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'

## files
oil_price_file  <- 'oil_price_projections_revised.xlsx'

## oil prices
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1:4)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

ref_oil <- ggplot(oilpx_scens %>% filter(oil_price_scenario == "reference case"), aes(y = oil_price_usd_per_bbl,
                                                                                      x = year)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) 

# Population and incidence
ct_inc_pop_45 <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

## census-tract level population-weighted incidence rate (for age>29)
ct_inc_pop_45_weighted <- ct_inc_pop_45 %>%
  filter(lower_age > 29)%>%
  group_by(ct_id, year) %>%
  mutate(ct_pop = sum(pop, na.rm = T),
         share = pop/ct_pop,
         weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
  summarize(weighted_incidence = unique(weighted_incidence),
            pop = unique(ct_pop)) %>%
  ungroup() %>%
  as.data.table()

## (2.1) Load demographic data
# Disadvantaged community definition
ces3 <- read.csv(paste0(main_path, "data/health/processed/ces3_data.csv"), stringsAsFactors = FALSE) %>%
  select(census_tract, population, CES3_score, disadvantaged) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  as.data.table()

## add counties
ces3 <- merge(ces3, ces_county, 
              by = "census_tract")

## DAC proportion
county_dac <- dcast(ces3, county + census_tract ~ disadvantaged, value.var = "population")
county_dac[, Yes := fifelse(is.na(Yes), 0, Yes)]
county_dac[, No := fifelse(is.na(No), 0, No)]
county_dac[, total_pop := No + Yes]
county_dac[, dac_share := Yes / total_pop]
county_dac <- county_dac[, .(dac_share = weighted.mean(dac_share, total_pop, na.rm = T)), by = .(county)]


