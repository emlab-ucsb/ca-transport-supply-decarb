## Tracey Mangin
## October 2, 2020
## prep setback files

library(tidyverse)

## path 
path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"

## read in files
setback_coverage <- read_csv(paste0(path, "outputs/setback/oilfields_setbackeffects.csv"))
setback_wells <- read.csv(paste0(path, "outputs/setback/activewells_prod_nearSR_multdist_csv.csv"))

## new wells -- coverage
## ---------------------------------------------------

setback_coverage2 <- setback_coverage %>%
  select(FieldCode = SR_OilFields_1mile.FIELD_CODE,
         coverage_5280 = CoveredArea_Percent_1mile,
         coverage_2500 = CoveredArea_Percent_2500ft,
         coverage_1000 = CoveredArea_Percent_1000ft) %>%
  pivot_longer(coverage_5280:coverage_1000, names_to = "setback_dist_ft", values_to = "area_coverage") %>%
  mutate(area_coverage = ifelse(area_coverage > 1, 1, area_coverage),
         setback_dist_ft = str_replace(setback_dist_ft, "coverage", "setback"),
         setback_dist_ft = paste0(setback_dist_ft, "ft")) %>%
  rename(setback_scenario = setback_dist_ft)

## add zero
setback_coverage3 <- tibble(FieldCode = c(unique(setback_coverage2$FieldCode)),
                            setback_scenario = c(rep("no_setback", length(unique(setback_coverage2$FieldCode)))),
                            area_coverage = c(rep(0, length(unique(setback_coverage2$FieldCode))))) %>%
  rbind(setback_coverage2) %>%
  filter(!FieldCode %in% c("G1-", "G2-", "G3-"))

## save input sheet
write.csv(setback_coverage3, paste0(path, 'outputs/setback/model-inputs/setback_coverage.csv'))


## individual wells
## -----------------------------------------------------

setback_wells2 <- setback_wells %>%
  select(api_ten_digit = active_wel, 
         setback_2500ft = NEAR_DIST_, 
         setback_1000ft = NEAR_DIST1, 
         setback_5280ft = NEAR_DIST) %>%
  mutate(no_setback = -1) %>%
  pivot_longer(setback_2500ft:no_setback, names_to = "setback_scenario", values_to = "in_setback") %>%
  mutate(in_setback = ifelse(in_setback == -1, 0, 1))

## save input sheet
write_csv(setback_wells2, paste0(path, 'outputs/setback/model-inputs/wells_in_setbacks_test.csv'))



