##################################################################
# This script generates the well attributes within_Xft of SR for X = {1,000ft, 2,500ft, and 1 mille}
# Sandy Sum
# sandysum@ucsb.edu
# written: 4/9/2021 | modified: 4/12/2021
# modified by Tracey Mangin
##################################################################
# revise : Feb 14, 2024 by Haejin 

# comment out and add your own machine's file path
home               <- "/capstone/freshcair/meds-freshcair-capstone"
buffer_path        <- "data/processed"
data_directory     <- "/capstone/freshcair/meds-freshcair-capstone/inputs/extraction"

## files
prod_file    <- "well_prod_m_processed.csv" 

# load packages
library(sf)
library(tidyverse)
library(purrr)
library(rgdal)
library(data.table)
library(gdalUtilities)
library(maps)
library(mapview)

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## quick CA
ca <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california") %>%
  st_transform(ca_crs)

################################# READ DATA AND TRANSFORM

buff1000 <- sf::st_read(file.path(home, buffer_path, "buffer_1000ft.shp")) # no file here 

buff2500 <- sf::st_read(file.path(home, buffer_path, "buffer_2500ft.shp")) # no file here 

buff5280 <- sf::st_read(file.path(home, buffer_path, "buffer_5280ft.shp")) # no file here 

# transform to NAD83(NSRS2007) / California Albers as well for wells and field boundaries
wells <- sf::st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells_gis/Wells_All.shp")) %>% # ---- missing file --------
  st_transform(ca_crs) %>%
  dplyr::select(API, WellStatus) %>%
  unique()

wells2 <- sf::st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells_gis/Wells_All.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(API, WellStatus, FieldName) %>%
  unique()

boundaries <- st_read(file.path(home, "/data/gis/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488) # update

## monthly well production
well_prod <- fread(paste0(data_directory, prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## wells that produce oil in time horizon
pos_prod <- well_prod[, .(api_prod = sum(OilorCondensateProduced, na.rm = T)), by = api_ten_digit][api_prod > 0]


################################ functions
## functions
## ----------------------------------------


# gen_field_setback_coverage <- function(field_index, buff_shape, buff_dist = c(1000, 2500, 5280)) {
#   field <- boundaries[field_index,]
#   for (i in 1:length(buff_dist)){
#     # browser()
#     d = buff_dist[i]
#     
#     int <- as_tibble(st_intersection(buff_shape, field))
#     if (nrow(int)==0){
#       area = 0
#     } else {
#       area = st_area(int$geometry)/st_area(field)
#     }
#     field <- field %>% 
#       mutate(!!(paste0("percent_within_", d)) := area)
#   }
#   field %>% as_tibble()
# }

# gen_within_vars <- function(sr_df, wells_df, ft) {
#   for (i in 1:length(ft)){
#     # browser()
#     d = ft[i]
#     m = d*0.3048
#     buf <- st_buffer(st_geometry(sr_df), dist = m)
#     wells_df <- wells_df %>% 
#       mutate(!!(paste0("within", d)) := (sf::st_within(wells_df, buf) %>% lengths() > 0 %>% as.numeric()))
#   }
#   wells_df
# }

## ensure multipolygon

# ensure_multipolygons <- function(X) {
#   tmp1 <- tempfile(fileext = ".gpkg")
#   tmp2 <- tempfile(fileext = ".gpkg")
#   st_write(X, tmp1)
#   ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
#   Y <- st_read(tmp2)
#   st_sf(st_drop_geometry(X), geom = st_geometry(Y))
# }

# convert the multisurface to multipolygon

# sr_1 <- ensure_multipolygons(sr[1,])
# sr[1,] <- sr_1

# drop the MULTISURFACE as it seems to be the union of all the other MULTIPOLYGONS in the rest of the file 
# see Sandy's notes in https://docs.google.com/document/d/1j_5GoAH2Mpqon4sHqZt29Qh31yAP5kKgCgGddfOQ4Nw/edit#

################################# GENERATE WELL SETBACK SCENARIO ATTRIBUTES (IN OR OUT FOR d = c(1000, 2500, 5280, 10000))
# sr <- sr[-1,]

## create data frame of wells, columns indicating if they are in or out of buffers

## filter for wells with positive historical prod
# wells_buffer <- wells %>%
#   filter(API %in% pos_prod$api_ten_digit)

## join wells with buffers
wells_within_df <- wells %>% 
  # it returns NA for those outside and 0 for those inside in the FID column
  st_join(buff1000, left = TRUE) %>%
  rename(within_1000 = FID) %>%
  st_join(buff2500, left = TRUE) %>%
  rename(within_2500 = FID) %>%
  st_join(buff5280, left = TRUE) %>%
  rename(within_5280 = FID) %>%
  as_tibble() %>%
  dplyr::select(api_ten_digit = API, well_status = WellStatus, within_1000:within_5280) %>%
  pivot_longer(within_1000:within_5280, names_to = "buffer_dist_ft", values_to = "within_setback") %>%
  mutate(within_setback = ifelse(within_setback == 0, 1, within_setback),
         within_setback = ifelse(is.na(within_setback), 0, within_setback),
         buffer_dist_ft = sub(".*_", "", buffer_dist_ft),
         setback_scenario = paste0("setback_", buffer_dist_ft))

## create no setback scenario
wells_within_df_all <- wells %>%
  as_tibble() %>%
  dplyr::select(api_ten_digit = API, well_status = WellStatus) %>%
  mutate(buffer_dist_ft = 0,
         within_setback = 0,
         setback_scenario = "no_setback") %>%
  rbind(wells_within_df) %>%
  dplyr::select(api_ten_digit, well_status, setback_scenario, buffer_dist_ft, within_setback) %>%
  arrange(api_ten_digit)

## reclassify two wells in las cienegas
## Las Cienegas, change two wells to in setback under the three setback distances
## c("0403700381", "0403700328")

recode_well_vec <- c("0403700381", "0403700328")

wells_within_df_all <- wells_within_df_all %>%
  ## recode las cienegas wells that appear to have incorrect gps info
  mutate(within_setback = ifelse(api_ten_digit %in% recode_well_vec & setback_scenario != 'no_setback', 1, within_setback)) 



## check to make sure four for each well
View(wells_within_df_all %>%
       group_by(api_ten_digit) %>%
       summarise(n = n()) %>%
       ungroup())

## make sure that the number of wells within larger buffers are increasing
wells_within_df_all %>%
       group_by(setback_scenario) %>%
       summarise(wells_within = sum(within_setback)) 

## finally, make a plot to ensure that the wells are encoded correctly

wells_within_df <- wells %>% 
  st_join(buff1000, left = TRUE) %>%
  rename(within_1000 = FID) %>%
  # st_join(buff2500, left = TRUE) %>%
  # rename(within_2500 = FID) %>%
  # st_join(buff5280, left = TRUE) %>%
  # rename(within_5280 = FID) %>% 
  mutate(within_setback = ifelse(within_1000 == 0, 1, within_1000),
         within_setback = ifelse(is.na(within_setback), 0, within_setback))

ggplot(data = buff1000) +
  geom_sf() +
  lims(x=xcheck, y=ycheck) +
  geom_sf(data = wells_within_df, aes(color = factor(within_setback)))


# # check number in setback compared to tracey's output
# 
# out_df %>% group_by(setback_scenario) %>% 
#   summarize(n_in = sum(in_setback))
# 
# tracey_wells <- read_csv(file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_test.csv")) %>%
#   mutate(api_ten_digit = as.character(paste0("0", api_ten_digit))) %>%
#   rename(in_setback_orig = in_setback) %>%
#   filter(setback_scenario != "no_setback")
# 
# 
# ## get wells that produce oil
# well_prod <- read_rds(paste0(home, "/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds")) %>%
#   mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
#   dplyr::select(api_ten_digit, OilorCondensateProduced) %>%
#   group_by(api_ten_digit) %>%
#   summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup() %>%
#   filter(prod > 0)
# 
# compare_df <- full_join(out_df, tracey_wells) %>%
#   filter(api_ten_digit %in% well_prod$api_ten_digit)
# 
# tracey_wells %>% group_by(setback_scenario) %>% 
#   summarize(n_in = sum(in_setback_orig))

# save output
write_csv(wells_within_df_all, file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/wells_in_setbacks_revised.csv")) # missing -------

## make maps



# map_figure <- ggplot(data = ca) +
#   geom_sf() +
#   geom_sf(data = out, aes(color = within10000), size = 0.2, alpha = 0.4) +
#   geom_sf(data = buf10000, color = "black", fill = NA) +
#   # scale_fill_viridis_c(option = "plasma") +
#   # # scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
#   # facet_wrap(~ year) +
#   theme_minimal() +
#   labs(
#     x = "",
#     y = "") +
#   theme(
#     panel.background = element_rect(fill="white"),
#     panel.grid.major = element_line(colour = "white"),
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     text = element_text(size=15),
#     legend.direction = "vertical"
#   ) 





################################# GENERATE FIELD COVERAGE ATTRIBUTES (IN OR OUT FOR d = c(1000, 2500, 5280, 10000))

## fields that produce oil in time horizon
pos_fields <- well_prod[, (prod = sum(OilorCondensateProduced)), by = .(doc_field_code)]

field_boundaries <- boundaries %>%
  mutate(orig_area_m2 = st_area(boundaries)) %>%
  filter(FIELD_CODE %in% pos_fields$doc_field_code) %>%
  dplyr::select(doc_field_code = FIELD_CODE, NAME, area_sq_mi = AREA_SQ_MI, area_acre = AREA_ACRE, orig_area_m2)

anti_join(pos_fields %>% dplyr::select(doc_field_code) %>% unique(), 
          field_boundaries %>% dplyr::select(doc_field_code) %>% unique())

## find ratio of field area covered by buffer for three buffer scenarios
## -----------------------------

field_coverage_df_1000 <- field_boundaries %>% 
  st_intersection(buff1000)

# plot(field_boundaries %>% dplyr::select(doc_field_code), 
#      xlim = xcheck, 
#      ylim = ycheck, 
#      axes = TRUE)
# 
# plot(field_coverage_df_1000 %>% dplyr::select(doc_field_code), 
#      xlim = xcheck, 
#      ylim = ycheck, 
#      axes = TRUE,
#      add = TRUE, 
#      color = "yellow")

field_coverage_df_1000_2 <- field_coverage_df_1000 %>%
  mutate(setback_area = st_area(field_coverage_df_1000),
         setback_1000 = setback_area / orig_area_m2) %>%
  dplyr::select(doc_field_code, setback_1000) %>%
  st_drop_geometry() %>%
  as.tibble()

field_coverage_df_2500 <- field_boundaries %>% 
  st_intersection(buff2500)

field_coverage_df_2500_2 <- field_coverage_df_2500 %>%
  mutate(setback_area = st_area(field_coverage_df_2500),
         setback_2500 = setback_area / orig_area_m2) %>%
  dplyr::select(doc_field_code, setback_2500) %>%
  st_drop_geometry() %>%
  as.tibble()

field_coverage_df_5280 <- field_boundaries %>% 
  st_intersection(buff5280) 

field_coverage_df_5280_2 <- field_coverage_df_5280 %>%
  mutate(setback_area = st_area(field_coverage_df_5280),
         setback_5280 = setback_area / orig_area_m2) %>%
  dplyr::select(doc_field_code, setback_5280) %>%
  st_drop_geometry() %>%
  as.tibble()

## combine all three setbacks
field_boundaries2 <- field_boundaries %>%
  left_join(field_coverage_df_1000_2) %>%
  left_join(field_coverage_df_2500_2) %>%
  left_join(field_coverage_df_5280_2) %>%
  st_drop_geometry() %>%
  units::drop_units() %>%
  mutate(no_setback = 0) %>%
  pivot_longer(setback_1000:no_setback, names_to = "setback_scenario", values_to = "rel_coverage") 

field_boundaries2[is.na(field_boundaries2)] <- 0

## check to make sure four for each well
View(field_boundaries2 %>%
       group_by(doc_field_code) %>%
       summarise(n = n()) %>%
       ungroup())

## add number of wells to each field
n_wells <- sf::st_read(file.path(home, "emlab/projects/current-projects/calepa-cn/data/GIS/raw/allwells_gis/Wells_All.shp")) %>% # missing one
  st_drop_geometry() %>%
  filter(!WellStatus %in% c("Abeyance")) %>%
  group_by(FieldName) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rename(NAME = FieldName)


## get ready to rename
anti_join(n_wells %>% dplyr::select(NAME) %>% unique(), field_boundaries2 %>% dplyr::select(NAME) %>% unique())
anti_join(field_boundaries2 %>% dplyr::select(NAME) %>% unique(), n_wells %>% dplyr::select(NAME) %>% unique())


## group by field, count
n_wells2 <- n_wells %>%
  mutate(NAME = ifelse(NAME == "Goleta", "Goleta (ABD)",
                       ifelse(NAME == "Jerry Slough (ABD)", "Jerry Slough",
                              ifelse(NAME == "Newgate", "Newgate (ABD)", NAME))))
  

field_boundaries3 <- field_boundaries2 %>%
  left_join(n_wells2) %>%
  rename(n_wells = n)

# save output

write_csv(field_boundaries3, file.path(home, "/data/processed/setback_coverage_R.csv")) 


## save maps for examining

coverage_map <-  
  mapview(field_boundaries, layer.name = "Field boundary", label = 'doc_field_code', col.regions = "yellow", legend = FALSE) +
  mapview(buff1000, layer.name = "1000ft", col.regions = "blue", legend = FALSE) +
  mapview(buff2500, layer.name = "2500ft", col.regions = "grey", legend = FALSE) +
  mapview(buff5280, layer.name = "5280ft", col.regions = "red", legend = FALSE) +
  mapview(field_coverage_df_1000, layer.name = "1000ft coverage", col.regions = "green", legend = FALSE) +
  mapview(field_coverage_df_2500, layer.name = "2500ft coverage", col.regions = "purple", legend = FALSE) +
  mapview(field_coverage_df_5280, layer.name = "5280ft coverage", col.regions = "orange", legend = FALSE) +
  mapview(wells2, layer.name = "Wells", label = 'WellStatus', cex = 0.3, alpha = 0, legend = FALSE)
  
# save output
mapshot(coverage_map, url = paste0(home, "data/processed/coverage_map.html"), selfcontained = F) 



  mapview(field_boundaries %>% filter(doc_field_code %in% c('660')), layer.name = "Field boundary", label = 'doc_field_code', col.regions = "yellow", legend = FALSE) +
  mapview(buff1000, layer.name = "1000ft", col.regions = "blue", legend = FALSE) +
  mapview(buff2500, layer.name = "2500ft", col.regions = "grey", legend = FALSE) +
  mapview(buff5280, layer.name = "5280ft", col.regions = "red", legend = FALSE) +
  mapview(field_coverage_df_1000, layer.name = "1000ft coverage", col.regions = "green", legend = FALSE) +
  mapview(field_coverage_df_2500, layer.name = "2500ft coverage", col.regions = "purple", legend = FALSE) +
  mapview(field_coverage_df_5280, layer.name = "5280ft coverage", col.regions = "orange", legend = FALSE) +
  mapview(wells2 %>% filter(FieldName == "Sansinena"), layer.name = "Wells", label = 'WellStatus', cex = 0.3, alpha = 0, legend = FALSE)

# # read in original output
# orig_coverage <- read_csv(file.path(home, "emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/setback_coverage.csv")) %>%
#   rename(orig_area_coverage = area_coverage)
# 
# 
# output <- read_csv(paste0(home, "/emlab/projects/current-projects/calepa-cn/outputs/setback/model-inputs/setback_coverage_R.csv")) %>%
#   rename(revised_coverage = percent_coverage)
# 
# comp_coverage <- full_join(output, orig_coverage) %>%
#   mutate(diff = revised_coverage - orig_area_coverage) %>%
#   filter(!is.na(orig_area_coverage),
#          diff < -0.01 | diff > 0.01)


## do test with doc field 660
  mapview(field_boundaries %>% filter(doc_field_code == '660'), layer.name = "Field boundary", label = 'doc_field_code', col.regions = "yellow", legend = FALSE) +
    mapview(buff2500, layer.name = "2500ft", col.regions = "grey", legend = FALSE) +
    mapview(buff5280, layer.name = "5280ft", col.regions = "red", legend = FALSE)


test <- field_boundaries %>% 
  filter(doc_field_code == '660') %>%
  st_intersection(buff2500) %>%
  mutate(setback_area = st_area(geometry),
         diff = orig_area_m2 - setback_area)
