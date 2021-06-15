## Tracey Mangin
## May 27, 2021
## Well density

library(tidyverse)
library(sf)
library(data.table)
library(maps)
library(raster)
library(mapview)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(viridis)

# comment out and add your own machine's file path
proj_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"

prod_file    <- "well_prod_m_processed.csv"

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

# transform to NAD83(NSRS2007) / California Albers as well for wells and field boundaries
wells <- sf::st_read(file.path(proj_dir, "data/GIS/raw/allwells_gis/Wells_All.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(API, WellStatus, doc_fieldname = FieldName) %>%
  unique() %>%
  st_drop_geometry()

# transform to NAD83(NSRS2007) / California Albers as well for wells and field boundaries
wells_sp <- sf::st_read(file.path(proj_dir, "data/GIS/raw/allwells_gis/Wells_All.shp")) %>% 
  st_transform(ca_crs)

## read in outputs from most recent run
outputs <- fread(paste0(proj_dir, "outputs/predict-production/extraction_2021-06-08/revised-new-entry-model/diagnostic-field-level-results.csv"), colClasses = c('doc_field_code' = 'character'))

## setback
setback_out <- fread(paste0(proj_dir, "outputs/setback/model-inputs/setback_coverage_R.csv"), colClasses = c('doc_field_code' = 'character'))

well_setback_out <- fread(paste0(proj_dir, "outputs/setback/model-inputs/wells_in_setbacks_revised.csv"), colClasses = c('api_ten_digit' = 'character'))

## boundaries
boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)

ggplot() +
  geom_sf(data = boundaries %>% filter(AREA_SQ_MI == 0)) 

## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character'))

## fields with production > 0 in historic time period
pos_fields <- well_prod %>%
  group_by(doc_field_code) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

## field name and field code combos
field_codes <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique() %>%
  filter(doc_field_code %in% pos_fields$doc_field_code)

## calculate area
field_area <- boundaries %>%
  rename(doc_field_code = FIELD_CODE) %>%
  mutate(area = st_area(geometry),
         area_km2 = units::drop_units(area) / 1e6) %>%
  dplyr::select(NAME, doc_field_code, AREA_SQ_MI, area, area_km2) %>%
  st_drop_geometry()

## wells, filter, join
# setDT(wells)
# wells[, gas_field := str_detect(doc_fieldname, "Gas")][, gas_field := ifelse(gas_field == TRUE, 1, 0)]
# wells <- wells[gas_field == 0]
# wells[, gas_field := NULL]

## check production by well status
wells_merge <- wells[, c("API", "WellStatus")]
setnames(wells_merge, "API", "api_ten_digit")

well_prod_status <- well_prod %>%
  left_join(wells_merge) %>%
  group_by(api_ten_digit, year, WellStatus) %>%
  summarise(prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()
## positive production from 'canceled' wells.

## calc percentage of prod from canceled wells by year
well_prod_status2 <- well_prod_status %>%
  group_by(WellStatus, year) %>%
  summarise(prod_bbl = sum(prod_bbl)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_bbl = sum(prod_bbl)) %>%
  ungroup() %>%
  mutate(rel_prod = prod_bbl / total_bbl)

well_prod_status3 <-well_prod %>%
  left_join(wells_merge) %>%
  group_by(doc_field_code, WellStatus, year) %>%
  summarise(prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, year) %>%
  mutate(total_bbl = sum(prod_bbl)) %>%
  ungroup() %>%
  mutate(rel_prod = prod_bbl / total_bbl)


wells2 <- as.tibble(wells) %>%
  filter(!WellStatus %in% c("Abeyance"))

## get ready to rename
anti_join(wells2 %>% dplyr::select(doc_fieldname) %>% unique(), field_codes %>% dplyr::select(doc_fieldname) %>% unique())
anti_join(field_codes %>% dplyr::select(doc_fieldname) %>% unique(), wells2 %>% dplyr::select(doc_fieldname) %>% unique())
anti_join(wells2 %>% dplyr::select(doc_fieldname) %>% unique(), boundaries %>% dplyr::select(NAME) %>% rename(doc_fieldname = NAME) %>% unique())


# recode(column_to_fix, "thing thats wrong"="thing thats right",

## group by field, count
n_wells_field <- wells2 %>%
  rename(NAME = doc_fieldname) %>%
  mutate(NAME = ifelse(NAME == "Goleta", "Goleta (ABD)",
                       ifelse(NAME == "Jerry Slough (ABD)", "Jerry Slough",
                              ifelse(NAME == "Newgate", "Newgate (ABD)", 
                                     ifelse(NAME == "Wilson Creek Gas", "Wilson Creek Gas (ABD)", 
                                            ifelse(NAME == "Collegeville, East, Gas", "Collegeville, East, Gas  Gas", NAME)))))) %>%
  group_by(NAME) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(field_area) %>%
  mutate(density_km2 = n / area_km2) %>%
  mutate(prod_field = ifelse(doc_field_code %in% pos_fields$doc_field_code, 1, 0)) %>%
  filter(prod_field == 1)
  
density_fig <- ggplot(n_wells_field, aes(y = density_km2, x = area_km2)) +
  geom_point(alpha = 0.7) +
  ylab("density (wells/km2)") +
  xlab("field area (km2)") +
  geom_text(label = n_wells_field$NAME, 
            # nudge_x = 1, nudge_y = 1, 
            check_overlap = T) +
  theme_bw()

ggsave(density_fig, 
       filename = file.path(proj_dir, 'model-development/density/density_fig.png'), 
       width = 8, 
       height = 7)


## well density throughout state
## --------------------------------------

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

st_crs(california) == st_crs(wells_sp)

r <- raster(california, ca_crs, res = 1000)

## rasterize
r_ca <- rasterize(california, r)

wells_sp_density <- wells_sp %>%
  filter(WellStatus != "Abeyance")

# well_raster <- rasterize(wells_sp_density %>% dplyr::select(geometry), r, fun='count', background=0)
well_raster <- rasterize(wells_sp_density %>% dplyr::select(geometry), r, fun='count')


plot(r_ca)
# plot(boundaries, col = "grey", add = TRUE)
plot(well_raster, add = TRUE)

## frequency
## ------------------

f <- freq(well_raster, useNA = 'no')
f <- as.tibble(f)
head(f)

## -------------------

well_density_df <- as.data.frame(well_raster, xy = TRUE)

well_density_df1 <- well_density_df %>%
  mutate(ID = row_number())

well_density_df2 <- well_density_df %>%
  mutate(ID = row_number()) %>%
  dplyr::select(x, y, ID)
 
well_id_raster <- rasterFromXYZ(well_density_df2)

extract_vec <- extract(well_id_raster, wells_sp)

wells_sp$cell_id <- extract_vec


## plot it
## --------------------------

raster_fig <- 
ggplot() +
  geom_sf(data = california, fill = NA, color = "grey50", size = 1) +
  geom_raster(data = well_density_df %>% filter(!is.na(layer)), aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(name = "# of wells", option = "plasma") 
  

ggsave(raster_fig, 
       filename = file.path(proj_dir, 'model-development/density/wells_per_km2.png'), 
       width = 11, 
       height = 10)





## dense area
max_id <- well_density_df1 %>% 
  filter(layer == max(layer, na.rm = T)) %>%
  dplyr::select(ID) %>%
  as.numeric()

## wells in dense area
dense_wells <- wells_sp %>%
  filter(cell_id == max_id) %>%
  dplyr::select(API, FieldName, WellStatus, cell_id)

mapview(dense_wells, cex = 0.5) 

## now look at well density by field for the three scenarios
## ---------------------------------------------------------

## add scenario name
outputs[, scenario := fifelse(setback_scenario == "setback_2500ft", "LCE2",
        fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

new_well_df <- outputs[, .(new_wells = sum(new_wells, na.rm = T)), by = .(scenario, doc_field_code)]

setback_out2 <- setback_out %>%
  dplyr::select(doc_field_code, setback_scenario, rel_coverage) %>%
  filter(setback_scenario %in% c("no_setback", "setback_2500"))

well_setback_out2 <- well_setback_out %>%
  dplyr::select(api_ten_digit, setback_scenario, within_setback)

## join to n_wells_field
## ----------------------------------

## group by field, count
n_wells_new <- wells2 %>%
  rename(NAME = doc_fieldname) %>%
  mutate(NAME = ifelse(NAME == "Goleta", "Goleta (ABD)",
                       ifelse(NAME == "Jerry Slough (ABD)", "Jerry Slough",
                              ifelse(NAME == "Newgate", "Newgate (ABD)", 
                                     ifelse(NAME == "Wilson Creek Gas", "Wilson Creek Gas (ABD)", 
                                            ifelse(NAME == "Collegeville, East, Gas", "Collegeville, East, Gas  Gas", NAME)))))) %>%
  rename(api_ten_digit = API) %>%
  left_join(well_setback_out2) %>%
  left_join(field_area) %>%
  filter(setback_scenario %in% c("no_setback", "setback_2500")) %>%
  group_by(NAME, setback_scenario, doc_field_code, area_km2) %>%
  summarise(n = n(),
            n_in_setback = sum(within_setback)) %>%
  ungroup() 



###
n_wells_new2 <- new_well_df %>%
  mutate(setback_scenario = ifelse(scenario == "LCE2", "setback_2500", "no_setback")) %>%
  left_join(n_wells_new) %>%
  mutate(n_wells_current = n - n_in_setback) %>%
  dplyr::select(scenario, doc_field_code, NAME, n_wells_current, new_wells, area_km2, setback_scenario) %>%
  left_join(setback_out2) %>%
  mutate(setback_area_km2 = area_km2 * (1 - rel_coverage)) %>%
  mutate(total_wells = n_wells_current + new_wells) %>%
  mutate(density_2045 = total_wells / setback_area_km2,
         above_max = ifelse(density_2045 > 2674, "over", "not over"))
  

density_fut_fig <- ggplot(n_wells_new2, aes(y = density_2045, x = setback_area_km2, color = above_max)) +
  geom_point(alpha = 0.7) +
  ylab("density (wells/km2)") +
  xlab("field area (km2)") +
  facet_wrap(~scenario, scales = "free") +
  scale_color_manual(values = c("black", "red")) +
  # geom_text(label = n_wells_field$NAME, 
  #           # nudge_x = 1, nudge_y = 1, 
  #           check_overlap = T) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(density_fut_fig, 
       filename = file.path(proj_dir, 'model-development/density/density_future_fig_updated.png'), 
       width = 12, 
       height = 8)


## check the two wells in Las Cienegas
check_wells <- c("0403700381", "0403700328")

mapview(wells_sp %>% filter(API %in% check_wells), cex = 0.5, color = "yellow") +
  mapview(boundaries)




  