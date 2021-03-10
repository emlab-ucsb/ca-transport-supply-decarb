## Tracey Mangin
## December 14, 2020
## Well density with setbacks

library(tidyverse)
library(sf)
library(plotly)


## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items



## paths
sp_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/"
output_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## -----------------------------------------------------------------
proj <- "+proj=longlat +datum=WGS84"

## read in data
fields_sf <- read_sf(dsn = paste0(sp_path, "raw/field-boundaries/", layer = "DOGGR_Admin_Boundaries_Master.shp")) %>%
  st_transform(proj) 

sb_coverage <- read_csv(paste0(output_path, "setback/model-inputs/setback_coverage.csv"))

nwells <- read_csv(paste0(output_path, "predict-production/production_with_exit/n_well_setbacks.csv"))

field_out <- read_csv(paste0(output_path, "predict-production/scenarios_20_all_scens/compiled/field_level_prod_emissions_2020-2045.csv"))

## create df with number of old wells, number of new wells, area, coverage, by year by field
## calculate density (n wells / area miles)

field_area <- fields_sf %>%
  select(FIELD_CODE, AREA_SQ_MI) %>%
  unique() %>%
  rename(doc_field_code = FIELD_CODE) %>%
  st_set_geometry(NULL)

nwells_field <- nwells %>%
  group_by(setback_scenario, doc_field_code, doc_fieldname) %>%
  summarise(n_wells_f = sum(n_wells),
            n_wells_in_setback_f = sum(n_wells_in_setback),
            adj_n_wells_f = sum(adj_no_wells)) %>%
  ungroup()

## n existing wells
nwells_existing <- nwells_field %>%
  filter(setback_scenario == "no_setback") %>%
  select(doc_field_code, n_wells = n_wells_f)


## calculate current density, no setbacks
density0 <- nwells_field %>%
  left_join(field_area) %>%
  filter(setback_scenario == "no_setback") %>%
  mutate(density_orig = adj_n_wells_f / AREA_SQ_MI)

ggplot(density0, aes(x = density_orig)) +
  geom_histogram(binwidth = 5) +
  xlab("Number of wells per square mile") +
  ylab("count (n fields)")
  

# field_area_adj <- sb_coverage %>%
#   select(-X1) %>%
#   rename(doc_field_code = FieldCode) %>%
#   left_join(field_area) %>%
#   mutate(adj_area_sq_mi = AREA_SQ_MI * (1 - area_coverage)) %>%
#   left_join(nwells_field) %>%
#   select(doc_field_code, setback_scenario, AREA_SQ_MI, area_coverage, adj_area_sq_mi, n_wells_f, n_wells_in_setback_f, adj_n_wells_f)


field_area_adj2 <- sb_coverage %>%
  select(-X1) %>%
  rename(doc_field_code = FieldCode) %>%
  left_join(field_area) %>%
  mutate(adj_area_sq_mi = AREA_SQ_MI * (1 - area_coverage)) %>%
  left_join(nwells_existing) %>%
  rename(existing_wells_in_area = n_wells)


##  
# field_out2 <- field_out %>%
#   select(year:well_type, n_wells) %>%
#   group_by(year, doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
#            setback_scenario, prod_quota_scenario, excise_tax_scenario, well_type) %>%
#   summarise(n_wells = sum(n_wells, na.rm = T)) %>%
#   ungroup() %>%
#   left_join(field_area_adj2)
  
# ## density
# density_df <- field_out2 %>%
#   mutate(density_nwell_sqmi = n_wells / adj_area_sq_mi)
# 
# ## check well count accurcay
# cascade <- field_out %>%
#   filter(doc_field_code == 120)

## focus on 2045, excise tax == 0
field_out2 <- field_out %>%
  select(year:well_type, n_wells, production_bbl) %>%
  arrange(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
          setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
  group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario, well_type) %>%
  mutate(cumul_prod = cumsum(replace_na(production_bbl, 0))) %>%
  ungroup() 

field_out3 <- field_out2 %>%
  filter(year == 2045,
         excise_tax_scenario == "no tax") %>%
  left_join(field_area_adj2) %>%
  mutate(adj_n_wells = ifelse(well_type == "new", n_wells * (1 - area_coverage), existing_wells_in_area)) %>%
  group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
  summarise(total_wells = sum(adj_n_wells, na.rm = T),
            cumul_prod_all = sum(cumul_prod, na.rm = T),
            area_sq_mi_0 = unique(AREA_SQ_MI),
            area_sq_mi_adj = unique(adj_area_sq_mi)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(density_n_wells_sqmi = total_wells / area_sq_mi_adj) %>%
  ungroup() %>%
  mutate(scenario_name = ifelse(oil_price_scenario == "iea oil price" &
                                carbon_price_scenario == "price floor" &
                                innovation_scenario == "low innovation" &
                                ccs_scenario == "medium CCS cost" &
                                excise_tax_scenario == "no tax" &
                                prod_quota_scenario == "no quota" &
                                setback_scenario %in% c("no_setback"), "BAU",
                                ifelse(oil_price_scenario == "iea oil price" &
                                       carbon_price_scenario == "price floor" &
                                       innovation_scenario == "low innovation" &
                                       ccs_scenario == "medium CCS cost" &
                                       excise_tax_scenario == "no tax" &
                                       prod_quota_scenario == "quota_20" &
                                       setback_scenario %in% c("no_setback"), "LCE1",
                                       ifelse(oil_price_scenario == "iea oil price" &
                                              carbon_price_scenario == "price floor" &
                                              innovation_scenario == "low innovation" &
                                              ccs_scenario == "medium CCS cost" &
                                              excise_tax_scenario == "no tax" &
                                              prod_quota_scenario == "quota_20" &
                                              setback_scenario %in% c("setback_2500ft"), "LCE2", "Other"))))


field_out4 <- field_out3 %>%
  mutate(setback = ifelse(setback_scenario == "no_setback", "No setback", "Setback"))


density_plot <- ggplot(field_out4 %>% filter(density_n_wells_sqmi != Inf), aes(y = cumul_prod_all / 1e9, x = density_n_wells_sqmi, color = setback_scenario)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~setback, ncol = 2, scales = "free") +
  xlab("Well density (wells per sq mile)") +
  ylab("Cumulative production (billion bbls)") +
  base_theme +
  theme(axis.title.x = element_text(size = 11))

density_plot <- ggplot(field_out4 %>% filter(density_n_wells_sqmi != Inf,
                                             density_n_wells_sqmi <= 5000), aes(y = cumul_prod_all / 1e6, x = density_n_wells_sqmi, color = setback_scenario)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~setback, ncol = 2, scales = "free") +
  xlab("Well density (wells per sq mile)") +
  ylab("Cumulative production (million bbls)") +
  base_theme +
  theme(axis.title.x = element_text(size = 11))

density_plot_scens <- ggplot(field_out4 %>% filter(density_n_wells_sqmi != Inf,
                                             scenario_name != "Other",
                                             density_n_wells_sqmi <= 3000), aes(y = cumul_prod_all / 1e6, x = density_n_wells_sqmi, color = setback_scenario, shape = scenario_name)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~setback, ncol = 2, scales = "free") +
  xlab("Well density (wells per sq mile)") +
  ylab("Cumulative production (million bbls)") +
  base_theme +
  theme(axis.title.x = element_text(size = 11))


max_noset <- field_out4 %>%
  filter(setback == "No setback",
         density_n_wells_sqmi != Inf) 

max_val <- max(max_noset$density_n_wells_sqmi)

sb_above_threshold <- field_out4 %>%
  filter(setback == "Setback",
         density_n_wells_sqmi != Inf,
         density_n_wells_sqmi > max_val * 5)


x <- ggplot(field_out2 %>% filter(scenario_name != "Other"), aes(x = adj_area_sq_mi, y = density_n_wells_sqmi, color = scenario_name, size = n_wells_total)) +
  geom_point() +
  xlab("Field area (after setback)") +
  ylab("Number of wells per square mile") +
  base_theme

ggplotly(x)


ggplot(field_out2 %>% filter(density_n_wells_sqmi <= 1500), aes(x = density_n_wells_sqmi)) +
  geom_histogram(bins = 100) +
  base_theme +
  xlab("Number of wells per square mile")

ggplot(field_out2 %>% filter(density_n_wells_sqmi <= 1500, setback_scenario == "no_setback"), aes(x = density_n_wells_sqmi)) +
  geom_histogram(bins = 100) +
  base_theme +
  xlab("Number of wells per square mile")


large_density <- field_out2 %>% filter(density_n_wells_sqmi > 1500)



