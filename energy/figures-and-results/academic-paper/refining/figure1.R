## Tracey Mangin
## December 8, 2021
## Fig 1 - refining

library(tidyverse)
library(sf)
library(maps)
library(viridis)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/'

## files
forecast_file     <- 'field_capex_opex_forecast_revised.csv'
entry_file        <- 'entry_df_final_revised.csv'
ghg_file          <- 'ghg_emissions_x_field_2018-2045.csv'
setback_file      <- 'setback_coverage_R.csv'
prod_file         <- "well_prod_m_processed.csv"

## projection for ca: transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488


## read in dfs
## ------------------

## well prod
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

field_prod <- well_prod[year == 2019, .(prod_2019 = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code)]

## county prod (top 5 2019)
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore")) 

county_prod <- merge(well_prod, county_lut,
                     by = c("doc_field_code", "county_name"),
                     all.x = T)

county_prod <- county_prod[year == 2019, .(prod_2019 = sum(OilorCondensateProduced, na.rm = T)), by = .(adj_county_name)]
county_prod[, rank := rank(-prod_2019)]
county_prod <- county_prod[rank <= 5]
county_prod[, lab := paste0(adj_county_name, " ", round(prod_2019 / 1e6), "m bbls")]


## load entry df 
entry_data = fread(file.path(main_path, 'outputs/stocks-flows', entry_file), header = T)
entry_data[, doc_field_code := sprintf("%03d", doc_field_code)]
entry_data <- entry_data[year == 2019, .(doc_field_code, capex_imputed, opex_imputed)]
entry_data[, sum_cost := capex_imputed + opex_imputed]

## load opex/ capex forecast
price_data = fread(file.path(main_path, 'outputs/stocks-flows', forecast_file), header = T)
price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
price_data[, sum_cost := m_opex_imputed + m_capex_imputed]
price_data <- price_data[year == 2020, .(doc_field_code, m_opex_imputed, m_capex_imputed, sum_cost)]

## emissions factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T)
ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]
ghg_factors <- ghg_factors[year == 2019, .(doc_field_code, upstream_kgCO2e_bbl)]

## setback coverage
setback_scens = fread(file.path(main_path, 'outputs/setback', 'model-inputs', setback_file), header = T, colClasses = c('doc_field_code' = 'character'))
setnames(setback_scens, 'rel_coverage', 'area_coverage')
setback_scens <- setback_scens[setback_scenario != "no_setback", .(doc_field_code, setback_scenario, area_coverage)]

## combine by field
field_df <- merge(ghg_factors, entry_data[, .(doc_field_code, sum_cost)],
                  by = "doc_field_code",
                  all.x = T)

field_df <- merge(field_df, field_prod,
                  by = "doc_field_code",
                  all.x = T)

field_df <- merge(field_df, setback_scens,
                  by = "doc_field_code",
                  all.y = T)

field_df[, prod_2019 := fifelse(is.na(prod_2019), 0, prod_2019)]

## make long format
field_df_long <- pivot_longer(field_df[, .(doc_field_code, upstream_kgCO2e_bbl,
                                           sum_cost, prod_2019)], cols = upstream_kgCO2e_bbl:prod_2019,
                              names_to = "metric",
                              values_to = "value")


## fields
field_boundaries <- st_read(file.path(main_path, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% 
  st_transform(3488) %>%
  select(doc_field_code = FIELD_CODE)

## add geometry
field_df_long <- merge(field_df_long, field_boundaries,
                       by = "doc_field_code",
                       all.x = T)


## califonia
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)

county_19 <- merge(county_prod, county_boundaries,
                   by = "adj_county_name",
                   all.x = T)

county_19 <- unnest(county_19, cols = c(geometry))


## wells
census_tracts <- st_read(file.path(main_path, "data/GIS/raw/census-tract/tl_2019_06_tract.shp")) %>% 
  st_transform(ca_crs) %>%
  rename(census_tract = GEOID) %>%
  select(census_tract)

## DAC and CES
dac_ces <- read_xlsx(paste0(main_path, 'data/health/raw/ces3results.xlsx'))

## dac
dac_ces <- dac_ces %>%
  select(`Census Tract`, `SB 535 Disadvantaged Community`) %>%
  rename(census_tract = `Census Tract`,
         dac = `SB 535 Disadvantaged Community`) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  mutate(ct_type = ifelse(dac == "Yes", "DAC", "Not DAC"))

## dac sp
dac_sp <- left_join(census_tracts, dac_ces)

## dac only
dac_areas <- dac_sp %>%
  filter(dac == "Yes")





## make map
fig1_map <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "#FFFAF5", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
  geom_sf(data = field_df_long %>% filter(metric == "prod_2019"), mapping = aes(geometry = geometry, fill = value / 1e6), lwd = 0, alpha = 0.8, show.legend = TRUE) +
  geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = NULL,
       fill = '2019 oil production\n(million bbls)',
       x = NULL,
       y = NULL) +
  scale_fill_viridis(option="plasma",
                     direction = -1) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0, 0.3),
    legend.title = element_text(size = 9)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))

ggsave(fig1_map,
       filename = file.path(main_path, fig_path, 'figure1_prod.png'),
       width = 6,
       height = 6,
       units = "in")