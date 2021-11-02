## Tracey Mangin
## October 28, 2021
## production locations

## libraries
library(data.table)
library(tidyverse)
library(purrr)
library(readxl)
library(openxlsx)
library(sf)
library(maps)


## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path <- 'outputs/academic-out/extraction/extraction_2021-10-22/'
data_path  <-'data/stocks-flows/processed/'

## county outputs
county_file <- "county-results/subset_county_results.csv"

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## ggplot plot, production
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)

## read in county outputs
county_out <- fread(paste0(main_path, extraction_folder_path, county_file))

## bau
bau_out <- county_out[(oil_price_scenario == "reference case" &
                       carbon_price_scenario == "price floor" &
                       ccs_scenario == "medium CCS cost" &
                       setback_scenario == "no_setback" &
                       excise_tax_scenario == "no tax" & year %in% c(2020, 2021)), .(scen_id, county, year, total_county_ghg_kgCO2e)]


bau_out[, year := paste0("X", year)]
bau_out <- dcast(bau_out, scen_id + county  ~ year, value.var = "total_county_ghg_kgCO2e")

bau_out[, diff := X2021 - X2020]

bau_map <- bau_out %>%
  left_join(county_boundaries, c("county" = "adj_county_name")) %>%
  mutate(diff2 = diff/ (1e6 * 1e3))

## spatial fig - bbls
comp_ghg <- ggplot() +
  geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
  geom_sf(data = bau_map, mapping = aes(geometry = geometry, fill = diff2), lwd = 0.25, show.legend = TRUE) +
  scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = 'GHG diff: 2021 vs 2020',
       fill = 'MtCO2e',
       x = NULL,
       y = NULL) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank()) 


