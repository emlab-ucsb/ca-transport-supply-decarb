## Tracey Mangin
## December 6, 2021
## fig 1: map of CA


library(tidyverse)
library(sf)
library(maps)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## califonia
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)


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

## add field locations instead of wells

## make map
fig1_map <- ggplot() +
  geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
  geom_sf(data = wells_sp, mapping = aes(geometry = geometry, color = well_status_adj), lwd = 0, alpha = 0.25, show.legend = TRUE) +
  geom_sf(data = dac_sp %>% filter(), mapping = aes(geometry = geometry, fill = ct_type), lwd = 0, show.legend = TRUE) +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = NULL,
       color = 'Well status: ',
       fill = NULL,
       x = NULL,
       y = NULL) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank()) 



