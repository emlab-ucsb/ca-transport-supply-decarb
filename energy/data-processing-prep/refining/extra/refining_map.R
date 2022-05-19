## Tracey Mangin
## June 2, 2020
## Refining map

## attach libraries
library(tidyverse)
library(sf)
library(mapview)
library(maps)
library(geosphere)
library(rebus)


## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/"
sp_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/GIS/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## read in the data
refinery_df <- read_csv(paste0(data_directory, "processed/refinery_capacity.csv"))

## clusters
north <- c("Richmond", "Martinez", "Benicia", "San Francisco", "Bakersfield")

south <- c("Carson", "El Segundo", "Torrance", "Wilmington", "Santa Maria", "South Gate")


## counties 
county_df <- tibble(location = c("Bakersfield", "San Francisco", "Benicia", "Martinez", "Richmond", "Wilmington", "South Gate", "Santa Maria",
                                 "Torrance", "El Segundo", "Carson"),
                    county = c("Kern", "Contra Costa", "Solano County", "Contra Costa", "Contra Costa", "Los Angeles", "Los Angeles", "Santa Barbara",
                               "Los Angeles", "Los Angeles", "Los Angeles"))

## refinery capacity
refinery_cap <- refinery_df %>%
  mutate(cap_trade = ifelse(carb_diesel == "Yes" & carb_gasoline == "Yes", "Diesel and gasoline",
                            ifelse(carb_diesel == "No" & carb_gasoline == "No", "Neither", "Diesel")),
         location = str_extract(refinery_name, pattern = ", " %R% one_or_more(WRD)),
         location = str_remove(location, pattern = START %R% ", ")) %>%
  select(site_id, refinery_name:notes, cap_trade, location) %>%
  mutate(location =  ifelse(location == "El", "El Segundo",
                            ifelse(location == "Golden", "Martinez",
                                   ifelse(location == "Rodeo", "San Francisco",
                                          ifelse(location == "Santa", "Santa Maria",
                                                 ifelse(location == "South", "South Gate", location)))))) %>%
  mutate(location = ifelse(refinery_name == "Valero Wilmington Asphalt Refinery", "Wilmington", location),
         cluster = ifelse(location %in% north, "North", "South"),
         refinery_name = str_remove(refinery_name, pattern = one_or_more("*"))) %>%
  arrange(cluster, barrels_per_day) %>%
  left_join(county_df) %>%
  mutate(label_name = paste0(refinery_name, " (", county, ")"))

refinery_cap$label_name <- factor(refinery_cap$label_name, levels = refinery_cap$label_name)


## p
proj <- "+proj=longlat +datum=WGS84"

## fields
refineries_sf <- read_sf(dsn = paste0(sp_dir, "raw/Petroleum_Refineries_US_EIA/", layer = "Petroleum_Refineries_US_2019_v2.shp")) %>%
  st_transform(proj) %>%
  filter(State == "California") %>%
  left_join(refinery_cap)

## save df without geometry
refinery_loc_cap <- refineries_sf %>%
  select(-geometry) %>%
  as_tibble()

write_csv(refinery_loc_cap, paste0(data_directory, "processed/refinery_loc_cap.csv"))


## counties
counties_sf <- read_sf(dsn = paste0(sp_dir, "raw/CA_Counties/", layer = "CA_Counties_TIGER2016.shp")) %>%
  st_transform(proj) %>%
  select(NAME, NAMELSAD, geometry)

## california
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

test <- 
ggplot(data = states %>% filter(ID == "california")) +
  geom_sf(fill = "lightgrey") +
  geom_sf(data = counties_sf, fill = NA, color = gray(.5)) +
  # geom_sf(data = refineries_sf, aes(size = barrels_per_day, fill = cap_trade), shape = 1) +
  geom_point(data = refineries_sf, aes(x = Longitude, y = Latitude, size = barrels_per_day, color = cap_trade), alpha = 0.6) +
  scale_size(range = c(1,15)) +
  # geom_text(data = refineries_sf, aes(x = Longitude, y = Latitude + 0.5 , label = Company), 
  #           size = 3.9, col = "black", fontface = "bold") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
# +
  # coord_sf(xlim = c(-124.5, -114), ylim = c(42.5, 32), expand = FALSE) +

ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/drafts/stocks-flows/test.pdf"), test, width = 12, height = 12, units = "in", dpi = 300)



