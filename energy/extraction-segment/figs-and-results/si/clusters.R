## Tracey Mangin
## December 27, 2022
## clusters


library(tidyverse)
library(sf)
library(maps)
# library(viridis)
# library(gridExtra)
# library(rebus)
# library(readxl)
library(data.table)
# library(cowplot)
library(hrbrthemes)
library(extrafont)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/nature-energy-revision/final/figs/si/'

## projection for ca: transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## plot title theme
plot_title_theme <- theme_ipsum(base_family = 'Arial',
                                grid = 'Y', 
                                plot_title_size = 10, 
                                subtitle_size = 9,
                                axis_title_just = 'center',
                                axis_title_size = 9, 
                                axis_text_size = 9,
                                strip_text_size = 9)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot')



## califonia
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)

## counties, no islands
CA_counties <- st_read(paste0(main_path, "data/GIS/raw/CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp")) %>%
  st_transform(ca_crs)

## remove islands
CA_counties_noisl <- CA_counties %>%
  filter(!OBJECTID %in% c(3, 49))

## extraction clusters
extraction_clusters <- st_read(paste0(main_path, "outputs/academic-out/extraction/extraction_fields_clusters_10km.shp")) %>%
  st_transform(ca_crs)

# cluster_bound <- extraction_clusters %>% dplyr::filter(OBJECTID == cluster_numb) %>%
#   mutate(name = "Field cluster")


## figure
cluster_fig <- ggplot() +
  # geom_sf(data = california, color = "grey", fill = "white") + 
  theme_void() + 
  geom_sf(data = extraction_clusters, aes(fill = as.character(OBJECTID)), alpha = 0.8) +
  geom_sf(data = CA_counties_noisl, color = "grey", fill = NA) +
  geom_sf_text(data = extraction_clusters, aes(label = OBJECTID), size = 2, hjust = 1, vjust = 1) +
  theme_void() +
  theme(legend.position = "none") 
  

ggsave(cluster_fig,
       filename = file.path(main_path, fig_path, 'cluster_si_fig.png'),
       width = 90,
       height = 110,
       dpi = 300,
       units = "mm")

ggsave(cluster_fig,
       filename = file.path(main_path, fig_path, 'cluster_si_fig.pdf'),
       width = 90,
       height = 110,
       units = "mm",
       dpi = 300,
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'cluster_si_fig.pdf'),
            outfile = paste0(main_path, fig_path, 'cluster_si_fig.pdf'))



