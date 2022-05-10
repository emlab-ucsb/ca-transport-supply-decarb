## Tracey Mangin
## May 2, 2022
## cluster fig for SI


library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(gridExtra)
library(readxl)
library(hrbrthemes)
library(extrafont)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
fig_path <- 'outputs/academic-out/extraction/figures/all-oil-px/'
#data_path         <- paste0(main_path, 'outputs/entry-model-results/')

## projection
ca_crs <- 3488

## counties, no islands
CA_counties <- st_read(paste0(main_path, "data/GIS/raw/CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp")) %>%
  st_transform(ca_crs)

## remove islands
CA_counties_noisl <- CA_counties %>%
  filter(!OBJECTID %in% c(3, 49))

## clusters
extraction_clusters <- st_read(paste0(main_path, "outputs/academic-out/extraction/extraction_fields_clusters_10km.shp")) %>%
  st_transform(ca_crs)

## clusters plot
cluster_fig <- ggplot() +
  geom_sf(data = extraction_clusters, aes(geometry = geometry), color= "black", lwd = 0.5) + 
  theme_void() + 
  labs(fill = NULL) +
  geom_sf(data = CA_counties_noisl, aes(geometry = geometry), fill = "transparent", color = "grey", lwd = 0.25) +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = "none",
    plot.margin = margin(0, 2, 0, 8),
    plot.title = element_text(face = 'bold', size = 5, hjust = -0.05))

ggsave(total_pm25,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/fig1c.png'),
       width = 50,
       height = 55,
       dpi = 300,
       units = "mm")

ggsave(total_pm25,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/fig1c.pdf'),
       width = 50,
       height = 55,
       dpi = 300,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/fig1c.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/fig1c.pdf'))



