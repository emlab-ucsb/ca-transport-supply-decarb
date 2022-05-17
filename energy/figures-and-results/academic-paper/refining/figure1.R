## Tracey Mangin
## May 11, 2022
## Refining fig 1: map of CA


library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(gridExtra)
library(rebus)
library(readxl)
library(data.table)
library(cowplot)
library(hrbrthemes)
library(extrafont)
library(xlsx)

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
ct_out_path <- 'outputs/academic-out/refining/refining_2021-11-22/census-tract-results/'
refin_out_path <- 'outputs/academic-out/refining/refining_2021-11-22/'
fig_path <- 'outputs/academic-out/refining/figures/'


## for SRM figure
outputFiles   <- "outputs/academic-out"
sourceFiles   <- "data/health/source_receptor_matrix"
inmapRefFiles  <- "health/source_receptor_matrix/inmap_processed_srm/refining"

## files
refinery_locs <- 'Petroleum_Refineries_US_2019_v2.shp'
refinery_plus_locs <- '/data/stocks-flows/processed/refinery_lat_long_revised.csv'
refin_manual_file <- 'refinery_loc_cap_manual.csv'
site_out_file <- 'site_refining_outputs.csv'
refin_cap_file <- 'refinery_loc_cap.csv'
# ct_file           <- "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_ct_results.rds"
# county_file       <- "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_county_results.rds"


## projection for ca: transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items

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



## read in dfs
## ------------------

## Manual refineries capacity
refin_locs_manuel <- fread(paste0(main_path, "/data/stocks-flows/processed/", refin_manual_file))

man_capacity <- refin_locs_manuel %>%
  filter(site_id %in% c(3422, 342)) %>%
  select(site_id, barrels_per_day)


## Refineries
refin_locations <- st_read(paste0(main_path, "data/GIS/raw/Petroleum_Refineries_US_EIA/", refinery_locs)) 

refin_crs <- st_crs(refin_locations)

## capacities
renewable_capacity <- setDT(read.xlsx(paste0(main_path, "/data/stocks-flows/processed/renewable_refinery_capacity.xlsx"), sheetIndex = 1))


alt_air_capacity <- setDT(read.xlsx(paste0(main_path, "/data/stocks-flows/raw/altair_refinery_capacity.xlsx"), sheetIndex = 1))



## Refineries plus
refin_new_locations <- fread(paste0(main_path, refinery_plus_locs)) %>%
  mutate(coords = gsub("^c\\(|\\)$", "", geometry)) %>%
  separate(coords, c('lon', 'lat'), sep = ',') %>%
  select(-geometry) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = refin_crs) %>%
  st_transform(ca_crs)

## site out
site_out <- fread(paste0(main_path, refin_out_path, site_out_file))

## 2019 info
site_out_2019 <- site_out %>% 
  filter(year == 2019,
         carbon_price_scenario == "price floor",
         ccs_scenario == "no ccs",
         demand_scenario == "BAU",
         refining_scenario == "historic production",
         oil_price_scenario == "reference case",
         innovation_scenario == "low innovation") 

## county out
county_out <- fread(paste0(main_path, refin_out_path, '/county_refining_outputs.csv'))

county_out_2019 <- county_out %>% 
  filter(year == 2019,
         carbon_price_scenario == "price floor",
         ccs_scenario == "no ccs",
         demand_scenario == "BAU",
         refining_scenario == "historic production",
         oil_price_scenario == "reference case",
         innovation_scenario == "low innovation")   

###
## alt air 2021 capacity
aa_cap <- alt_air_capacity %>%
  filter(year == 2021) %>%
  mutate(site_id  = 't-800') %>%
  select(site_id, barrels_per_day, installation_year = year)

renewable_cap <- renewable_capacity %>%
  filter(site_id %in% c('342-2', '99999')) %>%
  select(site_id, barrels_per_day = installation_capacity_bpd, installation_year) 

## future capacity
fut_cap <- rbind(aa_cap, renewable_cap)

## capacity
refin_capacity <- fread(paste0(main_path, 'data/stocks-flows/processed/', refin_cap_file)) %>%
  mutate(site_id = as.character(site_id)) %>%
  filter(State == 'California') %>%
  select(site_id, barrels_per_day) %>%
  filter(!site_id %in% man_capacity$site_id) %>%
  full_join(man_capacity %>% mutate(site_id = as.character(site_id))) %>%
  full_join(fut_cap) %>%
  mutate(installation_year = ifelse(is.na(installation_year), 'pre 2020', as.character(installation_year)))


## join with locations
refin_capacity <- refin_new_locations %>%
  left_join(refin_capacity) %>%
  mutate(installation = ifelse(installation_year == 'pre 2020', 'Existing capacity', 'Future capacity'))

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

## census tracts
census_tracts <- st_read(file.path(main_path, "data/GIS/raw/census-tract/tl_2019_06_tract.shp")) %>% 
  st_transform(ca_crs) %>%
  rename(census_tract = GEOID) %>%
  select(census_tract, ALAND)

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


## crop area
disp_win2_wgs84 <- st_sfc(st_point(c(-122.5, 33)), st_point(c(-117, 39)),
                          crs = 4326)

disp_win2_trans <- st_transform(disp_win2_wgs84, crs = ca_crs)

disp_win2_coord <- st_coordinates(disp_win2_trans)

disp_win_df <- as.data.frame(disp_win2_coord)

## limits for zoom
xlim <- c(disp_win_df$X[1], disp_win_df$X[2]) # Set limits for zoom panel
ylim <- c(disp_win_df$Y[1], disp_win_df$Y[2])


## st_union of no island counties
ca_union <- st_union(CA_counties_noisl)



## map inset, CA with box around zoom area
fig1_inset <- ggplot() +
  geom_sf(data = ca_union, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#9DBF9E", show.legend = TRUE) +
  # geom_sf(data = disp_win2_wgs84, shape = 0, size = 35, color = "red", stroke = 2) +# Draw box around zoomed region
  annotate(geom = "rect",
           xmin = xlim[1],
           xmax = xlim[2],
           ymin = ylim[1],
           ymax = ylim[2],
           color = "black",
           size = 0.5,
           fill = NA) +
  theme_void() +
  # coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
  #          datum = ca_crs, expand = FALSE) +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.15, 0.15),
    legend.title = element_text(size = 7),
    plot.title = element_text(hjust = 0, face = 'bold'),
    plot.title.position = 'plot') +
  guides(fill = guide_colourbar(title.position="top",
                                title.hjust = 0,
                                direction = "horizontal"))


## make map
fig1_map <- ggplot() +
  geom_sf(data = ca_union, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas, mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
  geom_sf(data = refin_capacity, mapping = aes(geometry = geometry, size = barrels_per_day / 1000, color = installation), alpha = 0.8, pch = 17) +
  geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = "A. Refinery capacity",
       color = NULL,
       size = 'Capacity (thous. bbls per day)',
       x = NULL,
       y = NULL) +
  scale_color_manual(values = c('#3D5A6C', '#F9564F')) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  scale_size_continuous(range = c(2, 6)) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0, 2, 0, 8),
        legend.title = element_text(size = 7),
        plot.title = element_text(hjust = -0.1, face = 'bold', size = 7))

## DAC legend
## ------------------------

fig1_dac_legend <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry, fill = ct_type), lwd = 0, show.legend = TRUE) +
  labs(title = "Oil production",
       fill = NULL,
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("DAC" = "#C0C0C0")) +
  theme(legend.text = element_text(size = 7))

dac_legend <- get_legend(
  fig1_dac_legend)

fig1_refing_legend <- ggplot() +
  geom_sf(data = ca_union, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
  geom_sf(data = refin_capacity, mapping = aes(geometry = geometry, size = barrels_per_day / 1000, color = installation), alpha = 0.8, pch = 17) +
  geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = "A. Refinery capacity",
       color = NULL,
       size = 'Capacity (thous. bbls per day)',
       x = NULL,
       y = NULL) +
  scale_size_continuous(range = c(2, 6),
                        breaks = c(10, 100, 300)) +
  scale_color_manual(values = c('#3D5A6C', '#F9564F')) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0, 0.2),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    plot.title = element_text(hjust = -0.1, face = 'bold', size = 7)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal",
                                ticks.colour = "black", frame.colour = "black"),
         size = guide_legend(direction = "horizontal"))

refin_legend <- get_legend(
  fig1_refing_legend)




## plot together
map_fig_a <- ggdraw(fig1_map, clip = "on") +
  draw_plot(fig1_inset, x = 0.7, y = 0.7, width = 0.2, height = 0.35) +
  draw_plot(dac_legend, x = 0.15, y = 0.21, width = 0.05, height = 0.01) +
  draw_plot(refin_legend, x = 0.1, y = 0.18, width = 0.025, height = 0.025) 
# +
#   draw_plot_label(
#     c("A. Oil fields and disadvantaged communities (DAC)", "", "", ""),
#     # c(0, 0.45),
#     # c(1, 0.95),
#     size = 12
#   )

ggsave(map_fig_a,
       filename = file.path(main_path, fig_path, 'fig1/figure1a.png'),
       width = 88,
       height = 110,
       dpi = 300,
       units = "mm")

ggsave(map_fig_a,
       filename = file.path(main_path, fig_path, 'fig1/figure1a.pdf'),
       width = 88,
       height = 110,
       units = "mm",
       dpi = 300,
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'fig1/figure1a.pdf'),
            outfile = paste0(main_path, fig_path, 'fig1/figure1a.pdf'))



## pulse fig
## --------------------------------------------------

## Census tracts
CA_ct <- st_read(paste0(main_path, "data/GIS/raw/census-tract/tl_2019_06_tract.shp")) %>%
  st_transform(ca_crs)

## refining sites
sites_vector <- c(226)

read_refining <- function(buff_site){
  
  bsite <- buff_site
  
  nh3<-read_csv(paste0(main_path, 'data/', inmapRefFiles,"/nh3/srm_nh3_site",bsite,".csv",sep=""))%>%mutate(poll="nh3")
  nox<-read_csv(paste0(main_path, 'data/', inmapRefFiles,"/nox/srm_nox_site",bsite,".csv",sep=""))%>%mutate(poll="nox")
  pm25<-read_csv(paste0(main_path, 'data/', inmapRefFiles,"/pm25/srm_pm25_site",bsite,".csv",sep=""))%>%mutate(poll="pm25")
  sox<-read_csv(paste0(main_path, 'data/', inmapRefFiles,"/sox/srm_sox_site",bsite,".csv",sep=""))%>%mutate(poll="sox")
  voc<-read_csv(paste0(main_path, 'data/', inmapRefFiles,"/voc/srm_voc_site",bsite,".csv",sep=""))%>%mutate(poll="voc")
  
  all_polls<-rbind(nh3,nox,pm25,sox,voc)
  
  all_polls$site=bsite
  
  tmp<-as.data.frame(all_polls) 
  
  return(tmp)
  
}

#DO THE FUNCTION
refining_srm <-map_df(sites_vector, read_refining) %>% bind_rows()
refining_srm <-dplyr::rename(refining_srm, weighted_totalpm25 = totalpm25_aw)

refining_srm_reshape <- dcast(refining_srm, site + GEOID ~ poll, value.var = "weighted_totalpm25")
srm_all_pollutants_refining <- dplyr::rename(refining_srm_reshape, 
                                             weighted_totalpm25nh3 = nh3, 
                                             weighted_totalpm25nox = nox,
                                             weighted_totalpm25pm25 = pm25,
                                             weighted_totalpm25sox = sox,
                                             weighted_totalpm25voc = voc,
                                             site_id = site)

srm_all_pollutants_refining$total_pm25 = srm_all_pollutants_refining$weighted_totalpm25nh3 + srm_all_pollutants_refining$weighted_totalpm25nox+srm_all_pollutants_refining$weighted_totalpm25pm25+srm_all_pollutants_refining$weighted_totalpm25sox+srm_all_pollutants_refining$weighted_totalpm25voc
ct_map <- left_join(CA_ct,srm_all_pollutants_refining,by=c("GEOID"))



##DACS

# dac_population <- read.csv(paste0(main_path, "data/health/raw/ces3results_part.csv"), stringsAsFactors = FALSE) %>%
#   subset(sb535_dac=="Yes")%>%
#   dplyr::rename(GEOID=census_tract)

CA_ct$GEOID = as.double(CA_ct$GEOID)

# dac_map <- left_join(CA_ct, dac_population, by=c("GEOID"))
# dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes" & COUNTYFP=="037")
# dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes")

## merge counties to census tracts
## -----------------------------------
county_code <- CA_counties %>%
  select(COUNTYFP, NAME) %>%
  st_drop_geometry() %>%
  unique() %>%
  rename(county_name = NAME)

ct_map_county <- ct_map %>%
  left_join(county_code)

## crop
## -----------------------------------
disp_win_la_wgs84 <- st_sfc(st_point(c(-118.5, 33.6)), st_point(c(-117.8, 34.2)),
                            crs = 4326)

disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)

disp_win_la_coord <- st_coordinates(disp_win_la_trans)

zoom_coord_df <- as.data.frame(disp_win_la_coord)

county_crop <- st_crop(CA_counties_noisl, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
ct_cropped <- st_crop(ct_map_county, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])

## only include census tracts that are in the crop
ct_intersect <- st_intersection(ct_map_county, county_crop)




## figure
total_pm25 <- ggplot() +
  geom_sf(data = ct_intersect, aes(fill=total_pm25), color=NA) + 
  theme_void() + 
  labs(title = expression(bold(paste("C. PM"[2.5], " concentration from Torrance Refinery"))),
       fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")"))) +
  scale_fill_gradient(high = "#A84268", low = "#FAFAFA", space = "Lab", na.value = "grey50",
                      limits = c(min(ct_cropped$total_pm25), max(ct_cropped$total_pm25)),
                      breaks = c(0.001, 0.004)) +
  geom_sf(data = county_crop, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
  # annotate(
  #   geom = "text", x = 75000, y = -375000, 
  #   label = "Los Angeles", hjust = 0, vjust = 1, size = 1.25, fontface = "bold"
  # ) +
  geom_sf(data = refin_new_locations %>% filter(site_id == '226'), mapping = aes(geometry = geometry), alpha = 0.9, pch = 16) +
  geom_sf_text(data = refin_new_locations %>% filter(site_id == '226') %>% mutate(name = "Torrance Refinery"), mapping = aes(geometry = geometry, label = name), size = 1, fontface = "bold", color = "black", vjust = -1) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.05, 0.15),
    legend.key.width = unit(0.7, "line"),
    legend.key.height = unit(0.5, "line"),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    plot.margin = margin(0, 2, 0, 8),
    plot.title = element_text(face = 'bold', size = 5, hjust = -0.05)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal",
                                ticks.colour = "black", frame.colour = "black",
                                order = 1),
         size = guide_legend(title.position = "top",
                             title.hjust = 0,
                             direction = "horizontal",
                             order = 2))


ggsave(total_pm25,
       filename = file.path(main_path, fig_path, 'fig1/fig1c.png'),
       width = 50,
       height = 55,
       dpi = 300,
       units = "mm")

ggsave(total_pm25,
       filename = file.path(main_path, fig_path, 'fig1/fig1c.pdf'),
       width = 50,
       height = 55,
       dpi = 300,
       units = "mm",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'fig1/fig1c.pdf'),
            outfile = paste0(main_path, fig_path, 'fig1/fig1c.pdf'))


## health and labor, 2019
## -------------------------------------------------


## health outputs, CT level
## ------------------------------------------------------------------------

bau_scn_name <- unique(site_out_2019$scen_id)

ct_out <- readRDS(paste0(main_path, 'outputs/academic-out/refining/refining_2022-05-11/subset-census-tract-results/', bau_scn_name, '_ct_results.rds'))

## filter for 2019
ct_2019 <- ct_out[year == 2019]
ct_2019[, pop_x_pm25 := total_pm25 * pop]
ct_2019 <- ct_2019[, .(scen_id, census_tract, disadvantaged, year, pop, total_pm25, pop_x_pm25)]

ct_2019 <- census_tracts %>%
  left_join(ct_2019) %>%
  filter(census_tract %in% ct_out$census_tract)

## color pals
blues_pal <- c("#FAFAFA", "#778DA9", "#415A77", "#1B263B", "#0D1B2A")


## figure
ct_health_map <- ggplot() +
  geom_sf(data = ct_2019, mapping = aes(geometry = geometry, fill = pop_x_pm25), lwd = 0.0, color = "white", alpha = 1, show.legend = TRUE) +
  scale_fill_gradient(high = "#A84268", low = "#FAFAFA", space = "Lab", na.value = "grey50",
                      limits = c(min(ct_2019$pop_x_pm25), max(ct_2019$pop_x_pm25)),
                      breaks = c(0, 8000, 16000)) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
  geom_sf_text(data = CA_counties_noisl %>% filter(NAME %in% c('Los Angeles', 'Orange', 'Solano')), mapping = aes(geometry = geometry, label = NAME), size = 1, fontface = "bold", color = "black") +
  geom_sf(data = refin_capacity %>% 
            mutate(object = "Refinery"), mapping = aes(geometry = geometry, shape = object), alpha = 0.8, color = 'black', size = 0.1) +
  scale_shape_manual(values = c(17)) +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = expression(bold(paste("D. PM"[2.5], " concentration of all refinery emissions"))),
       fill = expression(paste("Population-weighted PM"[2.5], " (",mu,"/",m^3,")")),
       shape = NULL,
       x = NULL,
       y = NULL) +
  # scale_fill_gradientn(colors = blues_pal) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0, 0.28),
    legend.key.width = unit(0.7, "line"),
    legend.key.height = unit(0.5, "line"),
    legend.title = element_text(size = 4),
    legend.text = element_text(size = 4),
    plot.margin = margin(0, 2, 0, 8),
    plot.title = element_text(face = 'bold', size = 4)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal",
                                ticks.colour = "black", frame.colour = "black"))


## save
ggsave(ct_health_map,
       filename = file.path(main_path, fig_path, 'fig1/fig1d.png'),
       width = 44,
       height = 55,
       dpi = 300,
       units = "mm")

ggsave(ct_health_map,
       filename = file.path(main_path, fig_path, 'fig1/fig1d.pdf'),
       width = 44,
       height = 55,
       units = "mm",
       dpi = 300,
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'fig1/fig1d.pdf'),
            outfile = paste0(main_path, fig_path, 'fig1/fig1d.pdf'))


## labor
## ----------------------------------------------

labor_out <- county_out[year == 2019]
labor_out <- labor_out[, .(scen_id, county, dac_share, year, total_emp, total_comp)]
labor_out <- labor_out[scen_id == bau_scn_name]

## deflate to 2019 dollars
#(https://fred.stlouisfed.org/series/CPALTT01USA661S)
cpi2020 <- 109.1951913
cpi2019 <- 107.8645906

labor_out <- CA_counties_noisl %>%
  select(NAME) %>%
  rename(county = NAME) %>%
  left_join(labor_out) %>%
  mutate(total_emp = ifelse(is.na(total_emp), 0, total_emp),
         total_comp = ifelse(is.na(total_comp), 0, total_comp),
         total_comp_usd19 = total_comp * cpi2019 / cpi2020)

labor_map <- ggplot() +
  geom_sf(data = labor_out, mapping = aes(geometry = geometry, fill = total_comp_usd19 / 1e6), lwd = 0.05, alpha = 1, show.legend = TRUE) +
  geom_sf_text(data = labor_out %>% filter(total_comp_usd19 > 0,
                                           county != "Los Angeles"), mapping = aes(geometry = geometry, label = county), size = 0.75, fontface = "bold", color = "black") +
  geom_sf_text(data = labor_out %>% filter(county == "Los Angeles"), mapping = aes(geometry = geometry, label = county), size = 0.75, fontface = "bold", color = "#B0B2B8") +
  geom_sf(data = refin_capacity %>% 
            mutate(object = "Refinery"), mapping = aes(geometry = geometry, shape = object), alpha = 0.8, color = 'black', size = 0.1) +
  scale_shape_manual(values = c(17)) +
  labs(
    title = 'E. Wages from refining',
    fill = 'USD million',
    shape = NULL,
    x = NULL,
    y = NULL) +
  scale_fill_gradientn(colors = blues_pal,
                       breaks = c(500, 2000)) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0, 0.28),
    legend.key.width = unit(0.7, "line"),
    legend.key.height = unit(0.5, "line"),
    legend.title = element_text(size = 4),
    legend.text = element_text(size = 4),
    plot.margin = margin(0, 2, 0, 8),
    plot.title = element_text(face = 'bold', size = 4)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal",
                                ticks.colour = "black", frame.colour = "black"))

## save
ggsave(labor_map,
       filename = file.path(main_path, fig_path, 'fig1/fig1e.png'),
       width = 44,
       height = 55,
       dpi = 300,
       units = "mm")

ggsave(labor_map,
       filename = file.path(main_path, fig_path, 'fig1/fig1e.pdf'),
       width = 44,
       height = 55,
       units = "mm",
       dpi = 300,
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'fig1/fig1e.pdf'),
            outfile = paste0(main_path, fig_path, 'fig1/fig1e.pdf'))





