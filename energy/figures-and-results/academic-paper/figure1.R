## Tracey Mangin
## December 6, 2021
## fig 1: map of CA


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

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
ct_out_path <- 'outputs/academic-out/extraction/extraction_2021-12-06/census-tract-results/'
county_out_path <- 'outputs/academic-out/extraction/extraction_2021-12-06/county-results/'
fig_path <- 'outputs/academic-out/extraction/figures/all-oil-px/'

## files
forecast_file     <- 'field_capex_opex_forecast_revised.csv'
entry_file        <- 'entry_df_final_revised.csv'
ghg_file          <- 'ghg_emissions_x_field_2018-2045.csv'
setback_file      <- 'setback_coverage_R.csv'
prod_file         <- "well_prod_m_processed.csv"
ct_file           <- "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_ct_results.rds"
county_file       <- "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_county_results.rds"

## projection for ca: transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items



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

setback_coverage <- setDT(setback_scens)
setback_coverage[, coverage_cat := fifelse(area_coverage > 0, as.numeric(str_extract(setback_scenario, one_or_more(DGT))), 0)]
setback_coverage <- setback_coverage[coverage_cat > 0]
setback_coverage[, coverage_min := min(coverage_cat), by = .(doc_field_code)]

setback_coverage <- unique(setback_coverage[, .(doc_field_code, coverage_min)])

setback_dt <- tibble(doc_field_code = unique(setback_scens$doc_field_code))
setback_dt <- merge(setback_dt, setback_coverage,
                    by = "doc_field_code",
                    all.x = T)

## combine by field
field_df <- price_data %>%
  select(doc_field_code, sum_cost) %>%
  left_join(ghg_factors)

field_df <- merge(field_df, field_prod,
                  by = "doc_field_code",
                  all.x = T)

field_df <- merge(field_df, setback_dt,
                  by = "doc_field_code",
                  all.x = T)

field_df[, prod_2019 := fifelse(is.na(prod_2019), 0, prod_2019)]
field_df[, coverage_min := fifelse(is.na(coverage_min), 0, coverage_min)]

field_df[, coverage_min := fifelse(coverage_min == 1000, "1000ft",
                                   fifelse(coverage_min == 2500, "2500ft",
                                           fifelse(coverage_min == 5280, "1 mile", "none")))]


## make long format
field_df_long <- pivot_longer(field_df[, .(doc_field_code, upstream_kgCO2e_bbl,
                                           sum_cost, prod_2019)], cols = upstream_kgCO2e_bbl:prod_2019,
                              names_to = "metric",
                              values_to = "value")




## fields
field_boundaries <- st_read(file.path(main_path, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% 
  st_transform(ca_crs) %>%
  select(doc_field_code = FIELD_CODE)

## add geometry
field_df_long <- field_boundaries %>%
  left_join(field_df_long) %>%
  filter(doc_field_code %in% field_df_long$doc_field_code)


st_transform(field_df_long, ca_crs)

## califonia
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)


county_19 <- merge(as_tibble(county_prod), county_boundaries,
                   by = "adj_county_name",
                   all.x = T)

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
disp_win_wgs84 <- st_sfc(st_point(c(-123, 33)), st_point(c(-116, 39)),
                         crs = 4326)

disp_win_trans <- st_transform(disp_win_wgs84, crs = ca_crs)

disp_win_coord <- st_coordinates(disp_win_trans)

## make map
fig1_map <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "#9DBF9E", show.legend = TRUE) +
  geom_sf(data = field_df_long %>% filter(metric == "prod_2019"), mapping = aes(geometry = geometry, fill = value / 1e6), lwd = 0, alpha = 1, show.legend = TRUE) +
  geom_sf(data = county_boundaries, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = "Oil production",
       fill = '2019 oil production\n(million bbls)',
       x = NULL,
       y = NULL) +
  scale_fill_viridis(option="magma",
                     direction = -1) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  # coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
  #          datum = ca_crs, expand = FALSE) +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.15, 0.15),
    legend.title = element_text(size = 9)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))

## DAC legend
## ------------------------

fig1_dac_legend <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry, fill = ct_type), lwd = 0, show.legend = TRUE) +
  labs(title = "Oil production",
       fill = '',
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("DAC" = "#9DBF9E")) 

dac_legend <- get_legend(
  fig1_dac_legend)

## plot together
map_fig_a <- ggdraw(fig1_map) +
  draw_plot(dac_legend, .17, .1, .1, .5) +
  draw_plot_label(
    c("A", ""),
    c(0, 0.45),
    c(1, 0.95),
    size = 12
  )

ggsave(map_fig_a,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1a.png'),
       width = 4,
       height = 9,
       units = "in")

ggsave(map_fig_a,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1a.pdf'),
       width = 4,
       height = 9,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/figure1a.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/figure1a.pdf'))








# ## ghg intensity
# fig1_ghg_map <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "#FFFAF5", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
#   geom_sf(data = field_df_long %>% filter(metric == "upstream_kgCO2e_bbl"), mapping = aes(geometry = geometry, fill = value), lwd = 0, alpha = 1, show.legend = TRUE) +
#   # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
#   # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
#   # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
#   labs(title = "GHG emissions intensity",
#        fill = '2019 GHG intensity\n(kgCO2e per bbl)',
#        x = NULL,
#        y = NULL) +
#   scale_fill_viridis(option="mako",
#                      direction = -1) +
#   # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
#   theme_void() +
#   coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
#            datum = ca_crs, expand = FALSE) +
#   theme(
#     # legend.justification defines the edge of the legend that the legend.position coordinates refer to
#     legend.justification = c(0, 1),
#     # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
#     legend.position = c(0.15, 0.15),
#     legend.title = element_text(size = 9)) +
#   guides(fill = guide_colourbar(title.position="top", 
#                                 title.hjust = 0,
#                                 direction = "horizontal"))
# 
# 
# ## cost
# fig1_cost_map <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "#FFFAF5", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
#   geom_sf(data = field_df_long %>% filter(metric == "sum_cost"), mapping = aes(geometry = geometry, fill = value), lwd = 0, alpha = 1, show.legend = TRUE) +
#   # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
#   # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
#   # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
#   labs(title = 'Cost',
#        fill = 'Opex + Capex (USD)',
#        x = NULL,
#        y = NULL) +
#   scale_fill_viridis(option="mako",
#                      direction = -1) +
#   coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
#            datum = ca_crs, expand = FALSE) +
#   # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
#   theme_void() +
#   theme(
#     # legend.justification defines the edge of the legend that the legend.position coordinates refer to
#     legend.justification = c(0, 1),
#     # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
#     legend.position = c(0.15, 0.15),
#     legend.title = element_text(size = 9)) +
#   guides(fill = guide_colourbar(title.position="top", 
#                                 title.hjust = 0,
#                                 direction = "horizontal"))
# 
# 
# ## setback ------
# ##-------------------
# 
# ## add geometry
# setback_map_df <- field_boundaries %>%
#   left_join(field_df) %>%
#   filter(doc_field_code %in% field_df_long$doc_field_code)
# 
# 
# fig1_setback_map <- ggplot() +
#   geom_sf(data = california, mapping = aes(), fill = "#FFFAF5", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
#   # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
#   geom_sf(data = setback_map_df, mapping = aes(geometry = geometry, fill = coverage_min), lwd = 0, alpha = 1, show.legend = TRUE) +
#   # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
#   # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
#   # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
#   labs(title = 'Setback',
#        fill = 'First setback\nto affect field',
#        x = NULL,
#        y = NULL) +
#   scale_fill_manual(values = c("1000ft" = "#355070",
#                                "2500ft" = "#73ba9b",
#                                "1 mile" = "#e56b6f",
#                                "none" = "grey")) +
#   coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
#            datum = ca_crs, expand = FALSE) +
#   # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
#   theme_void() +
#   theme(
#     # legend.justification defines the edge of the legend that the legend.position coordinates refer to
#     legend.justification = c(0, 1),
#     # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
#     legend.position = c(0.05, 0.40),
#     legend.title = element_text(size = 9)) 
# 
# ## plot together
# maps1 <-  grid.arrange(fig1_map, fig1_ghg_map, fig1_cost_map, fig1_setback_map, nrow = 2)
# 
# ggsave(maps1,
#        filename = file.path(main_path, fig_path, 'figure1_policies.png'),
#        width = 6,
#        height = 8,
#        units = "in")

## health and labor, 2019
## -------------------------------------------------

ct_out <- readRDS(paste0(main_path, ct_out_path, ct_file))

ct_2019 <- ct_out[year == 2019]
ct_2019[, pop_x_pm25 := total_pm25 * pop]
ct_2019 <- ct_2019[, .(scen_id, census_tract, disadvantaged, year, pop, total_pm25, pop_x_pm25)]

ct_2019 <- census_tracts %>%
  left_join(ct_2019) %>%
  filter(census_tract %in% ct_out$census_tract)

## health map

## blue color pal
blues_pal <- c("#E0E1DD", "#778DA9", "#415A77", "#1B263B", "#0D1B2A")


## crop area
disp_win2_wgs84 <- st_sfc(st_point(c(-123, 33)), st_point(c(-115, 39)),
                         crs = 4326)

disp_win2_trans <- st_transform(disp_win2_wgs84, crs = ca_crs)

disp_win2_coord <- st_coordinates(disp_win2_trans)


ct_map <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
  geom_sf(data = ct_2019, mapping = aes(geometry = geometry, fill = pop_x_pm25), lwd = 0.0, color = "white", alpha = 1, show.legend = TRUE) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  geom_sf(data = county_boundaries, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
  geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = 'Population * PM2.5 by census tract',
       fill = 'Population * PM2.5',
       x = NULL,
       y = NULL) +
  scale_fill_gradientn(colors = blues_pal) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.10, 0.15),
    legend.title = element_text(size = 7),
    plot.title = element_text(size = 11)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))

# ggsave(ct_map,
#        filename = file.path(main_path, fig_path, 'ctmap.png'),
#        width = 3,
#        height = 4,
#        units = "in")


## labor

labor_out <- readRDS(paste0(main_path, county_out_path, county_file))

labor_out <- labor_out[year == 2019]
labor_out <- labor_out[, .(scen_id, county, dac_share, year, total_emp, total_comp)]

labor_out <- county_boundaries %>%
  rename(county = adj_county_name) %>%
  left_join(labor_out) %>%
  mutate(total_emp = ifelse(is.na(total_emp), 0, total_emp),
         total_comp = ifelse(is.na(total_comp), 0, total_comp))

labor_map <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
  geom_sf(data = labor_out, mapping = aes(geometry = geometry, fill = total_comp / 1e6), lwd = 0.05, alpha = 1, show.legend = TRUE) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = 'Labor compensation',
       fill = 'USD million',
       x = NULL,
       y = NULL) +
  scale_fill_gradientn(colors = blues_pal) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.10, 0.15),
    legend.title = element_text(size = 7),
    plot.title = element_text(size = 11)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))


## plot together
maps2 <- plot_grid(
  ct_map,
  labor_map,
  align = 'vh',
  labels = c("C", "D"),
  label_size = 10,
  hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1)
)

ggsave(maps2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1_cd.png'),
       width = 4,
       height = 9,
       units = "in")

ggsave(maps2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1_cd.pdf'),
       width = 4,
       height = 9,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/figure1_cd.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/figure1_cd.pdf'))



## plot the two maps together
## -----------------------------------

# ## plot together
# fig1 <- plot_grid(
#   map_fig_a,
#   maps2,
#   align = 'h',
#   hjust = -1,
#   nrow = 1,
#   rel_widths = c(1, 1)
# )




