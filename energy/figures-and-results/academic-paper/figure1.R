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
data_path         <- paste0(main_path, 'outputs/entry-model-results/')

## for SRM figure
outputFiles   <- "outputs/academic-out"
sourceFiles   <- "data/health/source_receptor_matrix"
inmapExFiles  <- "health/source_receptor_matrix/inmap_processed_srm/extraction"


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


# ## load entry df 
# entry_data = fread(file.path(main_path, 'outputs/stocks-flows', entry_file), header = T)
# entry_data[, doc_field_code := sprintf("%03d", doc_field_code)]
# entry_data <- entry_data[year == 2019, .(doc_field_code, capex_imputed, opex_imputed)]
# entry_data[, sum_cost := capex_imputed + opex_imputed]
# 
# ## load opex/ capex forecast
# price_data = fread(file.path(main_path, 'outputs/stocks-flows', forecast_file), header = T)
# price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
# price_data[, sum_cost := m_opex_imputed + m_capex_imputed]
# price_data <- price_data[year == 2020, .(doc_field_code, m_opex_imputed, m_capex_imputed, sum_cost)]
# 
# ## emissions factors
# ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T)
# ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]
# ghg_factors <- ghg_factors[year == 2019, .(doc_field_code, upstream_kgCO2e_bbl)]
# 
# ## setback coverage
# setback_scens = fread(file.path(main_path, 'outputs/setback', 'model-inputs', setback_file), header = T, colClasses = c('doc_field_code' = 'character'))
# setnames(setback_scens, 'rel_coverage', 'area_coverage')
# setback_scens <- setback_scens[setback_scenario != "no_setback", .(doc_field_code, setback_scenario, area_coverage)]
# 
# setback_coverage <- setDT(setback_scens)
# setback_coverage[, coverage_cat := fifelse(area_coverage > 0, as.numeric(str_extract(setback_scenario, one_or_more(DGT))), 0)]
# setback_coverage <- setback_coverage[coverage_cat > 0]
# setback_coverage[, coverage_min := min(coverage_cat), by = .(doc_field_code)]
# 
# setback_coverage <- unique(setback_coverage[, .(doc_field_code, coverage_min)])
# 
# setback_dt <- tibble(doc_field_code = unique(setback_scens$doc_field_code))
# setback_dt <- merge(setback_dt, setback_coverage,
#                     by = "doc_field_code",
#                     all.x = T)
# 
# ## combine by field
# field_df <- price_data %>%
#   select(doc_field_code, sum_cost) %>%
#   left_join(ghg_factors)
# 
# field_df <- merge(field_df, field_prod,
#                   by = "doc_field_code",
#                   all.x = T)
# 
# field_df <- merge(field_df, setback_dt,
#                   by = "doc_field_code",
#                   all.x = T)

field_df <- copy(field_prod)
field_df[, prod_2019 := fifelse(is.na(prod_2019), 0, prod_2019)]
# field_df[, coverage_min := fifelse(is.na(coverage_min), 0, coverage_min)]

# field_df[, coverage_min := fifelse(coverage_min == 1000, "1000ft",
#                                    fifelse(coverage_min == 2500, "2500ft",
#                                            fifelse(coverage_min == 5280, "1 mile", "none")))]
# 

## make long format
# field_df_long <- pivot_longer(field_df[, .(doc_field_code, upstream_kgCO2e_bbl,
#                                            sum_cost, prod_2019)], cols = upstream_kgCO2e_bbl:prod_2019,
#                               names_to = "metric",
#                               values_to = "value")




## fields
field_boundaries <- st_read(file.path(main_path, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% 
  st_transform(ca_crs) %>%
  select(doc_field_code = FIELD_CODE)

## add geometry
# field_df_long <- field_boundaries %>%
#   left_join(field_df_long) %>%
#   filter(doc_field_code %in% field_df_long$doc_field_code)

field_df_long <- field_boundaries %>%
  left_join(field_df) %>%
  filter(doc_field_code %in% field_df$doc_field_code)


st_transform(field_df_long, ca_crs)

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

CA_counties_noisl <- CA_counties %>%
  filter(!OBJECTID %in% c(3, 49))


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


# ## crop area
# disp_win_wgs84 <- st_sfc(st_point(c(-123, 33)), st_point(c(-116, 39)),
#                          crs = 4326)
# 
# disp_win_trans <- st_transform(disp_win_wgs84, crs = ca_crs)
# 
# disp_win_coord <- st_coordinates(disp_win_trans)

## crop area
disp_win2_wgs84 <- st_sfc(st_point(c(-122.5, 33)), st_point(c(-117.5, 38)),
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
    legend.title = element_text(size = 9)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))


## make map
fig1_map <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = ca_union, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
  geom_sf(data = field_df_long, mapping = aes(geometry = geometry, fill = prod_2019 / 1e6), lwd = 0, alpha = 1, show.legend = TRUE) +
  geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = NULL,
       # title = "A. Oil fields and disadvantaged communities (DAC)",
       fill = 'Oil production (mil. bbls)',
       x = NULL,
       y = NULL) +
  scale_fill_viridis(option="viridis",
                     direction = -1) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  # geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)', scenario == name), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_void() +
  # coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
  #          datum = ca_crs, expand = FALSE) +
  theme(legend.position = "none")

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
  scale_fill_manual(values = c("DAC" = "#C0C0C0")) 

dac_legend <- get_legend(
  fig1_dac_legend)

fig1_oil_legend <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = ca_union, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#C0C0C0", lwd = 0, color = "#C0C0C0", show.legend = TRUE) +
  geom_sf(data = field_df_long, mapping = aes(geometry = geometry, fill = prod_2019 / 1e6), lwd = 0, alpha = 1, show.legend = TRUE) +
  geom_sf(data = CA_counties_noisl, mapping = aes(geometry = geometry), lwd = 0.05, fill = NA) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  # geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = "Oil production",
       fill = 'Oil production (mil. bbls)',
       x = NULL,
       y = NULL) +
  scale_fill_viridis(option="viridis",
                     direction = -1) +
  coord_sf(xlim = disp_win2_coord[,'X'], ylim = disp_win2_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
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

oil_prod_legend <- get_legend(
  fig1_oil_legend)




## plot together
map_fig_a <- ggdraw(fig1_map, clip = "on") +
  draw_plot(fig1_inset, x = -0.07, y = 0.15, width = 0.6, height = 0.3) +
  draw_plot(dac_legend, x = 0.15, y = 0.055, width = 0.025, height = 0.03) +
  draw_plot(oil_prod_legend, x = 0.3, y = 0.11, width = 0.03, height = 0.03) 
# +
#   draw_plot_label(
#     c("A. Oil fields and disadvantaged communities (DAC)", "", "", ""),
#     # c(0, 0.45),
#     # c(1, 0.95),
#     size = 12
#   )

ggsave(map_fig_a,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1a.png'),
       width = 4,
       height = 3.5,
       units = "in")

ggsave(map_fig_a,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1a.pdf'),
       width = 4,
       height = 3.5,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/figure1a.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/figure1a.pdf'))


## --------------------------------------------------
## figure 1b - well entry model
## --------------------------------------------------

## final model
pred_wells_fm <- read_csv(paste0(data_path, "new_wells_pred_revised.csv")) %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = as.character(str_sub(doc_field_code, start= -3))) 

# load historic production
well_prod_19 <- well_prod[year == 2019]

## top producers in 2019
prod2019 <- well_prod_19 %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(rank = rank(-prod)) %>%
  filter(rank <= 10) %>%
  arrange(rank) %>%
  mutate(doc_fieldname = ifelse(doc_fieldname == "Belridge  South", "Belridge South", doc_fieldname)) 

## field name
field_name <- well_prod %>%
  select(doc_field_code, doc_fieldname) %>%
  unique()

well_n_df_revised <- pred_wells_fm %>%
  left_join(field_name) %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_fieldname, "Non-top fields"),
         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  pivot_longer(new_wells:new_wells_pred, names_to = "category", values_to = "n_wells") %>%
  mutate(label_name = ifelse(category == "new_wells", "Observed", "Predicted")) %>%
  select(doc_field_code, doc_fieldname, field_name_adj, year, category, label_name, n_wells) %>%
  group_by(field_name_adj, year, category, label_name) %>%
  summarise(n_wells = sum(n_wells)) %>%
  ungroup() 

state_df <- well_n_df_revised %>%
  mutate(field_name_adj = "California") %>%
  group_by(field_name_adj, year, category, label_name) %>%
  summarise(n_wells = sum(n_wells)) %>%
  ungroup()

## state no hold out
state_fig_fs <- 
  ggplot(state_df %>% filter(category != "new_wells_pred_ho"), aes(x = year, y = n_wells, color = label_name, lty = label_name)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_manual(values = c("Observed" = "black", "Estimated" = "#737373")) +
  labs(y = "Number of new oil wells",
       x = NULL,
       color = NULL,
       lty = NULL) +
  guides(color = guide_legend(override.aes = list(lty = c('solid', 'dashed')))) +
  guides(lty = "none") +
  scale_x_continuous(limits = c(1978, 2020), breaks=c(1978, seq(1990,2020,10))) +
  scale_y_continuous(label = scales::comma, limits = c(0, 4000)) +
  theme_line +
  theme(legend.key.width = unit(1, 'cm'),
        legend.position = c(0.2, 0.1))


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
blues_pal <- c("#FAFAFA", "#778DA9", "#415A77", "#1B263B", "#0D1B2A")
red_pal <- c("#FAFAFA", "#A84268")


# ## crop area
# disp_win2_wgs84 <- st_sfc(st_point(c(-123, 33)), st_point(c(-115, 39)),
#                          crs = 4326)
# 
# disp_win2_trans <- st_transform(disp_win2_wgs84, crs = ca_crs)
# 
# disp_win2_coord <- st_coordinates(disp_win2_trans)


ct_health_map <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
  geom_sf(data = ct_2019, mapping = aes(geometry = geometry, fill = pop_x_pm25), lwd = 0.0, color = "white", alpha = 1, show.legend = TRUE) +
  scale_fill_gradient(high = "#A84268", low = "#FAFAFA", space = "Lab", na.value = "grey50",
                      limits = c(min(ct_2019$pop_x_pm25), max(ct_2019$pop_x_pm25))) +
  # geom_sf(data = county_19, mapping = aes(geometry = geometry), fill = NA, color = "#4A6C6F", lwd = 0.5) +
  geom_sf(data = CA_counties, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
  geom_sf_text(data = county_19, mapping = aes(geometry = geometry, label = adj_county_name), size = 2, fontface = "bold", color = "black") +
  # scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(title = 'Population * PM2.5 by census tract',
       fill = 'Population * PM2.5',
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

labor_out <- CA_counties %>%
  select(NAME) %>%
  rename(county = NAME) %>%
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
  ct_health_map,
  labor_map,
  align = 'v',
  # labels = c("C", "D"),
  # label_size = 10,
  # hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1)
)

ggsave(maps2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1_cd.png'),
       width = 7,
       height = 3.5,
       units = "in")

ggsave(maps2,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/figure1_cd.pdf'),
       width = 7,
       height = 3.5,
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

## -----------------------------------------------------------
## recreate Danae's figure
## -----------------------------------------------------------

#load and process cross-walk between fields and clusters 
extraction_field_clusters_10km <- read_csv(paste0(main_path,"data/health/source_receptor_matrix/extraction_fields_clusters_10km.csv",sep="")) %>%
  select(OUTPUT_FID, INPUT_FID) %>%
  rename(id = OUTPUT_FID, input_fid = INPUT_FID)

extraction_fields_xwalk <- foreign::read.dbf(paste0(main_path, "data/health/source_receptor_matrix/extraction_fields_xwalk_id.dbf", sep = "")) %>%
  rename(input_fid = id, doc_field_code = dc_fld_)

extraction_xwalk <- extraction_field_clusters_10km %>%
  left_join(extraction_fields_xwalk, by = c("input_fid")) 

## cluster 1 (LA cluster)
la_cluster_fields <- extraction_xwalk %>%
  filter(id == 1)

#Census tracts
CA_ct <- st_read(paste0(main_path, "data/GIS/raw/census-tract/tl_2019_06_tract.shp")) %>%
  st_transform(ca_crs)

#Site vector
sites_vector <- c(1)

read_extraction <- function(buff_site){
  
  bsite <- buff_site
  
  nh3<-read_csv(paste0(main_path, 'data/', inmapExFiles,"/nh3/srm_nh3_field",bsite,".csv",sep=""))%>%mutate(poll="nh3")
  nox<-read_csv(paste0(main_path, 'data/', inmapExFiles,"/nox/srm_nox_field",bsite,".csv",sep=""))%>%mutate(poll="nox")
  pm25<-read_csv(paste0(main_path, 'data/', inmapExFiles,"/pm25/srm_pm25_field",bsite,".csv",sep=""))%>%mutate(poll="pm25")
  sox<-read_csv(paste0(main_path, 'data/', inmapExFiles,"/sox/srm_sox_field",bsite,".csv",sep=""))%>%mutate(poll="sox")
  voc<-read_csv(paste0(main_path, 'data/', inmapExFiles,"/voc/srm_voc_field",bsite,".csv",sep=""))%>%mutate(poll="voc")
  
  all_polls<-rbind(nh3,nox,pm25,sox,voc)
  
  all_polls$site=bsite
  
  tmp<-as.data.frame(all_polls) 
  
  return(tmp)
  
}

#DO THE FUNCTION
extraction_srm <- map_df(sites_vector, read_extraction) %>% bind_rows()
extraction_srm <- dplyr::rename(extraction_srm, weighted_totalpm25 = totalpm25_aw)

extraction_srm_reshape <- dcast(extraction_srm, site + GEOID ~ poll, value.var = "weighted_totalpm25")
srm_all_pollutants_extraction <- dplyr::rename(extraction_srm_reshape, 
                                               weighted_totalpm25nh3 = nh3, 
                                               weighted_totalpm25nox = nox, 
                                               weighted_totalpm25pm25 = pm25, 
                                               weighted_totalpm25sox = sox, 
                                               weighted_totalpm25voc = voc, 
                                               site_id = site)

srm_all_pollutants_extraction$total_pm25 = srm_all_pollutants_extraction$weighted_totalpm25nh3 + srm_all_pollutants_extraction$weighted_totalpm25nox + srm_all_pollutants_extraction$weighted_totalpm25pm25 + srm_all_pollutants_extraction$weighted_totalpm25sox + srm_all_pollutants_extraction$weighted_totalpm25voc

ct_map <- left_join(CA_ct, srm_all_pollutants_extraction, by = c("GEOID"))

##DACS

dac_population <- read.csv(paste0(main_path, "data/health/raw/ces3results_part.csv"), stringsAsFactors = FALSE) %>%
  subset(sb535_dac=="Yes")%>%
  dplyr::rename(GEOID=census_tract)

CA_ct$GEOID = as.double(CA_ct$GEOID)

dac_map <- left_join(CA_ct, dac_population, by=c("GEOID"))
dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes" & COUNTYFP=="037")
dac_map <- dac_map %>% dplyr::filter(sb535_dac=="Yes")

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
disp_win_la_wgs84 <- st_sfc(st_point(c(-119.0, 33.3)), st_point(c(-117.4, 34.6)),
                          crs = 4326)

disp_win_la_trans <- st_transform(disp_win_la_wgs84, crs = ca_crs)

disp_win_la_coord <- st_coordinates(disp_win_la_trans)

zoom_coord_df <- as.data.frame(disp_win_la_coord)

county_crop <- st_crop(CA_counties_noisl, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])
ct_cropped <- st_crop(ct_map_county, xmin = zoom_coord_df$X[1], xmax = zoom_coord_df$X[2], ymin = zoom_coord_df$Y[1], ymax = zoom_coord_df$Y[2])

## only include census tracts that are in the crop
ct_intersect <- st_intersection(ct_map_county, county_crop)


## counties
# CA_counties<-st_read(paste0(main_path, "data/GIS/raw/CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp")) %>%
#   st_transform(ca_crs)

# LAcontour <- CA_counties %>% subset(COUNTYFP=='037')
# 
# LA_contour_cropped <- st_crop(LAcontour, xmin = -119.4, xmax = 117.46, ymin = 32, ymax = 34.84)

## extraction clusters
extraction_clusters <- st_read(paste0(main_path, "outputs/academic-out/extraction/extraction_fields_clusters_10km.shp")) %>%
  st_transform(ca_crs)

field1 <- extraction_clusters %>% dplyr::filter(OBJECTID==1) %>%
  mutate(name = "Field cluster")

# LA_contour_cropped <- st_crop(LAcontour, xmin = -118.9, xmax = -118.46, ymin = 32, ymax = 34.84)

## LA metro
# LA_metro <- st_read(paste0(main_path, "data/GIS/raw/OnlyLA_Urban_Areas/U.S._Census_Urbanized_Areas_%E2%80%93_SCAG_Region.shp")) %>%
#   st_transform(ca_crs)
# 
# LA_metro <- LA_metro %>% dplyr::filter(NAME10 == "Los Angeles--Long Beach--Anaheim, CA")

# field1wgs84<-st_transform(field1,4326)
# field1_intersection <- st_intersection(LA_metro, field1wgs84)

# field1_intersection <- st_intersection(LA_metro, field1)



# LA <- ct_map %>% subset(COUNTYFP=='037') %>% dplyr::filter(AWATER < ALAND)
# # LA1 <- st_transform(LA, 4326)
# LA2 <- st_intersection(LA_metro, LA)

# map2 <- st_transform(ct_map, ca_crs)

# ## califonia
# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# 
# california <- states %>% filter(ID == "california") %>%
#   st_transform(ca_crs)


# all_SC <- st_intersection(california, map2)


# total_pm25 <- ggplot(data = LA) +
#   geom_sf(data = LA2, aes(fill=total_pm25), color=NA) + theme_void() + labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")")))  +
#   scale_fill_gradient(high = "#FF0000", low = "#FFFFFF", space = "Lab", na.value = "gray50",
#                       limits = c(min(LA$total_pm25), max(LA$total_pm25))) +
#   #geom_sf(data = LA_contour_cropped, fill="transparent", color="gray65") +
#   geom_sf(data = LA_metro, fill="transparent", color="gray65")+
#   geom_sf(data = field1_intersection, fill="transparent", color="black")
# pdf("C:/Users/dhern125/Documents/GitHub/ca-transport-supply-decarb/health/outputs/LA_field_cluster_1.pdf")
# print(total_pm25)
# dev.off()
# plot(total_pm25)

## total pm2.5
# zoom_c <- c('Los Angeles', 'Ventura', 'Orange', 'San Bernardino', 'Kern')

total_pm25 <- ggplot() +
  geom_sf(data = ct_intersect, aes(fill=total_pm25), color=NA) + 
  theme_void() + 
  labs(fill=expression(paste("PM"[2.5], " (",mu,"/",m^3,")"))) +
  # labs(fill=expression(paste("PM"[2.5], " concentration from LA oil fields emissions (n = 51)"))) +
  # labs(fill = "PM2.5 concentration from XX oil field emissions") +
  scale_fill_gradient(high = "#A84268", low = "#FAFAFA", space = "Lab", na.value = "grey50",
                      limits = c(min(ct_cropped$total_pm25), max(ct_cropped$total_pm25))) +
  # geom_sf(data = california, mapping = aes(), fill = "#FAFAFA", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = county_crop, mapping = aes(geometry = geometry), lwd = 0.15, alpha = 0) +
  geom_sf(data = field_boundaries %>% filter(doc_field_code %in% la_cluster_fields$doc_field_code),
          aes(geometry = geometry), color = "black", fill = "transparent", lwd = 0.2) +
  geom_sf_text(data = CA_counties %>% filter(NAME %in% c("Los Angeles", "Orange")), mapping = aes(geometry = geometry, label = NAME), size = 2, fontface = "bold", color = "black") +
  # geom_sf(data = dac_map, fill="transparent", color="gray65", size = 0.1) +
  # geom_sf(data = LA_metro, fill="transparent", color="gray65") +
  geom_sf(data = field1, fill="transparent", color="black") +
  # geom_sf_text(data = field1, mapping = aes(geometry = geometry, label = name), 
  #              size = 2, fontface = "bold", color = "black", nudge_y = 20000, nudge_x = 10000) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.12, 0.2),
    legend.title = element_text(size = 11),
    plot.title = element_text(size = 11)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))

  
ggsave(total_pm25,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/fig1_exposure.png'),
       width = 3.5,
       height = 3.5,
       units = "in")

ggsave(total_pm25,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/fig1_exposure.pdf'),
       width = 3.5,
       height = 3.5,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/fig1_exposure.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/fig1_exposure.pdf'))


## -----------------------------------------------------------
## arrange all plots together
## -----------------------------------------------------------

## plot together
all_fig1 <- plot_grid(
  map_fig_a,
  state_fig_fs,
  labor_map,
  NULL,
  total_pm25,
  ct_health_map,
  align = 'hv',
  # labels = c("C", "D"),
  # label_size = 10,
  # hjust = -1,
  nrow = 2,
  rel_widths = c(1, 1, 1, 1, 1, 1)
)

ggsave(all_fig1,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/fig1_all.png'),
       width = 12,
       height = 7.5,
       units = "in")

ggsave(all_fig1,
       filename = file.path(main_path, fig_path, 'figs/main-text-revisions/fig1_all.pdf'),
       width = 12,
       height = 7.5,
       units = "in",
       device = 'pdf')

embed_fonts(paste0(main_path, fig_path, 'figs/main-text-revisions/fig1_all.pdf'),
            outfile = paste0(main_path, fig_path, 'figs/main-text-revisions/fig1_all.pdf'))




  