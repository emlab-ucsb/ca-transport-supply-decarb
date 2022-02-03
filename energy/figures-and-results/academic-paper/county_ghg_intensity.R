## Tracey Mangin
## February 2, 2022
## county-level ghg intensity, weighted by 2019 production

## libraries
library(data.table)
library(tidyverse)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "figures-and-results", "academic-paper", .x) %>% source()) # load local items


## paths 
main_path     <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'

## files
ghg_file  <- 'ghg_emissions_x_field_2018-2045.csv'
prod_file <- "well_prod_m_processed.csv"

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
                                                                                                 
## ghg factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]



## county information
## -------------------------------

## county lut
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## field name look up 
fname_lut <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique()

## get relative county production (most recent year of nonzero production available for each field)
prod_x_county <- well_prod %>%
  left_join(county_lut) %>%
  group_by(doc_field_code, doc_fieldname, year, adj_county_name) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, year) %>%
  mutate(field_total = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_prod = prod / field_total,
         rel_prod = ifelse(is.na(rel_prod) & prod == 0 & field_total == 0, 0, rel_prod)) %>%
  filter(rel_prod > 0) %>%
  group_by(doc_field_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(doc_field_code, adj_county_name, rel_prod)

## how many fields with positive prod?
# View(field_out[, c(prod = sum(total_prod_bbl, na.rm = T)), by = doc_field_code][V1 > 0])

## calculate 2019 production, emissions, revenue
init_prod <- well_prod %>%
  filter(year == 2019) %>%
  select(doc_field_code, doc_fieldname, year, OilorCondensateProduced) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(total_prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

setDT(init_prod)

## merge with ghg factors
init_prod <- merge(init_prod, ghg_factors_2019,
                   by = c('doc_field_code', 'year'),
                   all.x = T)

# init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]
# 
# 
# init_prod <- init_prod[, .(doc_field_code, year, total_prod_bbl, total_ghg_kgCO2e)]

## remove fields that do not ever produce oil and do not show up in results, as well as "Any Field)
init_prod <- init_prod[!doc_field_code %in% c("302", "502", "000")]

## join 
county_out <- merge(init_prod, prod_x_county,
                    by = c("doc_field_code"),
                    all.x = T,
                    allow.cartesian = T)

## county production
county_out[, county_prod_bbl := total_prod_bbl * rel_prod]

## county emissions factors, weighted by 2019 production
county_ghg_intensity <- county_out[, .(mean_ghg_intensity = weighted.mean(upstream_kgCO2e_bbl, total_prod_bbl),
                                       total_prod_bbl = sum(total_prod_bbl)), 
                                   by = .(adj_county_name)]

county_ghg_intensity <- county_ghg_intensity[total_prod_bbl > 0]

## plot

county_ghg_plot <- ggplot(county_ghg_intensity, aes(x = total_prod_bbl / 1e6, y = mean_ghg_intensity)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "County-level emission intensity") +
  ggrepel::geom_text_repel(data = county_ghg_intensity %>% filter(mean_ghg_intensity > 40), aes(x = total_prod_bbl / 1e6, y = mean_ghg_intensity, label = adj_county_name), 
                           hjust = 0, nudge_x = 0.1, size = 3) +
  theme_line
  


