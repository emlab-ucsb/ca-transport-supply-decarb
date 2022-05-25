## Tracey Mangin
## February 2, 2022
## county-level indicators

## libraries
library(data.table)
library(tidyverse)
library(openxlsx)
library(readxl)
library(sf)
library(maps)
library(viridis)


# fig 1: county-level ghg emissions intensity x 2019 production, calculated using weighted mean, w = 2019 production
# fig 2: county-level costs x 2019 production, calculated using weighted mean, w = 2019 production
# fig 3: county-level labor x 2019 production, use the indicator in the mechanisms figure
# fig 4: county-level mortality per bbl (2019)
# fig 5: county-level % of county affected by setback x 2019 production, use the setback scenario in mechanisms figures
# fig 6: county-level steam injection

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment", "figs-and-results", .x) %>% source()) # load local items

## paths 
main_path       <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'
outputs_path    <- 'outputs/academic-out/extraction/'
source_path     <- paste0(main_path, 'data/health/source_receptor_matrix/')

## results path (update this)
ct_results <- 'extraction_2022-02-08/census-tract-results/'

## files
ghg_file            <- 'ghg_emissions_x_field_2018-2045.csv'
prod_file           <- 'well_prod_m_processed.csv'
forecast_file       <- 'field_capex_opex_forecast_revised.csv'
setback_file        <- 'setback_coverage_R.csv'
ct_out_file         <- 'subset_census_tract_results.csv'
county_setback_file <- 'county_level_setback_coverage.csv'
field_cluster_file  <- 'extraction_field_cluster_xwalk.csv'
oil_price_file      <- 'oil_price_projections_revised.xlsx'
monthly_inj_file    <- 'well_inj_m_processed.csv'

## paths
data_path  <-'data/stocks-flows/processed/'
fig_path <- 'outputs/academic-out/extraction/figures/all-oil-px/figs/'

## figures - county-level indicator x 2019 production, add 90th percentile line
## ghg emission intensity, costs, employment multipliers, pm2.5?, setbacks?

## load data
## ---------------------------------------

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
## 2019 oil px
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1:4)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year == 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

oilpx_2019 <- oilpx_scens[, oil_price_usd_per_bbl][1]


## ghg factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## load opex/ capex
price_data = fread(file.path(main_path, 'outputs/stocks-flows/entry-input-df/final/', forecast_file), header = T)
price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
price_data[, sum_cost := m_opex_imputed + m_capex_imputed]
price_data <- price_data[year == 2020, .(doc_field_code, m_opex_imputed, m_capex_imputed, sum_cost)]

## county setback (use 1000ft)
county_setback_coverage <- fread(file.path(main_path, 'outputs/setback/county-level', county_setback_file), header = T)
county_setback_coverage <- county_setback_coverage[, .(adj_county_name, setback_scenario, county_field_coverage)]

## census tract health out
ct_health_out <- fread(paste0(main_path, outputs_path, ct_results, ct_out_file)) %>%
  filter(scen_id == "reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax",
         year == 2019)

## county - CT 
dac_ces <- read_xlsx(paste0(main_path, 'data/health/raw/ces3results.xlsx'))

ces_county <- dac_ces %>%
  select(`Census Tract`, `California County`) %>%
  rename(census_tract = `Census Tract`,
         county = `California County`) %>%
  mutate(census_tract = paste0("0", census_tract, sep=""))

county_mortality <- ct_health_out[, .(census_tract, mortality_level)]
county_mortality[, census_tract := paste0("0", census_tract)]

county_mortality <- merge(county_mortality, ces_county,
                          by = "census_tract",
                          all.x = T)

county_mortality <- county_mortality[, .(mortality_level = sum(mortality_level)), by = .(county)]
county_mortality <- county_mortality[!is.na(county)]

## labor
total_multipliers_ext <- read_xlsx(paste0(main_path, labor_processed, 'ica_multipliers_v2.xlsx'), sheet = 'ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, 
         indi_emp_mult = indirect_emp, 
         indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, 
         indi_comp_mult = indirect_comp, 
         indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, 
         ip.indi_comp_mult = ip.indirect_comp, 
         ip.indu_comp_mult = ip.induced_comp) %>%
  as.data.table() %>%
  rename(adj_county_name = county) %>%
  select(-segment)


# ## county results
# county_prod <- fread(paste0(main_path, outputs_path, county_results, county_out_file), header = T)

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

## add price data
county_out <- merge(county_out, price_data,
                    by = c("doc_field_code"),
                    all.x = T,
                    allow.cartesian = T)


## calculate employment
county_out[, revenue := county_prod_bbl * oilpx_2019]
# county_out[, employment]

## compute county values
## -------------------------------------------------

county_summaries <- county_out[, .(wm_upstream_kgCO2e_bbl = weighted.mean(upstream_kgCO2e_bbl, county_prod_bbl),
                                  wm_cost = weighted.mean(sum_cost, county_prod_bbl),
                                  total_prod_bbl = sum(county_prod_bbl),
                                  total_revenue = sum(revenue)), by = .(adj_county_name)]

## add labor info
## -------------------------

## add labor
county_summaries <- merge(county_summaries, total_multipliers_ext,
                    by = "adj_county_name",
                    all.x = T,
                    allow.cartesian = T)

##
county_summaries[, ':=' (c.dire_emp = (total_revenue / (10 ^ 6)) * dire_emp_mult,
                         c.indi_emp = (total_revenue / (10 ^ 6)) * indi_emp_mult,
                         c.indu_emp = (total_revenue / (10 ^ 6)) * indu_emp_mult,
                         c.dire_comp = (total_revenue / (10 ^ 6)) * dire_comp_mult,
                         c.indi_comp = (total_revenue / (10 ^ 6)) * ip.indi_comp_mult,
                         c.indu_comp = (total_revenue / (10 ^ 6)) * ip.indu_comp_mult)]

county_summaries[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
                         total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]


county_summaries <- county_summaries[, .(adj_county_name, wm_upstream_kgCO2e_bbl,
                                         wm_cost, total_prod_bbl, total_revenue,
                                         total_emp, total_comp)]


county_summaries[, emp_per_bbl := total_emp / total_prod_bbl]

county_summaries <- county_summaries[total_prod_bbl > 0]



## state summaries weighted mean (using 2019 production)
state_summaries <- copy(county_summaries)

state_summaries[, state := "California"]

state_summaries <- state_summaries[, .(wm_upstream_kgCO2e_bbl = weighted.mean(wm_upstream_kgCO2e_bbl, total_prod_bbl),
                                        wm_cost = weighted.mean(wm_cost, total_prod_bbl),
                                        total_emp = weighted.mean(total_emp, total_prod_bbl),
                                        total_comp = weighted.mean(total_comp, total_prod_bbl),
                                        emp_per_bbl = weighted.mean(emp_per_bbl, total_prod_bbl),
                                        total_prod_bbl = sum(total_prod_bbl)), by = .(state)]



## plot ghg emissions intensity
## -----------------------------------

county_ghg_plot <- ggplot(county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = wm_upstream_kgCO2e_bbl)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "County-level GHG emission intensity",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  ggrepel::geom_text_repel(data = county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = wm_upstream_kgCO2e_bbl, label = adj_county_name), 
                           hjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = state_summaries$wm_upstream_kgCO2e_bbl, lty = "dashed", color = "#848C8E") +
  scale_y_continuous(limits = c(0, 160)) +
  annotate("text", x = 30, y = 95, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  theme_line

ggsave(county_ghg_plot,
       filename = file.path(main_path, fig_path, 'county-ghg-emissions-intensity.png'),
       width = 6,
       height = 6,
       units = "in")

## plot cost
## -----------------------------------

county_cost_plot <- ggplot(county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = wm_cost)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "County-level cost per bbl",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  ggrepel::geom_text_repel(data = county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = wm_cost, label = adj_county_name), 
                           hjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = state_summaries$wm_cost, lty = "dashed", color = "#848C8E") +
  annotate("text", x = 30, y = 36, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  scale_y_continuous(limits = c(0, 60)) +
  theme_line

ggsave(county_cost_plot,
       filename = file.path(main_path, fig_path, 'county-cost.png'),
       width = 6,
       height = 6,
       units = "in")

## plot employment
## -----------------------------------

county_labor_plot <- ggplot(county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = emp_per_bbl)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "Jobs per bbl",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  ggrepel::geom_text_repel(data = county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = emp_per_bbl, label = adj_county_name),
                           hjust = 0, nudge_x = 0.1, size = 3) +
  scale_y_continuous(limits = c(0, 0.0005)) +
  geom_hline(yintercept = state_summaries$emp_per_bbl, lty = "dashed", color = "#848C8E") +
  annotate("text", x = 80, y = 1.7e-4, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  theme_line

# county_labor_plot2 <- ggplot(county_summaries %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = emp_per_bbl)) +
#   geom_point(alpha = 0.8) +
#   labs(title = "Jobs per bbl (San Mateo removed)",
#        x = "2019 production (million bbls)",
#        y = "Jobs per bbl") +
#   ggrepel::geom_text_repel(data = county_summaries %>% filter(adj_county_name != "San Mateo"), aes(x = total_prod_bbl / 1e6, y = emp_per_bbl, label = adj_county_name),
#                            hjust = 0, nudge_x = 0.1, size = 3) +
#   theme_line


# county_labor_fig <- plot_grid(
#   county_labor_plot,
#   county_labor_plot2,
#   align = 'vh',
#   # labels = c("A", "B", "C"),
#   hjust = -1,
#   nrow = 1,
#   rel_widths = c(1, 1)
# )

ggsave(county_labor_plot,
       filename = file.path(main_path, fig_path, 'county-labor.png'),
       width = 6,
       height = 6,
       units = "in")


## plot setback (1000ft)
## -----------------------------------

county_setback_coverage <- merge(county_setback_coverage, county_summaries[, .(adj_county_name, total_prod_bbl)],
                                 by = "adj_county_name",
                                 all.x = T,
                                 allow.cartesian = T)

county_setback_coverage[, setback_name := fifelse(setback_scenario == "setback_1000ft",
                                                  "1000ft setback",
                                                  fifelse(setback_scenario == "setback_2500ft",
                                                          "2500ft setback", "5280 setback"))]


## state summary
state_setback_coverage <- copy(county_setback_coverage)

state_setback_coverage[, state := "California"]

state_setback_coverage <- state_setback_coverage[!is.na(total_prod_bbl)]

state_setback_coverage <- state_setback_coverage[, .(county_field_coverage = weighted.mean(county_field_coverage, total_prod_bbl)), 
                                                 by = .(state, setback_scenario, setback_name)]

state_setback_coverage[, county_field_coverage := county_field_coverage * 100]

county_setback_plot <- ggplot(county_setback_coverage %>% filter(!is.na(total_prod_bbl),
                                                                 total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = county_field_coverage * 100, color = adj_county_name)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "Setback oil field coverage (%)",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  scale_y_continuous(limits = c(0, 100)) +
  # geom_hline(yintercept = state_setback_coverage$county_field_coverage, lty = "dashed", color = "#848C8E") +
  facet_wrap(~setback_name) +
  theme_line +
  theme(legend.title = element_blank())

ggsave(county_setback_plot,
       filename = file.path(main_path, fig_path, 'county-setback-coverage.png'),
       width = 8,
       height = 6,
       units = "in")

## setback, individual figs

y1000 <- state_setback_coverage %>%
  filter(setback_scenario == 'setback_1000ft') %>%
  select(county_field_coverage) %>%
  as.numeric()

county_setback_1000 <- ggplot(county_setback_coverage %>% filter(!is.na(total_prod_bbl),
                                                                 total_prod_bbl >= 1e6,
                                                                 setback_scenario == 'setback_1000ft'), aes(x = total_prod_bbl / 1e6, y = county_field_coverage * 100, color = adj_county_name)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "Setback oil field coverage (%)",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = y1000, lty = "dashed", color = "#848C8E") +
  annotate("text", x = 30, y = 16, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  facet_wrap(~setback_name) +
  theme_line +
  theme(legend.title = element_blank())

ggsave(county_setback_1000,
       filename = file.path(main_path, fig_path, 'county-setback-coverage-1000.png'),
       width = 4,
       height = 6,
       units = "in")


y2500 <- state_setback_coverage %>%
  filter(setback_scenario == 'setback_2500ft') %>%
  select(county_field_coverage) %>%
  as.numeric()

county_setback_2500 <- ggplot(county_setback_coverage %>% filter(!is.na(total_prod_bbl),
                                                                 total_prod_bbl >= 1e6,
                                                                 setback_scenario == 'setback_2500ft'), aes(x = total_prod_bbl / 1e6, y = county_field_coverage * 100, color = adj_county_name)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "Setback oil field coverage (%)",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = y2500, lty = "dashed", color = "#848C8E") +
  annotate("text", x = 80, y = 28, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  facet_wrap(~setback_name) +
  theme_line +
  theme(legend.title = element_blank())

ggsave(county_setback_2500,
       filename = file.path(main_path, fig_path, 'county-setback-coverage-2500.png'),
       width = 4,
       height = 6,
       units = "in")



y5280 <- state_setback_coverage %>%
  filter(setback_scenario == 'setback_5280ft') %>%
  select(county_field_coverage) %>%
  as.numeric()

county_setback_5280 <- ggplot(county_setback_coverage %>% filter(!is.na(total_prod_bbl),
                                                                 total_prod_bbl >= 1e6,
                                                                 setback_scenario == 'setback_5280ft'), aes(x = total_prod_bbl / 1e6, y = county_field_coverage * 100, color = adj_county_name)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "Setback oil field coverage (%)",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_hline(yintercept = y5280, lty = "dashed", color = "#848C8E") +
  annotate("text", x = 80, y = 45, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  facet_wrap(~setback_name) +
  theme_line +
  theme(legend.title = element_blank())

ggsave(county_setback_5280,
       filename = file.path(main_path, fig_path, 'county-setback-coverage-5280.png'),
       width = 4,
       height = 6,
       units = "in")



## county-level production
# ---------------------------------

# county_prod <- county_prod[oil_price_scenario == 'reference case' &
#                            innovation_scenario == 'low innovation'&
#                            carbon_price_scenario == 'price floor' &
#                            ccs_scenario == "no ccs" &
#                            prod_quota_scenario == "no quota" &
#                            excise_tax_scenario == 'no tax', .(scen_id, setback_scenario, county, year, total_county_bbl, total_county_ghg_kgCO2e)]
# 
# bau_prod <- county_prod[setback_scenario == "no_setback", .(county, year, total_county_bbl, total_county_ghg_kgCO2e)]
# 
# setnames(bau_prod, c("total_county_bbl", "total_county_ghg_kgCO2e"), c("bau_county_bbl", "bau_county_ghg"))
# 
# 
# county_prod = county_prod[bau_prod, on = .(county, year), allow.cartesian = T, nomatch = 0]
# 
# county_prod[, ':=' (diff_prod = total_county_bbl - bau_county_bbl,
#                     diff_ghg = total_county_ghg_kgCO2e - bau_county_ghg)]
# 
# 
# county_prod[, ':=' (rel_diff_prod = diff_prod / bau_county_bbl,
#                     rel_diff_ghg = diff_ghg / bau_county_ghg)]
# 
# county_prod[, ':=' (rel_diff_prod = fifelse(is.na(rel_diff_prod), 0, rel_diff_prod),
#                     rel_diff_ghg = fifelse(is.na(rel_diff_ghg), 0, rel_diff_ghg))]
# 
# sb_prod <- ggplot(county_prod %>% filter(setback_scenario != "no_setback"), aes(x = year, y = rel_diff_prod, group = county, color = county)) +
#   geom_line(alpha = 0.8) +
#   facet_wrap(~setback_scenario) +
#   theme_line +
#   theme(legend.title = element_blank(),
#         legend.position = "right")
# 
# ggsave(sb_prod,
#        filename = file.path(main_path, fig_path, 'county-setback-production.png'),
#        width = 8,
#        height = 6,
#        units = "in")

## health outcomes
## -------------------------------------

county_prod_summary <- county_summaries %>%
  rename(county = adj_county_name) %>%
  select(county, total_prod_bbl)

county_health_df <- county_mortality %>%
  left_join(county_prod_summary) %>%
  mutate(total_prod_bbl = ifelse(is.na(total_prod_bbl), 0, total_prod_bbl),
         mortality_per_bbl = mortality_level / total_prod_bbl) %>%
  pivot_longer(cols = c(mortality_level, mortality_per_bbl),
               names_to = "indicator",
               values_to = "value")

## califonia
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

ca_crs <- 3488

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(main_path, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(ca_crs) %>%
  dplyr::select(county = NAME)

county_health_df <- county_boundaries %>%
  left_join(county_health_df) 

## plot health impacts
## -----------------------------------

## crop area
disp_win_wgs84 <- st_sfc(st_point(c(-123, 32.5)), st_point(c(-113, 37)),
                         crs = 4326)

disp_win_trans <- st_transform(disp_win_wgs84, crs = ca_crs)

disp_win_coord <- st_coordinates(disp_win_trans)



health_map1 <- ggplot() +
  # geom_sf(data = california, mapping = aes(), fill = "white", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = california, mapping = aes(), fill = "#f8f9fa", lwd = 0.4, show.legend = FALSE) +
  # geom_sf(data = dac_areas , mapping = aes(geometry = geometry), fill = "#9DBF9E", lwd = 0, color = "white", show.legend = TRUE) +
  geom_sf(data = county_health_df %>% filter(indicator == "mortality_level"), mapping = aes(geometry = geometry, fill = value), lwd = 0.2, alpha = 1, show.legend = TRUE) +
  labs(title = "Mortality by county (2019)",
       fill = 'Mortality level',
       x = NULL,
       y = NULL) +
  scale_fill_viridis(option="mako",
                     direction = -1) +
  geom_sf_text(data = county_health_df %>% filter(indicator == "mortality_level"), 
               aes(geometry = geometry, label = county), colour = "#6c757d", size = 2) +
  theme_void() +
  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = "bottom",
    legend.title = element_text(size = 9)) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))



## health map 2
health_map2 <- ggplot() +
  geom_sf(data = california, mapping = aes(), fill = "#f8f9fa", lwd = 0.4, show.legend = FALSE) +
  geom_sf(data = county_health_df %>% filter(indicator == "mortality_per_bbl",
                                             total_prod_bbl >= 1e6), mapping = aes(geometry = geometry, fill = value), lwd = 0.2, alpha = 1, show.legend = TRUE) +
  labs(title = "Mortality per bbl by county (2019)",
       fill = 'Mortality level per bbl',
       x = NULL,
       y = NULL,
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  scale_fill_viridis(option="mako",
                     direction = -1) +
  geom_sf_text(data = county_health_df %>% filter(indicator == "mortality_per_bbl",
                                                  total_prod_bbl >= 1e6), 
               aes(geometry = geometry, label = county), colour = "#6c757d", size = 2) +
  theme_void() +
  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
           datum = ca_crs, expand = FALSE) +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.key.width= unit(1, 'cm')) +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 0,
                                direction = "horizontal"))


## county health maps
## ------------------------------------

county_health_maps <- plot_grid(
  health_map1, health_map2,
  nrow = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(1, 1)
)


ggsave(county_health_maps,
       filename = file.path(main_path, fig_path, 'county-mortality.png'),
       width = 8,
       height = 6,
       units = "in")

## fig 3

state_health <- county_health_df %>%
  mutate(state = "California") %>%
  filter(total_prod_bbl > 0) %>%
  group_by(state, indicator) %>%
  summarise(wm_value = weighted.mean(value, total_prod_bbl)) %>%
  ungroup() 

yval <- state_health %>%
  st_drop_geometry() %>%
  filter(indicator == "mortality_per_bbl") %>%
  select(wm_value) %>%
  as.numeric()

health_fig3 <- ggplot(county_health_df %>% filter(total_prod_bbl >= 1e6,
                                                  indicator == "mortality_per_bbl"), aes(x = total_prod_bbl / 1e6, y = value)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "2019 mortality per bbl",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  theme_line +
  ggrepel::geom_text_repel(data = county_health_df %>% filter(total_prod_bbl >= 1e6,
                                                              indicator == "mortality_per_bbl"), aes(x = total_prod_bbl / 1e6, y = value, label = county), 
                           hjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = yval, lty = "dashed", color = "#848C8E") +
  annotate("text", x = 80, y = 2.5e-7, label= "State weighted average (weight = production)", color = "#848C8E", size = 2.5) + 
  theme(legend.title = element_blank())

ggsave(health_fig3,
       filename = file.path(main_path, fig_path, 'county-mortality-v2.png'),
       width = 6,
       height = 6,
       units = "in")




## steam injectin by county
## ---------------------------------------------

## monthly well production
well_inj <- fread(paste0(main_path, "/data/stocks-flows/processed/", monthly_inj_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                       'doc_field_code' = 'character'))
                                                                                                 
# aggregate well injection data and production data by county and year -----
agg_inj_county = well_inj[!is.na(SteamWaterInjected), .(sum_steam = sum(SteamWaterInjected)), by = .(year, county_name)]

# filter out county-aggregated data for 2019 ------
agg_inj_county_2019 = agg_inj_county[year == 2019]

## adj county name
agg_inj_county_2019[, adj_county_name := str_remove(county_name, " Offshore")]

agg_inj_county_2019 = agg_inj_county_2019[, .(sum_steam = sum(sum_steam)), by = .(year, adj_county_name)]


# aggregate water and steam injection by well type and county for 2019 ------

agg_inj_2019 = well_inj[!is.na(SteamWaterInjected) & year == 2019, .(sum_steam = sum(SteamWaterInjected)), by = .(county_name, well_type_name, year)]

agg_inj_2019[, adj_county_name := str_remove(county_name, " Offshore")]

agg_inj_2019 = agg_inj_2019[, .(sum_steam = sum(sum_steam)), by = .(year, adj_county_name, well_type_name)]


## merge with county production
county_inj_prod <- agg_inj_county_2019 %>%
  full_join(county_summaries) %>%
  select(adj_county_name, total_prod_bbl, sum_steam) %>%
  mutate(sum_steam = ifelse(is.na(sum_steam), 0, sum_steam),
         steam_per_bbl = sum_steam / total_prod_bbl)



## figure
steam_inj_fig <- ggplot(county_inj_prod %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = steam_per_bbl)) +
  geom_point(alpha = 0.8) +
  labs(x = "2019 production (million bbls)",
       y = "2019 steam injection per bbl",
       caption = "Figure includes all counties that produced >= 1 million bbls in 2019") +
  theme_line +
  ggrepel::geom_text_repel(data = county_inj_prod %>% filter(total_prod_bbl >= 1e6), aes(x = total_prod_bbl / 1e6, y = steam_per_bbl, label = adj_county_name), 
                           hjust = 0, nudge_x = 0.1, size = 3) +
  theme(legend.title = element_blank())

ggsave(steam_inj_fig,
       filename = file.path(main_path, fig_path, 'county-steam-inj-per-bbl.png'),
       width = 6,
       height = 6,
       units = "in")








# # match well code with well type ----
# 
# agg_inj_2019 = agg_inj_2019[well_type_df, on = 'WellTypeCode', nomatch = 0]
# 
# # rename NA well types as 'Unknown' -----
# 
# agg_inj_2019[is.na(WellTypeCode), well_type_name := 'Unknown' ]
# 
# # rename non-common well types as 'Other' ------
# 
# agg_inj_2019[ ! well_type_name %in% c("Water Flood", "Water Disposal", "Steam Flood", "Cyclic Steam",  "Oil & Gas", 'Unknown'), well_type_name := 'Other']
# 
# # get top 10 water intensities for 2019 ------
# 
# county_prod_inj = agg_prod_county[agg_inj_county, on = c('year', 'county_name')]
# county_prod_inj_2019 = county_prod_inj[ year == 2019 ]
# 
# county_prod_inj_2019[, water_intensity := sum_steam/sum_prod ] # first calculate water intensities at the individual county level (not aggregated to "Other" yet ) because some counties have NA or Inf water intensities
# county_prod_inj_2019 = county_prod_inj_2019[ !is.na(water_intensity) & water_intensity < Inf ]
# county_prod_inj_2019[, adj_name := ifelse(county_name %in% top_prod_counties[, county_name], county_name, 'Other' )] # rename non-top 10 producing as "Other"
# 
# county_prod_inj_2019 = county_prod_inj_2019[, .(sum_steam = sum(sum_steam), sum_prod = sum(sum_prod)), by = .(year, adj_name)]  # re-calculate sums
# county_prod_inj_2019[, water_intensity := sum_steam/sum_prod ] # re-calculate water intensities
# setorder(county_prod_inj_2019, -water_intensity)
# top_int_counties = county_prod_inj_2019[1:10][, rank := 1:10]
# 
# # reorder factor levels -----
# 
# agg_inj_2019 = agg_inj_2019[, adj_name := factor(adj_name, levels = c('Other', rev(top_int_counties[, adj_name])))]
# 
# # plot -------
# 
# # bars are ranked from highest (top) to lowest (bottom) water intensity
# fig_well_perc = ggplot(agg_inj_2019, aes(x = adj_name, y = sum_steam, fill = well_type_name)) + 
#   geom_bar(stat = "identity", position = 'fill') +
#   labs(x = NULL,
#        y = 'Percentage of water or steam injected in 2019 in each county by well type',
#        fill = NULL) +
#   theme_ipsum(base_family = "Calibri", grid = "X", axis_title_just = "center",
#               axis_title_size = 20, axis_text_size = 16, strip_text_size = 14) +
#   scale_y_percent() +
#   scale_fill_manual(values = calepa_pal) + 
#   # base_theme +
#   theme(legend.position = "top") +
#   theme(legend.text = element_text(size = 12),
#         legend.title = element_text(size = 14, face = "bold"),
#         strip.background = element_blank(),
#         strip.text.x = element_blank(),
#         axis.text.x = element_text(angle = 0, vjust = 0.5, hjust =0.5))
# fig_well_perc = fig_well_perc + coord_flip()
# fig_well_perc

# uncomment if want to save:
# ggsave(filename = "bar_percentage-of-water-injected-by-well-type.png",
#        plot = fig_well_perc,
#        width = 10,
#        height = 8,
#        dpi = 800)
