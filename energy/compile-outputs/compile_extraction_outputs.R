
## Tracey Mangin
## July 26, 2021
## Compile extraction outputs (site and county, include 2019)

process_extraction_outputs <- function(output_extraction) {
  
## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
data_path  <-'data/stocks-flows/processed'
save_path <- "academic-out/extraction"


## files
prod_file  <- "well_prod_m_processed.csv"
oil_price_file <- 'oil_price_projections_revised.xlsx'
ghg_file <- 'ghg_emissions_x_field_2018-2045.csv'

## field outputs
field_out = output_extraction[[2]]

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))
## ghg factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## oil prices
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))


## county information
## -------------------------------

## county lut
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

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
  select(doc_field_code, adj_county_name, rel_prod)

## how many fields with positive prod?
# View(field_out[, c(prod = sum(total_prod_bbl, na.rm = T)), by = doc_field_code][V1 > 0])


browser()
  
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

init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]


init_prod <- init_prod[, .(doc_field_code, year, doc_fieldname, total_prod_bbl, total_ghg_kgCO2e)]

## add 2019 oil price


## filter out fields 000, 848 old wilmington, 154...



## prepare projection outputs
site_out <- field_out[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                          setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, doc_fieldname,
                          year, total_prod_bbl)]

## create out with all scenario, field, and year combinations
full_site_df <- expand.grid(oil_price_scenario = unique(site_out$oil_price_scenario),
                            innovation_scenario = unique(site_out$innovation_scenario),
                            carbon_price_scenario = unique(site_out$carbon_price_scenario),
                            ccs_scenario = unique(site_out$ccs_scenario),
                            prod_quota_scenario = unique(site_out$prod_quota_scenario),
                            excise_tax_scenario = unique(site_out$excise_tax_scenario),
                            doc_field_code = unique(c(site_out$doc_field_code, init_prod$doc_field_code)),
                            year = seq(2019, 2045, 1))

setDT(full_site_df)

full_site_df <- merge(full_site_df, init_prod,
                      by = c("doc_field_code", "year"),
                      all.x = T)




}

## segment, year, doc_field_code, doc_field_name, oil_price_scenario, innovation_scenario,	carbon_price_scenario,	ccs_scenario,
## setback_scenario,	prod_quota_scenario,	excise_tax_scenario,	production_bbl,	oil_price_usd_per_bbl_real, revenue,
## refining specific: demand_scenario, refining scenario, site_id, refinery_name, location, region, cluster, crude_e_total_bbl_net_exp
  
  
  



