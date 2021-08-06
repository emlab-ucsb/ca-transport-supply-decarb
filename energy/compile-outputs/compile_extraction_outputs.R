
## Tracey Mangin
## July 26, 2021
## Compile extraction outputs (site and county, include 2019)

process_extraction_outputs <- function(output_extraction) {

## create a folder to store outputs
compiled_save_path  = file.path('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/', paste0('extraction_', cur_date))
dir.create(compiled_save_path, showWarnings = FALSE)  
  
    
## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
data_path  <-'data/stocks-flows/processed'


## files
prod_file  <- "well_prod_m_processed.csv"
oil_price_file <- 'oil_price_projections_revised.xlsx'
ghg_file <- 'ghg_emissions_x_field_2018-2045.csv'

## state outputs
state_out = output_extraction[[3]]

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
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))


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

init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]


init_prod <- init_prod[, .(doc_field_code, year, total_prod_bbl, total_ghg_kgCO2e)]

## remove fields that do not ever produce oil and do not show up in results, as well as "Any Field)
init_prod <- init_prod[!doc_field_code %in% c("302", "502", "000")]

init_prod_bbls_only <- init_prod[, .(doc_field_code, year, total_prod_bbl)]

## check fields in 2019 vs outputs
## ------------------------------------------------------
anti_join(init_prod %>% select(doc_field_code), field_out %>% select(doc_field_code) %>% unique())

check_projection <- anti_join(field_out %>% select(doc_field_code) %>% unique(), init_prod %>% select(doc_field_code))

View(field_out[doc_field_code %chin% check_projection[, doc_field_code] & total_prod_bbl > 0])
## English Colony (ABD) has existing well production; Pacoima (ABD) has new well production

## prepare projection outputs
site_out <- field_out[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                          setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code,
                          year, total_prod_bbl)]

## create out with all scenario, field, and year combinations
all_scens <- unique(state_out[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                  setback_scenario, prod_quota_scenario, excise_tax_scenario)])

all_scens[, scen_id := .I]
setcolorder(all_scens, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                         "setback_scenario", "prod_quota_scenario", "excise_tax_scenario"))

full_site_df <- expand.grid(scen_id = unique(all_scens$scen_id),
                            doc_field_code = unique(c(site_out$doc_field_code, init_prod_bbls_only$doc_field_code)),
                            year = seq(2019, 2045, 1))

setDT(full_site_df)

## add scenario information using id
full_site_df <- merge(full_site_df, all_scens,
                      by = c("scen_id"),
                      all.x = T)

setcolorder(full_site_df, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                            "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "doc_field_code", "year"))

full_site_df_2019 <- full_site_df[year == 2019]

full_site_df_2019 <- merge(full_site_df_2019, fname_lut,
                           by = c("doc_field_code"),
                           all.x = T)

full_site_df_2019 <- merge(full_site_df_2019, init_prod_bbls_only,
                           by = c("doc_field_code", "year"),
                           all.x = T)

full_site_df_2019[,':=' (total_prod_bbl = fifelse(is.na(total_prod_bbl), 0, total_prod_bbl))]

## now do projection

full_site_df_proj <- full_site_df[year > 2019]

full_site_df_proj <- merge(full_site_df_proj, fname_lut,
                           by = c("doc_field_code"),
                           all.x = T)

full_site_df_proj <- merge(full_site_df_proj, site_out,
                           by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                  'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'doc_field_code',
                                  'year'),
                           all.x = T)

full_site_df_proj[,':=' (total_prod_bbl = fifelse(is.na(total_prod_bbl), 0, total_prod_bbl))]

## bind 2019 to projected
full_site_out <- rbind(full_site_df_2019, full_site_df_proj)

setcolorder(full_site_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'doc_field_code', 'doc_fieldname',
                             'year', 'total_prod_bbl'))

## which fields have zero production?
zero_prod <- full_site_out[, .(prod = sum(total_prod_bbl)), by = .(doc_field_code, doc_fieldname)]
zero_prod <- zero_prod[prod == 0]

prod_list <- full_site_out[, .(prod = sum(total_prod_bbl)), by = .(doc_field_code, doc_fieldname)]
prod_list <- prod_list[prod > 0]
prod_list <- prod_list[, c('doc_field_code', 'doc_fieldname')]

# save results ------

pos_field_fname = paste0('fields_positive_prod.csv')
fwrite(prod_list, file.path(compiled_save_path, pos_field_fname), row.names = F)
print(paste0('Fields with positive production saved to ', pos_field_fname))

## filter full site out for fields with > 0 production
full_site_out <- full_site_out[doc_field_code %in% prod_list[, doc_field_code]]

## add oil prices (including 2019... 2021 real dollars)
full_site_out <- merge(full_site_out, oilpx_scens,
                       by = c("oil_price_scenario", "year"),
                       all.x = T)

full_site_out[, revenue := total_prod_bbl * oil_price_usd_per_bbl]

setcolorder(full_site_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'doc_field_code', 'doc_fieldname',
                             'year', 'total_prod_bbl', 'oil_price_usd_per_bbl', 'revenue'))



## save site level output for health and labor
extraction_site_fname = paste0('site_extraction_outputs.csv')
fwrite(full_site_out, file.path(compiled_save_path, extraction_site_fname), row.names = F)
print(paste0('Extraction site outputs saved to ', extraction_site_fname))



## now do county-level
## ---------------------------------------------------------

county_out <- merge(full_site_out, prod_x_county,
                    by = c("doc_field_code"),
                    all.x = T,
                    allow.cartesian = T)

setcolorder(county_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                          'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                          'year', 'doc_field_code', 'doc_fieldname', 'total_prod_bbl', 'adj_county_name', 'rel_prod'))

## summarise at the county level
county_out <- county_out[, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                             'year', 'doc_field_code', 'doc_fieldname', 'total_prod_bbl', 'adj_county_name', 'rel_prod')]

county_out[, county_prod_bbl := total_prod_bbl * rel_prod]

county_out <- county_out[, .(total_county_bbl = sum(county_prod_bbl)), by = .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                                                           ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                                                           year, adj_county_name)]

## add oil price
county_out <- merge(county_out, oilpx_scens,
                    by = c("oil_price_scenario", "year"),
                    all.x = T)

county_out[, revenue := total_county_bbl * oil_price_usd_per_bbl]

setcolorder(county_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                          'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                          'year', 'adj_county_name', 'total_county_bbl', 'oil_price_usd_per_bbl', 'revenue'))

# ## test
# test <- county_out[, .(prod = sum(total_county_bbl)), by = .(oil_price_scenario, innovation_scenario,
#                                                              carbon_price_scenario, ccs_scenario,
#                                                              setback_scenario, prod_quota_scenario,
#                                                              excise_tax_scenario, year)]
# 
# test[, scen := fifelse(setback_scenario == "setback_2500ft" & prod_quota_scenario == "quota_20", "LCE2",
#                        fifelse(prod_quota_scenario == "quota_20" & setback_scenario == "no_setback", "LCE1", "BAU"))]
# 
# ggplot(test, aes(y = prod / 1e6, x = year, group = scen, color = scen)) +
#   geom_line()

## save outputs for health and labor
extraction_county_fname = paste0('county_extraction_outputs.csv')
fwrite(county_out, file.path(compiled_save_path, extraction_county_fname), row.names = F)
print(paste0('Extraction county outputs saved to ', extraction_county_fname))


}

## segment, year, doc_field_code, doc_field_name, oil_price_scenario, innovation_scenario,	carbon_price_scenario,	ccs_scenario,
## setback_scenario,	prod_quota_scenario,	excise_tax_scenario,	production_bbl,	oil_price_usd_per_bbl_real, revenue,
## refining specific: demand_scenario, refining scenario, site_id, refinery_name, location, region, cluster, crude_e_total_bbl_net_exp
  
  
  



