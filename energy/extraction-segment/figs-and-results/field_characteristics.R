## Tracey Mangin

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)
library(readxl)

## figure themes
theme_line = theme_ipsum(base_family = 'Arial',
                         grid = 'Y', 
                         plot_title_size = 10, 
                         subtitle_size = 9,
                         axis_title_just = 'center',
                         axis_title_size = 9, 
                         axis_text_size = 9,
                         strip_text_size = 9)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 8, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))

## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
source_path   <- paste0(main_path, 'data/health/source_receptor_matrix/')
inmap_ex_path  <- paste0(main_path, "data/health/source_receptor_matrix/inmap_processed_srm/extraction")
# inmap_ref_path  <- paste0(main_path, "data/health/source_receptor_matrix/inmap_processed_srm/refining")


## update these as needed
save_info_path <- paste0(main_path, 'outputs/academic-out/extraction/figures/revision-replicate/')


## files
bau_file <- 'reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax_field_results.rds'
labor_file <- 'reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax_county_results.rds'
forecast_file     <- 'field_capex_opex_forecast_revised.csv'
ghg_file          <- 'ghg_emissions_x_field_2018-2045.csv'
setback_file      <- 'setback_coverage_R.csv'
prod_file       <- "well_prod_m_processed.csv"

## well prod
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

## read in bau field out
bau_out <- readRDS(paste0(main_path, 'outputs/academic-out/extraction/extraction_2022-11-07/field-results/', bau_file))

## county out
county_out <- readRDS(paste0(main_path, 'outputs/academic-out/extraction/extraction_2022-11-07/county-results/', labor_file))

## load opex/ capex
price_data = fread(file.path(main_path, 'outputs/stocks-flows/entry-input-df/final/', forecast_file), header = T)
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

## dac 
dac_ces <- read_xlsx(paste0(main_path, 'data/health/raw/ces3results.xlsx'))

ces_county <- dac_ces %>%
  select(`Census Tract`, `California County`, `SB 535 Disadvantaged Community`) %>%
  rename(census_tract = `Census Tract`,
         county = `California County`,
         disadvantaged = `SB 535 Disadvantaged Community`) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) 

# ces3 <- read.csv(paste0(main_path, "data/health/processed/ces3_data.csv"), stringsAsFactors = FALSE) %>%
#   select(census_tract, CES3_score, disadvantaged) %>%
#   mutate(census_tract = paste0("0", census_tract, sep="")) %>%
#   as.data.table()

## census pop
ct_inc_pop_45 <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

## census tract, population > 29
ct_pop_time <- ct_inc_pop_45 %>%
  filter(lower_age > 29) %>%
  group_by(ct_id, year) %>%
  summarize(ct_pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  rename(census_tract = ct_id) %>%
  as.data.table()

## select population for 2019
ct_pop_2019 <- ct_pop_time %>%
  filter(year == 2019) %>%
  select(-year)

## DAC share
dac_df <- merge(ces_county, ct_pop_2019,
                by = 'census_tract',
                all.x = T)

setDT(dac_df)

dac_df[, dac_pop := fifelse(disadvantaged == "Yes", ct_pop, 0)]

## DAC by county
dac_county <- dac_df %>%
  group_by(county) %>%
  summarize(county_pop = sum(ct_pop),
            dac_pop = sum(dac_pop)) %>%
  ungroup() %>%
  mutate(dac_share = dac_pop / county_pop)


## make SRM for extraction segment
## ------------------------------------------------

# #  Load extraction source receptor matrix (srm) #######
# n_cores <- availableCores() - 1
# plan(multisession, workers = n_cores, gc = TRUE)
# 
# #fields vector
# fields_vector <- c(1:26)
# 
# #function
# read_extraction <- function(buff_field){
#   
#   bfield <- buff_field
#   
#   nh3  <- read_csv(paste0(inmap_ex_path, "/nh3/srm_nh3_field", bfield, ".csv", sep="")) %>% mutate(poll = "nh3")
#   nox  <- read_csv(paste0(inmap_ex_path, "/nox/srm_nox_field", bfield, ".csv", sep="")) %>% mutate(poll = "nox")
#   pm25 <- read_csv(paste0(inmap_ex_path, "/pm25/srm_pm25_field", bfield, ".csv", sep="")) %>% mutate(poll = "pm25")
#   sox  <- read_csv(paste0(inmap_ex_path, "/sox/srm_sox_field", bfield, ".csv", sep="")) %>% mutate(poll = "sox")
#   voc  <- read_csv(paste0(inmap_ex_path, "/voc/srm_voc_field", bfield, ".csv", sep="")) %>% mutate(poll = "voc")
#   
#   all_polls <- rbind(nh3, nox, pm25, sox, voc)
#   
#   all_polls$field = bfield
#   
#   tmp <- as.data.frame(all_polls) 
#   
#   return(tmp)
#   
# }
# 
# ## build extraction srm
# srm_all_pollutants_extraction <- future_map_dfr(fields_vector, read_extraction) %>% 
#   bind_rows() %>%
#   rename(weighted_totalpm25 = totalpm25_aw)%>%
#   select(-totalpm25) %>%
#   spread(poll, weighted_totalpm25) %>%
#   rename(weighted_totalpm25nh3 = nh3,
#          weighted_totalpm25nox = nox,
#          weighted_totalpm25pm25 = pm25,
#          weighted_totalpm25sox = sox,
#          weighted_totalpm25voc = voc,
#          id = field)
# 
# future:::ClusterRegistry("stop")
# 
# # (1.2) Calculate census tract ambient emissions for extraction  #######
# 
# #load and process cross-walk between fields and clusters 
# extraction_field_clusters_10km <- read_csv(paste0(source_path,"/extraction_fields_clusters_10km.csv",sep="")) %>%
#   select(OUTPUT_FID, INPUT_FID) %>%
#   rename(id = OUTPUT_FID, input_fid = INPUT_FID)
# 
# extraction_fields_xwalk <- foreign::read.dbf(paste0(source_path, "/extraction_fields_xwalk_id.dbf", sep = "")) %>%
#   rename(input_fid = id, doc_field_code = dc_fld_)
# 
# extraction_xwalk <- extraction_field_clusters_10km %>%
#   left_join(extraction_fields_xwalk, by = c("input_fid")) 
# 
# ## join with fields
# srm_fields <- merge(srm_all_pollutants_extraction, extraction_xwalk,
#                     by = c("id"),
#                     all.x = T)
# 
# setDT(srm_fields)
# 
# srm_fields[, input_fid := NULL]
# srm_fields[, NAME := NULL]
# 
# ## calculate air pollution using emission factors
# nh3 =  0.00061 / 1000
# nox =  0.04611 / 1000
# pm25 = 0.00165 / 1000
# sox =  0.01344 / 1000
# voc =  0.02614 / 1000
# 
# ## calculate total emissions per 1 bbl
# total_clusters <- srm_fields %>%
#   mutate(tot_nh3 = weighted_totalpm25nh3 * nh3,
#          tot_nox = weighted_totalpm25nox * nox,
#          tot_sox = weighted_totalpm25sox * sox,
#          tot_pm25 = weighted_totalpm25pm25 * pm25,
#          tot_voc = weighted_totalpm25voc * voc,
#          total_pm25 = tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc,
#          prim_pm25 = tot_pm25)
# 
# ct_exposure <- total_clusters %>%
#   ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012 
#   ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
#   mutate(GEOID = ifelse(GEOID == "06037137000", "06037930401", GEOID)) %>%
#   select(id, doc_field_code, census_tract = GEOID, total_pm25)
# 
# ## 2019 field level production
# prod_2019 <- bau_out %>%
#   filter(year == 2019) %>%
#   select(doc_field_code, doc_fieldname, bbl_2019 = total_prod_bbl)
# 
# ## combine exposure with 2019 prod and ces info (dac and ces3 score)
# srm_extraction <- ct_exposure %>%
#   left_join(ct_pop_2019, by = "census_tract") %>%
#   left_join(ces3, by = "census_tract") %>%
#   mutate(dac_pm25 = ifelse(disadvantaged == "Yes", total_pm25, 0))
# 
# ## summarise by field
# srm_extraction_field <- srm_extraction %>%
#   group_by(doc_field_code) %>%
#   summarise(total_pm25 = sum(total_pm25),
#             dac_pm25 = sum(dac_pm25, na.rm = T)) %>%
#   ungroup() %>%
#   left_join(prod_2019) %>%
#   mutate(dac_share_pm25 = dac_pm25 / total_pm25) %>%
#   ## Add 2019 capex + opex; carbon emissions; setback area closed
#   left_join(price_data) %>%
#   left_join(ghg_factors) %>%
#   left_join(setback_scens) %>%
#   select(doc_field_code, doc_fieldname, dac_pm25, total_pm25, dac_share_pm25, bbl_2019, capex_plus_opex_2020 = sum_cost, upstream_kgCO2e_bbl, area_coverage_mile = area_coverage)
# 
# 
# ## figure
# fig_srm_cost <- ggplot(srm_extraction_field, aes(x = sum_cost, y = dac_share_pm25, size = bbl_2019 / 1e6)) +
#   geom_point(alpha = 0.4) +
#   labs(title = "DAC share pm2.5 by field",
#        subtitle = "no CCS",
#        size = "2019 oil production (million bbls)",
#        x = "2020 opex + capex (USD)",
#        y = "DAC share total pm2.5 pollution") +
#        # color = "GHG emission target",
#        # shape = "Policy intervention") +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#     # scale_x_continuous(limits = c(0, NA)) +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# ggsave(fig_srm_cost, 
#        filename = file.path(save_info_path, 'srm_opex_capex.png'), 
#        width = 6, 
#        height = 6)
# 
# 
# ## figure
# fig_srm_ghg <- ggplot(srm_extraction_field, aes(x = upstream_kgCO2e_bbl, y = dac_share_pm25, size = bbl_2019 / 1e6)) +
#   geom_point(alpha = 0.4) +
#   labs(title = "DAC share pm2.5 by field",
#        subtitle = "no CCS",
#        size = "2019 oil production (million bbls)",
#        x = "2019 GHG emissions factor (kgCO2e per bbl)",
#        y = "DAC share total pm2.5 pollution") +
#   # color = "GHG emission target",
#   # shape = "Policy intervention") +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# ggsave(fig_srm_ghg, 
#        filename = file.path(save_info_path, 'srm_ghg_factor.png'), 
#        width = 6, 
#        height = 6)
# 
# ## figure
# fig_srm_setback <- ggplot(srm_extraction_field, aes(x = area_coverage_mile, y = dac_share_pm25, size = bbl_2019 / 1e6)) +
#   geom_point(alpha = 0.4) +
#   labs(title = "DAC share pm2.5 by field",
#        subtitle = "no CCS",
#        size = "2019 oil production (million bbls)",
#        x = "1 mile setback relative area coverage",
#        y = "DAC share total pm2.5 pollution") +
#   # color = "GHG emission target",
#   # shape = "Policy intervention") +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# ggsave(fig_srm_setback, 
#        filename = file.path(save_info_path, 'srm_setback.png'), 
#        width = 6, 
#        height = 6)

## save files for kyle
## field-level variables: oil field identifier, GHG emissions factor, 1 mile setback relative area, 2020 capex, 2020 opex
## county-level variables: county identifier, GHG emissions factor (averaged across oil fields in that county), 1 mile setback relative area (averaged across oil fields in that county), oil extraction jobs in that county in 2020

## field names
fnames <- well_prod %>%
  select(doc_field_code, doc_fieldname) %>%
  unique()

## calculate average production by field and county, 2015-2019
prod_x_field <- expand.grid(doc_field_code = unique(fnames$doc_field_code),
                            year = 2015:2019)

mean_prod_field <- well_prod %>%
  filter(year >= 2015) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T), .groups = 'drop') %>%
  right_join(prod_x_field) %>%
  mutate(prod = ifelse(is.na(prod), 0, prod)) %>%
  group_by(doc_field_code) %>%
  summarise(mean_prod = mean(prod), .groups = 'drop') 

## combine
field_df <- ghg_factors %>%
  left_join(price_data) %>%
  left_join(setback_scens) %>%
  left_join(mean_prod_field) %>%
  left_join(fnames) %>%
  filter(!is.na(setback_scenario)) %>%
  select(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl, setback_scenario, area_coverage, capex_2020 = m_capex_imputed, opex_2020 = m_opex_imputed, mean_prod)


## county ----

## county match
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## mean production
county_yr <- expand.grid(adj_county_name = unique(county_lut$adj_county_name),
                         year = 2015:2019)


## mean prod
mean_prod_county <- well_prod %>%
  left_join(county_lut) %>%
  filter(year >= 2015) %>%
  group_by(adj_county_name, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T), .groups = 'drop') %>%
  right_join(county_yr) %>%
  mutate(prod = ifelse(is.na(prod), 0, prod)) %>%
  group_by(adj_county_name) %>%
  summarise(mean_prod = mean(prod), .groups = 'drop') %>%
  rename(county = adj_county_name)

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


## calc number of jobs in 2019
jobs_2019 <- county_out %>%
  filter(year == 2019)  %>%
  select(county, total_emp)

## create county df
county_df <- prod_x_county %>%
  left_join(field_df) %>%
  filter(doc_field_code != '848') %>%
  filter(!is.na(upstream_kgCO2e_bbl)) %>%
  group_by(adj_county_name, setback_scenario) %>%
  summarise(mean_upstream_kgCO2e_bbl = mean(upstream_kgCO2e_bbl),
            wm_upstream_kgCO2e_bbl = weighted.mean(upstream_kgCO2e_bbl, rel_prod),
            mean_capex_2020 = mean(capex_2020),
            wm_capex_2020 = weighted.mean(capex_2020, rel_prod),
            mean_opex_2020 = mean(opex_2020),
            wm_opex_2020 = weighted.mean(opex_2020, rel_prod),
            mean_area_coverage = mean(area_coverage),
            wm_area_coverage = weighted.mean(area_coverage, rel_prod)) %>%
  ungroup() %>%
  rename(county = adj_county_name) %>%
  left_join(jobs_2019) %>%
  mutate(total_emp = ifelse(is.na(total_emp), 0, total_emp)) %>%
  left_join(dac_county) %>%
  left_join(mean_prod_county) %>%
  select(county, county_pop, dac_pop, dac_share, mean_prod, total_emp, mean_upstream_kgCO2e_bbl, wm_upstream_kgCO2e_bbl,
         mean_capex_2020, wm_capex_2020, mean_opex_2020, wm_opex_2020, setback_scenario, mean_area_coverage, wm_area_coverage)


## save 
fwrite(field_df, paste0(save_info_path, 'field_characteristics.csv'))

fwrite(county_df, paste0(save_info_path, 'county_characteristics.csv'))


# ## save 
# fwrite(field_df, paste0(main_path, 'outputs/academic-out/extraction/srm-info/field_characteristics.csv'))
# 
# fwrite(county_df, paste0(main_path, 'outputs/academic-out/extraction/srm-info/county_characteristics.csv'))
