## Tracey Mangin

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)

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
inmap_ref_path  <- paste0(main_path, "data/health/source_receptor_matrix/inmap_processed_srm/refining")


## update these as needed
save_info_path <- paste0(main_path, 'outputs/academic-out/extraction/exploratory-figs/figs_2021-11-09/srm')
ref_save_info_path <- paste0(main_path, 'outputs/academic-out/refining/exploratory-figs/figs_2021-11-15/srm')

## files
bau_file <- 'reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_field_results.rds'
forecast_file     <- 'field_capex_opex_forecast_revised.csv'
ghg_file          <- 'ghg_emissions_x_field_2018-2045.csv'
setback_file      <- 'setback_coverage_R.csv'

## read in bau field out
bau_out <- readRDS(paste0(main_path, 'outputs/academic-out/extraction/extraction_2021-11-09/field-results/', bau_file))

## load opex/ capex
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
setback_scens <- setback_scens[setback_scenario == "setback_5280", .(doc_field_code, area_coverage)]

## dac 
ces3 <- read.csv(paste0(main_path, "data/health/processed/ces3_data.csv"), stringsAsFactors = FALSE) %>%
  select(census_tract, CES3_score, disadvantaged) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  as.data.table()

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


## make SRM for extraction segment
## ------------------------------------------------

#  Load extraction source receptor matrix (srm) #######
n_cores <- availableCores() - 1
plan(multisession, workers = n_cores, gc = TRUE)

#fields vector
fields_vector <- c(1:26)

#function
read_extraction <- function(buff_field){
  
  bfield <- buff_field
  
  nh3  <- read_csv(paste0(inmap_ex_path, "/nh3/srm_nh3_field", bfield, ".csv", sep="")) %>% mutate(poll = "nh3")
  nox  <- read_csv(paste0(inmap_ex_path, "/nox/srm_nox_field", bfield, ".csv", sep="")) %>% mutate(poll = "nox")
  pm25 <- read_csv(paste0(inmap_ex_path, "/pm25/srm_pm25_field", bfield, ".csv", sep="")) %>% mutate(poll = "pm25")
  sox  <- read_csv(paste0(inmap_ex_path, "/sox/srm_sox_field", bfield, ".csv", sep="")) %>% mutate(poll = "sox")
  voc  <- read_csv(paste0(inmap_ex_path, "/voc/srm_voc_field", bfield, ".csv", sep="")) %>% mutate(poll = "voc")
  
  all_polls <- rbind(nh3, nox, pm25, sox, voc)
  
  all_polls$field = bfield
  
  tmp <- as.data.frame(all_polls) 
  
  return(tmp)
  
}

## build extraction srm
srm_all_pollutants_extraction <- future_map_dfr(fields_vector, read_extraction) %>% 
  bind_rows() %>%
  rename(weighted_totalpm25 = totalpm25_aw)%>%
  select(-totalpm25) %>%
  spread(poll, weighted_totalpm25) %>%
  rename(weighted_totalpm25nh3 = nh3,
         weighted_totalpm25nox = nox,
         weighted_totalpm25pm25 = pm25,
         weighted_totalpm25sox = sox,
         weighted_totalpm25voc = voc,
         id = field)

future:::ClusterRegistry("stop")

# (1.2) Calculate census tract ambient emissions for extraction  #######

#load and process cross-walk between fields and clusters 
extraction_field_clusters_10km <- read_csv(paste0(source_path,"/extraction_fields_clusters_10km.csv",sep="")) %>%
  select(OUTPUT_FID, INPUT_FID) %>%
  rename(id = OUTPUT_FID, input_fid = INPUT_FID)

extraction_fields_xwalk <- foreign::read.dbf(paste0(source_path, "/extraction_fields_xwalk_id.dbf", sep = "")) %>%
  rename(input_fid = id, doc_field_code = dc_fld_)

extraction_xwalk <- extraction_field_clusters_10km %>%
  left_join(extraction_fields_xwalk, by = c("input_fid")) 

## join with fields
srm_fields <- merge(srm_all_pollutants_extraction, extraction_xwalk,
                    by = c("id"),
                    all.x = T)

setDT(srm_fields)

srm_fields[, input_fid := NULL]
srm_fields[, NAME := NULL]

## calculate air pollution using emission factors
nh3 =  0.00061 / 1000
nox =  0.04611 / 1000
pm25 = 0.00165 / 1000
sox =  0.01344 / 1000
voc =  0.02614 / 1000

## calculate total emissions per 1 bbl
total_clusters <- srm_fields %>%
  mutate(tot_nh3 = weighted_totalpm25nh3 * nh3,
         tot_nox = weighted_totalpm25nox * nox,
         tot_sox = weighted_totalpm25sox * sox,
         tot_pm25 = weighted_totalpm25pm25 * pm25,
         tot_voc = weighted_totalpm25voc * voc,
         total_pm25 = tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc,
         prim_pm25 = tot_pm25)

ct_exposure <- total_clusters %>%
  ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012 
  ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
  mutate(GEOID = ifelse(GEOID == "06037137000", "06037930401", GEOID)) %>%
  select(id, doc_field_code, census_tract = GEOID, total_pm25)

## 2019 field level production
prod_2019 <- bau_out %>%
  filter(year == 2019) %>%
  select(doc_field_code, doc_fieldname, bbl_2019 = total_prod_bbl)

## combine exposure with 2019 prod and ces info (dac and ces3 score)
srm_extraction <- ct_exposure %>%
  left_join(ct_pop_2019, by = "census_tract") %>%
  left_join(ces3, by = "census_tract") %>%
  mutate(dac_pm25 = ifelse(disadvantaged == "Yes", total_pm25, 0))

## summarise by field
srm_extraction_field <- srm_extraction %>%
  group_by(doc_field_code) %>%
  summarise(total_pm25 = sum(total_pm25),
            dac_pm25 = sum(dac_pm25, na.rm = T)) %>%
  ungroup() %>%
  left_join(prod_2019) %>%
  mutate(dac_share_pm25 = dac_pm25 / total_pm25) %>%
  ## Add 2019 capex + opex; carbon emissions; setback area closed
  left_join(price_data) %>%
  left_join(ghg_factors) %>%
  left_join(setback_scens) %>%
  select(doc_field_code, doc_fieldname, dac_pm25, total_pm25, dac_share_pm25, bbl_2019, capex_plus_opex_2020 = sum_cost, upstream_kgCO2e_bbl, area_coverage_mile = area_coverage)

## save 
fwrite(srm_extraction_field, paste0(main_path, 'outputs/academic-out/extraction/srm-info/srm_characterisics_info.csv'))



## figure
fig_srm_cost <- ggplot(srm_extraction_field, aes(x = sum_cost, y = dac_share_pm25, size = bbl_2019 / 1e6)) +
  geom_point(alpha = 0.4) +
  labs(title = "DAC share pm2.5 by field",
       subtitle = "no CCS",
       size = "2019 oil production (million bbls)",
       x = "2020 opex + capex (USD)",
       y = "DAC share total pm2.5 pollution") +
       # color = "GHG emission target",
       # shape = "Policy intervention") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
    # scale_x_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_srm_cost, 
       filename = file.path(save_info_path, 'srm_opex_capex.png'), 
       width = 6, 
       height = 6)


## figure
fig_srm_ghg <- ggplot(srm_extraction_field, aes(x = upstream_kgCO2e_bbl, y = dac_share_pm25, size = bbl_2019 / 1e6)) +
  geom_point(alpha = 0.4) +
  labs(title = "DAC share pm2.5 by field",
       subtitle = "no CCS",
       size = "2019 oil production (million bbls)",
       x = "2019 GHG emissions factor (kgCO2e per bbl)",
       y = "DAC share total pm2.5 pollution") +
  # color = "GHG emission target",
  # shape = "Policy intervention") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  # scale_x_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_srm_ghg, 
       filename = file.path(save_info_path, 'srm_ghg_factor.png'), 
       width = 6, 
       height = 6)

## figure
fig_srm_setback <- ggplot(srm_extraction_field, aes(x = area_coverage_mile, y = dac_share_pm25, size = bbl_2019 / 1e6)) +
  geom_point(alpha = 0.4) +
  labs(title = "DAC share pm2.5 by field",
       subtitle = "no CCS",
       size = "2019 oil production (million bbls)",
       x = "1 mile setback relative area coverage",
       y = "DAC share total pm2.5 pollution") +
  # color = "GHG emission target",
  # shape = "Policy intervention") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  # scale_x_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(fig_srm_setback, 
       filename = file.path(save_info_path, 'srm_setback.png'), 
       width = 6, 
       height = 6)

## ------------------------------------------------------------------------------
## repeat for refining
## ------------------------------------------------------------------------------

