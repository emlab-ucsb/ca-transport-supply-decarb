## Tracey Mangin
## May 27, 2021
## historic o&g emissions

# ------------------------------------------- set up and inputs -----------------------------------
proj_dir           <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
data_directory    <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/"
raw_directory     <- "data/stocks-flows/raw/"

## production
mprod_fil    <- "well_prod_m.rds"
wells_19_fil <- "wells_19.csv"

## load libraries
library(tidyverse)
library(data.table)
library(readxl)

## outputs
extract_field_out <- fread('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/extraction_2021-06-08/revised-new-entry-model/diagnostic-field-level-results.csv', 
                           header = T, colClasses = c('doc_field_code' = 'character'))

## well data
wells_19 <- fread(paste0(proj_dir, '/data/stocks-flows/processed/', wells_19_fil))
wells_19[, api_ten_digit := substr(API, 1, 10)]
wells_19[, FieldCode := paste0("00", FieldCode)][, FieldCode := str_sub(FieldCode, start= -3)]

## raw production
well_prod <- read_rds(paste0(proj_dir, "/data/stocks-flows/processed/well_prod_m.rds"))

setDT(well_prod)

well_prod[, ':=' (api_ten_digit = substr(APINumber, 1, 10),
                  FieldCode = paste0("00", FieldCode))][, FieldCode := str_sub(FieldCode, start= -3)]

well_prod[, month_year :=  as.Date(as.yearmon(paste(year, month, sep = "-")))]

## make df of field codes and field names to join with well prod
fieldcodes <- unique(wells_19[, .(FieldCode, FieldName)])

## join to well prod
well_prod <- merge(well_prod, fieldcodes)

setnames(well_prod, c("FieldCode", "FieldName"), c('doc_field_code', "doc_fieldname"))


## ghg emissions
inv_df <- read.csv(paste0(proj_dir, raw_directory, "ghg_sector_data_og_updated.csv"), skip = 9)

inv_df <- janitor::clean_names(inv_df)

inv_df[inv_df == "No Data"] <- NA

## make state level
inv_df2 <- inv_df %>%
  mutate(x2001 = as.character(x2001),
         x2002 = as.character(x2002),
         x2003 = as.character(x2003),
         x2008 = as.character(x2008)) %>%
  pivot_longer(x2000:x2018, names_to = "year", values_to = "value") %>%
  mutate(year = str_sub(year, -4),
         year = as.integer(year),
         value = as.numeric(value)) %>%
  group_by(sub_sector_level_1, year) %>%
  summarise(co2e = sum(value, na.rm = T)) %>%
  ungroup()


write_csv(inv_df2, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/historic_ghg_emissions_og.csv")

## adjust emissions for natural gas
## -------------------------------------

ng <- read_xls(paste0(data_directory, "raw/N9010CA2a.xls"), sheet = 2, skip = 2)

## conversions
bbl_mj <- 6011
btu_mj <- 0.00105506
mmcf_mj <- 1094100


## ng

colnames(ng) <- c("date", "ng_withdrawls_mmcf")

ng_db <- ng %>%
  mutate(year = year(date),
         ng_mj_eia = ng_withdrawls_mmcf * mmcf_mj) %>%
  select(year, ng_mj_eia)

ng_db2 <- ng %>%
  mutate(year = year(date),
         ng_mj_eia = ng_withdrawls_mmcf * mmcf_mj)


## oil and gas production -- all fields
prod_df_allfields <- well_prod %>%
  group_by(year) %>%
  summarise(btu_gas_prod = sum(BTUofGasProduced, na.rm = T),
            bbls_oil_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(ng_db) %>%
  mutate(ng_mj = btu_gas_prod * 1e6 * btu_mj,
         oil_mj = bbls_oil_prod * bbl_mj) %>%
  rowwise() %>%
  mutate(total_energy_mj_doc = ng_mj + oil_mj,
         total_energy_mj_mix = ng_mj_eia + oil_mj,
         oil_rel_engery_doc = oil_mj / total_energy_mj_doc,
         ng_rel_energy_doc = ng_mj / total_energy_mj_doc,
         oil_rel_energy_mix = oil_mj / total_energy_mj_mix,
         ng_rel_energy_mix = ng_mj_eia / total_energy_mj_mix) %>%
  ungroup() %>%
  mutate(diff = ng_rel_energy_mix - ng_rel_energy_doc) %>%
  left_join(inv_df2) %>%
  mutate(kg_co2e = co2e * 1000 * 1e6,
         adj_kg_co2e = kg_co2e * oil_rel_energy_mix,
         adj_kg_co2e_doc = kg_co2e * oil_rel_engery_doc) %>%
  select(-sub_sector_level_1)

extract_emis_allfields <- prod_df_allfields %>%
  select(year, bbls_oil_prod, kg_co2e, adj_kg_co2e, adj_kg_co2e_doc) %>%
  filter(year >= 2000,
         year <= 2018) %>%
  mutate(description = "all_fields")


## oil and gas production -- only fields included in analysis
prod_df_select_fields <- well_prod %>%
  filter(doc_field_code %in% unique(extract_field_out$doc_field_code)) %>%
  group_by(year) %>%
  summarise(btu_gas_prod = sum(BTUofGasProduced, na.rm = T),
            bbls_oil_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(ng_db) %>%
  mutate(ng_mj = btu_gas_prod * 1e6 * btu_mj,
         oil_mj = bbls_oil_prod * bbl_mj) %>%
  rowwise() %>%
  mutate(total_energy_mj_doc = ng_mj + oil_mj,
         total_energy_mj_mix = ng_mj_eia + oil_mj,
         oil_rel_engery_doc = oil_mj / total_energy_mj_doc,
         ng_rel_energy_doc = ng_mj / total_energy_mj_doc,
         oil_rel_energy_mix = oil_mj / total_energy_mj_mix,
         ng_rel_energy_mix = ng_mj_eia / total_energy_mj_mix) %>%
  ungroup() %>%
  mutate(diff = ng_rel_energy_mix - ng_rel_energy_doc) %>%
  left_join(inv_df2) %>%
  mutate(kg_co2e = co2e * 1000 * 1e6,
         adj_kg_co2e = kg_co2e * oil_rel_energy_mix,
         adj_kg_co2e_doc = kg_co2e * oil_rel_engery_doc) %>%
  select(-sub_sector_level_1)

extract_emis_select_fields <- prod_df_select_fields %>%
  select(year, bbls_oil_prod, kg_co2e, adj_kg_co2e, adj_kg_co2e_doc) %>%
  filter(year >= 2000,
         year <= 2018) %>%
  mutate(description = "select_fields") 

emissions_out <- extract_emis_select_fields %>%
  rbind(extract_emis_allfields) %>%
  mutate(mtco2e = kg_co2e / (1e6 * 1000),
         adj_mtco2e = adj_kg_co2e / (1e6 * 1000),
         adj_doc_mtco2e = adj_kg_co2e_doc / (1e6 * 1000)) %>%
  select(year, mtco2e, adj_mtco2e, adj_doc_mtco2e, description) %>%
  pivot_longer(mtco2e:adj_doc_mtco2e, names_to = "emissions_type", values_to = "mtco2e")

# emission_fig <- 
#   # ggplot(emissions_out %>% filter(emissions_type != "adj_doc_mtco2e"), aes(x = year, y = mtco2e, lty = emissions_type, color = description)) +
#   ggplot(emissions_out, aes(x = year, y = mtco2e, lty = emissions_type, color = description)) +
#   # geom_line(size = 1, alpha = 0.8, color = prim_calepa_pal[2]) +
#   geom_line(size = 1, alpha = 0.6) +
#   scale_linetype_manual(values = c("dotted", "solid")) +
#   # scale_color_manual(values = prim_calepa_pal) +
#   # ylim(0, 100) +
#   ylab(expression(paste("Mt ", CO[2], "e"))) +
#   # base_theme +
#   labs(lty = "GHG emissions: ") +
#   theme(legend.key.width = unit(1,"cm"),
#         legend.title = element_text(size = 12))

## don't use field adj
## -------------------------

adj_emissions_df <- extract_emis_allfields %>%
  mutate(mtco2e = kg_co2e / (1e6 * 1000),
         adj_mtco2e = adj_kg_co2e / (1e6 * 1000),
         adj_doc_mtco2e = adj_kg_co2e_doc / (1e6 * 1000)) %>%
  select(year, mtco2e, adj_mtco2e) %>%
  pivot_longer(mtco2e:adj_mtco2e, names_to = "emissions_type", values_to = "mtco2e")

write_csv(adj_emissions_df, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/historic_ghg_emissions_og_ng_adjusted.csv")





         
         