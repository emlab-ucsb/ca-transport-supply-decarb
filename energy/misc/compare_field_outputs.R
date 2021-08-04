## Tracey Mangin
## August 4, 2021
## compare field-level outputs for BAU, new production

library(data.table)
library(tidyverse)

## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
output_path <- "outputs/predict-production"
prev_path <- "extraction_2021-07-22/revised-ccs-correct-new-setback"
recent_path <- "extraction_2021-08-03/update-adjust_existing_prod/"

## file names
field_file <- "diagnostic-field-level-results.csv"


## read in outputs
calepa_field_out <- fread('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/download/field_level_prod_emissions_2020-2045.csv', header = T,
                           colClasses = ('doc_field_code' = 'character'))

calepa_wells_out <- fread("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/archive/scenarios_20_all_scens/well_entry_state_1977-2045.csv")

prev_out <- fread(file.path(main_path, output_path, prev_path, field_file), colClasses = ('doc_field_code' = 'character'))

recent_out <- fread(file.path(main_path, output_path, recent_path, field_file), colClasses = ('doc_field_code' = 'character'))

## filter data sets to BAU, new production
## -----------------------------------------------------

## calepa
calepa_bau_out <- calepa_field_out[(oil_price_scenario == 'iea oil price' &
                                        innovation_scenario == 'low innovation' &
                                        carbon_price_scenario == 'price floor' &
                                        ccs_scenario == 'medium CCS cost' &
                                        excise_tax_scenario == 'no tax' &
                                        setback_scenario == 'no_setback' &
                                        prod_quota_scenario == 'no quota') & well_type == "new"]

calepa_bau_out[, version := "calepa"]
calepa_bau_out <- calepa_bau_out[, .(version, doc_field_code, year, n_wells, production_bbl)]


## prev
prev_out_new <- prev_out[(oil_price_scenario == 'reference case' &
                        innovation_scenario == 'low innovation' &
                        carbon_price_scenario == 'price floor' &
                        ccs_scenario == 'medium CCS cost' &
                        excise_tax_scenario == 'no tax' &
                        setback_scenario == 'no_setback' &
                        prod_quota_scenario == 'no quota'), .(doc_field_code, year, new_wells, new_prod_bbl)]

prev_out_new[, version := "update"]
setcolorder(prev_out_new, c("version", "doc_field_code", "year", "new_wells", "new_prod_bbl"))
setnames(prev_out_new, c("new_wells", "new_prod_bbl"), c("n_wells", "production_bbl"))

## most recent
recent_out_new <- recent_out[(oil_price_scenario == 'reference case' &
                        innovation_scenario == 'low innovation' &
                        carbon_price_scenario == 'price floor' &
                        ccs_scenario == 'medium CCS cost' &
                        excise_tax_scenario == 'no tax' &
                        setback_scenario == 'no_setback' &
                        prod_quota_scenario == 'no quota'), .(doc_field_code, year, new_wells, new_prod_bbl)]

recent_out_new[, version := "update2"]
setcolorder(recent_out_new, c("version", "doc_field_code", "year", "new_wells", "new_prod_bbl"))
setnames(recent_out_new, c("new_wells", "new_prod_bbl"), c("n_wells", "production_bbl"))

## bind
new_prod <- rbind(calepa_bau_out, prev_out_new, recent_out_new)
setcolorder(new_prod, c('doc_field_code', 'year', 'version', 'n_wells', 'production_bbl'))

new_prod2 <- copy(new_prod)
new_prod2[, n_wells := NULL]

new_prod2 <- dcast(new_prod2, doc_field_code + year ~ version, value.var = "production_bbl")
new_prod2[is.na(new_prod2)] <- 0

new_prod2[, ":=" (year = as.integer(year),
                  calepa = as.numeric(calepa),
                  update = as.numeric(update),
                  update2 = as.numeric(update2))]

## udpate2 compared to calepa and update
new_prod2[, ":=" (diff_calepa = update2 - calepa,
                  diff_orig_update = update2 - update)]

diff_update_plot <- ggplot(new_prod2, aes(x = year, y = diff_orig_update, group = doc_field_code)) +
  geom_line(size = 0.5, alpha = 0.5)

ggplotly(diff_update_plot)

diff_calepa_plot <- ggplot(new_prod2, aes(x = year, y = diff_calepa, group = doc_field_code)) +
  geom_line(size = 0.5, alpha = 0.5)

ggplotly(diff_calepa_plot)

## plot total production through time for Belridge South, Cymric, Wilmington, and Ventura

## prev -- total production
prev_out_total <- prev_out[(oil_price_scenario == 'reference case' &
                            innovation_scenario == 'low innovation' &
                            carbon_price_scenario == 'price floor' &
                            ccs_scenario == 'medium CCS cost' &
                            excise_tax_scenario == 'no tax' &
                            setback_scenario == 'no_setback' &
                            prod_quota_scenario == 'no quota'), .(doc_field_code, year, existing_prod_bbl, new_prod_bbl, total_prod_bbl)]

prev_out_total[, version := "update"]
setcolorder(prev_out_total, c("version", "doc_field_code", "year", "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"))


## recent -- total production
recent_out_total <- recent_out[(oil_price_scenario == 'reference case' &
                              innovation_scenario == 'low innovation' &
                              carbon_price_scenario == 'price floor' &
                              ccs_scenario == 'medium CCS cost' &
                              excise_tax_scenario == 'no tax' &
                              setback_scenario == 'no_setback' &
                              prod_quota_scenario == 'no quota'), .(doc_field_code, year, existing_prod_bbl, new_prod_bbl, total_prod_bbl)]

recent_out_total[, version := "update2"]
setcolorder(recent_out_total, c("version", "doc_field_code", "year", "existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"))

## bind
total_prod <- rbind(prev_out_total, recent_out_total)
total_prod[, ":=" (year = as.integer(year),
                   new_prod_bbl = as.numeric(new_prod_bbl),
                   existing_prod_bbl = as.numeric(existing_prod_bbl),
                   total_prod_bbl = as.numeric(total_prod_bbl))]
## melt
total_prod <- melt(total_prod, id.vars = c("version", "doc_field_code", "year"),
                   measure.vars = c("existing_prod_bbl", "new_prod_bbl", "total_prod_bbl"))

## wide to add zeros
total_prod <- dcast(total_prod, doc_field_code + year + variable ~ version, value.var = "value")
total_prod[is.na(total_prod)] <- 0

## melt back
total_prod <- melt(total_prod, id.vars = c("doc_field_code", "year", "variable"),
                   measure.vars = c("update", "update2"))

setnames(total_prod, c("variable", "variable.1"), c("prod_type", "version"))



total_prod_fields <- ggplot(total_prod %>% filter(doc_field_code %in% c("052", "190")), 
                            aes(x = year, y = value, group = doc_field_code, color = doc_field_code, lty = version)) +
  geom_line(size = 0.5, alpha = 0.5) +
  facet_wrap(~prod_type)

ggplotly(total_prod_fields)

## plot new wells for 052 and 190
## ----------------------------------------

## plot total production through time for Belridge South, Cymric, Wilmington, and Ventura

## prev -- total production
prev_out_wells <- prev_out[(oil_price_scenario == 'reference case' &
                              innovation_scenario == 'low innovation' &
                              carbon_price_scenario == 'price floor' &
                              ccs_scenario == 'medium CCS cost' &
                              excise_tax_scenario == 'no tax' &
                              setback_scenario == 'no_setback' &
                              prod_quota_scenario == 'no quota'), .(doc_field_code, year, new_wells)]

prev_out_wells[, version := "update"]
setcolorder(prev_out_wells, c("version", "doc_field_code", "year", "new_wells"))


## recent -- total production
recent_out_wells <- recent_out[(oil_price_scenario == 'reference case' &
                                  innovation_scenario == 'low innovation' &
                                  carbon_price_scenario == 'price floor' &
                                  ccs_scenario == 'medium CCS cost' &
                                  excise_tax_scenario == 'no tax' &
                                  setback_scenario == 'no_setback' &
                                  prod_quota_scenario == 'no quota'), .(doc_field_code, year, new_wells)]

recent_out_wells[, version := "update2"]
setcolorder(recent_out_wells, c("version", "doc_field_code", "year", "new_wells"))

## bind
wells_out <- rbind(prev_out_wells, recent_out_wells)
wells_out[, ":=" (year = as.integer(year),
                  new_wells = as.numeric(new_wells))]

## wide to add zeros
wells_out <- dcast(wells_out, doc_field_code + year ~ version, value.var = "new_wells")
wells_out[is.na(wells_out)] <- 0

## melt back
wells_out <- melt(wells_out, id.vars = c("doc_field_code", "year"),
                   measure.vars = c("update", "update2"))

wells_out[, id := paste0(doc_field_code, "-", variable)]

well_fig <- ggplot(wells_out %>% filter(doc_field_code %in% c("052", "190")), 
                            aes(x = year, y = value, group = id, color = doc_field_code, lty = variable)) +
  geom_line(size = 0.5, alpha = 0.5) 

ggplotly(well_fig)




