## November 28, 2022
## Compare entry/exit values

## libraries
library(data.table)
library(tidyverse)

## paths
main_path   <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
model_path  <- paste0(main_path, 'outputs/entry-model-results/')
save_path  <- paste0(main_path, 'outputs/academic-out/extraction/nature-energy-rev-outputs/')

## files
orig_nw_file    <- "new_wells_pred_revised.csv"
orig_entry_file <- "poisson_regression_coefficients_revised.csv" 
rev_nw_file     <- "new_wells_pred_revised_2211.csv"
rev_entry_file  <- "poisson_regression_coefficients_revised_2211.csv" 
entry_file      <- "entry_revised.csv"
r_entry_file  <- "entry_revised_2211.csv"

## read in files
orig_nw <- fread(paste0(model_path, orig_nw_file), header = T, colClasses = c('doc_field_code' = 'character'))
rev_nw <- fread(paste0(model_path, rev_nw_file), header = T, colClasses = c('doc_field_code' = 'character'))

orig_entry <- fread(paste0(model_path, orig_entry_file), header = T, colClasses = c('doc_field_code' = 'character'))
rev_entry <- fread(paste0(model_path, rev_entry_file), header = T, colClasses = c('doc_field_code' = 'character'))

orig_e <- fread(paste0(model_path, "rui-files/", entry_file), header = T, colClasses = c('doc_field_code' = 'character'))
rev_e <- fread(paste0(model_path, "rui-files/", r_entry_file), header = T, colClasses = c('doc_field_code' = 'character'))

## pivot longer and merge

orig_nw <- orig_nw %>%
  pivot_longer(new_wells:new_wells_pred, names_to = "name", values_to = "orig_value")

rev_nw <- rev_nw %>%
  pivot_longer(new_wells:new_wells_pred, names_to = "name", values_to = "rev_value")

new_well_comp <- orig_nw %>%
  left_join(rev_nw) %>%
  mutate(diff = rev_value - orig_value)

write_csv(new_well_comp, paste0(save_path, "comp_new_well_pred.csv"))

##
orig_entry <- orig_entry %>%
  pivot_longer(brent_hat:fixed_effect, names_to = "name", values_to = "orig_value")

rev_entry <- rev_entry %>%
  pivot_longer(brent_hat:fixed_effect, names_to = "name", values_to = "rev_value")

entry_comp <- orig_entry %>%
  left_join(rev_entry) %>%
  mutate(diff = rev_value - orig_value)

write_csv(entry_comp, paste0(save_path, "comp_entry_coefs.csv"))

##
orig_e <- orig_e %>%
  pivot_longer(new_wells:field_categ, names_to = "name", values_to = "orig_value")

rev_e <- rev_e %>%
  pivot_longer(new_wells:field_categ, names_to = "name", values_to = "rev_value")

e_comp <- orig_e %>%
  left_join(rev_e) %>%
  mutate(diff = rev_value - orig_value)

## category df
cat_comp <- e_comp %>%
  filter(name == "field_categ") %>%
  select(doc_field_code, doc_fieldname, name, orig_value, value_2211 = rev_value, diff) %>%
  unique() %>%
  mutate(diff_value = ifelse(diff != 0, 1, 0)) %>%
  select(-diff)

write_csv(cat_comp, paste0(save_path, "cat_comp_entry_revised.csv"))


