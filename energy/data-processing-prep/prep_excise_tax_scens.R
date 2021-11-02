## Tracey Mangin
## August 19 ,2021
## prep excise tax scenarios

library(tidyverse)
library(data.table)
library(rebus)

## paths
scen_path  = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/'

## create full set for finding new scenarios

excise_tax_df <- expand.grid(year = c(2020:2045),
                             tax_rate = c(0, 0.10, 0.50, 0.90, 1.00, seq(0.01, 0.02, 0.001), seq(0.05, 0.06, 0.001), seq(0.26, 0.27, 0.001),
                                          seq(0.70, 0.80, 0.001)))

excise_tax_df2 <- excise_tax_df %>%
  mutate(excise_tax_scenario = paste0("tax_", tax_rate),
         excise_tax_scenario = ifelse(excise_tax_scenario == "tax_0", "no tax", excise_tax_scenario)) %>%
  mutate(units = "fraction of oil price")

fwrite(excise_tax_df2, paste0(scen_path, 'all_excise_tax_scenarios.csv'))


## final set
## ----------------------------------------------

setback_tax_out <- fread(paste0(scen_path, "setback_tax_values.csv"))

## values are the same across ccs_scenarios, take unique vals
setback_tax_out <- unique(setback_tax_out[, .(excise_tax_scenario, total_ghg_mtCO2e, target_scen, target_emission)])


excise_tax_df <- expand.grid(year = c(2020:2045),
                             tax_rate = c(0, 0.05, 0.10, 0.50, 0.90, 1.00, as.numeric(str_remove(setback_tax_out[, excise_tax_scenario], "tax_"))))

rename_df <- setback_tax_out[, excise_tax_scenario, target_scen]
rename_df[, new_name := paste0('tax_', target_scen)]
# rename_df[, excise_tax_scenario := str_remove(excise_tax_scenario, "0" %R% DOT)]

excise_tax_df2 <- excise_tax_df %>%
  mutate(excise_tax_scenario = paste0("tax_", tax_rate)) %>%
  left_join(rename_df) %>%
  mutate(excise_tax_scenario = ifelse(excise_tax_scenario == "tax_0", "no tax",
                                      ifelse(is.na(target_scen) & excise_tax_scenario != "tax_0", excise_tax_scenario, new_name))) %>%
  select(-target_scen, -new_name) %>%
  mutate(units = "fraction of oil price")

fwrite(excise_tax_df2, paste0(scen_path, 'final_excise_tax_scenarios.csv'))

## now just include "new" scens

excise_tax_df3 <- excise_tax_df2 %>%
  filter(excise_tax_scenario %in% c("no tax", paste("tax", setback_tax_out$target_scen, sep = "_")))

fwrite(excise_tax_df3, paste0(scen_path, 'comparison_excise_tax_scenarios.csv'))


