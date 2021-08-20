## Tracey Mangin
## August 19 ,2021
## prep excise tax scenarios

library(tidyverse)
library(data.table)
library(rebus)

## paths
scen_path  = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/'

setback_tax_out <- fread(paste0(scen_path, "setback_tax_values.csv"))


excise_tax_df <- expand.grid(year = c(2020:2045),
                             tax_rate = c(0, 0.05, 0.10, 0.50, 0.90, 1.00, as.numeric(str_remove(setback_tax_out[, excise_tax_scenario], "tax_"))))

rename_df <- setback_tax_out[, excise_tax_scenario, match_scen]
rename_df[, new_name := paste0('tax_', match_scen)]
rename_df[, excise_tax_scenario := str_remove(excise_tax_scenario, "0" %R% DOT)]

excise_tax_df2 <- excise_tax_df %>%
  mutate(excise_tax_scenario = paste0("tax_", tax_rate)) %>%
  mutate(excise_tax_scenario = str_remove(excise_tax_scenario, "0" %R% DOT),
         excise_tax_scenario = ifelse(excise_tax_scenario %in% c('tax_5', 'tax_9'), paste0(excise_tax_scenario, "0"),
                                      ifelse(tax_rate == 1.000, 'tax_100', 
                                             ifelse(tax_rate == 0.100, 'tax_10', excise_tax_scenario)))) %>%
  left_join(rename_df) %>%
  mutate(excise_tax_scenario = ifelse(excise_tax_scenario == "tax_0", "no tax",
                                      ifelse(is.na(match_scen) & excise_tax_scenario != "tax_0", excise_tax_scenario, new_name))) %>%
  select(-match_scen, -new_name) %>%
  mutate(units = "fraction of oil price")

fwrite(excise_tax_df2, paste0(scen_path, 'final_excise_tax_scenarios.csv'))



# fwrite(excise_tax_df, paste0(scen_path, 'all_excise_tax_scenarios.csv'))

