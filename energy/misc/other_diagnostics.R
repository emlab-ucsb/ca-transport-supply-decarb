## Tracey Mangin
## June 24, 2021
## random diagnostics 

library(tidyverse)
library(data.table)
library(sf)
library(maps)
library(cowplot)

## paths
proj_dir          = "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
save_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/diagnostics/'

## files
ghg_file        = 'ghg_emissions_x_field_2015_revised.csv'
entry_file      = 'stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
prod_file       = "well_prod_m_processed.csv"

## read in info
ghg_factors = fread(file.path(outputs_path, 'stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors = ghg_factors[, .(doc_field_code, upstream_kgCO2e_bbl)]


## for opex and capex
entry_dt = fread(file.path(outputs_path, entry_file), header = T, colClasses = c('doc_field_code' = 'character'))

## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))

## 2019 well prod
prod_2019 <- well_prod[, .(prod = sum(OilorCondensateProduced, na.rm = T)), by = .(year, doc_field_code)]
prod_2019 <- prod_2019[year == 2019]

## opex
opex <- entry_dt %>%
  filter(year == 2019) %>%
  dplyr::select(doc_field_code, doc_fieldname, opex_imputed)

## combine
opex_emis_factors <- left_join(opex, ghg_factors) %>%
  left_join(prod_2019) %>%
  mutate(prod = ifelse(is.na(prod), 0, prod))

## ggplot

opex_fig <- 
ggplot(opex_emis_factors, aes(x = opex_imputed, y = upstream_kgCO2e_bbl, size = prod / 1e6, color = prod / 1e6)) +
  geom_point() +
  labs(x = '2019 opex imputed',
       y = 'kgCO2e per bbl',
       size = '2019 production (mbbls)',
       color = '2019 production (mbbls)') +
  theme_bw() +
  theme(legend.position = 'bottom')
# +
#   geom_text(data = opex_emis_factors, aes(label = doc_fieldname), size = 2, color = 'black')

ggsave(opex_fig, 
       filename = file.path(save_path, 'opex_emisfactor_fig.pdf'), 
       width = 8, S
       height = 8)

## new well entry by field, by 2045


