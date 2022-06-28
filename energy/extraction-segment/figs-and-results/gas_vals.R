## Tracey Mangin
## June 28, 2022
## natural gas production

library(tidyverse)
library(data.table)

## paths 
main_path       <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path       <-'data/stocks-flows/processed/'

## files
prod_file       <- 'well_prod_m_processed.csv'

## conversion information
btu_per_gal <- 5691000 # https://www.eia.gov/energyexplained/units-and-calculators/
btu_per_cf_ng <- 1037  # https://www.eia.gov/energyexplained/units-and-calculators/
btu_per_mcf_ng <- btu_per_cf_ng * 1000

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

## annual gas and oil produced
gas_prod_yr <- well_prod %>%
  group_by(year) %>%
  summarise(total_gas_mcf = sum(GasProduced, na.rm = T),
            total_bbls = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(btu_bbls = total_bbls * btu_per_gal,
         btu_ng = total_gas_mcf * btu_per_mcf_ng,
         total_btu = btu_bbls + btu_ng,
         ng_rel_btu = btu_ng / total_btu)

## from eia: nat gas from oil wells
ng_mcf_2019 <- 45270000 ## https://www.eia.gov/dnav/ng/NG_PROD_SUM_DC_SCA_MMCF_A.htm
ng_btu_2019 <- ng_mcf_2019 * btu_per_mcf_ng

oil_btu_2019 <- gas_prod_yr %>%
  filter(year == 2019) %>%
  select(btu_bbls) %>%
  as.numeric()

## rel energy content 2019
rel_val <- ng_btu_2019 / oil_btu_2019


## gas produced in mcf
## oil produced in bbls

## mcf = MCF is an abbreviation derived from the Roman numeral M for one thousand, 
## put together with cubic feet (CF) to measure a quantity of natural gas. 
## For example, a natural gas well that produces 400 MCF of gas per day operates 
## with a daily production rate of 400,000 cubic feet. In terms of energy output, 
## one thousand cubic feet (MCF) of gas is equal to approximately 1,000,000 BTU (British Thermal Units).



# ## 
# prod <- well_prod %>%
#   filter(year == 2018)
# 
# ## 2017 gas prod
# gas_prod_18_MMCF <- sum(prod$GasProduced, na.rm = T) / 1000
# 
# ## total natural gas consumed in ca according to EIA
# ## 2018: 2,136,907 MMcf
# ca_gas_consump <- 2136907 
# 
# ## ratio
# gas_prod_17_MMCF / ca_gas_consump
# 
# ## 2017 CA consumption natural gas for transportation
# ## 217261208 therms
# 
# ca_tranp_c <- 217261208
# 
# therm_mmbtu <- 0.10
# 
# ## therms to MMBTU
# ca_trans_c_mmbtu <- ca_tranp_c * 0.10
# 
# ## mcf
# ca_trans_c_mcf = ca_trans_c_mmbtu / 1000
# 
# ## MMcf
# ca_trans_c_mmcf <- ca_trans_c_mcf / 1000
# 
# ca_trans_c_mmcf / ca_gas_consump
# 
# 
# 
# 
# 
# 
# prod19 <- well_prod %>%
#   filter(year == 2019,
#          OilorCondensateProduced > 0) %>%
#   mutate(gas_prod = ifelse(GasProduced > 0, 1, 0))
# 
# sum(prod19$gas_prod)
# sum(prod19$GasProduced)
# 
# sum(prod19$gas_prod) / nrow(prod19)
# 
# sum(gas_prod19$GasProduced) / sum(prod$GasProduced, na.rm = T)
# 
# prod_wells_wt <- prod19 %>%
#   group_by(well_type_name) %>%
#   summarise(n = n()) %>%
#   ungroup()