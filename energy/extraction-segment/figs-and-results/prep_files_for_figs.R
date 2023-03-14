## Tracey Mangin
## December 17, 2022
## prep files for Kyle

library(data.table)
library(tidyverse)


main_path         = "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"
data_path         = paste0(main_path,'data/stocks-flows/processed')
rystad_path       = paste0(main_path, "data/Rystad/data/raw/")
save_path         = paste0(main_path, "outputs/academic-out/extraction/figures/nature-energy-revision/setback-revision/")

oil_price_file    = 'oil_price_projections_revised.xlsx'
brent_file        = "wti_brent.csv"


# load oil price data
oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## save
fwrite(oilpx_scens, paste0(save_path, "nominal_oil_px_scens.csv"))

## historical brent oil prices
prices <- fread(paste0(rystad_path, brent_file))

colnames(prices) <- c("year", "wti", "brent")

prices2 <- prices %>%
  filter(year != "Year",
         year >= 1977 & year <= 2019) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(year, brent)

fwrite(prices2, paste0(save_path, "historial_brent.csv"))




