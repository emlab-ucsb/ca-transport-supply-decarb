## Tracey Mangin
## November 16, 2021
## create social cost of carbon df

## revised: feb 16 2024 by Haejin 

## libraries
library(tidyverse)
library(data.table)
#library(blscrapeR)
# library(tabulizer)
library(zoo)
library(openxlsx)
library(readxl)


## path
main_path <- '/capstone/freshcair/meds-freshcair-capstone/' # revised file path 

## CPI data
carbon_px_file <- 'carbon_price_scenarios_revised.xlsx'

## read in carbon file
cpi_df <- setDT(read.xlsx(paste0(main_path, 'data/inputs/', carbon_px_file), sheet = 'BLS Data Series', startRow = 12))

cpi_df <- cpi_df[Year %in% c(2019, 2020), .(Year, Annual)]

setnames(cpi_df, c("Year", "Annual"), c("year", "annual"))

cpi2020 <- cpi_df %>%
  filter(year == 2020) %>%
  select(annual) %>%
  as.numeric()

cpi2019 <- cpi_df %>%
  filter(year == 2019) %>%
  select(annual) %>%
  as.numeric()


## ref
## https://www.whitehouse.gov/wp-content/uploads/2021/02/TechnicalSupportDocument_SocialCostofCarbonMethaneNitrousOxide.pdf

# scc_df <- extract_tables(paste0(main_path, 'data/stocks-flows/raw/social_cost_of_carbon_fact_sheet.pdf'), pages = 4)

year_df <- data.table(year = 2020:2050)

scc_df <- data.table(year = seq(2020, 2050, 5),
                     five_perc_avg = c(14, 17, 19, 22, 25, 28, 32),
                     three_perc_avg = c(51, 56, 62, 67, 73, 79, 85),
                     two_pt_five_avg = c(76, 83, 89, 96, 103, 110, 116),
                     perc_95 = c(152, 169, 187, 206, 225, 242, 260))

scc_df <- merge(year_df, scc_df,
                by = "year",
                all.x = T)

scc_df <- melt(scc_df, id.vars = c("year"),
               measure.vars = c("five_perc_avg", "three_perc_avg", "two_pt_five_avg", "perc_95"),
               variable.name = "discount_rate", value.name = "social_cost_co2")

scc_df[, social_cost_co2 := na.approx(social_cost_co2), by = .(discount_rate)]

## convert to 2019
scc_df[, social_cost_co2_19 := social_cost_co2 / cpi2020 * cpi2019]

## add ref
scc_df[, scc_ref := 'https://www.whitehouse.gov/wp-content/uploads/2021/02/TechnicalSupportDocument_SocialCostofCarbonMethaneNitrousOxide.pdf']


fwrite(scc_df, file.path(main_path, 'data/processed/social_cost_carbon.csv'), row.names = F)

