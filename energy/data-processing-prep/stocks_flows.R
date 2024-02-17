## Tracey Mangin
## February 20, 2020
## Data cleaning: Focus areas 1 and 2


## set directory
data_directory <- "/capstone/freshcair/meds-freshcair-capstone/data/inputs/"
save_directory <- "//capstone/freshcair/meds-freshcair-capstone/data/processed/"


## attach libraries
library(tidyverse)
library(janitor)
library(rebus)
library(readxl)
library(countrycode)
library(rJava)
# library(tabulizer)
# library(tabulizerjars)
library(lubridate)
library(scales)
library(openxlsx)


## Crude imports to CA ports by export country
## ---------------------------------------------------------------------------------------

## read in data
port_imports <- read_csv(paste0(data_directory, "inputs/Imports_of_Heavy_Sour_to_Los_Angeles_CA.csv"), skip = 4) ## first four rows mess up data
port_imports <- clean_names(port_imports)

## get info from raw data
port_imports_info <- read_csv(paste0(data_directory, "inputs/Imports_of_Heavy_Sour_to_Los_Angeles_CA.csv"))
port_imports_info <- port_imports_info[1:3, 1]
colnames(port_imports_info) <- c("info")

## break out information
oiltype <- c("Heavy Sweet", "Light Sour", "Light Sweet", "Medium", "Heavy Sour")

port_imports_clean <- port_imports %>%
  rename(cats = span_style_float_right_thousand_barrels_span) %>%
  mutate(export_region = countrycode(cats, 'country.name', 'country.name'),
         export_region = ifelse(cats == "World", "World", export_region),
         port = str_detect(cats, pattern = ", CA" %R% END),
         port = ifelse(port == TRUE, cats, NA),
         oil = ifelse(cats %in% oiltype, cats, NA)) %>%
  select(cats, export_region, port, oil, jan_2009:nov_2019) %>%
  fill(export_region, .direction = 'down') %>%
  fill(port, .direction = 'down') %>%
  select(-cats) %>%
  filter(!is.na(oil)) %>%
  pivot_longer(jan_2009:nov_2019, names_to = "date", values_to = "barrels_thous") %>%
  mutate(barrels_thous = as.numeric(ifelse(barrels_thous == "--", 0, barrels_thous)),
         month_orig = str_extract(date, pattern = ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR),
         month2 = paste0(toupper(substr(month_orig, 1, 1)), substr(month_orig, 2, nchar(month_orig))),
         month = match(month2, month.abb),
         year = as.numeric(str_extract(date, pattern = DIGIT %R% DIGIT %R% DIGIT %R% DIGIT))) %>%
  select(-month_orig, -month2) %>%
  mutate(source = port_imports_info$info[3],
         link = port_imports_info$info[1],
         download_date = port_imports_info$info[2]) %>%
  mutate(region_type = ifelse(export_region == "World", "world", "country")) %>%
  select(export_region, region_type, port:download_date)

## save clean file
write_csv(port_imports_clean, path = paste0(data_directory, "processed/crude_imports_port.csv"))


## WTI monthly prices of crude
## ---------------------------------------------------------------------------------------

## read in data
spt_price_m <- read_xls(paste0(data_directory, "raw/PET_PRI_SPT_S1_M.xls"), sheet = 2, skip = 2)
colnames(spt_price_m) <- c("date", "cushing_ok_wti_FOB", "europe_brent_FOB")

spt_price_m2 <- spt_price_m %>%
  pivot_longer(cushing_ok_wti_FOB:europe_brent_FOB, names_to = "price", values_to = "value") %>%
  mutate(unit = "dollars_per_barrel",
         description = ifelse(price == "cushing_ok_wti_FOB", "Cushing, OK WTI Spot Price FOB", "Europe Brent Spot Price FOB"),
         product = "crude_oil") %>%
  select(date, description, product, price, value, unit) %>%
  mutate(source = "EIA",
         url = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_m.htm")

## save clean file
write_csv(spt_price_m2, path = paste0(data_directory, "processed/spot_price_wti_m.csv"))

## WTI annual prices of crude
## ---------------------------------------------------------------------------------------

## read in data
spt_price_a <- read_xls(paste0(data_directory, "raw/PET_PRI_SPT_S1_A.xls"), sheet = 2, skip = 2)
colnames(spt_price_a) <- c("date", "cushing_ok_wti_FOB", "europe_brent_FOB")

spt_price_a2 <- spt_price_a %>%
  pivot_longer(cushing_ok_wti_FOB:europe_brent_FOB, names_to = "price", values_to = "value") %>%
  mutate(unit = "dollars_per_barrel",
         description = ifelse(price == "cushing_ok_wti_FOB", "Cushing, OK WTI Spot Price FOB", "Europe Brent Spot Price FOB"),
         product = "crude_oil") %>%
  select(date, description, product, price, value, unit) %>%
  mutate(source = "EIA",
         url = "https://www.eia.gov/dnav/pet/pet_pri_spt_s1_a.htm")

## save clean file
write_csv(spt_price_a2, file = paste0(data_directory, "processed/eia_spot_price_a.csv"))

## Domestic Crude Oil First Purchase Prices for Selected Crude Streams
## ---------------------------------------------------------------------------------------

## read in data
firstp_p_streams <- read_xls(paste0(data_directory, "raw/PET_PRI_DFP2_K_M.xls"), sheet = 2, skip = 2)
colnames(firstp_p_streams) <- c("date", "ak_ns", "ca_kr", "ca_ms", "hls", "lls", "mb", "wti", "wts", "ws")

firstp_p_streams2 <- firstp_p_streams %>%
  pivot_longer(ak_ns:ws, names_to = "crude_stream", values_to = "price") %>%
  mutate(unit = "dollars_per_barrel",
         description = ifelse(crude_stream == "ak_ns", "Alaska North Slope First Purchase Price", 
                              ifelse(crude_stream == "ca_kr", "California Kern River First Purchase Price",
                                     ifelse(crude_stream == "ca_ms", "California Midway-Sunset First Purchase Price",
                                            ifelse(crude_stream == "hls", "Heavy Louisiana Sweet First Purchase Price",
                                                   ifelse(crude_stream == "lls", "Light Louisiana Sweet First Purchase Price",
                                                          ifelse(crude_stream == "mb", "Mars Blend First Purchase Price",
                                                                 ifelse(crude_stream == "wti", "West Texas Intermediate First Purchase Price",
                                                                        ifelse(crude_stream == "wts", "West Texas Sour First Purchase Price", "Wyoming Sweet First Purchase Price")))))))),
         product = "crude_oil") %>%
  select(date, description, product, crude_stream, price, unit) %>%
  mutate(source = "EIA",
         url = "https://www.eia.gov/dnav/pet/pet_pri_dfp2_k_m.htm")

## save clean file
write_csv(firstp_p_streams2, path = paste0(data_directory, "processed/domestic_crude_first_p_price_streams.csv"))


## Crude oil productoin in California
## ---------------------------------------------------------------------------------------

## read in data
crude_prod_ca <- read_xls(paste0(data_directory, "raw/MCRFPCA1m.xls"), sheet = 2, skip = 2)
colnames(crude_prod_ca) <- c("date", "crude_prod_ca_thous_b")

crude_prod_ca2 <- crude_prod_ca %>%
  mutate(description = "California Field Production of Crude Oil",
         source = "EIA",
         url = "https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=mcrfpca1&f=m")

## save clean file
write_csv(crude_prod_ca2, path = paste0(data_directory, "processed/ca_crude_prod_m.csv"))

## Emissions by Facility
## ---------------------------------------------------------------------------------------
emissions_fac <- read_csv(paste0(data_directory, "raw/EmissionsByFacility.csv"))
emissions_fac <- clean_names(emissions_fac)

# ## how many refineries in 2017?
# refin_2017 <- emissions_fac %>%
#   filter(year == 2017,
#          naics_code == 324110)
# 
# refin_2014 <- emissions_fac %>%
#   filter(year == 2014,
#          naics_code == 324110)
# 
# refin_2013 <- emissions_fac %>%
#   filter(year == 2013,
#          naics_code == 324110)

## save clean file
write_csv(emissions_fac, path = paste0(data_directory, "processed/emissions_by_facility.csv"))

## oil and gas production by county
## ---------------------------------------------------------------------------------------
oil_gas_county <- read_xls(paste0(data_directory, "raw/oilgascounty.xls"), sheet = 1)
oil_gas_county <- clean_names(oil_gas_county)

## make longer
oil_gas_county2 <- oil_gas_county %>%
  pivot_longer(oil2000:gas2011, names_to = "type_year", values_to = "value") %>%
  mutate(type = str_extract(type_year, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR),
         year = as.numeric(str_extract(type_year, pattern = DIGIT %R% DIGIT %R% DIGIT %R% DIGIT %R% END))) %>%
  select(fips:oil_gas_change_group, type, year, value)

## save clean file
write_csv(oil_gas_county2, path = paste0(data_directory, "processed/oil_gas_county.csv"))  

## Crude oil productoin in California -- annual
## ---------------------------------------------------------------------------------------

## read in data
crude_prod_ca_a <- read_xls(paste0(data_directory, "raw/MCRFPCA1a.xls"), sheet = 2, skip = 2)
colnames(crude_prod_ca_a) <- c("date", "crude_prod_ca_thous_b")

crude_prod_ca_a2 <- crude_prod_ca_a %>%
  mutate(description = "California Field Production of Crude Oil",
         source = "EIA",
         url = "https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=mcrfpca1&f=a")

## save clean file
write_csv(crude_prod_ca_a2, path = paste0(data_directory, "processed/ca_crude_prod_a.csv"))

## Sector summaries
## ---------------------------------------------------------------------------
pdf1 <- paste0(data_directory, "raw/ghg_inventory_scopingplan_sum_2000-17.pdf")

inventory_sum <- file.path(pdf1)

pdf_txt <- tabulizer::extract_text(inventory_sum)
tbl1 <- tabulizer::extract_tables(inventory_sum, pages = 1, output = "data.frame")
tbl2 <- tabulizer::extract_tables(inventory_sum, pages = 2, output = "data.frame")

## clean up the table
page1 <- tbl1[[1]]
page2 <- tbl2[[1]]

## page 1
toprows <- page1[1:2,]

years <- c(2000:2017)
colnames2 <- paste0("X", years)
twoyears <- c("X2000", "X2001")

toprows2 <- toprows %>%
  select(X2000.2001) %>%
  rename(sector = X2000.2001) %>%
  mutate(sector = str_replace(sector, pattern = "On Road", "On_Road")) %>%
  separate(sector, into = c("sector", colnames2), sep = " ") %>%
  mutate(sector = str_replace(sector, pattern = "On_Road", "On Road"))

bottomrows <- page1[3:26,]

bottomrows2 <- bottomrows %>%
  mutate(sector = str_remove_all(X2000.2001, pattern = SPACE %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT) %R%
                                  SPACE %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT) %R% END),
         X2000.2001 = str_remove(X2000.2001, pattern = sector)) %>%
  select(sector, X2000.2001, X2002:X2017) %>%
  mutate(X2000.2001 = ifelse(X2000.2001 == "Aviation (Intrastate) 4.21 4.02", "4.21 4.02",
                             ifelse(X2000.2001 == "Off Road [1] 2.63 2.79", "2.63 2.79",
                                    ifelse(X2000.2001 == "Oil & Gas: Production & Processing [2] 19.39 19.62", "19.39 19.62", X2000.2001))),
         X2000.2001 = str_remove(X2000.2001, pattern = START %R% SPACE)) %>%
  separate(X2000.2001, into = twoyears, sep = " ")

row27 <- page1[27, ]

row27b <- row27 %>%
  select(X2000.2001, X2010:X2017) %>%
  rename(sector = X2000.2001) %>%
  mutate(sector = str_replace(sector, pattern = "Electric Power", "Electric_Power")) %>%
  separate(sector, into = c("sector", colnames2[1:10]), sep = " ") %>%
  mutate(sector = str_replace(sector, pattern = "Electric_Power", "Electric Power"))

lastrows <- page1[28:nrow(page1),]

lastrows2 <- lastrows %>%
  mutate(sector = str_remove_all(X2000.2001, pattern = SPACE %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT) %R%
                                   SPACE %R% one_or_more(DGT) %R% "." %R% one_or_more(DGT) %R% END),
         X2000.2001 = str_remove(X2000.2001, pattern = sector)) %>%
  select(sector, X2000.2001, X2002:X2017) %>%
  mutate(X2000.2001 = str_remove(X2000.2001, pattern = START %R% SPACE)) %>%
  separate(X2000.2001, into = twoyears, sep = " ")

page2 <- page2 %>%
  rename(sector = X)

## rbind
sector_emis <- rbind(toprows2, bottomrows2, row27b, lastrows2, page2) 

group_vec <- c(rep("Transportation", 9), rep("Industrial", 17), rep("Electric Power", 8), 
               rep("Commercial and Residential", 10), rep("Agriculture", 13),
               rep("High GWP", 4), rep("Recycling and Waste", 3))

sub_grp_vec <- c("Transportation", rep("On Road", 3), "Ships & Commercial Boats", "Aviation (Intrastate)",
                 "Rail", "Off Road [1]", "Unspecified", "Industrial", "Refineries and Hydrogen Production",
                 rep("General Fuel Use", 3), rep("Oil & Gas: Prodution & Processing [2]", 3),
                 rep("Cement Plants", 3), "Cogeneration Heat Output", rep("Other Fugitive and Process Emissions", 5),
                 "Electric Power", rep("In-State Generation", 4), rep("Imported Electricity", 3),
                 "Commercial and Residential", rep("Residential Fuel Use", 4), rep("Commercial Fuel Use", 3),
                 "Commercial Cogeneration Heat Output", "Other Commercial and Residential",
                 "Agriculture", rep("Livestock", 3), rep("Crop Growing & Harvesting", 4),
                 rep("General Fuel Use", 5), "High GWP", "Ozone Depleting Substance (ODS) Substitutes",
                 "Electricity Grid SF6 Losses [4]", "Semiconductor Manufacturing [3]", "Recycling and Waste",
                 "Landfills [3]", "Composting")

sector_emis2 <- cbind(group_vec, sub_grp_vec, sector_emis) %>%
  pivot_longer(X2000:X2017, names_to = "year", values_to = "million_tonnes_co2e") %>%
  mutate(year = as.numeric(str_remove(year, pattern = "X")))



## save clean file
write_csv(sector_emis2, path = paste0(data_directory, "processed/summary_emissions.csv"))


## Emissions by Facility -- larger data set
## ---------------------------------------------------------------------------------------
emissions_fac_all <- read_csv(paste0(data_directory, "raw/EmissionsByFacility_all.csv"))
emissions_fac_all <- clean_names(emissions_fac_all)

## oil and gas industry definition
upstraem_vec <- c(211, 213111, 213112, 333132)
midstream_vec <- c(23712, 4247, 486)
downstream_vec <- c(32411, 324191, 32511)
market_vec <- c(2212, 447, 45431)

emissions_fac_all2 <- emissions_fac_all %>%
  filter(naics_code %in% c(upstraem_vec, midstream_vec, downstream_vec, market_vect))


## refinery production potential
## ---------------------------------------------------------------------------------------
refin_cap <- read_xlsx(paste0(data_directory, "raw/Ca_oil_refinery_loc_cap.xlsx"))
refin_cap <- clean_names(refin_cap)

## save clean file
write_csv(refin_cap, path = paste0(data_directory, "processed/refinery_capacity.csv"))


## weekly fuel watch (make a function, lots of these)
## ---------------------------------------------------------------------------------------

## north
fw_north_df <- read_xlsx(paste0(data_directory, "raw/WeeklyFuelsWatch_Summary_2014-2020_North_South.xlsx"), sheet = 3, skip = 3)

date_colnames_n <- colnames(fw_north_df)
date_colnames_n2 <- convertToDate(date_colnames_n[6:length(date_colnames_n)])
date_colnames_n3 <- paste0("X", date_colnames_n2)

fw_north_df_cols <- c("stock_flow", "category", "sub_cat", "cat_total", "empty", date_colnames_n3)
colnames(fw_north_df) <- fw_north_df_cols

fw_north_df2 <- fw_north_df %>%
  select(-empty) %>%
  mutate(remove = ifelse(is.na(stock_flow) & is.na(category) & is.na(sub_cat) & is.na(cat_total), 1, 0)) %>%
  filter(remove != 1) %>%
  fill(stock_flow, .direction = "down") %>%
  fill(category, .direction = "down") %>%
  filter(!is.na(`X2014-01-03`),
         is.na(cat_total)) %>%
  select(-cat_total, -remove) %>%
  pivot_longer(`X2014-01-03`:`X2020-04-17`, names_to = "date", values_to = "thous_barrels") %>%
  mutate(date = as.Date(str_remove(date, pattern = START %R% "X")),
         year = year(date),
         region = "North", 
         source = "EIA800 Data, PIIRA Database") 
  

## south
fw_south_df <- read_xlsx(paste0(data_directory, "raw/WeeklyFuelsWatch_Summary_2014-2020_North_South.xlsx"), sheet = 4, skip = 3)

date_colnames_s <- colnames(fw_south_df)
date_colnames_s2 <- convertToDate(date_colnames_s[6:length(date_colnames_s)])
date_colnames_s3 <- paste0("X", date_colnames_s2)

fw_south_df_cols <- c("stock_flow", "category", "sub_cat", "cat_total", "empty", date_colnames_s3)
colnames(fw_south_df) <- fw_south_df_cols

fw_south_df2 <- fw_south_df %>%
  select(-empty) %>%
  mutate(remove = ifelse(is.na(stock_flow) & is.na(category) & is.na(sub_cat) & is.na(cat_total), 1, 0)) %>%
  filter(remove != 1) %>%
  fill(stock_flow, .direction = "down") %>%
  fill(category, .direction = "down") %>%
  filter(!is.na(`X2014-01-03`),
         is.na(cat_total)) %>%
  select(-cat_total, -remove) %>%
  pivot_longer(`X2014-01-03`:`X2020-04-17`, names_to = "date", values_to = "thous_barrels") %>%
  mutate(date = as.Date(str_remove(date, pattern = START %R% "X")),
         year = year(date),
         region = "South", 
         source = "EIA800 Data, PIIRA Database") 

fw_all_df <- rbind(fw_north_df2, fw_south_df2) %>%
  select(stock_flow, category, sub_cat, region, date, year, thous_barrels, source)


## save clean file
write_csv(fw_all_df, path = paste0(data_directory, "processed/fuel_watch_data.csv"))




## total crude supply to refineries
## ---------------------------------------------------------------------------------------
oil_supply <- read_csv(paste0(data_directory, "raw/oil_supply_sources_a.csv"))

oil_supply2 <- oil_supply %>%
  select(year = Year, ca, ak, foreign, total, notes) %>%
  pivot_longer(ca:foreign, names_to = "source", values_to = "thous_barrels") %>%
  select(year, source, thous_barrels, total, notes) %>%
  mutate(source_type = ifelse(source == "ca", "domestic", "imported")) %>%
  group_by(year, source_type) %>%
  mutate(sum_source_grp = sum(thous_barrels)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_prod = sum(thous_barrels)) %>% 
  ungroup() %>%
  mutate(diff = total_prod - total,
         perc_prod = thous_barrels / total_prod,
         perc_grp = sum_source_grp / total_prod) %>%
  select(year, source, source_type, thous_barrels,perc_prod, sum_source_grp, perc_grp, total_prod, total_orig = total)

## save clean file
write_csv(oil_supply2, path = paste0(data_directory, "processed/domestic_import_crude.csv"))


## fuel prices, CA
## ---------------------------------------------------------------------------------------
ca_retail_p <- read_xls(paste0(data_directory, "raw/PET_PRI_GND_DCUS_SCA_W.xls"), sheet = 2, skip = 2)

ca_retail_p2 <- ca_retail_p %>%
  pivot_longer('Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)':'Weekly California No 2 Diesel Ultra Low Sulfur (0-15 ppm) Retail Prices  (Dollars per Gallon)',
               names_to = "type", values_to = "dollars_per_gallon") %>%
  mutate(type = str_remove(type, pattern = "Weekly California"),
          type = str_remove(type, pattern = ANY_CHAR %R% "Dollars per Gallon" %R% ANY_CHAR),
          type = str_trim(type, side = "both")) %>%
  rename(date = Date) %>%
  mutate(source = "EIA",
         url = "https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_sca_w.htm")

## save clean file
write_csv(ca_retail_p2, path = paste0(data_directory, "processed/ca_gas_d_prices.csv"))

## fuel prices, USA
## ---------------------------------------------------------------------------------------
usa_retail_p <- read_xls(paste0(data_directory, "raw/PET_PRI_GND_DCUS_NUS_W.xls"), sheet = 2, skip = 2)

usa_retail_p2 <- usa_retail_p %>%
  pivot_longer('Weekly U.S. All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)':'Weekly U.S. No 2 Diesel Low Sulfur (15-500 ppm) Retail Prices  (Dollars per Gallon)',
               names_to = "type", values_to = "dollars_per_gallon") %>%
  mutate(type = str_remove(type, pattern = "Weekly U.S."),
         type = str_remove(type, pattern = ANY_CHAR %R% "Dollars per Gallon" %R% ANY_CHAR),
         type = str_trim(type, side = "both")) %>%
  rename(date = Date) %>%
  mutate(source = "EIA",
         url = "https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_w.htm")

## save clean file
write_csv(usa_retail_p2, path = paste0(data_directory, "processed/usa_retail_p2.csv"))

## oil and gas emissions over time
## -----------------------------------------------------------------------------------------
og_emissions <- read_csv(paste0(data_directory, "raw/ghg_sector_data_og.csv"), skip= 9)

og_e2 <- clean_names(og_emissions) %>%
  pivot_longer(x2000:x2017, names_to = "year", values_to = "Mt_CO2eq") %>%
  mutate(year = as.numeric(str_remove(year, pattern = "x")),
         source = "CARB GHG Emissions Inventory")

write_csv(og_e2, path = paste0(data_directory, "processed/oil_gas_emissions.csv"))
         
## oil and gas emissions over time
## -----------------------------------------------------------------------------------------
r_emissions <- read_csv(paste0(data_directory, "raw/ghg_sector_data_refining2.csv"), skip= 9)

r2 <- clean_names(r_emissions) %>%
  pivot_longer(x2000:x2017, names_to = "year", values_to = "Mt_CO2eq") %>%
  mutate(year = as.numeric(str_remove(year, pattern = "x")),
         source = "CARB GHG Emissions Inventory")

write_csv(r2, path = paste0(data_directory, "processed/refining_emissions.csv"))         
         

## oil produciton by well
## ------------------------------------------------------------------------------------------

## Monthly Year-to-Date Well Production Injection Database.pdf
## API -- The well API. This number is used to uniquely identify each well and wellbore (state, couty, well, wellbore)
## FieldCode -- the field to which the completion interval is associated.
## AreaCode -- The area to which the completion interval is associated.
## PoolCode -- The code for the pool
## WellTypeCode -- The code for the Completion type.
## OilorCondensateProduced

ccodes <- read_csv(paste0(data_directory, "raw/prod/county_codes.csv")) %>%
  rename(county_name = county,
         county = number) %>%
  select(county_name, county)

## well type code

welltype_df <- tibble(WellTypeCode = c("AI", "DG", "GD", "GS", 
                                       "LG", "LG",  "MW", "NW",
                                       "OB", "OG", "PM", "SC",
                                       "SF", "WD", "WF", "WS"),
                      well_type_name = c("Air Injector", "Dry Gas Production", "Gas Disposal Injector", "Gas Storage Injector/Producer",
                                         "Liquid Petroleum Gas Injector/Producer", "Liquid Gas", "Ground Monitoring", "New",
                                         "Observation Well", "Oil & Gas Production", "Pressure Maintenance Injector", "Cyclic Steam",
                                         "Steam Flood Injector", "Water Disposal Injector", "Water Flood Injector", "Water Source Injector"))


prod2018 <- read_csv(paste0(data_directory, "raw/prod/2018CaliforniaOilAndGasWellMonthlyProduction.csv")) 

prod2019 <- read_csv(paste0(data_directory, "raw/prod/2019CaliforniaOilAndGasWellMonthlyProduction.csv")) 

inject2018 <- read_csv(paste0(data_directory, "raw/prod/2018CaliforniaOilAndGasWellMonthlyInjection.csv")) %>%
  rename(inject_r_or_e = ReportedOrEstimated)

inject2019 <- read_csv(paste0(data_directory, "raw/prod/2019CaliforniaOilAndGasWellMonthlyInjection.csv")) %>%
  rename(inject_r_or_e = ReportedOrEstimated)

## production data
all_prod <- rbind(prod2018, prod2019) %>%
  mutate(county = as.numeric(str_sub(APINumber, 3, 5)),
         year = year(ProductionReportDate),
         month = month(ProductionReportDate)) %>%
  left_join(ccodes) %>%
  left_join(welltype_df) %>%
  mutate(well_type_name = ifelse(is.na(well_type_name), WellTypeCode, well_type_name))

write_csv(all_prod, path = paste0(data_directory, "processed/all_prod_test.csv"))  

## injection data
all_inject <- rbind(inject2018, inject2019) %>%
  mutate(county = as.numeric(str_sub(APINumber, 3, 5)),
         year = year(InjectionDate),
         month = month(InjectionDate)) %>%
  left_join(ccodes) %>%
  left_join(welltype_df) %>%
  mutate(well_type_name = ifelse(is.na(well_type_name), WellTypeCode, well_type_name))

write_csv(all_inject, path = paste0(data_directory, "processed/all_inject_test.csv"))  

## 2018 and 2018 well info
## ------------------------------------

wells2018 <- read_csv(paste0(data_directory, "raw/prod/2018CaliforniaOilAndGasWells.csv")) 

wells2019 <- read_csv(paste0(data_directory, "raw/prod/2019CaliforniaOilAndGasWells.csv")) 

## MRR GHG emissions
## -----------------------------------

## 2008
ghg2008 <- read_xlsx(paste0(data_directory, "raw/2008-ghg-emissions-facility-2012-03-12.xlsx"), sheet = 3, skip = 11) %>%
  select(-'...13') %>%
  mutate(report_yr = 2008) %>%
  select('Facility ID #', 'Facility Name', report_yr, 'CO2e Total \r\n(non-biomass + biomass)':'NAICS Code')

mrr_colnames_2008 <- c("ARB_ID", "facility_name", "report_yr", "total_co2e", "co2e_nbio", "co2e_bio",
                       "report_sub_status", "facil_city", "facil_state", "facil_zip",
                       "prim_report_sect", "sec_report_sect", "NAICS")

colnames(ghg2008) <- mrr_colnames_2008

## 2009
ghg2009 <- read_xlsx(paste0(data_directory, "raw/2009-ghg-emissions-facility-2012-03-12.xlsx"), sheet = 3, skip = 11) %>%
  select(-'...15') %>%
  mutate(report_yr = 2009) %>%
  select('Facility ID #', 'Facility Name', report_yr, 'CO2e Total \r\n(non-biomass + biomass)':'NAICS Code')

mrr_colnames_2009 <- c("ARB_ID", "facility_name", "report_yr", "total_co2e", "co2e_nbio", "co2e_bio",
                       "verif_overall_report", "free_mm", "free_nonconf", "facil_city", "facil_state", "facil_zip",
                       "prim_report_sect", "sec_report_sect", "NAICS")

colnames(ghg2009) <- mrr_colnames_2009

## 2010
ghg2010 <- read_xlsx(paste0(data_directory, "raw/2010-ghg-emissions-2015-06-15.xlsx"), sheet = 3, skip = 14) %>%
  select(-'...15') %>%
  mutate(report_yr = 2010) %>%
  select('Facility ID #', 'Facility Name', report_yr, 'CO2e Total \r\n(non-biomass + biomass)':'NAICS Code')

mrr_colnames_2010 <- c("ARB_ID", "facility_name", "report_yr", "total_co2e", "co2e_nbio", "co2e_bio",
                       "verif_overall_report", "free_mm", "free_nonconf", "facil_city", "facil_state", "facil_zip",
                       "prim_report_sect", "sec_report_sect", "NAICS")

colnames(ghg2010) <- mrr_colnames_2010

## 2011
ghg2011 <- read_xlsx(paste0(data_directory, "raw/2011-ghg-emissions-2018-11-05.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

mrr_colnames <- c("ARB_ID", "facility_name", "report_yr", "total_co2e", "AEL",
                  "co2e_nb_ch4_n2o", "co2_bf", "fs_co2e", "fs_co2_bf", "elec_imp_co2e",
                  "emitter_covered_emis", "fs_covered_emis", "elec_imp_covered_emis", "total_covered_emis",
                  "total_nocovered_emis", "emissions_data", "product_data", "verif_body", "facil_city", "facil_state", "facil_zip",
                  "NAICS", "usepa_arb_subparts", "industry_sector")
  
colnames(ghg2011) <- mrr_colnames

## 2012
ghg2012 <- read_xlsx(paste0(data_directory, "raw/2012-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

colnames(ghg2012) <- mrr_colnames

## 2013
ghg2013 <- read_xlsx(paste0(data_directory, "raw/2013-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

colnames(ghg2013) <- mrr_colnames

## 2014
ghg2014 <- read_xlsx(paste0(data_directory, "raw/2014-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

colnames(ghg2014) <- mrr_colnames

## 2015
ghg2015 <- read_xlsx(paste0(data_directory, "raw/2015-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

colnames(ghg2015) <- mrr_colnames

## 2016
ghg2016 <- read_xlsx(paste0(data_directory, "raw/2016-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23')

colnames(ghg2016) <- mrr_colnames

## 2017
ghg2017 <- read_xlsx(paste0(data_directory, "raw/2017-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

colnames(ghg2017) <- mrr_colnames

## 2018
ghg2018 <- read_xlsx(paste0(data_directory, "raw/2018-ghg-emissions-2019-11-04.xlsx"), sheet = 3, skip = 8) %>%
  select(-'...4', -'...7', -'...13', -'...19', -'...23', -'...30')

colnames(ghg2018) <- mrr_colnames

## combine those that match up...
## 2008; 2009 & 2010; 2011:2018

## 2011:2018
ghg_2011_2018 <- rbind(ghg2011, ghg2012, ghg2013, ghg2014, ghg2015, ghg2016, ghg2017, ghg2018) %>%
  mutate(NAICS_code = str_extract(NAICS, pattern = START %R% one_or_more(DIGIT)))

ghg_2009_2010 <- rbind(ghg2009, ghg2010) %>%
  mutate(NAICS_code = str_extract(NAICS, pattern = START %R% one_or_more(DIGIT)))

## 2008
ghg_2008 <- ghg2008 %>%
  mutate(NAICS_code = str_extract(NAICS, pattern = START %R% one_or_more(DIGIT)))

write_csv(ghg_2011_2018, path = paste0(data_directory, "processed/ghg_mrr/ghg_mrr_2011-2018.csv"))   
write_csv(ghg_2009_2010, path = paste0(data_directory, "processed/ghg_mrr/ghg_mrr_2009-2010.csv"))
write_csv(ghg_2008, path = paste0(data_directory, "processed/ghg_mrr/ghg_mrr_2008.csv"))

## ghg inventory
## ----------------------------------------------
ghg_inv_oil <- read_csv(paste0(data_directory, "raw/ghg_sector_data_og.csv"), skip = 9)

ghg_inv_oil <- clean_names(ghg_inv_oil) %>%
  pivot_longer(x2000:x2017, names_to = "year", values_to = "co2e") %>%
  mutate(year = as.numeric(str_remove(year, pattern = "x")))

ghg_inv_refin <- read_csv(paste0(data_directory, "raw/ghg_sector_data_refining2.csv"), skip = 9)
ghg_inv_refin <- clean_names(ghg_inv_refin) %>%
  pivot_longer(x2000:x2017, names_to = "year", values_to = "co2e") %>%
  mutate(year = as.numeric(str_remove(year, pattern = "x")))

inventory_df <- rbind(ghg_inv_oil, ghg_inv_refin)

write_csv(inventory_df, path = paste0(data_directory, "processed/ghg_inventory_tool.csv"))

## ghg from EPA
## ------------------------------------------------
epa_ghg <- read_csv(paste0(data_directory, "raw/epa_emissions.csv"))

epa_ghg2 <- clean_names(epa_ghg)
colnames1 <- colnames(epa_ghg2)
colnames2 <- str_remove_all(colnames1, pattern = "v_ghg_emitter_facilities_")

colnames(epa_ghg2) <- colnames2
codevec <- c(211111, 324110, 486910, 422710)

epa_ghg_filt <- epa_ghg2 %>%
  filter(primary_naics_code %in% codevec) 

write_csv(epa_ghg_filt, path = paste0(data_directory, "processed/ghg_emissions_epa.csv"))

## clean permit data
## ------------------------------------------------

## wims 1990s-2018
hist_permit_wims <- read_xlsx(paste0(data_directory, "raw/Historic Permit CalWIMS.xlsx"))

date_recieved <- hist_permit_wims$DateReceived
date_recieved2 <- convertToDate(date_recieved)

preport_issue_date <- hist_permit_wims$PReportIssueDate
preport_issue_date2 <- convertToDate(preport_issue_date)

status_notice_change_date <- hist_permit_wims$StatusOfNoticeChangeDate
status_notice_change_date2 <- convertToDate(status_notice_change_date)

completion_date <- hist_permit_wims$CompletionDate
completion_date2 <- convertToDate(completion_date)

final_letter_date <- hist_permit_wims$FinalLetterDate
final_letter_date2 <- convertToDate(final_letter_date)

hist_permit_wims2 <- cbind(hist_permit_wims, date_recieved2, preport_issue_date2, status_notice_change_date2, completion_date2, final_letter_date2) %>%
  select(-DateReceived, -PReportIssueDate, -StatusOfNoticeChangeDate, -CompletionDate, -FinalLetterDate)

hist_permit_wims2 <- clean_names(hist_permit_wims2)

write_csv(hist_permit_wims2, path = paste0(data_directory, "processed/historic_permit_calwims.csv"))


## welstar 2018 - present
hist_permit_wellstar <- read_xlsx(paste0(data_directory, "raw/Historic Permit WellStar.xlsx"))

hist_permit_wellstar <- clean_names(hist_permit_wellstar) %>%
  mutate(year_received = year(notice_received_date),
         year_approved = year(notice_approval_date))

# write_csv(hist_permit_wellstar, path = paste0(data_directory, "processed/historic_permit_wellstar.csv"))

## ca crude production
## -----------------------------------

## read raw
ca_crude <- read_xls(paste0(data_directory, "raw/MCRFPCA1a.xls"), sheet = 2, skip = 2) %>%
  rename(date = Date, crude_extract_thous_bbl = `California Field Production of Crude Oil (Thousand Barrels)`) %>%
  mutate(crude_extract_bbl = crude_extract_thous_bbl * 1000,
         sourcekey = "MCRFPCA1",
         source = "EIA")

write_csv(ca_crude, path = paste0(data_directory, "processed/eia_ca_crude_prod.csv"))

## crude production from CEC, thousand barrels
## --------------------------------------

## north
ncrude_cec <- read.xlsx(paste0(data_directory, "raw/Crude Oil Receipts 1981-2020.xlsx"), sheet = 2) 

ncrude_cec2 <- clean_names(ncrude_cec) %>%
  pivot_longer(alaska:foreign, names_to = "origin", values_to = "bbls") %>%
  mutate(bbls = bbls * 1000) %>%
  mutate(region = "north",
         data_source = "EIA810 Monthly Data, PIIRA Database",
         source = "California Energy Commission") %>%
  arrange(year, month)

## south
scrude_cec <- read.xlsx(paste0(data_directory, "raw/Crude Oil Receipts 1981-2020.xlsx"), sheet = 3)

scrude_cec2 <- clean_names(scrude_cec) %>%
  pivot_longer(alaska:foreign, names_to = "origin", values_to = "bbls") %>%
  mutate(bbls = bbls * 1000) %>%
  mutate(region = "south",
         data_source = "EIA810 Monthly Data, PIIRA Database",
         source = "California Energy Commission") %>%
  arrange(year, month)

reg_refin_crude_receipts <- rbind(ncrude_cec2, scrude_cec2)

write_csv(reg_refin_crude_receipts, path = paste0(data_directory, "processed/reg_refin_crude_receipts.csv"))


## crude production by state
## ---------------------------------------------------

usa_crude <- read_excel(paste0(data_directory, "raw/PET_CRD_CRPDN_ADC_MBBL_A.xls"), sheet = 2, skip = 3)

colnames_usa <- c("date", "usa", "PADD1", "FL", "NY", "PA", "VA", "WV", 
                  "PADD2", "IL", "IN", "KS", "KY", "MI", "MO", "NE", "ND", "OH", "OK", "SD", "TN",
                  "PADD3", "AL", "AR", "LA", "MS", "NM", "TX", "fed_offshore_GOM",
                  "PADD4", "CO", "ID", "MT", "UT", "WY",
                  "PADD5", "AK", "AK_sf", "AK_ns", "AZ", "CA", "NV", "fed_offshore_padd5")

colnames(usa_crude) <- colnames_usa 

usa_crude2 <- usa_crude %>%
  mutate(year = year(date)) %>%
  select(date, year, usa:fed_offshore_padd5) %>%
  pivot_longer(usa:fed_offshore_padd5,
               names_to = "location",
               values_to = "bbls_thousand") %>%
  mutate(bbls = bbls_thousand * 1000) %>%
  select(-bbls_thousand) %>%
  mutate(source = "EIA",
         website = "https://www.eia.gov/dnav/pet/pet_crd_crpdn_adc_mbbl_a.htm")

write_csv(usa_crude2, path = paste0(data_directory, "processed/usa_crude_prod.csv"))


## imports to refineries
## ---------------------------------------------------------

imports_refin <- read_csv(paste0(data_directory, "raw/Imports_of_Light_Sweet_to_California.csv"), skip = 4) %>%
  rename(region = "<span style='float:right;'>(thousand barrels)</span>") %>%
  filter(region != "California") %>%
  mutate(grade = ifelse(region %in% c("Heavy Sour", "Heavy Sweet", "Light Sour", "Light Sweet", "Medium"), region, NA),
         region = ifelse(!region %in% c("Heavy Sour", "Heavy Sweet", "Light Sour", "Light Sweet", "Medium"), region, NA)) %>%
  select(region, grade, `Jan 2009`:`Nov 2019`) %>%
  fill(region) %>%
  filter(!is.na(grade)) %>%
  pivot_longer(`Jan 2009`:`Nov 2019`, names_to = "date", values_to = "bbls") %>%
  mutate(bbls = as.numeric(bbls) * 1000,
         month = substr(date, 1, 3),
         month = match(month, month.abb),
         year = str_extract(date, pattern = one_or_more(DIGIT)),
         month_year = paste(year, month, sep = "-"),
         month_year =  as.Date(as.yearmon(month_year))) %>%
  select(region, month_year, month, year, grade, bbls) %>%
  mutate(source = "EIA",
         website = "https://www.eia.gov/petroleum/imports/browser/#/?d=0g000000000&dt=RS&e=201911&f=m&gg=i&od=d&s=200901&vs=PET_IMPORTS.WORLD-RS_CA-LSW.M")


write_csv(imports_refin, path = paste0(data_directory, "processed/imports_to_refineries.csv"))

## CEC CA consumption data (from Gordon)
## -------------------------------------------------------

ca_fuel_consp <- read_xlsx(paste0(data_directory, "raw/California Transportion Fuel Consumption - Summary 2020-06-01 GDS.xlsx"), sheet = 7, range = "A8:R25")

cnames <- c("year","Finished_gasoline", "CARBOB", "E85", "Fuel_ethanol", "X", 
            "Finished_diesel", "Diesel_fuel_less_biodiesel_and_RDiesel", "Dyed_diesel", "Finished_diesel_less_dyed", "X2", 
            "Biodiesel", "Renewable_diesel", "X3",
            "Commercial_jet_fuel", "Military_jet_fuel", "X4",
            "Aviation_gasoline") 

colnames(ca_fuel_consp) <- cnames 


ca_fuel_consp2 <- ca_fuel_consp %>%
  select(-X, -X2, -X3, -X4) %>%
  pivot_longer(Finished_gasoline:Aviation_gasoline, names_to = "product", values_to = "gallons") %>%
  mutate(metric = "gallons",
         product = str_replace_all(product, "_", " "),
         source = "CEC",
         file_name = "California Transportion Fuel Consumption - Summary 2020-06-01 GDS.xlsx")

write_csv(ca_fuel_consp2, path = paste0(data_directory, "processed/ca_fuel_consumption_cec.csv"))

## CI data
## -------------------------------------------------------------

## first 2010

## for pulling out gammas
pdf2010 <- '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/ci/050515staffreport_opgee.pdf'

ci <- file.path(pdf2010)

pdf_txt <- tabulizer::extract_text(pdf2010)
tbls <- tabulizer::extract_tables(ci, pages = 26:32, output = "data.frame")

## clean up the table
ci1 <- tbls[[1]]
ci2 <- tbls[[2]]
ci3 <- tbls[[3]]
ci4 <- tbls[[4]]
ci5 <- tbls[[5]]
ci6 <- tbls[[6]]
ci7 <- tbls[[7]]

## clean up the table
ci1 <- tbls[[1]] %>% 
  select(-X) 

cnames <- c("region", "crude_name", "ci_g_MJ", "volume_bbl")

colnames(ci1) <- cnames

ci1 <- ci1 %>%
  mutate(region = ifelse(crude_name == "2010 Baseline Crude Average CI", "WA", region))

## table 2
ci2_row1 <- colnames(ci2)

colnames(ci2) <- cnames

ci2_row1_df <- tibble(region =  "",
                      crude_name = "Suncor Synthetic",
                      ci_g_MJ = "23.78",
                      volume_bbl = "2,733,903")

ci2_adj <- rbind(ci2_row1_df, ci2) %>%
  fill(region)

## table 3
ci3_row1 <- colnames(ci3)

colnames(ci3) <- cnames

ci3_row1_df <- tibble(region =  "",
                      crude_name = "Belridge, North",
                      ci_g_MJ = "4.70",
                      volume_bbl = "2,931.540")

ci3_adj <- rbind(ci3_row1_df, ci3)

## table 4
ci4_row1 <- colnames(ci4)

colnames(ci4) <- cnames

ci4_row1_df <- tibble(region =  "",
                      crude_name = "Honor Rancho",
                      ci_g_MJ = "3.51",
                      volume_bbl = "53,687")

ci4_adj <- rbind(ci4_row1_df, ci4)

## table 5
ci5_row1 <- colnames(ci5)

colnames(ci5) <- cnames

ci5_row1_df <- tibble(region =  "",
                      crude_name = "Ojai",
                      ci_g_MJ = "2.78",
                      volume_bbl = "262,361")

ci5_adj <- rbind(ci5_row1_df, ci5)

## table 6
ci6_row1 <- colnames(ci6)

colnames(ci6) <- cnames

ci6_row1_df <- tibble(region =  "",
                      crude_name = "Sargent",
                      ci_g_MJ = "4.96",
                      volume_bbl = "22,844")

ci6_adj <- rbind(ci6_row1_df, ci6)

## table 6
ci7_row1 <- colnames(ci7)

colnames(ci7) <- cnames

ci7_row1_df <- tibble(region =  "",
                      crude_name = "Point Arguello",
                      ci_g_MJ = "14.59",
                      volume_bbl = "1,969,836")

ci7_adj <- rbind(ci7_row1_df, ci7)

## 
ci_2010_df <- rbind(ci1, ci2_adj, ci3_adj, ci4_adj, ci5_adj, ci6_adj, ci7_adj) %>%
  mutate(region = ifelse(region == "", NA, region)) %>%
  fill(region) %>%
  mutate(year = 2010,
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/peerreview/050515staffreport_opgee.pdf")


## bind all 2012
## -----------------------

ci_2012 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2012.csv")


ci_2012_df <- ci_2012 %>%
  mutate(year = 2012) %>%
  fill(region) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2014_crude_average_ci_value_final.pdf")

## repeat -- 2013
## -----------------------
ci_2013 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2013.csv")


ci_2013_df <- ci_2013 %>%
  mutate(year = 2013) %>%
  fill(region) %>%
  rename(volume_bbl = volume_bbls) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2014_crude_average_ci_value_final.pdf")

## repeat -- 2014
## -----------------------
ci_2014 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2014.csv")


ci_2014_df <- ci_2014 %>%
  mutate(year = 2014) %>%
  fill(region) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2014_crude_average_ci_value_final.pdf")


## repeat -- 2015
## -----------------------
ci_2015 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2015.csv")


ci_2015_df <- ci_2015 %>%
  mutate(year = 2015) %>%
  fill(region) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2015_crude_average_ci_value_final.pdf")

## repeat -- 2016
## -----------------------
ci_2016 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2016.csv")


ci_2016_df <- ci_2016 %>%
  mutate(year = 2016) %>%
  fill(region) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2016_crude_average_ci_value_final.pdf") 



## repeat -- 2017
## -----------------------
ci_2017 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2017.csv")


ci_2017_df <- ci_2017 %>%
  mutate(year = 2017) %>%
  fill(region) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2017_crude_average_ci_value_final.pdf") 


## repeat -- 2018
## -----------------------
ci_2018 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/ci-values/ci_2018.csv")


ci_2018_df <- ci_2018 %>%
  mutate(year = 2018) %>%
  select(-X5) %>%
  fill(region) %>%
  mutate(ci_g_MJ = as.numeric(ci_g_MJ),
         volume_bbl = as.numeric(str_remove_all(volume_bbl, pattern = ",")),
         source = "CARB",
         url = "https://ww3.arb.ca.gov/fuels/lcfs/crude-oil/2018_crude_average_ci_value_final.pdf") 



ci_all_df <- rbind(ci_2010_df, ci_2012_df, ci_2013_df, ci_2014_df, ci_2015_df, ci_2016_df, ci_2017_df, ci_2018_df)


## save clean file
write_csv(ci_all_df, path = paste0(data_directory, "processed/ci_streams_all_yrs.csv"))


## create a look up table for fields
## ------------------------------------------------------------

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10))

fieldcodes <- janitor::clean_names(wells_19) %>%
  select(field_code, field_name, county) %>%
  unique()

## save clean file
write_csv(fieldcodes, path = paste0(data_directory, "processed/field_codes.csv"))


## Brent proojections
## ------------------------------------------------------------

brent_projections <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/Total_Energy_Nominal_Prices_Brent.csv", skip = 4)

brent_cols <- c("year", "reference_case_nom_usd_per_bbl", "high_nom_usd_per_bbl", "low_nom_usd_per_bbl", "AEO2019_reference_nom_usd_per_bbl")

colnames(brent_projections) <- brent_cols

brent_p2 <- brent_projections %>%
  pivot_longer(reference_case_nom_usd_per_bbl:AEO2019_reference_nom_usd_per_bbl, names_to = "scenario", values_to = "nom_usd_per_bbl") %>%
  mutate(scenario = str_remove(scenario, pattern = "_nom_usd_per_bbl")) %>%
  mutate(scenario = ifelse(scenario == "high", "high_oil_price", 
                           ifelse(scenario == "low", "low_oil_price", scenario))) %>%
  arrange(year)

b_info <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/Total_Energy_Nominal_Prices_Brent.csv")

bweb <- as.character(b_info[1,1])

brent_p3 <- brent_p2 %>%
  mutate(url = bweb,
         source = "EIA")

## save clean file
write_csv(brent_p3, path = paste0(data_directory, "processed/brent_oil_price_projections.csv"))

## updated sector ghg emissions
## -------------------------------------------

indust_ghg <- read_xlsx(paste0(data_directory, "raw/2000_2019_ghg_inventory_trends_figures.xlsx"), sheet = "Figure 13", skip = 4)

source <- as.character(indust_ghg[9, 1])

indust_ghg <- indust_ghg[1:7, ]

indust_ghg2 <- indust_ghg %>%
  pivot_longer(`2000`:`2019`, names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  rename(segment = Parameter,
         unit = Unit) %>%
  mutate(source = source)

## save clean file
write_csv(indust_ghg2, file = paste0(data_directory, "processed/indust_emissions_2000-2019.csv"))


