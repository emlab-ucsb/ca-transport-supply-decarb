## Tracey Mangin
## October 19, 2021
## Census data

library(censusapi)
library(tidycensus)
library(tidyverse)
library(data.table)

main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'

mycensuskey <- "ae05491f7dfe185b0af5b9d56f1287b4c2c78eca"

# apis <- listCensusApis()
# View(apis)
# 
# availablevars <- listCensusMetadata(name="cps/asec/mar", vintage = 2021)
# View(availablevars)
# 
# 
# ## B19013_001E = Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
# ## B19013_001EA = Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
# ## B19013_001M = Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
# ## B19013_001MA	 = Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
# 
# acs_income <- getCensus(
#   name = "acs/acs5",
#   key = mycensuskey,
#   vintage = 2019, 
#   vars = c("NAME", "B19013_001E", "B19013_001EA", "B19013_001M", "B19013_001MA"), 
#   region = "tract:*", 
#   regionin = "state:06")
# head(acs_income)

## tidycensus
options(tigris_use_cache = TRUE)

census_api_key(mycensuskey)


income <- get_acs(state = "CA", geography = "tract", 
                  variables = "B19013_001", geometry = FALSE)

income <- income %>%
  mutate(source = "2015-2019 5-year ACS, 2019 dollars")

fwrite(income, paste0(main_path, "data/Census/ca-median-house-income.csv"))

## repeat for county

county_income <- get_acs(state = "CA", geography = "county", 
                  variables = "B19013_001", geometry = FALSE)

county_income <- county_income %>%
  mutate(county = str_remove(NAME, " County, California"),
         source = "2015-2019 5-year ACS, 2019 dollars") %>%
  select(county, variable, estimate, moe, source)

fwrite(county_income, paste0(main_path, "data/Census/ca-median-house-income-county.csv"))

