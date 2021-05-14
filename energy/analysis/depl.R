## Ruiwen Lee
## May 13, 2021
## Create depl variable for entry df using data from DOC's 2009 report
## (2009 Annual Report of the State Oil & Gas Supervisor)

library(tidyverse)

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/entry-model-results/"

## read in the data
doc_df <- read_csv(paste0(data_directory, "DOC_production_reserves_2009.csv"))

## Process data
# Rename columns
doc_df <- rename(doc_df, cum_prod=`Cumulative_oil_and_condensate(Mbbl)`)
doc_df <- rename(doc_df, reserves=`Estimated_oil_reserves(Mbbl)`)
# Drop county subtotals
doc_df <- doc_df[1:500,]
# Drop if doc_field_code is NA (fields that don't exist in main dataset)
doc_df <- doc_df %>% drop_na(doc_field_code)
# Replace NA values with zero for cumulative production and estimated remaining reserves 
doc_df <- doc_df %>% replace_na(list(cum_prod=0,reserves=0))
# Create new column that is the sum of cumulative production and estimated reserves,
# and convert from Mbbl to bbl
doc_df <- doc_df %>%
  mutate(
    resource = 1000*(cum_prod + reserves)
  )

# Keep wanted columns
doc_df <- doc_df %>%
  select(doc_field_code, resource)

## Save
write_csv(doc_df, path = paste0(save_directory, "field_resource_doc.csv"))

  
