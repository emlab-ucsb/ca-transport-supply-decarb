## Ruiwen Lee
## May 13, 2021
## Use data from DOC's 2009 report (2009 Annual Report of the State Oil & Gas Supervisor) to:
## 1. Create field-level resources variable for predicting production in future period
## 2. Create depl variable for entry df 

library(tidyverse)

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/"
outputs_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/entry-model-results/"

## read in the data
reserves_df <- read_csv(paste0(data_directory, "raw/DOC_production_reserves_2009.csv"))
prod_annual_df <- read_csv(paste0(outputs_directory, "crude_prod_x_field_revised.csv"))

## Process reserves data
# Rename columns
reserves_df <- rename(reserves_df, cum_prod_2009=`Cumulative_oil_and_condensate(Mbbl)`)
reserves_df <- rename(reserves_df, reserves_2009=`Estimated_oil_reserves(Mbbl)`)
# Drop if doc_field_code is NA (fields that don't exist in main dataset)
reserves_df <- reserves_df %>% drop_na(doc_field_code)
# Replace NA values with zero for cumulative production and estimated remaining reserves 
reserves_df <- reserves_df %>% replace_na(list(cum_prod_2009=0,reserves_2009=0))
# Create new column that is the sum of cumulative production and estimated reserves,
# and convert from Mbbl to bbl
reserves_df <- reserves_df %>%
  mutate(
    resource = 1000*(cum_prod_2009 + reserves_2009)
  )

# Keep wanted columns
field_resource_df <- reserves_df %>%
  select(doc_field_code, resource)

## Save field-level resources data
write_csv(field_resource_df, path = paste0(save_directory, "field_resource_doc.csv"))
rm(field_resource_df)

## Calculate annual cumulative production based on DOC's 2009 data
# Keep wanted columns 
reserves_df <- reserves_df %>%
  select(doc_field_code, resource, cum_prod_2009)
# Convert from Mbbl to bbl
reserves_df <- reserves_df %>%
  mutate(
    cum_prod_2009 = 1000*cum_prod_2009
  )
# Merge reserves data with annual production data
merged_df <- prod_annual_df %>% left_join(reserves_df, by = "doc_field_code")
# Check which fields don't have resource or cum_prod_2009 data:
# 000 Any field, 154 Coal Oil Point Offshore (ABD), 848 Old Wilmington (ABD)
na_df <- merged_df[rowSums(is.na(merged_df)) > 0,]
rm(na_df)

# Split dataset into pre-, post-, and 2009 itself
pre2009_df <- merged_df %>% filter(year<=2009)
post2009_df <- merged_df %>% filter(year>2009)

# Calculate historical cumulative prod for pre- and post-2009 subsets
pre2009_df <- pre2009_df %>%
  group_by(doc_field_code) %>%
  arrange(desc(year)) %>%
  mutate(
    cum_prod_from_2009 = cumsum(total_bbls)
  )
pre2009_df <- pre2009_df %>%
  mutate(
    cum_prod = cum_prod_2009 - cum_prod_from_2009 + total_bbls
  )  
pre2009_df <- pre2009_df %>%
  select(-cum_prod_from_2009)

post2009_df <- post2009_df %>%
  group_by(doc_field_code) %>%
  arrange(year) %>%
  mutate(
    cum_prod_from_2010 = cumsum(total_bbls)
  )
post2009_df <- post2009_df %>%
  mutate(
    cum_prod = cum_prod_2009 + cum_prod_from_2010
  )  
post2009_df <- post2009_df %>%
  select(-cum_prod_from_2010)

# Merge back
final_df <- bind_rows(pre2009_df, post2009_df)

# Calculate depletion rate, set to 1 if resource is 0
final_df <- final_df %>% 
  mutate(
    depl = cum_prod/resource
  )  

final_df <- final_df %>% 
  mutate(
    depl = ifelse(resource==0,1,depl)
  )  

final_df %>%
  ungroup() %>%
  summarise(mean = mean(depl, na.rm=TRUE), 
            min = min(depl, na.rm=TRUE), 
            max = max(depl, na.rm=TRUE))

# Adjust cum prod so that depletion rate is non-negative
# Find minimum of cum prod
final_df <- final_df %>% 
  group_by(doc_field_code) %>%
  mutate(
    cum_prod_min = min(cum_prod)
  )  
# If minimum is negative, add its magnitude to each year
# Same as cum sum from 1977
final_df <- final_df %>% 
  group_by(doc_field_code) %>%
  mutate(
    cum_prod_adj = cum_prod + ifelse(cum_prod_min<0,-cum_prod_min,0)
  )  

# Adjust resource so that depletion rate is not greater than 1
# But some fields that are still producing already have depletion rate of 1
final_df <- final_df %>% 
  mutate(
    resource_adj = max(cum_prod_adj,resource)
  )  

final_df <- final_df %>% 
  mutate(
    depl_adj = cum_prod_adj/resource_adj
  )  

final_df %>%
  ungroup %>%
  summarise(mean = mean(depl_adj, na.rm=TRUE), 
            min = min(depl_adj, na.rm=TRUE), 
            max = max(depl_adj, na.rm=TRUE))

# Keep wanted columns
depl_df <- final_df %>%
  select(doc_field_code, year, depl_adj)

depl_df <- rename(depl_df, depl=depl_adj)


## Save field-level resources data
write_csv(depl_df, path = paste0(save_directory, "depl.csv"))
