## Tracey Mangin
## May 27, 2021
## historic o&g emissions

# ------------------------------------------- set up and inputs -----------------------------------
proj_dir           <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
raw_directory     <- "data/stocks-flows/raw/"

library(tidyverse)

## ghg emissions
inv_df <- read.csv(paste0(proj_dir, raw_directory, "ghg_sector_data_og_updated.csv"), skip = 9)

inv_df <- janitor::clean_names(inv_df)

inv_df[inv_df == "No Data"] <- NA

## make state level
inv_df2 <- inv_df %>%
  mutate(x2001 = as.character(x2001),
         x2002 = as.character(x2002),
         x2003 = as.character(x2003),
         x2008 = as.character(x2008)) %>%
  pivot_longer(x2000:x2018, names_to = "year", values_to = "value") %>%
  mutate(year = str_sub(year, -4),
         year = as.integer(year),
         value = as.numeric(value)) %>%
  group_by(sub_sector_level_1, year) %>%
  summarise(co2e = sum(value, na.rm = T)) %>%
  ungroup()


write_csv(inv_df2, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/historic_ghg_emissions_og.csv")



         
         