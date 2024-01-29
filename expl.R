library(dplyr)
library(lubridate)
library(ggplot2)

# Convert month_year to Date type
well_prod_m_processed$month_year <- as.Date(well_prod_m_processed$month_year)

# Identify wells with both 'Active' and 'Idle' statuses
wells_with_active_idle <- well_prod_m_processed %>%
  group_by(APINumber) %>%
  filter(any(ProductionStatus == "Active") & any(ProductionStatus == "Idle")) %>%
  ungroup()

# Calculate lifespan for wells that transitioned from 'Active' to 'Idle'
well_lifespans <- wells_with_active_idle %>%
  group_by(APINumber) %>%
  summarise(Start = min(month_year[ProductionStatus == "Active"]),
            End = max(month_year[ProductionStatus == "Idle"]),
            Lifespan = as.numeric(difftime(End, Start, units = "days")) / 365.25)

# Calculate average lifespan by year of establishment
average_lifespan_by_year <- well_lifespans %>%
  group_by(EstablishmentYear = year(Start)) %>%
  summarise(AverageLifespan = mean(Lifespan, na.rm = TRUE))

# Identify wells that are not 'Idle'
wells_not_idle <- well_prod_m_processed %>%
  group_by(APINumber) %>%
  filter(!any(ProductionStatus == "Idle")) %>%
  ungroup()

# Determine establishment year for non-idle wells
wells_establishment_year <- wells_not_idle %>%
  group_by(APINumber) %>%
  summarise(EstablishmentYear = year(min(month_year)))

# Count wells by year of establishment
count_wells_by_year <- wells_establishment_year %>%
  group_by(EstablishmentYear) %>%
  summarise(NumberOfWells = n())

# Combine lifespan and count data
combined_data <- left_join(average_lifespan_by_year, count_wells_by_year, by = "EstablishmentYear")

# Count the number of unique "new" wells per year
new_wells_count_per_year <- well_prod_m_processed %>%
  filter(ProductionStatus == "new") %>%
  mutate(year = year(month_year)) %>%
  group_by(year) %>%
  summarise(NumberOfNewWells = n_distinct(APINumber))

all_combined_data <- left_join(combined_data, new_wells_count_per_year, by = c("EstablishmentYear" = "year"))

all_combined_data

new_status_count <- well_prod_m_processed %>%
  filter(ProductionStatus == "New") 

new_status_count
