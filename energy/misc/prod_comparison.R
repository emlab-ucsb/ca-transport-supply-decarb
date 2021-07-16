## Tracey Mangin
## July 9, 2021
## compare 2019 to 2020; last 20 vs pred 20; field and county

## libraries
library(data.table)
library(tidyverse)
library(scales)

## paths
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
projection_path   = 'predict-production/extraction_2021-07-15/revised-edit-exit-model'

## files
histprod_file   = 'stocks-flows/crude_prod_x_field_revised.csv'
model_out       = 'diagnostic-field-level-results.csv'

## read in files
prod_hist = fread(file.path(outputs_path, histprod_file), header = T, colClasses = c("doc_field_code" = "character"))
projected_prod = fread(file.path(outputs_path, projection_path, model_out), header = T, colClasses = c("doc_field_code" = "character"))

## bau production

bau_prod = projected_prod[prod_quota_scenario == "no quota" & setback_scenario == "no_setback"]
bau_prod = bau_prod[, .(doc_field_code, year, total_prod_bbl)]
setnames(bau_prod, "total_prod_bbl", "total_bbls")

## 2019 vs 2020
comp_19_20 = prod_hist[year == 2019]
comp_19_20[, year := NULL]
setnames(comp_19_20, "total_bbls", "bbls_2019")

comp_20 = bau_prod[year == 2020]
comp_20[, year := NULL]
setnames(comp_20, "total_bbls", "bbls_2020")

comp_19_20 = merge(comp_19_20, comp_20,
                   all.x = T)

comp_19_20[, bbls_2020 := fifelse(is.na(bbls_2020), 0, bbls_2020)]

comp_19_20 <- comp_19_20 %>%
  pivot_longer(bbls_2019:bbls_2020, names_to = "year", values_to = "bbls") %>%
  mutate(year = ifelse(year == "bbls_2019", 2019, 2020),
         year = as.integer(year))


## make plot
year_comp <- ggplot(comp_19_20, aes(x = year, y = bbls, group = doc_fieldname)) +
  geom_line() +
  labs(x = NULL,
      y = "barrels") +
  geom_point(data = comp_19_20, aes(x = year, y = bbls, group = doc_fieldname)) +
  geom_text(data = comp_19_20 %>% filter(year == 2020), aes(x = year, label = doc_fieldname)) +
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(breaks=c(2019, 2020)) +
  theme_bw()

## cumulative
hist_20 = prod_hist[year >= 2015, .(cumul_hist = sum(total_bbls)), by = .(doc_field_code, doc_fieldname)]
pred_20 = bau_prod[year <=  2024, .(cumul_projected = sum(total_bbls)), by = .(doc_field_code)]

cumul_comp <- full_join(hist_20, pred_20)
cumul_comp[is.na(cumul_comp)] = 0

cumul_comp <- cumul_comp %>%
  pivot_longer(cumul_hist:cumul_projected, names_to = "period", values_to = "bbls") %>%
  mutate(period = ifelse(period == "cumul_hist", "historic", "projected"))

## make plot
year_comp <- ggplot(cumul_comp, aes(x = period, y = bbls / 1e6, group = doc_fieldname)) +
  geom_line() +
  labs(x = NULL,
       y = "million bbls") +
  geom_point(data = cumul_comp, aes(x = period, y = bbls  / 1e6, group = doc_fieldname)) +
  geom_text(data = cumul_comp %>% filter(period == "projected"), aes(x = "projected", label = doc_fieldname), hjust = 0) +
  theme_bw()


