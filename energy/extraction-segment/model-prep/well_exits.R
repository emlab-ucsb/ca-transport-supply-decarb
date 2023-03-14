## Tracey Mangin
## Septebmer 10, 2020
## Well exits exploration

## load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(scales)
library(zoo)
library(data.table)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## functions

## remove outliers for plotting
filter_lims <- function(x){
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

## calculate first and 99th percentile
filter_lims2 <- function(x, minp, maxp, val) {
  
  l <- as.numeric(length(x))
  
  vals <- quantile(x, c(minp, maxp)) 
  
  min_percentile <- as.numeric(vals[1])
  
  max_percentile <- as.numeric(vals[2])
  
  z <- ifelse(val >= min_percentile & val <= max_percentile, val, NA)
  
  z <- ifelse(l < 3, val, z)
  
  return(z)
}

calc_adj_val <- function(x, pval) {
  
  val <- as.numeric(quantile(x, pval))
  
  return(val)
  
}

  

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"

## well production
well_prod <- fread(paste0(data_directory, "well_prod_m_processed.csv"), colClasses = c('api_ten_digit' = 'character',
                                                                                       'doc_field_code' = 'character'))


# ## for initial year production
# init_yr_prod <- read.csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_start_yr/well_start_prod_api10_x_field.csv") %>%
#   mutate(api_ten_digit = paste0("0", api_ten_digit)) %>%
#   mutate(FieldCode2 = paste0("00", FieldCode),
#          FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
#   rename(orig_fc = FieldCode,
#          FieldCode = FieldCode3) %>%
#   select(-orig_fc, -FieldCode2)



## all wells
all_wells <- fread("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/AllWells_table/AllWells_20210427.csv", colClasses = c('API' = 'character')) %>%
  mutate(spud_date = as.Date(SpudDate, format = "%m/%d/%Y")) 

old_all_wells <- read_xlsx("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/All_wells_20200417.xlsx")

## well field codes
well_fields <- all_wells %>%
  select(api_ten_digit = API, doc_fieldname = FieldName) %>%
  unique() 

## plugged wells
plugged <- all_wells %>%
  select(-SpudDate, -spud_date) %>%
  unique() %>%
  filter(WellStatus == "Plugged") 

## any duplicates?
# all_wells_p <- all_wells %>%
#   select(-SpudDate, -spud_date) %>%
#   unique() %>%
#   filter(API %in% plugged$API) %>%
#   group_by(API) %>%
#   mutate(n = n()) %>%
#   ungroup()


pluggedonly <- all_wells %>%
  select(-SpudDate, -spud_date) %>%
  unique() %>%
  filter(WellStatus == "PluggedOnly") 

## top 10 fields 2019
top_fields_2019 <- well_prod %>%
  filter(year == 2019) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(total_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  arrange(-total_prod) %>%
  mutate(field_rank = rank(-total_prod)) %>%
  filter(field_rank <= 10) %>%
  select(doc_field_code, doc_fieldname, field_rank)



# ## filter init_year for plugged wells
# plugged_prod <- init_yr_prod %>%
#   filter(api_ten_digit %in% c(plugged$API, pluggedonly$API)) %>%
#   mutate(start_year = year(start_date),
#          vintage = ifelse(start_year < 1978, "pre 1978",
#                    ifelse(start_year >= 1978 & start_year < 1983, "1978-1982",
#                           ifelse(start_year >= 1983 & start_year < 1988, "1983-1987",
#                                  ifelse(start_year >= 1988 & start_year < 1993, "1988-1992", 
#                                         ifelse(start_year >= 1993 & start_year < 1998, "1993-1997",
#                                                ifelse(start_year >= 1998 & start_year < 2003, "1998-2002",
#                                                       ifelse(start_year >= 2003 & start_year < 2008, "2003-2007",
#                                                              ifelse(start_year >= 2008 & start_year < 2013, "2008-2012", "2013-2019")))))))))
# 
# 
# ## investigate newer vintage, plugged
# vintage_df <- plugged_prod %>%
#   select(api_ten_digit, start_date, start_year, vintage) %>%
#   unique() %>%
#   filter(vintage == "2013-2019")


## zero_production
well_api_prod <- well_prod %>%
  # filter(api_ten_digit %in% c(plugged$API)) %>%
  filter(api_ten_digit %in% plugged$API) %>%
  # filter(api_ten_digit %in% c(plugged$API, pluggedonly$API)) %>%
  # mutate(doc_field_code = ifelse(doc_field_code == "848", "849", doc_field_code)) %>%
  group_by(api_ten_digit, doc_field_code, doc_fieldname, year, month, month_year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(api_field = paste0(api_ten_digit, "-", doc_field_code))

# total_api_field_prod <- well_api_prod %>%
#   group_by(api_field) %>%
#   summarise(api_field_total = sum(prod)) %>%
#   ungroup() %>%
#   filter(api_field_total > 0)

pos_well_api_prod <- well_api_prod %>%
  group_by(api_ten_digit, api_field) %>%
  summarise(api_total = sum(prod)) %>%
  ungroup() %>%
  filter(api_total > 0) %>%
  mutate(doc_field_code = as.character(str_sub(api_field, -3, -1)))

### test
test_fc <- "052"

test_df <- pos_well_api_prod %>%
  mutate(doc_field_code = as.character(str_sub(api_field, -3, -1))) %>%
  filter(doc_field_code == test_fc) %>%
  select(api_ten_digit) %>%
  unique() 

anti_join(well_api_prod %>% select(doc_field_code) %>% unique(), pos_well_api_prod %>% select(doc_field_code) %>% unique())


## last date of production info

plugged_last_prod <- well_api_prod %>%
  filter(api_field %in% pos_well_api_prod$api_field) %>%
  # filter(api_ten_digit %in% test_df$api_ten_digit) %>%
  # filter(prod > 0) %>%
  ## CHANGE ABOVE TO FILTER OUT TRAILING ZEROS
  # group_by(api_ten_digit, FieldCode, api_field) %>%
  group_by(api_ten_digit) %>%
  filter(month_year == max(month_year)) %>%
  ungroup() %>%
  # select(api_ten_digit, FieldCode, api_field, last_prod_date = month_year) %>%
  select(api_ten_digit, doc_field_code, doc_fieldname, api_field, last_prod_date = month_year) %>%
  unique()

## which have two values?
plugged_field_n <- plugged_last_prod %>%
  group_by(api_ten_digit) %>%
  mutate(n = n()) %>%
  ungroup() %>% 
  filter(n > 1)

## get all combos
well_year_combos <- expand.grid(api_field = as.character(unique(plugged_last_prod$api_field)),
                                month_year = unique(well_api_prod$month_year))


## find last 12 months
plugged_prod_12mo <- well_api_prod %>%
  filter(api_ten_digit %in% pos_well_api_prod$api_ten_digit) %>%
  full_join(well_year_combos) %>%
  mutate(api_ten_digit = ifelse(is.na(api_ten_digit), as.character(str_sub(api_field, 1, 10)), api_ten_digit),
         doc_field_code = ifelse(is.na(doc_field_code), as.character(str_sub(api_field, -3, -1)), doc_field_code)) %>%
  mutate(prod = ifelse(is.na(prod), 0, prod)) %>%
  filter(api_field %in% pos_well_api_prod$api_field) %>%
  left_join(plugged_last_prod) %>%
  filter(month_year <= last_prod_date) %>%
  arrange(api_ten_digit, month_year) %>%
  mutate(count = 1) %>%
  group_by(api_ten_digit) %>%
  mutate(cumsum = cumsum(count),
         max_count = max(cumsum),
         max_sub = max_count - 12) %>%
  filter(cumsum > max_sub) %>%
  ungroup() 

## test 2
test2 <- plugged_prod_12mo %>%
  filter(api_ten_digit %in% test_df$api_ten_digit)


## n for each well
ntime <- plugged_prod_12mo %>%
  group_by(api_ten_digit) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n < 12)

## how many in multiple fields
plug_prod_n <- plugged_prod_12mo %>%
  select(api_ten_digit, doc_field_code) %>%
  unique() %>%
  group_by(api_ten_digit) %>%
  mutate(n = n()) %>%
  ungroup() 



## define quantiles
q1 <- 0.01
q2 <- 0.99

## final year production
plugged_prod_final_yr <- plugged_prod_12mo %>%
  # group_by(api_ten_digit, FieldCode) %>%
  group_by(api_ten_digit, doc_field_code, doc_fieldname) %>%
  summarise(final_yr_prod = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, doc_fieldname) %>%
  mutate(n_wells = n()) %>%
  ungroup() %>%
  group_by(doc_field_code, doc_fieldname) %>%
  mutate(minq = calc_adj_val(x = final_yr_prod, pval = q1),
         maxq = calc_adj_val(x = final_yr_prod, pval = q2)) %>%  
  ungroup() %>%
  mutate(final_yr_prod_adj = ifelse(n_wells < 3, final_yr_prod,
                                    ifelse(final_yr_prod >= minq & final_yr_prod <= maxq, final_yr_prod, NA))) 

# %>%
#   left_join(top_fields_2019) %>%
#   mutate(field_rank = ifelse(is.na(field_rank), 11, field_rank)) %>%
#   mutate(adj_field_name = ifelse(doc_field_code %in% top_fields_2019$doc_field_code, paste0(FieldName, ": Rank = ", field_rank), "Other"))

## xxx
nf <- plugged_prod_final_yr %>%
  select(api_ten_digit, doc_field_code) %>%
  unique() %>%
  group_by(api_ten_digit) %>%
  summarise(n = n()) %>%
  ungroup()

## test
test3 <- plugged_prod_final_yr %>%
  group_by(doc_field_code) %>%
  summarise(meanprod = mean(final_yr_prod), 
            meanadj = mean(final_yr_prod_adj, na.rm = T)) %>%
  ungroup()



# ## add factors
# plugged_prod_final_yr$adj_field_name <- factor(plugged_prod_final_yr$adj_field_name, levels = c("Belridge  South: Rank = 1", "Midway-Sunset: Rank = 2", "Kern River: Rank = 3",
#                                                                                                 "Cymric: Rank = 4", "Wilmington: Rank = 5", "Lost Hills: Rank = 6", "San Ardo: Rank = 7", 
#                                                                                                 "Elk Hills: Rank = 8", "Coalinga: Rank = 9", "Poso Creek: Rank = 10", "Other"))

ggplot(plugged_prod_final_yr, aes(x = adj_field_name, y = final_yr_prod)) + 
  geom_boxplot(na.rm = TRUE, coef = 5) +  # remove NAs, and set the whisker length to all included points
  ylab("Crude production in last month\n(thousand bbls)") +
  scale_y_continuous(labels = comma) +
  draft_theme +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

## summary of exit info
exit_prod <- plugged_prod_final_yr %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(mean_final_yr_prod = mean(final_yr_prod),
            mean_final_yr_prod_adj = mean(final_yr_prod_adj, na.rm = T),
            diff = mean_final_yr_prod - mean_final_yr_prod_adj,
            median_final_yr_prod = median(final_yr_prod),
            median_final_yr_prod_adj = median(final_yr_prod_adj, na.rm = T),
            n_wells = n()) %>%
  ungroup()

exit_prod_output <- exit_prod %>%
  select(doc_field_code, mean_final_yr_prod, mean_final_yr_prod_adj, n_wells) 

# old <- fread("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_exit_volume_x_field_v1.csv", colClasses = c('doc_field_code' = 'character'))
# 
# old <- old %>%
#   rename(old_mean_final_yr_prod = mean_final_yr_prod,
#          old_mean_final_yr_prod_adj = mean_final_yr_prod_adj,
#          old_n_wells = n_wells)
# 
# comp <- old %>%
#   full_join(exit_prod_output) %>%
#   mutate(diff = mean_final_yr_prod - old_mean_final_yr_prod,
#          diff_adj = mean_final_yr_prod_adj - old_mean_final_yr_prod_adj,
#          diff_wells = n_wells - old_n_wells)

write_csv(exit_prod_output, file = "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/well_exit_volume_x_field_v1_revised.csv")





ggplot(exit_prod %>% filter(mean_final_yr_prod < 10000), aes(x = mean_final_yr_prod)) +
  geom_histogram(bins = 100)


# plugged_prod_final_yr$vintage <- factor(plugged_prod_final_yr$vintage, levels = c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))

## figures
final_prod_fig <- ggplot(plugged_prod_final_yr, aes(x = vintage, y = final_yr_prod)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~adj_field_name, scales = "free_y") +
  ylab("Crude production in last month\n(thousand bbls)") +
  scale_y_continuous(labels = comma) +
  draft_theme +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
  

plugged_prod_final_yr %>%
  group_by(adj_field_name, vintage) %>%
  mutate(final_yr_prod_adj = filter_lims(final_yr_prod)) %>%  # new variable (value2) so as not to displace first one)
  ggplot(aes(x = vintage, y = final_yr_prod_adj)) + 
  geom_boxplot(na.rm = TRUE, coef = 5) +  # remove NAs, and set the whisker length to all included points
  facet_wrap( ~ adj_field_name, scales = "free_y") + 
  ylab("Crude production in last month\n(thousand bbls)") +
  scale_y_continuous(labels = comma) +
  draft_theme +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

## add the year before shutdown year
plugged_prod_2fy <- plugged_prod %>%
  left_join(plugged_last_prod) %>%
  mutate(month_year = as.Date(month_year)) %>%
  filter(month_year <= last_prod_date) %>%
  mutate(count = 1) %>%
  group_by(api_ten_digit, FieldCode, api_field) %>%
  mutate(cumsum = cumsum(count),
         max_count = max(cumsum),
         max_sub = max_count - 24,
         max_sub2 = max_count - 12) %>%
  filter(cumsum <= max_sub2 & cumsum > max_sub) %>%
  ungroup() %>%
  group_by(api_ten_digit, FieldCode, n_field, api_field, vintage) %>%
  summarise(prod = sum(api_prod, na.rm = T)) %>%
  ungroup() %>%
  left_join(field_names_df) %>%
  rename(doc_field_code = FieldCode) %>%
  left_join(top_fields_2019) %>%
  mutate(field_rank = ifelse(is.na(field_rank), 11, field_rank)) %>%
  mutate(adj_field_name = ifelse(doc_field_code %in% top_fields_2019$doc_field_code, paste0(FieldName, ": Rank = ", field_rank), "Other"),
         year_grp = "second_to_last")
  

## add factors
plugged_prod_2fy$adj_field_name <- factor(plugged_prod_2fy$adj_field_name, levels = c("Belridge  South: Rank = 1", "Midway-Sunset: Rank = 2", "Kern River: Rank = 3",
                                                                                                "Cymric: Rank = 4", "Wilmington: Rank = 5", "Lost Hills: Rank = 6", "San Ardo: Rank = 7", 
                                                                                                "Elk Hills: Rank = 8", "Coalinga: Rank = 9", "Poso Creek: Rank = 10", "Other"))
plugged_prod_2fy$vintage <- factor(plugged_prod_2fy$vintage, levels = c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))

## combine
comp_df <- plugged_prod_final_yr %>%
  rename(prod = final_yr_prod) %>%
  mutate(year_grp = "final_year") %>%
  rbind(plugged_prod_2fy) %>%
  group_by(adj_field_name, vintage, year_grp) %>%
  mutate(prod_adj = filter_lims(prod)) %>%
  ungroup()

comp_df$year_grp <- factor(comp_df$year_grp, levels = c("second_to_last", "final_year"))


## plot
ggplot(comp_df, aes(x = vintage, y = prod_adj, color = year_grp)) + 
  geom_boxplot(na.rm = TRUE, coef = 5) +  # remove NAs, and set the whisker length to all included points
  facet_wrap( ~ adj_field_name, scales = "free_y") + 
  ylab("Crude production in last month\n(thousand bbls)") +
  scale_y_continuous(labels = comma) +
  draft_theme +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

## do all plugged years, one estimate by field

plugged_prod_final_yr_fl <- plugged_prod_12mo %>%
  group_by(api_ten_digit, FieldCode, n_field, api_field, vintage) %>%
  summarise(final_yr_prod = sum(api_prod, na.rm = T)) %>%
  ungroup() %>%
  left_join(field_names_df) %>%
  rename(doc_field_code = FieldCode) %>%
  left_join(top_fields_2019) %>%
  mutate(field_rank = ifelse(is.na(field_rank), 11, field_rank)) %>%
  mutate(adj_field_name = ifelse(doc_field_code %in% top_fields_2019$doc_field_code, paste0(FieldName, ": Rank = ", field_rank), "Other"))





## do this again, well age
## ------------------------------------

## zero_production
plugged_age_final_prod <- plugged_prod %>%
  filter(api_prod > 0) %>%
  mutate(month_year = as.Date(month_year)) %>%
  group_by(api_ten_digit) %>%
  filter(month_year == max(month_year)) %>%
  ungroup() %>%
  mutate(well_age_year = well_age / 365) %>%
  left_join(field_names_df) %>%
  rename(doc_field_code = FieldCode) %>%
  left_join(top_fields_2019) %>%
  mutate(field_rank = ifelse(is.na(field_rank), 11, field_rank)) %>%
  mutate(adj_field_name = ifelse(doc_field_code %in% top_fields_2019$doc_field_code, paste0(FieldName, ": Rank = ", field_rank), "Other")) %>%
  group_by(adj_field_name, vintage) %>%
  mutate(well_age_year_adj = filter_lims(well_age_year)) %>%
  ungroup() 


## add factors
plugged_age_final_prod$adj_field_name <- factor(plugged_age_final_prod$adj_field_name, levels = c("Belridge  South: Rank = 1", "Midway-Sunset: Rank = 2", "Kern River: Rank = 3",
                                                                                        "Cymric: Rank = 4", "Wilmington: Rank = 5", "Lost Hills: Rank = 6", "San Ardo: Rank = 7", 
                                                                                        "Elk Hills: Rank = 8", "Coalinga: Rank = 9", "Poso Creek: Rank = 10", "Other"))
plugged_age_final_prod$vintage <- factor(plugged_age_final_prod$vintage, levels = c("pre 1978", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2019"))


## plot
ggplot(plugged_age_final_prod, aes(x = vintage, y = well_age_year_adj)) + 
  geom_boxplot(na.rm = TRUE, coef = 5) +  # remove NAs, and set the whisker length to all included points
  facet_wrap( ~ adj_field_name, scales = "free_y") + 
  ylab("Well age at least time of production\n(years)") +
  scale_y_continuous() +
  draft_theme +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

vintage_subset <- c("1978-1982", "1983-1987", "1988-1992")


## histogram
ggplot(plugged_age_final_prod %>% filter(vintage %in% vintage_subset), aes(x = round(well_age_year), fill = vintage)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = rev(prim_calepa_pal[1:3]))

## histogram
ggplot(plugged_age_final_prod %>% filter(vintage %in% vintage_subset), aes(x = round(well_age_year), fill = adj_field_name)) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = calepa_pal)

## cumulative production

well_info <- plugged_prod %>%
  select(api_ten_digit, start_year, FieldCode, vintage) %>%
  unique() %>%
  group_by(api_ten_digit) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(api_ten_digit, start_year) %>%
  group_by(api_ten_digit) %>%
  filter(start_year == min(start_year)) %>%
  ungroup() %>%
  select(-n)

plugged_cumul_prod <- well_info %>%
  left_join(plugged_prod) %>%
  arrange(api_ten_digit, month_year) %>%
  group_by(api_ten_digit) %>%
  mutate(cumul_prod = cumsum(api_prod),
         sum_well_prod = sum(api_prod)) %>%
  ungroup() 


plugged_cumul_prod2 <- plugged_cumul_prod %>%
  group_by(vintage, FieldCode, well_age) %>%
  summarise(sum_prod = sum(api_prod)) %>%
  ungroup() %>%
  group_by(vintage, FieldCode) %>%
  mutate(cumul_prod = cumsum(sum_prod),
         total_prod = sum(sum_prod)) %>%
  ungroup() %>%
  mutate(rel_prod = cumul_prod / total_prod) %>%
  left_join(field_names_df) %>%
  rename(doc_field_code = FieldCode) %>%
  left_join(top_fields_2019) %>%
  mutate(field_rank = ifelse(is.na(field_rank), 11, field_rank)) %>%
  mutate(adj_field_name = ifelse(doc_field_code %in% top_fields_2019$doc_field_code, paste0(FieldName, ": Rank = ", field_rank), "Other"))

test <- plugged_cumul_prod2 %>%
  filter(field_rank == 1,
         vintage == "pre_1987")


