## Tracey Mangin
## November 17, 2020
## macroeconomic condition figures

library(data.table)
library(tidyverse)
library(openxlsx)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items


## data path
data_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/'
save_directory    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/drafts/fuels-model/'

## read in data
cost_data <- read_csv(paste0(data_path, "field_capex_opex_forecast_final.csv"))
cost_data_h <- read_csv(paste0(data_path, "entry_df_final.csv"))

# load historic production
well_prod_org <- read_rds("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds") %>% as.data.table()

setnames(well_prod_org, "FieldCode", "doc_field_code")
well_prod <- well_prod_org[year == 2019]

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10)) 

field_names <- wells_19 %>%
  select(FieldCode, FieldName) %>%
  unique() %>%
  rename(doc_field_name = FieldName,
         doc_field_code = FieldCode)

## top producers in 2019
prod2019 <- well_prod_org %>%
  mutate(api_ten_digit = substr(APINumber, 1, 10)) %>%
  mutate(FieldCode2 = paste0("00", doc_field_code),
         FieldCode3 = str_sub(FieldCode2, start= -3)) %>%
  rename(orig_fc = doc_field_code,
         FieldCode = FieldCode3) %>%
  select(-orig_fc, -FieldCode2) %>%
  filter(year == 2019) %>%
  group_by(FieldCode) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(rank = rank(-prod)) %>%
  rename(doc_field_code = FieldCode) %>%
  filter(rank <= 10) %>%
  arrange(rank) %>%
  left_join(field_names) %>%
  mutate(doc_field_name = ifelse(doc_field_name == "Belridge  South", "Belridge South", doc_field_name)) 

## join

cost_data2 <- cost_data %>%
  left_join(field_names) %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_field_name, "Non-top field"),
         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  group_by(field_name_adj, year) %>%
  summarise(m_opex_imputed = mean(m_opex_imputed),
            m_capex_imputed = mean(m_capex_imputed)) %>%
  ungroup() %>%
  pivot_longer(m_opex_imputed:m_capex_imputed, names_to = "indicator", values_to = "cost") %>%
  mutate(indicator_name =  ifelse(indicator == "m_opex_imputed", "OpEx", "CapEx"))

## historic data

cost_data_h2 <- cost_data_h %>%
  select(doc_fieldname, doc_field_code, year, opex_imputed, capex_imputed) %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_fieldname, "Non-top field"),
         field_name_adj = ifelse(field_name_adj == "Belridge  South", "Belridge South", field_name_adj)) %>%
  group_by(field_name_adj, year) %>%
  summarise(m_opex_imputed = mean(opex_imputed),
            m_capex_imputed = mean(capex_imputed)) %>%
  ungroup() %>%
  pivot_longer(m_opex_imputed:m_capex_imputed, names_to = "indicator", values_to = "cost") %>%
  mutate(indicator_name =  ifelse(indicator == "m_opex_imputed", "OpEx", "CapEx"))


## facet figure
cost_cols <- length(unique(cost_data2$field_name_adj))
cost_value_cols <- colorRampPalette(ucsb_distinct)(cost_cols)

## factor
cost_data2$field_name_adj <- factor(cost_data2$field_name_adj, levels = c(prod2019$doc_field_name, "Non-top field"))
cost_data_h2$field_name_adj <- factor(cost_data_h2$field_name_adj, levels = c(prod2019$doc_field_name, "Non-top field"))

cost_fig <- 
  ggplot(cost_data2, aes(x = year, y = cost, color = field_name_adj)) +
  geom_line(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = cost_value_cols) +
  facet_wrap(~indicator_name) +
  ylab("Cost (USD per barrel)") +
  base_theme +
  guides(color = guide_legend(nrow = 2, byrow = FALSE))
# +
#   theme(legend.key.width = unit(1,"cm"))

ggsave(filename =  paste0(save_directory, "interim-fuels-f4.png"), cost_fig, width = 10, height = 5, units = "in", dpi = 300)

## historic
cost_figh <- 
  ggplot(cost_data_h2 %>% filter(year > 1977), aes(x = year, y = cost, color = field_name_adj)) +
  geom_line(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = cost_value_cols) +
  scale_x_continuous(limits = c(1978, 2019), breaks=c(1978,seq(1985,2020,5))) +
  facet_wrap(~indicator_name) +
  ylab("Cost (USD per barrel)") +
  base_theme +
  guides(color = guide_legend(nrow = 2, byrow = FALSE))
# +
#   theme(legend.key.width = unit(1,"cm"))

ggsave(filename =  paste0(save_directory, "interim-fuels-f5.png"), cost_figh, width = 10, height = 5, units = "in", dpi = 300)



