## Tracey Mangin
## January 28, 2022
## capex and opex figs

library(data.table)
library(tidyverse)
library(openxlsx)

## source figs
items <- "figure_themes.R"

walk(items, ~ here::here("energy", "extraction-segment",  "figs-and-results", .x) %>% source()) # load local items


## data path
data_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/entry-input-df/final/'
save_directory    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/figures/si-figs/'


## read in data
cost_data <- read_csv(paste0(data_path, "field_capex_opex_forecast_revised.csv"))
cost_data_h <- read_csv(paste0(data_path, "entry_df_final_revised.csv"))

# load historic production
well_prod_org <- fread("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m_processed.csv", colClasses = c('api_ten_digit' = 'character',
                                                                                                                                                                           'doc_field_code' = 'character'))

## field names
field_names <- well_prod_org %>%
  select(doc_field_code, doc_fieldname) %>%
  unique()

## top producers in 2019
prod2019 <- well_prod_org %>%
  filter(year == 2019) %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  mutate(rank = rank(-prod)) %>%
  filter(rank <= 10) %>%
  arrange(rank) %>%
  mutate(doc_fieldname = ifelse(doc_fieldname == "Belridge  South", "Belridge South", doc_fieldname)) 

## join
cost_data2 <- cost_data %>%
  left_join(field_names) %>%
  mutate(field_name_adj = ifelse(doc_field_code %in% prod2019$doc_field_code, doc_fieldname, "Non-top field"),
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
cost_data2$field_name_adj <- factor(cost_data2$field_name_adj, levels = c(prod2019$doc_fieldname, "Non-top field"))
cost_data_h2$field_name_adj <- factor(cost_data_h2$field_name_adj, levels = c(prod2019$doc_fieldname, "Non-top field"))

cost_fig <- 
  ggplot(cost_data2, aes(x = year, y = cost, color = field_name_adj)) +
  geom_line(size = 0.75, alpha = 0.8) +
  facet_wrap(~indicator_name) +
  scale_color_manual(values = cost_value_cols) +
  ylab("Cost (USD per barrel)") +
  theme_line +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = FALSE))


ggsave(filename =  paste0(save_directory, "projected-capex-opex-si-fig.png"), cost_fig, width = 10, height = 5, units = "in", dpi = 300)

## historic
cost_figh <- 
  ggplot(cost_data_h2 %>% filter(year > 1977), aes(x = year, y = cost, color = field_name_adj)) +
  geom_line(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = cost_value_cols) +
  scale_x_continuous(limits = c(1978, 2019), breaks=c(1978,seq(1985,2020,5))) +
  facet_wrap(~indicator_name) +
  ylab("Cost (USD per barrel)") +
  theme_line +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank()) +
  guides(color = guide_legend(nrow = 2, byrow = FALSE))
# +
#   theme(legend.key.width = unit(1,"cm"))

ggsave(filename =  paste0(save_directory, "historical-capex-opex-si-fig.png"), cost_figh, width = 10, height = 5, units = "in", dpi = 300)

