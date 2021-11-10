## tracey mangin
## november 4, 2021
## 2037 kinks exploration

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(plotly)
library(readxl)
library(openxlsx)

## figure themes
theme_line = theme_ipsum(base_family = 'Arial',
                         grid = 'Y', 
                         plot_title_size = 10, 
                         subtitle_size = 9,
                         axis_title_just = 'center',
                         axis_title_size = 9, 
                         axis_text_size = 9,
                         strip_text_size = 9)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.2, 'cm'),
        axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
        axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
        legend.title = element_text(size = 8, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.5),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))


## paths 
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path  <-'data/stocks-flows/processed/'
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'
extraction_folder_path <- 'outputs/academic-out/extraction/extraction_2021-10-22/'
recent_extraction_folder_path <- 'outputs/academic-out/extraction/extraction_2021-11-04/'
state_save_path     = paste0(main_path, extraction_folder_path, 'state-results/')

##
save_info_path <- paste0(main_path, 'outputs/academic-out/extraction/exploratory-figs/identify-kink')

## files
oil_price_file  <- 'oil_price_projections_revised.xlsx'
prod_file       <- "well_prod_m_processed.csv"

## oil prices
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1:4)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

ref_oil <- ggplot(oilpx_scens %>% filter(oil_price_scenario == "reference case"), aes(y = oil_price_usd_per_bbl,
                                                                                      x = year)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) 

## Create population by year time series
ct_population <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, year, pop) %>%
  as.data.table()

state_population <- ct_population %>%
  filter(lower_age > 29)%>%
  group_by(year) %>%
  mutate(state_pop = sum(pop, na.rm = T)) %>%
  summarize(state_pop = unique(state_pop),
            year = unique(year)) %>%
  ungroup() %>%
  as.data.table()


## read inputs
state_out <- fread(paste0(state_save_path, "subset_state_results.csv"))

## filter for BAU

bau_out <- state_out[(oil_price_scenario == "reference case" &
                          carbon_price_scenario %in% c("price floor") &
                            ccs_scenario %in% c("medium CCS cost", "no ccs") &
                            setback_scenario == "no_setback" &
                            excise_tax_scenario == "no tax")]

# merge with population series
bau_out <- merge(bau_out, state_population,
                     by = c("year"),
                     all.x = T)

## production
bau_prod <- ggplot(bau_out, aes(y = total_state_bbl / 1e6, x = year, color = ccs_scenario)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_line

## pull in county level outputs

county_bau <- readRDS(paste0(main_path, recent_extraction_folder_path, 'county-results/reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_county_results.rds'))

county_bau <- county_bau[, .(county, year, county_pop, total_county_bbl, total_county_ghg_kgCO2e, revenue, total_emp, total_comp)]
county_bau[, total_county_ghg_MtCO2e := total_county_ghg_kgCO2e / 1e9]
county_bau[, total_county_ghg_kgCO2e := NULL]
county_bau[, pop_thous := county_pop / 1000]
county_bau[, norm_empl := total_emp / pop_thous]
county_bau[, norm_comp := total_comp / pop_thous]

county_bau <- melt(county_bau, id.vars = c("county", "year"), 
                   measure.vars = c("county_pop", "pop_thous", "total_county_bbl", "total_county_ghg_MtCO2e", "revenue", "total_emp", "norm_empl", "total_comp", "norm_comp"),
                                                                               value.name = 'val')
county_out <- ggplot(county_bau, aes(x = year, y = val, color = county)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw() +
  theme_line +
  theme(legend.position = "top",
        legend.title = element_blank())
  
ggplotly(county_out)
## la county has kink

ggsave(county_out, 
       filename = file.path(save_info_path, 'outputs_x_county.png'), 
       width = 11, 
       height = 11)



## which fields in LA are driving production drop?
field_bau <- readRDS(paste0(main_path, recent_extraction_folder_path, 'field-results/reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_field_results.rds'))

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

## county lut
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

field_bau <- merge(field_bau, county_lut,
                   by = "doc_field_code",
                   allow.cartesian = T)

la_bau <- field_bau[adj_county_name == "Los Angeles"]
la_bau[, county_name := NULL]
la_bau <- distinct(la_bau)



fig_la <- ggplot(la_bau,
                 aes(x = year, y = total_prod_bbl / 1e6, color = doc_fieldname)) +
  geom_line(alpha = 0.8) +
  labs(title = "Production by field in Los Angeles, BAU",
       y = "Production (million barrels)",
       x = NULL) +
  theme_bw() +
  geom_vline(xintercept = 2037, lty = "dashed", color = "grey") +
  geom_text(data = la_bau %>% filter(year == 2045 & total_prod_bbl >= 1.4 * 1e6), aes(x = year - 2, y = (total_prod_bbl / 1e6) + 1, label = doc_fieldname)) +
  theme_line +
  theme(legend.position = "none",
        legend.title = element_blank()) 

ggplotly(fig_la)

##
ggsave(fig_la, 
       filename = file.path(save_info_path, 'outputs_x_field_la.png'), 
       width = 8, 
       height = 6)

## depletion and exit
bau_depl <- read_csv(paste0(main_path, 
                            'outputs/predict-production/extraction_2021-11-03/subset_updated_ccs/depl-out/reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_depletion.csv'))

setDT(bau_depl)
bau_depl <- bau_depl[doc_field_code == "849"]

ggplot(bau_depl, aes(x = year, y = depl)) +
  geom_line() +
  geom_hline(yintercept = 0.9999, lty = "dashed", color = "grey") +
  geom_vline(xintercept = 2038, lty = "dashed", color = "grey") +
  annotate(geom = "text", x = 2038, y = 0.93, label = "2038", hjust = 0, vjust = 1, size = 4) +
  theme_line 

## exit
bau_exit <- read_csv(paste0(main_path, 
                            'outputs/predict-production/extraction_2021-11-03/subset_updated_ccs/exit-out/reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_exit.csv'))

setDT(bau_exit)
bau_exit <- bau_exit[doc_field_code == "849"]

