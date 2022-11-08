## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(scales)

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

## update these as needed
# save_info_path <- paste0(main_path, 'outputs/academic-out/extraction/figures/manuscript-update/')
save_revision_path <- paste0(main_path, 'outputs/academic-out/extraction/figures/nature-energy-revision/')

## files
ghg_file        <- 'ghg_emissions_x_field_2018-2045.csv'
prod_file       <- "well_prod_m_processed.csv"
forecast_file     <- 'field_capex_opex_forecast_revised.csv'

## load prod
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

## field names and codes
fnames <- well_prod %>%
  select(doc_field_code, doc_fieldname) %>%
  unique()

## calculate average production by field, 2015-2019
prod_x_field <- expand.grid(doc_field_code = unique(fnames$doc_field_code),
                            year = 2015:2019)

mean_prod_field <- well_prod %>%
  filter(year >= 2015) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T), .groups = 'drop') %>%
  right_join(prod_x_field) %>%
  mutate(prod = ifelse(is.na(prod), 0, prod)) %>%
  group_by(doc_field_code) %>%
  summarise(mean_prod = mean(prod), .groups = 'drop') 

mean_prod_field <- left_join(mean_prod_field, fnames)

## load opex/ capex
price_data = fread(file.path(main_path, 'outputs/stocks-flows/entry-input-df/final/', forecast_file), header = T)
price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
price_data[, sum_cost := m_opex_imputed + m_capex_imputed]
price_data <- price_data[year == 2020, .(doc_field_code, m_opex_imputed, m_capex_imputed, sum_cost)]

## emissions factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T)
ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]
ghg_factors <- ghg_factors[year == 2019, .(doc_field_code, upstream_kgCO2e_bbl)]

## merge
indicators_df <- full_join(mean_prod_field, price_data) %>%
  left_join(ghg_factors) %>%
  filter(!is.na(m_opex_imputed)) %>%
  select(doc_field_code, doc_fieldname, mean_prod, m_opex_imputed:upstream_kgCO2e_bbl)

## save
write_csv(indicators_df, paste0(save_revision_path, "ghg_cost_df.csv"))

## plot
ghg_v_cost_fig <- ggplot(indicators_df, aes(x = m_opex_imputed, y = upstream_kgCO2e_bbl, size = mean_prod / 1e6)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm, show.legend = F) +
  labs(subtitle = "Fields with 0 production 2015-2019 removed",
       size = "Mean annual oil production\n2015-2019 (million bbls)",
       x = "2020 forecasted opex (USD per bbl)",
       y = "2019 GHG intensity (CO2e per bbl)") +
       # color = "GHG emission target",
       # shape = "Policy intervention") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
    # scale_x_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(ghg_v_cost_fig,
       filename = file.path(save_revision_path, 'ghg_x_production_cost.pdf'),
       width = 6,
       height = 6)

embed_fonts(paste0(save_revision_path, 'ghg_x_production_cost.pdf'),
            outfile = paste0(save_revision_path, 'ghg_x_production_cost.pdf'))


# ## capex + opex
# ghg_v_cost2_fig <- ggplot(indicators_df, aes(x = sum_cost, y = upstream_kgCO2e_bbl, size = mean_prod / 1e6)) +
#   geom_point(alpha = 0.6) +
#   labs(size = "Mean annual oil production\n2015-2019 (million bbls)",
#        x = "2020 forecasted opex + capex (USD per bbl)",
#        y = "2019 GHG intensity (CO2e per bbl)") +
#   # color = "GHG emission target",
#   # shape = "Policy intervention") +
#   theme_line +
#   scale_y_continuous(limits = c(0, NA)) +
#   # scale_x_continuous(limits = c(0, NA)) +
#   theme(legend.position = "bottom",
#         legend.key.width= unit(1, 'cm'),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggsave(ghg_v_cost2_fig,
#        filename = file.path(save_revision_path, 'ghg_x_capex_plus_opex.pdf'),
#        width = 6,
#        height = 6)
# 
# embed_fonts(paste0(save_revision_path, 'ghg_x_capex_plus_opex.pdf'),
#             outfile = paste0(save_revision_path, 'ghg_x_capex_plus_opex.pdf'))
# 




