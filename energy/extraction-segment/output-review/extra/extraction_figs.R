## Tracey Mangin
## August 11, 2021
## extraction figures

library(tidyverse)
library(data.table)
library(openxlsx)
library(plotly)
library(cowplot)
library(hrbrthemes)


## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
results_path <- "outputs/predict-production/extraction_2021-08-10/update_final/"
save_path    <- 'supporting-figs/'

## file names
prod_file  <- "well_prod_m_processed.csv"
ghg_file <- 'ghg_emissions_x_field_2018-2045.csv'
state_out_file <- "benchmark-state-level-results.csv"
field_file <- "benchmark-field-level-results.csv"
depl_file <- "benchmark-depletion-results.csv"
density_file <- "benchmark-density-results.csv"
exit_file <- "benchmark-exit-results.csv"


## plot theme
theme_line =  
  theme_ipsum(base_family = 'Arial',
              grid = 'Y',
              plot_title_size = 12,
              subtitle_size = 11,
              axis_title_just = 'center',
              axis_title_size = 12,
              axis_text_size = 12,
              strip_text_size = 12)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
        # axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), angle = 90),
        axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.25, "cm"),
        axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
        plot.margin = unit(c(1,2,1,1), "lines"),
        strip.text = element_text(hjust = 0.5),
        legend.text = element_text(size = 11),
        legend.position = 'bottom',
  )


## load data
## --------------

## monthly well prod
well_prod <- fread(paste0(calepa_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                   'doc_field_code' = 'character'))
## ghg factors
ghg_factors = fread(file.path(calepa_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## outputs
state_out <- fread(paste0(calepa_path, results_path, state_out_file))
field_out <- fread(paste0(calepa_path, results_path, field_file), header = T, colClasses = c('doc_field_code' = 'character'))
depl_out <- fread(paste0(calepa_path, results_path, depl_file), header = T, colClasses = c('doc_field_code' = 'character'))
density_out <- fread(paste0(calepa_path, results_path, density_file), header = T, colClasses = c('doc_field_code' = 'character'))
exit_out <- fread(paste0(calepa_path, results_path, exit_file), header = T, colClasses = c('doc_field_code' = 'character'))

## Why does production drop around 2040 under BAU?
## field level production and depletion
## ------------------------------------------

## filter for BAU but high oil price

high_px_out <- field_out %>%
  filter(oil_price_scenario == "high oil price",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         setback_scenario == "no_setback",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax") %>%
  mutate(scenario = "high_oil_px") %>%
  select(scenario, doc_field_code, doc_fieldname, year, ccs_adopted:cumulative_wells)

high_px_depl <- depl_out %>%
  filter(oil_price_scenario == "high oil price",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         setback_scenario == "no_setback",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax") %>%
  mutate(scenario = "high_oil_px") %>%
  select(scenario, doc_field_code, year, depl)

high_px_prod_depl <- high_px_out %>%
  full_join(high_px_depl)



## ggplot plot
depl_fig <- ggplot(high_px_prod_depl, aes(x = year, y = depl, group = doc_field_code)) +
  geom_line()

## production figure
prod_fig <- ggplot(high_px_prod_depl, aes(x = year, y = total_prod_bbl / 1e6, group = doc_field_code, color = doc_field_code)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_line(data = state_out %>%
              filter(oil_price_scenario == "high oil price",
                     innovation_scenario == "low innovation",
                     carbon_price_scenario == "price floor",
                     ccs_scenario == "medium CCS cost",
                     setback_scenario == "no_setback",
                     prod_quota_scenario == "no quota",
                     excise_tax_scenario == "no tax"), aes(x = year, y = total_prod_bbl / 1e6), color = "black", inherit.aes = F) +
  labs(y = "production (million bbls)",
       x = NULL) +
  annotate("text", x = 2039, y = 70, label = "Elk Hills hits\ndepletion cap in 2041\n(stops new well entry)") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 220)) +
  theme_line +
  theme(legend.position = "none") 

# ggplotly(prod_fig)

## save
ggsave(prod_fig, 
       filename = paste0(calepa_path, results_path, save_path, "high_px_production.pdf"), 
       width = 6, 
       height = 6)

embed_fonts(paste0(calepa_path, results_path, save_path, "high_px_production.pdf"),
            outfile = paste0(calepa_path, results_path, save_path, "high_px_production.pdf"))


## setbacks
### --------------------------

## Why does ghg average GHG emissions per bbl of oil decrease with highest setback?
## plot all four setback scenarios, by field, color = ghg emissions intensity

## filter for all setback scenarios

setback_out <- field_out %>%
  filter(oil_price_scenario == "reference case",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax") %>%
  mutate(scenario = "setbacks") %>%
  select(scenario, setback_scenario, doc_field_code, doc_fieldname, year, ccs_adopted:cumulative_wells)

ghg_factors_join <- ghg_factors %>%
  select(doc_field_code, year, upstream_kgCO2e_bbl)

setback_out <- setback_out %>%
  left_join(ghg_factors_join)

ggplot(setback_out, aes(x = year, y = total_prod_bbl / 1e6, group = doc_field_code, color = upstream_kgCO2e_bbl)) +
  geom_line(size = 0.5, alpha = 0.6) +
  facet_wrap(~setback_scenario, ncol = 4) +
  labs(y = "production (million bbls)",
       x = NULL,
       color = "upstream kgCO2e\nper bbl") +
  paletteer::scale_color_paletteer_c("viridis::plasma", direction = -1) +
  theme_line +
  theme(legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1, 'cm')) 
  

## compare production from setback scenarios
## ------------------------------------------------

setback_compare <- setback_out %>%
  filter(setback_scenario %in% c("setback_2500ft", "setback_5280ft")) %>%
  select(doc_field_code, doc_fieldname, year, upstream_kgCO2e_bbl, setback_scenario, total_prod_bbl) %>%
  pivot_wider(names_from = setback_scenario, values_from = total_prod_bbl) %>%
  mutate(diff = setback_5280ft - setback_2500ft)

## weight mean 2045 emission factor
avg_factor <- setback_out %>%
  filter(setback_scenario == "setback_5280ft",
         year == 2045) %>%
  group_by(year) %>%
  summarise(avg_ef = weighted.mean(upstream_kgCO2e_bbl, total_prod_bbl)) %>%
  ungroup()

setback_comp_fig <- ggplot(setback_compare, aes(x = year, y = diff / 1e6, group = doc_field_code, color = upstream_kgCO2e_bbl)) +
  geom_line(size = 0.5, alpha = 0.6) +
  labs(y = "production difference (million bbls)\n 5280ft vs 2500ft setback",
       x = NULL,
       color = "upstream kgCO2e\nper bbl") +
  # scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = avg_factor$avg_ef[1]) +
  paletteer::scale_color_paletteer_c("viridis::plasma", direction = -1) +
  theme_line +
  geom_hline(yintercept = 0, lty = "dashed", color = "black") +
  theme(legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  annotate("text", x = 2039, y = -5, label = "Midway-Sunset,\n~175 kgCO2e per bbl") 
# +
#   scale_x_continuous(expand = c(.5, .5)) +
#   geom_text(data = setback_compare %>% filter(year == 2045), 
#             aes(label = doc_fieldname), hjust=0, vjust=0)

# ggplotly(setback_comp_fig)

## save
ggsave(setback_comp_fig, 
       filename = paste0(calepa_path, results_path, save_path, "setback_avg_ghg_bbl.pdf"), 
       width = 6, 
       height = 6)

embed_fonts(paste0(calepa_path, results_path, save_path, "setback_avg_ghg_bbl.pdf"),
            outfile = paste0(calepa_path, results_path, save_path, "setback_avg_ghg_bbl.pdf"))


## plot cost vs GHG factors to explain average ghg emissions under quota scenarios (plot opex vs ghg factors)
## --------------------------------------------





