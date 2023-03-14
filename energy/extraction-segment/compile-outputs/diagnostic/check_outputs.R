## Tracey Mangin
## September 16, 2021
## Review labor outputs

## 
## libraries
library(data.table)
library(tidyverse)
library(readxl)
library(openxlsx)

## paths
main_path       <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
academic_out    <- paste0(main_path, 'outputs/academic-out/extraction')
# outputs_path    <- 'outputs/academic-out/extraction/extraction_2021-09-07/'
outputs_path    <- 'outputs/predict-production/extraction_2022-11-15/revision-sb-test/'
data_path       <-'data/stocks-flows/processed/'
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'
revision_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/nature-energy-rev-outputs'

## files
oil_price_file  <- 'oil_price_projections_revised.xlsx'
scen_file       <- 'scenario_id_list.csv'
scen_id_file      = 'scenario_id_list_targets.csv'


## oil prices
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1:4)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))


## labor file
total_multipliers_ext <- read_xlsx(paste0(main_path, labor_processed, 'ica_multipliers_v2.xlsx'), sheet = 'ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, 
         indi_emp_mult = indirect_emp, 
         indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, 
         indi_comp_mult = indirect_comp, 
         indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, 
         ip.indi_comp_mult = ip.indirect_comp, 
         ip.indu_comp_mult = ip.induced_comp)


## scenario list
# scen_list <- fread(file.path(academic_out, scen_file), header = T) 

# subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

## for testing
scen_id_list = fread(file.path(revision_path, scen_id_file), header = T)

subset_list <- scen_id_list[subset_scens == 1 & 
                                 oil_price_scenario == "reference case" & setback_scenario == "setback_2500ft"]


subset_ids <- subset_list[, scen_id]

## start function
## 1) read in rds for subset ids; 2) add to list 

field_out_list <- list()
county_out_list <- list()
state_out_list <- list()


for (i in 1:length(subset_ids)) {
  
  id_name_tmp <- subset_ids[i]
  
  field_out_tmp <- readRDS(paste0(main_path, outputs_path, 'field-results/', id_name_tmp, '_field_results.rds'))
  
  
  county_out_tmp <- readRDS(paste0(main_path, outputs_path, 'county-results/', id_name_tmp, '_county_results.rds'))
  
  
  state_out_tmp <- readRDS(paste0(main_path, outputs_path, 'state-results/', id_name_tmp, '_state_results.rds'))
  
  
  field_out_list[[i]] <- field_out_tmp
  county_out_list[[i]] <- county_out_tmp
  state_out_list[[i]] <- state_out_tmp
  
}

field_subset_all <- rbindlist(field_out_list)
county_subset_all <- rbindlist(county_out_list)
state_subset_all <- rbindlist(state_out_list)

## figures
## ---------------------

## employment x time by oil price

direct_emp_oil <- ggplot(state_subset_all, aes(x = year, y = c.dire_emp, color = oil_price_scenario, group = scen_id)) +
  geom_line(alpha = 0.8) +
  geom_line(data = state_subset_all %>% filter(oil_price_scenario == "reference case",
                                               innovation_scenario == "low innovation",
                                               carbon_price_scenario == "price floor",
                                               ccs_scenario == "medium CCS cost",
                                               setback_scenario == "no_setback",
                                               prod_quota_scenario == "no quota",
                                               excise_tax_scenario == "no tax"), aes(x = year, y = c.dire_emp), inherit.aes = F, color = "black") +
  labs(y = "Direct FTE",
       x = NULL) +
  annotate("text", x = 2025, y = 40000, label = "black line = BAU", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom")

# ggsave(direct_emp_oil, filename = paste0(main_path, outputs_path, 'diagnostics/labor_oil_px_fig.png'))
# 



ggplot(state_subset_all, aes(x = year, y = c.dire_emp, color = ccs_scenario, group = scen_id)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~oil_price_scenario) +
  geom_line(data = state_subset_all %>% filter(oil_price_scenario == "reference case",
                                               innovation_scenario == "low innovation",
                                               carbon_price_scenario == "price floor",
                                               ccs_scenario == "medium CCS cost",
                                               setback_scenario == "no_setback",
                                               prod_quota_scenario == "no quota",
                                               excise_tax_scenario == "no tax"), aes(x = year, y = c.dire_emp), inherit.aes = F, color = "black") +
  labs(y = "Direct FTE",
       x = NULL) +
  annotate("text", x = 2025, y = 40000, label = "black line = BAU", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom")


## revenue and labor
## ---------------------

rev_x_jobs <- ggplot(state_subset_all, aes(x = total_state_revenue / 1e9, y = c.dire_emp, color = oil_price_scenario)) +
  geom_point(alpha = 0.5) +
  geom_point(data = state_subset_all %>% filter(oil_price_scenario == "reference case",
                                               innovation_scenario == "low innovation",
                                               carbon_price_scenario == "price floor",
                                               ccs_scenario == "medium CCS cost",
                                               setback_scenario == "no_setback",
                                               prod_quota_scenario == "no quota",
                                               excise_tax_scenario == "no tax"), aes(x = total_state_revenue / 1e9, y = c.dire_emp), inherit.aes = F, color = "black") +
  labs(y = "Direct FTE",
       x = "Total state revenue (USD billions)") +
  # annotate("text", x = 2025, y = 40000, label = "black line = BAU", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom")


# ggsave(rev_x_jobs, filename = paste0(main_path, outputs_path, 'diagnostics/revenue_x_labor_fig.png'))


prod_x_jobs <- ggplot(state_subset_all, aes(x = total_state_bbl / 1e6, y = c.dire_emp, color = oil_price_scenario)) +
  geom_point(alpha = 0.5) +
  geom_point(data = state_subset_all %>% filter(oil_price_scenario == "reference case",
                                                innovation_scenario == "low innovation",
                                                carbon_price_scenario == "price floor",
                                                ccs_scenario == "medium CCS cost",
                                                setback_scenario == "no_setback",
                                                prod_quota_scenario == "no quota",
                                                excise_tax_scenario == "no tax"), aes(x = total_state_bbl / 1e6, y = c.dire_emp), inherit.aes = F, color = "black") +
  labs(y = "Direct FTE",
       x = "Total production (million bbls)") +
  # annotate("text", x = 2025, y = 40000, label = "black line = BAU", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom")


# ggsave(prod_x_jobs, filename = paste0(main_path, outputs_path, 'diagnostics/revenue_x_labor_fig.png'))

## cowplot combine
all_plots <- cowplot::plot_grid(direct_emp_oil, rev_x_jobs, prod_x_jobs, ncol = 1)

ggsave(all_plots, filename = paste0(main_path, outputs_path, 'diagnostics/laobr_fig.png'), width = 5, height = 11, units = "in")

