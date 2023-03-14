## Tracey Mangin
## November 28, 2020


library(tidyverse)

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

out_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/'
save_directory    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/figures/interim-report-figures/final/fuels-model/'

out <- read_csv(paste0(out_path, "predict-production/scenarios_20_all_scens/compiled/field_level_prod_emissions_2020-2045.csv"))

# load historic production
well_prod_org <- read_rds("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds") %>% as.data.table()

setnames(well_prod_org, "FieldCode", "doc_field_code")

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  mutate(api_ten_digit = substr(API, 1, 10)) 

# plot theme & palettes ------

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
        legend.margin = margin(t = 0, b = 0, unit = 'cm'),
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))


field_names <- wells_19 %>%
  select(FieldCode, FieldName) %>%
  unique() %>%
  rename(doc_field_name = FieldName,
         doc_field_code = FieldCode)

## historic production
well_prod19 <- well_prod_org %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = str_sub(doc_field_code, start= -3)) %>%
  group_by(doc_field_code, year) %>%
  summarise(total_bbls = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(field_names) %>%
  ## remove 000, remove gas
  filter(doc_field_code != "000") %>%
  mutate(gas = str_extract(doc_field_name, "Gas")) %>%
  filter(is.na(gas)) %>%
  select(-gas)

historic_prod <- well_prod19 %>%
  group_by(year) %>%
  summarise(prod_bbls = sum(total_bbls, na.rm = T)) %>%
  ungroup()

## projected production new and existing
pred_prod <- out %>%
  group_by(year, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario, well_type) %>%
  summarise(prod_bbls = sum(production_bbl, na.rm = T)) %>%
  ungroup()
  
## selected scenarios
pred_prod_scen <- pred_prod %>%
  filter(oil_price_scenario == "iea oil price",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario %in% c("no quota", "quota_20"),
         setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
  mutate(scenario_name = ifelse(setback_scenario == "no_setback" & prod_quota_scenario == "no quota", "BAU",
                                ifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "Extraction Scenario 1: Quota",
                                       ifelse(setback_scenario == "setback_2500ft" & prod_quota_scenario == "quota_20",  "Extraction Scenario 2: Quota + 2500ft setback", NA)))) %>%
  filter(!is.na(scenario_name)) %>%
  mutate(scenario = ifelse(scenario_name == "BAU", "BAU",
                           ifelse(scenario_name == "Extraction Scenario 1: Quota", "LCE1", "LCE2"))) %>%
  select(year, well_type, scenario, prod_bbls)

## all historic prod
all_his_prod <- expand.grid(scenario = unique(pred_prod_scen$scenario),
                            year = unique(historic_prod$year))

all_hist_prod2 <- all_his_prod %>%
  left_join(historic_prod) %>%
  mutate(well_type = "historic") %>%
  rbind(pred_prod_scen) %>%
  mutate(prod_type = ifelse(well_type == "historic", "Historic",
                            ifelse(well_type == "existing", "Existing wells", "New wells")))

# hist_2020_vals <- all_hist_prod2 %>%
#   filter(year == 2020) %>%
#   group_by(scenario, year) %>%
#   summarise(prod_bbls = sum(prod_bbls)) %>%
#   ungroup() %>%
#   mutate(well_type = "historic",
#          prod_type = "Historic")
# 
# all_hist_prod3 <- all_hist_prod2 %>%
#   filter(year != 2020) %>%
#   rbind(hist_2020_vals)

## 
all_hist_prod2$prod_type <- factor(all_hist_prod2$prod_type, levels = c("Historic",  "New wells", "Existing wells"))


prod_plot <-
  ggplot(all_hist_prod2, aes(x = year, y = prod_bbls / 1e6, fill = prod_type)) +
  geom_area() +
  geom_vline(xintercept = 2019, lty = "dashed", color = "black") +
  scale_fill_manual(values = ucsb_distinct) +
  scale_x_continuous(limits = c(1977, 2045), breaks=c(1977,seq(1985,2045,5))) +
  facet_wrap(~scenario, ncol = 1) +
  ylab("Crude oil extraction (million bbls)") +
  base_theme +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
ggsave(filename =  paste0(save_directory, "extraction-production-new-old-1977-2045.png"), prod_plot, width = 8, height = 10, units = "in", dpi = 300)

## remake 2019-2045
all_hist_prod3 <- all_hist_prod2 %>%
  filter(year >= 2019) %>%
  mutate(well_type = ifelse(well_type == "historic", "existing", well_type),
         prod_type = ifelse(prod_type == "Historic", "Existing wells",
                            ifelse(prod_type == "Existing wells", "Existing wells", "New wells")))

all_hist_prod4 <- expand.grid(scenario = c("BAU", "LCE1", "LCE2"),
                              year = c(2019),
                              well_type = c("new"),
                              prod_type = c("New wells"),
                              prod_bbls = c(0)) %>%
  rbind(all_hist_prod3)


prod_plot_2019 <- 
  ggplot(all_hist_prod4, aes(x = year, y = prod_bbls / 1e6, fill = prod_type)) +
  geom_area() +
  facet_wrap(~scenario, ncol = 1) +
  geom_vline(xintercept = 2019, lty = "dashed", color = "black") +
  scale_fill_manual(values = ucsb_distinct[2:3]) +
  scale_x_continuous(limits = c(2019, 2045), breaks=c(2019,seq(2025, 2045, 5))) +
  ylab("Crude oil extraction (million bbls)") +
  base_theme +
  theme_line +
  theme(legend.title=element_blank(),
        axis.title.x = element_blank())

ggsave(filename =  paste0(save_directory, "extraction-production-new-old-2019-2045.png"), prod_plot_2019, width = 8, height = 10, units = "in", dpi = 300)
ggsave(filename =  paste0(save_directory, "extraction-production-new-old-2019-2045.pdf"), prod_plot_2019, width = 8, height = 10, units = "in", dpi = 300)

embed_fonts(file.path(save_directory, 'extraction-production-new-old-2019-2045.pdf'),
            outfile = file.path(save_directory, 'extraction-production-new-old-2019-2045.pdf'))

## new and old wells production change
prod_diff <- all_hist_prod3 %>%
  select(scenario, year, well_type, prod_bbls) %>%
  filter(year == 2019 |  year == 2045) %>%
  mutate(year = paste0("X", year)) %>%
  pivot_wider(names_from = year,
              values_from = prod_bbls) %>%
  mutate(X2019 = ifelse(is.na(X2019), 0, X2019),
         diff = X2045 - X2019,
         rel_diff = diff / X2019)

