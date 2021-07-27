## Tracey Mangin
## May 28, 2020
## GHG emissions

library(tidyverse)
library(sf)
library(wesanderson)
library(plotly)
library(readxl)
library(RColorBrewer)
library(extrafont)
library(data.table)

## deal with the fonts
# font_import(pattern = "(?i)calibri") # load only calibri fonts
# 
# loadfonts(device = "pdf")

## source items
items <- list.files(here::here("src"))

walk(items, ~ here::here("src", .x) %>% source()) # load local items

## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
# save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/project-materials/focus-areas-1-2/"
save_directory2 <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

##  read in data
ghg_2011_2018 <- read_csv(paste0(data_directory, "ghg_mrr/ghg_mrr_2011-2018.csv"))  
ghg_2009_2010 <- read_csv(paste0(data_directory, "ghg_mrr/ghg_mrr_2009-2010.csv"))
ghg_2008 <- read_csv(paste0(data_directory, "ghg_mrr/ghg_mrr_2008.csv"))
inv_df <- read_csv(paste0(data_directory, "ghg_inventory_tool.csv"))


# ghg_sector_yr <- read_xlsx("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/2000_2017_ghg_inventory_trends_figures.xlsx", sheet = "Figure 3", range = "A5:T12")

## well prod
well_prod <- fread(paste0(data_directory, "well_prod_m_processed.csv"), colClasses =  c('doc_field_code' = 'character', 'api_ten_digit' = 'character')) 

## zip codes
zipcodes <- read_csv(paste0(data_directory, "zipcodes.csv"))

## fuel watch stock inputs, outputs, stores
fw_df <- read_csv(paste0(data_directory, "fuel_watch_data.csv")) 

## refinery source
refin_clusters <- read_csv(paste0(data_directory, "reg_refin_crude_receipts.csv"))

## process refining ghg
proc_refin_ghg <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/intermediary/refin_match_a.csv")

## bbls per  year
bbls_yr <- well_prod %>%
  group_by(year) %>%
  summarise(sum_prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

## refiniery site id
site_id <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/refinery_loc_cap.csv") %>%
  select(refinery_name, site_id)

##  how reliable is reported data
## ---------------------------------

# ael_df <- ghg_2011_2018 %>%
#   filter(AEL == "Yes") %>%
#   mutate(sum_rep = co2e_nb_ch4_n2o + co2_bf + fs_co2e + fs_co2_bf + elec_imp_co2e,
#          sum_calc = total_covered_emis + total_nocovered_emis,
#          diff = (sum_calc - sum_rep) / sum_rep)
# 
# unique(ael_df$NAICS)
# unique(ael_df$industry_sector)
# 
# ael_df2 <- ael_df %>%
#   filter(industry_sector %in% c("Refinery and Hydrogen Plant / Transportation Fuel Supplier / CO2 Supplier",
#                                 "Oil and Gas Production",
#                                 "Oil and Gas Production / Supplier of Natural Gas, NGL, or LPG",
#                                 "Supplier of Natural Gas, NGL, or LPG",
#                                 "Transportation Fuel Supplier"))
## off by -2% - 7%

# table(ghg_2009_2010$verif_overall_report)
# table(ghg_2008$report_sub_status)

## decision -- use total co2e. for most, this is reported data.
## --------------------------------------------------------------

total_ghg1 <- ghg_2011_2018 %>%
  select(ARB_ID, facility_name, facil_city, facil_state, facil_zip,
         NAICS, NAICS_code, industry_sector, report_yr, total_co2e, co2e_nb_ch4_n2o, co2_bf, AEL) %>%
  mutate(prim_report_sect = NA,
         sec_report_sect = NA)

total_ghg2 <- ghg_2009_2010 %>%
  mutate(co2e_nb_ch4_n2o = NA, 
         co2_bf = NA,
         industry_sector = NA,
         AEL = NA) %>%
  select(ARB_ID, facility_name, facil_city, facil_state, facil_zip, 
         NAICS, NAICS_code, industry_sector, report_yr, total_co2e, co2e_nb_ch4_n2o, co2_bf, AEL, prim_report_sect,
         sec_report_sect)

total_ghg3 <- ghg_2008 %>%
  mutate(co2e_nb_ch4_n2o = NA, 
         co2_bf = NA,
         industry_sector = NA,
         AEL = NA) %>%
  select(ARB_ID, facility_name, facil_city, facil_state, facil_zip, 
         NAICS, NAICS_code, industry_sector, report_yr, total_co2e, co2e_nb_ch4_n2o, co2_bf, AEL, prim_report_sect,
         sec_report_sect)

total_ghg_df <- rbind(total_ghg1, total_ghg2, total_ghg3)

## plot for NAICS categories
## ------------------------------------------

oil_vec <- c(211, 21111, 211111)
# oil_vec2 <- c(oil_vec, 324199, 424710, 424720, 454312)
## why the gap 20008-2010? Could these make up?
## 324199 All Other Petroleum and Coal Products Manufacturing (Other Combustion Source)
## 424710 Petroleum Bulk Stations and Terminals (Transportation Fuel Supplier)
## 424720 - Petroleum and Petroleum Products Merchant Wholesalers (except Bulk Stations and Terminals)
## 454312 - Liquefied Petroleum Gas (Bottled Gas) Dealers (Supplier of Natural Gas, NGL, or LPG)

refin_vec <- c(32411, 324110)

sf_ghg_df <- total_ghg_df %>%
  filter(NAICS_code %in% c(oil_vec, refin_vec)) %>%
  mutate(adj_total_co2e = ifelse(report_yr >= 2011, co2e_nb_ch4_n2o + co2_bf, total_co2e),
         cat = ifelse(NAICS_code %in% oil_vec, "Petroleum Extraction", "Petroleum Refineries"),
         diff = adj_total_co2e -  total_co2e)

# coal_df <- total_ghg_df %>%
#   filter(NAICS_code %in% c(oil_vec, 324199)) %>%
#   group_by(report_yr, NAICS_code) %>%
#   summarise(total_co2e = sum(total_co2e, na.rm = T)) %>%
#   ungroup()

# cdf2 <- expand.grid(report_yr = unique(coal_df$report_yr),
#                    NAICS_code = unique(coal_df$NAICS_code)) %>%
#   full_join(coal_df)

# ggplot(cdf2, aes(x = report_yr, y = total_co2e / 1e6, group = NAICS_code, fill = as.factor(NAICS_code))) +
#   geom_area() +
#   scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))

## quick plot
ggplot(sf_ghg_df, aes(x = report_yr, y = adj_total_co2e / 1e6, group = ARB_ID, color = as.factor(NAICS_code))) +
  geom_line(size = 0.75, alpha = 0.5) +
  facet_wrap(~cat, scales = "free_y") +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018))

## total emissions, compare with 2017 doc
sf_2017 <- sf_ghg_df %>%
  filter(report_yr == 2017) %>%
  group_by(cat) %>%
  summarise(sum_co2e = sum(adj_total_co2e)) %>%
  ungroup() %>%
  mutate(sum_co2e_m = sum_co2e / 1e6)

# ## state wide 424
# total_ca <- 424
# og_p <- 0.041
# ref_p <- 0.07
# og_e <- og_p * total_ca
# ref_e <- ref_p * total_ca
# 
# report <- tibble(report_val = c(og_e, ref_e))
# 
# sf_2017_2 <- cbind(sf_2017, report) 
# 
# ## for ref, 2017 sum
# total_ghg_df %>%
#   filter(report_yr == 2017) %>%
#   summarise(sum_co2e = sum(adj_total_co2e) / 1e6) %>%
#   as.numeric()
## 404.4

## summary -- refinery emssions appear to be much bigger than that reported in 
## the 2017 document

tot_annual <- total_ghg_df %>%
  group_by(report_yr) %>%
  summarise(ca_co2 = sum(total_co2e, na.rm = T)) %>%
  ungroup() %>%
  mutate(ca_co2_m = ca_co2 / 1e6) %>%
  select(report_yr, ca_co2_m)

## total emissions each year
sf_annual <- sf_ghg_df %>%
  group_by(cat, report_yr) %>%
  summarise(sum_co2e = sum(total_co2e)) %>%
  ungroup() %>%
  mutate(sum_co2e_m = sum_co2e / 1e6) %>%
  left_join(tot_annual) %>%
  mutate(rel_emis = sum_co2e_m / ca_co2_m)

## NOTE: the values from the report appear to match when querying the GHG emissions inventory
## https://www.arb.ca.gov/app/ghg/2000_2017/ghg_sector.php?_ga=2.105158595.1752297031.1586974429-502454941.1579050011
## Industrial --> Petroleum refining


## use inventory tool
## ----------------------------------------------------------

inv_df_all <- inv_df %>%
  group_by(year) %>%
  summarise(total_co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

inv_df2 <- inv_df %>%
  group_by(sub_sector_level_1, year) %>%
  summarise(total_co2e = sum(co2e, na.rm = T)) %>%
  ungroup() %>%
  mutate(cat = ifelse(sub_sector_level_1 == "Oil & Gas: Production & Processing", "Petroleum Extraction", "Petroleum Refineries")) %>%
  select(cat, report_yr = year, total_co2e) %>%
  left_join(sf_annual) %>%
  select(cat, report_yr, inv_tool_co2e = total_co2e, mrr_co2e = sum_co2e_m) %>%
  pivot_longer(inv_tool_co2e:mrr_co2e, names_to = "source", values_to = "co2e")

ggplot(inv_df2, aes(x = report_yr, y = co2e, color = source)) +
  geom_line(size = 1, alpha = 0.8) +
  facet_wrap(~cat) +
  scale_color_manual(name = "Data source: ",
                     values = c("blue", "darkgreen"),
                     labels = c("GHG Inventory Tool", "MRR")) +
  ylab("Mt CO2e") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

## oil and gas + refinery / total
## -------------------------------------------

## total emissions over time
yr_total_df <- janitor::clean_names(ghg_sector_yr) %>%
  pivot_longer(x2000:x2017, names_to = "year", values_to = "co2e") %>%
  mutate(year = as.numeric(str_remove(year, "x"))) %>%
  group_by(year, unit) %>%
  summarise(year_co2e = sum(co2e)) %>%
  ungroup()

ghg_2000 <- yr_total_df %>%
  filter(year == 2000) %>%
  select(year_co2e) %>%
  as.numeric()

ghg_2017 <- yr_total_df %>%
  filter(year == 2017) %>%
  select(year_co2e) %>%
  as.numeric()

ghg_delta <- calc_annual_change(init_val = ghg_2000, final_val = ghg_2017, init_yr = 2000, final_yr = 2017)


## sector total
sector_total <- inv_df2 %>%
  filter(source != "mrr_co2e") %>%
  group_by(report_yr) %>%
  summarise(co2e = sum(co2e)) %>%
  ungroup() %>%
  rename(year = report_yr) %>%
  left_join(yr_total_df) %>%
  mutate(rel_ghg = co2e / year_co2e) %>%
  select(year, co2e, year_co2e, rel_ghg, unit)





## MRR data vs. GHG Emissions Inventory
## ---------------------------------------

## MRR data
mrr_ghg_df <- sf_ghg_df %>%
  group_by(report_yr, cat) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = T),
            adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
  ungroup() %>%
  mutate(total_co2e = total_co2e / 1e6,
         adj_total_co2e = adj_total_co2e / 1e6,
         source = "MRR")

## GHG Emissions invetory tool
ghge_it_df <- inv_df %>%
  group_by(sub_sector_level_1, year) %>%
  summarise(total_co2e = sum(co2e, na.rm = T)) %>%
  ungroup() %>%
  mutate(cat = ifelse(sub_sector_level_1 == "Oil & Gas: Production & Processing", "Petroleum Extraction", "Petroleum Refineries")) %>%
  select(cat, report_yr = year, total_co2e) %>%
  mutate(adj_total_co2e = NA,
         source = "GHG Emissions Inventory Tool")

## rbind
annual_emission_df <- rbind(mrr_ghg_df, ghge_it_df) %>%
  select(report_yr, cat, source, total_co2e, adj_total_co2e) %>%
  pivot_longer(total_co2e:adj_total_co2e, names_to = "emission_type", values_to = "co2e") %>%
  mutate(label = ifelse(emission_type == "total_co2e", "Total", "Adjusted"))


## comparison figure
emissions_comp_fig <- ggplot(annual_emission_df, aes(x = report_yr, y = co2e, color = source, lty = label)) +
  geom_line(size = 1, alpha = 0.8) +
  facet_wrap(~cat) +
  scale_color_manual(name = "Data source: ",
                     values = c("blue", "darkgreen"),
                     labels = c("GHG Inventory Tool", "MRR")) +
  scale_linetype_manual(name = "Emissions:",
                        labels = c("Adjusted", "Total"),
                        values = c("dashed", "solid")) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
  ylab("Mt CO2e") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.key.width = unit(1,"cm"))


# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/emissions_comp_fig.png"), emissions_comp_fig, width = 12, height = 8, units = "in", dpi = 300)


## make summary dfs for each thing that we are interested in
## ----------------------------------------------------------------

## subsect 2
ghge_sub2_df <- inv_df %>%
  group_by(sub_sector_level_1, sub_sector_level_2, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

ggplot(ghge_sub2_df, aes(x = year, y = co2e, color = sub_sector_level_2)) +
  geom_line() +
  facet_wrap(~sub_sector_level_1)

## activity
ghge_activity_df <- inv_df %>%
  group_by(sub_sector_level_1, main_activity, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

ggplot(ghge_activity_df, aes(x = year, y = co2e, color = main_activity)) +
  geom_line() +
  facet_wrap(~sub_sector_level_1)

## ghg
ghge_ghg_df <- inv_df %>%
  group_by(sub_sector_level_1, ghg, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()


## table for converting
## SAR2 GWP / AR4 GWP

ghg_conv <- tibble(ghg = c("CH4", "CO2", "N2O"),
                   sar_gwp = c(21, 1, 310),
                   ar4_gwp = c(25, 1, 298)) %>%
  mutate(sar_ar4_ratio = sar_gwp / ar4_gwp)

ghge_ghg_df_conv <- ghge_ghg_df %>%
  left_join(ghg_conv) %>%
  mutate(adj_co2e = co2e * sar_ar4_ratio)

ghge_convt_df <- ghge_ghg_df_conv %>%
  group_by(sub_sector_level_1, year) %>%
  summarise(sum_co2e = sum(co2e),
            sum_adj_co2e = sum(adj_co2e)) %>%
  ungroup()

###
ghge_ghg_df2 <- ghge_ghg_df %>%
  group_by(sub_sector_level_1, year) %>%
  mutate(total_co2e = sum(co2e)) %>%
  ungroup() %>%
  mutate(rel = co2e / total_co2e)

ggplot(ghge_ghg_df, aes(x = year, y = co2e, color = ghg)) +
  geom_line() +
  facet_wrap(~sub_sector_level_1)



## total
ghge_total_df <- inv_df %>%
  group_by(sub_sector_level_1, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

ggplot(ghge_total_df, aes(x = year, y = co2e)) +
  geom_line() +
  facet_wrap(~sub_sector_level_1)


## GHG Inventory emissions
## ------------------------------------------

## gas type
gas_type_fig <-
ggplot(ghge_ghg_df, aes(x = year, y = co2e, fill = ghg)) +
  geom_area() +
  scale_fill_manual(breaks = c("CH4", "CO2", "N2O"),
                    labels = c(expression(CH[4]), expression(CO[2]), expression(paste(N[2], "O"))),
                    values = prim_calepa_pal) +
  facet_wrap(~sub_sector_level_1) +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  guides(color = guide_legend(nrow=2,byrow=TRUE)) +
  base_theme 

# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/gas_type_fig.png"), gas_type_fig, width = 8, height = 5, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/fig37.png"), gas_type_fig, width = 8, height = 5, units = "in", dpi = 300)


## total emissions by sector
ghgeinvt_fig <- 
ggplot(ghge_total_df, aes(x = year, y = co2e, color = sub_sector_level_1)) +
  geom_line(size = 1.5, alpha = 0.8) +
  ylab("Mt CO2e") +
  ylim(0, 35) +
  theme_bw() +
  guides(color = guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 25),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/ghge_inventory_total.png"), ghgeinvt_fig, width = 12, height = 12, units = "in", dpi = 300)


## total emissions by bbl, extraction
bbls_yr2 <- bbls_yr %>%
  mutate(sub_sector_level_1 = "Oil & Gas: Production & Processing") %>%
  rename(bbls = sum_prod)

## total emissions by bbl, refining
ca_refin_input <- fw_df %>%
  filter(stock_flow == "Refinery Input") %>%
  group_by(stock_flow, year) %>%
  summarise(bbls = sum(thous_barrels) * 1000) %>%
  ungroup()

ca_refin_source <- refin_clusters %>%
  group_by(year) %>%
  summarise(bbls = sum(bbls, na.rm = T)) %>%
  ungroup() %>%
  mutate(stock_flow = "source_df") %>%
  select(stock_flow, year, bbls)

ggplot(ca_refin_input %>% filter(year != 2020), aes(x = year, y = bbls / 1e6, color = stock_flow)) +
  geom_line()+
  geom_line(data = ca_refin_source %>% filter(year != min(ca_refin_source$year),
                                              year != max(ca_refin_source$year)), aes(x= year, y = bbls / 1e6))

## use ca_refin_source df
bbls_df3 <- ca_refin_source %>%
  mutate(sub_sector_level_1 = "Petroleum Refining and Hydrogen Production") %>%
  select(sub_sector_level_1, year, bbls) %>%
  rbind(bbls_yr2)

ghge_bbl <- ghge_total_df %>%
  left_join(bbls_df3) %>%
  rename(mtco2e = co2e) %>%
  mutate(co2e = mtco2e * 1e6,
         co2e_bbl = co2e / bbls,
         kg_co2e_bbl = co2e_bbl * 1000)


## total emissions per bbl by sector
emission_bbl_fig <- 
  ggplot(ghge_bbl, aes(x = year, y = kg_co2e_bbl, color = sub_sector_level_1)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_manual(values = prim_calepa_pal) +
  ylim(0, 100) +
  ylab(expression(paste("kg ", CO[2], "e per bbl"))) +
  base_theme +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) 

#ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/emission_bbl_fig.png"), emission_bbl_fig, width = 4, height = 4, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/fig38.png"), emission_bbl_fig, width = 4, height = 4, units = "in", dpi = 300)


## percent change start to end
bbl_e_diff <- ghge_bbl %>%
  filter(year %in% c(min(year), max(year))) %>%
  select(sub_sector_level_1, year, kg_co2e_bbl) %>%
  mutate(year = paste0("x", year)) %>%
  pivot_wider(names_from = year, values_from = kg_co2e_bbl) %>%
  mutate(diff = x2017 - x2000,
         rel_diff = diff / x2000) 





### --------------


ghge_oil_df <- inv_df %>%
  filter(sub_sector_level_1 == "Oil & Gas: Production & Processing") %>%
  group_by(sub_sector_level_1, sub_sector_level_2, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

ghge_oil_df2 <- inv_df %>%
  filter(sub_sector_level_1 == "Oil & Gas: Production & Processing") %>%
  group_by(sub_sector_level_1, main_activity, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

ghge_oil_df2 <- inv_df %>%
  filter(sub_sector_level_1 == "Oil & Gas: Production & Processing") %>%
  group_by(sub_sector_level_1, main_activity, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()


ghg_oil_fig <- ggplot(ghge_oil_df, aes(x = year, y = co2e, fill = sub_sector_level_2)) +
  geom_area() +
  labs(fill = "Sub-sector 2: ") +
  ggtitle("GHG Emissions Inventory: Oil & Gas: Production & Processing") +
  ylab("Mt CO2e") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 25),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/ghg_oil_fig.png"), ghg_oil_fig, width = 14, height = 8, units = "in", dpi = 300)


## MRR -----------------------------
## --------------------------

mrr_oil_df <- sf_ghg_df %>%
  filter(cat == "Petroleum Extraction") %>%
  rename(year = report_yr) %>%
  group_by(industry_sector, year) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = T),
            adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(total_co2e:adj_total_co2e, names_to = "emission_type", values_to = "co2e") %>%
  mutate(label = ifelse(emission_type == "total_co2e", "Total", "Adjusted"))

mrr_oil_exp <- expand.grid(industry_sector = c(unique(mrr_oil_df$industry_sector)),
                           year = c(unique(mrr_oil_df$year)),
                           label = c(unique(mrr_oil_df$label)))

mrr_oil_df2 <- mrr_oil_df %>%
  full_join(mrr_oil_exp) %>%
  mutate(co2e = ifelse(is.na(co2e), 0, co2e)) 

mrr_oil_df2$label <- factor(mrr_oil_df2$label, levels = c("Total", "Adjusted"))

## 2017 diff 
invt_2017_og <- 17.22314

mrr_oil_df2 %>% filter(year == 2017) %>%
  group_by(year, label) %>%
  summarise(sum = sum(co2e)) %>%
  ungroup() %>%
  mutate(diff = (sum / 1e6) - invt_2017_og)


mrr_oil_df2_adj <- mrr_oil_df %>%
  full_join(mrr_oil_exp) %>%
  mutate(co2e = ifelse(is.na(co2e), 0, co2e),
         industry_sector = ifelse(is.na(industry_sector), "No information", industry_sector)) %>%
  filter(industry_sector != "Supplier of Natural Gas, NGL, or LPG")



mrr_oil_fig <- ggplot(mrr_oil_df2_adj %>% filter(label == "Adjusted"), aes(x = year, y = co2e / 1e6, fill = industry_sector)) +
  geom_area() +
  geom_line(data = ghge_convt_df %>% filter(sub_sector_level_1 == "Oil & Gas: Production & Processing", year >= 2008), aes(x = year, y = sum_adj_co2e), color = "black", size = 1.5, inherit.aes = F) +
  # facet_wrap(~label, ncol = 2) +
  scale_fill_manual(values = prim_calepa_pal,
                    na.value = prim_calepa_pal[4]) +
  annotate("text", x = 2010, y = 15, size = 5, label = "Total from GHG Emissions\nInventory Tool", family = "Calibri") +
  labs(fill = "Industry sector: ") +
  ggtitle("Oil & Gas: Production & Processing\nMRR NAICS codes 211, 21111, 211111") +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  base_theme +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12))
    

# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_oil_fig.png"), mrr_oil_fig, width = 12, height = 8, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/figa1.png"), mrr_oil_fig, width = 8, height = 6, units = "in", dpi = 300)


## by company
## ---------------------------------

mrr_oil_df_facil <- sf_ghg_df %>%
  filter(cat == "Petroleum Extraction") %>%
  rename(year = report_yr) 
  # group_by(industry_sector, year) %>%
  # summarise(total_co2e = sum(total_co2e, na.rm = T),
  #           adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
  # ungroup() %>%
  # pivot_longer(total_co2e:adj_total_co2e, names_to = "emission_type", values_to = "co2e") %>%
  # mutate(label = ifelse(emission_type == "total_co2e", "Total", "Adjusted"))

mrr_oil_exp_facil <- expand.grid(facility_name = c(unique(mrr_oil_df_facil$facility_name)),
                           year = c(unique(mrr_oil_df_facil$year)))

mrr_oil_df_facil2 <- mrr_oil_df_facil %>%
  full_join(mrr_oil_exp_facil) %>%
  mutate(adj_total_co2e = ifelse(is.na(adj_total_co2e), 0, adj_total_co2e))

levels <- mrr_oil_df_facil2 %>% 
  filter(year == 2018) %>%
  arrange(-adj_total_co2e) %>%
  mutate(rank = rank(-adj_total_co2e)) %>%
  filter(rank <= 10) %>%
  arrange(-rank) %>%
  mutate(facility_name = as.character(facility_name)) %>%
  select(facility_name, rank)


mrr_oil_df_facil3 <- mrr_oil_df_facil2 %>%
  mutate(adj_name = ifelse(facility_name %in% levels$facility_name, as.character(facility_name), "Other")) %>%
  group_by(year, adj_name) %>%
  summarise(adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
  ungroup() 


labs <- mrr_oil_df_facil3 %>%
  filter(year == 2018) %>%
  arrange(adj_total_co2e) %>%
  mutate(cumsum = cumsum(adj_total_co2e),
         adj_name = as.character(adj_name))

labs2 <- mrr_oil_df_facil3 %>%
  filter(year == 2018) %>%
  arrange(-adj_total_co2e) %>%
  mutate(cumsum = cumsum(adj_total_co2e),
         adj_name = as.character(adj_name))

mrr_oil_df_facil3$adj_name <- factor(mrr_oil_df_facil3$adj_name, levels = labs$adj_name)

mrr_oil_facil_fig <- ggplot(mrr_oil_df_facil3, aes(x = year, y = adj_total_co2e / 1e6, fill = adj_name)) +
  geom_area() +
  scale_fill_manual(values = calepa_pal) +
  # geom_label(data = labs2, aes(x = year - 1, y = (cumsum / 1e6), label = adj_name)) +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  geom_line(data = ghge_convt_df %>% filter(sub_sector_level_1 == "Oil & Gas: Production & Processing", year >= 2008), aes(x = year, y = sum_adj_co2e), color = "black", size = 1.5, inherit.aes = F) +
  annotate("text", x = 2011, y = 18, size = 5, label = "Total from GHG Emissions\nInventory Tool", family = "Calibri") +
  # geom_text(data = levels, aes(x = year - 4, y = (adj_total_co2e / 1e6) - 1, label = lab), color = "white") +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  guides(fill = guide_legend(nrow = 6, byrow = TRUE, reverse = T)) +
  # geom_text(show.legend = FALSE) +
  base_theme +
  theme(legend.text = element_text(size = 13, family = "Calibri"),
        axis.title.y = element_text(size = 14, family = "Calibri"),
        axis.text = element_text(size = 13, family = "Calibri"))
 
# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_oil_facil_fig.png"), mrr_oil_facil_fig, width = 9, height = 6, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/fig41.png"), mrr_oil_facil_fig, width = 10, height = 8, units = "in", dpi = 300)



perc_facil_og_2018 <- mrr_oil_df_facil3 %>%
  filter(year == 2018) %>%
  mutate(sum = sum(adj_total_co2e),
         ratio = adj_total_co2e / sum)




## refineries ---------------------------------
## ------------------------------------------

ghge_ref_df <- inv_df %>%
  filter(sub_sector_level_1 == "Petroleum Refining and Hydrogen Production") %>%
  group_by(sub_sector_level_1, sub_sector_level_2, year) %>%
  summarise(co2e = sum(co2e, na.rm = T)) %>%
  ungroup()

ghg_ref_fig <- ggplot(ghge_ref_df, aes(x = year, y = co2e, fill = sub_sector_level_2)) +
  geom_area() +
  labs(fill = "Sub-sector 2: ") +
  ggtitle("Petroleum Refining and Hydrogen Production") +
  ylab("Mt CO2e") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 25),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/ghg_ref_fig.png"), ghg_ref_fig, width = 12, height = 8, units = "in", dpi = 300)

## mrr refineries ----------------------------------------
## -------------------------------------------------------

mrr_ref_df <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries") %>%
  rename(year = report_yr) %>%
  group_by(industry_sector, year) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = T),
            adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(total_co2e:adj_total_co2e, names_to = "emission_type", values_to = "co2e") %>%
  mutate(label = ifelse(emission_type == "total_co2e", "Total", "Adjusted"))

mrr_ref_exp <- expand.grid(industry_sector = c(unique(mrr_ref_df$industry_sector)),
                           year = c(unique(mrr_ref_df$year)),
                           label = c(unique(mrr_ref_df$label)))

mrr_ref_df2 <- mrr_ref_df %>%
  full_join(mrr_ref_exp) %>%
  mutate(co2e = ifelse(is.na(co2e), 0, co2e)) 

mrr_ref_df2$label <- factor(mrr_ref_df2$label, levels = c("Total", "Adjusted"))

mrr_ref_df2_adj <- mrr_ref_df %>%
  full_join(mrr_ref_exp) %>%
  mutate(co2e = ifelse(is.na(co2e), 0, co2e)) %>%
  filter(label == "Adjusted") %>%
  mutate(industry_sector = ifelse(is.na(industry_sector), "No information", industry_sector)) %>%
  group_by(industry_sector) %>%
  mutate(sector_sum = sum(co2e)) %>%
  ungroup() %>%
  filter(sector_sum > 0)


## 2017 diff 
invt_2017_ref <- 29.81505

mrr_ref_df2 %>% filter(year == 2017) %>%
  group_by(year, label) %>%
  summarise(sum = sum(co2e)) %>%
  ungroup() %>%
  mutate(diff = (sum / 1e6) - invt_2017_ref)


mrr_ref_fig <- ggplot(mrr_ref_df2_adj, aes(x = year, y = co2e / 1e6, fill = industry_sector)) +
  geom_area(stat = "identity") +
  scale_fill_manual(values = rev(c(calepa_pal[5], calepa_pal[7:12])),
                    na.value = calepa_pal[1]) +
  geom_line(data = ghge_convt_df %>% filter(sub_sector_level_1 == "Petroleum Refining and Hydrogen Production", year >= 2008), aes(x = year, y = sum_adj_co2e), color = "black", size = 1.5, inherit.aes = F) +
  # facet_wrap(~label, ncol = 2) +
  annotate("text", x = 2012, y = 34, size = 4, label = "Total from GHG Emissions\nInventory Tool") +
  labs(fill = "Industry sector: ") +
  ggtitle("MRR: Petroleum Refining and Hydrogen Production\nMRR NAICS codes 324110, 32411") +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  guides(fill = guide_legend(nrow = 9, byrow = TRUE)) +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  base_theme +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 15, family = "Calibri"),
        strip.text = element_text(size = 15, family = "Calibri"),
        axis.title.y = element_text(size = 15, family = "Calibri"),
        axis.title = element_text(size = 15, family = "Calibri"),
        legend.text = element_text(size = 15, family = "Calibri"),
        plot.title = element_text(size = 16, family = "Calibri"),
        legend.title = element_text(size = 15, family = "Calibri"))


# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_fig.png"), mrr_ref_fig, width = 12, height = 10, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/figa2.png"), mrr_ref_fig, width = 10, height = 8, units = "in", dpi = 300)


## mrr - remove cogeneration
## -------------------------------

mrr_ref_df_cf <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries",
         !sec_report_sect %in% c("Cogeneration Facility", "Hydrogen Plant, Cogeneration Facility", "Hydrogen Plant, Cogeneration Facility, Electricity Generation")) %>%
  rename(year = report_yr) %>%
  group_by(industry_sector, year) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = T),
            adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(total_co2e:adj_total_co2e, names_to = "emission_type", values_to = "co2e") %>%
  mutate(label = ifelse(emission_type == "total_co2e", "Total", "Adjusted"))

mrr_ref_exp_cf <- expand.grid(industry_sector = c(unique(mrr_ref_df_cf$industry_sector)),
                              year = c(unique(mrr_ref_df_cf$year)),
                              label = c(unique(mrr_ref_df_cf$label)))

mrr_ref_df_cf2 <- mrr_ref_df_cf %>%
  full_join(mrr_ref_exp_cf) %>%
  mutate(co2e = ifelse(is.na(co2e), 0, co2e)) 

mrr_ref_df_cf2$label <- factor(mrr_ref_df_cf2$label, levels = c("Total", "Adjusted"))


mrr_ref_cf_fig <- ggplot(mrr_ref_df_cf2, aes(x = year, y = co2e / 1e6, fill = industry_sector)) +
  geom_area(stat = "identity") +
  geom_line(data = ghge_it_df %>% filter(cat == "Petroleum Refineries", report_yr >= 2007), aes(x = report_yr, y = total_co2e), color = "black", size = 1.5, inherit.aes = F) +
  facet_wrap(~label, ncol = 2) +
  annotate("text", x = 2014, y = 42, size = 5, label = "Total from GHG Emissions\nInventory Tool") +
  labs(fill = "Industry sector: ") +
  ggtitle("MRR: Petroleum Refining and Hydrogen Production\nMRR NAICS codes 324110, 32411") +
  ylab("Mt CO2e") +
  guides(fill = guide_legend(nrow = 9, byrow = TRUE)) +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 25),
        strip.text = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_fig.png"), mrr_ref_fig, width = 12, height = 10, units = "in", dpi = 300)


###########
###########

## by company
## ---------------------------------

mrr_ref_df_facil <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries") %>%
  rename(year = report_yr) 
# group_by(industry_sector, year) %>%
# summarise(total_co2e = sum(total_co2e, na.rm = T),
#           adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
# ungroup() %>%
# pivot_longer(total_co2e:adj_total_co2e, names_to = "emission_type", values_to = "co2e") %>%
# mutate(label = ifelse(emission_type == "total_co2e", "Total", "Adjusted"))

## use matched version
proc_refin_ghg2 <- proc_refin_ghg %>%
  # filter(!is.na(industry_sector)) %>%
  mutate(new_id = ifelse(is.na(site_id), ARB_ID, site_id))
  
  mrr_ref_exp_facil <- expand.grid(site_id = c(unique(proc_refin_ghg2$new_id)),
                                 year = c(unique(proc_refin_ghg2$year)))

  
  ## save for manual matching
  mm_ref_df <- mrr_ref_df_facil2 %>%
    mutate(adj_name = ifelse(facility_name %in% levelsr$facility_name, as.character(facility_name), "Other"))
  
  # write.csv(mm_ref_df, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/intermediary/refin_match_a.csv")
  
  
proc_refin_ghg3 <- proc_refin_ghg2 %>%
  full_join(mrr_ref_exp_facil) %>%
  filter(!is.na(adj_total_co2e))



##  names
name_df <- proc_refin_ghg3 %>%
  select(facility_name_adj, facility_name, site_id, ARB_ID, adj_name) %>%
  unique()



## expand grid
proc_refin_exp <- expand.grid(facility_name_adj = c(unique(proc_refin_ghg3$facility_name_adj)),
                              year = unique(proc_refin_ghg3$year))

proc_refin_ghg4 <- proc_refin_ghg3 %>%
  full_join(proc_refin_exp) %>%
  mutate(adj_total_co2e = ifelse(is.na(adj_total_co2e), 0 , adj_total_co2e)) %>%
  filter(!is.na(facility_name_adj)) %>%
  mutate(facility_name_adj = ifelse(facility_name_adj == "Phillips 66 Company - Los Angeles Refinery", "Phillips 66 Company Los Angeles Refinery", facility_name_adj))

levs <- proc_refin_ghg4 %>%
  filter(year == max(year)) %>%
  arrange(-adj_total_co2e) %>%
  filter()

updated_names <- tibble(facility_name_adj = c("Tesoro Refining & Marketing Company LLC (Carson)", 
                                             "Tesoro (Wilmington)",
                                             "Chevron Products Company - El Segundo Refinery",
                                             "Chevron Products Company - Richmond Refinery", 
                                             "Tesoro Refining and Marketing Co. (Martinez)", 
                                             "Torrance Refinery", 
                                             "Shell Oil Products US (Martinez)", 
                                             "Valero Refining Co (Benicia)", 
                                             "Phillips 66 Company Los Angeles Refinery",        
                                             "Phillips 66 Company (Rodeo)", 
                                             "Phillips 66 Company - Santa Maria Refinery", 
                                             "Ultramar Inc - Valero",
                                             "Kern Oil and Refining Company", 
                                             "San Joaquin Refining Company", 
                                             "Greka Santa Maria Refinery",
                                             "Lunday-Thagard Company",
                                             "Valero Refining Co (Wilmington, Asphalt Plant)", 
                                             "Tricor Refining, LLC", 
                                             "Alon Bakersfield Refinery - Areas 1&2",
                                             "Alon Bakersfield Refinery - Area 3", 
                                             "Paramount Petroleum Corporation", 
                                             "Edgington Oil Company"),
                        updated_name = c("Marathon Petroleum Corp., Carson Refinery",
                                         "Marathon Petroleum Corp., Wilmington Refinery",
                                         "Chevron U.S.A. Inc., El Segundo Refinery",
                                         "Chevron U.S.A. Inc., Richmond Refinery",
                                         "Marathon Petroleum Corp., Golden Eagle Martinez Refinery",
                                         "PBF Energy, Torrance Refinery",
                                         "PBF Energy, Martinez Refinery",
                                         "Valero Energy, Benicia Refinery",
                                         "Phillips 66, Wilmington Refinery",
                                         "Phillips 66, Rodeo San Francisco Refinery",
                                         "Phillips 66, Santa Maria Refinery",
                                         "Valero Energy, Wilmington Refinery",
                                         "Kern Oil & Refining Company, Bakersfield Refinery",
                                         "San Joaquin Refining Company Inc., Bakersfield Refinery",
                                         "Greka Energy, Santa Maria Refinery",
                                         "Lunday Thagard, South Gate Refinery",
                                         "Valero Wilmington Asphalt Refinery",
                                         "Tricor Refining, LLC (Closed)",
                                         "Alon Bakersfield Refinery - Areas 1&2",
                                         "Alon Bakersfield Refinery - Area 3",
                                         "Paramount Petroleum Corporation (Delek, Idle)",
                                         "Edgington Oil Company (Delek, Idle)"))


proc_refin_ghg5 <- proc_refin_ghg4 %>%
  group_by(facility_name_adj, year) %>%
  summarise(adj_total_co2e = sum(adj_total_co2e)) %>%
  ungroup() %>%
  left_join(updated_names)


write_csv(proc_refin_ghg5, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/refinery_name_adjustments.csv")


# proc_refin_ghg5_id <- proc_refin_ghg4 %>%
#   group_by(facility_name_adj, site_id, year) %>%
#   summarise(adj_total_co2e = sum(adj_total_co2e)) %>%
#   ungroup() %>%
#   left_join(updated_names)
# 
# write_csv(proc_refin_ghg5, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/refinery_name_adjustments.csv")



updated_names_plotting <- tibble(updated_name = c("Marathon Petroleum Corp., Carson Refinery",
                                         "Marathon Petroleum Corp., Wilmington Refinery",
                                         "Chevron U.S.A. Inc., El Segundo Refinery",
                                         "Chevron U.S.A. Inc., Richmond Refinery",
                                         "Marathon Petroleum Corp., Golden Eagle Martinez Refinery",
                                         "PBF Energy, Torrance Refinery",
                                         "PBF Energy, Martinez Refinery",
                                         "Valero Energy, Benicia Refinery",
                                         "Phillips 66, Wilmington Refinery",
                                         "Phillips 66, Rodeo San Francisco Refinery",
                                         "Phillips 66, Santa Maria Refinery",
                                         "Valero Energy, Wilmington Refinery",
                                         "Kern Oil & Refining Company, Bakersfield Refinery",
                                         "San Joaquin Refining Company Inc., Bakersfield Refinery",
                                         "Greka Energy, Santa Maria Refinery",
                                         "Lunday Thagard, South Gate Refinery",
                                         "Valero Wilmington Asphalt Refinery",
                                         "Tricor Refining, LLC (Closed)",
                                         "Alon Bakersfield Refinery - Areas 1&2",
                                         "Alon Bakersfield Refinery - Area 3",
                                         "Paramount Petroleum Corporation (Delek, Idle)",
                                         "Edgington Oil Company (Delek, Idle)"),
                                 updated_name2 = c("Marathon Petroleum Corp., Carson Refinery",
                                                  "Marathon Petroleum Corp., Wilmington Refinery",
                                                  "Chevron U.S.A. Inc., El Segundo Refinery",
                                                  "Chevron U.S.A. Inc., Richmond Refinery",
                                                  "Marathon Petroleum Corp., Golden Eagle Martinez Refinery",
                                                  "PBF Energy, Torrance Refinery",
                                                  "PBF Energy, Martinez Refinery",
                                                  "Valero Energy, Benicia Refinery",
                                                  "Phillips 66, Wilmington Refinery",
                                                  "Phillips 66, Rodeo San Francisco Refinery",
                                                  "Phillips 66, Santa Maria Refinery",
                                                  "Valero Energy, Wilmington Refinery",
                                                  "Kern Oil & Refining Co., Bakersfield Refinery",
                                                  "San Joaquin Refining Co. Inc., Bakersfield Refinery",
                                                  "Greka Energy, Santa Maria Refinery",
                                                  "Lunday Thagard, South Gate Refinery",
                                                  "Valero Wilmington Asphalt Refinery",
                                                  "Tricor Refining LLC (Closed)",
                                                  "Alon Bakersfield Refinery - Areas 1&2",
                                                  "Alon Bakersfield Refinery - Area 3",
                                                  "Paramount Petroleum Corp. (Delek, Idle)",
                                                  "Edgington Oil Co. (Delek, Idle)"))

proc_refin_ghg6 <- proc_refin_ghg5 %>%
  left_join(updated_names_plotting)

proc_refin_ghg6$updated_name2 <- factor(proc_refin_ghg6$updated_name2, rev(updated_names_plotting$updated_name2))


# levelsr <- proc_refin_ghg2 %>% 
#   filter(year == 2018) %>%
#   arrange(-adj_total_co2e) %>%
#   mutate(rank = rank(-adj_total_co2e)) %>%
#   arrange(-rank) 
# 
# fig_labs <- levelsr %>%
#   filter(rank <= 10) %>%
#   mutate(name_lab = ifelse(is.na(facility_name_adj), facility_name, facility_name_adj)) %>%
#   select(ARB_ID, name_lab, year, adj_total_co2e) 
# 
# mrr_ref_df_facil3 <- proc_refin_ghg3 %>%
#   mutate(adj_name = ifelse(ARB_ID %in% fig_labs$ARB_ID, as.character(facility_name_adj), "Other")) %>%
#   group_by(year, adj_name) %>%
#   summarise(adj_total_co2e = sum(adj_total_co2e, na.rm = T)) %>%
#   ungroup() 
# 
# labsr <- mrr_ref_df_facil3 %>%
#   filter(year == 2018) %>%
#   arrange(adj_total_co2e) %>%
#   mutate(cumsum = cumsum(adj_total_co2e),
#          adj_name = as.character(adj_name))
# 
# labs2r <- mrr_ref_df_facil3 %>%
#   filter(year == 2018) %>%
#   arrange(-adj_total_co2e) %>%
#   mutate(cumsum = cumsum(adj_total_co2e),
#          adj_name = as.character(adj_name))
# 
# mrr_ref_df_facil3$adj_name <- factor(mrr_ref_df_facil3$adj_name, levels = labsr$adj_name)

nb_cols <- nrow(updated_names)
mycolors <- colorRampPalette(calepa_pal)(nb_cols)


mrr_ref_facil_fig <- ggplot(proc_refin_ghg6, aes(x = year, y = adj_total_co2e / 1e6, fill = updated_name2)) +
  geom_area() +
  geom_line(data = ghge_convt_df %>% filter(sub_sector_level_1 == "Petroleum Refining and Hydrogen Production", year >= 2008), aes(x = year, y = sum_adj_co2e), color = "black", size = 1.5, inherit.aes = F) +
  annotate("text", x = 2013, y = 31, size = 7, label = "Total from GHG Emissions Inventory Tool", family = "Calibri") +
  # geom_label(data = labs2r, aes(x = year - 1, y = (cumsum / 1e6), label = adj_name)) +
  # geom_text(data = levels, aes(x = year - 4, y = (adj_total_co2e / 1e6) - 1, label = lab), color = "white") +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  scale_fill_manual(values = mycolors) +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  # geom_text(show.legend = FALSE) +
  guides(fill = guide_legend(nrow = 11, byrow = TRUE, reverse = TRUE)) +
  base_theme +
  theme(axis.text = element_text(size = 18, family = "Calibri"),
        axis.title.y = element_text(size = 18, family = "Calibri"),
        legend.text = element_text(size = 18, family = "Calibri"))

# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_facil_fig.png"), mrr_ref_facil_fig, width = 12, height = 10, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/fig44.png"), mrr_ref_facil_fig, width = 14, height = 10, units = "in", dpi = 300)


## save emissions file for Meas

proc_refin_ghg7 <- proc_refin_ghg6 %>%
  mutate(adj_total_Mt_co2e = adj_total_co2e / 1e6) %>%
  select(facility_name_adj, refinery_name = updated_name, updated_name2, year, adj_total_co2e, adj_total_Mt_co2e) %>%
  left_join(site_id) %>%
  mutate(site_id = ifelse(facility_name_adj == "Shell Oil Products US (Martinez)", 279, site_id))

write_csv(proc_refin_ghg7, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/refinery_ghg_emissions.csv")



# ### 
# mrr_ref_facil_fig <- ggplot(mrr_ref_df_facil3, aes(x = year, y = adj_total_co2e / 1e6, fill = adj_name)) +
#   geom_area() +
#   geom_line(data = ghge_it_df %>% filter(cat == "Petroleum Refineries", report_yr >= 2008), aes(x = report_yr, y = total_co2e), color = "black", size = 1.5, inherit.aes = F) +
#   annotate("text", x = 2013, y = 28, size = 5, label = "Total from GHG Emissions\nInventory Tool") +
#   geom_label(data = labs2r, aes(x = year - 1, y = (cumsum / 1e6), label = adj_name)) +
#   # geom_text(data = levels, aes(x = year - 4, y = (adj_total_co2e / 1e6) - 1, label = lab), color = "white") +
#   scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
#   ylab("Mt CO2e") +
#   # geom_text(show.legend = FALSE) +
#   theme_bw() +
#   theme(legend.position = "none",
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 20),
#         axis.text = element_text(size = 20),
#         plot.title = element_text(size = 25),
#         strip.text = element_text(size = 20),
#         legend.text = element_text(size = 20),
#         legend.title = element_text(size = 20))
# 
# 
# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_facil_fig.png"), mrr_ref_facil_fig, width = 12, height = 8, units = "in", dpi = 300)
# 





## 
perc_facil_ref_2018 <- proc_refin_ghg4 %>%
  filter(year == 2018) %>%
  mutate(sum = sum(adj_total_co2e),
         ratio = adj_total_co2e / sum) %>%
  arrange(-adj_total_co2e) %>%
  mutate(cumsum = cumsum(ratio))


### emissions by county -- refining
## ----------------------------------------

zipcodes2 <- zipcodes %>%
  rename(facil_zip =  `Zip Code`, county = "County") %>%
  select(facil_zip, county) %>%
  mutate(facil_zip = as.character(facil_zip))

## use proc_refin_ghg5 and proc_refin_ghg4

refin_zip_df <- proc_refin_ghg4 %>%
  group_by(facility_name_adj, facil_zip, year) %>%
  summarise(adj_total_co2e = sum(adj_total_co2e)) %>%
  ungroup() %>%
  mutate(facil_zip = as.character(facil_zip)) %>%
  left_join(updated_names) %>%
  left_join(zipcodes2) %>%
  arrange(facility_name_adj, year) %>%
  fill(facil_zip) %>%
  fill(county)


write_csv(refin_zip_df, "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/refinery_co2e.csv")


####
mrr_ref_df_county <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries") %>%
  rename(year = report_yr) %>%
  left_join(zipcodes2) %>%
  group_by(county, year) %>%
  summarise(adj_total_co2e = sum(adj_total_co2e))

mrr_ref_exp_county <- expand.grid(county = c(unique(mrr_ref_df_county$county)),
                                 year = c(unique(mrr_ref_df_county$year)))

mrr_ref_df_county2 <- mrr_ref_df_county %>%
  full_join(mrr_ref_exp_county) %>%
  mutate(adj_total_co2e = ifelse(is.na(adj_total_co2e), 0, adj_total_co2e))

levels_county <- mrr_ref_df_county2 %>% 
  filter(year == 2018) %>%
  arrange(adj_total_co2e) %>%
  mutate(cumsum = cumsum(adj_total_co2e))



mrr_ref_df_county2$county <- factor(mrr_ref_df_county2$county, levels = c(NA, "Santa Barbara", "San Luis Obispo", "Kern", 
                                                                          "Solano", "Contra Costa", "Los Angeles"))

mrr_ref_county_fig <- ggplot(mrr_ref_df_county2 %>% filter(!is.na(county)), aes(x = year, y = adj_total_co2e / 1e6, fill = county)) +
  geom_area() +
  scale_fill_manual(values = rev(prim_calepa_pal)) +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  # geom_label(data = levels_county, aes(x = year - 0.5, y = (cumsum / 1e6), label = adj_name)) +
  # geom_text(data = levels, aes(x = year - 4, y = (adj_total_co2e / 1e6) - 1, label = lab), color = "white") +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = T)) +
  # geom_text(show.legend = FALSE) +
  base_theme 


# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_county_fig.png"), mrr_ref_county_fig, width = 8, height = 6, units = "in", dpi = 300)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/final/stocks-flows/fig45.png"), mrr_ref_county_fig, width = 6, height = 4, units = "in", dpi = 300)




perc_county_ref_2018 <- mrr_ref_df_county2 %>%
  filter(year == 2018) %>%
  group_by(year) %>%
  mutate(sum = sum(adj_total_co2e)) %>%
  ungroup() %>%
  mutate(ratio = adj_total_co2e / sum) %>%
  arrange(-ratio) %>%
  mutate(cumsum = cumsum(ratio))

perc_ns_ref_2018 <- mrr_ref_df_county2 %>%
  filter(year == 2018) %>%
  mutate(region = ifelse(county %in% c("Kern", "Contra Costa", "Solano"), "North", "South")) %>%
  group_by(region, year) %>%
  summarise(reg_sum = sum(adj_total_co2e)) %>%
  ungroup() %>%
  mutate(sum = sum(reg_sum),
         ratio = reg_sum / sum) 











# ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_oil_fig.png"), mrr_oil_fig, width = 12, height = 8, units = "in", dpi = 300)








############
###########




mrr_ref_df2_filt <- mrr_ref_df2 %>%
  filter(!industry_sector %in% c("Transportation Fuel Supplier", "Electricity Importer")) %>%
  mutate(industry_sector = ifelse(is.na(industry_sector), "NA", industry_sector))



mrr_ref_fig_filt <- ggplot(mrr_ref_df2_filt, aes(x = year, y = total_co2e / 1e6, fill = industry_sector)) +
  geom_area(stat = "identity") +
  geom_line(data = ghge_it_df %>% filter(cat == "Petroleum Refineries", report_yr >= 2007), aes(x = report_yr, y = total_co2e), color = "black", size = 1.5, inherit.aes = F) +
  annotate("text", x = 2017, y = 22, size = 5, label = "Total from GHG Emissions\nInventory Tool") +
  labs(fill = "Industry sector: ") +
  ggtitle("MRR: Oil & Gas: NAICS codes 324110, 32411\nRemoved 'Electricity Importer' & 'Transporation Fuel Supplier'") +
  ylab("Mt CO2e") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(nrow = 9, byrow = TRUE)) +
  scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 25),
        strip.text = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_filt_fig.png"), mrr_ref_fig_filt, width = 12, height = 10, units = "in", dpi = 300)

### by company

write_csv(sf_ghg_df, paste0(save_directory, "mrr_ghg_df.csv"))

mrr_ref_facil_df <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries") %>%
  rename(year = report_yr) %>%
  group_by(facility_name, year) %>%
  summarise(total_co2e = sum(total_co2e, na.rm = T)) %>%
  ungroup() 

mrr_ref_facil_exp <- expand.grid(facility_name = c(unique(mrr_ref_facil_df$facility_name)),
                                 year = c(unique(mrr_ref_df$year)))

mrr_ref_facil_df2 <- mrr_ref_facil_df %>%
  full_join(mrr_ref_facil_exp) %>%
  mutate(total_co2e = ifelse(is.na(total_co2e), 0, total_co2e)) 


mrr_ref_facil_fig <- ggplot(mrr_ref_facil_df2, aes(x = year, y = total_co2e / 1e6, fill = facility_name)) +
  geom_area(stat = "identity") +
  geom_line(data = ghge_it_df %>% filter(cat == "Petroleum Refineries", report_yr >= 2007), aes(x = report_yr, y = total_co2e), color = "black", size = 1.5, inherit.aes = F) +
  annotate("text", x = 2017, y = 22, size = 5, label = "Total from GHG Emissions\nInventory Tool") +
  ggtitle("MRR: Oil & Gas: NAICS codes 324110, 32411") +
  ylab("Mt CO2e") +
  guides(fill = guide_legend(nrow = 9, byrow = TRUE) +
           scale_x_continuous(breaks=c(2008, 2010, 2012, 2014, 2016, 2018)) +
           theme_bw() +
           theme(axis.title.x = element_blank(),
                 axis.title.y = element_text(size = 20),
                 axis.text = element_text(size = 20),
                 plot.title = element_text(size = 25),
                 strip.text = element_text(size = 20),
                 legend.position = "none"))


ggplotly(mrr_ref_facil_fig)

ggsave(filename =  paste0(save_directory2, "figures/synthesis-report-figures/drafts/stocks-flows/mrr_ref_fig.png"), mrr_ref_fig, width = 12, height = 10, units = "in", dpi = 300)




####

View(sf_ghg_df %>% filter(cat == "Petroleum Refineries", industry_sector == "Refinery and Hydrogen Plant / Transportation Fuel Supplier / CO2 Supplier"))



## try to figure out why there is a gap before 2014 for petroleum extraction
## -------------------------------------------------------------------------

extract_df <- total_ghg_df %>%
  filter(NAICS_code %in% c(oil_vec2)) %>%
  group_by(NAICS, report_yr) %>%
  summarise(sum_coe2 = sum(total_co2e)) %>%
  ungroup() 

ggplot(extract_df, aes(x = report_yr, y = sum_coe2 / 1e6, color = NAICS)) +
  geom_line(size = 1, alpha = 0.8) +
  guides(color = guide_legend(ncol = 1)) +
  geom_line(data = inv_df2 %>% filter(cat == "Petroleum Extraction",
                                      source == "inv_tool_co2e"), aes(x = report_yr, y = co2e), 
            color = "black", size = 1) +
  ylab("Mt CO2e") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

## refineries
##--------------------------------

refinery_df <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries")

refin_sector <- refinery_df %>%
  rename(year = report_yr) %>%
  mutate(industry_sector = ifelse(is.na(industry_sector), sec_report_sect, industry_sector)) %>%
  group_by(ARB_ID, facility_name, industry_sector, year) %>%
  summarise(sum_co2e = sum(total_co2e, na.rm = T)) %>%
  ungroup()


emiss_ind_fig <- refin_sector %>%
  group_by(industry_sector, year) %>%
  summarise(sum_co2e = sum(sum_co2e, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = sum_co2e / 1e6, color = industry_sector) +
  geom_line(size = 0.75, alpha = 0.8) +
  geom_line(data = inv_df2 %>% filter(cat == "Petroleum Refineries",
                                      source == "inv_tool_co2e",
                                      report_yr >= 2008), aes(x = report_yr, y = co2e), color = "black", size = 1, lty = "dashed") +
  ylab(expression(paste("Mt ", CO[2], "e"))) +
  guides(color = guide_legend(ncol = 1)) +
  annotate("text", x = 2016, y = 32, size = 5, label = "Total: GHG Inventory Tool") +
  scale_x_continuous(breaks=c(2008,2010,2012, 2014, 2016, 2018)) +
  theme_bw() +
  ggtitle("Refinery emissions by industry") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18),
        strip.text = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_blank())

ggsave(paste0(save_directory, "figs/refin_emissions_ind.png"), emiss_ind_fig, width = 10, height = 12, units = "in", dpi = 300)


emiss_facil_fig <- refin_sector %>%
  group_by(ARB_ID, year) %>%
  summarise(sum_co2e = sum(sum_co2e, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = sum_co2e / 1e6, color = as.factor(ARB_ID)) +
  geom_line(size = 0.75, alpha = 0.8) +
  geom_line(data = inv_df2 %>% filter(cat == "Petroleum Refineries",
                                      source == "inv_tool_co2e",
                                      report_yr >= 2008), aes(x = report_yr, y = co2e), color = "black", size = 1, lty = "dashed") +
  ylab("Mt CO2e") +
  guides(color = guide_legend(ncol = 1)) +
  annotate("text", x = 2016, y = 30.2, size = 5, label = "Total: GHG Inventory Tool") +
  annotate("text", x = 2012, y = 34, size = 4, label = "Chevron Products Company - Headquarters Fuel Supplier (Transportation Fuel Supplier)") +
  scale_x_continuous(breaks=c(2008,2010,2012, 2014, 2016, 2018)) +
  theme_bw() +
  ggtitle("Refinery emissions by facility") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18),
        strip.text = element_text(size = 20),
        legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_blank())

ggsave(paste0(save_directory, "figs/refin_emissions_facil.png"), emiss_facil_fig, width = 10, height = 12, units = "in", dpi = 300)

## how well do refineries map onto known refineries?

refinery_facil <- sf_ghg_df %>%
  filter(cat == "Petroleum Refineries") %>%
  mutate(facil_state = ifelse(facil_state == "California", "CA", facil_state)) %>%
  select(ARB_ID:facil_zip) %>%
  unique()

write_csv(refinery_facil, paste0(save_directory, "tables/refinery_facility_names.csv"))

## for well matching
## make figure for extraction... take most recent year. bar plot, emissions by facility, highest to lowest.

extract_facil_2018 <- sf_ghg_df %>%
  filter(report_yr == max(sf_ghg_df$report_yr),
         cat == "Petroleum Extraction") %>%
  select(ARB_ID:facil_zip, NAICS_code, industry_sector, total_co2e)

city_county_lut <- tibble(facil_city = unique(extract_facil_2018$facil_city),
                          county = c("Monterey", "Kern", "Ventura", "Santa Clarita", "Kern",
                                     "Los Angeles", "Orange", "Kern", "Los Angeles", "Solano",
                                     "Ventura", "Contra Costa", "Kern", "Santa Barbara", "Santa Barbara",
                                     "Kern", "Kern", "Santa Barbara", "Kern", "San Luis Obispo",
                                     "Santa Barbara", "Los Angeles"))

extract_facil_2018b <- left_join(extract_facil_2018, city_county_lut)

extract_emis_fig <- ggplot(extract_facil_2018b, aes(y = total_co2e / 1e6, x = reorder(facility_name, -total_co2e), fill = county)) +
  geom_bar(stat = "identity") +
  ylab("Mt CO2e") +
  scale_y_continuous(expand = c(0, 0.1)) +
  ggtitle("2018 GHG emissions by petroleum extraction facility") +
  labs(fill = "County") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5),
        legend.position = "right")

ggsave(paste0(save_directory, "figs/extract_emissions_facil.png"), extract_emis_fig, width = 10, height = 12, units = "in", dpi = 300)



## production 2018
prod2018 <- all_prod %>%
  filter(year == 2018) %>%
  select(APINumber, OilorCondensateProduced) %>%
  group_by(APINumber) %>%
  summarise(sum_production = sum(OilorCondensateProduced)) %>%
  ungroup() %>%
  rename(API = APINumber)

wells2018b <- wells2018 %>%
  left_join(prod2018) %>%
  filter(!is.na(sum_production)) %>%
  group_by(OperatorName, County) %>%
  summarise(sum_bbl = sum(sum_production)) %>%
  ungroup() %>%
  filter(sum_bbl > 0)

## lump smaller operators together by county
wells_2018_op <- wells2018b %>%
  group_by(OperatorName) %>%
  summarise(op_sum_bbl = sum(sum_bbl)) %>%
  ungroup() %>%
  mutate(prod = ifelse(op_sum_bbl >= 1000000, "high", "low"))

wells2018c <- wells2018b %>%
  left_join(wells_2018_op) %>%
  select(-op_sum_bbl) %>%
  mutate(OperatorName = ifelse(prod == "low", "Other", OperatorName)) %>%
  group_by(OperatorName, County) %>%
  summarise(sum_bbl = sum(sum_bbl)) %>%
  ungroup() %>%
  group_by(OperatorName) %>%
  mutate(op_sum = sum(sum_bbl)) %>%
  ungroup()

extract_bbl_fig <- ggplot(wells2018c, aes(y = sum_bbl / 1e6, x = reorder(OperatorName, -op_sum), fill = County)) +
  geom_bar(stat = "identity") +
  ylab("Million bbls") +
  scale_y_continuous(expand = c(0, 1)) +
  ggtitle("2018 oil production by operator") +
  guides(fill = guide_legend(ncol = 1)) +
  labs(fill = "County") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0.5),
        legend.position = "right")

ggsave(paste0(save_directory, "figs/extract_bbl_operator.png"), extract_bbl_fig, width = 10, height = 12, units = "in", dpi = 300)