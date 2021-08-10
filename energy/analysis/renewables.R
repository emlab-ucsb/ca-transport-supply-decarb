## Tracey Mangin
## May 29, 2020
## renewable deisel and biofuels

library(tidyverse)
library(readxl)
library(rebus)
library(cowplot)
# library(extrafont)

# ## deal with the fonts
# font_import(pattern = "(?i)calibri") # load only calibri fonts
# 
# loadfonts(device = "pdf")


## set directory
data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## source items
# items <- list.files(here::here("src"))
# 
# walk(items, ~ here::here("src", .x) %>% source()) # load local items
### 

## read in the data
renewable_df <- read_xlsx(paste0(data_directory, "CARB_RE_fuels_CA_imports_figure10_053120.xlsx"), sheet = 1, skip = 1)

renewable_df2 <- janitor::clean_names(renewable_df) %>%
  select(x1:x2019) %>%
  rename(fuel_type = x1)

renewable_df3 <- renewable_df2[1:30,]

unit_df <- tibble(unit = c(rep("gallons", 14), rep("gge", 14)))

renewable_df4 <- renewable_df3 %>%
  filter(!is.na(x2011)) %>%
  cbind(unit_df) %>%
  mutate(ca = str_detect(fuel_type, pattern = START %R% "CA"),
         source = ifelse(ca == TRUE, "California", "Out-of-state")) %>%
  select(fuel_type, source, unit, x2011:x2019) %>%
  pivot_longer(x2011:x2019,
               names_to = "year",
               values_to = "production") %>%
  mutate(year = as.numeric(str_extract(year, pattern = one_or_more(DGT))),
         fuel_type = str_remove(fuel_type, pattern = START %R% "CA "),
         fuel_type = str_remove(fuel_type, pattern = START %R% "Imported "),
         type = str_remove(fuel_type, pattern = START %R% "Biodiesel-" %|% "Ethanol-" %|% "Renewable Diesel-"),
         type = str_remove(type, pattern = START %R% "Imported " %R% "Biodiesel-" %|% "Ethanol-" %|% "Renewable Diesel-"),
         type = str_remove(type, pattern = START %R% "Imported "),
         fuel_type = str_remove(fuel_type, pattern = "-" %R% one_or_more(ANY_CHAR, char_class=FALSE) %R% END)) %>%
  select(fuel_type, type, source:production)

## percent ethanol instate, 2019
ca_ethanol_perc <- renewable_df4 %>%
  filter(fuel_type == "Ethanol") %>%
  group_by(year, source, unit) %>%
  summarise(reg_total_prod = sum(production)) %>%
  ungroup() %>%
  group_by(year, unit) %>%
  mutate(total = sum(reg_total_prod)) %>%
  ungroup() %>%
  mutate(rel_prod = reg_total_prod / total)

ca_ethanol_perc2 <- renewable_df4 %>%
  filter(fuel_type == "Ethanol") %>%
  group_by(year, source, unit) %>%
  mutate(total = sum(production)) %>%
  ungroup() %>%
  mutate(rel_prod = production / total)


## percent ethanol instate, 2019
ca_diesel_perc <- renewable_df4 %>%
  filter(fuel_type %in% c("Biodiesel", "Renewable Diesel")) %>%
  group_by(fuel_type, year, source, unit) %>%
  summarise(reg_total_prod = sum(production)) %>%
  ungroup() %>%
  group_by(fuel_type, year, unit) %>%
  mutate(total = sum(reg_total_prod)) %>%
  ungroup() %>%
  mutate(rel_prod = reg_total_prod / total)

# ca_diesel_perc2 <- renewable_df4 %>%
#   filter(fuel_type %in% c("Biodiesel", "Renewable Diesel")) %>%
#   group_by(year, unit) %>%
#   mutate(total = sum(production)) %>%
#   ungroup() %>%
#   mutate(rel_prod = production / total)



## fig 1: ethanol, all types, imports, in state, gallsons and equiv
ethanol_fig <-
ggplot(renewable_df4 %>% filter(fuel_type == "Ethanol"), aes(x = year, y = production / 1e6, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = prim_calepa_pal) +
  facet_grid(source ~ unit) +
  theme_bw() +
  ylab("Production (millions)") +
  scale_x_continuous(breaks=c(seq(2011, 2019, 2))) +
  base_theme 

# ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/drafts/stocks-flows/ethanol_fig.png"), ethanol_fig, width = 6, height = 6, units = "in", dpi = 300)
ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/final/stocks-flows/fig28.png"), ethanol_fig, width = 6, height = 6, units = "in", dpi = 300)

## percent
eth_val <- renewable_df4 %>%
  filter(fuel_type == "Ethanol",
         unit == "gallons") %>%
  group_by(year, source) %>%
  summarise(sum = sum(production)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total = sum(sum)) %>%
  ungroup() %>%
  mutate(rel_val = sum / total)

## fig 2: biodesiel and renewable desiel
renewable_fig <-
  ggplot(renewable_df4 %>% filter(fuel_type == "Renewable Diesel"), aes(x = year, y = production / 1e6, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(source ~ unit) +
  ggtitle("Renewable Diesel") +
  # scale_fill_manual(values = prim_calepa_pal) +
  ylab("Production (millions)") +
  scale_x_continuous(breaks=c(seq(2011, 2019, 2))) +
  # base_theme +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1, vjust = 1))

ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/drafts/stocks-flows/renewable_fig.png"), renewable_fig, width = 8, height = 8, units = "in", dpi = 300)


## save renewable diesel df for scenario selection input
rdiesel <- renewable_df4 %>%
  filter(year == 2019,
         source == "California",
         fuel_type == "Renewable Diesel",
         unit == "gallons") %>%
  group_by(fuel_type, source, unit, year) %>%
  summarise(production = sum(production)) %>%
  ungroup()

write_csv(rdiesel, '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/model-development/scenario-plot/renewable_diesel_2019.csv')

biodiesel_fig <-
  ggplot(renewable_df4 %>% filter(fuel_type == "Biodiesel"), aes(x = year, y = production / 1e6, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  ylim(0, 400) +
  scale_fill_manual(values = prim_calepa_pal) +
  facet_grid(source ~ unit) +
  ggtitle("Biodiesel") +
  scale_x_continuous(breaks=c(seq(2011, 2019, 2))) +
  ylab("Production (millions)") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1, vjust = 1))

ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/drafts/stocks-flows/biodesel_fig.png"), renewable_fig, width = 8, height = 8, units = "in", dpi = 300)


## put renewable and bioseiel together
both_plots <- plot_grid(renewable_fig, biodiesel_fig, labels = c('A', 'B'))


# ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/drafts/stocks-flows/biodesel_renewable_fig.png"), both_plots, width = 6, height = 4, units = "in", dpi = 300)
ggsave(filename =  paste0(save_directory, "figures/synthesis-report-figures/final/stocks-flows/fig30.png"), both_plots, width = 6, height = 4, units = "in", dpi = 300)






# ## ethanol
# ethanol_df <- renewable_df2 %>%
#   filter(fuel_type == "Ethanol") %>%
#   group_by(year, unit) %>%
#   summarise(sum_prod = sum(production)) %>%
#   ungroup() %>%
#   filter(year == 2011 | year == 2018) %>%
#   mutate(year = paste0("x", year)) %>%
#   pivot_wider(names_from = "year", values_from = "sum_prod") %>%
#   mutate(diff = x2018 - x2011,
#          rel_diff = diff / x2011)
# 
# ethanol_df2 <- renewable_df2 %>%
#   filter(fuel_type == "Ethanol") %>%
#   group_by(year, unit, source) %>%
#   summarise(sum_prod = sum(production)) %>%
#   ungroup() %>%
#   filter(year == 2011 | year == 2018) %>%
#   mutate(year = paste0("x", year)) %>%
#   pivot_wider(names_from = "year", values_from = "sum_prod") %>%
#   mutate(diff = x2018 - x2011,
#          rel_diff = diff / x2011)
# 
