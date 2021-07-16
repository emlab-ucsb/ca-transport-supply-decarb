## Tracey Mangin
## July 9, 2021
## compare 2019 to 2020; last 20 vs pred 20; field and county

## libraries
library(data.table)
library(tidyverse)
library(scales)
library(extrafont)
library(hrbrthemes)

## paths
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
projection_path   = 'predict-production/extraction_2021-07-15/revised-edit-exit-model'

## files
histprod_file   = 'stocks-flows/crude_prod_x_field_revised.csv'
model_out       = 'diagnostic-field-level-results.csv'

## read in files
prod_hist = fread(file.path(outputs_path, histprod_file), header = T, colClasses = c("doc_field_code" = "character"))
projected_prod = fread(file.path(outputs_path, projection_path, model_out), header = T, colClasses = c("doc_field_code" = "character"))

## bau production

bau_prod = projected_prod[prod_quota_scenario == "no quota" & setback_scenario == "no_setback"]
bau_prod = bau_prod[, .(doc_field_code, year, total_prod_bbl)]
setnames(bau_prod, "total_prod_bbl", "total_bbls")

## 2019 vs 2020
comp_19_20 = prod_hist[year == 2019]
comp_19_20[, year := NULL]
setnames(comp_19_20, "total_bbls", "bbls_2019")

comp_20 = bau_prod[year == 2020]
comp_20[, year := NULL]
setnames(comp_20, "total_bbls", "bbls_2020")

comp_19_20 = merge(comp_19_20, comp_20,
                   all.x = T)

comp_19_20[, bbls_2020 := fifelse(is.na(bbls_2020), 0, bbls_2020)]
comp_19_20[, diff := bbls_2020 - bbls_2019]

comp_19_20 <- comp_19_20 %>%
  pivot_longer(bbls_2019:bbls_2020, names_to = "year", values_to = "bbls") %>%
  mutate(year = ifelse(year == "bbls_2019", 2019, 2020),
         year = as.integer(year)) %>%
  group_by(doc_field_code, doc_fieldname) %>%
  mutate(max_bbl = max(bbls)) %>%
  ungroup() %>%
  mutate(facet_grp = ifelse(max_bbl <= 386028, "subset-1", "subset-2")) %>%
  filter(!doc_field_code %in% c("000", "848", "154")) 


## make plots
## ----------------
year_comp <- ggplot(comp_19_20, aes(x = year, y = bbls, group = doc_fieldname, color = diff / 1e6)) +
  geom_line(alpha = 0.9) +
  labs(x = NULL,
       y = "barrels",
       color = "Difference\n(million)") +
  facet_wrap(~facet_grp, scales = "free_y") +
  geom_point(data = comp_19_20, aes(x = year, y = bbls, group = doc_fieldname, color = diff / 1e6)) +
  geom_text(data = comp_19_20 %>% filter(year == 2020), aes(x = year, label = doc_fieldname), nudge_x = 0.1, hjust = 0) +
  scale_y_continuous(labels = comma) + 
  scale_color_gradient2(low = muted("red"),
                        mid = "white",
                        high = muted("blue"),
                        midpoint = 0) +
  # xlim(2019, 2021) + 
  scale_x_continuous(limits = c(2019, 2021), breaks=c(2019, 2020, 2021)) +
  theme_bw() 

ggsave(year_comp,
       filename = file.path(outputs_path, projection_path, "diagnostic-figs/comparison_2019_2020.pdf"),
       width = 8,
       height = 8)

embed_fonts(file.path(outputs_path, projection_path, "diagnostic-figs/comparison_2019_2020.pdf"),
            outfile = file.path(outputs_path, projection_path, "diagnostic-figs/comparison_2019_2020.pdf"))  

## bars
year_comp_bars <- ggplot(comp_19_20 %>% select(doc_field_code, doc_fieldname, diff) %>% unique(), aes(y = reorder(doc_fieldname, diff) , x = diff/ 1e6)) +
  geom_bar(stat = "identity") +
  labs(x = "difference (million bbls)",
       y = NULL) +
  scale_x_continuous(labels = comma) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 4))

ggsave(year_comp_bars,
       filename = file.path(outputs_path, projection_path, "diagnostic-figs/comparison_2019_2020_bars.pdf"),
       width = 8,
       height = 8)

embed_fonts(file.path(outputs_path, projection_path, "diagnostic-figs/comparison_2019_2020_bars.pdf"),
            outfile = file.path(outputs_path, projection_path, "diagnostic-figs/comparison_2019_2020_bars.pdf"))  





## cumulative
hist_20 = prod_hist[year >= 2015, .(cumul_hist = sum(total_bbls)), by = .(doc_field_code, doc_fieldname)]
pred_20 = bau_prod[year <=  2024, .(cumul_projected = sum(total_bbls)), by = .(doc_field_code)]

cumul_comp <- full_join(hist_20, pred_20)
cumul_comp[is.na(cumul_comp)] = 0

cumul_comp <- cumul_comp %>%
  pivot_longer(cumul_hist:cumul_projected, names_to = "period", values_to = "bbls") %>%
  mutate(period = ifelse(period == "cumul_hist", "historic", "projected"))

## make plot
year_comp <- ggplot(cumul_comp, aes(x = period, y = bbls / 1e6, group = doc_fieldname)) +
  geom_line() +
  labs(x = NULL,
       y = "million bbls") +
  geom_point(data = cumul_comp, aes(x = period, y = bbls  / 1e6, group = doc_fieldname)) +
  geom_text(data = cumul_comp %>% filter(period == "projected"), aes(x = "projected", label = doc_fieldname), hjust = 0) +
  theme_bw()


