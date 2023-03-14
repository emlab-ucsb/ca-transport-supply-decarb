## Tracey Mangin
## November 10, 2022
## Labor multipliers

library(tidyverse)
library(data.table)

## paths 
main_path     <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'
save_revision_path <- paste0(main_path, 'outputs/academic-out/extraction/nature-energy-rev-outputs/')
save_revision_fig <- paste0(main_path, 'outputs/academic-out/extraction/figures/nature-energy-revision/exploratory/')

## figure themes
theme_line =  theme(plot.title = element_text(hjust = 0, face = 'bold'),
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



## file names
prod_file       <- "well_prod_m_processed.csv"


## load labor file
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

## create table that we use when compiling outputs
county_multipliers <- total_multipliers_ext %>%
  select(county, dire_emp_mult, indi_emp_mult, indu_emp_mult,
         dire_comp_mult, ip.indi_comp_mult, ip.indu_comp_mult)

## load prod
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

## county lut
county_lut <- well_prod %>%
  dplyr::select(county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## calculate average production by field, 2015-2019
prod_x_county <- expand.grid(adj_county_name = unique(county_lut$adj_county_name),
                            year = 2015:2019)

mean_prod_county <- well_prod %>%
  filter(year >= 2015) %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore")) %>%
  group_by(adj_county_name, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T), .groups = 'drop') %>%
  right_join(prod_x_county) %>%
  mutate(prod = ifelse(is.na(prod), 0, prod)) %>%
  group_by(adj_county_name) %>%
  summarise(mean_prod = mean(prod), .groups = 'drop') %>%
  rename(county = adj_county_name)

## make sure county names match
labor_df <- mean_prod_county %>% 
  left_join(county_multipliers) %>%
  filter(mean_prod > 0) %>%
  pivot_longer(dire_emp_mult:ip.indu_comp_mult, names_to = "multiplier_name", values_to = "multiplier") %>%
  mutate(mult_per_bbl = multiplier / mean_prod)


## save 
write_csv(labor_df, paste0(save_revision_path, "labor_multipliers.csv"))

rank_df <- mean_prod_county %>%
  filter(mean_prod > 0) %>%
  mutate(rank = rank(-mean_prod)) %>%
  filter(rank <= 5)

labor_df$multiplier_name <- factor(labor_df$multiplier_name, levels = c("dire_emp_mult", "indi_emp_mult", "indu_emp_mult",
                                                                        "dire_comp_mult", "ip.indi_comp_mult", "ip.indu_comp_mult"))

## plot
labor_fig <- ggplot(labor_df %>% filter(county %in% rank_df$county), aes(x = mean_prod/1e6, y = mult_per_bbl, size = mean_prod / 1e6, color = county)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~multiplier_name, scales = "free") +
  labs(subtitle = "Labor multipliers",
       x = "Mean annual oil production 2015-2019 (million bbls)",
       y = "Multiplier per bbl",
       color = "") +
  theme_line +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw() +
  guides(size = "none") +
  # scale_x_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'))

ggsave(labor_fig,
       filename = file.path(save_revision_fig, 'labor_multipliers.pdf'),
       width = 8,
       height = 6)

  
  

