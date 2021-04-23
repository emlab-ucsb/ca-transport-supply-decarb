## compare brent price

proj_dir           <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
data_directory     <- "data/stocks-flows/processed/"
output_dir         <- "outputs/stocks-flows/"
rystad_path        <- "data/Rystad/data/"

## files
brent_file              <- "eia_spot_price_a.csv"
rystad_b                <- "wti_brent.csv"
brent_p_file            <- "brent_oil_price_projections.csv"

## library
library(data.table)
library(tidyverse)
library(hrbrthemes)

## theme
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


## read in data
eia <- fread(paste0(proj_dir, data_directory, brent_file))

eia <- eia %>%
  select(date, price, value) %>%
  filter(price == "europe_brent_FOB") %>%
  mutate(date = as.Date(date),
         year = year(date),
         source = "EIA historic") %>%
  select(source, year, brent_price = value) %>%
  filter(!is.na(brent_price))
  

rystad_b <- fread(paste0(proj_dir, rystad_path, "raw/", rystad_b))



rystad<- rystad_b %>%
  rename(year = `[Data Values]`,
         wti = `WTI Cushing oil price (USD/bbl)`,
         brent_price = `Brent oil price (USD/bbl)`) %>%
  mutate(source = "rystad") %>%
  select(source, year, brent_price) %>% 
  rbind(eia) %>% 
  filter(year != "Year") %>%
  mutate(year = as.integer(year))

# eia_proj <- fread(paste0(proj_dir, data_directory, brent_p_file))

# all_sources <- eia_proj %>%
#   select(scenario, year, brent_price = nom_usd_per_bbl) %>%
#   rename(source = scenario) %>%
#   rbind(rystad)

ggplot(rystad %>% filter(year <= 2020), aes(x = year, y = brent_price, color = source, group = source, lty = source)) +
  geom_path(size = 0.7, alpha = 0.8) +
  ylab("price") +
  theme_line +
  theme(axis.title.x = element_blank())
  
  
ggplot(rystad %>% filter(year %in% unique()), aes(x = year, y = brent_price, color = source, group = source, lty = source)) +
  geom_path(size = 0.7, alpha = 0.8) +
  ylab("price") +
  theme_line +
  theme(axis.title.x = element_blank())

