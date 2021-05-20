# meas meng
# aug 18, 2020
# well injection type by field


# inputs ------

data_dir        = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/'
save_dir        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/focus-areas-1-2/figs/injection-well-type/'
# inj_file        = 'well_inject_m.rds'
inj_file        = 'well_inj_m_processed.csv'
type_file       = 'well_type_df.csv'
# info_file       = 'wells_19.csv'

# load libraries -------- 

library(data.table)  
library(lubridate)
library(openxlsx)
library(ggplot2)
library(hrbrthemes)
library(extrafont)

# load data -----  


# monthly wellstar injection data
well_inj <- fread(paste0(data_dir, inj_file), colClasses = c('api_ten_digit' = 'character',
                                                             'doc_field_code' = 'character'))
# well_inj = readRDS(paste0(data_dir, "well_inject_m.rds")) 
# well_inj = setDT(well_inj)
# well_inj = well_inj[ WellTypeCode %in% c('WF', 'SC', 'SF')]

well_type = fread(paste0(data_dir, "well_type_df.csv"))

# well_info = fread(paste0(data_dir, info_file), colClasses = c('character', rep(NA, 22))) # info on wells, operators, location, etc
# well_field = well_info[, c('API', 'FieldName')]
# well_field = unique(well_field)

ci_info = fread('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/ci/table_CA-fields-carbon-intensities_2018.csv', header = T)
ci_info[ field_name == 'Belridge South', field_name := 'Belridge  South']


# get number of fields belonging to each API, filter for wells that have non-zero injection

pos_well_inj <- well_inj[SteamWaterInjected > 0, .(count = uniqueN(doc_fieldname)), by = api_ten_digit]

# # get number of fields belonging to each API -----
#   
#   no_field_api = well_inj[, .(count = uniqueN(doc_fieldname)), by = api_ten_digit]
#   # no_field_api = well_field[, .(count = uniqueN(FieldName)), by = API]
#   # no_field_api = no_field_api[ API %in% unique(well_inj[, APINumber])]

# separate injection data -----

inj_mult = well_inj[api_ten_digit %in% pos_well_inj[ count > 1, api_ten_digit]]
# inj_mult = well_inj[APINumber %in% no_field_api[ count > 1, API]]

inj_single = well_inj[api_ten_digit %in% pos_well_inj[ count == 1, api_ten_digit]]
# inj_single = well_inj[APINumber %in% no_field_api[ count == 1, API]]
inj_single = inj_single[,-'well_type_name']
# inj_single = inj_single[well_field, on = c('APINumber' = 'API'), nomatch = 0]
inj_single = inj_single[well_type, on = 'WellTypeCode', nomatch = 0]

# get info on water/steam injected by type in each field -----

type_field = inj_single[, .(type_inj = sum(SteamWaterInjected, na.rm = T)), by = .(doc_field_code, doc_fieldname, well_type_name)] 
type_field[is.na(type_inj), type_inj := 0]
type_field[, total_inj := sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname)]
type_field[, perc := type_inj/sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname)]
type_field = type_field[order(-total_inj,-type_inj)]

# get info on injection by type in each, over the years -----

type_field_yr = inj_single[, .(type_inj = sum(SteamWaterInjected, na.rm = T)), by = .(doc_field_code, doc_fieldname, year, well_type_name)] 
type_field_yr[is.na(type_inj), type_inj := 0]
type_field_yr[, total_inj_year := sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname, year)]
type_field_yr[, perc := type_inj/sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname, year)]
type_field_yr = type_field_yr[order(doc_fieldname,year,-type_inj)]

type_field_yr = type_field_yr[ci_info, on = c('doc_fieldname' = 'field_name'), nomatch = 0]
labs_field_yr = unique(type_field_yr[, c('doc_field_code', 'doc_fieldname', 'year', 'total_inj_year', 'ci_2015')])

# 2018 only -----

type_field_2018 = inj_single[ year == 2018, .(type_inj = sum(SteamWaterInjected, na.rm = T)), by = .(doc_field_code, doc_fieldname, well_type_name)] 
type_field_2018[is.na(type_inj), type_inj := 0]
type_field_2018[, total_inj := sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname)]
type_field_2018[, perc := type_inj/sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname)]
type_field_2018 = type_field_2018[order(-total_inj,-type_inj)]

# 2015 only -----

type_field_2015 = inj_single[ year == 2015, .(type_inj = sum(SteamWaterInjected, na.rm = T)), by = .(doc_field_code, doc_fieldname, well_type_name)] 
type_field_2015[is.na(type_inj), type_inj := 0]
type_field_2015[, total_inj := sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname)]
type_field_2015[, perc := type_inj/sum(type_inj, na.rm = T), by = .(doc_field_code, doc_fieldname)]
type_field_2015 = type_field_2015[order(-total_inj,-type_inj)]

# plot theme -----

theme_line = theme_ipsum(base_family = 'Secca Soft',
                         grid = 'Y', 
                         plot_title_size = 28, 
                         subtitle_size = 22,
                         axis_title_just = 'center',
                         axis_title_size = 16, 
                         axis_text_size = 18,
                         strip_text_size = 14)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 12, color = '#5c5c5c', face = 'plain'),
        axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.25, "cm"),
        axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = 'bottom',
        legend.text = element_text(size = 18))

# plot top 10 injection fields -----

top10inj = unique(type_field[order(-total_inj), doc_fieldname])[1:10]
top10prod = c('Belridge  South', 'Midway-Sunset', 'Kern River', 'Cymric', 'Wilmington', 'Lost Hills', 'San Ardo', 'Elk Hills', 'Coalinga', 'Huntington Beach')

prim_calepa_pal <- c("#DBB95E", "#47778B", "#98B468", "#7A7095", "#636466", "#D1585C")

bar_top10inj = ggplot(type_field_yr[ year %in% c(2015,2018) & doc_fieldname %in% top10inj], 
                      aes(x = doc_fieldname, y = type_inj/1e6)) + geom_bar(stat = 'identity', aes(fill = well_type_name)) +
  labs(title = 'Breakdown of water and steam injection by well type for top 10 injection fields',
       subtitle = 'Million bbls of water/steam injected', 
       x = NULL,
       y = NULL,
       fill = NULL) +
  facet_wrap(vars(year), nrow = 2) +
  scale_y_continuous(limits = c(0, 600), breaks = seq(0,600,100)) + 
  # scale_fill_manual(values = prim_calepa_pal[c(1,3,2)]) + 
  geom_text(data = labs_field_yr[year %in% c(2015,2018) & doc_fieldname %in% top10inj], 
            aes(x = doc_fieldname, y = (total_inj_year/1e6) + 30, label = paste0(ci_2015, ' g/MJ')),
            size = 5.5, fontface = 'plain', family = 'Secca Soft') +
  theme_line
# bar_top10inj

ggsave(bar_top10inj,
       filename = paste0(save_dir, 'injection-by-well-type_top10inj_2015-vs-2018.png'),
       width = 16,
       height = 9,
       dpi = 400)


bar_top10prod = ggplot(type_field_yr[ year %in% c(2015,2018) & doc_fieldname %in% top10prod], 
                       aes(x = doc_fieldname, y = type_inj/1e6)) + geom_bar(stat = 'identity', aes(fill = well_type_name)) +
  labs(title = 'Breakdown of water and steam injection by well type for top ten oil producing fields',
       subtitle = 'Billion bbls of water/steam injected', 
       x = NULL,
       y = NULL,
       fill = NULL) +
  facet_wrap(vars(year), nrow = 2) +
  scale_fill_manual(values = prim_calepa_pal[c(1,3,2)]) + 
  geom_text(data = labs_field_yr[year %in% c(2015,2018) & doc_fieldname %in% top10prod], 
            aes(x = doc_fieldname, y = (total_inj_year/1e6) + 30, label = paste0(ci_2015, ' g/MJ')),
            size = 5.5, fontface = 'plain', family = 'Secca Soft') +
  theme_line
# bar_top10prod

ggsave(bar_top10prod,
       filename = paste0(save_dir, 'injection-by-well-type_top10prod_2015-vs-2018.png'),
       width = 16,
       height = 9,
       dpi = 400)


# export to csv files ----

fwrite(type_field, paste0(data_dir, 'injection-by-well-type-per-field_1977-2018_revised.csv'), row.names = F)
fwrite(type_field_yr, paste0(data_dir, 'injection-by-well-type-per-field-per-year_1977-2018_revised.csv'), row.names = F)