# analyze historic trends of extraction emissions by process
# created: 2023-03-01
# author: @measrainsey

# inputs ------------

main_path       = '/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn'
res_path        = 'data/OPGEE'
prod_file       = 'outputs/stocks-flows/crude_prod_x_field_revised.csv'
entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
inj_file        = 'data/stocks-flows/processed/injection-by-well-type-per-field-per-year_1977-2018_revised.csv'

# outputs ------

save_path       = file.path(main_path, "outputs/ghg-emissions/opgee-results")

# load packages ------

library(data.table)
library(ggplot2)
library(hrbrthemes)
library(extrafont)

# read in results files -----

files_opgee = list.files(path = file.path(main_path, res_path), pattern = 'opgee_doc_results')
list_opgee = lapply(file.path(main_path, res_path, files_opgee), fread)
dt_opgee = rbindlist(list_opgee)

# convert from wide to long format ------

dt_opgee_long = melt(dt_opgee, 
                     id.vars = c("year", "field_name"), 
                     measure.vars = colnames(dt_opgee)[4:11],
                     variable.name = "process",
                     value.name = "ghg_gCO2e_MJ")
dt_opgee_long[, process := gsub("_gCO2e_MJ", "", process)]
dt_opgee_long[, process := gsub("_", " ", process)]

# rename fields ------

dt_opgee_long[, doc_fieldname := field_name]
dt_opgee_long[, doc_fieldname := gsub(',', '', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'North$', doc_fieldname := gsub('North', ' North', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'South$', doc_fieldname := gsub('South', ' South', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'East$', doc_fieldname := gsub('East', ' East', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'West$', doc_fieldname := gsub('West', ' West', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'N$', doc_fieldname := gsub('N', ' North', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'S$', doc_fieldname := gsub('S', ' South', doc_fieldname)]
dt_opgee_long[doc_fieldname %like% 'Northwest$', doc_fieldname := gsub('Northwest', ' Northwest', doc_fieldname)]
dt_opgee_long[doc_fieldname == 'Elwood S. Offshore', doc_fieldname := 'Elwood  South  Offshore']

# list top 10 fields --------

top10_fields = c("Belridge South", 
                 "Midway-Sunset", 
                 "Kern River", 
                 "Cymric", 
                 "Wilmington", 
                 "Lost Hills", 
                 "San Ardo", 
                 "Elk Hills", 
                 "Coalinga", 
                 "Poso Creek")

# create group for top 10 and non-top 10 fields -------

dt_opgee_long[, field_category := fifelse(doc_fieldname %chin% top10_fields, doc_fieldname, "non-top 10 field")]

# aggregate across field type ------

median_field_ghg_process = dt_opgee_long[, .(ghg_gCO2e_MJ = median(ghg_gCO2e_MJ, na.rm = T)), 
                                         by = .(year, process, field_category)]
median_field_ghg_process[, ghg_kgCO2e_bbl := ghg_gCO2e_MJ*(1/(2e-4))*(1/1000)]

# reorder factor levels -------

process_names = gsub("_gCO2e_MJ", "", colnames(dt_opgee)[4:11])
process_names = gsub("_", " ", process_names)

median_field_ghg_process[, field_category := factor(field_category, 
                                                    levels = c("Belridge South", 
                                                               "Midway-Sunset", 
                                                               "Kern River", 
                                                               "Cymric", 
                                                               "Wilmington", 
                                                               "Lost Hills", 
                                                               "San Ardo", 
                                                               "Elk Hills", 
                                                               "Coalinga", 
                                                               "Poso Creek",
                                                               "non-top 10 field"))]


median_field_ghg_process[, process := factor(process, levels = process_names)]


# theme ---------

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
        legend.position = 'bottom',
        strip.text = element_text(hjust = 0.5, face = 'bold'),
        plot.margin = unit(c(1,3,1,1), 'lines'),
        plot.background = element_rect(color = 'white'))

pal_fields = c("Belridge South" = "#a6cee3", 
               "Midway-Sunset" = "#1f78b4", 
               "Kern River" = "#b2df8a", 
               "Cymric" = "#33a02c", 
               "Wilmington" = "#fb9a99", 
               "Lost Hills" = "#e31a1c", 
               "San Ardo" = "#fdbf6f", 
               "Elk Hills" = "#ff7f00", 
               "Coalinga" = "#cab2d6", 
               "Poso Creek" = "#6a3d9a", 
               "non-top 10 field" = "#000000")

# plot ghg factor over time --------

fig_ghg_factor = ggplot(median_field_ghg_process, 
                        aes(x = year, y = ghg_kgCO2e_bbl, group = field_category, color = field_category)) +
  geom_line() +
  facet_wrap(~ process, nrow = 2, scales = "free_y") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Year",
       y = "GHG factor (kg CO2e per bbl)",
       color = NULL) +
  scale_color_manual(values = pal_fields) +
  theme_line
fig_ghg_factor

ggsave(fig_ghg_factor,
       filename = file.path(save_path, "opgee_ghg_factors_by_process.pdf"),
       width = 11,
       height = 6,
       dpi = 400, 
       units = 'in', 
       device = 'pdf')

