## Tracey Mangin
## September 16, 2021

library(hrbrthemes)

## figure themes
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
        legend.position = 'top',
        strip.text = element_text(hjust = 0.5),
        plot.margin = unit(c(1,1,1,1), 'lines'))

## target colors
target_colors <- c("55%" = "#133a54",
                   "60%" = "#1d577e",
                   "75%" = "#6390ae",
                   "90%" = "#a6bfd1")

policy_colors_all <-  c('BAU' = "black",
                        'carbon tax' = "#fcb97d",
                        'excise tax' = '#ff5e5b',
                        'setback' = '#4a6c6f',
                        'carbon tax & setback' = "#9DBF9E")

policy_colors_subset <-  c('carbon tax' = "#fcb97d",
                           'excise tax' = '#ff5e5b',
                           'setback' = '#4a6c6f')


policy_symbols <- c("BAU" = 16,
                    "carbon tax" = 17,
                    "excise tax" = 15,
                    "setback" = 3)



