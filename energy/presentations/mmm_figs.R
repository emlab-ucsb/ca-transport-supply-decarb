## Tracey Mangin
## August27, 2021
## figures for MMM

## libraries
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(extrafont)

## paths
proj_dir <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path <- 'data/stocks-flows/processed/'
outputs_path <- 'outputs/predict-production/extraction_2021-08-27/comp-scens/'

## files
benchmark_file <- 'comparison_scens-state-level-results.csv'


## theme
theme_line =  
  theme_ipsum(base_family = 'Arial',
              grid = 'Y',
              plot_title_size = 12,
              subtitle_size = 11,
              axis_title_just = 'center',
              axis_title_size = 20,
              axis_text_size = 20,
              strip_text_size = 20)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 16, color = '#5c5c5c', face = 'plain'),
        # axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), angle = 90),
        axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.25, "cm"),
        axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
        plot.margin = unit(c(1,2,1,1), "lines"),
        strip.text = element_text(hjust = 0.5),
        legend.text = element_text(size = 18),
        legend.position = 'bottom',
  )


## benchmark outputs with correction
## ----------------------------

benchmark_out <- fread(paste0(proj_dir, outputs_path, benchmark_file), header = T)

## get indicators
outputs <- benchmark_out[, .(excise_tax_scenario, setback_scenario, prod_quota_scenario,
                             year, total_prod_bbl, total_ghg_mtCO2e, total_ghg_kgCO2e)]
outputs[, emissions_per_bbl := total_ghg_kgCO2e / total_prod_bbl]
outputs[, total_prod_bbl := total_prod_bbl / 1e6]
outputs[, total_ghg_kgCO2e := NULL]

outputs <- melt(outputs, id.vars = c("excise_tax_scenario", "setback_scenario", "prod_quota_scenario", "year"),
               measure.vars = c("total_prod_bbl", "total_ghg_mtCO2e", "emissions_per_bbl"))

outputs[, variable_title := fifelse(variable == "total_prod_bbl", "Production (million bbls)",
                                    fifelse(variable == "total_ghg_mtCO2e", "GHG emission (MtCO2e)", "Emissions kgCO2e per bbl"))]

outputs$variable_title <- factor(outputs$variable_title, levels = c('Production (million bbls)', 'GHG emission (MtCO2e)', 'Emissions kgCO2e per bbl'))


outputs[, scenario := fifelse(excise_tax_scenario == "no tax" &
                              setback_scenario == "no_setback" &
                              prod_quota_scenario == "no quota", "BAU",
                              fifelse(setback_scenario != "no_setback", setback_scenario,
                                      fifelse(excise_tax_scenario != "no tax", excise_tax_scenario, prod_quota_scenario)))]

plot1 <- ggplot(outputs %>% filter(excise_tax_scenario == "no tax",
                                   setback_scenario == "no_setback",
                                   prod_quota_scenario == "no quota"), aes(x = year, y = value, group = variable_title, color = scenario)) +
  geom_line(size = 2) +
  facet_wrap(~variable_title, scales = "free") +
  scale_color_manual(values = c("BAU" = "black",
                                "setback_1000ft" = "#FFBF00",
                                "setback_2500ft" = "#FF7F50",
                                "setback_5280ft" = "#DE3163")) +
  scale_y_continuous(limits = c(0, max(outputs$value))) +
  theme_line +
  labs(y = NULL,
       x = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave(plot1, 
       filename = paste0(proj_dir, outputs_path, "figs/fig1.png"), 
       width = 20, 
       height = 10)




plot2 <- ggplot(outputs %>% filter(scenario %in% c("BAU", "setback_1000ft", "setback_2500ft", "setback_5280ft")), aes(x = year, y = value, 
                                                                                                                      color = scenario, group = scenario)) +
  geom_line(size = 2) +
  facet_wrap(~variable_title, scales = "free") +
  scale_color_manual(values = c("BAU" = "black",
                                "setback_1000ft" = "#FFBF00",
                                "setback_2500ft" = "#FF7F50",
                                "setback_5280ft" = "#DE3163")) +
  scale_y_continuous(limits = c(0, max(outputs$value))) +
  theme_line +
  labs(y = NULL,
       x = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave(plot2, 
       filename = paste0(proj_dir, outputs_path, "figs/fig2.png"), 
       width = 20, 
       height = 10)


plot3 <- ggplot(outputs %>% filter(scenario %in% c("BAU", "setback_5280ft")), aes(x = year, y = value, 
                                                                                                                      color = scenario, group = scenario)) +
  geom_line(size = 2) +
  facet_wrap(~variable_title, scales = "free") +
  scale_color_manual(values = c("BAU" = "black",
                                "setback_5280ft" = "#DE3163",
                                "tax_setback_5280ft" = "#6495ED")) +
  scale_y_continuous(limits = c(0, max(outputs$value))) +
  theme_line +
  labs(y = NULL,
       x = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave(plot3, 
       filename = paste0(proj_dir, outputs_path, "figs/fig3.png"), 
       width = 20, 
       height = 10)


plot4 <- ggplot(outputs %>% filter(scenario %in% c("BAU", "setback_5280ft", "tax_setback_5280ft")), aes(x = year, y = value, 
                                                                                                        color = scenario, group = scenario)) +
  geom_line(size = 2) +
  facet_wrap(~variable_title, scales = "free") +
  scale_color_manual(values = c("BAU" = "black",
                                "setback_5280ft" = "#DE3163",
                                "tax_setback_5280ft" = "#6495ED")) +
  scale_y_continuous(limits = c(0, max(outputs$value))) +
  theme_line +
  labs(y = NULL,
       x = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave(plot4, 
       filename = paste0(proj_dir, outputs_path, "figs/fig4.png"), 
       width = 20, 
       height = 10)

plot5 <- ggplot(outputs %>% filter(scenario %in% c("BAU", "setback_5280ft", "tax_setback_5280ft", "tax_90_perc_reduction")), aes(x = year, y = value, 
                                                                                                        color = scenario, group = scenario)) +
  geom_line(size = 2) +
  facet_wrap(~variable_title, scales = "free") +
  scale_color_manual(values = c("BAU" = "black",
                                "setback_5280ft" = "#DE3163",
                                "tax_setback_5280ft" = "#6495ED",
                                "tax_90_perc_reduction" = "navy")) +
  scale_y_continuous(limits = c(0, max(outputs$value))) +
  theme_line +
  labs(y = NULL,
       x = NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave(plot5, 
       filename = paste0(proj_dir, outputs_path, "figs/fig5.png"), 
       width = 20, 
       height = 10)



