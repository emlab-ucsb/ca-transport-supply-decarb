## Tracey Mangin
## April 8, 2021
## Review outputs

## libraries
library(data.table)
library(tidyverse)
library(hrbrthemes)

## paths
base_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
scen_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
model_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'

## files
prod_quota_file = 'prod_quota_scenarios.csv'
histprod_file   = 'crude_prod_x_field_revised.csv'
entry_df_file   = 'entry_df_final_revised.csv'

## read in outputs
vintage_base = fread(file.path(base_path, 'extraction_2021-07-12', 'revised-incorporate-exit-v2', 'diagnostic-vintage-level-results.csv'), header = T)
field_base = fread(file.path(base_path, 'extraction_2021-07-12', 'revised-incorporate-exit-v2', 'diagnostic-field-level-results.csv'), header = T)
state_base = fread(file.path(base_path, 'extraction_2021-07-12', 'revised-incorporate-exit-v2', 'diagnostic-state-level-results.csv'), header = T)

# load production quota file
prod_quota_scens = fread(file.path(scen_path, prod_quota_file), header = T)
prod_quota_scens = subset(prod_quota_scens, select = -units)

# load historic production
prod_hist = fread(file.path(model_path, 'stocks-flows', histprod_file), header = T)

## load entry df
entry_df <- fread(file.path(model_path, 'stocks-flows', entry_df_file), header = T)

## pad field codes
vintage_base[, doc_field_code := sprintf("%03s", doc_field_code)]
field_base[, doc_field_code := sprintf("%03s", doc_field_code)]
prod_hist[, doc_field_code := sprintf("%03s", doc_field_code)]
entry_df[, doc_field_code := sprintf("%03s", doc_field_code)]

## make sure quota is numeric
prod_quota_scens[, quota := as.numeric(gsub(",", "", quota))]

## add scenario to state and field outputs
state_base[, scenario := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                 fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

## field-level new wells
field_base[, scenario := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                 fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

## state production compared to quota
state_prod <- state_base[, c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                             "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "year", "total_prod_bbl")]

state_prod <- merge(state_prod, prod_quota_scens)

state_prod[, quota := fifelse(is.infinite(quota), 150000000, quota)]

state_prod[, scenario := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                 fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]


## hitting the quota? yes
ggplot(state_prod, aes(x = year, y = total_prod_bbl / 1e6)) +
  geom_line(size = 1.5, color = "blue") +
  geom_line(data = state_prod, aes(x = year, y = quota / 1E6), size = 1.5, color = "red", lty = "dotted") +
  facet_wrap(~scenario)

## plot field-level new wells, new well production, old well production, and total production
## include historic and predicted -----------------------------------------------------------

# plot theme --------------------
theme_line =  
  theme_ipsum(base_family = 'Arial',
              grid = 'Y',
              plot_title_size = 16,
              subtitle_size = 14,
              axis_title_just = 'center',
              axis_title_size = 16,
              axis_text_size = 16,
              strip_text_size = 16)  +
  theme(plot.title = element_text(hjust = 0, face = 'bold'),
        plot.title.position = 'plot',
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
        axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
        axis.line.x = element_line(color = 'black'),
        axis.ticks.x = element_line(color = 'black'),
        axis.ticks.length.x = unit(0.25, "cm"),
        axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
        plot.margin = unit(c(1,2,1,1), "lines"),
        strip.text = element_text(hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.position = 'bottom',
  )


# save info file
save_info_path = file.path(base_path, 'extraction_2021-06-04', 'revised-remove-plugged', 'field-outputs') ## update this to go with run
dir.create(save_info_path, showWarnings = FALSE)
print(paste0("Saving field figures to ", save_info_path))


## create historic df to bind to predicted values
hist_new_wells <- entry_df[, c("doc_fieldname", "doc_field_code", "year", "n_new_wells", "new_prod")]
hist_new_wells[, scenario := "historic"]

setnames(hist_new_wells, 
         c("n_new_wells", "new_prod"), 
         c("new_wells", "new_prod_bbl"))

hist_new_wells <- hist_new_wells[, c("scenario", "doc_fieldname", "doc_field_code", "year", "new_wells", "new_prod_bbl")]

hist_new_wells <- hist_new_wells[doc_field_code %in% field_base[, doc_field_code]]

## add historic production

prod_hist[, doc_fieldname := NULL]

prod_hist <- merge(hist_new_wells, prod_hist)

setnames(prod_hist,
         c("total_bbls"),
         c("total_prod_bbl"))

prod_hist[, existing_prod_bbl := total_prod_bbl - new_prod_bbl]

prod_hist <- prod_hist[, c("scenario", "doc_fieldname", "doc_field_code", "year", "new_wells", "new_prod_bbl", "existing_prod_bbl", "total_prod_bbl")]

## add projected values

field_base2 <- field_base[, c("scenario", "doc_fieldname", "doc_field_code", "year", "new_wells", "new_prod_bbl", "existing_prod_bbl", "total_prod_bbl")]

## bind
all_field <- rbind(prod_hist, field_base2)

## state-level
## ---------------------------
prod_hist_state <- prod_hist[, lapply(.SD, sum, na.rm = T), .SDcols = c("new_wells", "new_prod_bbl", "existing_prod_bbl", "total_prod_bbl"), by = .(scenario, year)]

state_pred_wells <- state_base[, c("scenario", "year", "new_wells", "new_prod_bbl", "existing_prod_bbl", "total_prod_bbl")]

## bind
state_new_wells <- rbind(prod_hist_state, state_pred_wells)
state_new_wells[, scenario := factor(scenario, levels = c("historic", "BAU", "LCE1", "LCE2"))]


## pal
pal_vals <- c("black", "red", "#219ebc", "#ffb703")

## save state-level fig
state_nwells_fig <- ggplot(state_new_wells %>% filter(year > 1977), aes(x = year, y = new_wells, color = scenario, group = scenario)) +
  geom_line(size = 1, alpha = 0.7) +
  labs(title = "State-level new wells",
       subtitle = "# new wells", 
       x = 'Year',
       y = NULL) +
  scale_color_manual(values = pal_vals) +
  geom_vline(xintercept = 2020,  color = "darkgray", lty = "dashed") +
  theme_line +
  theme(legend.title = element_blank())

# save figure ----
ggsave(state_nwells_fig,
       filename = file.path(save_info_path, "0_state_nwells.pdf"),
       width = 11,
       height = 8.5)

embed_fonts(file.path(save_info_path, "0_state_nwells.pdf"),
            outfile = file.path(save_info_path, "0_state_nwells.pdf"))


## save state-level production fig
state_prod_fig <- ggplot(state_new_wells %>% filter(year > 1977), aes(x = year, y = new_prod_bbl / 1e6, color = scenario, group = scenario)) +
  geom_line(size = 1, alpha = 0.7) +
  labs(title = "State-level production from new wells",
       subtitle = "million bbls", 
       x = 'Year',
       y = NULL) +
  scale_color_manual(values = pal_vals) +
  geom_vline(xintercept = 2020,  color = "darkgray", lty = "dashed") +
  theme_line +
  theme(legend.title = element_blank())

# save figure ----
ggsave(state_prod_fig,
       filename = file.path(save_info_path, "0_state_nwells_prod.pdf"),
       width = 11,
       height = 8.5)

embed_fonts(file.path(save_info_path, "0_state_nwells_prod.pdf"),
            outfile = file.path(save_info_path, "0_state_nwells_prod.pdf"))

## prepare data frame for field level plots
## ----------------------------------------------------

field_base2 <- field_base[, c("scenario", "doc_fieldname", "doc_field_code", "year", "new_wells", "new_prod_bbl")]

field_base2 <- rbind(hist_new_wells, field_base2)

field_base2[, scenario := factor(scenario, levels = c("historic", "BAU", "LCE1", "LCE2"))]

## field names
field_names <- unique(field_base2[, doc_fieldname])

## function
plot_field_wells <- function(field) {
  
  fieldn <- field
  
  field_info <- field_base2[doc_fieldname == fieldn]
  field_info <- field_info[year > 1977]
  
  # fcode <- unique(field_info[, doc_field_code])
  
  field_nwells_fig <- ggplot(field_info, aes(x = year, y = new_wells, color = scenario, lty = scenario)) +
    geom_line(size = 1.5, alpha = 0.7) +
    ylim(0, max(round(field_info[, new_wells])) + 1) +
    labs(title = fieldn,
         subtitle = "new wells", 
         x = 'Year',
         y = NULL) +
    scale_color_manual(values = pal_vals) +
    geom_vline(xintercept = 2020,  color = "darkgray", lty = "dashed") +
    theme_line +
    theme(legend.title = element_blank())
  
  field_new_prod_fig <- ggplot(field_info, aes(x = year, y = new_prod_bbl, color = scenario, lty = scenario)) +
    geom_line(size = 1.5, alpha = 0.7) +
    ylim(0, max(round(field_info[, new_prod_bbl])) + 5) +
    labs(title = fieldn,
         subtitle = "new well production (bbls)", 
         x = 'Year',
         y = NULL) +
    scale_color_manual(values = pal_vals) +
    geom_vline(xintercept = 2020,  color = "darkgray", lty = "dashed") +
    theme_line +
    theme(legend.title = element_blank())

  
  
  
  
  
  # save figures ----
  well_fname = paste0(fieldn, '_new_wells.pdf')
  ggsave(field_nwells_fig,
         filename = file.path(save_info_path, well_fname),
         width = 11,
         height = 8.5)
  
  embed_fonts(file.path(save_info_path, well_fname),
              outfile = file.path(save_info_path, well_fname))
  
}

purrr::map(field_names, plot_field_wells)

