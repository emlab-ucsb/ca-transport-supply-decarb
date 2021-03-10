## Meas Meng
## July 23, 2020
## process products movements data

################################# INPUTS ########################################

data.dir        = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/'
fil.movements   = 'raw/Finished_Products_Movements.xlsx'

#################################################################################

# libraries ------

  library(data.table)
  library(openxlsx)
  library(plyr)

# read in data -----
  fpm_gasoline = setDT(read.xlsx(paste0(data.dir,fil.movements), sheet = 'Gasoline Chart Data', startRow = 3, detectDates = T))
  colnames(fpm_gasoline)[2] = 'code'
  fpm_diesel = setDT(read.xlsx(paste0(data.dir,fil.movements), sheet = 'Diesel Chart Data', startRow = 4, detectDates = T))
  colnames(fpm_diesel)[2] = 'code'
  fpm_jet = setDT(read.xlsx(paste0(data.dir,fil.movements), sheet = 'Jet Fuel Chart Data', startRow = 3, detectDates = T))
  colnames(fpm_jet)[2] = 'code'


# reorganize gasoline finished product movements data -----

  # label sources (marine, pipeline)
  fpm_gasoline[2:11, source := 'marine']  
  fpm_gasoline[14:17, source := 'pipeline']  
  fpm_gasoline[18:19, source := 'marine_and_pipeline']
  
  # label flow movement
  fpm_gasoline[, movement_type := code ]
  fpm_gasoline[, movement_type := revalue(movement_type, c("DE" = 'domestic_export', 
                                                           "DI" = 'domestic_import', 
                                                           "FE" = 'foreign_export', 
                                                           "FI" = 'foreign_import', 
                                                           "NS" = 'north_to_south', 
                                                           "PE" = 'domestic_export', 
                                                           "SN" = 'south_to_north'))]
  fpm_gasoline[ `Product:.Gasoline.&.Blendstocks` %like% 'Total SC Pipeline', movement_type := 'domestic_export']
  fpm_gasoline[ `Product:.Gasoline.&.Blendstocks` %like% 'Net Imports', movement_type := 'net_import']
  
  
  # label location
  fpm_gasoline[ `Product:.Gasoline.&.Blendstocks` %like% 'NC', location := 'north']
  fpm_gasoline[ `Product:.Gasoline.&.Blendstocks` %like% 'SC', location := 'south']
  fpm_gasoline[ movement_type %in% c('north_to_south', 'south_to_north'), location := 'both']
  
  # remove NA rows
  fpm_gasoline2 = fpm_gasoline[!is.na(source)]
  
  # melt data table from wide to long format 
  fpm_gasoline2 = melt(fpm_gasoline2, measure.vars = colnames(fpm_gasoline2)[3:158],
                       variable.name = "date", value.name = "thous_bbl")
  colnames(fpm_gasoline2)[1] = 'movement'
  
  # add fuel column
  fpm_gasoline2[, fuel := 'gasoline']

# reorganize diesel data --------
  
  # label sources (marine, pipeline)
  fpm_diesel[1:10, source := 'marine']  
  fpm_diesel[12:16, source := 'pipeline']  
  
  # label flow movement
  fpm_diesel[, movement_type := code ]
  fpm_diesel[, movement_type := revalue(movement_type, c("DE" = 'domestic_export', 
                                                         "DI" = 'domestic_import', 
                                                         "FE" = 'foreign_export', 
                                                         "FI" = 'foreign_import', 
                                                         "NS" = 'north_to_south', 
                                                         "PE" = 'domestic_export', 
                                                         "SN" = 'south_to_north'))]
  fpm_diesel[ `Source:.Marine.(SLC-PIERS.Database)` %like% 'Total SC Pipeline', movement_type := 'domestic_export']
  fpm_diesel[ `Source:.Marine.(SLC-PIERS.Database)` %like% 'Net Imports', movement_type := 'net_import']
  
  
  # label location
  fpm_diesel[ `Source:.Marine.(SLC-PIERS.Database)` %like% 'NC', location := 'north']
  fpm_diesel[ `Source:.Marine.(SLC-PIERS.Database)` %like% 'SC', location := 'south']
  fpm_diesel[ movement_type %in% c('north_to_south', 'south_to_north'), location := 'both']
  
  # remove NA rows
  fpm_diesel2 = fpm_diesel[!is.na(source)]
  
  # melt data table from wide to long format 
  fpm_diesel2 = melt(fpm_diesel2, measure.vars = colnames(fpm_diesel2)[3:158],
                     variable.name = "date", value.name = "thous_bbl")
  
  fpm_diesel2 = fpm_diesel2[!is.na(movement_type)]
  colnames(fpm_diesel2)[1] = 'movement'
  
  # add fuel column
  fpm_diesel2[, fuel := 'diesel']
  
# reorganize jet data --------
  
  # label sources (marine, pipeline)
  fpm_jet[2:11, source := 'marine']  
  fpm_jet[14:17, source := 'pipeline']  
  
  # label flow movement
  fpm_jet[, movement_type := code ]
  fpm_jet[, movement_type := revalue(movement_type, c("DE" = 'domestic_export', 
                                                      "DI" = 'domestic_import', 
                                                      "FE" = 'foreign_export', 
                                                      "FI" = 'foreign_import', 
                                                      "NS" = 'north_to_south', 
                                                      "PE" = 'domestic_export', 
                                                      "SN" = 'south_to_north'))]
  fpm_jet[ `Product:.Jet.Fuel` %like% 'Total SC Pipeline', movement_type := 'domestic_export']
  fpm_jet[ `Product:.Jet.Fuel` %like% 'Net Imports', movement_type := 'net_import']
  
  
  # label location
  fpm_jet[ `Product:.Jet.Fuel` %like% 'NC', location := 'north']
  fpm_jet[ `Product:.Jet.Fuel` %like% 'SC', location := 'south']
  fpm_jet[ movement_type %in% c('north_to_south', 'south_to_north'), location := 'both']
  
  # remove NA rows
  fpm_jet2 = fpm_jet[!is.na(source)]
  
  # melt data table from wide to long format 
  fpm_jet2 = melt(fpm_jet2, measure.vars = colnames(fpm_jet2)[3:158],
                  variable.name = "date", value.name = "thous_bbl")
  
  fpm_jet2 = fpm_jet2[!is.na(movement_type)]
  colnames(fpm_jet2)[1] = 'movement'  
  
  # add fuel column
  fpm_jet2[, fuel := 'jet']
  
# combine all fuel data -----
  
  fpm_all = rbindlist(list(fpm_gasoline2, fpm_diesel2, fpm_jet2), use.names = T, fill = T)
  
# export csv ------
  
  fwrite(fpm_all, paste0(data.dir, 'processed/finished_product_movements_weekly_cec.csv'), row.names = F)
  