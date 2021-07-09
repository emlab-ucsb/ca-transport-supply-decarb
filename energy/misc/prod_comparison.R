## Tracey Mangin
## July 9, 2021
## compare 2019 to 2020; last 20 vs pred 20; field and county

## libraries
library(data.table)
library(tidyverse)

## paths
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
projection_path   = 'predict-production/extraction_2021-07-07/revised-density-calc'

## files
histprod_file   = 'stocks-flows/crude_prod_x_field_revised.csv'
model_out       = 'diagnostic-field-level-results.csv'

## read in files
prod_hist = fread(file.path(outputs_path, histprod_file), header = T, colClasses = c("doc_field_code" = "character"))
projected_prod = fread(file.path(outputs_path, projection_path, model_out), header = T, colClasses = c("doc_field_code" = "character"))

## 2019 and 2020 production

