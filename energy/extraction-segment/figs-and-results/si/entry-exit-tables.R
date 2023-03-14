## Tracey Mangin
## December 17, 2022
## create tables for SI

library(data.table)
library(tidyverse)
library(stargazer)

## paths
main_path         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/'
data_path         <- paste0(main_path, "nature-energy-rev-outputs/")
save_directory    <- paste0(main_path, "figures/nature-energy-revision/setback-revision/figs/si/tables/")

## file names
entry_file <- "entry_coefs.csv"
exit_file  <- "exit_coefs.csv"

## read in csvs
entry_df <- fread(paste0(data_path, entry_file), fill = TRUE, skip = 2)
exit_df <- fread(paste0(data_path, exit_file))