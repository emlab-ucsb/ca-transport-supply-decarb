## Tracey Mangin
## January 4, 2023
## Check health SRM outputs

library(tidyverse)

## paths 
main_path                <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
srm_save_path_general    <- paste0(main_path, "data/health/source_receptor_matrix/inmap_processed_srm/extraction/")
srm_save_path_nh3        <- paste0(srm_save_path_general, "nh3/")
srm_save_path_nox        <- paste0(srm_save_path_general, "nox/")
srm_save_path_pm25        <- paste0(srm_save_path_general, "pm25/")
srm_save_path_sox        <- paste0(srm_save_path_general, "sox/")
srm_save_path_voc       <- paste0(srm_save_path_general, "voc/")


## files names for text
nh3_fn <- "srm_nh3_field4.csv"
nox_fn <- "srm_nox_field6.csv"
pm25_fn <- "srm_pm25_field13.csv"
sox_fn <- "srm_sox_field14.csv"
voc_fn <- "srm_voc_field26.csv"

## nh3
nh3_gen <- read_csv(paste0(srm_save_path_general, nh3_fn))

nh3_comp <- read_csv(paste0(srm_save_path_nh3, nh3_fn)) %>%
  select(GEOID, totalpm25_aw_used = totalpm25_aw) %>%
  left_join(nh3_gen) %>%
  mutate(diff = totalpm25_aw_used - totalpm25_aw) %>%
  filter(diff != 0)

nrow(nh3_comp)

## nox
nox_gen <- read_csv(paste0(srm_save_path_general, nox_fn))

nox_comp <- read_csv(paste0(srm_save_path_nox, nox_fn)) %>%
  select(GEOID, totalpm25_aw_used = totalpm25_aw) %>%
  left_join(nox_gen) %>%
  mutate(diff = totalpm25_aw_used - totalpm25_aw) %>%
  filter(diff != 0)

nrow(nox_comp)

## pm25
pm25_gen <- read_csv(paste0(srm_save_path_general, pm25_fn))

pm25_comp <- read_csv(paste0(srm_save_path_pm25, pm25_fn)) %>%
  select(GEOID, totalpm25_aw_used = totalpm25_aw) %>%
  left_join(pm25_gen) %>%
  mutate(diff = totalpm25_aw_used - totalpm25_aw) %>%
  filter(diff != 0)

nrow(pm25_comp)

## sox
sox_gen <- read_csv(paste0(srm_save_path_general, sox_fn))

sox_comp <- read_csv(paste0(srm_save_path_sox, sox_fn)) %>%
  select(GEOID, totalpm25_aw_used = totalpm25_aw) %>%
  left_join(sox_gen) %>%
  mutate(diff = totalpm25_aw_used - totalpm25_aw) %>%
  filter(diff != 0)

nrow(sox_comp)

## voc
voc_gen <- read_csv(paste0(srm_save_path_general, voc_fn))

voc_comp <- read_csv(paste0(srm_save_path_voc, voc_fn)) %>%
  select(GEOID, totalpm25_aw_used = totalpm25_aw) %>%
  left_join(voc_gen) %>%
  mutate(diff = totalpm25_aw_used - totalpm25_aw) %>%
  filter(diff != 0)

nrow(voc_comp)
