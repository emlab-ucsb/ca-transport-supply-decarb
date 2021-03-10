## Tracey Mangin
## October 28, 2020
## Rern oil renewable diesel production

library(tidyverse)
library(rebus)

## path
data_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/raw/"
save_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/"

credits <- read_csv(paste0(data_path, "LCFS Credit Recipients (3).csv"))

credits2 <- janitor::clean_names(credits) %>%
  select(lcfs_credit_recipient, fuel_type, range_of_credits) %>%
  filter(fuel_type == "Renewable Diesel") %>%
  mutate(range2 = str_sub(range_of_credits, 5),
         range2 = str_remove(range2, START %R% "< "),
         range2 = str_remove_all(range2, ","),
         low_end = ifelse(lcfs_credit_recipient == "AltAir Paramount, LLC", 0, as.numeric(str_extract(range2, START %R% one_or_more(DGT)))),
         high_end = ifelse(lcfs_credit_recipient == "AltAir Paramount, LLC", range2, 
                           as.numeric(str_extract(range2, one_or_more(DGT) %R% END)))) %>%
  select(-range2) %>%
  mutate(high_end = as.numeric(high_end)) %>%
  pivot_longer(low_end:high_end, names_to = "range_end", values_to = "credits") %>%
  group_by(lcfs_credit_recipient, range_of_credits) %>%
  summarise(mean_credits = mean(credits)) %>%
  ungroup() %>%
  mutate(total_credits = sum(mean_credits)) %>%
  ungroup() %>%
  mutate(credit_ratio = mean_credits / total_credits)

write_csv(credits2, paste0(save_path, "renewable_diesel_credits.csv"))


