# Pruning original dataset from logan app

library(tidyverse)

# Download Waquoit Bay data using SWMPr

# Plot recent years

# Pull app data

# Prune app data

oxygen <- readRDS("data/diel_oxygen_2023_2024.RDS")

dat_prune <- oxygen %>% 
  select(-ATemp) %>%
  filter(minute(DateTimeStamp2) %in% c(0, 30)) %>%
  mutate(station_name = factor(station_name, levels = c("Sage Lot", "Childs River")))

saveRDS(dat_prune, "data/diel_oxygen_30min.RDS")

write_csv(dat_prune, "data/diel_oxygen_30min.csv")
