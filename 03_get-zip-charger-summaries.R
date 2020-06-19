library(tidyverse)

# load block-level charger summaries
block_chargers <- read_rds("Data/wblk_chargers_1km.rds") %>% 
  mutate(tot_chargers = lvl1 + lvl2 + dcfc)

# block to zip
zip_to_wblk <- read_rds("Data/ca_lodes2017_hzip_to_wblock.rds") %>% 
  drop_na(home_zip) %>% 
  filter(str_starts(home_zip, "9"))

# match
zip_to_wblk_chargers <- zip_to_wblk %>% 
  inner_join(block_chargers, by = c("w_geocode" = "blk_geoid"))

# summarize ... want mean, and % with any
zip_charger_summaries <- zip_to_wblk_chargers %>% 
  group_by(home_zip) %>% 
  summarize(across(lvl1:tot_chargers,
                   list(mean = ~ sum(. * job_wt),
                        frac = ~ sum((. > 0) * job_wt))),
            .groups = "drop")
  
# write summaries
zip_charger_summaries %>% 
  write_rds("Outputs/homezip_workcharger_summaries.rds") %>% 
  write_csv("Outputs/homezip_workcharger_summaries.csv")
