# adapted from my rural utilities script
library(tidyverse)

# read the crosswalk data, only the columns we care about (zip and block)
ca_blXzip <- read_csv("Data/ca_xwalk.csv.gz",
                      col_types = cols_only(zcta = "c", tabblk2010 = "c",
                                            blklatdd = "d", blklondd = "d")) %>% 
  rename(blk_geoid = tabblk2010) %>% 
  # only want valid zip codes and blocks in South Carolina
  filter(zcta != "99999",
         str_sub(blk_geoid, 1, 2) == "06")

attr(ca_blXzip, "spec") <- NULL

# and lodes
ca_lodes <- read_csv("Data/ca_od_main_JT00_2017.csv.gz", 
                     col_types = cols_only(w_geocode = "c", h_geocode = "c", S000 = "i"))

attr(ca_lodes, "spec") <- NULL

# need to attach zip code matching home block and collapse all unique home_zip:work_block combos
ca_lodes_zips <- ca_lodes %>% 
  left_join(select(ca_blXzip, blk_geoid, zcta), by = c("h_geocode" = "blk_geoid")) %>% 
  rename(home_zip = zcta) %>% 
  group_by(home_zip, w_geocode) %>% 
  summarize(jobs = sum(S000), .groups = "drop_last") %>% 
  # get the fraction of this zip codes jobs with each block as a destination
  mutate(job_wt = jobs / sum(jobs)) %>% 
  ungroup()

# get lat lon for all zip codes that appear as work destinations in LODES
ca_bl_centroid <- ca_blXzip %>% 
  semi_join(distinct(ca_lodes, w_geocode), by = c("blk_geoid" = "w_geocode")) %>% 
  select(blk_geoid, blklatdd, blklondd)

# write results to disk
ca_lodes_zips %>% 
  write_rds("Data/ca_lodes2017_hzip_to_wblock.rds", compress = "gz")

ca_bl_centroid %>% 
  write_rds("Data/ca_wblock_cents.rds", compress = "gz")
