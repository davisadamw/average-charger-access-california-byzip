library(tidyverse)
library(sf)

# load relevant block centroids
ca_bl_centroid <- read_rds("Data/ca_wblock_cents.rds")

# load charger locations
chargers <- read_csv("Data/combined_afdc_plugshare.csv") %>% 
  mutate(tot_chargers = lvl2 + dcfc)

# project block centroids to 3310
ca_bl_centroidXY <- ca_bl_centroid %>% 
  st_as_sf(coords = c("blklondd", "blklatdd"), crs = 4326, remove = FALSE) %>% 
  st_transform(3310) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  st_drop_geometry()

# distance threshold of 1km?  ... returns list of vectors, with ids of all matches
get_frnn <- function(x_col, y_col, to_xy, eps = 1000) {
  neighbors <- dbscan::frNN(to_xy, eps, query = tibble({{ x_col }}, {{ y_col }}))
  neighbors$id
}

# match block group to chargers within 1km
ca_bl_centroidXY_matched <- ca_bl_centroidXY %>% 
  mutate(neighbs = get_frnn(X, Y, select(chargers, X, Y)))

# function for summing a vector based on indices if present
sum_by_index <- function(id, sum_vec, na.rm = TRUE) {
  if (length(id) == 0) return(0)
  
  sum(sum_vec[id], na.rm = na.rm)
}

#attach number of chargers here
ca_bl_centroidXY_chargers <- ca_bl_centroidXY_matched %>% 
  mutate(lvl1 = map_dbl(neighbs, sum_by_index, sum_vec = chargers$lvl1),
         lvl2 = map_dbl(neighbs, sum_by_index, sum_vec = chargers$lvl2),
         dcfc = map_dbl(neighbs, sum_by_index, sum_vec = chargers$dcfc),
         .keep = "unused")

ca_bl_centroidXY_chargers %>% 
  select(blk_geoid, lvl1:dcfc) %>% 
  write_rds("Data/wblk_chargers_1km.rds", compress = "gz")
