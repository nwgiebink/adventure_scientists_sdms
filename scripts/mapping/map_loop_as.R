# looping through all species and outputting maps
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-01

#packages
library(stringr)

# loading env rasters
bv_t2 = readRDS("./data/bioclim_t2.rds")

#sourcing functions
# source("./script/mapping/make_map_time_compare.R)
source("./scripts/mapping/make_map_time_compare_threshold_as.R")

#getting list of files
full_list = list.files("./output", full.names = TRUE)
to_run = full_list[str_detect(full_list, ".rds")]

for(i in 1:length(to_run)){
  # make_map_time_compare(species_rds_out = to_run[i],
  #                       bv_t1, 
  #                       bv_t2)
  try(make_map_time_compare_threshold(species_rds_out = to_run[i],
                        bv_t2))                        
}