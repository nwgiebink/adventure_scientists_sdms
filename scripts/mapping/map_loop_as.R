# looping through all species and outputting maps
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-01

#packages
library(stringr)

# loading env rasters
<<<<<<< HEAD
bv_t2 = readRDS("./data/bioclim_t2.rds")

#sourcing functions
# source("./script/mapping/make_map_time_compare.R)
source("./scripts/mapping/make_map_time_compare_threshold_as.R")
=======
bv_t1 = readRDS("./data/bioclim_t1.rds")
bv_t2 = readRDS("./data/bioclim_t2.rds")

#sourcing functions
source("./script/analysis_mapping/make_map_time_compare.R")
source("./script/analysis_mapping/make_map_time_compare_threshold.R")
>>>>>>> 7b915eb8e8c945be06a2b7a0ecd9fc6eafdc69b2

#getting list of files
full_list = list.files("./output", full.names = TRUE)
to_run = full_list[str_detect(full_list, ".rds")]

for(i in 1:length(to_run)){
<<<<<<< HEAD
  # make_map_time_compare(species_rds_out = to_run[i],
  #                       bv_t1, 
  #                       bv_t2)
  try(make_map_time_compare_threshold(species_rds_out = to_run[i],
                        bv_t2))                        
}
=======
  make_map_time_compare(species_rds_out = to_run[i],
                        bv_t1, 
                        bv_t2)
  make_map_time_compare_threshold(species_rds_out = to_run[i],
                        bv_t1, 
                        bv_t2)                        
}

# Had to manually do some
# make_map_time_compare(species_rds_out = "./output/vanessa_atalanta.rds", bv_t1, bv_t2)
# make_map_time_compare(species_rds_out = "./output/vanessa_cardui.rds", bv_t1, bv_t2)
# make_map_time_compare(species_rds_out = "./output/vanessa_virginiensis.rds", bv_t1, bv_t2)
# make_map_time_compare(species_rds_out = "./output/strymon_melinus.rds", bv_t1, bv_t2)
>>>>>>> 7b915eb8e8c945be06a2b7a0ecd9fc6eafdc69b2
