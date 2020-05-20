# Master function that runs build_sdm() for a single species

source("./scripts/functions/build_sdm_as.R")

spec = commandArgs()[[6]]
filename = paste0("./data/split_data/", spec)

# Running a specific species
test_obj = build_sdm_as(filename = filename, 
                     env_raster = bv_t2,
<<<<<<< HEAD
		             cores = 14)
=======
                     cores = 14)
>>>>>>> 7b915eb8e8c945be06a2b7a0ecd9fc6eafdc69b2

# writing list out
file_out = paste0("./output/", spec)
saveRDS(test_obj, file_out)