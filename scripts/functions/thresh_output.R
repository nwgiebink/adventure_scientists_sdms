# outputting threshold objects
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-20

# packages
library(tidyverse)
library(lubridate)
library(sp)
library(raster)
library(maptools)
library(ggmap)
library(viridis)
library(ggthemes)
library(rgeos)
library(maps)
library(ggpubr)
library(ENMeval)
library(maxnet)
library(stringr)
library(dismo)



# Geographic Mapping Data ---------------------------------------


# environmental raster
bv_t2 = readRDS("./data/bioclim_t2.rds")

# Function
save_thresholds = function(species_rds_out, 
                                           env_raster){
  
  # reading in list of stuff
  spec_list = readRDS(species_rds_out)
  
  #Cropping data to actual occurences
  max_lat = ceiling(max(spec_list$raw_data$latitude))
  min_lat = floor(min(spec_list$raw_data$latitude))
  max_lon = ceiling(max(spec_list$raw_data$longitude))
  min_lon = floor(min(spec_list$raw_data$longitude))
  
  # added a 1º buffer in every direction
  geographic_extent <- extent(x = c(min_lon-1, max_lon+1, min_lat-1, max_lat+1))
  
  # Crop bioclim data to geographic extent of species
  env_raster_cropped <- crop(x = env_raster, y = geographic_extent)
  
  # Create new data to predict on
  newdata = as(env_raster_cropped, "SpatialPixelsDataFrame")
  newdata = as.data.frame(newdata) %>%
    drop_na()
  
  names = c(names(spec_list$prepped_data$env_data), "x", "y")
  names(newdata) = names
  
  # predictions
  pred_inat = dismo::predict(object = spec_list$full_mods$full_mod_inat,
                             newdata = newdata,
                             x = spec_list$prepped_data$env_data,
                             ext = spec_list$prepped_data$env_data@extent,
                             type = "cloglog")
  
  if(class(pred_inat) == "RasterLayer"){
    pred_inat_df = as(pred_inat, "SpatialPixelsDataFrame")
    pred_inat_df = as.data.frame(pred_inat_df) %>%
      drop_na()
    
    colnames(pred_inat_df) = c("value", "x", "y")
    
  } else {
    pred_inat_df = newdata %>%
      dplyr::select(x, y) %>%
      cbind(as.data.frame(pred_inat)) %>%
      as_tibble() 
    
    colnames(pred_inat_df) = c("x", "y", "value")
  }
  
  #all Predictions
  pred_all = dismo::predict(object = spec_list$full_mods$full_mod_all,
                            newdata = newdata,
                            x = spec_list$prepped_data$env_data,
                            ext = spec_list$prepped_data$env_data@extent,
                            type = "cloglog")
  
  if(class(pred_all) == "RasterLayer"){
    pred_all_df = as(pred_all, "SpatialPixelsDataFrame")
    pred_all_df = as.data.frame(pred_all_df) %>%
      drop_na()
    
    colnames(pred_all_df) = c("value", "x", "y")
    
  } else {
    pred_all_df = newdata %>%
      dplyr::select(x, y) %>%
      cbind(as.data.frame(pred_all)) %>%
      as_tibble() 
    
    colnames(pred_all_df) = c("x", "y", "value")
  }
  
  # Extracting the species name
  species = str_remove(str_to_sentence(str_replace(str_remove(str_remove(species_rds_out, 
                                                                         "_output.rds"), 
                                                              "./output/"), 
                                                   "_", " ")
  ), ".rds")
  
  # loading evaluation objs and thresholds
  eval_inat = spec_list$eval_objs$eval_inat
  eval_all = spec_list$eval_objs$eval_all
  
  thresh_inat = threshold(eval_inat, 'spec_sens')
  thresh_all = threshold(eval_all, 'spec_sens')
  
  # thresholded filter
  inat_threshold = pred_inat_df %>%
    mutate(ifelse(value > thresh_inat, 1, 0))
  
  all_threshold = pred_all_df %>%
    mutate(ifelse(value > thresh_all, 1, 0))
  
  saveRDS(inat_threshold, file = paste0("./output/thresh_maps/", species, "_inat_thresh.rds"))
  saveRDS(all_threshold, file = paste0("./output/thresh_maps/", species, "_all_thresh.rds"))
  
}

files = list.files("./output/", full.names = TRUE)
to_run = files[str_detect(files, ".rds")]
to_run = to_run[-15]

for(i in 1:length(to_run)){
  save_thresholds(species_rds = to_run[i], env_raster = bv_t2)
}
