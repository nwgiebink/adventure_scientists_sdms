# Making threshold maps
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-01

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

#Pulling in polygons for states and provinces
#Getting map data
usa = getData(country = 'USA', level = 1, path = "./data/")
#extract states (need to uppercase everything)
to_remove = c("Alaska", "Hawaii")

#filtering
usa = usa[-match(toupper(to_remove), toupper(usa$NAME_1)),]

#simplying polygons
simple_map_US = gSimplify(usa, tol = 0.01, topologyPreserve = TRUE)

#Pulling Canada Province data
can = getData(country = 'CAN', level = 1, path = "./data/")
simple_map_can = gSimplify(can, tol = 0.01, topologyPreserve = TRUE)

#Pulling Mexico data
mex = getData(country = 'MEX', level = 1, path = "./data/")
simple_map_mex = gSimplify(mex, tol = 0.01, topologyPreserve = TRUE)

# environmental raster
bv_t2 = readRDS("./data/bioclim_t2.rds")

# Function
make_map_time_compare_threshold = function(species_rds_out, 
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
    filter(value > thresh_inat)
  
  all_threshold = pred_all_df %>%
    filter(value > thresh_all)
  
  
  # Plotting
  # 
  # inat
  g1 = ggplot() +  
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color=NA, size=0.25, fill = "#440154FF") +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), 
                 color = NA, size = 0.25, fill = "#440154FF") +
    geom_polygon(data = simple_map_mex, aes(x = long, y = lat, group = group), 
                 color = NA, size = 0.25, fill = "#440154FF") +
    geom_tile(data=inat_threshold, aes(x=x, y=y), fill = "lightgrey") + 
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color="grey50", size=0.20, fill = NA) +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), 
                 color = "grey50", size = 0.20, fill = NA) +
    geom_polygon(data = simple_map_mex, aes(x = long, y = lat, group = group), 
                 color = "grey50", size = 0.20, fill = NA) +
    scale_fill_viridis(name = "Probability of Occurence") +
    theme(legend.position="right") +
    theme(legend.key.width=unit(2, "cm"),
          plot.title = element_text(hjust = 0.5, size = 24)) +
    theme_nothing(legend = TRUE) +
    coord_quickmap() +
    ggtitle("iNaturalist Only")
  
  #all
  g2 = ggplot() +  
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color=NA, size=0.25, fill = "#440154FF") +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), 
                 color = NA, size = 0.25, fill = "#440154FF") +
    geom_polygon(data = simple_map_mex, aes(x = long, y = lat, group = group), 
                 color = NA, size = 0.25, fill = "#440154FF") +
    geom_tile(data=all_threshold, aes(x=x, y=y), fill = "lightgrey") + 
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color="grey50", size=0.20, fill = NA) +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), 
                 color = "grey50", size = 0.20, fill = NA) +
    geom_polygon(data = simple_map_mex, aes(x = long, y = lat, group = group), 
                 color = "grey50", size = 0.20, fill = NA) +
    scale_fill_viridis(name = "Probability of Occurence") +
    theme(legend.position="right") +
    theme(legend.key.width=unit(2, "cm"),
          plot.title = element_text(hjust = 0.5, size = 24)) +
    theme_nothing(legend = TRUE) +
    coord_quickmap() +
    ggtitle("iNaturalist + Adventure Scientist")
  
  gfull = ggarrange(g1, g2, common.legend = TRUE)
  gfull = annotate_figure(gfull,
                          top = text_grob(species, face = "italic", size = 22))
  plotname = paste0("./output/thresh_maps/", species,"_thresh", ".png")
  print(species)
  print(plotname)
  ggsave(plotname, gfull)
  
}


# Testing
<<<<<<< HEAD
# make_map_time_compare_threshold(species_rds_out = "./output/aglais_milberti.rds",
#                        bv_t2)
=======
make_map_time_compare_threshold(species_rds_out = "./output/aglais_milberti.rds",
                       bv_t2)
>>>>>>> 7b915eb8e8c945be06a2b7a0ecd9fc6eafdc69b2
