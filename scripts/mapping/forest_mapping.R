# Mapping National Forest boundaries test case (Aglais milberti in MT)
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-14

# packages
library(tidyverse)
library(lubridate)
library(maxnet)
library(ENMeval)
library(sp)
library(raster)
library(maptools)
library(ggmap)
library(viridis)
library(ggthemes)
library(rgeos)
library(maps)
library(dismo)
library(splancs)
library(ggpubr)

#Getting map data
usa = getData(country = 'USA', level = 1, path = "./data/")
#extract states (need to uppercase everything)
to_remove = c("Alaska", "Hawaii")

#filtering
montana = usa[match(toupper("Montana"), toupper(usa$NAME_1)),]

#simplying polygons
simple_map_MT = gSimplify(montana, tol = 0.01, topologyPreserve = TRUE)

# loading in list obj with most of the information we need
occs = read_csv("./data/species_obs.csv") 
spec_list = readRDS("./output/aglais_milberti.rds")
forest_serv = rgdal::readOGR("./data/S_USA.AdministrativeForest.shp")
forest_to_map = subset(forest_serv, REGION=="01")

#species name
species_rds_out = "./output/aglais_milberti.rds"
species = str_remove(str_to_sentence(str_replace(str_remove(str_remove(species_rds_out, 
                                                                       "_output.rds"), 
                                                            "./output/"), 
                                                 "_", " ")
), ".rds")

#setting env raster
env_raster = readRDS("./data/bioclim_t2.rds")

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
pred_inat = predict(object = spec_list$full_mods$full_mod_inat,
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
pred_all = predict(object = spec_list$full_mods$full_mod_all,
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

# mapping
g1 = ggplot() +  
  geom_polygon(data = forest_to_map, 
               aes(x = long, 
                   y = lat, 
                   group = group), 
               fill = "cornsilk", 
               color = "grey75", 
               size = 0.25) +
  geom_polygon(data=simple_map_MT, 
               aes(x=long, y=lat), 
               fill = NA, 
               color = "grey50") +
  geom_tile(data = inat_threshold, 
            aes(x = x, y = y), 
            fill = "deepskyblue",
            alpha = 0.4) +
  geom_point(data = spec_list$raw_data, 
             aes(x = longitude, 
                 y = latitude, 
                 color = provider), 
             size = 1) +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, 
                                  size = 18)) +
  theme_nothing(legend = TRUE) +
  theme(legend.position="right", plot.margin = unit(c(0.25,0.25,0.25,0.25), "inches")) +
  coord_quickmap(xlim = c(extent(simple_map_MT)[1]-0.1, 
                          extent(simple_map_MT)[2]+0.1), 
                   ylim = c(extent(simple_map_MT)[3]-0.1, 
                            extent(simple_map_MT)[4]+0.1)) +
  scale_color_discrete(name = "Data Provider", 
                       labels = c("Adventure Scientists", 
                                  "iNaturalist")) +
  ggtitle("iNaturalist Only")

#all
g2 = ggplot() +  
  geom_polygon(data = forest_to_map, 
               aes(x = long, 
                   y = lat, 
                   group = group), 
               fill = "cornsilk", 
               color = "grey75", 
               size = 0.25) +
  geom_polygon(data=simple_map_MT, 
               aes(x=long, y=lat), 
               fill = NA, 
               color = "grey50") +
  geom_tile(data = all_threshold, 
            aes(x = x, y = y), 
            fill = "deepskyblue",
            alpha = 0.4) +
  geom_point(data = spec_list$raw_data, 
             aes(x = longitude, 
                 y = latitude, 
                 color = provider), 
             size = 1) +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, 
                                  size = 18)) +
  theme_nothing(legend = TRUE) +
  theme(legend.position="right", plot.margin = unit(c(0.25,0.25,0.25,0.25), "inches")) +
  coord_quickmap(xlim = c(extent(simple_map_MT)[1]-0.1, 
                          extent(simple_map_MT)[2]+0.1), 
                 ylim = c(extent(simple_map_MT)[3]-0.1, 
                          extent(simple_map_MT)[4]+0.1)) +
  scale_color_discrete(name = "Data Provider", 
                       labels = c("Adventure Scientists", 
                                  "iNaturalist")) +
  ggtitle("iNaturalist Only + Adventure Scientist")

gfull = ggarrange(g1, g2, common.legend = TRUE, ncol = 1, nrow =2)
gfull = annotate_figure(gfull,
                        top = text_grob(species, face = "italic", size = 22))
plotname = paste0("./output/thresh_maps/", species,"_thresh", ".png")
print(species)
print(plotname)
ggsave(plotname, gfull)
