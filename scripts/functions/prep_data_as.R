# Prepping Occurence Data for Modeling
# Keaton Wilson
# keatonwilson@me.com
# 2019-11-25

# packages
require(tidyverse)
require(raster)
require(dismo)
require(sp)

#' Initial prepping of occurence data to create SDMs
#'
#' @param data A dataframe outputted from the butt_obs script. Generated from 
#' \code{\link[spocc]{occ}}
#' @param env_raster The env raster associated with the given time frame
#'
#' @return A list two elements long: the first is presence data converted to a 
#' spatial points data frame.The second items is an env raster cropped 
#'  to the area of the occurences.
#'
#' @examples
prep_data = function(data, env_raster) {
  
  # selecting the pieces we want and separating by time
  # Mutating with name changes to match stuff down the line
  small_data = data %>%
    rename(name = scientific_name, date = observed_on) %>%
    dplyr::select(name, longitude, latitude, date, provider)
  
  # calculating extent of occurences
  max_lat = ceiling(max(small_data$latitude))
  min_lat = floor(min(small_data$latitude))
  max_lon = ceiling(max(small_data$longitude))
  min_lon = floor(min(small_data$longitude))
  
  # added a 1º buffer in every direction
  geographic_extent <- extent(x = c(min_lon-1, max_lon+1, min_lat-1, max_lat+1))
  
  # Crop bioclim data to geographic extent of species
  env_raster_cropped <- crop(x = env_raster, y = geographic_extent)
  
  # splitting into the two data sets
  df_inat = small_data %>% 
    filter(provider == "iNat")
  
  df_all = small_data
  
  # Generate 10k background points for each one. 
  bg_inat = dismo::randomPoints(bv_t2, 10000)
  colnames(bg_inat) = c("longitude", "latitude")
  
  bg_all = randomPoints(bv_t2, 10000)
  colnames(bg_all) = c("longitude", "latitude")
  
  # Merging background data and occurence data
  df_comb_inat = data.frame(df_inat) %>%
    mutate(pb = 1) %>%
    dplyr::select(pb, longitude, latitude) %>%
    bind_rows(data.frame(bg_inat) %>% 
                mutate(pb = 0))  %>%
    mutate(Species = as.integer(pb)) %>%
    dplyr::select(-pb)
  
  df_comb_all = data.frame(df_all) %>%
    mutate(pb = 1) %>%
    dplyr::select(pb, longitude, latitude) %>%
    bind_rows(data.frame(bg_all) %>% 
                mutate(pb = 0)) %>%
    mutate(Species = as.integer(pb)) %>%
    dplyr::select(-pb)
  
  # Changing to a spatial points data frame
  df_sp_inat = SpatialPointsDataFrame(df_comb_inat[,c("longitude","latitude")], 
                                    df_comb_inat, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  df_sp_all = SpatialPointsDataFrame(df_comb_all[,c("longitude","latitude")], 
                                  df_comb_all, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #Converting to a list with the two dataframes
  prepared_data_list = list(data = list(inat = df_sp_inat, all = df_sp_all),
                            env_data_cropped = env_raster_cropped)
  #Names
  bio_names = c()
  for(i in 1:19){
    bio_names[i] = paste0("Bio", i)
  }
  names(prepared_data_list[[2]]) = bio_names
  
  return(prepared_data_list)
}
