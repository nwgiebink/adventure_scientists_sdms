# Occurrence data for all western US
# taken from all_data.R in adventure_scientists repo
# 2020-3-3
# Noah Giebink


#packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggmap)
library(mapr)


# Data
west_as <- read.csv("data/west_as.csv")
west_iNat <- read.csv("data/west_iNat.csv")

# Prep data ----
# Remove adventure scientist observations from west_iNat
west_iNat_only <- filter(west_iNat, !(west_iNat$id %in% west_as$id))

#Candidate species: top 5 by number of records in each state:
#Adventure Scientists
top_5_west = west_as %>%
  filter(place_state_name != "") %>%
  filter(place_state_name %in% c("Washington", "Utah", 
                                 "Montana", "California", 
                                 "Arizona")) %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  top_n(n = 5, wt = num_records) %>%
  arrange((place_state_name), desc(num_records))


#All species with at least 20 observations in iNat alone, by state
iNat_coverage_west <- west_iNat_only %>%
  filter(place_state_name != "") %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  filter(num_records > 19) %>%
  arrange((place_state_name), desc(num_records))

# Choose candidates ----
# top species by state with acceptable AS & iNat coverage 
# df all_cand: data frame of dated lat/long observations by provider (as or iNat)
# top 5 AS in each state and any with more than 20 obs. in iNat only
shared_coverage_west <- top_5_west %>%
  ungroup() %>%
  mutate(place_state_name = as.character(place_state_name), 
         scientific_name = as.character(scientific_name)) %>%
  inner_join(iNat_coverage_west %>%
               ungroup() %>%
               mutate(place_state_name = as.character(place_state_name),
                      scientific_name = as.character(scientific_name)), by = "scientific_name") %>%
  select(scientific_name, iNat_data_state = place_state_name.y, num_records_iNat_data = num_records.y, 
         as_data_state = place_state_name.x, num_records_as_data = num_records.x) %>%
  filter(iNat_data_state == as_data_state) %>%
  select("Species" = scientific_name, 
         "State" = iNat_data_state, 
         "iNat_Records" = num_records_iNat_data,
         "AS_Records" = num_records_as_data)

# get distribution summary of proportion AS to iNat observations
# for each shared_coverage_west observation
prop <- shared_coverage_west$AS_Records/shared_coverage_west$iNat_Records
summary(prop)

# remove observations where proportion of AS/iNat obs are below 1st quartile
cand <- filter(shared_coverage_west, AS_Records/iNat_Records > summary(prop)[[2]])

# get list of candidate species
cand_list <- unique(cand$Species)

# include only observations that are in cand_list

iNat_cand <- filter(west_iNat_only, scientific_name %in% cand_list) %>%
  dplyr::select(scientific_name, latitude, longitude, observed_on) %>%
  mutate(provider = 'iNat')
as_cand <- filter(west_as, scientific_name %in% cand_list) %>%
  dplyr::select(scientific_name, latitude, longitude, observed_on) %>%
  mutate(provider = 'as')
species_obs <- rbind(iNat_cand, as_cand)
write_csv(species_obs, 'data/species_obs.csv')
