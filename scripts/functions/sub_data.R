# Subset observations 
# Noah Giebink
# 2020-3-24

# Packages
require(tidyverse)
require(caret)


#' sub_data
#' 
#' @param data species observations labeled by provider (see Set A below).
#' Requires column: data$provider)
#' 
#' @return a list of two elements: the first (Set 'B') is data only from the 
#' first provider (e.g. 'iNat') and the second is a sample of the set 
#' provided in param data, stratified by provider (e.g. 'iNat' and 'as')
#'  
#' 
#' Set Theory description: 
#' provider_a, provider_b ∈ A
#' B ⊂ A : provider_b ∉ B
#' C ⊂ A : provider_a2 ⊂ provider_a, provider_b2 ⊂ provider_b,
#'         provider_a2, provider_b2 ∈ C,
#'        P(provider_a2 in C) ~= P(provider_a in A), 
#'        P(provider_b2 in C) ~= P(provider_b in A),
#'        |C| ~= |B|


sub_data <- function(data){
prov <- unique(data$provider)
B <- filter(data, provider == prov[1])
part <- createDataPartition(data$provider, 
                    p = length(which(data$provider == prov[1]))/length(data$provider),
                    list = FALSE)
C <- data[part,]
return(list(B,C))
}

# # test sub_data ----
# species_obs <- read_csv('data/species_obs.csv')
# po <- filter(species_obs, scientific_name == 'Polygonia gracilis')
# sub_test <- sub_data(data = po)
# 
# # Does output match Set Theory description (above)?
# # Proportion from each provider should be approximately
# # equal in original 'pre' and subsetted data 'post'
# pre <- group_by(po, provider) %>% 
#   summarise(obs = n()) %>% 
#   mutate(prop = obs/nrow(po))
# post <- group_by(sub_test[[2]], provider) %>% 
#   summarise(obs = n()) %>% 
#   mutate(prop = obs/nrow(sub_test[[2]]))
# post2 <- group_by(sub_test[[1]], provider) %>% 
#   summarise(obs = n())
# all.equal(pre$prop,post$prop)
# all.equal(pre[2,]$obs,post2$obs)
#   # looks good!
# nrow(sub_test[[1]])
# nrow(sub_test[[2]])
# # RESULT: output matches Set Theory description (above)
# 
# 
# # make sure 'iNat' and 'as' have same unique() index each time
# s_list <- split(species_obs, f = species_obs$scientific_name)
# for (s in s_list) {
#   print(unique(s$provider))
# }
#   # result: they do