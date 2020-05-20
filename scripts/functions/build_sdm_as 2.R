# build_sdm 2.0
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-28  

# packages
require(raster)
require(tidyverse)
require(parallel)

# List of operations and associated functions
# 1. Prepping data - prep_data
source("./scripts/functions/prep_data_as.R")
# 2. Running blockCV - run_block_cv
source("./scripts/functions/run_block_cv_as.R")
# 3. Prepping Data more - prep_data_2
source("./scripts/functions/prep_data_2_as.R")
# 4. Training and testing split - train_test_split
source("./scripts/functions/train_test_split_as.R")
# 5. Modeling - model_func
source("./scripts/functions/model_func_as.R")
# 6. Choosing the best model - best_mod
source("./scripts/functions/best_mod_as.R")
# 7. Evaluating the best model - evaluate_models
source("./scripts/functions/evaluate_models_as.R")
# 8. Extracting arguments from the best model - make_args
source("./scripts/functions/make_args_as.R")
# 9. Building the full model on all data - full_model
source("./scripts/functions/full_model_as.R")

# importing env raster into the workspace
bv_t2 = readRDS("./data/bioclim_t2.rds")

#' build_sdm - master function for building temporally explicit models
#' Goes through data manipulation and year-splitting, blockCV, training and 
#' testing splits, model development and evaluation.
#'
#' @param filename the path of the single-species dataframe as a .rds file
#' @param env_raster_t1 the environmental raster associated with the first 
#' time period
#' @param env_raster_t2 the environmental raster associated with the second 
#' time period
#' @param year_split what year you want to split the data by (e.g. if you 
#' pick 2000, your two groups would years up to 1999, and 2000 to current)
#' @param full_or_minimal parameter for selecting whether you want the full 
#' complement of objects returned, or just the raw data, evaluation metrics, 
#' full models for each time period. 
#'
#' @return a list (either 9 units long (full), or 3 units long (minimal))
#' @export
#'
#' @examples
build_sdm_as = function(filename, 
                     env_raster,
                     full_or_minimal = "full",
                     cores = NULL){
  
  # Setting seed for reproducibility
  set.seed(42)
  
  # Reading in the data
  raw_data = readRDS(filename)
  
  # Prepping Data
    prepped_data = prep_data(data = raw_data, env_raster = env_raster)
    
  # Creating the master list to feed stuff into - nice to have it in one place
    master_list = list("raw_data" = raw_data, "prepped_data" = prepped_data)
    rm(raw_data)
    rm(prepped_data)
    
  # Block CV for each data set
    block_inat = try(run_block_cv(prepped_data = master_list[[2]][[1]][[1]], 
                                  bv_raster = master_list[[2]][[2]]))
    block_all = try(run_block_cv(prepped_data = master_list[[2]][[1]][[2]], 
                                  bv_raster = master_list[[2]][[2]]))
  # writing block objects to data list
    master_list$block_objs = list("block_inat" = block_inat, 
                                  "block_all" = block_all)
    rm(block_inat)
    rm(block_all)
    
  # Second round of data prep - 
    prepped_2_inat = try(prep_data_2(data = master_list$prepped_data$data$inat, 
                                    env_raster = master_list$prepped_data$env_data_cropped))
    prepped_2_all = try(prep_data_2(data = master_list$prepped_data$data$all, 
                                   env_raster = master_list$prepped_data$env_data_cropped))
    master_list$extra_prepped = list("extra_prepped_inat" = prepped_2_inat, 
                                     "extra_prepped_all" = prepped_2_all)
    rm(prepped_2_inat)
    rm(prepped_2_all)
    
  # Training and testing split  
    training_list_inat = try(train_test_split(master_list$extra_prepped$extra_prepped_inat,
                                            blocked_obj = master_list$block_objs$block_inat))
    training_list_all = try(train_test_split(master_list$extra_prepped$extra_prepped_all,
                                            blocked_obj = master_list$block_objs$block_all))
    
  # writing training and test data to master list  
    master_list$train_test = list("train_test_inat" = training_list_inat,
                                  "train_test_all" = training_list_all)
    rm(training_list_inat)
    rm(training_list_all)
    
  # Setting up internal parallelization within a single species for model 
  # building
  if(is.null(cores)){
    total_cores = parallel::detectCores()
    to_use = total_cores - 2
    doParallel::registerDoParallel(to_use)
  } else {
    to_use = cores
    doParallel::registerDoParallel(to_use)
  }
    
  # Modeling
    models_inat = try(model_func(data = master_list$train_test$train_test_inat$training_data,
                                 env_raster = master_list$prepped_data$env_data_cropped,
                                 num_cores = to_use))
    models_all = try(model_func(data = master_list$train_test$train_test_all$training_data, 
                                 env_raster = master_list$prepped_data$env_data_cropped,
                                 num_cores = to_use))
  # Writing to master list
    master_list$model_objs = list("models_inat" = models_inat, 
                                    "models_all" = models_all)
    rm(models_inat)
    rm(models_all)

  # Model selection
    best_mod_inat = try(best_mod(model_obj = master_list$model_objs$models_inat))
    best_mod_all = try(best_mod(model_obj = master_list$model_objs$models_all))
    
    master_list$best_mods = list("best_mod_inat" = best_mod_inat, 
                                 "best_mod_all" = best_mod_all)
    rm(best_mod_inat)
    rm(best_mod_all)
    
  # evaluating models on test data
    ev_inat = try(evaluate_models(test_data = master_list$train_test$train_test_inat$test_data,
                                model = master_list$best_mods$best_mod_inat[[1]],
                                env_raster = master_list$prepped_data$env_data))
    ev_all = try(evaluate_models(test_data = master_list$train_test$train_test_all$test_data,
                                model = master_list$best_mods$best_mod_all[[1]],
                                env_raster = master_list$prepped_data$env_data))
  # Writing evaluate objects to master list  
    master_list$eval_objs = list("eval_inat" = ev_inat, 
                                 "eval_all" = ev_all)
    rm(ev_inat)
    rm(ev_all)
    
    
  # Building full models on all data
    full_mod_inat = (full_model(models_obj = master_list$model_objs$models_inat,
                                 best_model_index = master_list$best_mods$best_mod_inat[[2]],
                                 full_data = master_list$extra_prepped$extra_prepped_inat,
                                 env_raster = master_list$prepped_data$env_data_cropped))
    
    full_mod_all = try(full_model(models_obj = master_list$model_objs$models_all,
                                 best_model_index = master_list$best_mods$best_mod_all[[2]],
                                 full_data = master_list$extra_prepped$extra_prepped_all,
                                 env_raster = master_list$prepped_data$env_data_cropped))
    
  # Writing full model objs to master list  
    master_list$full_mods = list("full_mod_inat" = full_mod_inat, 
                                 "full_mod_all" = full_mod_all)
    rm(full_mod_inat)
    rm(full_mod_all)
    
  # slimming down list if minimal is selected in function argument
    if(full_or_minimal == "minimal"){
      master_list = master_list[[c(1,8,9)]]
    }
    
    return(master_list)
}


