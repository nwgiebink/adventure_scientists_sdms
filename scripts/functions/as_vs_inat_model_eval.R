# Model comparison
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-20

# packages
library(tidyverse)
library(dismo)
library(gt)

# loading in one object
test = readRDS("./output/aglais_milberti.rds")

#extracting useful metrics
eval = test$eval_objs
inat_auc = eval$eval_inat@auc
as_auc = eval$eval_inat@auc

inat_thresh = threshold(eval$eval_inat, 'spec_sens')
as_thresh = threshold(eval$eval_all, 'spec_sens')

t_confusion_inat = data.frame(cbind(t = eval$eval_inat@t, eval$eval_inat@confusion))
confusion_at_thresh_inat = filter(t_confusion_inat, t == inat_thresh)

t_confusion_as = data.frame(cbind(t = eval$eval_all@t, eval$eval_all@confusion))
confusion_at_thresh_as = filter(t_confusion_as, t == as_thresh)

inat_recall = confusion_at_thresh_inat$tp/(confusion_at_thresh_inat$tp+confusion_at_thresh_inat$fn)
as_recall = confusion_at_thresh_as$tp/(confusion_at_thresh_as$tp+confusion_at_thresh_as$fn)

# loop through all data
files = list.files("./output/", full.names = TRUE)
to_run = files[str_detect(files, ".rds")]
to_run = to_run[-15]

evals = data.frame()
for(i in 1:length(to_run)){
  data = readRDS(to_run[i])
  species_name = str_remove(to_run[i], "./output//") %>% str_remove(".rds")
  eval = data$eval_objs
  
  inat_auc = eval$eval_inat@auc
  as_auc = eval$eval_all@auc
  
  inat_thresh = threshold(eval$eval_inat, 'spec_sens')
  as_thresh = threshold(eval$eval_all, 'spec_sens')
  
  t_confusion_inat = data.frame(cbind(t = eval$eval_inat@t, eval$eval_inat@confusion))
  confusion_at_thresh_inat = filter(t_confusion_inat, t == inat_thresh)
  
  t_confusion_as = data.frame(cbind(t = eval$eval_all@t, eval$eval_all@confusion))
  confusion_at_thresh_as = filter(t_confusion_as, t == as_thresh)
  
  inat_recall = confusion_at_thresh_inat$tp/(confusion_at_thresh_inat$tp+confusion_at_thresh_inat$fn)
  as_recall = confusion_at_thresh_as$tp/(confusion_at_thresh_as$tp+confusion_at_thresh_as$fn)
  
  out = data.frame(species = species_name, 
                   inat_auc = inat_auc, 
                   as_auc = as_auc, 
                   inat_recall = inat_recall,
                   as_recall = as_recall, 
                   num_occ = dim(data$raw_data[1]))
  
  evals = rbind(evals, out)
  print(paste('Finished species:', species_name))
}

eval_table = evals %>% 
  filter(num_occ != 1) %>%
  arrange(desc(num_occ)) %>%
  mutate(bin = ntile(num_occ, 3)) %>%
  mutate(species = str_to_sentence(str_replace(species, "_", " "))) %>%
  gt(rowname_col = "species") %>%
  tab_stubhead(label = "Species") %>%
  tab_header(title = md("Model Evaluation Comparison"), 
             subtitle = "iNaturalist versus iNaturalist + Adventure Scientists") %>%
  cols_label(inat_auc = "iNaturalist AUC", 
             as_auc  = "iNaturalist + AS AUC", 
             inat_recall = "iNaturalist Recall", 
             as_recall = "iNaturalist + AS Recall", 
             species = "Species", 
             num_occ = "Number of Occurrences", 
             bin = "Bin") %>%
  tab_style(style = list(cell_text(style = "italic")), 
            locations = cells_stub())

gtsave(eval_table, "./output/eval_summary_table.pdf")

# paired t.tests by bin
to_test = evals %>% 
  filter(num_occ != 1) %>%
  arrange(desc(num_occ)) %>%
  mutate(bin = ntile(num_occ, 3))

split_data = split(to_test, to_test$bin)

big_obs = t.test(split_data$`3`$inat_auc, split_data$`3`$as_auc, paired = TRUE)
med_obs = t.test(split_data$`2`$inat_auc, split_data$`2`$as_auc, paired = TRUE)
small_obs = t.test(split_data$`1`$inat_auc, split_data$`1`$as_auc, paired = TRUE)
