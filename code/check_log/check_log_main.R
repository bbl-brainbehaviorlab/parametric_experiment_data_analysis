library(tidyverse)
source(here::here("code/check_log/check_log_functions.R"))

path <-  "~/Dropbox/Working/01_EntrainmentProject/0_ExperimentParametric/03_ExperimentCoding/important_log_archive/pilot"
files <- list.files(path = path, full.names = F)

preceeding_standards_all <- NULL

for (file in files){
  raw <-  read.csv(file.path(path,file),skip=3)
  
  picture <- raw %>% 
    filter(Event.Type == "Picture" & counter_balance_id.str.!= "88888" & counter_balance_id.str.!= "Instruction_Page") %>% 
    mutate(duration_diff = Duration/10 - wav_file_duration.str.) %>% 
    cleanColName()
  
  picture %>% 
    run_all_check()
  
  preceeding_standards <- picture %>% 
    mean_num_preceeding_standard()
  
  preceeding_standards_all <- rbind(preceeding_standards_all, preceeding_standards)
}

