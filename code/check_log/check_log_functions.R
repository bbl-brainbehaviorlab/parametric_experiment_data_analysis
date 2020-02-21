# this script includes functions for performing checks on the log files

library(tidyverse)

cbtable_path = "~/Dropbox/Working/01_EntrainmentProject/0_ExperimentParametric/0_Design/ParametricDesign_counterbalance_v4.ods"

deviant_order_definition <- list(c(1,2),c(2,1))
regularity_order_definition <- list(c(1,2),c(2,1))
interval_order_definition <- list(c(1,2,16,4),c(2,4,1,16),c(4,16,2,1),c(16,1,4,2))

cbtable <- readODS::read_ods(cbtable_path)



cleanColName <- function(df){
  colnames(df) = gsub(".str.", "", colnames(df))
  colnames(df) = gsub(".num.", "", colnames(df))
  return(df)
}

### functions for checking ####

check_block_num <-  function(df) {
  if (df$blockIndex %>% unique() %>% length()==16){
    return("TRUE. Block num == 16.")
  }
  else return("FALSE. Block num wrong.")
}

check_trial_num <- function(df){
  count <- df %>% group_by(blockIndex) %>% summarise(count = n())
  if (all(count$count == 512)) return("TRUE. trial num == 512") else return("FALSE. trial num != 512")
}

check_conditions <- function(df){
  check <- df %>% group_by(deviant_condition,regularity_condition,interval_condition) %>% summarize(count=n())
  
  res <- all(c(rle(check$deviant_condition)$lengths == c(8,8) ,
    rle(check$deviant_condition)$values == c(1,2) ,
    rle(check$regularity_condition)$lengths == c(4,4,4,4),
    rle(check$regularity_condition)$values == c(1,2,1,2),
    rle(check$interval_condition)$lengths == rep(1,16),
    rle(check$interval_condition)$values == rep(c(1,2,4,16),4)) ==TRUE)
  
  if (res == T) return("TRUE. Condition num correct.") else return("FALSE. Condition num wrong.")
}

get_cb_id <- function(df){
  df$counter_balance_id[1] %>% as.character() %>% as.numeric()
}

check_deviant_order <- function(df){
  cb_id <- get_cb_id(df)
  res <- all.equal(rle(df$deviant_condition)$values, deviant_order_definition[[cbtable$OrderDeviant[cb_id]]])
  if (res) return("TRUE. Deviant_order") else return("FALSE. deviant order")
}

check_regularity_order <- function(df){
  cb_id <- get_cb_id(df)
  res <- all.equal(rle(df$regularity_condition)$values, 
                   regularity_order_definition[[cbtable$OrderRegularity[cb_id]]] %>% rep(2))
  if (res) return("TRUE. Regularity_order") else return("FALSE. regularity order")
}

check_interval_order <- function(df){
  
  cb_id <- get_cb_id(df)
  res <- all.equal(rle(df$interval_condition)$values, 
                   interval_order_definition[[cbtable$OrderInterval[cb_id]]] %>% rep(4))
  if (res) return("TRUE. Interval_order") else return("FALSE. interval order")
}

# check number of deviant in each block
check_deviant_num <- function(df){
  res <- df %>% 
    group_by(blockIndex) %>% 
    summarize(count = sum(is_deviant)) %>% 
    {all(.$count == 64)}
  if (res) return("TRUE. DeviantNum == 64") else return("FALSE. deviant num")
}

#check each interval has equal chance of being deviant
check_deviant_equal_chance <- function(df){
  # check for each block
  for (i in 1:16) {
    res <- df %>% 
      filter(blockIndex == i) %>% 
      group_by(stimuli_file) %>% 
      summarize(count=sum(is_deviant)) %>% 
      {all(.$count == .$count[1])}
    
    if (res == F) return(paste0("FALSE. Block ",i, " intervals do not have equal chance of being deviant."))
  }
  
  return("TRUE. All intervals have equal chance of deviant.")
}

# check whether port code correctly reflect the conditions
check_port_code <- function(df){
  test <- df %>% 
    mutate(dev_code = substr(my_port_code,1,1), reg_code = substr(my_port_code,2,2), int_code = substr(my_port_code,3,3), isdev_code = substr(my_port_code,4,4), sound_code = substr(my_port_code,5,6) %>% as.numeric()) %>% 
    mutate(int_code = case_when(int_code == "9" ~ "16",
                                TRUE ~ int_code))
  res <- all(c(all(test$deviant_condition == test$dev_code),
        all(test$regularity_condition == test$reg_code),
        all(test$interval_condition == test$int_code),
        all(test$is_deviant == test$isdev_code),
        all(test$stimuli_file == test$sound_code)
        )==TRUE)
  
  if (res) return("TRUE. Port code == conditions") else return("FALSE. Port code != conditions")
}


run_all_check <- function(df){
  message("====== Current participant: ", get_cb_id(df), " =========")
  message(df %>% check_block_num())
  message(df %>% check_trial_num())
  message(df %>% check_conditions())
  message(df %>% check_deviant_order())
  message(df %>% check_regularity_order())
  message(df %>% check_interval_order())
  message(df %>% check_deviant_num())
  message(df %>% check_deviant_equal_chance())
}



#### Standards preceeding deviant
mean_num_preceeding_standard <- function(df) {
  cbid <- c()
  deviant_conditon <- c()
  regularity_condition <- c()
  interval_condition <- c()
  mean_preceeding <- c()
  sd_preceeding <- c()
  min_preceeding <- c()
  max_preceeding <- c()
  # mean number of standards preceeding deviant
  for (z in c(1,2)){
    for(j in c(1,2)){
      for (i in c(1,2,4,16)){
        preceeding <- df %>% 
          filter(deviant_condition==z & regularity_condition==j &interval_condition==i) %>% 
          {
            rle(.$is_deviant) %>% unclass() %>% data.frame() %>% 
              filter(values != 1)  # get rid of the deviant, only get preceedings
            
          }
        cbid <- c(get_cb_id(df))
        deviant_conditon <- c(deviant_conditon,z)
        regularity_condition <- c(regularity_condition,j)
        interval_condition <- c(interval_condition,i)
        mean_preceeding <- c(mean_preceeding,mean(preceeding$lengths[1:nrow(preceeding)-1]) )
        min_preceeding <- c(min_preceeding,range(preceeding$lengths[1:nrow(preceeding)-1])[1] )
        max_preceeding <- c(max_preceeding,range(preceeding$lengths[1:nrow(preceeding)-1])[2] )
        sd_preceeding <- c(sd_preceeding,sd(preceeding$lengths[1:nrow(preceeding)-1]))
        #message(i,": ",mean(preceeding$lengths[1:nrow(preceeding)-1]), "; range = ",range(preceeding$lengths[1:nrow(preceeding)-1]) %>% paste(collapse = ","))#do not get the last row because it may be break by the end of block
      }
    }
  }
  
  return(data.frame(
    cbid,deviant_conditon,regularity_condition,interval_condition,mean_preceeding,sd_preceeding,min_preceeding,max_preceeding
  ))
}