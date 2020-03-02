# this script includes functions for performing checks on the log files

library(tidyverse)

cbtable_path = "~/Dropbox/Working/01_EntrainmentProject/0_ExperimentParametric/0_Design/ParametricDesign_counterbalance_v5.1.ods"

deviant_order_definition <- list(c(1,2),c(2,1))
regularity_order_definition <- list(c(1,2),c(2,1))
interval_order_definition <- list(c(1,2,16,4),c(2,4,1,16),c(4,16,2,1),c(16,1,4,2))
int2_variation_definition <- list(c(1,16),c(2,15),c(3,14),c(4,13),c(5,12),c(6,11),c(7,10),c(8,9))
int4_variation_definition <- list(c(1,6,11,16), c(4,7,10,13),c(7,8,9,10))

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


# Check whether the variation version is the same as cbtable
check_variation_version <- function(df){
  cb_id <- get_cb_id(df)
  int2 = all(df$int2_variation == cbtable$Interval2_variation[cb_id])
  int4 = all(df$int4_variation == cbtable$Interval4_variation[cb_id])
  if (all(c(int2,int4) == T)) return("TRUE. Variation version correct") else return("FALSE. Variation version incorrect")
}

# check whether the file used for the variation version is correct
check_variation_files <- function(df){
  cb_id <- get_cb_id(df)
  int2_files <- int2_variation_definition[[cbtable$Interval2_variation[cb_id]]]
  int4_files <- int4_variation_definition[[cbtable$Interval4_variation[cb_id]]]
  
  int2 <- df %>% 
    filter(interval_condition==2)
  int4 <- df %>% 
    filter(interval_condition==4)
  
  res2 <- all(gsub(".wav","",gsub("^.*_", "", int2$wav_file_name)) %in% int2_files == T)
  res4 <- all(gsub(".wav","",gsub("^.*_", "", int4$wav_file_name)) %in% int4_files == T)
  
  if (all(c(res2,res4) == TRUE)) return("TRUE. Variation files correct.") else return("FALSE. Variation files incorrect")
}

check_regular_sound_file_order <- function(df){
  cb_id <- get_cb_id(df)
  interval4_order_definition <- list(
    c(1,2,3,4),
    c(1,2,4,3),
    c(1,3,2,4),
    c(1,3,4,2),
    c(1,4,2,3),
    c(1,4,3,2)
  )
  interval16_order_definition <- list(
    c(1,2,6,5,7,16,9,12,15,13,4,3,8,11,10,14),
    c(9,4,12,6,1,2,13,10,8,14,5,11,15,3,16,7),
    c(4,10,7,8,12,11,9,13,6,16,3,5,15,1,2,14),
    c(5,2,4,7,12,8,10,9,15,16,3,6,1,14,11,13),
    c(13,16,2,12,7,8,11,6,4,9,10,1,15,5,3,14),
    c(16,9,7,8,14,10,4,15,5,11,3,1,12,13,2,6),
    c(10,9,11,6,5,8,14,4,7,16,13,2,1,3,12,15),
    c(12,2,11,16,7,3,8,5,9,4,13,10,1,6,14,15),
    c(13,9,7,5,4,15,8,1,2,6,3,16,11,10,14,12),
    c(7,15,13,6,11,10,9,12,16,1,3,14,5,4,8,2),
    c(4,2,8,7,16,6,5,11,1,15,10,3,13,12,9,14),
    c(13,9,14,10,2,15,4,1,16,5,3,6,11,12,8,7),
    c(2,11,1,14,12,3,10,6,16,8,7,15,5,9,4,13),
    c(1,4,13,7,12,14,15,8,2,3,5,9,16,11,6,10),
    c(11,16,4,9,1,14,10,5,8,7,12,2,6,15,3,13),
    c(4,6,2,9,14,7,10,16,11,3,5,15,13,1,8,12),
    c(6,2,9,13,12,7,3,11,16,10,4,14,1,15,5,8),
    c(10,9,16,15,11,2,14,3,12,5,1,6,4,7,8,13),
    c(3,10,1,16,14,7,12,9,8,15,6,13,4,5,2,11),
    c(10,8,1,16,7,15,9,5,4,13,2,11,12,6,14,3),
    c(15,1,4,6,13,2,5,11,3,7,14,16,8,9,12,10),
    c(7,13,9,14,10,11,4,15,6,1,5,16,8,2,3,12),
    c(13,11,4,10,7,9,16,12,15,14,3,2,6,8,1,5),
    c(14,4,13,16,10,11,2,7,15,5,6,12,1,9,8,3),
    c(14,13,16,1,6,9,8,4,11,15,10,5,12,2,7,3),
    c(6,1,12,13,15,9,2,16,8,5,4,7,3,11,14,10),
    c(13,16,5,2,14,9,10,12,11,7,6,15,4,8,1,3),
    c(10,9,7,12,5,6,13,4,14,8,15,2,3,16,11,1),
    c(2,1,4,3,9,10,16,11,14,6,8,15,12,13,7,5),
    c(13,2,15,10,11,8,5,7,4,12,9,16,14,6,3,1),
    c(9,12,3,5,1,16,4,15,14,2,10,8,13,7,6,11),
    c(10,14,3,12,11,16,8,15,2,4,1,6,7,5,13,9)
  )
  interval2_order <- c(1,2)
  interval4_order <- interval4_order_definition[[cbtable$Interval4_regularOrder[cb_id]]]
  interval16_order <- interval16_order_definition[[cbtable$Interval16_regularOrder[cb_id]]]
  
  int2 <- df %>% filter(interval_condition==2,regularity_condition == 1) 
  res2 <- all(int2$stimuli_file == rep(interval2_order,nrow(int2)/2))
  int4 <- df %>% filter(interval_condition==4,regularity_condition == 1) 
  res4 <- all(int4$stimuli_file == rep(interval4_order,nrow(int2)/4))
  int16 <- df %>% filter(interval_condition==16,regularity_condition == 1) 
  res16 <- all(int16$stimuli_file == rep(interval16_order,nrow(int16)/4))
  
  if (all(c(res2,res4,res16)==T)) return("TRUE. Sound files order in regular condition correct") else return("FALSE. Sound files order in regular condition incorrect")
  
  
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
  test <- df %>% 
    mutate(
      my_port_code = stringr::str_pad(as.character(my_port_code),3,pad="0"),
      dev_code = case_when(
        substr(my_port_code,3,3) %in% c("1","2","5","6") ~ "1",
        TRUE ~ "2"
      ),
      reg_code = case_when(
        substr(my_port_code,3,3) %in% c("1","2","3","4") ~ "1",
        TRUE ~ "2"
      ),
      isdev_code = case_when(
        substr(my_port_code,3,3) %in% c("1","3","5","7") ~ "0",
        TRUE ~ "1"
      ),
      int_code = case_when(
        as.numeric(substr(my_port_code,1,2)) < 2 ~ 1,
        as.numeric(substr(my_port_code,1,2)) < 4 ~ 2,
        as.numeric(substr(my_port_code,1,2)) < 8 ~ 4,
        TRUE ~ 16
      ),
      sound_code = case_when(
        int_code == 1 ~ 1,
        int_code == 2 ~ as.numeric(substr(my_port_code,1,2)) - 1,
        int_code == 4 ~ as.numeric(substr(my_port_code,1,2)) - 3,
        int_code == 16 ~ as.numeric(substr(my_port_code,1,2)) - 7,
      )
      )
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
  message(df %>% check_port_code())
  message(df %>% check_block_num())
  message(df %>% check_trial_num())
  message(df %>% check_conditions())
  message(df %>% check_deviant_order())
  message(df %>% check_regularity_order())
  message(df %>% check_interval_order())
  message(df %>% check_deviant_num())
  message(df %>% check_deviant_equal_chance())
  message(df %>% check_variation_version())
  message(df %>% check_variation_files())
  message(df %>% check_regular_sound_file_order())
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