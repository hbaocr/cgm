# read_data_utils.R
# generalized functions to read data, either from disk or from databases
#

# Note: only works with p4mi database (or any db that includes a user_id)
# Sys.setenv(R_CONFIG_ACTIVE = "p4mi")

library(tidyverse)
library(lubridate)

# returns a dataframe of glucose values for user_id ID
read_glucose <- function(conn_args=config::get("dataconnection"),
                         ID=13,
                         fromDate="2019-11-01"){

  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)


  glucose_df <- tbl(con, conn_args$glucose_table)  %>%
    filter(user_id %in% ID & record_date >= fromDate) %>% collect()# & top_n(record_date,2))# %>%

  glucose_raw <- glucose_df %>% transmute(time = force_tz(as_datetime(record_date) + record_time, "America/Los_Angeles"),
                                          scan = value, hist = value, strip = NA, value = value,
                                          food = as.character(stringr::str_match(notes,"Notes=.*")),
                                          user_id = factor(user_id))
  glucose_raw
}


read_notes <- function(conn_args=config::get("dataconnection"),
                    ID=13,
                    fromDate="2019-11-01"){

  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)

  notes_df <- tbl(con, "notes_records") %>%   filter(user_id %in% ID ) %>%
    collect() %>% mutate(Activity = factor(Activity),
                         user_id = factor(user_id))

  glucose_df <- tbl(con, conn_args$glucose_table)  %>%
    filter(user_id %in% ID & record_date >= fromDate) %>% collect() %>%
    transmute(time = force_tz(as_datetime(record_date) + record_time, Sys.timezone()),
                                          scan = value, hist = value, strip = NA, value = value,
                                          food = as.character(stringr::str_match(notes,"Notes=.*")),
                                          user_id = factor(user_id))


  nr <- glucose_df %>%
    filter(!is.na(food)) %>%
    select(Start = time, Comment= food, user_id) %>%
    mutate(Activity=factor("Food"),
           Comment = stringr::str_replace(as.character(Comment),"Notes=",""),
           End=as_datetime(NA), Z=as.numeric(NA),
           user_id = factor(user_id))

  # consider 
  all_levels <- forcats::lvls_union(list(nr$user_id,notes_df$user_id))
  
  notes_records <- nr %>% mutate(user_id=factor(user_id,all_levels)) %>%
                                   bind_rows(notes_df %>% mutate(user_id=factor(user_id,all_levels))) %>%
                                   mutate(Activity=factor(Activity),
                                          user_id=factor(user_id, all_levels))

  return(notes_records)


}


# returns df of glucose values for ID after startDate
# eg. read_glucose_for_user_at_time(ID=22,startTime = as_datetime("2020-02-16 00:50:00", tz=Sys.timezone()))

read_glucose_for_users_at_time <- function(conn_args=config::get("dataconnection"),
                                          ID=13,
                                          startTime=now()-hours(36),
                                          timelength=120){


  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)


  cutoff_1 <- as_datetime(startTime)
  cutoff_2 <- as_datetime(startTime + minutes(timelength))

  glucose_df <- tbl(con, conn_args$glucose_table)  %>%
    filter(user_id %in% ID & record_date_time >= cutoff_1 &
             record_date_time <= cutoff_2) %>% collect()

  #  filter(user_id == ID & (record_date+record_time) >= startTime & (record_date+record_time) <= (startTime + timelength)) %>% collect()# & top_n(record_date,2))# %>%

  glucose_raw <- glucose_df %>% transmute(time = force_tz(as_datetime(record_date) + record_time, Sys.timezone()),
                                          scan = value, hist = value, strip = NA, value = value,
                                          food = as.character(stringr::str_match(notes,"Notes=.*")),
                                          user_id = factor(user_id))



  glucose_raw
}


# return rows where food matches food
# eg. records_with_food(ID=8, foodname="apple")
records_with_food <- function(conn_args=config::get("dataconnection"),
                              ID=13,
                              foodname = "banana"){


  con <- DBI::dbConnect(drv = conn_args$driver,
                        user = conn_args$user,
                        host = conn_args$host,
                        port = conn_args$port,
                        dbname = conn_args$dbname,
                        password = conn_args$password)

  gf = read_glucose(ID=ID) %>% mutate(food=str_to_lower(stringr::str_replace(food,"Notes=","")),
                                      user_id=factor(user_id))

  return(slice(gf,str_which(gf$food,foodname)))

  # nf = read_notes(ID=ID)
  #
  # slice(gf,str_which(str_to_lower(nf$Comment),str_to_lower(foodname))) %>% pull(time)
}

# converts the timestamp into time objects
zero_time <- function(times_vector){
  start <- min(times_vector)
  
  return(as.numeric((times_vector-start)/60))
  
}

make_zero_time_df <- function(df){
  return(arrange(df,time) %>% transmute(t=zero_time(time),
                                        value=value,
                                        meal=meal,
                                        user_id=user_id))
}

# return a new df where value are normalized to start from zero.
normalize_value <- function(df){
  return(df %>% mutate(value=value-first(value)))
  
  
}


# return a dataframe of the first timeLength glucose values for every record that includes foodname
food_times_df <- function(ID=13, timeLength=120, foodname="apple juice"){
  f <- records_with_food(ID=ID, foodname=foodname)
  
  original_levels <- levels(f$user_id) # to prevent a conversion of id_user to char later
  
  df <- NULL
  for(user in ID){
    g <- f %>% filter(user_id==user)
    for(t in g$time){
      new_segment_df <- read_glucose_for_users_at_time(ID=user, startTime = t) %>%
        mutate(meal=paste0(user,"-",month(as_datetime(t)),"/",day(as_datetime(t))),
               user_id=factor(user_id, levels = original_levels))
      df <- bind_rows(df,make_zero_time_df(new_segment_df)) 
    }
  }
  
  return(df)
}

