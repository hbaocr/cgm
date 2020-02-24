# read_cgm_db.R  # reads cgm from the database
# creates these new variables from the P4MI database:
# glucose_raw
# notes_records: combines data from the database with what it also finds in the glucose comments fields.
# right now it's hard-wired to only pull data from USER_ID


library(tidyverse)
library(dplyr)
library(DBI)

library(lubridate)

library(conflicted)
conflict_prefer("filter", "dplyr")

USER_ID = 13

Sys.setenv(R_CONFIG_ACTIVE = "p4mi")

conn_args <- config::get("dataconnection")
conn_args

# first time only. Connect to the server, but not to a specific database
con <- DBI::dbConnect(drv = conn_args$driver,
                      user = conn_args$user,
                      host = conn_args$host,
                      port = conn_args$port,
                      dbname = conn_args$dbname,
                      password = conn_args$password)

# list the tables in this database
# should include "notes_records" and "glucose_records"
DBI::dbListTables(con)


glucose_df <- tbl(con, "glucoses_glucose") %>% select(-created,-modified) %>%
  filter(user_id == USER_ID & record_date > "2019-11-01") %>% collect()# & top_n(record_date,2))# %>%

glucose_raw <- glucose_df %>% transmute(time = force_tz(as_datetime(record_date) + record_time, Sys.timezone()),
                                        scan = value, hist = value, strip = NA, value = value,
                                        food = as.character(stringr::str_match(notes,"Notes=.*")),
                                        user_id = user_id)

notes_df <- tbl(con, "notes_records") %>%   filter(user_id == USER_ID ) %>%
  collect() %>% mutate(Activity = factor(Activity))

nr <- glucose_raw %>%
  filter(!is.na(food)) %>%
  select(Start = time, Comment= food) %>%
  mutate(Activity=factor("Food"),
         Comment = stringr::str_replace(as.character(Comment),"Notes=",""),
         End=as_datetime(NA), Z=as.numeric(NA),
         user_id = USER_ID)

notes_records <- nr %>% bind_rows(notes_df) %>% mutate(Activity=factor(Activity))
# DBI::dbWriteTable(con, name = "notes_records",
#                   value = notes_records,
#                   row.names = FALSE, overwrite = TRUE)

rm(nr)
rm(notes_df)
rm(glucose_df)


#
# glucose_raw <- tbl(con,"glucose_records") %>% collect()
# activity_raw <- tbl(con,"notes_records") %>% collect()
# activity_raw$Activity <- factor(activity_raw$Activity)

DBI::dbDisconnect(con)
