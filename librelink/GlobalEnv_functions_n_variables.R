# ====Global Variables===============================================
#: ----Sys.getenv extract these env variables from  ~/.Renviron.----
databaseName <- Sys.getenv("CGMDBNAME")
dbPassword <- Sys.getenv("CGMDBPASSWORD")
dbPortNumber <- Sys.getenv("CGMDBPORTNUMBER")
dbUsername <-  Sys.getenv("CGMDBUSERNAME")

#: ----define dbDriver obj----
psql <- dbDriver("PostgreSQL")
#: ----define schema.tablename and colnames (header) in each table----
table_name_librelink <- "raw.cgm_librelink" # always in the format of "schemaname.tablename"
table_name_activity_track <- "raw.activity_track"
# column names in each table
colnames_librelink <- c("user_id", "meter", "serial_number", "meter_timestamp", "record_type", "historic_glucose", "scan_glucose",
                        "non_numeric_rapid_acting_insulin", "rapid_acting_insulin_unit",
                        "non_numeric_food", "carbohydrates_gram", "carbohydrates_serving",
                        "non_numeric_long_acting_insulin", "long_acting_insulin_unit",
                        "notes", "strip_glucose", "ketone",
                        "meal_insulin", "correction_insulin", "user_change_insulin") %>% paste(., collapse = ",")
colnames_activity_track <- c("user_id", "start_time", "end_time", "activity_category", "comment") %>% paste(., collapse = ",")
#: ----glucose reference band----
# static
# a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
glucose_ref_band <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=100,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)


# ====Global Functions===============================================
#: ----char_to_datetime function----
#' convert timestamp from character data type to POSIXct data type. The format can be customized.
#'
#' @param characters the timestamp variable in character data type
#' @param tz timezone string
#' @param format timestamp format input
#'
#' @return the timestamp variable in POSIXct data type
#' @export
#'
#' @examples
#' char_to_datetime("02/15/2019 15:30")
char_to_datetime <- function(characters, tz = "US/Pacific", format = "%m/%d/%Y %H:%M" ){
  #: POSIXct is a numeric and can be operated `:=`; POSIXlt cannot.
  # so lubridate::as_datetime will not work here, as it will return POSIXlt.
  as.POSIXct(characters, tz = tz, format = format)
}







#: ----cgm plotting function-----
# show glucose levels between start and end times
cgm_display <- function(start=lubridate::now()-lubridate::hours(18),
                        end=lubridate::now(),
                        activity_df=activity_raw,
                        glucose_df=glucose_raw,
                        ref_band = glucose_ref_band) {
  ggplot(glucose_df ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
    #geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
    ref_band +
    geom_rect(data=activity_df %>% dplyr::filter(Activity == "Sleep") %>%
                select(xmin = Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill="red",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_rect(data=activity_df %>% dplyr::filter(Activity == "Exercise") %>%
                select(xmin = Start,xmax = End),
              aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
              fill="blue",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(Activity == "Event" & Comment == "awake") %>% select("Start") %>% unlist(),
               color = "green") +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(Activity == "Food") %>% select("Start") %>% unlist(),
               color = "yellow")+
    geom_text(data = activity_df %>%
                dplyr::filter(Activity == "Food") %>% select("Start","Comment") ,
              aes(x=Start,y=50, angle=90, hjust = FALSE,  label = Comment),
              size = 6) +
    labs(title = "Glucose (mg/dL)", subtitle = start) +  theme(plot.title = element_text(size=22))+
    scale_x_datetime(limits = c(start,end))

}

#: ----define saveData function----
#' saveData
#' save data for the database table
#' @param data an R data.frame or data.table
#' @param table_name the remote SQL tablename
#' @param colnames_table field names of the remote SQL table
#'
#' @return no return
#' @export
#'
#' @examples
#' saveData(data = librelink_dt, table_name = table_name_librelink, colnames_table = colnames_librelink)
saveData <- function(data, drv = psql, table_name = table_name_librelink, colnames_table = colnames_librelink) {
  # Connect to the database
  # "psql" from GlobalEnv.
  pcon <- dbConnect(drv, dbname = databaseName, host = "localhost", port = dbPortNumber,
                    user = dbUsername, password = dbPassword)

  paranthesis_concatenated_string <- data %>% apply(., 1, paste, collapse = "','") %>% sprintf("('%s')", .) %>% paste(., collapse = ', ' ) %>% gsub("'NA'", "NULL", . )

  # Construct the update query by looping over the data fields
  query <- sprintf( "INSERT INTO %s (%s) VALUES %s",
                    table_name,
                    colnames_table,
                    paranthesis_concatenated_string
  )


  # Submit the update query and disconnect
  dbSendQuery(pcon, query)
  # dbSendQuery(pcon, query, params=data[["message"]])
  dbDisconnect(pcon)
}

#: ----define saveData_user_account function----
# a specialized version modified from the generic 'saveData' function by including embeded SQL function to generate field in addition to "data".
#' saveData_user_account
#' save data for the table "raw.user_account"
#' a specialized version modified from the generic 'saveData' function by including embeded SQL function to generate field in addition to "data".
#' @param data an R data.frame or data.table
#' @param table_name the remote SQL tablename
#' @param colnames_table field names of the remote SQL table
#'
#' @return no return
#' @export
#'
#' @examples
#' saveData_user_account(data = user_account_dt)
saveData_user_account <- function(data, drv = psql, table_name = "raw.user_account",
                                  colnames_table = "username, password, email, created_on, last_login") {
  # Connect to the database
  # "psql" from GlobalEnv.
  pcon <- dbConnect(drv, dbname = databaseName, host = "localhost", port = dbPortNumber,
                    user = dbUsername, password = dbPassword)

  #: specialized concate string just for one row input for "raw.user_account" table
  # use email for both username and email as now.
  paranthesis_concatenated_string <- data %>% stringr::str_glue_data("('{email}','{password}','{email}', CURRENT_TIMESTAMP(2), NULL)")

  #paranthesis_concatenated_string <- data %>% apply(., 1, paste, collapse = "','") %>% sprintf("('%s')", .) %>% paste(., collapse = ', ' ) %>% gsub("'NA'", "NULL", . )
  # Construct the update query by looping over the data fields
  query <- sprintf( "INSERT INTO %s (%s) VALUES %s",
                    table_name,
                    colnames_table,
                    paranthesis_concatenated_string
  )
  #

  #
  # # Construct the update query by looping over the data fields
  # query <- sprintf( "INSERT INTO %s (%s) VALUES %s",
  #                   table_name,
  #                   colnames_table,
  #                   paranthesis_concatenated_string )
  #
  # #
  # query_not_from_dt <- sprintf( "INSERT INTO %s (%s) VALUES %s",
  #                               table_name,
  #                               colnames_others,
  #                               "(CURRENT_TIMESTAMP(2), NULL)"
  # )



  # Submit the update query and disconnect
  dbSendQuery(pcon, query)
  #dbSendQuery(pcon, query_not_from_dt)
  # dbSendQuery(pcon, query, params=data[["message"]])
  dbDisconnect(pcon)
}

#: example
# saveData_user_account(user_account_dt)

# ----if need to close too many dead connections----
#: if there are too many dead connections due to program dead in the middle (usually under dev mode)
# use this cmd to close all.
# sapply(RPostgreSQL::dbListConnections(psql), FUN = dbDisconnect)


#: ----define loadData function----
#' loadData
#' select all rows from a remote database table and import into an R data.table
#' @param table_name table name string as in the remote database
#'
#' @return an R data.table containing all the rows from the remote table
#' @export
#'
#' @examples
#' loadData(table_name = "raw.user_account")
loadData <- function(table_name = table_name_librelink, drv = psql ) {
  # Connect to the database
  pcon <- dbConnect(drv, dbname = databaseName, host = "localhost", port = dbPortNumber,
                    user = dbUsername, password = dbPassword)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table_name)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(pcon, query)
  dbDisconnect(pcon)
  data %>% as.data.table()
}

# loadData(table_name = "raw.user_account")

#: ----define loadData_checkExistingUser function----
#' loadData_checkExistingUser
#' check if user existed in the database user table by searching the email
#' @param table_name the user account table in the database
#' @param email_fieldname_table field name for storing the email in the table
#' @param email_to_check the email address to check
#'
#' @return a boolean, TRUE for found and FALSE for not.
#' @export
#'
#' @examples
#' loadData_checkExistingUser(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "xyy2006@msn.com")
loadData_checkExistingUser <- function(table_name = "raw.user_account", drv = psql, email_fieldname_table = "email", email_to_check = "xyy2006@msn.com"  ) {
  # Connect to the database
  pcon <- dbConnect(drv, dbname = databaseName, host = "localhost", port = dbPortNumber,
                    user = dbUsername, password = dbPassword)
  # Construct the fetching query
  query <- stringr::str_glue("SELECT * FROM {table_name}
                               where {email_fieldname_table} = '{email_to_check}'" )

  # Submit the fetch query and disconnect
  data <- dbGetQuery(pcon, query)
  dbDisconnect(pcon)
  # data

  #: criteria: a data.table with 1 row (user found) or 0 row (no such user)
  dplyr::if_else(condition = nrow(data) > 0, true = TRUE, false = FALSE)
}
#: example
# loadData_checkExistingUser(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "xyy2006@msn.com")
# loadData_checkExistingUser(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "richard.sprague@msn.com")

#: ----define loadData_extractExistingUserID function----
#' loadData_checkExistingUser
#' check if user existed in the database user table by searching the email
#' @param table_name the user account table in the database
#' @param email_fieldname_table field name for storing the email in the table
#' @param email_to_check the email address to check
#'
#' @return user_id, a SMALLINT in database or NULL if user not exists.
#' @export
#'
#' @examples
#' loadData_extractExistingUserID(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "xyy2006@msn.com")
loadData_extractExistingUserID <- function(table_name = "raw.user_account", drv = psql, email_fieldname_table = "email", email_to_check = "xyy2006@msn.com"  ) {
  # Connect to the database
  pcon <- dbConnect(drv, dbname = databaseName, host = "localhost", port = dbPortNumber,
                    user = dbUsername, password = dbPassword)
  # Construct the fetching query
  query <- stringr::str_glue("SELECT * FROM {table_name}
                               where {email_fieldname_table} = '{email_to_check}'" )

  # Submit the fetch query and disconnect
  data <- dbGetQuery(pcon, query)
  dbDisconnect(pcon)

  stopifnot(nrow(data) < 2 ) # return 0 row or 1 row if user exists.

  data$user_id # should return either NULL or 1 int if user exists.


}
#: example
# loadData_extractExistingUserID(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "xyy2006@msn.com")
# 1
# loadData_extractExistingUserID(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "richard.sprague@msn.com")
# NULL



# #:----define myDate class for read in timestamp column as timestamp
# # e.g., fread(..., colClasses = list("myDate" = "col_A_in_data"))
# setClass('myTimestamp')
# setAs("character","myTimestamp", function(from) lubridate::as_datetime(from, tz = "US/Pacific", format="%m/%d/%Y %H:%M") )
# setAs("character","myTimestamp", function(from) as.POSIXct(from, tz = "US/Pacific", format="%m/%d/%Y %H:%M") )
#: a func to convert character column to datetime tppe.

