#
# This is the server logic of a Shiny web application to display my glucose levels. You can run the
# application by clicking 'Run App' above.
#
# To get started, you will need to upload a few CSV/Excel files.

library(shiny)

library(tidyverse)
library(lubridate)
library(ggthemes)
library(RPostgreSQL)



#: ====global Env===============================================
# #:----define myDate class for read in timestamp column as timestamp
# # e.g., fread(..., colClasses = list("myDate" = "col_A_in_data"))
setClass('myTimestamp')
# setAs("character","myTimestamp", function(from) lubridate::as_datetime(from, tz = "US/Pacific", format="%m/%d/%Y %H:%M") )
# setAs("character","myTimestamp", function(from) as.POSIXct(from, tz = "US/Pacific", format="%m/%d/%Y %H:%M") )
#: a func to convert character column to datetime tppe.
char_to_datetime <- function(characters, tz = "US/Pacific", format = "%m/%d/%Y %H:%M" ){
  #: POSIXct is a numeric and can be operated `:=`; POSIXlt cannot.
  # so lubridate::as_datetime will not work here, as it will return POSIXlt.
  as.POSIXct(characters, tz = tz, format = format)
}


#: ----glucose reference band----
# static
# a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
glucose_ref_band <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=100,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)




#: ----cgm plotting function-----
# show glucose levels between start and end times
cgm_display <- function(start=lubridate::now()-lubridate::hours(18),
                        end=lubridate::now(),
                        activity_df=activity_raw,
                        glucose_df=glucose_raw,
                        ref_band = glucose_ref_band) {
  ggplot(glucose_df ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
    geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
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


#: ====define shinyServer()===============================================
# Define server logic required to draw a histogram
shinyServer( function(input, output, session) {

  psql <- dbDriver("PostgreSQL")

  #: ----define schema.tablename and colnames (header) in each table----
  # entity names
  databaseName <- "postgres"
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
  saveData <- function(data, table_name = table_name_librelink, colnames_table = colnames_librelink) {
    # Connect to the database
    # "psql" from GlobalEnv.
    pcon <- dbConnect(psql, dbname = databaseName, host = "localhost", port = 5433,
                      user = "postgres", password = "sigai")

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
  saveData_user_account <- function(data, table_name = "raw.user_account", 
                                    colnames_table = "username, password, email, created_on, last_login") {
    # Connect to the database
    # "psql" from GlobalEnv.
    pcon <- dbConnect(psql, dbname = databaseName, host = "localhost", port = 5433,
                      user = "postgres", password = "sigai")
    
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
  loadData <- function(table_name = table_name_librelink ) {
    # Connect to the database
    pcon <- dbConnect(psql, dbname = databaseName, host = "localhost", port = 5433,
                      user = "postgres", password = "sigai")
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
  loadData_checkExistingUser <- function(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "xyy2006@msn.com"  ) {
    # Connect to the database
    pcon <- dbConnect(psql, dbname = databaseName, host = "localhost", port = 5433,
                      user = "postgres", password = "sigai")
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
  loadData_extractExistingUserID <- function(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = "xyy2006@msn.com"  ) {
    # Connect to the database
    pcon <- dbConnect(psql, dbname = databaseName, host = "localhost", port = 5433,
                      user = "postgres", password = "sigai")
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
  
  #: ----check whether two email string inputs are consistent, if not, report error immediately and disable submit button function----
  # if (input$email_input_1 == input$email_input_2) {
  #   email_check_consist_msg <- "Pass!"
  #   output$email_check_consist_msg <- renderText({ if_else()email_check_consist_msg })
  #   
  # } else {
  #   email_check_consist_msg <- "Two inputs of emails are not the same!"
  #   output$email_check_consist_msg <- renderText({ input$caption })
  # }
  
  #: define a reactive function.
  msg_print <- reactive({
    # boolean, whether two strings are exactly the same after trim and nchar > 0, i.e., non-empty.
    #: define a few conditions, need unit tests here to be more robust.
    same_flag <- str_trim(input$email_input_1) == str_trim(input$email_input_2)
    both_non_empty_flag <- nchar(str_trim(input$email_input_1)) > 0 & nchar(str_trim(input$email_input_2)) > 0
    any_empty_flag <- nchar(str_trim(input$email_input_1)) == 0 | nchar(str_trim(input$email_input_2)) == 0
    #: very simple check for valid email address using regex, could be improved.
    #contain_ata_flag <- grepl("[^@]+@[^@]+\.[^@]+", x = input$email_input_1) & grepl("[^@]+@[^@]+\.[^@]+", x = input$email_input_2) # assume every email address should contain "@".
    contain_ata_flag <- stringr::str_detect(pattern = ("[^@]+@[^@]+\\.[^@]+"), string = input$email_input_1) | stringr::str_detect(pattern = ("[^@]+@[^@]+\\.[^@]+"), string = input$email_input_2)
    
    if (same_flag & both_non_empty_flag & contain_ata_flag) {
      value_return <- "same"
    } else if (any_empty_flag ) {
      value_return <- "enter"
    } else if (!contain_ata_flag) {
      value_return <- "invalid"
    } else {
      value_return <- "differ"
    }
    
    switch(EXPR = value_return,
           same =  "Pass! Now you can upload your files!",
           enter = "Please enter your emails above.",
           differ = "Two emails are not the same! Please fix...",
           invalid = "Invalid email address!"
    )
    
  }) 
  
  output$email_check_consist_msg <- renderText({
    
    msg_print()
    
  })
  
  #: for sending value from server.r to ui.r, we need to use renderUI here to manipulate dynamic conditions, e.g., font/color pending on
  # the value of a intermediate variable.
  output$print_email_check_consist_msg <- renderUI({
    uiOutput <- textOutput("email_check_consist_msg")
    color <- switch(msg_print(),
                    "Please enter your emails above." = "gray",
                    "Pass! Now you can upload your files!" = "green",
                    "red"
                      )
      # dplyr::if_else (msg_print() == "Pass! Now you can upload your files!",
      #                 "green",
      #                 "red")
    tagList(uiOutput,
            #: css syntax below,
            tags$head(tags$style(
              sprintf(
                "#email_check_consist_msg{color: %s;
                font-size: 16px;
                font-style: bold;}",
                color
              )
            )))
  })
  
  
  #: obsolete 
  # output$color_font_email_check_consist_msg <- reactive({
  #   if (msg_print() == "Pass! Now you can upload your files!"){
  #     "green"
  #   } else {
  #     "red"
  #   }
  # })
  # outputOptions(output, 'color_font_email_check_consist_msg', suspendWhenHidden = FALSE) # send value as option to UI
  

  #: ----detect if emails inputs "pass", return TRUE or FALSE for 'flag_fail_invalidMail_panel'.   
  # then transferred as options to ui.r as a javascript condition.
  output$flag_fail_invalidMail_panel <- eventReactive(input$submit,
                                                        {
                                                          dplyr::if_else(condition = msg_print() != "Pass! Now you can upload your files!",
                                                                        true = TRUE,
                                                                        false = FALSE)
                                                        })
  outputOptions(output, "flag_fail_invalidMail_panel", suspendWhenHidden = FALSE)  
    
  
  
  
  #: ----When the Submit button is clicked, save the uploaded data, draw table, visualize, etc.----
  observeEvent(input$submit, {
   
    #:----check if flag_fail_invalidMail_panel == TRUE, it yes, return NULL----
    if (msg_print() != "Pass! Now you can upload your files!") {
      return()
    }
    
    #: ----for "file_glucose_measure_librelink", it is necessary to upload----
    inFile <- input$file_glucose_measure_librelink
    #
    if (is.null(inFile))
      return(NULL)
    #
    #: 1, first row contains meta information of the user, and should be extracted first
    # assume fixed format as "Export Patient's Glucose Data,Generated on,3/25/19,Generated by,sample,..."
    # that will allow us to extract by index.
    metaInfo_line <- readLines(inFile$datapath, n = 1)
    metaInfo_line_char <- metaInfo_line %>% str_split(., pattern = ",") %>% unlist
    #
    date_glucoseData_download <- metaInfo_line_char[3] %>% as.Date(., "%m/%d/%y")
    user_name_dataMetaInfo <- metaInfo_line_char[5]
    #: extract email from input$
    user_email <- input$email_input_1
    user_pwd <- 123456 # currently the same pwd as placeholder for every user since no login portal yet.
    user_account_dt <- data.table(username = user_name_dataMetaInfo, 
               password = user_pwd,
               email = user_email # email could be used twice for now to represent both "username" and "email" in the raw.user_account table in the database.
               #created_on = "", # omitted here, let SQL function take care in the function 'saveData_user_account'.
               #last_login = "" # omitted here, let SQL function take care.
               )
    
    #: check if user already existed in the raw.user_account by checking either username or email, using "loadData" func.
    flag_user_exists <- loadData_checkExistingUser(table_name = "raw.user_account", email_fieldname_table = "email", email_to_check = user_account_dt$email)
    #: if not found, upload user info by using saveData_user_account()
    if (!flag_user_exists) {
      saveData_user_account(data = user_account_dt, table_name = "raw.user_account", colnames_table =  "username, password, email, created_on, last_login")
    }
    

    
    #: 2, start from 2nd row, columnar format.
    glucose_dt <- data.table::fread(inFile$datapath, header = input$header_1,
                        sep = input$sep_1, quote = input$quote_1, skip = 1) # skip the 1st row as it contains metaInfo.
    #glucose_dt <- data.table::fread("Librelink_Export_03-25-2019.csv", skip = 1) # for debugging
    
    #: 3, insert the user_id column into the 1st column position in the data.table
    # a), extract the user_id_value first
    user_id_value <- loadData_extractExistingUserID(table_name = "raw.user_account", email_fieldname_table = "email",
                                                  email_to_check = user_account_dt$email) # should be one row returned if exists
    # b), insert 
    glucose_dt[,  user_id := user_id_value]
    setkey(glucose_dt, user_id)
    # c), column reorder
    glucose_dt %>% setcolorder(., neworder = key(.)) # move the key to the front place.
    
    
    datetime_value_vec <- glucose_dt[ , char_to_datetime(`Meter Timestamp`)] # convert to datetime column
    glucose_dt[ , ("Meter Timestamp") := datetime_value_vec]
    #
    saveData(glucose_dt, table_name = table_name_librelink, colnames_table = colnames_librelink)
    #: summary, now we dont regulate duplicate upload check in the server.r. The burden is relatively huge if we want to do it, e.g., by
    # comparing current upload and the existing rows in the database (need loadData back into memeroy and compare), then upload the setdiff part rows.
    # Instead, this can be accompanished by ETL stages regarding the database schema. For example, we can build 'load' stage/schema, which extract
    # unique rows from the tables in the 'raw' stage; 'raw' stage will be periodically updated with the 'load' corresponding table to reduce oversize issue. 
    #
    #:----renderTable of file_glucose_measure_librelink----
    output$rT_file_glucose_measure_librelink <- renderDataTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      glucose_dt
    })


    #: ----for "file_activity_track", it is optional----
    inFile <- input$file_activity_track
    #
    if (!is.null(inFile)) {
      activity_dt <- data.table::fread(inFile$datapath, header = input$header_2,
                           sep = input$sep_2, quote = input$quote_2)
      # b), insert 
      activity_dt[,  user_id := user_id_value]
      setkey(activity_dt, user_id)
      # c), column reorder
      activity_dt %>% setcolorder(., neworder = key(.)) # move the key to the front place.
      #
      activity_dt[ , `Start` := char_to_datetime(`Start`)] # convert to datetime column
      activity_dt[ , `End` := char_to_datetime(`End`)] # convert to datetime column
      #
      saveData(activity_dt, table_name = table_name_activity_track, colnames_table = colnames_activity_track)
      #:----renderTable of file_glucose_measure_librelink----
      output$rT_file_activity_track <- renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        activity_dt
      })
    }

    #: ----only plot if both files are present-----
    #: ----only plot if "file_glucose_measure_librelink" are present-----
    if ( !is.null(input$file_glucose_measure_librelink)) {
      
      #: ----pre-process the data.tables before plotting----
      # 1, for glucose_dt
      glucose_dt[ , `Meter Timestamp` := lubridate::force_tz(`Meter Timestamp`, tzone = "US/Pacific")]
      glucose_dt <- glucose_dt %>% select(time = "Meter Timestamp",
                                          #scan = "Scan Glucose(mmol/L)",
                                          #hist = "Historic Glucose(mmol/L)",
                                          #strip = "Strip Glucose(mmol/L)",
                                          scan = "Scan Glucose(mg/dL)",
                                          hist = "Historic Glucose(mg/dL)",
                                          strip = "Strip Glucose(mg/dL)",
                                          food = "Notes")
      
      glucose_dt[, value := dplyr::if_else( condition = is.na(scan), true = hist, false = scan )]
      # glucose_dt[, value := scan ]
      # glucose_dt[is.na(value), value := hist ]


      #libre_raw$`Meter Timestamp` <- lubridate::force_tz(libre_raw$`Meter Timestamp`, "US/Pacific")
      # activity_raw <- dplyr::full_join(readxl::read_excel("Rik Activity 2019.xlsx", sheet = "2018"),
      #readxl::read_excel("Rik Activity 2019.xlsx", sheet = "2019"))

      # 2, for activity_dt
      if (!is.null(input$file_activity_track) ) {
        # 2, for activity_dt
        activity_dt[, Start := lubridate::force_tz(Start, tzone = "US/Pacific")]
        activity_dt[, End := lubridate::force_tz(End, tzone = "US/Pacific")]
      } else {
        activity_dt <- NULL # if no file provided, set the dt variable to NULL.
      }
      
      #: ----renderPlot----
      output$glucoseLevelsPlot <- renderPlot({
        #:----set the theme before plotting----
        theme_set(theme_stata())
        # glucose <- dplyr::filter(glucose, time >= input$date_range[1] & time <= input$date_range[2] + lubridate::hours(6))
        # activity <- dplyr::filter(activity_raw, Start >= input$date_range[1] &
        #                             Start <= input$date_range[2] + lubridate::hours(6))
        # activity$Activity <- factor(activity$Activity)
        #

        #: ----plotting----
        # if input$date_range changes, that will trigger renderPlot again.
        cgm_display(start = lubridate::as_datetime(input$date1), end = lubridate::as_datetime(input$date2), activity_df = activity_dt,
                    glucose_df = glucose_dt, ref_band = glucose_ref_band)

      })
    }

  })



  # #:----renderTable of file_glucose_measure_librelink----
  # output$rT_file_glucose_measure_librelink <- renderTable({
  #   # input$file1 will be NULL initially. After the user selects
  #   # and uploads a file, it will be a data frame with 'name',
  #   # 'size', 'type', and 'datapath' columns. The 'datapath'
  #   # column will contain the local filenames where the data can
  #   # be found.
  #
  #   inFile <- input$file_glucose_measure_librelink
  #
  #   if (is.null(inFile))
  #     return(NULL)
  #
  #   read.csv(inFile$datapath, header = input$header,
  #            sep = input$sep, quote = input$quote)
  # })

  # # Show the previous responses
  # # (update with current response when Submit is clicked)
  # output$responses <- DT::renderDataTable({
  #   input$submit
  #   loadData()
  # })





}) # end of shinyServer.

#
# #:----from shiny tutorial pasted below----------
#: print the table.
#
# # By default, the file size limit is 5MB. It can be changed by
# # setting this option. Here we'll raise limit to 9MB.
# options(shiny.maxRequestSize = 9*1024^2)
#
# function(input, output) {
#   output$contents <- renderTable({
#     # input$file1 will be NULL initially. After the user selects
#     # and uploads a file, it will be a data frame with 'name',
#     # 'size', 'type', and 'datapath' columns. The 'datapath'
#     # column will contain the local filenames where the data can
#     # be found.
#
#     inFile <- input$file1
#
#     if (is.null(inFile))
#       return(NULL)
#
#     read.csv(inFile$datapath, header = input$header,
#              sep = input$sep, quote = input$quote)
#   })
# }
