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
  colnames_librelink <- c("meter_timestamp", "record_type", "historic_glucose", "scan_glucose",
                          "non_numeric_food", "carbohydrates_gram", "carbohydrates_serving",
                          "notes", "strip_glucose", "ketone") %>% paste(., collapse = ",") 
  colnames_activity_track <- c("start_time", "end_time", "activity_category", "comment") %>% paste(., collapse = ",")
  
  #: ----define saveData function----
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
  
  #: ----define loadData function----
  loadData <- function(table_name = table_name_librelink ) {
    # Connect to the database
    pcon <- dbConnect(psql, dbname = databaseName, host = "localhost", port = 5433, 
                      user = "postgres", password = "sigai")
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table_name) 
    # Submit the fetch query and disconnect
    data <- dbGetQuery(pcon, query)
    dbDisconnect(pcon)
    data
  }
  
  # When the Submit button is clicked, save the uploaded data
  observeEvent(input$submit, {
    
    #: ----for "file_glucose_measure_librelink", it is necessary to upload----
    inFile <- input$file_glucose_measure_librelink
    #
    if (is.null(inFile))
      return(NULL)
    #
    glucose_dt <- fread(inFile$datapath, header = input$header_1,
                        sep = input$sep_1, quote = input$quote_1)
    #
    datetime_value_vec <- glucose_dt[ , char_to_datetime(`Meter Timestamp`)] # convert to datetime column
    glucose_dt[ , ("Meter Timestamp") := datetime_value_vec]
    #
    saveData(glucose_dt, table_name = table_name_librelink, colnames_table = colnames_librelink)
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
      activity_dt <- fread(inFile$datapath, header = input$header_2,
                           sep = input$sep_2, quote = input$quote_2)
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
    
    #: ----only plot if "file_glucose_measure_librelink" are present-----
    if ( !is.null(input$file_glucose_measure_librelink)) {
      
      #: ----pre-process the data.tables before plotting---- 
      # 1, for glucose_dt
      glucose_dt[ , `Meter Timestamp` := lubridate::force_tz(`Meter Timestamp`, tzone = "US/Pacific")]
      glucose_dt <- glucose_dt %>% select(time = "Meter Timestamp",
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
        # if activity_df = NULL, need to guarantee "cgm_display" still work.
        cgm_display(start = input$date_range[1], end = input$date_range[2], activity_df = activity_dt,
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
