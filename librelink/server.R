#
# This is the server logic of a Shiny web application to display my glucose levels. You can run the
# application by clicking 'Run App' above.
#
# To get started, you will need to upload a few CSV/Excel files.

# deploy: rsconnect::deployApp("Librelink")


library(shiny)

library(tidyverse)
library(lubridate)
library(ggthemes)
library(cgmr)

library(DBI)
library(RPostgres)
# one-time setup creates a few dataframes for glucose levels and activity
# now connect to the glucose db and write to it


con <-DBI::dbConnect(RPostgres::Postgres(),
                     # driver = "/usr/local/lib/psqlodbcw.so",
                     host = "psdev.ctmxeolrv0ba.us-east-1.rds.amazonaws.com",
                     user = "postgres",
                     dbname = "glucose_db",
                     password = "testtest",
                     port = 5432)

glucose_raw <- tbl(con,"glucose_records") %>% collect()

con <-DBI::dbConnect(RPostgres::Postgres(),
                     # driver = "/usr/local/lib/psqlodbcw.so",
                     host = "psdev.ctmxeolrv0ba.us-east-1.rds.amazonaws.com",
                     user = "postgres",
                     dbname = "notes_db",
                     password = "testtest",
                     port = 5432)


activity_raw <- tbl(con,"notes_records") %>% collect()

DBI::dbDisconnect(con)

theme_set(theme_stata())


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

#    renderPrint(input$date1)
    output$glucoseLevelsPlot <- renderPlot({

        #glucose <- dplyr::filter(glucose, time >= input$date_range[1] & time <= input$date_range[2] + lubridate::hours(6))
        # activity <- dplyr::filter(activity_raw, Start >= input$date_range[1] &
        #                             Start <= input$date_range[2] + lubridate::hours(6))
        # activity$Activity <- factor(activity$Activity)
        #
        # cgm_display(lubridate::as_datetime("2019-10-16"),
        #             lubridate::as_datetime("2019-10-30"),
        #             activity_raw,
        #             glucose_raw)

        cgmr::cgm_display(lubridate::as_datetime(input$date1),
                    lubridate::as_datetime(input$date2),activity_raw,glucose_raw)

    })


})
