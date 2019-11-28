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
# one-time setup creates a few dataframes for glucose levels and activity

libre_raw <- readxl::read_excel("Librelink.xlsx")
libre_raw$`Meter Timestamp` <- lubridate::force_tz(libre_raw$`Meter Timestamp`, "US/Pacific")


activity_raw <- dplyr::full_join(readxl::read_excel("Rik Activity 2019.xlsx", sheet = "2018"),
                                 readxl::read_excel("Rik Activity 2019.xlsx", sheet = "2019"))

activity_raw$Start <- lubridate::force_tz(activity_raw$Start, "US/Pacific")
activity_raw$End <- lubridate::force_tz(activity_raw$End, "US/Pacific")

glucose <- libre_raw %>% select(time = "Meter Timestamp",
                                scan = "Scan Glucose(mg/dL)",
                                hist = "Historic Glucose(mg/dL)",
                                strip = "Strip Glucose(mg/dL)",
                                food = "Notes")


glucose$value <- dplyr::if_else(is.na(glucose$scan),glucose$hist,glucose$scan)
glucose_raw <- glucose

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
