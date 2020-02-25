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
#library(cgmr)

library(DBI)
library(RPostgres)

# Setup ----
# one-time setup creates a few dataframes for glucose levels and activity
# now connect to the glucose db and write to it

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
# DBI::dbListTables(con)


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

DBI::dbDisconnect(con)
Sys.setenv(R_CONFIG_ACTIVE = "cloud")

conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(drv = conn_args$driver,
                      user = conn_args$user,
                      host = conn_args$host,
                      dbname = conn_args$dbname,
                      port = conn_args$port,
                      password = conn_args$password)



watch_data <- DBI::dbReadTable(con, "watch_records") %>% as_tibble()
DBI::dbDisconnect(con)

rm(nr)
rm(notes_df)
rm(glucose_df)


# Functions ----

glucose_target_gg <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=85,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)

glucose_chart_title <-   labs(title = "title", subtitle = "start",
                              y = "Glucose (mg/dL)",
                              x = "") +  theme(plot.title = element_text(size=22))

cgm_display <- function(start=lubridate::now()-lubridate::hours(18),
                        end=now(),
                        notes_df,
                        glucose_df,
                        title = "Glucose",
                        show.label = TRUE) {

    ndf <- notes_df %>% dplyr::filter(Start >= start & End <=end) %>%
        bind_rows(tibble(Start=now()-hours(24),End=now(),Comment=NA,Activity=NA,Z=NA))
    gdf <- glucose_df %>% dplyr::filter(time >= start & time <=end)
    ggplot(gdf ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
        #geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
        glucose_target_gg +
        labs(title = title, subtitle = start,
             y = "Glucose (mg/dL)",
             x = "") +  theme(plot.title = element_text(size=22))+
        scale_x_datetime(limits = c(start,end),
                         date_labels = "%m/%d %H:%M",
                         timezone = "US/Pacific")

}

# ggplot object that adds Apple Watch heartrate data to the secondary axis
heartRate_display <- function (start=lubridate::now()-lubridate::hours(18),
                               end=now(),
                               bpm_data
){

    ggplot(bpm_data) +
        geom_line(inherit.aes = FALSE,
                  stat = "identity",
                  aes(x = time, y = value*2),
                  color = "brown") +
        scale_y_continuous(sec.axis = sec_axis(~./2,
                                               name = "Heart Rate (bpm)"))
}





#
# glucose_raw <- tbl(con,"glucose_records") %>% collect()
# activity_raw <- tbl(con,"notes_records") %>% collect()
# activity_raw$Activity <- factor(activity_raw$Activity)

DBI::dbDisconnect(con)

theme_set(theme_stata())

# Server ----

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

        cgm_display(start=lubridate::as_datetime(input$date1),
                          end=lubridate::as_datetime(input$date2),
                          notes_records,
                          glucose_raw) +
            geom_vline(xintercept = notes_records %>% dplyr::filter(.data$Activity == "Food") %>% pull("Start"),
                                                    color = "yellow")  +

            geom_rect(data=notes_records%>% dplyr::filter(.data$Activity == "Sleep") %>%
                          select(xmin = .data$Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
                      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                      fill="red",
                      alpha=0.2,
                      inherit.aes = FALSE) +
            geom_text(data = notes_records %>%
                          dplyr::filter(.data$Activity == "Food") %>% select("Start","Comment") ,
                      aes(x=Start,y=50, angle=90, hjust = FALSE,  label =  Comment),
                      size = 6) +
            geom_line(data=watch_data %>% dplyr::filter(type == "HeartRate" &
                                                            startDate > now() - hours(36) &
                                                            endDate < now()) %>%
                          select(time = startDate, value = heart_rate), inherit.aes = FALSE,
                      stat = "identity",
                      aes(x = time, y = value*2),
                      color = "brown") +
            scale_y_continuous(sec.axis = sec_axis(~./2,
                                                   name = "Heart Rate (bpm)"))

    })


})
