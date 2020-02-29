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


source("read_data_utils.R")



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
                        show.label = TRUE) {

    ndf <- notes_df %>% dplyr::filter(Start >= start & End <=end) %>%
        bind_rows(tibble(Start=now()-hours(24),End=now(),Comment=NA,Activity=NA,Z=NA))
    gdf <- glucose_df %>% dplyr::filter(time >= start & time <=end)
    ggplot(gdf ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
        #geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
        glucose_target_gg +
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

theme_set(theme_stata())

# Server ----

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

#    renderPrint(input$date1)
    output$glucoseLevelsPlot <- renderPlot({
        
        USER_ID <- input$a_user
        message("user=",USER_ID)

        #glucose <- dplyr::filter(glucose, time >= input$date_range[1] & time <= input$date_range[2] + lubridate::hours(6))
        # activity <- dplyr::filter(activity_raw, Start >= input$date_range[1] &
        #                             Start <= input$date_range[2] + lubridate::hours(6))
        # activity$Activity <- factor(activity$Activity)
        #
        # cgm_display(lubridate::as_datetime("2019-10-16"),
        #             lubridate::as_datetime("2019-10-30"),
        #             activity_raw,
        #             glucose_raw)
        
        Sys.setenv(R_CONFIG_ACTIVE = "p4mi")
        
        message("reading glucose from db")
        glucose_raw <- read_glucose(config::get("dataconnection"),ID=USER_ID,fromDate = input$date1)
        
        notes_records1 <- read_notes(config::get("dataconnection"),ID=USER_ID,fromDate = input$date1)
        fake_sleep = tibble(Start=as_datetime("2000-01-01"),End=as_datetime("2000-01-02"),Comment=NA,Activity="Sleep",Z=0,user_id=USER_ID)
        notes_records <- bind_rows(notes_records1,fake_sleep)
        notes_records$Activity <- factor(notes_records$Activity)
        
        Sys.setenv(R_CONFIG_ACTIVE = "cloud")
        
        conn_args <- config::get("dataconnection")
        con <- DBI::dbConnect(drv = conn_args$driver,
                              user = conn_args$user,
                              host = conn_args$host,
                              dbname = conn_args$dbname,
                              port = conn_args$port,
                              password = conn_args$password)
        
        
        message('reading watch data from db')
        watch_data <- tbl(con, "watch_records") 
        
        message('ready to display data')

        cgm_display(start=lubridate::as_datetime(input$date1, tz="America/Los_Angeles"),
                          end=lubridate::as_datetime(input$date2, tz="America/Los_Angeles"),
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
            geom_line(data=watch_data %>% 
                          dplyr::filter(type == "HeartRate" &
                                            startDate >= lubridate::as_datetime(input$date1, tz="America/Los_Angeles") &
                                            endDate <= lubridate::as_datetime(input$date2, tz="America/Los_Angeles")) %>%
                          select(time = startDate, value = heart_rate) %>% as_tibble(), inherit.aes = FALSE,
                      stat = "identity",
                      aes(x = time, y = value*2),
                      color = "brown") +
            scale_y_continuous(sec.axis = sec_axis(~./2,
                                                   name = "Heart Rate (bpm)")) +
            labs(title = paste("Glucose values for user:",USER_ID),
                 subtitle = as.character(input$date1),
                 y = "Glucose (mg/dL)",
                 x = "") +
            theme(plot.title = element_text(size=22))

    })
    
    output$foodComparePlot <- renderPlot({
        
        Sys.setenv(R_CONFIG_ACTIVE = "p4mi")
        d = food_times_df(ID=input$user_list,foodname=stringr::str_to_lower(input$foodname))
        message("made foodtimes df")
        
        g <- d
        
        if(input$norm_value) {
            g <- d %>%  group_by(meal) %>% normalize_value()
        }
        
        
        g %>% mutate(user=factor(meal)) %>%
            ggplot(aes(x=t,y=value,color=user)) + geom_line(size=2) + labs(title=input$foodname)
        
        
    })


})
