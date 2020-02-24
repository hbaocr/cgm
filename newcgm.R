# newcgm.r
# starting from scratch. Hopefully this will later be used to fix the other routines

# First you need two dataframes:
# glucose_raw
# notes_records
source("read_cgm_db.R")

oldEnv <- Sys.getenv("R_CONFIG_ACTIVE")
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
Sys.setenv(R_CONFIG_ACTIVE=oldEnv)


library(tidyverse)
library(lubridate)
# target bands ----
# a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
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

watch_data %>% dplyr::filter(type == "HeartRate" &
                               startDate > now() - hours(24) &
                               endDate < now()) %>%
  select(time = startDate, value = heart_rate)

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



#   geom_text(data = ndf %>%
#               dplyr::filter(.data$Activity == "Food") %>% select("Start","Comment") ,
#             aes(x=.data$Start,y=50, angle=90, hjust = FALSE,  label = .data$Comment),
#             size = 6, inherit.aes = FALSE) +
#

ndf <- notes_records %>% dplyr::filter(Start >=now()-weeks(2))


g <- cgm_display(start = as_datetime(now()-hours(36)),
            end = as_datetime(now()),
            notes_records,
            glucose_raw) + glucose_chart_title

g + geom_vline(xintercept = ndf %>%
                 dplyr::filter(.data$Activity == "Food") %>% pull("Start"),
               color = "yellow")  +

geom_rect(data=ndf %>% dplyr::filter(.data$Activity == "Sleep") %>%
            select(xmin = .data$Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
          aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
          fill="red",
          alpha=0.2,
          inherit.aes = FALSE) +
geom_text(data = ndf %>%
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





