# scratch.r okay to delete

#source("librelink/GlobalEnv_functions_n_variables.R")
library(tidyverse)

inFile <- "start"
inFile$datapath <- file.path(here::here(),"librelink","Librelink.csv") #input$file_glucose_measure_librelink
inFile$file_activity_track <- file.path(here::here(),"librelink","Rik Activity 2019.csv")

libre_raw <- data.table::fread(inFile$datapath, header = TRUE,
                                sep = ",",  #input$sep_1,
                                quote = "", #input$quote_1,
                                skip = 1) # skip the 1st row as it contains metaInfo.


glucose <- libre_raw %>% select(time = "Meter Timestamp",
                                scan = "Scan Glucose(mg/dL)",
                                hist = "Historic Glucose(mg/dL)",
                                strip = "Strip Glucose(mg/dL)",
                                food = "Notes")


glucose$value <- dplyr::if_else(is.na(glucose$scan),glucose$hist,glucose$scan)
glucose$time <- lubridate::as_datetime(glucose$time)
glucose_raw <- glucose


activity_dt <- data.table::fread(inFile$file_activity_track, header = TRUE,
                                 sep = ",") #input$quote_2)
# b), insert
# activity_dt[,  user_id := user_id_value]
# setkey(activity_dt, user_id)
# # c), column reorder
# activity_dt %>% setcolorder(., neworder = key(.)) # move the key to the front place.
#
activity_dt[ , `Start` := char_to_datetime(`Start`)] # convert to datetime column
activity_dt[ , `End` := char_to_datetime(`End`)] # convert to datetime column

# ggplot(glucose_raw ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
#   glucose_ref_band +
#   geom_rect(data=activity_dt %>% dplyr::filter(Activity == "Sleep") %>%
#               select(xmin = Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
#             aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
#             fill="red",
#             alpha=0.2,
#             inherit.aes = FALSE)

cgm_display(activity_df = activity_dt,
             glucose_df = glucose_raw, ref_band = glucose_ref_band)
