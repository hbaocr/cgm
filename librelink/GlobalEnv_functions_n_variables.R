#: ====Global Variables===============================================
#: ----glucose reference band----
# static
# a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
glucose_ref_band <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=100,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)


#: ====Global Functions===============================================
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





# #:----define myDate class for read in timestamp column as timestamp
# # e.g., fread(..., colClasses = list("myDate" = "col_A_in_data"))
# setClass('myTimestamp')
# setAs("character","myTimestamp", function(from) lubridate::as_datetime(from, tz = "US/Pacific", format="%m/%d/%Y %H:%M") )
# setAs("character","myTimestamp", function(from) as.POSIXct(from, tz = "US/Pacific", format="%m/%d/%Y %H:%M") )
#: a func to convert character column to datetime tppe.

