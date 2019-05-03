#: ----plotting----
# if input$date_range changes, that will trigger renderPlot again.
cgm_display(start = lubridate::as_datetime(input$date1), 
            end = lubridate::as_datetime(input$date2), 
            activity_df = activity_dt,
            glucose_df = glucose_dt, 
            ref_band = glucose_ref_band)


p <- ggplot(glucose_df ,aes(x=time,y=value)) + geom_line(size=2, color = "red")
# p <- p + geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")
p <- p + ref_band
p <- p +  geom_rect(data=activity_df %>% dplyr::filter(Activity == "Sleep") %>%
              select(xmin = Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="red",
            alpha=0.2,
            inherit.aes = FALSE)
p <- p +  geom_rect(data=activity_df %>% dplyr::filter(Activity == "Exercise") %>%
              select(xmin = Start,xmax = End),
            aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
            fill="blue",
            alpha=0.2,
            inherit.aes = FALSE)
p <- p +  geom_vline(xintercept = activity_df %>%
               dplyr::filter(Activity == "Event" & Comment == "awake") %>% select("Start") %>% unlist(),
             color = "green")
p <- p +  geom_vline(xintercept = activity_df %>%
               dplyr::filter(Activity == "Food") %>% select("Start") %>% unlist(),
             color = "yellow")
p <- p +  geom_text(data = activity_df %>%
              dplyr::filter(Activity == "Food") %>% select("Start","Comment") ,
            aes(x=Start,y=50, angle=90, hjust = FALSE,  label = Comment),
            size = 6)
p <- p +  labs(title = "Glucose (mg/dL)", subtitle = start) +  theme(plot.title = element_text(size=22))+
  scale_x_datetime(limits = c(start,end))



p <- glucose_dt %>% plot_ly(., x = ~time, y = ~value, 
                       mode = "lines+markers", 
                       name = "cgm", 
                       type = "scatter",
                       color = I("gray60"))

p <- p %>% add_ribbons(., x = ~time, ymin = 100, ymax = 140, 
                  line = list(color = 'rgba(7, 164, 181, 0.05)'), # blue, use this ref website: https://convertingcolors.com/rgb-color-7_164_181.html
                  fillcolor = 'rgba(34, 139, 34, 0.2)',# forest green: 34,139,34
                  #color = I("green"), opacity = 0.1, 
                  inherit = FALSE,
                  name = "safe zone")

#: define the 
sleep_activity_dt <- activity_dt %>% dplyr::filter(Activity == "Sleep") %>%
  dplyr::select(xmin = Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf) %>% as.data.table()

p %>% add_segments(., data = sleep_activity_dt,
                   x = sleep_activity_dt$xmin, xend = sleep_activity_dt$xmax, y = sleep_activity_dt$ymin, yend = sleep_activity_dt$ymax, 
                   inherit = TRUE, color = I("red"), name = "sleep hours"
                   )

p %>% add_ribbons(., data = sleep_activity_dt,
                       x = ~xmin, ymin = 100, ymax = 140, 
                       #line = list(color = 'rgba(7, 164, 181, 0.05)'), # blue, use this ref website: https://convertingcolors.com/rgb-color-7_164_181.html
                       #fillcolor = 'rgba(34, 139, 34, 0.2)',# forest green: 34,139,34
                       color = I("red"), opacity = 0.1, 
                       inherit = FALSE,
                       name = "sleep hours")


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


