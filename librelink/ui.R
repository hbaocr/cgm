#
# This is the user-interface definition of a Shiny web application for displaying glucose levels. You can
# run the application by clicking 'Run App' above.
#
# deploy: rsconnect::deployApp("Librelink")


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Glucose levels for https://psm.personalscience.com"),


    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # sliderInput("date_range",
            #             "Choose Date Range:",
            #             min = lubridate::as_datetime("2018-12-03"), max = lubridate::now(),
            #             value = c(lubridate::as_datetime("2019-10-16"), lubridate::as_datetime("2019-10-16"))
            # ),
            dateInput("date1", "Start Date:", value = lubridate::as_datetime("2019-10-28")),
            dateInput("date2", "End Date:", value = lubridate::as_datetime("2019-10-30"))

        ),

        # Show a plot of the generated distribution
        mainPanel(

            plotOutput("glucoseLevelsPlot")
        )
    )
))
