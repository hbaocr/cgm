#
# This is the user-interface definition of a Shiny web application for displaying glucose levels. You can
# run the application by clicking 'Run App' above.
#
# deploy: rsconnect::deployApp("Librelink")

library(lubridate)

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(


    # Application title
    titlePanel("Richard Sprague Continuous Monitoring", windowTitle = "Richard Sprague"),
    tags$a(href="https://richardsprague.com", "More details"),


    # Sidebar
    sidebarLayout(
        sidebarPanel(
            # sliderInput("date_range",
            #             "Choose Date Range:",
            #             min = lubridate::as_datetime("2018-12-03"), max = lubridate::now(),
            #             value = c(lubridate::as_datetime("2019-10-16"), lubridate::as_datetime("2019-10-16"))
            # ),
            dateInput("date1", "Start Date:", value = as_datetime(now()-days(2), tz="America/Los_Angeles")),
            dateInput("date2", "End Date:", value = as_datetime(now(),tz="America/Los_Angeles")),
            numericInput(inputId="a_user", label="User ID", value=13),
            submitButton( text="Submit"),

        ),

        # Show a plot of the  results
        mainPanel(
            verbatimTextOutput(outputId="a_user"),

            plotOutput("glucoseLevelsPlot")
        )
    )
))
