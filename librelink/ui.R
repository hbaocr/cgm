#
# This is the user-interface definition of a Shiny web application for displaying glucose levels. You can
# run the application by clicking 'Run App' above.
#
# deploy: rsconnect::deployApp("Librelink")

library(lubridate)

library(shiny)

USER_LIST = c(9, 8, 13, 22, 17)

# Define UI for application that draws a histogram
shinyUI(fluidPage(


    # Application title
    titlePanel("Personal Science Experiments", windowTitle = "Personal Science, Inc."),
    tags$a(href="https://personalscience.com", "More details"),


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
            textInput(inputId="foodname", label="Compare food:", value="Apple Juice"),
            submitButton( text="Submit"),
            checkboxGroupInput(inputId="user_list",label="Users to compare:",
                               choices=USER_LIST,
                               selected=USER_LIST)

        ),

        # Show a plot of the  results
        mainPanel(
            verbatimTextOutput(outputId="a_user"),

            plotOutput("glucoseLevelsPlot"),
            plotOutput("foodComparePlot")
        )
    )
))
