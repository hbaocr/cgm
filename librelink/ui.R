#
# This is the user-interface definition of a Shiny web application for displaying glucose levels. You can
# run the application by clicking 'Run App' above.
#
# deploy: rsconnect::deployApp("Librelink")


# Define UI for application that draws a histogram

shinyUI(
  
  fluidPage(
    
    # Application title
    titlePanel("Glucose levels for https://psm.personalscience.com"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      
      
      sidebarPanel(
        
        textInput("email_input_1", "Enter your email", value = "", width = NULL,
                  placeholder = "e.g., richard.sprague@gmail.com"),
        textInput("email_input_2", "Re-Enter your email", value = "", width = NULL,
                  placeholder = "e.g., richard.sprague@gmail.com"),
        uiOutput("print_email_check_consist_msg"),
        textOutput("email_check_consist_msg"),

        tags$hr(),
        #: ----1, input window for glucose measure data----
        fileInput('file_glucose_measure_librelink', 'Choose continuous glucose monitor (librelink) file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        checkboxInput('header_1', 'Header', TRUE),
        radioButtons('sep_1', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote_1', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr(),
        
        #:----2, input window for diet record----
        fileInput('file_activity_track', 'Choose activity track file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        checkboxInput('header_2', 'Header', TRUE),
        radioButtons('sep_2', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote_2', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr(),
        
        p('If you want a sample .csv or .tsv file to upload,',
          'you can first download the sample',
          a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
          a(href = 'pressure.tsv', 'pressure.tsv'),
          'files, and then try uploading them.'
        ),
        
        #--------submit button----------------------
        tags$hr(),
        
        actionButton("submit", "Submit"),
        conditionalPanel(condition = "output.flag_fail_invalidMail_panel == true", # as javascript expression
                         {
                           span("Please fix email inputs and submit!", style = "color:red")
                         }), 
        
        
        #--------data range---------------------------
        tags$hr(),
        sliderInput("date_range",
                    "Choose Date Range:",
                    min = lubridate::as_datetime("2018-12-03"), max = lubridate::now(),
                    value = c(lubridate::as_datetime("2018-12-06"), lubridate::as_datetime("2018-12-07"))
        ),
      dateInput("date1", "Start Date:", value = lubridate::as_datetime("2018-12-06")),
      dateInput("date2", "End Date:", value = lubridate::as_datetime("2018-12-07"))
        
        
        
        
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        dataTableOutput('rT_file_glucose_measure_librelink'),
        dataTableOutput('rT_file_activity_track'),
        plotOutput("glucoseLevelsPlot")
      )

    )
  ))
