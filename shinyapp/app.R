## app.R ##
library(shiny)
library(shinydashboard)

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "lanalytics dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Input data", tabName = "dashboard", icon = icon("database")),
    menuItem("Data analysis", tabName = "widgets", icon = icon("dashboard")),
    menuItem("Output data", tabName = "widgets", icon = icon("save"))
  )),
  
  dashboardBody(fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                tags$hr(),
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ','),
                radioButtons('quote', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '"'),
                tableOutput('contents')
  ),
  skin = "black"
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  output$contents <- renderTable({

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    head(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote))
  })
}

shinyApp(ui, server)