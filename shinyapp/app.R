## app.R ##
library(shiny)
library(shinydashboard)

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "lanalytics dashboard"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Import one quiz", tabName = "Input", icon = icon("database")),
    menuItem("Import multiple quizzes", tabName = "Input_multiple", icon = icon("database")),
    menuItem("Data analysis", tabName = "Analysis", icon = icon("dashboard")),
    menuItem("Output data", tabName = "Output", icon = icon("save"))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Input",
              h2("Import the data from Learning Catalytics"),
              fluidRow(
                box(
                  fileInput('file1', 'Choose CSV File',
                            accept = c('text/csv', 
                                       'text/comma-separated-values,text/plain',
                                       '.csv')),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',', Semicolon=';', Tab='\t'), ','),
                  radioButtons('quote', 'Quote', 
                               c(None='', 'Double Quote'='"', 'Single Quote'="'"),'"')
                ),
                
                box(
                  tableOutput('contents')
                )
              )
              ),
      
      tabItem(tabName = "Input_multiple",
              h2("hola input multiple")
      ),
      
      tabItem(tabName = "Analysis",
              h2("hola analisis")
              ),
      
      tabItem(tabName = "Output",
              h2("hola output")
              )
      )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  source("../R/1_parse.R")
  output$contents <- renderTable({

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    head(read_lc(inFile$datapath))
  })
}

shinyApp(ui, server)