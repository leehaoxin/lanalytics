## app.R ##
library(devtools)
library(fields)
library(Hmisc)
library(lubridate)
library(knitr)
library(openssl)
library(pkgdown)
library(random)
library(RColorBrewer)
library(roxygen2)
library(rstudioapi)
library(stringr)
library(shiny)
library(shinydashboard)
library(testthat)
library(tidyverse)
library(xlsx)

# ui ----------------------------------------------------------------------
if (interactive()) {
  
header <- dashboardHeader(title = "lanalytics dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Import one quiz", tabName = "Input", icon = icon("database")),
  menuItem("Import multiple quizzes", tabName = "Input_multiple", icon = icon("database")),
  menuItem("Data analysis", tabName = "Analysis", icon = icon("dashboard")),
  menuItem("Output data", tabName = "Output", icon = icon("save"))
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Input",
            fluidRow(
              box(title = "Import quizz file", status = "primary", width = 4,
                  solidHeader = TRUE, collapsible = TRUE,
                  fileInput('file1', 'Select file:',
                            accept = c('text/csv', 
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',', Semicolon=';', Tab='\t'), ','),
                  radioButtons('quote', 'Quote', 
                               c(None='', 'Double Quote'='"', 'Single Quote'="'"),'"')
              ),
              
              box(title = "First 6 rows of data:", status = "primary", width = 8,
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput('contents')
              )
            ), # fluidrow 1
            fluidRow(
              box(title = "Import cognitive levels file", status = "primary", width = 4,
                  solidHeader = TRUE, collapsible = TRUE,
                  fileInput('file2', 'Select file:',
                            accept = c('text/csv', 
                                       'text/comma-separated-values,text/plain',
                                       '.csv')),
                  checkboxInput('header2', 'Header', TRUE),
                  radioButtons('sep2', 'Separator',
                               c(Comma=',', Semicolon=';', Tab='\t'), ','),
                  radioButtons('quote2', 'Quote', 
                               c(None='', 'Double Quote'='"', 'Single Quote'="'"),'"')
              ),
              
              box(title = "First 6 rows of data:", status = "primary", width = 8,
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput('contents2')
              )
            ) # fluidrow 2
    ),
    
    tabItem(tabName = "Input_multiple",
            h2("hola analisis")
    ),
    
    tabItem(tabName = "Analysis",
            fluidRow(
              box(title = "Order plot", status = "primary", width = 6,
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("plot")
              ),
              box(title = "Easiness-time", status = "primary", width = 6,
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("plot2"))
            ),
            fluidRow(
              box(title = "Guessers", status = "primary", width = 6,
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("plot3")
              ),
              box(title = "ETL", status = "primary", width = 6,
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("plot4")
              )
            )
    ),
    
    tabItem(tabName = "Output",
            h2("hola output")
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  source("../R/1_parse.R")
  source("../R/2_order.R")
  source("../R/3_easiness_time.R")
  source("../R/4_guessers.R")
  source("../R/5_ETL.R") 
  
  
  output$contents <- renderTable({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      df_test <- read_lc(inFile$datapath)
      head(df_test) %>% dplyr::select(id, question, responded.at, score) %>% 
          mutate(question = as.character(question),
                 responded.at = strftime(responded.at, format = "%Y-%m-%d %H:%M:%S"))
    })  
  
  output$contents2 <- renderTable({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    df_test_2 <- read.csv(inFile2$datapath)
    head(df_test_2)
  })  

  output$plot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df_test <- read_lc(inFile$datapath)
    if(exists("df_test")){
      input$newplot
      plot_order(df_test)  
    }
  })
  
  output$plot2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df_test <- read_lc(inFile$datapath)
    if(exists("df_test")){
      input$newplot
      plot_easiness_time(df_test)  
    }
  })
  
  output$plot3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df_test <- read_lc(inFile$datapath)
    if(exists("df_test")){
      input$newplot
      guessers(df_test)  
    }
  })
  
  output$plot4 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df_test <- read_lc(inFile$datapath)
    
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    df_test_2 <- read_csv(inFile2$datapath) %>% data.frame()
    
    if(exists("df_test")){
      input$newplot
      etl(df_test, df_test_2)  
    }
  })
  
  
}

shinyApp(ui, server)
}