## app.R ##
library(devtools)
library(fields)
library(Hmisc)
library(lubridate)
library(knitr)
library(openssl)
library(random)
library(RColorBrewer)
library(roxygen2)
library(rstudioapi)
library(shiny)
library(shinydashboard)
library(testthat)
library(tidyverse)
library(xlsx)

# ui ----------------------------------------------------------------------
if (interactive()) {
  
header <- dashboardHeader(title = "lanalytics dashboard")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Import quizzes", tabName = "Input", icon = icon("database")),
  menuItem("Data analysis", tabName = "Analysis", icon = icon("dashboard"))
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Input",
            fluidRow(
              box(title = "Import quiz file", width = 4,
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  fileInput('file1', 'Select file:',
                            accept = c('text/csv', 
                                     'text/comma-separated-values,text/plain',
                                     '.csv'
                                     ),
                            multiple = TRUE
                            ),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',', Semicolon=';', Tab='\t'), ',')
              ),
              
              box(title = "Quiz dataset:", status = "primary", width = 8,
                  solidHeader = TRUE, collapsible = TRUE,
                  DT::dataTableOutput('contents_single')
              )
            ), # fluidrow 1
            fluidRow(
              box(title = "Import cognitive levels file", status = "primary", width = 4,
                  solidHeader = TRUE, collapsible = TRUE,
                  fileInput('file2', 'Select file:',
                            accept = c('text/csv', 
                                       'text/comma-separated-values,text/plain',
                                       '.csv')
                            ),
                  checkboxInput('header2', 'Header', TRUE),
                  radioButtons('sep2', 'Separator',
                               c(Comma=',', Semicolon=';', Tab='\t'), ',')
              ),
              
              box(title = "Cognitive levels dataset:", status = "primary", width = 8,
                  solidHeader = TRUE, collapsible = TRUE,
                  DT::dataTableOutput('contents_cognitive')
              )
            ) # fluidrow 2
    ),
    tabItem(tabName = "Analysis",
            fluidRow(
              box(title = "Select quizzes to analize:", status = "info", width = 12,
                  solidHeader = TRUE, collapsible = TRUE,
                  uiOutput("choose_files")
              )
            ),
            
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
  
  
  output$contents_single <- DT::renderDataTable({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      print(inFile$datapath)
      xx <- lapply(1:nrow(inFile), function(num){
        read_lc(inFile$datapath[num]) %>% 
          mutate(quiz = inFile$name[num]) %>% 
          dplyr::select(quiz, question, id, responded.at, score )
      })
      df_test <- data.frame(do.call(rbind, xx))
      DT::datatable(df_test,
                    extensions = 'Responsive',
                    options = list(
                      deferRender = TRUE,
                      scrollY = 200,
                      scroller = TRUE
                      ))
    })  
  
  output$contents_cognitive <- DT::renderDataTable({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    df_test_2 <- read.csv(inFile2$datapath)
    DT::datatable(df_test_2,
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })  

  output$plot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    print(inFile$datapath)
    xx <- lapply(1:nrow(inFile), function(num){
      read_lc(inFile$datapath[num]) %>% 
        mutate(quiz = inFile$name[num])
    })
    df_test <- data.frame(do.call(rbind, xx))
    if(exists("df_test")){
      input$newplot
      plot_order(df_test %>% filter(quiz %in% input$files))  
    }
  })
  
  output$plot2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    print(inFile$datapath)
    xx <- lapply(1:nrow(inFile), function(num){
      read_lc(inFile$datapath[num]) %>% 
        mutate(quiz = inFile$name[num]) 
    })
    df_test <- data.frame(do.call(rbind, xx))
    if(exists("df_test")){
      input$newplot
      plot_easiness_time(df_test %>% filter(quiz %in% input$files))  
    }
  })
  
  output$plot3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    print(inFile$datapath)
    xx <- lapply(1:nrow(inFile), function(num){
      read_lc(inFile$datapath[num]) %>% 
        mutate(quiz = inFile$name[num])
    })
    df_test <- data.frame(do.call(rbind, xx))
    if(exists("df_test")){
      input$newplot
      plot_guessers(df_test %>% filter(quiz %in% input$files))  
    }
  })
  
  output$plot4 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    xx <- lapply(1:nrow(inFile), function(num){
      read_lc(inFile$datapath[num]) %>% 
        mutate(quiz = inFile$name[num]) 
    })
    df_test <- data.frame(do.call(rbind, xx))
    
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    df_test_2 <- read_csv(inFile2$datapath) %>% data.frame()
    
    if(exists("df_test")){
      input$newplot
      plot_etl(df_test %>% filter(quiz %in% input$files), df_test_2)  
    }
  })
  
output$choose_files <- renderUI({
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
    checkboxGroupInput("files", "Choose files", 
                       choices  = c(inFile$name),
                       selected = c(inFile$name)[1])
  })
}

shinyApp(ui, server)
}