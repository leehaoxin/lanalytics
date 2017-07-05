## app.R ##
library(shiny)
library(shinydashboard)
library(lanalytics)
library(tidyverse)
library(openssl)
library(stringr)

## lanalytics ##

read_lc <- function(file){
  quiz_sheet <- readr::read_csv(file) %>%  
    data.frame() %>% 
    setNames(tolower(names(.))) %>% 
    dplyr::filter(stringr::str_detect(email.address, "@")) %>% 
    dplyr::mutate(email.address = as.character(email.address),
                  id = md5(email.address)) %>% 
    data.frame() 
  
  
  quiz_long <- lapply(c("response", "responded.at", "score"), function(df_subset){
    cols_select <- c("email.address", "id", "last.name", "first.name", 
                     names(quiz_sheet)[stringr::str_detect(names(quiz_sheet), df_subset)])
    
    quiz_long_join <- quiz_sheet[names(quiz_sheet) %in% cols_select] %>% 
      tidyr::gather(question, value, 
                    -c(email.address, id, last.name, first.name)) %>% 
      tidyr::separate(col = question, 
                      into = c("question", "round", "variable"), 
                      sep = "\\.\\.") %>% 
      dplyr::select(-round, -variable) 
    
    names(quiz_long_join)[which(names(quiz_long_join) == "value")] <- df_subset
    quiz_long_join %>% data.frame
  }) 
  
  purrr::reduce(quiz_long, left_join) %>% 
    dplyr::mutate(quiz = file,
                  responded.at = as.POSIXct(responded.at, 
                                            format = "%d/%m/%Y %H:%M"),
                  response = as.character(response)) %>% 
    dplyr::group_by(quiz, id) %>%
    dplyr::arrange(quiz, id, responded.at) %>% 
    dplyr::mutate(n = 1:n(),
                  time_per_question = responded.at - lag(responded.at),
                  time_per_quiz = sum(time_per_question, na.rm = T),
                  question = as.numeric(str_extract(question, "[0-9]+"))) %>% 
    data.frame %>% 
    dplyr::select(-response)
}

plot_order <- function(df_quizzes){
  df_quizzes %>% 
    ggplot2::ggplot(aes(x = factor(n), 
                        y = factor(question), 
                        group = id)) +
    ggplot2::facet_wrap(~quiz, ncol = 2) +
    ggplot2::geom_point(alpha = .1) + 
    ggplot2::geom_abline(slope=1, intercept=0, color = "red")
}

plot_easiness_time <- function(df_quizzes){
  df_quizzes %>%
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(rate_correct = mean(score, na.rm = T),
                     median_time = median(time_per_question, na.rm = T)) %>% 
    data.frame %>%
    dplyr::filter(rate_correct > 0, 
                  median_time < 600) %>% 
    ggplot2::ggplot(aes(x = median_time, 
                        y = rate_correct)) +
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm")
}
plot_guessers <- function(df_quizzes){
  thresholds_df <- df_quizzes %>% 
    dplyr::group_by(quiz) %>% 
    dplyr::filter(question %in% c(1,2, max(question), (max(question)-1))) %>% 
    data.frame() %>% 
    dplyr::group_by(id, quiz) %>% 
    dplyr::summarise(threshold = min(time_per_question, na.rm = T),
                     threshold = ifelse(threshold == Inf, NA, threshold),
                     threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) %>% data.frame() 
  
  guessing <- df_quizzes %>% 
    dplyr::left_join(thresholds_df) %>% 
    dplyr::mutate(guessing = ifelse((!is.na(time_per_question) & time_per_question < threshold), 
                                    ifelse(score == 1, 1, -1), 0)) %>% arrange(time_per_question) 
  
  guessing %>% 
    dplyr::mutate(quiz_response=question) %>% 
    dplyr::select(id, guessing, quiz_response) %>% 
    dplyr::filter(guessing != 0) %>%
    ggplot2::ggplot(aes(x = as.factor(id), 
                        y = quiz_response, 
                        color = factor(guessing))) +
    ggplot2::geom_point()  
}

plot_etl <- function(df_quizzes, challengeLevel){
  xx1 <- df_quizzes %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(rate_correct = mean(score, na.rm = T),
                     median_time = median(time_per_question, na.rm = T)) %>% data.frame %>% 
    dplyr::filter(rate_correct > 0, 
                  median_time < 600) %>% 
    dplyr::mutate(question_id = paste0("Q4_", "q", question)) %>% 
    dplyr::select(question_id, median_time, rate_correct)
  
  xx2 <- challengeLevel %>% 
    dplyr::rename(question_id = MCM.2014.item,
                  rating = Rating.HB) %>% 
    dplyr::select(question_id, rating)
  
  xx1 %>% 
    dplyr::left_join(xx2) %>% 
    dplyr::mutate(median_time = as.numeric(median_time),
                  rating = factor(rating)) %>% 
    ggplot2::ggplot(aes(x = median_time, 
                        y = rate_correct, 
                        color = rating, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggplot2::geom_text()
}




# ui ----------------------------------------------------------------------
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
#  source("../R/1_parse.R")
#  source("../R/2_order.R")
#  source("../R/3_easiness_time.R")
#  source("../R/4_guessers.R")
#  source("../R/5_ETL.R") 
  
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