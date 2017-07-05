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
    setNames(tolower(names(.)))
  
  if(!("email address" %in% names(quiz_sheet))){
    stop("Error: The file does not contain a column called \"email address\"")
  }
  
  quiz_sheet <-  quiz_sheet %>% 
    dplyr::filter(stringr::str_detect(`email address`, "@")) %>% 
    dplyr::mutate(`email address` = as.character(`email address`))
  
  quiz_long <- lapply(c("responded at", "score"), function(df_subset){
    selected_cols <- c("email address",
                       names(quiz_sheet)[stringr::str_detect(names(quiz_sheet), df_subset)])
    
    quiz_long_sheet <- quiz_sheet[names(quiz_sheet) %in% selected_cols] %>% 
      tidyr::gather(question, value, 
                    -c(`email address`)) %>% 
      dplyr::mutate(question = str_extract(tolower(question), "question.[0-9]*"),
                    question = str_replace(question, "question ", ""))
    
    names(quiz_long_sheet)[which(names(quiz_long_sheet) == "value")] <- df_subset
    quiz_long_sheet
  }) 
  
  quiz_long <- purrr::reduce(quiz_long, left_join) %>% 
    dplyr::mutate(quiz = file,
                  `responded at` = parse_datetime(x = `responded at`, 
                                                  format = "%d/%m/%Y %H:%M"))
  class(quiz_long) <- c("quizz", class(quiz_long))
  quiz_long
}

add_times <- function(course){
  course <- course %>% 
    dplyr::group_by(quiz, `email address`) %>%
    dplyr::arrange(quiz, `email address`, `responded at`) %>% 
    dplyr::mutate(`order answer` = 1:n(),
                  `time per question` = `responded at` - lag(`responded at`),
                  question = as.numeric(question))
}

plot_order <- function(quiz_object){
  quiz_object %>% 
    dplyr::group_by(question) %>% 
    dplyr::mutate(`time tercil` = cut(as.numeric(`time per question`), 
                                      breaks = quantile(as.numeric(.$`time per question`), 
                                                        probs = c(0, .33, .66, 1), 
                                                        na.rm = T), 
                                      include.lowest = T, 
                                      labels = c("First Tercil", "Second Tercil", "Third Tercil"))) %>% 
    dplyr::filter(!is.na(`time tercil`)) %>% 
    dplyr::group_by(question, `time tercil`) %>% 
    dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
    dplyr::filter(`mean score`>.05) %>% 
    ggplot2::ggplot(aes(x = question, 
                        y = `mean score`, 
                        group = `time tercil`, 
                        color = `time tercil`)) +
    ggplot2::facet_wrap(~`time tercil`, ncol = 1) +
    geom_line() +
    geom_point()
}


plot_easiness_time <- function(quiz_object){
  quiz_object %>%
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>% 
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    ggplot2::ggplot(aes(x = `mean time`, 
                        y = `mean score`)) +
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm")
}

plot_guessers <- function(quiz_object){
  thresholds_df <- quiz_object %>% 
    dplyr::group_by(quiz) %>% 
    dplyr::filter(question %in% c(1,2, max(question), (max(question)-1))) %>% 
    dplyr::group_by(`email address`, quiz) %>% 
    dplyr::summarise(threshold = min(`time per question`, na.rm = T),
                     threshold = ifelse(threshold == Inf, NA, threshold),
                     threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) 
  
  guessing <- quiz_object %>% 
    dplyr::left_join(thresholds_df) %>% 
    dplyr::mutate(guessing = ifelse((!is.na(`time per question`) & `time per question` < threshold), 
                                    ifelse(score == 1, 1, -1), 0)) %>% 
    dplyr::arrange(`time per question`) 
  
  guessing %>% 
    dplyr::select(`email address`, guessing, question) %>% 
    dplyr::filter(guessing != 0) %>%
    ggplot2::ggplot(aes(x = `email address`, 
                        y = question, 
                        color = factor(guessing))) +
    ggplot2::geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust = 1))
}


plot_etl <- function(quiz_object, challengeLevel){
  homo_quiz_object <- quiz_object %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>%
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    dplyr::mutate(question_id = paste0("Q4_", "q", question)) %>% 
    dplyr::select(question_id, `mean time`, `mean score`)
  
  homo_challenge_level <- challengeLevel %>% 
    dplyr::rename(question_id = MCM.2014.item,
                  rating = Rating.HB) %>% 
    dplyr::select(question_id, rating)
  
  homo_quiz_object %>% 
    dplyr::left_join(homo_challenge_level) %>% 
    dplyr::mutate(`mean time` = as.numeric(`mean time`),
                  rating = factor(rating)) %>% 
    ggplot2::ggplot(aes(x = `mean time`, 
                        y = `mean score`, 
                        color = rating, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggplot2::geom_text()
}






# ui ----------------------------------------------------------------------
header <- dashboardHeader(title = "lanalytics Dashboard")

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
 # source("../R/1_parse.R")
 # source("../R/2_order.R")
 # source("../R/3_easiness_time.R")
 # source("../R/4_guessers.R")
 # source("../R/5_ETL.R")
 #  
  output$contents_single <- DT::renderDataTable({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      print(inFile$datapath)
      xx <- lapply(1:nrow(inFile), function(num){
        add_times(read_lc(inFile$datapath[num]) %>% 
                    mutate(quiz = inFile$name[num]))
      })
      df_test <- do.call(rbind, xx)
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
      add_times(read_lc(inFile$datapath[num]) %>% 
                  mutate(quiz = inFile$name[num]))
    })
    df_test <- do.call(rbind, xx)
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
      add_times(read_lc(inFile$datapath[num]) %>% 
                  mutate(quiz = inFile$name[num]))
    })
    df_test <- do.call(rbind, xx)
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
      add_times(read_lc(inFile$datapath[num]) %>% 
                  mutate(quiz = inFile$name[num]))
    })
    df_test <- do.call(rbind, xx)
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
      add_times(read_lc(inFile$datapath[num]) %>% 
                  mutate(quiz = inFile$name[num]))
    })
    df_test <- do.call(rbind, xx)
    
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