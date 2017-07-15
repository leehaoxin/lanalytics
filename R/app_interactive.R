# ## app.R ##
# library(shiny)
# library(shinydashboard)
# library(lanalytics)
# library(tidyverse)
# library(stringr)
# library(ggvis)
# library(ltm)
# 
# ## lanalytics ##
# 
# read_lc <- function(file){
#   quiz_sheet <- readr::read_csv(file) %>% 
#     setNames(tolower(names(.)))
#   
#   if(!("email address" %in% names(quiz_sheet))){
#     stop("Error: The file does not contain a column called \"email address\"")
#   }
#   
#   quiz_sheet <-  quiz_sheet %>% 
#     dplyr::filter(stringr::str_detect(`email address`, "@")) %>% 
#     dplyr::mutate(`email address` = as.character(`email address`))
#   
#   quiz_long <- lapply(c("responded at", "score"), function(df_subset){
#     selected_cols <- c("email address",
#                        names(quiz_sheet)[stringr::str_detect(names(quiz_sheet), df_subset)])
#     
#     quiz_long_sheet <- quiz_sheet[names(quiz_sheet) %in% selected_cols] %>% 
#       tidyr::gather(question, value, 
#                     -c(`email address`)) %>% 
#       dplyr::mutate(question = str_extract(tolower(question), "question.[0-9]*"),
#                     question = str_replace(question, "question ", ""))
#     
#     names(quiz_long_sheet)[which(names(quiz_long_sheet) == "value")] <- df_subset
#     quiz_long_sheet
#   }) 
#   
#   quiz_long <- purrr::reduce(quiz_long, left_join) %>% 
#     dplyr::mutate(quiz = file,
#                   `responded at` = parse_datetime(x = `responded at`, 
#                                                   format = "%d/%m/%Y %H:%M"))
#   class(quiz_long) <- c("quizz", class(quiz_long))
#   quiz_long
# }
# 
# add_times <- function(course){
#   course <- course %>% 
#     dplyr::group_by(quiz, `email address`) %>%
#     dplyr::arrange(quiz, `email address`, `responded at`) %>% 
#     dplyr::mutate(`order answer` = 1:n(),
#                   `time per question` = `responded at` - lag(`responded at`),
#                   question = as.numeric(question))
# }
# 
# plot_order <- function(quiz_object){
#   quiz_object %>% 
#     dplyr::group_by(question) %>% 
#     dplyr::mutate(`time tercil` = cut(as.numeric(`time per question`), 
#                                       breaks = quantile(as.numeric(.$`time per question`), 
#                                                         probs = c(0, .33, .66, 1), 
#                                                         na.rm = T), 
#                                       include.lowest = T, 
#                                       labels = c("First Tercil", "Second Tercil", "Third Tercil"))) %>% 
#     dplyr::filter(!is.na(`time tercil`)) %>% 
#     dplyr::group_by(question, `time tercil`) %>% 
#     dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
#     dplyr::filter(`mean score`>.05) %>% 
#     ggplot2::ggplot(aes(x = question, 
#                         y = `mean score`, 
#                         group = `time tercil`, 
#                         color = `time tercil`)) +
#     ggplot2::facet_wrap(~`time tercil`, ncol = 1) +
#     geom_line() +
#     geom_point()
# }
# 
# 
# plot_easiness_time <- function(quiz_object){
#   quiz_object %>%
#     dplyr::mutate(score = as.numeric(score)) %>% 
#     dplyr::group_by(quiz, question) %>% 
#     dplyr::summarise(`mean score` = mean(score, na.rm = T),
#                      `mean time` = median(`time per question`, na.rm = T)) %>% 
#     dplyr::filter(`mean score` > 0, 
#                   `mean time` < 600) %>% 
#     ggplot2::ggplot(aes(x = `mean time`, 
#                         y = `mean score`)) +
#     ggplot2::geom_point()+
#     ggplot2::geom_smooth(method = "lm")
# }
# 
# plot_guessers <- function(quiz_object){
#   thresholds_df <- quiz_object %>% 
#     dplyr::group_by(quiz) %>% 
#     dplyr::filter(question %in% c(1,2, max(question), (max(question)-1))) %>% 
#     dplyr::group_by(`email address`, quiz) %>% 
#     dplyr::summarise(threshold = min(`time per question`, na.rm = T),
#                      threshold = ifelse(threshold == Inf, NA, threshold),
#                      threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) 
#   
#   guessing <- quiz_object %>% 
#     dplyr::left_join(thresholds_df) %>% 
#     dplyr::mutate(guessing = ifelse((!is.na(`time per question`) & `time per question` < threshold), 
#                                     ifelse(score == 1, 1, -1), 0)) %>% 
#     dplyr::arrange(`time per question`) 
#   
#   guessing %>% 
#     dplyr::select(`email address`, guessing, question) %>% 
#     dplyr::filter(guessing != 0) %>%
#     ggplot2::ggplot(aes(x = `email address`, 
#                         y = question, 
#                         color = factor(guessing))) +
#     ggplot2::geom_point() +
#     theme(axis.text.x = element_text(angle=90, hjust = 1))
# }
# 
# 
# plot_etl <- function(quiz_object, challengeLevel){
#   homo_quiz_object <- quiz_object %>% 
#     dplyr::mutate(score = as.numeric(score)) %>% 
#     dplyr::group_by(quiz, question) %>% 
#     dplyr::summarise(`mean score` = mean(score, na.rm = T),
#                      `mean time` = median(`time per question`, na.rm = T)) %>%
#     dplyr::filter(`mean score` > 0, 
#                   `mean time` < 600) %>% 
#     dplyr::mutate(question_id = paste0("Q4_", "q", question)) %>% 
#     dplyr::select(question_id, `mean time`, `mean score`)
#   
#   homo_challenge_level <- challengeLevel %>% 
#     dplyr::rename(question_id = MCM.2014.item,
#                   rating = Rating.HB) %>% 
#     dplyr::select(question_id, rating)
#   
#   homo_quiz_object %>% 
#     dplyr::left_join(homo_challenge_level) %>% 
#     dplyr::mutate(`mean time` = as.numeric(`mean time`),
#                   rating = factor(rating)) %>% 
#     ggplot2::ggplot(aes(x = `mean time`, 
#                         y = `mean score`, 
#                         color = rating, 
#                         label = question_id)) +
#     ggplot2::geom_point() +
#     ggplot2::geom_text()
# }
# 
# 
# 
# plot_rasch <- function(quiz_object){
#   x_vals = seq(zrange[1], zrange[2], length = 100)
#   d_matrix <- cbind(1, x_vals)
#   
#   data_tibble <- quiz_object %>% 
#     ungroup() %>% 
#     dplyr::select(`email address`, question, score) %>% 
#     tidyr::spread(question, score, fill = 0) %>% 
#     dplyr::select(-`email address`) %>% 
#     stats::setNames(paste("item", names(.))) %>% 
#     purrr::map_if(is.character, as.numeric) %>% 
#     tibble::as_tibble() %>% 
#     purrr::discard(~sum(.)==0)
#   
#   model <- rasch(data_tibble)
#   betas <- model$coefficients
#   
#   plogis(d_matrix %*% t(betas)) %>%
#     as_tibble() %>% 
#     mutate(x_vals = x_vals) %>% 
#     gather(item, value, -x_vals) %>% 
#     ggplot(aes(x = x_vals, y = value, group = item, color = item)) +
#     geom_line()
# }  
# 
# vis_rasch <- function(quiz_object){
#   x_vals = seq(zrange[1], zrange[2], length = 100)
#   d_matrix <- cbind(1, x_vals)
#   
#   data_tibble <- quiz_object %>% 
#     dplyr::select(`email address`, question, score) %>% 
#     tidyr::spread(question, score, fill = 0) %>% 
#     dplyr::select(-c(`email address`)) %>% 
#     setNames(paste("item", names(.))) %>% 
#     map_if(is.character, as.numeric) %>% 
#     as_tibble() %>% 
#     discard(~sum(.)==0)
#   
#   model <- rasch(data_tibble)
#   betas <- model$coefficients
#   
#   plogis(d_matrix %*% t(betas)) %>%
#     as_tibble() %>% 
#     mutate(x_vals = x_vals) %>% 
#     gather(item, value, -x_vals) %>% 
#     ggvis(~x_vals, ~value, stroke = ~item, strokeWidth:= 3) %>% 
#     layer_lines() %>% 
#     group_by(item) %>% 
#     add_tooltip(function(df) df$item)
# }  
# 
# 
# 
# 
# # ui ----------------------------------------------------------------------
# header <- dashboardHeader(title = "lanalytics Dashboard")
# 
# sidebar <- dashboardSidebar(sidebarMenu(
#   br(),
#   menuItem("Instructions", tabName = "1_instructions", icon = icon("info")),
#   menuItem("Import quizzes", tabName = "2_input", icon = icon("th")),
#   menuItem("Display quizzes", tabName = "3_display", icon = icon("eye")),
#   menuItem("Data analysis", tabName = "4_analysis", icon = icon("dashboard"),
#            menuSubItem("Individual analysis", tabName = "4_1_individual", icon = icon("dashboard")),
#            menuSubItem("Quiz analysis", tabName = "4_2_quiz", icon = icon("dashboard")),
#            menuSubItem("Grupal analysis", tabName = "4_3_grupal", icon = icon("dashboard"))
#   ),
#   br(),
#   actionButton("quit_button", "Exit", icon("sign-out")),
#   helpText("\t Press 'Exit' to quit app")
# ), width = 200)
# 
# body <- dashboardBody(
#   
#   tabItems(
#     tabItem(tabName = "1_instructions", 
#             fluidPage(
#               titlePanel(strong("Welcome to the lanalytics dashboard!")),
#               br(),
#               br(),
#               sidebarLayout(
#                 sidebarPanel(
#                   h2("Introduction"),
#                   p("The lanalytics dashboard is an interface of the", strong("lanalytics"), "package, 
#                     which provides useful functions to analyze online quizzes."),
#                   br(),
#                   br(),
#                   p("If you want to use the functions outside this dashboard, you can install the package
#                     with devtools:"),
#                   code("install_github('savrgg/lanalytics')"),
#                   br(),
#                   br(),
#                   p("For an introduction and examples visit the ", 
#                     a("lanalytics homepage.", 
#                       href = "https://savrgg.github.io/lanalytics/"))
#                   ),
#                 mainPanel(
#                   h1("Instructions"),
#                   p("1) Load quiz dataset from a *.csv file (hint: you can upload several *.csv files at the same time)"), 
#                   br(),
#                   p("For an introduction and live examples, visit the ",
#                     a("Shiny homepage.", 
#                       href = "http://www.rstudio.com/shiny")),
#                   br(),
#                   h2("Features"),
#                   p("Feature 1"),
#                   p("Feature 2")
#                 )
#                 )
#             )
#     ),
#     
#     tabItem(tabName = "2_input",
#             fluidRow(
#               box(title = "Import quiz file", width = 6,
#                   status = "primary", 
#                   collapsible = TRUE,
#                   fileInput('file1', 'Select file:',
#                             accept = c('.csv'),
#                             multiple = TRUE
#                   ),
#                   actionButton(inputId = "upload_quiz_dataset", 
#                                label = "Upload quiz dataset")
#               ),
#               box(title = "Import cognitive levels file", status = "primary", width = 6,
#                   collapsible = TRUE,
#                   fileInput('file2', 'Select file:',
#                             accept = c('.csv')
#                   ),
#                   actionButton(inputId = "upload_cognitive_dataset", 
#                                label = "Upload cognitive dataset")
#               )
#             ) # end fluidrow 2.2
#     ),
#     
#     tabItem(tabName = "3_display", 
#             fluidRow(
#               box(title = "Quiz dataset:", status = "primary", width = 12,
#                   collapsible = TRUE,
#                   DT::dataTableOutput('quiz_dataset')
#               )
#             ), 
#             fluidRow(
#               box(title = "Cognitive levels dataset:", status = "primary", width = 12,
#                   collapsible = TRUE,
#                   DT::dataTableOutput('cognitive_dataset')
#               )
#             )
#     ),
#     
#     tabItem(tabName = "4_analysis"),
#     
#     tabItem(tabName = "4_1_individual",
#             fluidRow(
#               br(),
#               titlePanel(strong("Individual!")),
#               box(title = "Select quizzes to analize:", status = "info", width = 10,
#                   collapsible = TRUE,
#                   uiOutput("choose_files_1")
#               )
#             ),
#             
#             fluidRow(
#               box(title = "Guessers", status = "primary", width = 6,
#                   collapsible = TRUE,
#                   plotOutput("plot_guessers")
#               ),
#               box(title = "Order plot", status = "primary", width = 6,
#                   collapsible = TRUE,
#                   plotOutput("plot_order")
#               )
#             )
#             
#             
#     ),
#     
#     tabItem(tabName = "4_2_quiz",
#             fluidRow(
#               br(),
#               titlePanel(strong("Quiz")),
#               box(title = "Select quizzes to analize:", status = "info", width = 10,
#                   collapsible = TRUE,
#                   uiOutput("choose_files_2")
#               )
#             ),
#             
#             fluidRow(
#               box(title = "ICC plot", status = "primary", width = 12,
#                   collapsible = TRUE,
#                   plotOutput("plot_rasch")
#               )
#             ),
#             
#             fluidRow(
#               # box(title = "Easiness-time", status = "primary", width = 6,
#               #     collapsible = TRUE,
#               #     plotOutput("plot_et")
#               # ),
#               box(title = "ETL", status = "primary", width = 6,
#                   collapsible = TRUE,
#                   plotOutput("plot_etl")
#               )
#             )
#     ),
#     
#     tabItem(tabName = "4_3_grupal",
#             
#             fluidRow(
#               titlePanel(strong("Grupal")),
#               box(title = "Select quizzes to analize:", status = "info", width = 12,
#                   collapsible = TRUE,
#                   uiOutput("choose_files_3")
#               )
#             ),
#             fluidRow( ## borrar
#               box(title = "Easiness-time", status = "primary", width = 6,  ## borrar
#                   collapsible = TRUE, ## borrar
#                   plotOutput("plot_et")
#               ), ## borrar
#               box(title = "Plot test", status = "primary", width = 6,  ## borrar
#                   collapsible = TRUE, ## borrar
#                   ggvisOutput("plot_test")
#               ) ## borrar
#             )   ## borrar
#     )
#     )
# )
# 
# 
# ui <- dashboardPage(
#   header,
#   sidebar,
#   body,
#   skin = "black"
# )
# 
# # server ------------------------------------------------------------------
# 
# server <- function(input, output) {
#   # source("../R/1_parse.R")
#   # source("../R/2_order.R")
#   # source("../R/3_easiness_time.R")
#   # source("../R/4_guessers.R")
#   # source("../R/5_ETL.R")
#   
#   q <- observe({
#     if (input$quit_button == 1) stopApp()
#   })
#   
#   infile_quiz <- eventReactive(input$upload_quiz_dataset, {
#     infile_quiz <- input$file1
#     if (is.null(infile_quiz))
#       return(NULL)
#     infile_quiz
#   })
#   
#   infile_cognitive <- eventReactive(input$upload_cognitive_dataset, {
#     infile_cognitive <- input$file2
#     if (is.null(infile_cognitive))
#       return(NULL)
#     infile_cognitive
#   })
#   
#   df_quiz <- eventReactive(input$upload_quiz_dataset, {
#     xx <- lapply(1:nrow(infile_quiz()), function(num){
#       add_times(read_lc(infile_quiz()$datapath[num]) %>% 
#                   mutate(quiz = infile_quiz()$name[num]))
#     })
#     do.call(rbind, xx)
#   })
#   
#   df_cognitive <- eventReactive(input$upload_cognitive_dataset, {
#     read.csv(infile_cognitive()$datapath)
#   })
#   
#   output$quiz_dataset <- DT::renderDataTable({
#     DT::datatable(df_quiz(),
#                   extensions = 'Responsive',
#                   options = list(
#                     deferRender = TRUE,
#                     scrollY = 200,
#                     scroller = TRUE
#                   ))
#   })  
#   
#   output$cognitive_dataset <- DT::renderDataTable({
#     DT::datatable(df_cognitive(),
#                   extensions = 'Responsive',
#                   options = list(
#                     deferRender = TRUE,
#                     scrollY = 200,
#                     scroller = TRUE
#                   ))
#   })  
#   
#   output$plot_order <- renderPlot({
#     if(exists("df_quiz")){
#       input$newplot
#       plot_order(df_quiz() %>% filter(quiz %in% input$choose_files_1))  
#     }
#   })
#   
#   output$plot_guessers <- renderPlot({
#     if(exists("df_quiz")){
#       input$newplot
#       plot_guessers(df_quiz() %>% filter(quiz %in% input$choose_files_1))  
#     }
#   })
#   
#   output$plot_et <- renderPlot({
#     if(exists("df_quiz")){
#       input$newplot
#       plot_easiness_time(df_quiz() %>% filter(quiz %in% input$choose_files_2))  
#     }
#   })
#   
#   output$plot_etl <- renderPlot({
#     if(exists("df_quiz")){
#       input$newplot
#       plot_etl(df_quiz() %>% filter(quiz %in% input$choose_files_2), df_cognitive())  
#     }
#   })
#   
#   output$plot_rasch <- renderPlot({
#     if(exists("df_quiz")){
#       input$newplot
#       plot_rasch(df_quiz() %>% filter(quiz %in% input$choose_files_2))  
#     }
#   })
#   
#   output$shiny_test <- reactive({
#     vis_rasch(quiz_object)
#   }) %>% bind_shiny("plot_test")
#   
#   
#   output$choose_files_1 <- renderUI({
#     checkboxGroupInput("choose_files_1", "Choose files", 
#                        choices  = c(infile_quiz()$name),
#                        selected = c(infile_quiz()$name)[1])
#   })
#   output$choose_files_2 <- renderUI({
#     checkboxGroupInput("choose_files_2", "Choose files", 
#                        choices  = c(infile_quiz()$name),
#                        selected = c(infile_quiz()$name)[1])
#   })
#   output$choose_files_3 <- renderUI({
#     checkboxGroupInput("choose_files_3", "Choose files", 
#                        choices  = c(infile_quiz()$name),
#                        selected = c(infile_quiz()$name)[1])
#   })
#   
# } # end server
# 
# shinyApp(ui, server)