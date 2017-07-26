## app.R ##
library(shiny)
library(shinydashboard)
library(lanalytics)
library(tidyverse)
library(stringr)
library(ggvis)
library(ltm)
library(ggrepel)
library(directlabels)
library(eRm)

## lanalytics ##
# f2 input --------------------------------------------------------
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
                    question = str_replace(question, "question ", "")) %>% 
      filter(!is.na(value))
    
    names(quiz_long_sheet)[which(names(quiz_long_sheet) == "value")] <- df_subset
    quiz_long_sheet
  }) 
  
  quiz_long <- purrr::reduce(quiz_long, left_join) %>% 
    dplyr::mutate(quiz = file,
                  `responded at` = parse_datetime(x = `responded at`, 
                                                  format = "%d/%m/%Y %H:%M"))
  class(quiz_long) <- c("quizz", class(quiz_long))
  quiz_long %>% 
    mutate(score = as.integer(score))
}
add_times <- function(course){
  course <- course %>% 
    dplyr::group_by(quiz, `email address`) %>%
    dplyr::arrange(quiz, `email address`, `responded at`) %>% 
    dplyr::mutate(`order answer` = 1:n(),
                  `time per question` = `responded at` - lag(`responded at`),
                  question = as.numeric(question))
}
# f3 display ------------------------------------------------------
# f4 IRT-discrim --------------------------------------------------
rasch_model <- function(quiz_object){
  x_vals = seq(-3.8, 3.8, length = 100)
  d_matrix <- cbind(1, x_vals)
  
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)==0)
  
  model <- rasch(data_tibble)
  model
}
pars2_model <- function(quiz_object){
  x_vals = seq(-3.8, 3.8, length = 100)
  d_matrix <- cbind(1, x_vals)
  
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)==0)
  
  model <- ltm(data_tibble ~ z1)
  model
}  
pars3_model <- function(quiz_object){
  x_vals = seq(-3.8, 3.8, length = 100)
  d_matrix <- cbind(1, x_vals)
  
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)==0)
  
   model <- tpm(data_tibble)
   model
}  
# f5 IRT-NO-discrim -----------------------------------------------
plot_jointICC <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  plotjointICC(model, xlim = c(-8, 5))
}  
plot_personItem <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  plotPImap(model)
}  
plot_personParameter <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  pp <- person.parameter(model)
  plot(pp)
} 
# f6 display ------------------------------------------------------
plot_hist <- function(quiz_object){
  summarized_data <- quiz_object %>%  
    dplyr::group_by(`email address`, `quiz`) %>% 
    dplyr::summarise(`total score` = sum(as.numeric(score)/n()*10, na.rm = T)) 
  
  summarized_data %>% 
    ggplot(aes(x = `total score`*10)) +
    geom_histogram(binwidth = 10, alpha = .7, 
                   color = "dodgerblue4", fill = "dodgerblue4") +
    labs(x = "Total score (max score = 100)", 
         y = "Count") +
    facet_wrap(~quiz, ncol = 2)
}
plot_boxplots <- function(quiz_object){
  summarized_data <- quiz_object %>%  
    dplyr::group_by(`email address`, `quiz`) %>% 
    dplyr::summarise(`total score` = sum(as.numeric(score)/n()*10, na.rm = T)) 
  
  summarized_data %>% 
    ggplot(aes(x = factor(`quiz`), 
               y = `total score`*10)) +
    geom_boxplot() +
    labs(x = "Quiz name", 
         y = "Total score (max score = 100)")
}
plot_order <- function(quiz_object){
  quiz_object %>% 
    dplyr::group_by(question, quiz) %>% 
    dplyr::mutate(`Tercil per time` = cut(as.numeric(`time per question`), 
                                          breaks = quantile(as.numeric(.$`time per question`), 
                                                            probs = c(0, .33, .66, 1), 
                                                            na.rm = T), 
                                          include.lowest = T, 
                                          labels = c("First Tercil (Fastest)", "Second Tercil", "Third Tercil (Slowest)")),
                  temp = sum(!is.na(`Tercil per time`)), 
                  `Tercil per time` = if_else(temp > 10, as.character(`Tercil per time`), "NA")) %>% 
    dplyr::filter(!is.na(`Tercil per time`), `Tercil per time`!= "NA") %>% 
    dplyr::group_by(question, `Tercil per time`, quiz) %>% 
    dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
    dplyr::filter(`mean score`>.05) %>% 
    ggplot2::ggplot(aes(x = factor(question), 
                        y = `mean score` * 100, 
                        group = `Tercil per time`, 
                        color = `Tercil per time`,
                        label = round(`mean score` * 100, 0))) +
    ggplot2::facet_wrap(~`Tercil per time`, ncol = 1) +
    geom_line() +
    geom_point() +
    labs(x = "Question in the quiz",
         y = "Average score per tercil (max score = 100)") +
    ylim(0, 100) +
    geom_label(alpha = .7) +
    facet_wrap(~quiz, ncol = 1)
} 
plot_easiness_time <- function(quiz_object){
  quiz_object %>%
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>% 
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    ggplot2::ggplot(aes(x = as.numeric(`mean time`), 
                        y = `mean score`* 100, 
                        label = paste0("q", `question`))) +
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm") +
    ggrepel::geom_label_repel() +
    labs(y = "Average score per item (max score = 100)", 
         x = "Average time taken to answer each item (in seconds)") +
    ylim(0,100) +
    facet_wrap(~quiz, ncol = 1)
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
                        y = factor(question), 
                        color = factor(guessing))) +
    ggplot2::geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust = 1)) +
    labs(x = "Email address", y = "Question number (Item)", color = "Guessing score")+
    facet_wrap(~quiz, ncol = 1)
}
plot_etl <- function(quiz_object, challengeLevel, item = "MCM.2014.item", rating = "Rating.HB"){
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
    dplyr::select(get(item), get(rating)) %>% 
    setNames(c("question_id", "rating"))
  
  rating_df <- data.frame(rating = factor(c(1,2,3)), 
                          rating2 = factor(c("Low cognitive level", 
                                             "Medium cognitive level", 
                                             "High cognitive level")))
  
  homo_quiz_object %>% 
    dplyr::left_join(homo_challenge_level) %>% 
    dplyr::mutate(`mean time` = as.numeric(`mean time`),
                  rating = factor(rating)) %>% 
    left_join(rating_df) %>% 
    dplyr::select(-rating) %>% 
    ggplot2::ggplot(aes(x = `mean time`, 
                        y = `mean score` *100, 
                        color = rating2, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel() +
    labs(y = "Average score per item (max score = 100)",
         x = "Average time taken to answer each item (in seconds)",
         color = "Cognitive level") +
    facet_wrap(~quiz, ncol = 1)
}
plot_group <- function(quiz_object, email_filtered){
  quiz_object %>% 
    group_by(`email address`, quiz) %>% 
    dplyr::summarise(`mean score` = round(100*mean(as.numeric(score), na.rm = T), 0)) %>% 
    filter(`email address` %in% email_filtered) %>% 
    ggplot(aes(x = quiz, y = `mean score`, group = `email address`)) +
    geom_line() + geom_point() +
    facet_wrap(~`email address`, ncol = 2)
}

# u0 interface --------------------------------------------------------
header <- dashboardHeader(title = span("Learning Analytics Dashboard", 
                                       img(src="logo.png", width = 60)), 
                          titleWidth=360)
sidebar <- dashboardSidebar(sidebarMenu(
  br(),
  menuItem("Instructions", tabName = "1_instructions", icon = icon("info")),
  menuItem("Import quizzes", tabName = "2_input", icon = icon("th")),
  menuItem("Display quizzes", tabName = "3_display", icon = icon("eye")),
  menuItem("High discrimination quizz", tabName = "4_irt", icon = icon("book"),
           menuSubItem("1-PL IRT", tabName = "4_1_irt", icon = icon("dashboard")),
           menuSubItem("2-PL IRT", tabName = "4_2_irt", icon = icon("dashboard")),
           menuSubItem("3-PL IRT", tabName = "4_3_irt", icon = icon("dashboard"))
           ),
  menuItem("Low discrimination quizz", tabName = "5_irt", icon = icon("book"),
           menuSubItem("Item Characteristic Curves", tabName = "5_1_irt", icon = icon("desktop")),
           menuSubItem("Person-Item Map", tabName = "5_2_irt", icon = icon("desktop")),
           menuSubItem("Person parameters", tabName = "5_3_irt", icon = icon("desktop"))
  ),
  menuItem("Data analysis", tabName = "6_analysis", icon = icon("line-chart"),
           menuSubItem("Individual analysis", tabName = "6_1_individual", icon = icon("dashboard")),
           menuSubItem("Quiz analysis", tabName = "6_2_quiz", icon = icon("dashboard")),
           menuSubItem("Grupal analysis", tabName = "6_3_grupal", icon = icon("dashboard"))
           ),
  br(),
  actionButton("quit_button", "Exit", icon("sign-out")),
  helpText("\t Press 'Exit' to quit app")
), width = 200)

body <- dashboardBody(
  tabItems(
# u1 instructions --------------------------------------------------------
    tabItem(tabName = "1_instructions", 
            fluidPage(
              titlePanel(strong("Welcome to the lanalytics dashboard!")),
              br(),
              br(),
              sidebarLayout(
                sidebarPanel(
                  h2("Introduction"),
                  p("This dashboard is an interface of the ", 
                    strong("eRm"), ", ", strong("ltm"), " and the ", 
                    strong("lanalytics"), " packages, which provide useful functions to 
                    analyze online quizzes. The first two are available in the CRAN, 
                    while the last is available in GitHub."),
                  br(),
                  p("The ", strong("ltm"), " and the ", strong("eRm"),
                    " packages are two important packages in the psychometric section of the 
                    CRAN and these can be installed with the followind command: ",
                  code("install_packages('ltm')"),
                  code("install_packages('eRm')"),
                  "while the lanalytics package contain some useful statistical analysis 
                   to be used for online quizzes. This package can be installed with the following command: ",
                  code("install_github('savrgg/lanalytics')"),
                  br(),
                  p("For an introduction and examples of the lanalytics package, please visit the ", 
                    a("lanalytics homepage.", 
                      href = "https://savrgg.github.io/lanalytics/")))
                ),
                mainPanel(
                  h1("Instructions"),
                  p("The Learning Analytics dashboard contains five tabs, three of them are used for different kind of analysis
                      while the other two are used for import data and display the datasets."), 
                  br(),
                  p("For an introduction and live examples, visit the ",
                    a("Shiny homepage.", 
                      href = "http://www.rstudio.com/shiny")),
                  br(),
                  h2("Features"),
                  p("Feature 1"),
                  p("Feature 2")
                  )
                )
              )
            ),
# u2 input --------------------------------------------------------
    tabItem(tabName = "2_input",
            fluidRow(
              tabBox(
                title = "Import quiz file", width = 6, 
                id = "2_tabset_1", height = "350px",
                tabPanel("Learning Catalytics",
                         p("Select a *.csv file exported from the Learning Catalytics software. This file must contain
                          two columns per item, the first indicating the ", em("score"), " and the second indicating the ", 
                           em("answering time") ,". In addition, the file should contain an ID column called ", em("email address")),
                  fileInput('file1', 'Select file:',
                            accept = c('.csv'),
                            multiple = TRUE
                            ),
                  column(3, 
                         actionButton(inputId = "upload_quiz_dataset", 
                                      label = "Add quiz dataset")
                         ),
                  column(3, offset = 3,
                         actionButton(inputId = "remove_quiz_dataset", 
                                      label = "Remove quiz datasets")
                         )
                ),
                tabPanel("Google Forms", "Tab content 2"),
                tabPanel("R file", "Tab content 2")
              ),
              tabBox(
                title = "Import cognitive levels file", width = 6,
                id = "2_tabset_2", height = "350px",
                tabPanel("From *.csv file", 
                         p("Select a *.csv file exported from the Learning Catalytics software. This file must contain
                           two columns, one indicating the quiz and item number in the format Q1_q3 (Quiz 1, question 3) and the
                           other indicating the cognitive level in the scale 1-3 (1=low, 3=high)."),
                         fileInput('file2', 'Select file:',
                                   accept = c('.csv')
                         ),
                         column(3, 
                                actionButton(inputId = "upload_cognitive_dataset", 
                                             label = "Upload cognitive dataset")
                         ),
                         column(3, offset = 3,
                                actionButton(inputId = "remove_cognitive_dataset", 
                                             label = "Remove congnitive datasets")
                         )       
                )
              )
            ),
            fluidRow(
              box(title = "Uploaded files:", status = "primary", width = 6,
                  collapsible = TRUE,
                  tableOutput('names_quiz')
              ),
              box(title = "Uploaded files:", status = "primary", width = 6,
                  collapsible = TRUE,
                  tableOutput('names_cognitive')
              )
            )
    ),
# u3 display --------------------------------------------------------
    tabItem(tabName = "3_display", 
            fluidRow(
              box(title = "Quiz dataset:", status = "primary", width = 12,
                  collapsible = TRUE,
                  DT::dataTableOutput('quiz_dataset')
                  )
              ), 
            fluidRow(
              box(title = "Select column of the item:", 
                  status = "primary", width = 6,
                  collapsible = TRUE,
                  uiOutput('choose_cognitive_item')
              ),
              box(title = "Select column of the rating:", 
                  status = "primary", width = 6,
                  collapsible = TRUE,
                  uiOutput('choose_cognitive_rating')
              )
            ),
            fluidRow(
              box(title = "Cognitive levels dataset:", status = "primary", width = 12,
                  collapsible = TRUE,
                  DT::dataTableOutput('cognitive_dataset')
              )
            )
           ),
# u4 IRT-discrim --------------------------------------------------------
    tabItem(tabName = "4_irt"),
    tabItem(tabName = "4_1_irt",
            fluidRow(
              br(),
              titlePanel(strong("1 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 6,
                  collapsible = TRUE,
                  uiOutput("choose_files_4_1")
              ),
              box(title = "Select condition number:", status = "info", width = 6,
                  collapsible = TRUE,
                  numericInput("cond_4_1", "Observations:", 10000, min = 1, max = 10000000000000)
              )
            ), 
            fluidRow(
              box(title = "1 PL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("rasch_model")
              ),
              box(title = "1 PL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("rasch_model2")
              )
            ),
            fluidRow(
              box(title = "Coefficients:", status = "primary", width = 12,
                  collapsible = TRUE,
                  tableOutput('rasch_model_coef')
              )
            )
            ),
    tabItem(tabName = "4_2_irt",
            fluidRow(
              br(),
              titlePanel(strong("2 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 6,
                  collapsible = TRUE,
                  uiOutput("choose_files_4_2")
              ),
              box(title = "Select condition number:", status = "info", width = 6,
                  collapsible = TRUE,
                  numericInput("cond_4_2", "Observations:", 10000, min = 1, max = 10000000000000)
              )
            ),
            fluidRow(
              box(title = "2 PL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("pars2_model")
              ),
              box(title = "2 PL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("pars2_model2")
              )
            ),
            fluidRow(
              box(title = "Coefficients:", status = "primary", width = 12,
                  collapsible = TRUE,
                  tableOutput('pars2_model_coef')
              )
            )
            ),
    tabItem(tabName = "4_3_irt", 
            fluidRow(
              br(),
              titlePanel(strong("3 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 6,
                  collapsible = TRUE,
                  uiOutput("choose_files_4_3")
              ),
              box(title = "Select condition number:", status = "info", width = 6,
                  collapsible = TRUE,
                  numericInput("cond_4_3", "Observations:", 10000, min = 1, max = 10000000000000)
              )
            ),
            fluidRow(
              box(title = "3 PL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("pars3_model")
              ),
              box(title = "3 PL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("pars3_model2")
              )
            ),
            fluidRow(
              box(title = "Coefficients:", status = "primary", width = 12,
                  collapsible = TRUE,
                  tableOutput('pars3_model_coef')
              )
            )
            ),
# u5 IRT-NO-discrim --------------------------------------------------------
    tabItem(tabName = "5_irt"),
    tabItem(tabName = "5_1_irt",
            fluidRow(
              br(),
              titlePanel(strong("1 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 12,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_1")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_jointICC")
              )
            )
            ),
    tabItem(tabName = "5_2_irt",
            fluidRow(
              br(),
              titlePanel(strong("1 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_2")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_personItem")
              )
            )
            ),
    tabItem(tabName = "5_3_irt",
            fluidRow(
              br(),
              titlePanel(strong("1 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_3")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_personParameter")
              )
            )),
# u6 analysis --------------------------------------------------------
    tabItem(tabName = "6_analysis"),
    tabItem(tabName = "6_1_individual",
            fluidRow(
              br(),
              titlePanel(strong("Individual!")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_1")
              )
            ),
            fluidRow(
              box(title = "Guessers", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_guessers")
              )
            ),
            fluidRow(
              box(title = "Order plot", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_order")
              )
            )
    ),
    tabItem(tabName = "6_2_quiz",
            fluidRow(
              br(),
              titlePanel(strong("Quiz")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_2")
                  )
            ),
            fluidRow(
              box(title = "Histogram", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_hist")
              )
            ),
            fluidRow(
              box(title = "Boxplot", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_boxplots")
              )
            ),
            fluidRow(
              box(title = "Easiness-time", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("plot_et")
              ),
              box(title = "ETL", status = "primary", width = 6,
                  collapsible = TRUE,
                  plotOutput("plot_etl")
              )
            )
    ),
    tabItem(tabName = "6_3_grupal",
            fluidRow(
              titlePanel(strong("Grupal")),
              box(title = "Select quizzes to analize:", status = "info", width = 6,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_3")
              ),
              box(title = "Select email to filter:", status = "info", width = 6,
                  collapsible = TRUE,
                  uiOutput("email_filtered")
              )
            ),
            fluidRow(
              box(title = "Group history", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_group_tot")
                  )
              )
            )
))

# u-final --------------------------------------------------------
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "black"
)


# s0 server ------------------------------------------------------------------
server <- function(input, output) {
  q <- observe({
    if (input$quit_button == 1) stopApp()
  })
# s1 instructions ------------------------------------------------------------------
# s2 input ------------------------------------------------------------------
  infile_quiz <- eventReactive(input$upload_quiz_dataset, {
    validate(need(input$file1, message = "Please add a quiz dataset"))  
    infile_quiz <- input$file1
    infile_quiz
  })
  infile_cognitive <- eventReactive(input$upload_cognitive_dataset, {
    validate(need(input$file2, message = "Please select a quizz"))
    infile_cognitive <- input$file2
    infile_cognitive
  })
  quiz_values <- reactiveValues(df_data = NULL)
  cognitive_values <- reactiveValues(df_data = NULL)
  observeEvent(input$remove_quiz_dataset, {
    quiz_values$df_data <- NULL
    })
  observeEvent(input$upload_quiz_dataset, {
    xx <- lapply(1:nrow(infile_quiz()), function(num){
      add_times(read_lc(infile_quiz()$datapath[num]) %>% 
                  mutate(quiz = infile_quiz()$name[num]))
    })
    xx_temp = bind_rows(xx)
    temp <- is.null(quiz_values$df_data)
    if(temp){
      quiz_values$df_data = xx_temp
    }else{
      quiz_values$df_data = unique(bind_rows(quiz_values$df_data, xx_temp))
    }
  })
  observeEvent(input$remove_cognitive_dataset, {
    cognitive_values$df_data <- NULL
  })
  observeEvent(input$upload_cognitive_dataset, {
    cognitive_values$df_data <- read.csv(infile_cognitive()$datapath) %>% 
      mutate(file = infile_cognitive()$name[1])
  })
  df_quiz <- reactive(quiz_values$df_data)
  df_cognitive <- reactive(cognitive_values$df_data)
  output$names_quiz <- renderTable({
    if((df_quiz()$quiz %>% length())>0){
      df_quiz()$quiz %>% unique %>% data.frame() %>% setNames("Uploaded files")}
  })
  output$names_cognitive <- renderTable({
    if((df_cognitive()$file %>% unique %>% length())>0){
      df_cognitive()$file %>% unique %>% data.frame() %>% setNames("Uploaded files")}
  })
# s3 display ------------------------------------------------------------------
  output$choose_cognitive_item <- renderUI({
    validate(need((df_cognitive() %>% data.frame() %>% names)[1], "Introduce a valid file."))
    radioButtons("choose_cognitive_item", "Choose level column", 
                 choices  = c(df_cognitive() %>% data.frame() %>% names),
                 selected = c(df_cognitive() %>% data.frame() %>% names)[1])
  })
  output$choose_cognitive_rating <- renderUI({
    validate(need((df_cognitive() %>% data.frame() %>% names)[1], "Introduce a valid file."))
    radioButtons("choose_cognitive_rating", "Choose rating column", 
                 choices  = c(df_cognitive() %>% data.frame() %>% names),
                 selected = c(df_cognitive() %>% data.frame() %>% names)[1])
  })
  output$quiz_dataset <- DT::renderDataTable({
    DT::datatable(df_quiz(),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })
  output$cognitive_dataset <- DT::renderDataTable({
    req(input$choose_cognitive_item)
    req(input$choose_cognitive_item)
    validate(need(nrow(df_cognitive())>0, message = "Please select a cognitive file"))
    DT::datatable(df_cognitive() %>% 
                    dplyr::select(get(input$choose_cognitive_item),
                                  get(input$choose_cognitive_rating)),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })  
# s4 IRT-disc ------------------------------------------------------------------
  model_rasch_val <- reactive({
    set.seed(123456)
    validate(need(input$choose_files_4_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      model <- rasch_model(df_quiz() %>% filter(quiz %in% input$choose_files_4_1))
      validate(need(kappa(model$hessian)<input$cond_4_1, message = paste("The condition number is larger: ", kappa(model$hessian))))
      model}
  })
  output$rasch_model <- renderPlot({
    
      input$newplot
      plot(model_rasch_val(), type = c("ICC"))
  })
  output$rasch_model2 <- renderPlot({
      input$newplot
      plot(model_rasch_val(), type = c("IIC"))
  })
  output$rasch_model_coef <- renderTable({
      coef(summary(model_rasch_val())) %>% 
        data.frame() %>% 
        mutate(beta = row.names(.)) %>% 
        as.tibble() %>% 
        dplyr::rename(item = beta, `Easiness parameter` = value) %>% 
        dplyr::select(item, `Easiness parameter`, std.err, z.vals) %>% 
        na.omit()
  })
  model_pars2_val <- reactive({
    set.seed(123456)
    validate(need(input$choose_files_4_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      model <- pars2_model(df_quiz() %>% filter(quiz %in% input$choose_files_4_2))
      validate(need(kappa(model$hessian)<input$cond_4_2, message = paste("The condition number is larger: ", kappa(model$hessian))))
    }
  })
  output$pars2_model <- renderPlot({
      input$newplot
      plot(model_pars2_val(), type = c("ICC"))  
  })
  output$pars2_model2 <- renderPlot({
      input$newplot
      plot(model_pars2_val(), type = c("IIC"))
  })
  output$pars2_model_coef <- renderTable({
      coef(summary(model_pars2_val())) %>% 
        data.frame() %>% 
        mutate(beta = row.names(.)) %>% 
        as.tibble() %>% 
        dplyr::rename(item= beta, `Parameter value` = value) %>% 
        dplyr::select(item, `Parameter value`, std.err, z.vals) %>% 
        na.omit()
  })
  model_pars3_val <- reactive({
    set.seed(123456)
    validate(need(input$choose_files_4_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      model <- pars3_model(df_quiz() %>% filter(quiz %in% input$choose_files_4_3))
      validate(need(kappa(model$hessian)<input$cond_4_3, message = paste("The condition number is larger: ", kappa(model$hessian))))
    }
  })
  output$pars3_model <- renderPlot({
      input$newplot
      plot(model_pars3_val(), type = c("ICC"))
  })
  output$pars3_model2 <- renderPlot({
      input$newplot
      plot(model_pars3_val(), type = c("IIC"))
  })
  output$pars3_model_coef <- renderTable({
      coef(summary(model_pars3_val())) %>% 
        data.frame() %>% 
        mutate(beta = row.names(.)) %>% 
        as.tibble() %>% 
        dplyr::rename(item= beta, `Parameter value` = value) %>% 
        dplyr::select(item, `Parameter value`, std.err, z.vals) %>% 
        na.omit()
  })
# s5 IRT-NO-disc ------------------------------------------------------------------
  output$plot_jointICC <- renderPlot({
    validate(need(input$choose_files_5_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_jointICC(df_quiz() %>% filter(quiz %in% input$choose_files_5_1))
    }
  })
  output$plot_personItem <- renderPlot({
    validate(need(input$choose_files_5_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_personItem(df_quiz() %>% filter(quiz %in% input$choose_files_5_2))
    }
  })
  output$plot_personParameter <- renderPlot({
    validate(need(input$choose_files_5_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_personParameter(df_quiz() %>% filter(quiz %in% input$choose_files_5_3))
    }
  })
# s6 analysis ------------------------------------------------------------------
  output$plot_guessers <- renderPlot({
    validate(need(input$choose_files_6_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_guessers(df_quiz() %>% filter(quiz %in% input$choose_files_6_1))  
    }
  })
  output$plot_order <- renderPlot({
    validate(need(input$choose_files_6_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_order(df_quiz() %>% filter(quiz %in% input$choose_files_6_1))  
    }
  })
  output$plot_hist <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_hist(df_quiz() %>% filter(quiz %in% input$choose_files_6_2))  
    }
  })
  output$plot_boxplots <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_boxplots(df_quiz() %>% filter(quiz %in% input$choose_files_6_2))  
    }
  })
  output$plot_et <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_easiness_time(df_quiz() %>% filter(quiz %in% input$choose_files_6_2))  
    }
  })
  output$plot_etl <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    validate(need(nrow(df_cognitive())>0, message = "Please select a cognitive file"))
    if(exists("df_quiz")){
      input$newplot
      plot_etl(df_quiz() %>% filter(quiz %in% input$choose_files_6_2), 
               df_cognitive(), 
               item = input$choose_cognitive_item, 
               rating = input$choose_cognitive_rating)  
    }
  })
  output$plot_group_tot <- renderPlot({
    validate(need(input$choose_files_6_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_group(df_quiz() %>% filter(quiz %in% input$choose_files_6_3), input$email_filtered)
    }
  })
# s-final ------------------------------------------------------------------
  output$choose_files_4_1 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_4_1", "Choose files", 
                         choices  = c(df_quiz()$quiz %>% unique),
                         selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_4_2 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_4_2", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_4_3 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_4_3", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_5_1 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_5_1", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_5_2 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_5_2", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_5_3 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_5_3", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_6_1 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    checkboxGroupInput("choose_files_6_1", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_6_2 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    checkboxGroupInput("choose_files_6_2", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_6_3 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    checkboxGroupInput("choose_files_6_3", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique))
  })
  output$email_filtered <- renderUI({
    validate(need((df_quiz()$`email address` %>% unique)[1], "Introduce a quiz datafile"))
    selectInput("email_filtered", "Choose email", 
                       choices  = c(df_quiz()$`email address` %>% unique),
                       selected = c(df_quiz()$`email address` %>% unique)[1])
  })
} # end server


shinyApp(ui, server)