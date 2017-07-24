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
plot_rasch <- function(quiz_object, type = c("ICC", "IIC")){
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
  betas <- model$coefficients
  
  plot_vals <- if (type == "ICC") {
    y_lab <- "Probability of correctness"
    plogis(d_matrix %*% t(betas))
  }else {
    y_lab <- "Information"
    temp <- plogis(d_matrix %*% t(betas))
    betas[1, 2]^2 * temp * (1 - temp)
  }
  
  plot_vals %>%
    as_tibble() %>% 
    mutate(x_vals = x_vals) %>% 
    gather(item, value, -x_vals) %>% 
    ggplot(aes(x = x_vals, 
               y = value, 
               group = item, 
               color = item)) +
    geom_line() +
    labs(x = "Ability", y = y_lab) 
}  
plot_2pars <- function(quiz_object, type = c("ICC", "IIC")){
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
  betas <- model$coefficients
  
  plot_vals <- if (type == "ICC") {
    y_lab <- "Probability of correctness"
    plogis(d_matrix %*% t(betas))
  }
  else {
    y_lab <- "Information"
    temp <- plogis(d_matrix %*% t(betas))
    temp2 <- temp * (1 - temp)
    t(t(temp2) * betas[, 2]^2)
  }
  
  plot_vals %>%
    as_tibble() %>% 
    mutate(x_vals = x_vals) %>% 
    gather(item, value, -x_vals) %>% 
    ggplot(aes(x = x_vals, y = value, group = item, color = item)) +
    geom_line() +
    labs(x = "Ability", y = y_lab) 
}  
plot_3pars <- function(quiz_object, type = c("ICC", "IIC")){
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
  thetas <- model$coefficients
  temp <- plogis(thetas[, 1]) * model$max.guessing
  betas <- thetas[, 2:3]
  p <- nrow(betas)
  
  
  plot_vals <- if (type == "ICC") {
    temp <- matrix(temp, length(x_vals), p, TRUE)
    y_lab <- "Probability of correctness"
    temp + (1 - temp) * ltm:::probs(d_matrix %*% t(betas)) 
    
  } else {
    y_lab <- "Information"
    pi_2 <- plogis(d_matrix %*% t(betas))
    temp <- matrix(temp, length(x_vals), p, TRUE)
    pi <- temp + (1 - temp) * pi_2
    pqr <- pi * (1 - pi) * (pi_2/pi)^2
    t(t(pqr) * betas[, 2]^2)
  }
  
  plot_vals %>% 
    as_tibble() %>% 
    mutate(x_vals = x_vals) %>% 
    gather(item, value, -x_vals) %>% 
    ggplot(aes(x = x_vals, y = value, group = item, color = item)) +
    geom_line() +
    labs(x = "Ability", y = y_lab) 
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
    geom_smooth() +
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

# u0 interface --------------------------------------------------------
header <- dashboardHeader(title = "lanalytics Dashboard")
sidebar <- dashboardSidebar(sidebarMenu(
  br(),
  menuItem("Instructions", tabName = "1_instructions", icon = icon("info")),
  menuItem("Import quizzes", tabName = "2_input", icon = icon("th")),
  menuItem("Display quizzes", tabName = "3_display", icon = icon("eye")),
  menuItem("High discrimination quizz", tabName = "4_irt", icon = icon("eye"),
           menuSubItem("1-PL IRT", tabName = "4_1_irt", icon = icon("dashboard")),
           menuSubItem("2-PL IRT", tabName = "4_2_irt", icon = icon("dashboard")),
           menuSubItem("3-PL IRT", tabName = "4_3_irt", icon = icon("dashboard"))
           ),
  menuItem("Low discrimination quizz", tabName = "5_irt", icon = icon("eye"),
           menuSubItem("Item Characteristic Curves", tabName = "5_1_irt", icon = icon("dashboard")),
           menuSubItem("Person-Item Map", tabName = "5_2_irt", icon = icon("dashboard")),
           menuSubItem("Person parameters", tabName = "5_3_irt", icon = icon("dashboard"))
  ),
  menuItem("Data analysis", tabName = "6_analysis", icon = icon("dashboard"),
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
                  p("The lanalytics dashboard is an interface of the", strong("lanalytics"), "package, 
                    which provides useful functions to analyze online quizzes."),
                  br(),
                  br(),
                  p("If you want to use the functions outside this dashboard, you can install the package
                    with devtools:"),
                  code("install_github('savrgg/lanalytics')"),
                  br(),
                  br(),
                  p("For an introduction and examples visit the ", 
                    a("lanalytics homepage.", 
                      href = "https://savrgg.github.io/lanalytics/"))
                ),
                mainPanel(
                  h1("Instructions"),
                  p("1) Load quiz dataset from a *.csv file (hint: you can upload several *.csv files at the same time)"), 
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
              box(title = "Import quiz file", width = 6,
                  status = "primary", 
                  collapsible = TRUE,
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
              box(title = "Import cognitive levels file", status = "primary", width = 6,
                  collapsible = TRUE,
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
            ),
            fluidRow(
              box(title = "Uploaded files:", status = "primary", width = 6,
                  collapsible = TRUE,
                  DT::dataTableOutput('names_quiz')
              ),
              box(title = "Uploaded files:", status = "primary", width = 6,
                  collapsible = TRUE,
                  DT::dataTableOutput('names_cognitive')
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
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_4_1")
              )
            ), 
            fluidRow(
              box(title = "1 PL", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_rasch")
              )
            ),
            fluidRow(
              box(title = "1 PL", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_rasch2")
              )
            )
            ),
    tabItem(tabName = "4_2_irt",
            fluidRow(
              br(),
              titlePanel(strong("2 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_4_2")
              )
            ),
            fluidRow(
              box(title = "2 PL", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_2pars")
              )
            ),
            fluidRow(
              box(title = "2 PL", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_2pars2")
              )
            )
            ),
    tabItem(tabName = "4_3_irt", 
            fluidRow(
              br(),
              titlePanel(strong("3 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_4_3")
              )
            ),
            fluidRow(
              box(title = "3 PL", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_3pars")
              )
            ),
            fluidRow(
              box(title = "3 PL", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_3pars2")
              )
            )
            ),
# u5 IRT-NO-discrim --------------------------------------------------------
    tabItem(tabName = "5_irt"),
    tabItem(tabName = "5_1_irt",
            fluidRow(
              br(),
              titlePanel(strong("1 PL")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
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
              box(title = "Select quizzes to analize:", status = "info", width = 12,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_3")
              )
            )
    )
  )
)

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
  output$names_quiz <- DT::renderDataTable({
    DT::datatable(df_quiz()$quiz %>% unique %>% data.frame(),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })
  output$names_cognitive <- DT::renderDataTable({
    DT::datatable(df_cognitive()$file %>% unique %>% data.frame(),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })
# s3 display ------------------------------------------------------------------
  output$quiz_dataset <- DT::renderDataTable({
      DT::datatable(df_quiz(),
                    extensions = 'Responsive',
                    options = list(
                      deferRender = TRUE,
                      scrollY = 200,
                      scroller = TRUE
                      ))
    })
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
  output$cognitive_dataset <- DT::renderDataTable({
    req(input$choose_cognitive_item)
    req(input$choose_cognitive_item)
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
  output$plot_rasch <- renderPlot({
    validate(need(input$choose_files_4_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_rasch(df_quiz() %>% filter(quiz %in% input$choose_files_4_1), type = c("ICC"))  
    }
  })
  output$plot_rasch2 <- renderPlot({
    validate(need(input$choose_files_4_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_rasch(df_quiz() %>% filter(quiz %in% input$choose_files_4_1), type = c("IIC"))  
    }
  })
  output$plot_2pars <- renderPlot({
    validate(need(input$choose_files_4_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_2pars(df_quiz() %>% filter(quiz %in% input$choose_files_4_2), type = c("ICC"))  
    }
  })
  output$plot_2pars2 <- renderPlot({
    validate(need(input$choose_files_4_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_2pars(df_quiz() %>% filter(quiz %in% input$choose_files_4_2), type = c("IIC"))  
    }
  })
  output$plot_3pars <- renderPlot({
    validate(need(input$choose_files_4_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_3pars(df_quiz() %>% filter(quiz %in% input$choose_files_4_3), type = c("ICC"))
    }
  })
  output$plot_3pars2 <- renderPlot({
    validate(need(input$choose_files_4_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_3pars(df_quiz() %>% filter(quiz %in% input$choose_files_4_3), type = c("IIC"))
    }
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
    if(exists("df_quiz")){
      input$newplot
      plot_etl(df_quiz() %>% filter(quiz %in% input$choose_files_6_2), 
               df_cognitive(), 
               item = input$choose_cognitive_item, 
               rating = input$choose_cognitive_rating)  
    }
  })
# s-final ------------------------------------------------------------------
  output$choose_files_4_1 <- renderUI({
    radioButtons("choose_files_4_1", "Choose files", 
                         choices  = c(infile_quiz()$name),
                         selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_4_2 <- renderUI({
    radioButtons("choose_files_4_2", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_4_3 <- renderUI({
    radioButtons("choose_files_4_3", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_5_1 <- renderUI({
    radioButtons("choose_files_5_1", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_5_2 <- renderUI({
    radioButtons("choose_files_5_2", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_5_3 <- renderUI({
    radioButtons("choose_files_5_3", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })  
  output$choose_files_6_1 <- renderUI({
    checkboxGroupInput("choose_files_6_1", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_6_2 <- renderUI({
    checkboxGroupInput("choose_files_6_2", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
  output$choose_files_6_3 <- renderUI({
    checkboxGroupInput("choose_files_6_3", "Choose files", 
                       choices  = c(infile_quiz()$name),
                       selected = c(infile_quiz()$name)[1])
  })
} # end server


shinyApp(ui, server)