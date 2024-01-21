#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app was built with help from James Wade's three part series:
# https://www.youtube.com/watch?v=d7l4EZYlZE0
# https://www.youtube.com/watch?v=agPFcEeQSEQ
# https://www.youtube.com/watch?v=odBkUdWlwUk
#

library(shiny)
library(bslib) # make app look professional.
library(httr2)
library(jsonlite)
library(purrr)
library(glue)
library(tidyverse)

source("helper.R")

# Default is fluidPage(), but bslib::page_sidebar() has a nicer look.
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "united"),
  title = "Chatbot with R and OpenAI",
  sidebar = sidebar(
    open = "closed",
    textInput("api_key", label = "API Key"),
    selectInput("model", label = "Model", choices = c("gpt-3.5-turbo", "gpt-4")),
    selectInput("task", label = "Task", choices = c("general", "code"))
  ),
  textAreaInput("user_content", label = NULL, width = "400px"),
  actionButton("chat", label = NULL, icon = icon("paper-plane"), width = "75px", class = "m-2 btn-secondary"),
  uiOutput("chat_history")
)

# Server logic.
# Before running set App to Run in View Pane, and In Background Job
server <- function(input, output) {
  rv <- reactiveValues()
  rv$chat_history <- NULL
  observe({
    req(input$user_content != "")
    response <- chat(
      user_content = input$user_content,
      chat_history = rv$chat_history,
      chat_model = input$model,
      chat_task = input$task,
      api_key = input$api_key
    )
    rv$chat_history <- update_history(rv$chat_history, input$user_content, response)
    output$chat_history <- renderUI(
      map(rv$chat_history, \(x) markdown(glue("{x$role}: {x$content}")))
    )
  }) |> bindEvent(input$chat)
}

# Run the application
shinyApp(ui = ui, server = server)
