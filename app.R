#
# This is a Shiny web app. Run by clicking the 'Run App' button. This app is
# published on shinyapps.io at https://mpfoley73.shinyapps.io/R-Chatbot/.
#
# This app was built with help from James Wade's three part series:
# https://www.youtube.com/watch?v=d7l4EZYlZE0
# https://www.youtube.com/watch?v=agPFcEeQSEQ
# https://www.youtube.com/watch?v=odBkUdWlwUk
#
# Shiny cheatsheet: https://www.rstudio.com/resources/cheatsheets/
# Shiny book: https://mastering-shiny.org/
#

library(shiny)
library(bslib) # make app look professional.
library(httr2)
library(jsonlite)
library(purrr)
library(glue)
library(tidyverse)

source("chatbot.R")

# Shiny apps usually have a file named app.R with three components:
# 1. ui: how the app looks, usually a call to fluidPage(), but in
#   this case, bslib::page_sidebar()
# 2. server(): a function with two parameters: input and output. Defines how the
#   app works.
# 3. A call to shinyApp(ui, server) to construct the app.

# Define the app layout. The sidebar has 3 input controls. The main panel has an
# additional input control and an output control. In reactive programming,
# output controls automatically react when input controls change.
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "chat_style.css")
  ),
  titlePanel("Chatbot with R and OpenAI"),
  theme = bs_theme(bootswatch = "united"),
  tabsetPanel(
    tabPanel(
      "Settings",
      p(""),
      textInput("api_key", label = "API Key"),
      selectInput("model", label = "Model", choices = c("gpt-3.5-turbo", "gpt-4")),
      selectInput("task", label = "Task", choices = c("general", "code"))
    ),
    tabPanel(
      "Chat!",
      p(""),
      wellPanel(
        style = "height: 400px; width: 600px; overflow-y: auto;",
        uiOutput("chat_history")
      ),
      p(""),
      textAreaInput("user_content", label = NULL, width = "600",
                     placeholder = "Enter your message..."),
      actionButton("chat", label = NULL, icon = icon("paper-plane"),
                   width = "80px",
                   class = "m-2 btn-secondary")
    )
  )
)

# Server logic.
# Before running set App to Run in View Pane, and In Background Job
server <- function(input, output) {
  chat_history <- reactiveVal()
  # Wrap the server logic in observe() attached to the actionButton with
  # bindEvent().
  observe({
    # Require that the user has typed something into the textbox. If the user
    # clicks the action button with empty user_content, nothing happens.
    req(input$user_content != "")
    response <- chat(
      user_content = input$user_content,
      chat_history = chat_history(),
      chat_model = input$model,
      chat_task = input$task,
      api_key = input$api_key
    )
    # chat_history is a reactive value. To update date it, pipe the updated
    # history into it as if it were a function.
    update_history(chat_history(), input$user_content, response) |> chat_history()
    # Write to the `chat_history` uiOutput
    output$chat_history <- renderUI(map(chat_history(), \(x) markdown(glue("{x$content}"))))
  }) |> bindEvent(input$chat)
}

# Run the application
shinyApp(ui = ui, server = server)
