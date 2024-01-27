library(shiny)
library(tidyverse)

freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )

  ggplot(df, aes(x, colour = g)) +
    geom_freqpoly(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
}

ui <- fluidPage(
  title = "Chatbot with R and OpenAI",
  theme = bslib::bs_theme(bootswatch = "united"),
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
)

# Server logic.
# Before running set App to Run in View Pane, and In Background Job
server <- function(input, output) {
  # timer <- reactiveTimer(500)

  x1 <- eventReactive(input$simulate, {
    # timer()
    rpois(input$n, input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    # timer()
    rpois(input$n, input$lambda2)
  })

  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

# Run the application
shinyApp(ui = ui, server = server)
