library(shiny)
library(clinicalsize)

# Define UI ----

ui <- fluidPage(
  titlePanel("Sample Size Calculator"),

  sidebarLayout(
    sidebarPanel(
      selectInput("sample", "Sample:",
                  c("two-sample", "one-sample")),
      selectInput("design", "Design:",
                  c("parallel", "crossover")),
      selectInput("outcome", "Outcome:",
                  c("mean", "proportion")),
      selectInput("type", "Test:",
                  c("non-inferiority", "equivalence", "superiority", "equality")),

      numericInput("x1", "x1:", 5.0),
      numericInput("x2", "x2:", 5.0),
      numericInput("SD", "SD:", 0.1),
      numericInput("delta", "delta:", -0.05),

      actionButton("calc", "Calculate")
    ),

    mainPanel(
      verbatimTextOutput("results")
    )
  )
)

# Define server logic ----

server <- function(input, output) {

  results <- eventReactive(input$calc, {
    tryCatch({
      sample_size(
        sample = input$sample,
        design = input$design,
        outcome = input$outcome,
        type = input$type,
        x1 = input$x1,
        x2 = input$x2,
        SD = input$SD,
        delta = input$delta
      )
    }, error = function(e) {
      paste("Error:", e$message)
    })
  })

  output$results <- renderPrint({
    results()
  })
}

# Run the app ----

shinyApp(ui, server)
