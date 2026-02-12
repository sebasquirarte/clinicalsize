library(shiny)
library(clinicalsize)

ui <- fluidPage(

  tags$head(
    tags$style(HTML("

      .title-container {
        display: flex;
        align-items: center;
        gap: 25px;
        padding: 15px 0;
      }
      .title-container img {
        height: 150px;
        width: auto;
      }
      .title-text h2 {
        margin: 0;
        font-size: 50px;
        font-weight: 400;
      }
      .title-text p {
        margin: 5px 0 0 0;
        color: #666;
        font-size: 30px;
      }

      .main-grid {
        display: grid;
        grid-template-columns: 240px 1fr 3fr;
        grid-template-rows: auto auto auto;
        row-gap: 0;
        column-gap: 12px;
        margin-top: 12px;
      }

      .sidebar-left {
        grid-row: 1 / 4;
        padding: 20px;
      }

      .panel {
        background: #F9FCFD;
        border-radius: 4px;
        padding: 20px;
      }

      .top-middle {
      }

      .top-right {
        display: flex;
        flex-direction: column;
      }
      .top-right .shiny-plot-output {
        flex: 1;
        min-height: 0;
      }

      .middle-wide {
        grid-column: 2 / 4;
      }

      .bottom-wide {
        grid-column: 2 / 4;
      }

      .calc-button {
        background: white;
        color: #2C5F6F;
        border: 3px solid white;
        padding: 15px 30px;
        border-radius: 6px;
        margin: 25px auto 0 auto;
        display: block;
        width: 150px;
        font-size: 18px;
        font-weight: 700;
        cursor: pointer;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        transition: all 0.3s ease;
      }
      .calc-button:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
      }
      .calc-button:active {
        transform: translateY(0);
      }

    "))
  ),

  div(
    class = "title-container",
    img(src = "logo.png", alt = "clinicalsize logo"),
    div(
      class = "title-text",
      tags$h2("clinicalsize"),
      tags$p("Sample Size Calculator for Clinical Trials")
    )
  ),

  div(
    class = "main-grid",

    div(
      class = "sidebar-left panel",
      selectInput("sample", "Sample:",
                  choices = c("one-sample", "two-sample"),
                  selected = "two-sample"),

      conditionalPanel(
        condition = "input.sample == 'two-sample'",
        selectInput("design", "Design:",
                    choices = c("parallel", "crossover"),
                    selected = "parallel")
      ),

      selectInput("outcome", "Outcome:",
                  choices = c("mean", "proportion"),
                  selected = "mean"),

      selectInput("type", "Test Type:",
                  choices = c("equality", "equivalence",
                              "non-inferiority", "superiority"),
                  selected = "non-inferiority"),

      numericInput("alpha", "Alpha (Type I error):",
                   value = 0.05, min = 0.001, max = 0.2, step = 0.001),

      numericInput("beta", "Beta (Type II error):",
                   value = 0.20, min = 0.01, max = 0.5, step = 0.01),

      numericInput("x1", "x1 (Treatment):", value = 5),
      numericInput("x2", "x2 (Control/Reference):", value = 5),

      conditionalPanel(
        condition = "input.outcome == 'mean' || (input.sample == 'two-sample' && input.design == 'crossover')",
        numericInput("SD", "Standard Deviation (SD):", value = 0.1)
      ),

      conditionalPanel(
        condition = "input.type != 'equality'",
        numericInput("delta", "Delta (Margin):", value = -0.05)
      ),

      conditionalPanel(
        condition = "input.sample == 'two-sample' && input.design == 'parallel'",
        numericInput("k", "Allocation Ratio (k = n1/n2):",
                     value = 1, min = 0.1, step = 0.1)
      ),

      numericInput("dropout", "Dropout Rate (0–1):",
                   value = 0.1, min = 0, max = 0.9, step = 0.01),

      tags$hr(),
      tags$strong("Range Plot Settings"),
      numericInput("x1_min", "x1 Range Min:", value = 4.9),
      numericInput("x1_max", "x1 Range Max:", value = 5.1),
      numericInput("step", "Step:", value = 0.01, min = 0.001, step = 0.001),

      actionButton("calc", "Calculate", class = "calc-button")
    ),

    div(class = "panel top-middle",
        uiOutput("results")
    ),
    div(class = "panel top-right",
        plotOutput("range_plot", height = "100%")
    ),
    div(class = "panel middle-wide"),
    div(class = "panel bottom-wide")
  )
)

server <- function(input, output, session) {

  calculated <- reactiveVal(FALSE)

  observeEvent(input$calc, {
    calculated(TRUE)
  })

  results <- reactive({

    if (!calculated()) return(NULL)

    tryCatch({
      sample_size(
        sample = input$sample,
        design = if (input$sample == "two-sample") input$design else NULL,
        outcome = input$outcome,
        type = input$type,
        alpha = input$alpha,
        beta = input$beta,
        x1 = input$x1,
        x2 = input$x2,
        SD = input$SD,
        delta = input$delta,
        dropout = input$dropout,
        k = input$k
      )
    }, error = function(e) {
      paste("Error:", e$message)
    })
  })

  output$range_plot <- renderPlot({
    if (!calculated()) return(NULL)

    tryCatch({
      result <- sample_size_range(
        x1_range = c(input$x1_min, input$x1_max),
        x2 = input$x2,
        step = input$step,
        sample = input$sample,
        design = if (input$sample == "two-sample") input$design else NULL,
        outcome = input$outcome,
        type = input$type,
        alpha = input$alpha,
        SD = input$SD,
        delta = input$delta,
        dropout = input$dropout,
        k = input$k
      )
      result$plot
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 1.2)
    })
  })

  output$results <- renderUI({

    res <- results()

    if (is.null(res)) {
      return(
        div(
          tags$h3("Sample Size"),
          tags$hr(),
          tags$p(style="color:#999;",
                 "Results will appear here after calculation.")
        )
      )
    }

    if (is.character(res)) {
      return(div(style="color:red;", res))
    }

    div(
      tags$h3("Sample Size"),
      tags$hr(),
      tags$pre(
        style = "
          font-size: 14px;
          background: none;
          border: none;
          padding: 0;
          white-space: pre-wrap;
        ",
        paste(capture.output(print(res)), collapse = "\n")
      )
    )
  })
}

shinyApp(ui, server)
