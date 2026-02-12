library(shiny)
library(clinicalsize)

# Define UI ----
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
        grid-template-columns: 240px 2fr 3fr;
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
        max-height: 600px;
        overflow-y: auto;
      }
      .top-right {
        max-height: 600px;
        overflow: hidden;
      }
      .middle-wide, .bottom-wide {
        grid-column: 2 / 4;
      }
      .middle-wide {
        overflow: auto;
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
      selectInput(
        "sample", "Sample:",
        choices = c("one-sample", "two-sample"),
        selected = "two-sample"
      ),
      conditionalPanel(
        condition = "input.sample == 'two-sample'",
        selectInput(
          "design", "Design:",
          choices = c("parallel", "crossover"),
          selected = "parallel"
        )
      ),
      selectInput(
        "outcome", "Outcome:",
        choices = c("mean", "proportion"),
        selected = "proportion"
      ),
      selectInput(
        "type", "Test Type:",
        choices = c(
          "equality", "equivalence",
          "non-inferiority", "superiority"
        ),
        selected = "non-inferiority"
      ),
      numericInput(
        "alpha", "Alpha (Type I error):",
        value = 0.05, min = 0.001, max = 0.2, step = 0.001
      ),
      numericInput(
        "beta", "Beta (Type II error):",
        value = 0.20, min = 0.01, max = 0.5, step = 0.01
      ),
      numericInput("x1", "x1 (Treatment):", value = 0.7),
      numericInput("x2", "x2 (Control/Reference):", value = 0.65),
      conditionalPanel(
        condition = paste0(
          "input.outcome == 'mean' || ",
          "(input.sample == 'two-sample' && input.design == 'crossover')"
        ),
        numericInput("SD", "Standard Deviation (SD):", value = 0.1)
      ),
      conditionalPanel(
        condition = "input.type != 'equality'",
        numericInput("delta", "Delta (Margin):", value = -0.1)
      ),
      conditionalPanel(
        condition = "input.sample == 'two-sample' && input.design == 'parallel'",
        numericInput(
          "k", "Allocation Ratio (k):",
          value = 1, min = 0.1, step = 0.1
        )
      ),
      numericInput(
        "dropout", "Dropout Rate (0\u20131):",
        value = 0.1, min = 0, max = 0.9, step = 0.01
      ),
      numericInput("x1_min", "x1 Range Min:", value = 0.65),
      numericInput("x1_max", "x1 Range Max:", value = 0.70),
      numericInput(
        "step", "Step:",
        value = 0.01, min = 0.001, step = 0.001
      ),
      actionButton("calc", "Calculate", class = "calc-button")
    ),
    div(
      class = "panel top-middle",
      uiOutput("results")
    ),
    div(
      class = "panel top-right",
      tags$h3("Power Curves"),
      tags$hr(),
      plotOutput("range_plot", height = "400px")
    ),
    div(
      class = "panel middle-wide",
      uiOutput("range_table")
    ),
    div(class = "panel bottom-wide")
  )
)

# Define server logic ----
server <- function(input, output, session) {
  calculated <- reactiveVal(TRUE)

  observeEvent(input$calc, {
    calculated(TRUE)
  })

  common_params <- reactive({
    list(
      sample = input$sample,
      design = if (input$sample == "two-sample") input$design else NULL,
      outcome = input$outcome,
      type = input$type,
      alpha = input$alpha,
      x2 = input$x2,
      SD = input$SD,
      delta = input$delta,
      dropout = input$dropout,
      k = input$k
    )
  })

  results <- reactive({
    if (!calculated()) return(NULL)
    tryCatch(
      do.call(sample_size, c(
        common_params(),
        list(beta = input$beta, x1 = input$x1)
      )),
      error = function(e) paste("Error:", e$message)
    )
  })

  range_result <- reactive({
    if (!calculated()) return(NULL)
    tryCatch(
      do.call(sample_size_range, c(
        common_params(),
        list(
          x1_range = c(input$x1_min, input$x1_max),
          step = input$step
        )
      )),
      error = function(e) NULL
    )
  })

  output$range_plot <- renderPlot({
    res <- range_result()
    if (is.null(res)) return(NULL)
    res$plot
  })

  output$range_table <- renderUI({
    result <- range_result()
    if (is.null(result)) return(NULL)

    df <- result$data
    power_colors <- c("70" = "#C5F4C1", "80" = "#79E1BE", "90" = "#33BFBC")
    power_text <- c("70" = "#2D6A2E", "80" = "#1B5E4B", "90" = "#0E4B5A")

    header_cells <- lapply(names(df), function(col) {
      tags$th(
        style = "padding: 10px 14px; text-align: center; font-weight: 600; font-size: 11px; letter-spacing: 0.8px; color: #fff;",
        col
      )
    })

    rows <- lapply(seq_len(nrow(df)), function(i) {
      power_key <- as.character(df$power[i])
      bg <- power_colors[power_key]
      txt <- power_text[power_key]
      cells <- lapply(names(df), function(col) {
        val <- df[i, col]
        if (col == "power") {
          tags$td(
            style = "padding: 8px 14px; text-align: center;",
            tags$span(
              style = paste0(
                "display: inline-block; padding: 3px 14px; border-radius: 20px;",
                " background-color:", bg, "; color:", txt,
                "; font-weight: 700; font-size: 12px;"
              ),
              paste0(val, "%")
            )
          )
        } else {
          tags$td(
            style = "padding: 8px 14px; text-align: center; color: #334155;",
            format(val)
          )
        }
      })
      tags$tr(
        style = paste0(
          "background-color:", bg, "33;",
          "border-bottom: 1px solid #e2e8f0;",
          "transition: background-color 0.15s ease;"
        ),
        cells
      )
    })

    tags$div(
      style = "max-height: 400px; overflow-y: auto; box-shadow: 0 1px 3px rgba(0,0,0,0.1), 0 1px 2px rgba(0,0,0,0.06);",
      tags$table(
        style = "width: 100%; border-collapse: collapse; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; font-size: 13px;",
        tags$thead(
          style = "position: sticky; top: 0; background: #4599AC;",
          tags$tr(header_cells)
        ),
        tags$tbody(rows)
      )
    )
  })

  output$results <- renderUI({
    res <- results()
    if (is.null(res)) {
      return(div(
        tags$h3("Sample Size"),
        tags$hr(),
        tags$p(
          style = "color:#999;",
          "Results will appear here after calculation."
        )
      ))
    }
    if (is.character(res)) {
      return(div(style = "color:red;", res))
    }
    div(
      tags$h3("Sample Size"),
      tags$hr(),
      tags$pre(
        style = "font-size: 14px; background: none; border: none; padding: 0; white-space: pre-wrap;",
        paste(capture.output(print(res)), collapse = "\n")
      )
    )
  })
}

# Run the app ----
shinyApp(ui, server)
