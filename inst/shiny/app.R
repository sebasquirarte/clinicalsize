library(shiny)
library(clinicalsize)

# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body { height: 100%; margin: 0; }
      .title-container {
        display: flex;
        align-items: center;
        gap: 16px;
        padding: 8px 0 6px 0;
        border-bottom: 1px solid #e2e8f0;
        margin-bottom: 8px;
      }
      .title-container img {
        height: 70px;
        width: auto;
      }
      .title-text h2 {
        margin: 0;
        font-size: 24px;
        font-weight: 500;
      }
      .title-text p {
        margin: 2px 0 0 0;
        color: #666;
        font-size: 13px;
      }
      .main-grid {
        display: grid;
        grid-template-columns: 200px 1fr 2fr;
        grid-template-rows: 1fr auto;
        column-gap: 10px;
        row-gap: 10px;
        height: calc(100vh - 110px);
      }
      .sidebar-left {
        grid-row: 1 / 3;
        overflow-y: auto;
        padding: 12px;
        background: #f8fafc;
        border-radius: 4px;
        border: 1px solid #e2e8f0;
      }
      .sidebar-left .form-group { margin-bottom: 8px; }
      .sidebar-left label { font-size: 12px; font-weight: 600; margin-bottom: 2px; }
      .sidebar-left .form-control { height: 28px; font-size: 12px; padding: 2px 6px; }
      .sidebar-left select.form-control { height: 28px; }
      .panel {
        border-radius: 4px;
        padding: 12px;
        border: 1px solid #e2e8f0;
        overflow: auto;
      }
      .top-middle { grid-row: 1; grid-column: 2; }
      .top-right  { grid-row: 1; grid-column: 3; }
      .bottom-wide {
        grid-row: 2;
        grid-column: 2 / 4;
        max-height: 260px;
        overflow: auto;
      }
      .calc-button {
        background: #2C5F6F;
        color: white;
        border: none;
        padding: 8px 20px;
        border-radius: 6px;
        margin: 10px auto 0 auto;
        display: block;
        width: 100%;
        font-size: 13px;
        font-weight: 700;
        cursor: pointer;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        transition: all 0.2s ease;
      }
      .calc-button:hover { background: #1e4455; }
    "))
  ),
  div(
    class = "title-container",
    img(src = "logo.png", alt = "clinicalsize logo"),
    div(
      class = "title-text",
      tags$h2("clinicalsize: Sample Size Calculator for Clinical Trials"),
      tags$p("Developed by the biostatistics team at Laboratorios Sophia S.A. de C.V.")
    )
  ),
  div(
    class = "main-grid",
    div(
      class = "sidebar-left",
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
        choices = c("equivalence", "non-inferiority", "superiority"),
        selected = "non-inferiority"
      ),
      numericInput(
        "alpha", "Alpha (\u03b1):",
        value = 0.05, min = 0.001, max = 0.2, step = 0.001
      ),
      numericInput(
        "beta", "Beta (\u03b2):",
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
      numericInput("delta", "Delta (Margin):", value = -0.1),
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
      tags$hr(style = "margin: 8px 0;"),
      tags$small(tags$b("x1 Range (for plot/table)")),
      numericInput("x1_min", "Min:", value = 0.65),
      numericInput("x1_max", "Max:", value = 0.70),
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
      plotOutput("range_plot", height = "100%")
    ),
    div(
      class = "panel bottom-wide",
      tags$h5(style = "margin: 0 0 6px 0;", "Sample Size Range"),
      tags$hr(style = "margin: 0 0 8px 0;"),
      uiOutput("range_table")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
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
    tryCatch(
      do.call(sample_size, c(
        common_params(),
        list(beta = input$beta, x1 = input$x1)
      )),
      error = function(e) paste("Error:", e$message)
    )
  })

  range_result <- reactive({
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
        style = "padding: 6px 12px;
                 text-align: center;
                 font-weight: 600;
                 font-size: 11px;
                 letter-spacing: 0.8px;
                 color: #fff;",
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
            style = "padding: 5px 12px; text-align: center;",
            tags$span(
              style = paste0(
                "display: inline-block; padding: 2px 10px; border-radius: 20px;",
                " background-color:", bg, "; color:", txt,
                "; font-weight: 700; font-size: 11px;"
              ),
              paste0(val, "%")
            )
          )
        } else {
          tags$td(
            style = "padding: 5px 12px; text-align: center; color: #334155; font-size: 12px;",
            format(val)
          )
        }
      })
      tags$tr(
        style = paste0(
          "background-color:", bg, "33;",
          "border-bottom: 1px solid #e2e8f0;"
        ),
        cells
      )
    })

    tags$div(
      style = "overflow-y: auto;
               box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
      tags$table(
        style = "width: 100%;
                 border-collapse: collapse;
                 font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
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
        tags$hr()
      ))
    }
    if (is.character(res)) {
      return(div(style = "color:red;", res))
    }
    div(
      tags$h4(style = "margin: 0 0 6px 0;", "Sample Size"),
      tags$hr(style = "margin: 0 0 8px 0;"),
      tags$pre(
        style = "font-size: 13px;
                 background: none;
                 border: none;
                 padding: 0;
                 white-space: pre-wrap;",
        paste(capture.output(print(res)), collapse = "\n")
      )
    )
  })
}

# Run the app ----
shinyApp(ui, server)
