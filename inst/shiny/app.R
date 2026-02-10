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
        grid-template-columns: 250px 1fr 2fr;
        grid-template-rows: auto auto auto;
        gap: 12px;
        margin-top: 20px;
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
        min-height: 340px;
      }

      .top-right {
        min-height: 340px;
      }

      .middle-wide {
        grid-column: 2 / 4;
        min-height: 180px;
      }

      .bottom-wide {
        grid-column: 2 / 4;
        min-height: 180px;
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
      selectInput("sample", "Sample:", c("two-sample", "one-sample")),
      selectInput("design", "Design:", c("parallel", "crossover")),
      selectInput("outcome", "Outcome:", c("mean", "proportion")),
      selectInput("type", "Test:", c("non-inferiority", "equivalence", "superiority", "equality")),
      numericInput("x1", "x1:", 5.0),
      numericInput("x2", "x2:", 5.0),
      numericInput("SD", "SD:", 0.1),
      numericInput("delta", "delta:", -0.05),
      tags$button("Calculate", class = "calc-button")
    ),

    div(class = "panel top-middle"),
    div(class = "panel top-right"),
    div(class = "panel middle-wide"),
    div(class = "panel bottom-wide")
  )
)

server <- function(input, output) {}

shinyApp(ui, server)
