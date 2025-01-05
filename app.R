library(shiny)
source("R/fetch_data.R")
source("R/calculations.R")
source("R/plotting.R")

# Define UI
ui <- fluidPage(
  titlePanel("Portfolio Optimizer"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbols", "Enter Tickers (comma-separated):", value = "BTC-USD, ETH-USD"),
      numericInput("growth", "Target Growth (%):", value = 18, min = 0, max = 100),
      numericInput("risk", "Max Risk (%):", value = 10, min = 0, max = 100),
      actionButton("fetch", "Fetch Data"),
      actionButton("optimize", "Optimize Portfolio")
    ),
    mainPanel(
      tableOutput("data_preview"),
      textOutput("weights"),
      plotOutput("price_plot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  fetched_data <- reactiveVal(list())
  
  # Fetch data when the "Fetch Data" button is clicked
  observeEvent(input$fetch, {
    data_list <- fetch_data(input$symbols)
    fetched_data(data_list)
    
    # Display preview of first asset data
    output$data_preview <- renderTable({
      if (length(data_list) > 0) {
        head(Cl(data_list[[1]]))  # Show the first symbol's closing prices
      } else {
        data.frame(Message = "No data fetched")
      }
    })
  })
  
  # Plot price data
  output$price_plot <- renderPlot({
    plot_prices(fetched_data())
  })
  
  # Optimize portfolio when the "Optimize Portfolio" button is clicked
  observeEvent(input$optimize, {
    data_list <- fetched_data()
    if (length(data_list) > 0) {
      weights <- optimize_portfolio(data_list, input$risk / 100, input$growth / 100)
      output$weights <- renderText({
        paste(names(weights), ": ", round(weights * 100, 2), "%", collapse = ", ")
      })
    } else {
      showNotification("No data available for optimization!", type = "error")
    }
  })
}

# Run App
shinyApp(ui, server)
