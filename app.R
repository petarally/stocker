library(shiny)
library(shinyWidgets)  # For modern input elements
source("R/fetch_data.R")
source("R/calculations.R")
source("R/plotting.R")

ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),  # Use a modern Bootstrap theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "icon", href = "www/logo.png")  # Optional favicon
  ),
  navbarPage(
    title = "Portfolio Optimizer",
    tabPanel(
      "Dashboard",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Investment Preferences"),
          textInput("symbols", "Enter Tickers (comma-separated):", value = "BTC-USD, ETH-USD"),
          numericInput("target_growth", "Target Growth (%):", value = 18, min = 0, max = 100),
          numericInput("max_risk", "Max Risk (%):", value = 10, min = 0, max = 100),
          numericInput("months", "Investment Period (Months)", 12, min = 1),
          numericInput("investment_amount", "Total Investment Amount (€):", value = 1000, min = 1),  # New input field
          actionButton("fetch", "Fetch Data", class = "btn btn-primary btn-lg"),
          br(),
          actionButton("optimize_btn", "Optimize Portfolio", class = "btn btn-success btn-lg")
        ),
        mainPanel(
          tags$h4("Data Preview"),
          tableOutput("data_preview"),
          tags$hr(),
          tags$h4("Optimized Weights"),
          textOutput("weights"),
          tags$hr(),
          tags$h4("Investment Distribution"),
          textOutput("investment_distribution"),  # New output element
          tags$hr(),
          tags$h4("Price Trends"),
          plotOutput("price_plot")
        )
      )
    ),
    tabPanel(
      "About",
      fluidRow(
        column(
          12,
          tags$h3("Portfolio Optimizer"),
          tags$p("This app helps you analyze investment opportunities in stocks or cryptocurrencies. 
                  Enter tickers, set your risk tolerance and target growth, and let the app optimize your portfolio."),
          tags$h4("How it Works"),
          tags$ul(
            tags$li("Fetch data dynamically from Yahoo Finance."),
            tags$li("Visualize historical price trends."),
            tags$li("Optimize portfolio based on risk-return tradeoffs.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  fetched_data <- reactiveVal(list())
  
  # Fetch data
  observeEvent(input$fetch, {
    showNotification("Fetching data...", type = "message")
    tryCatch({
      data_list <- fetch_data(input$symbols)  # Fetch data for tickers
      if (length(data_list) == 0) {
        stop("No data was fetched. Please check your tickers.")
      }
      fetched_data(data_list)
      
      # Show preview of the data
      output$data_preview <- renderTable({
        head(data.frame(Date = as.Date(index(data_list[[1]])), Price = as.numeric(Cl(data_list[[1]]))))
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Plot price trends
  output$price_plot <- renderPlot({
    tryCatch({
      plot_prices(fetched_data())  # Assuming plot_prices function exists
    }, error = function(e) {
      showNotification(paste("Error plotting data:", e$message), type = "error")
    })
  })
  
  # Optimize portfolio
  observeEvent(input$optimize_btn, {
    tryCatch({
      data_list <- fetched_data()
      if (length(data_list) == 0) {
        stop("No data available for optimization!")
      }
      
      # Convert target growth percentage to decimal
      target_growth_decimal <- input$target_growth / 100
      print(paste("Target Growth (decimal):", target_growth_decimal))  # Debugging statement
      
      # Calculate the target return based on the target growth and months
      target_return <- calculate_annualized_return(target_growth_decimal, input$months)
      print(paste("Target Return:", target_return))  # Debugging statement
      
      # Ensure target_return is a single numeric value
      if (length(target_return) != 1 || !is.numeric(target_return)) {
        stop("Invalid target return calculated.")
      }
      
      # Optimize the portfolio based on max risk, target return, and investment amount
      result <- optimize_portfolio(data_list, input$max_risk / 100, target_return, input$investment_amount)
      
      # Display the optimized portfolio weights and investment amounts
      output$weights <- renderText({
        portfolio_weights <- paste(names(result$weights), ": ", round(result$weights * 100, 2), "%", collapse = ", ")
        portfolio_weights
      })
      
      output$investment_distribution <- renderText({
        investment_distribution <- paste(names(result$investment_amounts), ": €", round(result$investment_amounts, 2), collapse = ", ")
        investment_distribution
      })
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

shinyApp(ui, server)