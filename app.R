library(shiny)
library(shinyWidgets)  # For modern input elements
library(jsonlite)  # For JSON parsing
source("R/fetch_data.R")
source("R/calculations.R")
source("R/plotting.R")
source("R/portfolio_value.R")

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        setTimeout(function() {
          alert('Upozorenje! Ova aplikacija je samo demonstracija višeciljnog odlučivanja te ne pruža stvarne financijske savjete. ');
        }, 0);
      });
    ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "icon", href = "www/logo.png")
  ),
  theme = shinythemes::shinytheme("cerulean"),
  navbarPage(
    title = "Optimizator Portfelja",
    tabPanel(
      "Nadzorna Ploča",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Preferencije Ulaganja"),
          textInput("symbols", "Unesite oznake dionica (odvojene zarezom):", value = "BTC-USD, ETH-USD"),
          numericInput("target_growth", "Ciljani rast (%):", value = 18, min = 0, max = 100),
          numericInput("max_risk", "Maksimalni rizik (%):", value = 10, min = 0, max = 100),
          numericInput("months", "Razdoblje ulaganja (mjeseci):", 12, min = 1),
          numericInput("investment_amount", "Ukupni iznos ulaganja (€):", value = 1000, min = 1),
          fluidRow(
            column(12, class = "d-flex justify-content-between",
              actionButton("fetch", "Dohvati podatke", class = "btn btn-primary btn-lg"),
              actionButton("optimize_btn", "Optimiziraj portfelj", class = "btn btn-success btn-lg")
            )
          )
        ),
        mainPanel(
          tags$h4("Optimizirani Udjeli"),
          textOutput("weights"),
          tags$hr(),
          tags$h4("Raspodjela Ulaganja"),
          textOutput("investment_distribution"),
          tags$hr(),
          tags$h4("Kretanje Cijena"),
          plotOutput("price_plot")
        )
      )
    ),
    tabPanel(
      "Izračun Vrijednosti Portfelja",
      sidebarLayout(
        sidebarPanel(
          tags$h4("Unos Ulaganja"),
          textInput("stock_symbol", "Unesite oznaku dionice:", value = "AAPL"),
          textAreaInput("investment_data", "Unesite amount, date (yyyy-mm-dd) parove u JSON formatu:", 
                        value = '[{"amount": 1000, "date": "2022-01-01"}, {"amount": 1500, "date": "2022-06-01"}]', 
                        rows = 10),
          actionButton("calculate_value", "Izračunaj Vrijednost", class = "btn btn-primary btn-lg")
        ),
        mainPanel(
          tags$h4("Vrijednost Portfelja"),
          textOutput("portfolio_value"),
          tags$hr()
        )
      )
    ),
    tabPanel(
      "O Aplikaciji",
      fluidRow(
        column(
          12,
          tags$h3("Optimizator Portfelja"),
          tags$p("Ova aplikacija pomaže u analizi mogućnosti ulaganja u dionice ili kriptovalute. 
                 Unesite oznake, postavite toleranciju rizika i ciljani rast, i pustite aplikaciju da optimizira vaš portfelj."),
          tags$h4("Kako Radi"),
          tags$ul(
            tags$li("Dinamički dohvaća podatke s Yahoo Finance-a."),
            tags$li("Oznake dionica je potrebno pronaći na https://finance.yahoo.com/"),
            tags$li("Vizualizira povijesne trendove cijena."),
            tags$li("Optimizira portfelj na temelju odnosa rizika i povrata.")
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
  
  # Optimize portfolio using default method
  observeEvent(input$optimize_btn, {
    tryCatch({
      data_list <- fetched_data()
      if (length(data_list) == 0) {
        stop("No data available for optimization!")
      }
      
      result <- optimize_portfolio(
        data_list = data_list,
        max_risk = input$max_risk,
        target_growth = input$target_growth,
        investment_amount = input$investment_amount
      )
      
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
  
  # Calculate portfolio value based on (x, y) pairs
  observeEvent(input$calculate_value, {
    showNotification("Calculating portfolio value...", type = "message")
    tryCatch({
      # Parse the JSON input
      investment_data <- fromJSON(input$investment_data)
      
      # Debugging: Print the parsed investment data
      print("Parsed investment data:")
      print(investment_data)
      
      stock_symbol <- input$stock_symbol
      
      # Ensure investment_data is a list of lists
      if (!is.list(investment_data) || !all(sapply(investment_data, is.list))) {
        # Convert data frame to list of lists
        investment_data <- lapply(seq_len(nrow(investment_data)), function(i) as.list(investment_data[i, ]))
      }
      
      # Debugging: Print the converted investment data
      print("Converted investment data:")
      print(investment_data)
      
      # Determine the earliest date from the investment data
      earliest_date <- min(as.Date(sapply(investment_data, function(x) x$date)))
      
      # Fetch historical data for the specified stock starting from the earliest date
      stock_data <- fetch_data(stock_symbol)
      if (length(stock_data) == 0) {
        stop("No data was fetched for the specified stock. Please check the stock symbol.")
      }
      
      # Debugging statement
      print("Fetched stock data:")
      print(stock_data)
      
      # Convert investment amounts to euros and calculate portfolio value
      portfolio_values <- calculate_portfolio_value(investment_data, stock_data[[stock_symbol]])
      
      # Debugging: Print the portfolio values
      print("Portfolio values:")
      print(portfolio_values)
      
      output$portfolio_value <- renderText({
        paste("Vrijednost Portfelja: €", round(portfolio_values, 2))
      })
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

shinyApp(ui, server)