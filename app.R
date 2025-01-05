library(shiny)
library(shinyWidgets)  # For modern input elements
source("R/fetch_data.R")
source("R/calculations.R")
source("R/plotting.R")

ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "icon", href = "www/logo.png")
  ),
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
          actionButton("fetch", "Dohvati podatke", class = "btn btn-primary btn-lg"),
          br(),
          actionButton("optimize_btn", "Optimiziraj portfelj", class = "btn btn-success btn-lg")
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
  
# Optimize portfolio
observeEvent(input$optimize_btn, {
  tryCatch({
    data_list <- fetched_data()
    if (length(data_list) == 0) {
      stop("No data available for optimization!")
    }
    
    # Optimize the portfolio using the input values directly
    result <- optimize_portfolio(
      data_list = data_list,
      max_risk = input$max_risk,
      target_growth = input$target_growth,
      investment_amount = input$investment_amount
    )
    
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