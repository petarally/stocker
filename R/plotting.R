library(ggplot2)
library(gridExtra)

plot_prices <- function(data_list, investment_data = NULL, portfolio_values = NULL) {
  if (length(data_list) == 0) {
    return(NULL)
  }

  plots <- lapply(names(data_list), function(symbol) {
    data <- data_list[[symbol]]
    
    data_df <- data.frame(Date = as.Date(index(data)), Price = as.numeric(Cl(data)))
    
    p <- ggplot(data = data_df, aes(x = Date, y = Price)) +
      geom_line(linewidth = 1.2, color = "#2980b9") +
      ggtitle(paste("Price of", symbol)) +
      theme_minimal() +
      xlab("Date") +
      ylab("Price") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    # Plot investment data if provided
    if (!is.null(investment_data)) {
      investment_df <- data.frame(Date = as.Date(sapply(investment_data, function(x) x$date)),
                                  Amount = sapply(investment_data, function(x) x$amount))
      p <- p + geom_point(data = investment_df, aes(x = Date, y = Amount), color = "red", size = 3)
    }
    
    return(p)
  })

  # Plot portfolio values if provided
  if (!is.null(portfolio_values)) {
    portfolio_plot <- ggplot(data = portfolio_values, aes(x = Date, y = Value)) +
      geom_line(linewidth = 1.2, color = "#27ae60") +
      ggtitle("Portfolio Value Over Time") +
      theme_minimal() +
      xlab("Date") +
      ylab("Portfolio Value (€)") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    plots <- c(plots, list(portfolio_plot))
  }

  do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
}