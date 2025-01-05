library(ggplot2)
library(gridExtra)

plot_prices <- function(data_list) {
  if (length(data_list) == 0) {
    return(NULL)
  }

  plots <- lapply(names(data_list), function(symbol) {
    data <- data_list[[symbol]]
    
    data_df <- data.frame(Date = as.Date(index(data)), Price = as.numeric(Cl(data)))
    
    ggplot(data = data_df, aes(x = Date, y = Price)) +
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
  })

  do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
}
