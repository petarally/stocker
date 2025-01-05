library(ggplot2)
library(gridExtra)

plot_prices <- function(data_list) {
  if (length(data_list) == 0) {
    return(NULL)
  }
  
  plots <- lapply(names(data_list), function(symbol) {
    data <- data_list[[symbol]]
    ggplot(data = fortify.zoo(Cl(data)), aes(x = Index, y = Cl)) +
      geom_line() +
      ggtitle(paste("Price of", symbol)) +
      xlab("Date") +
      ylab("Price")
  })
  
  do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
}
