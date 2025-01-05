library(quantmod)

fetch_data <- function(tickers) {
  symbols <- unlist(strsplit(tickers, ",\\s*"))
  data_list <- list()
  
  for (symbol in symbols) {
    tryCatch({
      data <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
      data_list[[symbol]] <- data
    }, error = function(e) {
      message(paste("Error fetching data for:", symbol))
    })
  }
  return(data_list)
}
