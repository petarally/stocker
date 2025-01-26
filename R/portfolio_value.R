library(quantmod)

# Function to calculate portfolio value
calculate_portfolio_value <- function(investment_data, stock_data) {
  total_shares <- 0
  closing_prices <- Cl(stock_data) # Closing prices
  trading_dates <- time(closing_prices) # Dates
  
  for (investment in investment_data) {
    # Parse investment details
    amount <- investment[["amount"]]
    input_date <- as.Date(investment[["date"]])
    
    # Find the closest trading date (handle time series discrepancies)
    date_index <- which.min(abs(trading_dates - input_date))
    stock_price <- as.numeric(closing_prices[date_index])
    
    if (is.na(stock_price)) {
      stop(paste("No stock price available for the date:", input_date))
    }
    
    # Calculate shares purchased
    shares <- amount / stock_price
    total_shares <- total_shares + shares
    
    # Debugging
    print(paste("Processing investment: amount =", amount, ", date =", input_date))
    print(paste("Stock price on", trading_dates[date_index], "=", stock_price))
    print(paste("Shares purchased on", input_date, "=", shares))
  }
  
  # Get the current stock price (the last available price)
  current_price <- as.numeric(tail(closing_prices, 1)) # Use tail() to get the last closing price
  print(paste("Current stock price:", current_price))
  
  total_value <- total_shares * current_price # Total portfolio value
  
  # Debugging
  print(paste("Total shares:", total_shares))
  print(paste("Total portfolio value:", total_value))
  
  return(total_value)
}