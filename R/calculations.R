library(ROI)
library(ROI.plugin.quadprog)
library(PerformanceAnalytics)

# Define calculate_annualized_returns function
# Function to calculate annualized return based on the target growth for a given number of months
calculate_annualized_returns <- function(data_list) {
  # Extract daily returns from closing prices of the assets in the data list
  daily_returns <- lapply(data_list, function(data) dailyReturn(Cl(data)))
  
  # Compute the annualized returns
  annualized_returns <- sapply(daily_returns, function(ret) {
    # Use the geometric mean to annualize returns
    annualized_return <- (prod(1 + ret)^(252 / length(ret)) - 1)  # Assuming 252 trading days in a year
    return(annualized_return)
  })
  
  return(annualized_returns)
}

# Define calculate_annualized_return function
calculate_annualized_return <- function(target_growth, months) {
  # Convert target growth percentage to decimal
  target_growth_decimal <- target_growth / 100
  
  # Calculate the annualized return
  annualized_return <- (1 + target_growth_decimal)^(12 / months) - 1
  
  return(annualized_return)
}

# Define optimize_portfolio function
optimize_portfolio <- function(data_list, max_risk, target_growth, investment_amount) {
  # Calculate annualized return based on target growth
  target_return <- calculate_annualized_return(target_growth, months = 12)  # Use 12 months directly
  
  # Calculate the annualized returns for each asset
  annualized_returns <- calculate_annualized_returns(data_list)
  
  # Calculate the covariance matrix for the assets
  cov_matrix <- cov(do.call(cbind, lapply(data_list, function(data) dailyReturn(Cl(data)))), use = "complete.obs")
  
  # Validate data
  if (any(is.na(annualized_returns)) || any(is.na(cov_matrix))) {
    stop("Invalid data: Unable to compute returns or covariance matrix.")
  }
  
  # Get the feasible return range
  min_return <- min(annualized_returns)
  max_return <- max(annualized_returns)
  
  # Check if the target return is within the feasible range
  if (target_return < min_return) {
    showNotification(sprintf(
      "Target return %.2f%% is not feasible. Minimum feasible return is %.2f%%.",
      target_return * 100, min_return * 100
    ), type = "warning")
    target_return <- min_return
  }
  
  # Define the optimization problem
  n <- length(annualized_returns)
  opt_portfolio <- OP(
    objective = Q_objective(Q = cov_matrix, L = rep(0, n)),  # Minimize risk
    constraints = L_constraint(
      L = rbind(rep(1, n), annualized_returns), 
      dir = c("==", ">="),  # Corrected direction of constraints
      rhs = c(1, target_return)
    ),
    bounds = V_bound(lb = rep(0, n), ub = rep(1, n))  # Weights between 0 and 1
  )
  
  # Solve the optimization problem
  solution <- ROI_solve(opt_portfolio)
  
  if (!is.null(solution$message) && grepl("infeasible", solution$message)) {
    stop("Optimization failed: Constraints are not feasible. Adjust risk or return.")
  }
  
  # Calculate the investment amounts
  weights <- solution$solution
  investment_amounts <- weights * investment_amount
  names(investment_amounts) <- names(data_list)
  
  return(list(weights = weights, investment_amounts = investment_amounts))
}