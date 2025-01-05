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

optimize_portfolio <- function(data_list, max_risk, target_growth, investment_amount) {
  max_risk_decimal <- max_risk / 100
  
  # Calculate returns and risk metrics
  annualized_returns <- calculate_annualized_returns(data_list)
  daily_returns <- do.call(cbind, lapply(data_list, function(data) dailyReturn(Cl(data))))
  cov_matrix <- cov(daily_returns, use = "complete.obs")
  
  n <- length(annualized_returns)
  
  # Add maximum weight constraint to ensure diversification
  max_weight <- 0.60  # Maximum 60% in any single asset
  
  opt_portfolio <- OP(
    objective = Q_objective(Q = cov_matrix, L = rep(0, n)),
    constraints = L_constraint(
      L = rbind(
        rep(1, n),                # Sum of weights = 1
        diag(n),                  # Individual weight constraints
        -diag(n),                 # Maximum weight constraints
        annualized_returns        # Return constraint
      ),
      dir = c("==", rep(">=", n), rep(">=", n), ">="),
      rhs = c(1, rep(0, n), rep(-max_weight, n), target_growth/100)
    ),
    bounds = V_bound(lb = rep(0.1, n), ub = rep(max_weight, n))  # Minimum 10% in each asset
  )
  
  solution <- ROI_solve(opt_portfolio)
  weights <- solution$solution
  names(weights) <- names(data_list)
  
  investment_amounts <- weights * investment_amount
  names(investment_amounts) <- names(data_list)
  
  return(list(
    weights = weights,
    investment_amounts = investment_amounts,
    achieved_return = sum(weights * annualized_returns),
    portfolio_risk = sqrt(t(weights) %*% cov_matrix %*% weights)
  ))
}

