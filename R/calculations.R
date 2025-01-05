library(ROI)
library(ROI.plugin.quadprog)
library(PerformanceAnalytics)

# Define calculate_annualized_returns function
calculate_annualized_returns <- function(data_list) {
  daily_returns <- lapply(data_list, function(data) dailyReturn(Cl(data)))
  annualized_returns <- sapply(daily_returns, function(ret) {
    annualized_return <- (prod(1 + ret)^(252 / length(ret)) - 1)  # Assuming 252 trading days in a year
    return(annualized_return)
  })
  return(annualized_returns)
}

# Define calculate_annualized_return function
calculate_annualized_return <- function(target_growth, months) {
  target_growth_decimal <- target_growth / 100
  annualized_return <- (1 + target_growth_decimal)^(12 / months) - 1
  return(annualized_return)
}

optimize_portfolio <- function(data_list, max_risk, target_growth, investment_amount) {
  max_risk_decimal <- max_risk / 100
  
  annualized_returns <- calculate_annualized_returns(data_list)
  daily_returns <- do.call(cbind, lapply(data_list, function(data) dailyReturn(Cl(data))))
  cov_matrix <- cov(daily_returns, use = "complete.obs")
  
  # Debugging statements
  print("Annualized Returns:")
  print(annualized_returns)
  print("Covariance Matrix:")
  print(cov_matrix)
  
  n <- length(annualized_returns)
  
  opt_portfolio <- OP(
    objective = Q_objective(Q = cov_matrix, L = rep(0, n)),
    constraints = L_constraint(
      L = rbind(
        rep(1, n),                # Sum of weights = 1
        annualized_returns,       # Return constraint
        sqrt(diag(cov_matrix))    # Risk constraint
      ),
      dir = c("==", ">=", "<="),
      rhs = c(1, target_growth / 100, max_risk_decimal)
    ),
    bounds = V_bound(lb = rep(0, n), ub = rep(1, n))
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