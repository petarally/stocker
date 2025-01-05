library(ROI)
library(ROI.plugin.quadprog)
library(PerformanceAnalytics)

optimize_portfolio <- function(data_list, max_risk, target_return) {
  # Calculate returns
  returns_list <- lapply(data_list, function(data) dailyReturn(Cl(data)))
  mean_returns <- sapply(returns_list, Return.annualized)
  cov_matrix <- cov(do.call(cbind, returns_list))
  
  # Define optimization problem
  n <- length(mean_returns)
  opt_portfolio <- OP(
    objective = Q_objective(Q = cov_matrix, L = -mean_returns),
    constraints = L_constraint(cbind(rep(1, n), mean_returns), dir = c("=", ">="), rhs = c(1, target_return)),
    bounds = V_bound(lb = rep(0, n), ub = rep(1, n))
  )
  
  solution <- ROI_solve(opt_portfolio)
  return(solution$solution)
}
