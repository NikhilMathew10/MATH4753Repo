

#' ntickets function
#'
#' @param N, number of tickets
#' @param gamma, gamma value
#' @param p, probabilty value
#'
#' @return two graphs showing the optimal tickets for discrete and continuous
#' distributions
#' @export
#'
#' @examples
ntickets <- function(N, gamma, p) {

  # Calculate number of tickets using discrete distribution
  nd <- qpois(p * (1 + gamma), lambda = N * p)

  # Calculate number of tickets using normal approximation
  nc <- round(N * p + qnorm(1 - gamma) * sqrt(N * p * (1 - p)))

  # Print the list with required components
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Plot objective function vs n for discrete distribution
  n_values_discrete <- seq(0, N, by = 1)

  objective_discrete <- 1 - gamma - ppois(n_values_discrete, lambda = N * p)

  plot(sort(n_values_discrete), sort(objective_discrete), type = "l", col = "blue", xlab = "n", ylab = "Objective", main = sprintf("Objective Vs n to find optimal tickets sold gamma = %.2f, N = %d discrete", gamma, N))

  # Plot objective function vs n for normal distribution
  n_values_continuous <- seq(0, N, by = 1)

  objective_continuous <- 1 - gamma - pnorm(n_values_continuous, mean = round(N * p), sd = sqrt(N * p * (1 - p)))

  plot(sort(n_values_continuous), sort(objective_continuous), type = "l", col = "red", xlab = "n", ylab = "Objective", main = sprintf("Objective Vs n to find optimal tickets sold gamma = %.2f, N = %d continuous", gamma, N))
}
