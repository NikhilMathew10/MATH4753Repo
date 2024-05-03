#' Curve plotting function
#'
#' @param mu the mean of the distribution
#' @param sigma the standard deviation of the distribution
#' @param a the limit for the area covered by the plotting function
#'
#' @return the curve with area shaded and a list with the mean, standard deviation, and
#' calculated probability
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma),
        xlab = "X", ylab = "Probability Density",
        main = "Normal Distribution Curve", col = "black", lwd = 2)

  # Shade the area under the curve up to x = a
  x_shaded <- seq(mu - 3*sigma, a, length.out = 100)
  y_shaded <- dnorm(x_shaded, mean = mu, sd = sigma)
  polygon(c(x_shaded, a, mu - 3*sigma), c(y_shaded, 0, 0), col = "red", border = NA)

  # Calculate the probability P(X <= a)
  prob_x_less_than_a <- pnorm(a, mean = mu, sd = sigma)

  # Print the probability to the command line
  cat("Probability (P(X <= a)): ", prob_x_less_than_a, "\n")

  # Return a list with mean, standard deviation, and calculated probability
  return(list(mu = mu, sigma = sigma, probability = prob_x_less_than_a))

}
