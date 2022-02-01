# Function to calculate parameters of truncated normal distribution based on
# given truncation (a, b), FID capacity, x% of feasibility capacity  and
# corresponding assumption on the probabilities of these capacities
calcTruncNormParam2 <- function(a, b, cap.1, cumprob.1, cap.mean) {
  # Function that includes both equations
  # x[1] = mean, x[2] = standard deviation
  fn  <- function(x) {
    # First condition: Mean value
    root1 <- 
      x[1] - x[2] * 
      (dnorm((b - x[1]) / x[2]) - dnorm((a - x[1]) / x[2])) / 
      (pnorm((b - x[1]) / x[2]) - pnorm((a - x[1]) / x[2])) - cap.mean
    
    # Second function (second condition)
    root2 <-
      (pnorm((cap.1 - x[1]) / x[2]) - pnorm((a - x[1]) / x[2])) /
      (pnorm((b - x[1]) / x[2]) - pnorm((a - x[1]) / x[2])) - cumprob.1
    
    return(c(root1, root2))
  }
  
  # Solve nonlinear system of equations
  est <- nleqslv(c(cap.1, cap.1), fn)
  
  # Return solution
  result <- c("mean" = est$x[1], "sd" = est$x[2])
  return(result)
}