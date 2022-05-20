calcGompertzParam <- function(start, growth, demand.full, t.fix, startval){
  
  Cmax <- demand.full
  C0 <- start
  C.fix <- start*(1+growth)**t.fix
  
  fn <- function(z) {
    # x[1] = k
    # x[2] = t0
    x <- z**2  # To ensure x is positive
    cond1 <- Cmax * exp(-exp(-x[1] * (t.fix - x[2]))) - C.fix
    cond2 <- Cmax * exp(-exp(-x[1] * (0 - x[2]))) - C0
    return(c(cond1, cond2))
  }
  
  calc <- nleqslv(startval**0.5, fn, method = "Newton")
  b1 <- calc$x[1]**2
  t0 <- calc$x[2]**2
  
  if (calc$termcd == 1) return(tibble(b1,t0))
}