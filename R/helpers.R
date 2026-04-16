#' Check Analytic Gradient against Numeric Gradient
#' @keywords internal
check_gradient <- function(par, fn, gr, tol = 1e-4, ...) {
  num_g <- tryCatch(numDeriv::grad(fn, par, ...), error = function(e) NULL)
  ana_g <- tryCatch(gr(par, ...), error = function(e) NULL)

  if (is.null(num_g) || is.null(ana_g)) return(FALSE)

  max_diff <- max(abs(num_g - ana_g))
  return(max_diff < tol)
}

#' Generate Multiple Starting Points
#' @keywords internal
generate_starts <- function(par, lower, upper, n_starts) {
  n_par <- length(par)
  starts <- matrix(nrow = n_starts, ncol = n_par)
  starts[1, ] <- par # First start is always the user's guess

  if (n_starts > 1) {
    for (i in 2:n_starts) {
      # Add 10% jitter to the parameters
      jitter <- stats::runif(n_par, min = 0.9, max = 1.1)
      candidate <- par * jitter

      # Ensure candidate stays within bounds
      if (length(lower) == 1) lower <- rep(lower, n_par)
      if (length(upper) == 1) upper <- rep(upper, n_par)

      candidate <- pmax(candidate, lower)
      candidate <- pmin(candidate, upper)
      starts[i, ] <- candidate
    }
  }
  return(starts)
}
