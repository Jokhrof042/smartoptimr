#' @title Smart Integration
#' @description Performs numerical integration.
#' @param fn The function to integrate.
#' @param lower The lower limit of integration.
#' @param upper The upper limit of integration.
#' @param ... Additional arguments.
#' @export
smart_integrate <- function(fn, lower, upper, ...) {
  n_dim <- length(lower)
  if (length(upper) != n_dim) stop("Lower and upper bounds must have the same length.")

  # Bulletproof wrapper: evaluates one point at a time and catches NAs
  safe_fn <- function(x) {
    val <- tryCatch(fn(x, ...), error = function(e) NA)
    if (length(val) != 1 || is.na(val) || !is.finite(val)) {
      return(1e30) # Return a huge number if the function breaks here to avoid crashing the whole integral
    }
    return(val)
  }

  # Use Adaptive Cubature with vectorInterface = FALSE for maximum reliability
  res <- tryCatch({
    cubature::hcubature(f = safe_fn, lowerLimit = lower, upperLimit = upper,
                        tol = 1e-5, vectorInterface = FALSE)
  }, error = function(e) {
    stop("Integration failed. The function might have extreme discontinuities. Error: ", e$message)
  })

  out <- list(
    value = res$integral,
    error_estimate = res$error,
    evaluations = res$functionEvaluations,
    dimensions = n_dim
  )

  class(out) <- "smart_integrate"
  return(out)
}
#' @export
print.smart_integrate <- function(x, ...) {
  cat("\n\U25B6 Smart Integration Results\n")
  cat(rep("-", 40), "\n", sep="")
  cat(sprintf("\U25AA Dimensions: %dD\n", x$dimensions))
  cat(sprintf("\U25AA Integral Value: %g\n", x$value))
  cat(sprintf("\U25AA Est. Error: %g\n", x$error_estimate))
  cat(sprintf("\U25AA Function Evals: %d\n", x$evaluations))
  invisible(x)
}
