#' Smart Numerical Differentiation
#' @export
smart_deriv <- function(fn, x, ...) {
  # Richardson extrapolation is the most accurate numerical method
  grad <- tryCatch(numDeriv::grad(fn, x, method = "Richardson", ...),
                   error = function(e) numDeriv::grad(fn, x, method = "simple", ...))

  hess <- tryCatch(numDeriv::hessian(fn, x, method = "Richardson", ...),
                   error = function(e) matrix(NA, length(x), length(x)))

  # Analyze the curvature using Eigenvalues of the Hessian
  shape <- "Unknown"
  if (!any(is.na(hess))) {
    eigs <- eigen(hess)$values
    if (all(eigs > 1e-6)) shape <- "Local Minimum (Convex)"
    else if (all(eigs < -1e-6)) shape <- "Local Maximum (Concave)"
    else if (any(eigs > 1e-6) && any(eigs < -1e-6)) shape <- "Saddle Point"
    else shape <- "Flat / Singular"
  }

  out <- list(
    x = x,
    gradient = grad,
    hessian = hess,
    shape = shape
  )
  class(out) <- "smart_deriv"
  return(out)
}

#' @export
print.smart_deriv <- function(x, ...) {
  cat("\n\U25B6 Smart Differentiation Results\n")
  cat(rep("-", 40), "\n", sep="")
  cat(sprintf("\U25AA Point evaluated: %s\n", paste(round(x$x, 4), collapse = ", ")))
  cat(sprintf("\U25AA Surface shape: %s\n", x$shape))
  cat("\nGradient (First Derivative):\n")
  print(x$gradient)
  cat("\nHessian Matrix (Second Derivative):\n")
  print(x$hessian)
  invisible(x)
}
