#' Smart Symbolic Differentiation
#'
#' Takes an R function and returns a brand new R function that represents its exact symbolic derivative.
#'
#' @param fn An R function to differentiate.
#' @param var Character vector of variables to differentiate with respect to (default is "x").
#' @return A new R function representing the derivative.
#' @export
smart_sym_deriv <- function(fn, var = "x") {
  tryCatch({
    # Attempt to write the exact symbolic derivative
    res_fn <- Deriv::Deriv(fn, var)

    # Safety check: ensure Deriv didn't fail silently
    if (is.null(res_fn)) stop("Returned NULL")

    # Assign our custom class for pretty printing
    class(res_fn) <- c("smart_sym", class(res_fn))
    return(res_fn)

  }, error = function(e) {
    # Intercept the ugly error and provide our beautiful custom recommendation
    stop(
      "\U274C The symbolic engine cannot differentiate this function (it may contain loops, logic, or unsupported math).\n",
      "\U2139 RECOMMENDATION: Use smart_deriv() for high-precision numerical differentiation instead.\n",
      call. = FALSE
    )
  })
}


#' Smart Symbolic Integration
#'
#' Takes a mathematical formula (as a string) and returns a brand new R function representing the exact symbolic integral.
#'
#' @param formula_str A string representing the mathematical formula (e.g., "x^2 * sin(x)").
#' @param var Character. The variable to integrate with respect to (default is "x").
#' @return A new R function representing the integrated formula.
#' @export
smart_sym_integrate <- function(formula_str, var = "x") {

  # 1. Universal Translator: R math -> Yacas math
  # Ryacas needs capitalized math functions, so we translate them safely.
  yacas_str <- formula_str
  yacas_str <- gsub("\\bexp\\b", "Exp", yacas_str)
  yacas_str <- gsub("\\bsin\\b", "Sin", yacas_str)
  yacas_str <- gsub("\\bcos\\b", "Cos", yacas_str)
  yacas_str <- gsub("\\btan\\b", "Tan", yacas_str)
  yacas_str <- gsub("\\blog\\b", "Ln", yacas_str)
  yacas_str <- gsub("\\basin\\b", "ArcSin", yacas_str)
  yacas_str <- gsub("\\bacos\\b", "ArcCos", yacas_str)
  yacas_str <- gsub("\\bsqrt\\b", "Sqrt", yacas_str)

  # 2. Ask Yacas to Integrate AND Simplify
  yacas_cmd <- sprintf("Simplify(Integrate(%s) %s)", var, yacas_str)

  tryCatch({
    # 3. Execute and translate back to pure R code
    res_expr <- Ryacas::yac_expr(yacas_cmd)

    # 4. SAFETY CHECK: Did Yacas give up?
    # If Yacas can't solve it, it returns the word "Integrate" or "AntiDeriv" unevaluated.
    expr_text <- deparse(res_expr)
    if (any(grepl("Integrate|AntiDeriv", expr_text))) {
      stop(
        "\U274C The symbolic engine cannot find an exact formula for this integral.\n",
        "\U2139 RECOMMENDATION: Use smart_integrate() to calculate the numerical area instead."
      )
    }

    # 5. Build the brand new R function
    int_fn <- function() {}
    formals(int_fn) <- stats::setNames(list(bquote()), var)
    body(int_fn) <- res_expr

    # Assign our custom class for pretty printing
    class(int_fn) <- c("smart_sym", "function")
    return(int_fn)

  }, error = function(e) {
    # If our custom stop() triggered, just pass that message up cleanly
    if (grepl("RECOMMENDATION", e$message)) stop(e$message, call. = FALSE)
    # Otherwise, catch general Yacas syntax errors
    stop("\U274C Integration failed. Ensure your formula string is valid R math. Error: ", e$message, call. = FALSE)
  })
}


#' Print method for smart symbolic functions
#'
#' Prints the generated mathematical function cleanly to the console.
#' @export
print.smart_sym <- function(x, ...) {
  cat("\n\U25B6 Smart Symbolic Form Generated\n")
  cat(rep("-", 40), "\n", sep="")

  # Remove the custom class just for printing so it displays as a normal R function
  class(x) <- "function"
  print(x)

  invisible(x)
}
