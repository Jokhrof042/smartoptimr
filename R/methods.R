#' @export
print.smart_optim <- function(x, ...) {
  cat("\n\U25B6 Optimization Results (smartoptimr)\n")
  cat(rep("-", 45), "\n", sep="")

  if (x$convergence_code == 0) cat("\U2705 Status: Success\n") else cat("\U274C Status: Failed or Iteration Limit\n")

  cat(sprintf("\U25AA Winning Method: %s\n", x$method_used))
  cat(sprintf("\U25AA Best objective: %g\n", x$value))
  cat(sprintf("\U25AA Total Run time: %s secs\n", x$time))

  if (!is.null(x$benchmark_results)) {
    cat("\n\U25B6 Benchmark Leaderboard:\n")
    # Sort by best value
    df <- x$benchmark_results[order(x$benchmark_results$value), ]
    rownames(df) <- NULL
    print(df)
  }

  cat("\nBest Parameters:\n")
  print(x$par)
  invisible(x)
}
