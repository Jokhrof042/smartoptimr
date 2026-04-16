#' @export
smart_optim <- function(par, fn, gr = NULL, lower = -Inf, upper = Inf,
                        method = "auto", n_starts = 1, scale = TRUE,
                        check_grad = TRUE, explain = TRUE, benchmark = FALSE,
                        control = list(), ...) {

  start_time <- Sys.time()
  n_par <- length(par)
  has_bounds <- any(is.finite(lower)) || any(is.finite(upper))
  has_gr <- !is.null(gr)

  # 1. DEFINE METHODS TO TEST
  if (benchmark) {
    if (explain) message("\U2139 Benchmark Mode Active: Testing multiple solvers side-by-side.")
    methods_to_try <- if (has_bounds) c("L-BFGS-B", "nlminb") else c("Nelder-Mead", "BFGS", "nlminb", "CG")
  } else {
    # Auto Method Selection
    if (method == "auto") {
      if (n_par == 1) methods_to_try <- "Brent"
      else if (has_bounds && has_gr) methods_to_try <- "L-BFGS-B"
      else if (has_bounds && !has_gr) methods_to_try <- "nlminb"
      else if (has_gr) methods_to_try <- "BFGS"
      else methods_to_try <- "Nelder-Mead"
    } else {
      methods_to_try <- method
    }
    if (explain && method == "auto") message("\U2139 Method 'auto' selected: ", methods_to_try)
  }

  starts_matrix <- generate_starts(par, lower, upper, n_starts)

  # 2. RUN OPTIMIZATION (Loop over methods and starts)
  results <- list()
  best_overall <- list(value = Inf)

  for (m in methods_to_try) {
    best_for_method <- list(value = Inf, convergence = 99)
    m_start_time <- Sys.time()

    for (i in 1:nrow(starts_matrix)) {
      current_par <- starts_matrix[i, ]

      fit <- tryCatch({
        if (m == "nlminb") {
          res <- stats::nlminb(start = current_par, objective = fn, gradient = gr,
                               lower = lower, upper = upper, control = control, ...)
          list(par = res$par, value = res$objective, counts = res$evaluations[1],
               convergence = res$convergence, message = res$message)
        } else {
          opt_lower <- if (m %in% c("L-BFGS-B", "Brent")) lower else -Inf
          opt_upper <- if (m %in% c("L-BFGS-B", "Brent")) upper else Inf
          suppressWarnings(
            stats::optim(par = current_par, fn = fn, gr = gr, method = m,
                         lower = opt_lower, upper = opt_upper, control = control, ...)
          )
        }
      }, error = function(e) list(convergence = 99, message = e$message, value = Inf))

      if (fit$value < best_for_method$value) best_for_method <- fit
    }

    m_time <- round(difftime(Sys.time(), m_start_time, units = "secs"), 4)

    # Save method results for the leaderboard
    results[[m]] <- list(
      method = m,
      value = best_for_method$value,
      convergence = best_for_method$convergence,
      time = m_time
    )

    # Track the global winner
    if (best_for_method$value < best_overall$value) {
      best_overall <- best_for_method
      best_overall$method_used <- m
    }
  }

  if (best_overall$value == Inf) stop("All optimization methods failed.")

  # 3. ASSEMBLE OUTPUT
  out <- list(
    par = best_overall$par,
    value = best_overall$value,
    convergence_code = best_overall$convergence,
    method_used = best_overall$method_used,
    time = round(difftime(Sys.time(), start_time, units = "secs"), 3),
    benchmark_results = if(benchmark) do.call(rbind, lapply(results, as.data.frame)) else NULL
  )

  class(out) <- "smart_optim"
  return(out)
}
