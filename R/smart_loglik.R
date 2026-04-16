#' Fetch Known Negative Log-Likelihood Functions
#'
#' Automatically retrieves the mathematical formula for the Negative Log-Likelihood
#' of 30 common statistical distributions.
#'
#' @param distn_name A character string of the distribution name (e.g., "normal", "gumbel").
#' @return An R function mathematically prepared for symbolic differentiation.
#' @export
smart_loglik <- function(distn_name) {

  distn_name <- tolower(trimws(distn_name))

  # The Master Statistical Library
  ll_func <- switch(distn_name,

                    # --- THE CLASSICS ---
                    "normal" = function(mu, sigma2, x, n) {
                      (n / 2) * log(2 * pi * sigma2) + (sum(x^2) - 2*mu*sum(x) + n*mu^2) / (2 * sigma2)
                    },
                    "exponential" = function(lambda, x, n) {
                      -n * log(lambda) + lambda * sum(x)
                    },
                    "poisson" = function(lambda, x, n) {
                      -sum(x) * log(lambda) + n * lambda
                    },
                    "bernoulli" = function(p, x, n) {
                      -sum(x) * log(p) - (n - sum(x)) * log(1 - p)
                    },
                    "binomial" = function(p, size, x, n) {
                      -sum(x) * log(p) - sum(size - x) * log(1 - p)
                    },
                    "geometric" = function(p, x, n) {
                      -n * log(p) - sum(x) * log(1 - p)
                    },

                    # --- CONTINUOUS MULTI-PARAMETER ---
                    "gamma" = function(alpha, beta, x, n) {
                      -n * alpha * log(beta) + n * lgamma(alpha) - (alpha - 1) * sum(log(x)) + beta * sum(x)
                    },
                    "beta" = function(alpha, beta, x, n) {
                      n * lbeta(alpha, beta) - (alpha - 1) * sum(log(x)) - (beta - 1) * sum(log(1 - x))
                    },
                    "lognormal" = function(mu, sigma2, x, n) {
                      (n / 2) * log(2 * pi * sigma2) + sum(log(x)) + (sum(log(x)^2) - 2*mu*sum(log(x)) + n*mu^2) / (2 * sigma2)
                    },
                    "weibull" = function(shape, scale, x, n) {
                      -n * log(shape) + n * shape * log(scale) - (shape - 1) * sum(log(x)) + sum((x / scale)^shape)
                    },
                    "cauchy" = function(location, scale, x, n) {
                      n * log(pi) + n * log(scale) + sum(log(1 + ((x - location) / scale)^2))
                    },
                    "logistic" = function(location, scale, x, n) {
                      n * log(scale) + sum((x - location) / scale) + 2 * sum(log(1 + exp(-(x - location) / scale)))
                    },
                    "pareto" = function(scale, shape, x, n) {
                      -n * log(shape) - n * shape * log(scale) + (shape + 1) * sum(log(x))
                    },
                    "inverse_gaussian" = function(mu, lambda, x, n) {
                      -(n / 2) * log(lambda) + (lambda / (2 * mu^2)) * sum(x) - (n * lambda) / mu + (lambda / 2) * sum(1 / x)
                    },
                    "negative_binomial" = function(size, prob, x, n) {
                      -sum(lgamma(x + size)) + n * lgamma(size) - n * size * log(prob) - sum(x) * log(1 - prob)
                    },
                    "rayleigh" = function(sigma, x, n) {
                      2 * n * log(sigma) + sum(x^2) / (2 * sigma^2) - sum(log(x))
                    },

                    # --- THE NEW ADDITIONS (EXTREME VALUE, BAYESIAN, PHYSICS) ---

                    # 17. Laplace (Double Exponential) [Parameters: mu, b]
                    "laplace" = function(mu, b, x, n) {
                      n * log(2 * b) + sum(abs(x - mu)) / b
                    },

                    # 18. Gumbel (Extreme Value Type I) [Parameters: mu, beta]
                    "gumbel" = function(mu, beta, x, n) {
                      n * log(beta) + sum((x - mu) / beta) + sum(exp(-(x - mu) / beta))
                    },

                    # 19. Log-Logistic [Parameters: alpha (scale), beta (shape)]
                    "loglogistic" = function(alpha, beta, x, n) {
                      -n * log(beta) + n * beta * log(alpha) - (beta - 1) * sum(log(x)) + 2 * sum(log(1 + (x / alpha)^beta))
                    },

                    # 20. Inverse Gamma [Parameters: alpha (shape), beta (scale)]
                    "inverse_gamma" = function(alpha, beta, x, n) {
                      -n * alpha * log(beta) + n * lgamma(alpha) + (alpha + 1) * sum(log(x)) + beta * sum(1 / x)
                    },

                    # 21. Half-Normal [Parameter: sigma]
                    "half_normal" = function(sigma, x, n) {
                      n * log(sigma) + sum(x^2) / (2 * sigma^2)
                    },

                    # 22. Maxwell-Boltzmann (Physics) [Parameter: a]
                    "maxwell" = function(a, x, n) {
                      3 * n * log(a) - 2 * sum(log(x)) + sum(x^2) / (2 * a^2)
                    },

                    # 23. Fréchet (Extreme Value Type II) [Parameters: shape, scale]
                    "frechet" = function(shape, scale, x, n) {
                      -n * log(shape) + n * shape * log(scale) + (shape + 1) * sum(log(x)) + sum((x / scale)^(-shape))
                    },

                    # 24. Gompertz (Survival Analysis) [Parameters: shape, scale]
                    "gompertz" = function(shape, scale, x, n) {
                      -n * log(scale) - shape * sum(x) + (scale / shape) * sum(exp(shape * x) - 1)
                    },

                    # 25. Kumaraswamy (Alternative to Beta) [Parameters: a, b]
                    "kumaraswamy" = function(a, b, x, n) {
                      -n * log(a) - n * log(b) - (a - 1) * sum(log(x)) - (b - 1) * sum(log(1 - x^a))
                    },

                    # 26. Student's t-Distribution [Parameter: df]
                    "student_t" = function(df, x, n) {
                      -n * lgamma((df + 1) / 2) + n * lgamma(df / 2) + (n / 2) * log(df) + ((df + 1) / 2) * sum(log(1 + x^2 / df))
                    },

                    # 27. Chi-Square [Parameter: df]
                    "chi_square" = function(df, x, n) {
                      n * (df / 2) * log(2) + n * lgamma(df / 2) - (df / 2 - 1) * sum(log(x)) + sum(x) / 2
                    },

                    # 28. Lévy (Financial modeling) [Parameters: mu, c]
                    "levy" = function(mu, c, x, n) {
                      -(n / 2) * log(c) + (3 / 2) * sum(log(x - mu)) + (c / 2) * sum(1 / (x - mu))
                    },

                    # 29. Erlang [Parameters: shape (k), rate (lambda)]
                    "erlang" = function(shape, rate, x, n) {
                      -n * shape * log(rate) + n * lgamma(shape) - (shape - 1) * sum(log(x)) + rate * sum(x)
                    },

                    # 30. Zero-Truncated Poisson [Parameter: lambda]
                    "zero_truncated_poisson" = function(lambda, x, n) {
                      n * log(exp(lambda) - 1) - log(lambda) * sum(x)
                    },

                    # Catch-all error if the distribution doesn't exist
                    stop(paste("Distribution '", distn_name, "' not found! Check spelling or consider adding it to the package.", sep=""))
  )

  return(ll_func)
}
