# 🚀 smartoptimr: Automated Calculus & Optimization for R

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**`smartoptimr`** is a powerful computational statistics engine for R. It completely automates the hardest parts of mathematical statistics: deriving complex Gradients and Hessian matrices by hand.

Provide `smartoptimr` with a statistical distribution (or any custom function), and it will automatically generate the Negative Log-Likelihood, perform algebraic symbolic differentiation, and produce highly-optimized R functions ready for Maximum Likelihood Estimation (MLE).

## ✨ Key Features
* 📚 **Built-in Statistical Library**: Instantly fetch the exact Negative Log-Likelihood functional forms for **30 different statistical distributions** (Normal, Gamma, Weibull, Poisson, Rayleigh, Student's t, and more).
* 🧮 **Symbolic Calculus Engine**: Automatically applies the Chain Rule, Product Rule, and Quotient Rule to generate exact algebraic formulas for Gradients (1st derivative) and Hessians (2nd derivative).
* ⚡ **Lightning Fast Math**: Uses Common Subexpression Elimination (CSE) to optimize the generated calculus, ensuring your matrices compute at C-level speeds without crashing R.
* 🎯 **Numerical Fallbacks & Optimizers**: Includes robust numerical differentiation (`smart_deriv`), integration (`smart_integrate`), and a custom optimizer (`smart_optim`) for functions that defy pure algebra.

---

## 📦 Installation

You can install the development version of `smartoptimr` directly from GitHub using `devtools`:

```R
# install.packages("devtools")
devtools::install_github("Jokhrof042/smartoptimr")

library(smartoptimr)
```

---

## 🚀 Quick Start: Defeating Manual Calculus

Say goodbye to doing grueling calculus by hand or making typos in massive matrices. Here is how `smartoptimr` solves a textbook Maximum Likelihood Estimation problem in just 3 steps:

### 1. Fetch the Log-Likelihood
Instantly summon the functional form of the Normal distribution:
```R
ll_normal <- smart_loglik("normal")
```

### 2. Generate the Exact Calculus
Ask the engine to derive the Gradient and the Hessian algebraically:
```R
# Generate the exact algebraic Gradient (1st Derivative)
g_sym <- smart_sym_deriv(ll_normal, var = c("mu", "sigma2"))

# Generate the exact algebraic Hessian Matrix (2nd Derivative)
H_sym <- smart_sym_deriv(g_sym, var = c("mu", "sigma2"))

# Look at the beautiful, optimized R code it generates automatically!
print(H_sym)
```

### 3. Evaluate with Real Data
You no longer need to type out massive fraction formulas manually. Just plug your data straight into the automatically generated Hessian!
```R
set.seed(123)
my_data <- rnorm(25, mean = 17, sd = 2)

# Evaluate the Hessian at mu = 17, sigma2 = 4
H_values <- H_sym(mu = 17, sigma2 = 4, x = my_data, n = 25)
H_matrix <- matrix(H_values, nrow = 2, dimnames = list(c("mu", "sigma2"), c("mu", "sigma2")))

print(H_matrix)
```

---

## 📖 The Statistical Library (`smart_loglik`)

`smartoptimr` comes with 30 statistical distributions pre-programmed. You can pass any of these directly into the symbolic calculus engine:

```R
ll_gamma    <- smart_loglik("gamma")
ll_weibull  <- smart_loglik("weibull")
ll_poisson  <- smart_loglik("poisson")
ll_rayleigh <- smart_loglik("rayleigh")
```
*Run `?smart_loglik` in R to see the full list of supported distributions.*

---

## 🛠 Advanced Usage: Custom Functions
You are not limited to the built-in distributions. You can write **any** continuous mathematical function using standard R syntax, and `smartoptimr` will derive it.

```R
# A custom, highly complex function
my_complex_func <- function(x, y) {
  exp(-x^2) * log(y) / sin(x)
}

# The package knows the quotient rule, chain rule, and product rule!
custom_gradient <- smart_sym_deriv(my_complex_func, var = c("x", "y"))
```
