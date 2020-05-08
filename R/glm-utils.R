# Helper functions to fit generalized linear models
# via regular Fisher scoring (stats::glm)
# via combinatorial expectation maximization (logbin::logbin and addreg::addreg)
# via logit link--the logistic model just for comparison

# (2) Binomial
estimate_glm <- function(formula, data, link, start = NULL, ...) {
  fit <- glm(formula, data = data, family = binomial(link = link), start = start, ...)
  if(!is.null(start))
    class(fit) <- c("glm_start", class(fit))
  fit <- estimate_maxprob(fit)
  return(fit)
}

# (3r) Log binomial via logbin::
estimate_logbin <- function(formula, data, start = NULL, method = "cem", ...) {
  fit <- logbin::logbin(formula = formula, data = data,
                        method = method, start = start, ...)
  return(estimate_maxprob(fit, start = start))
}

# (3d) Additive binomial via addreg::
estimate_addreg <- function(formula, data, start = NULL, method = "cem", ...) {
  fit <- addreg::addreg(formula = formula, data = data,
                        family = binomial(link = "identity"),
                        method = method, start = start, ...)
  return(estimate_maxprob(fit, start = start))
}

# (5) Logistic model, for comparison only
estimate_logistic <- function(formula, data, ...) {
  fit <- glm(formula, data = data, family = binomial(link = "logit"), ...)
  class(fit) <- c("logistic", class(fit))
  fit <- estimate_maxprob(fit)
  return(fit)
}
