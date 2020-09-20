# Helper functions to fit generalized linear models
# via regular Fisher scoring (stats::glm)
# via combinatorial expectation maximization (logbin::logbin and addreg::addreg)
# via logit link--the logistic model just for comparison

#' @import stats
#' @import addreg
#' @importFrom logbin logbin


# (2) Binomial
estimate_glm <- function(formula, data, link, start = NULL, weights = NULL, ...) {
  fit <- eval(substitute(stats::glm(formula, data = data, family = binomial(link = link),
                                    start = start, weights = weights, ...)))
  if(!is.null(start))
    class(fit) <- c("glm_start", class(fit))
  fit <- estimate_maxprob(fit = fit, formula = formula, data = data,
                          link = link, start = start)
  return(fit)
}

# (3r) Log binomial via logbin::
estimate_logbin <- function(formula, data, start = NULL, method = "cem", ...) {
  fit <- logbin::logbin(formula = formula, data = data,
                        method = method, start = start, ...)
  return(estimate_maxprob(fit = fit, formula = formula, data = data, link = "log",
                          start = start))
}

# (3d) Additive binomial via addreg::
estimate_addreg <- function(formula, data, start = NULL, method = "cem", ...) {
  fit <- addreg::addreg(formula = formula, data = data,
                        family = stats::binomial(link = "identity"),
                        method = method, start = start, ...)
  return(estimate_maxprob(fit, formula = formula, data = data, link = "identity",
                          start = start))
}

# (5) Logistic model, for comparison only
estimate_logistic <- function(formula, data, weights = NULL, ...) {
  fit <- eval(substitute(stats::glm(formula, data = data, family = stats::binomial(link = "logit"),
                                    weights = weights, ...)))
  class(fit) <- c("logistic", class(fit))
  fit <- estimate_maxprob(fit = fit, formula = formula, data = data, link = "logit")
  return(fit)
}
