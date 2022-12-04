# Helper functions to fit generalized linear models
# via regular Fisher scoring (stats::glm)
# via combinatorial expectation maximization (logbin::logbin and addreg::addreg)
# via logit link--the logistic model just for comparison
#' @import stats

# (2) Binomial
estimate_glm <- function(formula, data, link, start = NULL, start_type = "",
                         ...) {
  fit <- stats::glm(formula,
                    data = data,
                    family = stats::binomial(link = link),
                    start = start, ...)
  if(!is.null(start))
    class(fit) <- c(paste0("glm_start", start_type), class(fit))
  fit <- estimate_maxprob(fit = fit, formula = formula, data = data,
                          link = link, start = start, start_type = start_type)
  return(fit)
}

# (3r) Log binomial via logbin::
estimate_logbin <- function(formula, data, start = NULL, start_type = "",
                            method = "cem", ...) {
  fit <- logbin::logbin(formula = formula, data = data,
                        method = method, start = start, ...)
  return(estimate_maxprob(fit = fit, formula = formula, data = data, link = "log",
                          start = start, start_type = start_type))
}

# (3d) Additive binomial via addreg::
estimate_addreg <- function(formula, data, start = NULL, start_type = "",
                            method = "cem", ...) {
  fit <- addreg::addreg(formula = formula, data = data,
                        family = stats::binomial(link = "identity"),
                        method = method, start = start, ...)
  return(estimate_maxprob(fit, formula = formula, data = data, link = "identity",
                          start = start, start_type = start_type))
}

# (5) Logistic model, for comparison only
estimate_logistic <- function(formula, data, ...) {
  fit <- stats::glm(formula, data = data,
                    family = stats::binomial(link = "logit"),
                    ...)
  class(fit) <- c("logistic", class(fit))
  fit <- estimate_maxprob(fit = fit, formula = formula,
                          data = data, link = "logit")
  return(fit)
}


#' Helper function for logbin and addreg packages
#'
#' This is a 1:1 copy of a function from logbin that is needed but not exported
#' by logbin and addreg.
#'
#' @param theta1 theta1
#' @param theta2 theta2
#' @param epsilon epsilon
#'
#' @return Logical.
#' @export
#' @keywords internal
conv.test <- function (theta1, theta2, epsilon)
{
  diffvec <- theta1 - theta2
  diff <- sqrt(sum(diffvec^2)) / sqrt(sum(theta1^2))
  (diff < epsilon)
}
