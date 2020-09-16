# Helper functions for estimate_risk()
#' @import stats tidyverse

# Fitting functions for individual models
# Estimate maximum predicted probability, ensure later it is < 1
estimate_maxprob <- function(fit, formula, data, link, start = NULL) {
  implausible <- 0.99999  # logbin and addreg predicted probablities do not exact reach 1
                          # in case they converge on implausible values
  fit$maxprob <- maxprob <- max(predict(fit, type = "response"))
  if(maxprob > implausible)
    message(paste("Implausible predicted probability >", implausible, "occurred:", maxprob))
  if(!is.null(start) & fit$method != "glm.fit")
    fit$risks_start <- "_start"
  else
    fit$risks_start <- ""
  fit$call$formula <- formula
  fit$call$family$link <- link
  fit$call$data <- substitute(data)
  fit$call$start <- dplyr::if_else(!is.null(start),
                           true  = "(from Poisson model)",
                           false = "(no starting values)")
  class(fit) <- c("risks", class(fit))
  return(fit)
}

# Exception handlers
possibly_estimate_poisson <- purrr::possibly(
  .f = estimate_poisson,
  otherwise = list(family = list(family = "poisson"),
                   converged = FALSE, boundary = FALSE, maxprob  = NA_real_))

possibly_estimate_glm <- purrr::possibly(
  .f = estimate_glm,
  otherwise = list(family = list(family = "binomial"),
                   converged = FALSE, boundary = FALSE, maxprob = NA_real_))

possibly_estimate_logbin <- purrr::possibly(
  .f = estimate_logbin,
  otherwise = list(family = list(family = "binomial", link = "log"),
                   converged = FALSE, boundary = FALSE, maxprob = NA_real_))

possibly_estimate_addreg <- purrr::possibly(
  .f = estimate_addreg,
  otherwise = list(family = list(family = "binomial", link = "identity"),
                   converged = FALSE, boundary = FALSE, maxprob = NA_real_))

possibly_estimate_logistic <- purrr::possibly(
  .f = estimate_logistic,
  otherwise = list(family = list(family = "binomial", link = "logit"),
                   converged = FALSE, boundary = FALSE, maxprob = NA_real_))

possibly_estimate_margstd <- purrr::possibly(
  .f = estimate_margstd,
  otherwise = list(family = list(family = "binomial", link = "logit"),
                   converged = FALSE, boundary = FALSE, maxprob = NA_real_))
