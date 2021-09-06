# Helper functions for estimate_risk()

# Fitting functions for individual models
# Estimate maximum predicted probability, ensure later it is < 1
estimate_maxprob <- function(fit, formula, data, link, start = NULL) {
  implausible <- 0.99999
  # logbin and addreg predicted probablities do not exactly reach 1
  # in case they converge on implausible values
  fit$maxprob <- maxprob <- max(predict(fit, type = "response"))
  if(maxprob > implausible)
    message(paste0("Implausible predicted probability >", implausible,
                   " occurred: ", maxprob))
  if(!is.null(start) & fit$method != "glm.fit")
    fit$risks_start <- "_start"
  else
    fit$risks_start <- ""
  fit$call$formula <- formula
  fit$call$family$link <- link
  #fit$call$data <- substitute(data)

  # get name to dataset provided to the main frontend function
  calls_list <- sys.calls()
  goback <- (length(calls_list) - which(grepl("riskratio|riskdiff|estimate_risk",
                                              calls_list))) * -1
  fit$call$data <- tryCatch({ match.call(definition = sys.function(which = goback),
                                         call = sys.call(which = goback))$data },
                            error = function(e) "data")
  # end
  fit$call$start <- dplyr::if_else(!is.null(start),
                           true  = "(from Poisson model)",
                           false = "(no starting values)")
  class(fit) <- c("risks", class(fit))
  return(fit)
}

# Extended purrr::possibly() that allows 'otherwise =' to be a function
# based on https://github.com/tidyverse/purrr/issues/640
ext_possibly <- function(.f, otherwise, quiet = TRUE) {
  .f <- purrr::as_mapper(.f)
  force(otherwise)
  function(...) {
    tryCatch(.f(...), error = function(e) {
      if (!quiet)
        message("Error: ", e$message)
      otherwise
    }, interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    })
  }
}

# Return values if model fitting failed.
# Still assign the class/type of the model that was meant to be fitted.
return_failure <- function(family, classname) {
  ret <- list(family = family,
              converged = FALSE, boundary = FALSE, maxprob = NA_real_)
  class(ret) <- c("risks", classname, "glm", "lm")
  return(ret)
}

# Exception handlers
possibly_estimate_poisson <- ext_possibly(
  .f = estimate_poisson,
  otherwise = return_failure(family = list(family = "poisson"),
                             classname = "robpoisson"))

possibly_estimate_duplicate <- ext_possibly(
  .f = estimate_duplicate,
  otherwise = return_failure(family = list(family = "binomial"),
                             classname = "duplicate"))

possibly_estimate_glm <- ext_possibly(
  .f = estimate_glm,
  otherwise = return_failure(family = list(family = "binomial"),
                             classname = NULL))

possibly_estimate_logbin <- ext_possibly(
  .f = estimate_logbin,
  otherwise = return_failure(family = list(family = "binomial", link = "log"),
                             classname = "logbin"))

possibly_estimate_addreg <- ext_possibly(
  .f = estimate_addreg,
  otherwise = return_failure(family = list(family = "binomial", link = "identity"),
                             classname = "addreg"))

possibly_estimate_logistic <- ext_possibly(
  .f = estimate_logistic,
  otherwise = return_failure(family = list(family = "binomial", link = "logit"),
                             classname = "logistic"))

possibly_estimate_margstd <- ext_possibly(
  .f = estimate_margstd,
  otherwise = return_failure(family = list(family = "binomial", link = "logit"),
                             classname = "margstd"))
