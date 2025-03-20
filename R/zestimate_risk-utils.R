# Helper functions for estimate_risk()

# Fitting functions for individual models
# Estimate maximum predicted probability, ensure later it is < 1
estimate_maxprob <- function(fit, formula, data, link,
                             start = NULL, start_type = "") {
  # GLM's predicted probabilities do not exactly reach 1
  # in case they converge on implausible values (also cf. Wacholder)
  implausible <- 0.99999

  fit$maxprob <- max(predict(object = fit, type = "response"))
  if(fit$maxprob > implausible)
    message(paste0(class(fit)[1],
                   ": Implausible predicted probability >", implausible,
                   " occurred: ", fit$maxprob))

  # Note whether starting values were in the model 'type'
  if(!is.null(start) & fit$method != "glm.fit")
    fit$risks_start <- paste0("_start", start_type)
  else
    fit$risks_start <- ""
  fit$call$formula <- formula
  fit$call$family$link <- link

  # Get name to dataset provided to the main frontend function
  calls_list <- sys.calls()
  goback <- (length(calls_list) -
               which(grepl("riskratio|riskdiff|estimate_risk",
                           calls_list))) * -1
  fit$call$data <- tryCatch({
    match.call(definition = sys.function(which = goback),
               call = sys.call(which = goback))$data },
    error = function(e) "data")
  # end
  fit$call$start <- dplyr::case_when(
    start_type == "p" ~ "(from Poisson model)",
    start_type == "d" ~ "(from case-duplication logistic model)",
    TRUE              ~ "(no starting values)")
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
  ret <- list(
    family = family,
    converged = FALSE,
    boundary = FALSE,
    maxprob = NA_real_,
    margstd_delta_interaction = FALSE)
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

possibly_estimate_glm_startp <- ext_possibly(
  .f = estimate_glm,
  otherwise = return_failure(family = list(family = "binomial"),
                             classname = "glm_startp"))

possibly_estimate_glm_startd <- ext_possibly(
  .f = estimate_glm,
  otherwise = return_failure(family = list(family = "binomial"),
                             classname = "glm_startd"))

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

possibly_estimate_margstd_boot <- ext_possibly(
  .f = estimate_margstd_boot,
  otherwise = return_failure(family = list(family = "binomial", link = "logit"),
                             classname = "margstd_boot"))

possibly_estimate_margstd_delta <- ext_possibly(
  .f = estimate_margstd_delta,
  otherwise = return_failure(family = list(family = "binomial", link = "logit"),
                             classname = "margstd_delta"))
