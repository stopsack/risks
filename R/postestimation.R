# Postestimation methods for 'risks' models
#
# print(), summary(), summary.print(), and tidy()

#' @import stats tidyverse
#' @importFrom rlang .data

norowname <- function(x) {
  rownames(x) <- NULL
  x
}

# Helper function for tidy.risks()
risks_process_lm <- function(ret, x, conf.int = FALSE, conf.level = 0.95,
                             bootreps = 100,
                             exponentiate = FALSE,
                             default = TRUE, ...) {
  # This is broom:::process_lm(), but defaults to calling confint.default(),
  # i.e., normality-based confidence intervals,
  # instead of profile likelihood-based confidence intervals from confint.glm(),
  # where profiling fails in difficult cases for the binomial model.
  # In addition, for the margstd model, use bootstrapped CIs.
  if (exponentiate) {
    if (is.null(x$family) || (x$family$link != "logit" &&
                              x$family$link != "log")) {
      warning(paste("Exponentiating coefficients, but model did not use",
                    "a log or logit link function."))
    }
    trans <- exp
  }
  else {
    trans <- identity
  }
  if (conf.int) {
    # Start change here
    # use robust CIs for the robpoisson model
    if("robpoisson" %in% class(x))
      CI <- suppressMessages(confint.robpoisson(x, level = conf.level, ...))
    # use bootstrapped CIs for the marginally standardized model
    if(("margstd" %in% class(x)))
      CI <- suppressMessages(confint.margstd(x, level = conf.level,
                                             bootrepeats = bootreps, ...))
    # Use normality-based confidence intervals in general:
    if(default == TRUE & !("margstd" %in% class(x)) & !("robpoisson" %in% class(x)))
      CI <- suppressMessages(stats::confint.default(x, level = conf.level))
    # profile-likelihood based confidence intervals; this may fail:
    if(default == FALSE & !("margstd" %in% class(x)) & !("robpoisson" %in% class(x)))
      CI <- suppressMessages(stats::confint(x, level = conf.level))
    p <- x$rank
    if (!is.null(p) && !is.null(x$qr) &
        !("margstd" %in% class(x)) &
        !("robpoisson" %in% class(x))) {
      # End change
      piv <- x$qr$pivot[seq_len(p)]
      CI <- CI[piv, , drop = FALSE]
    }
    colnames(CI) <- c("conf.low", "conf.high")
    ret <- cbind(ret, trans(norowname(CI)))  # instead of :::unrowname
  }
  ret$estimate <- trans(ret$estimate)
  tibble::as_tibble(ret)
}

#' Tidy model summaries for risks models
#'
#' Obtain a tibble (data frame) with parameters,
#' coefficients, standard errors, confidence limits,
#' and p-values. Usual disclaimers about the use of the latter.
#' A column with the type of model fitted is also being added.
#'
#' If running \code{tidy(...)} on a model using marginal
#' standardization via \code{approach = "margstd"},
#' by default, the number of bootstrap repeats is very low.
#' It is strongly recommended to increase them to >1000. See examples.
#'
#' If multiple types of models are fitted, \code{tidy()} can be used
#' to parameters for all models at once, in one tibble. The last
#' column of the tibble includes the name of the model. See examples.
#'
#' @import broom
#'
#' @param x Model
#' @param conf.int Show confidence intervals?
#' @param conf.level Confidence level
#' @param bootrepeats Bootstrap repeats. >1000 recommended
#' @param exponentiate Exponentiate? For log links
#' @param default Use normality-based confidence intervals?
#' @param ... Passed on
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Define example data
#' library(tibble)  # for tibble() function
#' library(broom)   # required for tidy.risks()
#' dat <- tibble(
#'   id = 1:192,
#'   death = c(rep(1, 54), rep(0, 138)),
#'   stage = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
#'             rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
#'   receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
#'                rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
#'                rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))
#' # Fit and tidy the model
#' fit_rr <- estimate_risk(formula = death ~ stage + receptor, data = dat)
#' tidy(fit_rr)
#'
#' # Marginal standardization,
#' # increase number of bootstrap repeats:
#' fit_rr <- estimate_risk(formula = death ~ stage + receptor, data = dat, approach = "margstd")
#' tidy(fit_rr, bootrepeats = 250)
#'
#' # Multiple types of models fitted:
#' fit_rr <- estimate_risk(formula = death ~ stage + receptor, data = dat, approach = "all")
#' tidy(fit_rr)
tidy.risks <- function(
  x,
  conf.int     = TRUE,
  conf.level   = 0.95,
  bootrepeats  = 100,
  exponentiate = FALSE,
  default      = TRUE,
  ...) {
  tidysummarylm <- get("tidy.summary.lm", envir = asNamespace("broom"), inherits = FALSE)

  if(is.null(purrr::pluck(x, "all_models"))) {
    #ret <- broom:::tidy.summary.lm(summary(x))
    ret <- tidysummarylm(summary(x))
    risks_process_lm(ret, x, conf.int = conf.int, conf.level = conf.level,
                     bootreps = bootrepeats,
                     exponentiate = exponentiate, default = default, ...) %>%
      dplyr::mutate(model = paste0(class(x)[2], x$risks_start))
  } else {  # in case estimate_risk(approach = "all") was called
    purrr::map_dfr(
      .x = purrr::pluck(x, "all_models"),
      .f = ~{
        if((purrr::pluck(.x, "converged") == TRUE) & (purrr::pluck(.x, "boundary") == FALSE)) {
          tryCatch({
            ret <- tidysummarylm(summary(.x))
            risks_process_lm(ret, .x, conf.int = conf.int, conf.level = conf.level,
                             bootreps = bootrepeats,
                             exponentiate = exponentiate, default = default, ...) %>%
              dplyr::mutate(model = paste0(class(.x)[2], .x$risks_start))
          },
          error   = function(x) { tibble() })
        }
      })
  }
}

#' Print model
#'
#' @param x Fitted model
#' @param ... Passed to print.glm()
#'
#' @export
print.risks <- function(x, ...) {
  if(!is.null(x$estimate))
    estimate <- x$estimate
  else
    estimate <- ""
  cat(paste("\nRisk",
            dplyr::if_else(x$family$link == "identity" | estimate == "rd",
                    true = "difference", false = "ratio"),
            "model"))
  #stats:::print.glm(x, ...)
  print_glm <- get("print.glm", envir = asNamespace("stats"), inherits = FALSE)
  print_glm(x, ...)
}

#' Generate model summary
#'
#' Determine type of "risks" model fitted and
#' generate appropriate summary.
#'
#' @param object Fitted model
#' @param ... Passed on
#'
#' @export
summary.risks <- function(object, ...) {
  # Exception: Multiple models were fitted (approach = "all") but the Poisson model failed
  # Retrieve the first converged model to make sure summary() does not fail
  if(object$converged == FALSE & !is.null(object$all_models)) {
    all_models <- object$all_models
    converged <- min(which(purrr::map(object$all_models, .f = ~purrr::pluck(.x, "converged")) == TRUE))
    if(is.null(converged))
      stop("No summary: No model converged")
    object <- object$all_models[[converged]]
    object$all_models <- all_models
  }

  # Call regular summary.*()
  mysummary <- switch(
    EXPR = class(object)[2],
    addreg = addreg::summary.addreg(object, ...),
    margstd = summary.margstd(object, ...),
    summary.glm(object, ...))
  myclass <- class(mysummary)

  if(!is.null(object$estimate))
    estimate <- object$estimate
  else
    estimate <- ""
  modeltype <- paste0(class(object)[2], object$risks_start)
  # Type of model that was returned
  modeltypes <- c(
    "robpoisson" = "as Poisson model with robust covariance",
    "glm"        = "as binomial model",
    "glm_start"  = "as binomial model with starting values from Poisson model",
    "addreg"     = "as binomial model with combinatorial expectation maximization",
    "addreg_start" = "as binomial model with combinatorial expectation maximization, starting values",
    "logbin"       = "as binomial model with combinatorial expectation maximization",
    "logbin_start" = "as binomial model with combinatorial expectation maximization, starting values",
    "margstd"      = "via marginal standardization of a logistic model")
  modeldescr <- paste0("\nRisk ",
                       dplyr::if_else(object$family$link == "identity" | estimate == "rd",
                               true = "difference", false = "ratio"),
                       " model, fitted ",
                       modeltypes[modeltype], " (", modeltype, ").")

  # Transfer additional elements from 'object' fit for use in print.summary.risks()
  mysummary <- append(mysummary, list(modeldescr = modeldescr,
                                      modeltype  = class(object)[2],
                                      object     = object))
  if(!is.null(purrr::pluck(object, "all_models")))
    mysummary <- append(mysummary, list(all_models = purrr::pluck(object, "all_models")))

  # Return a class of 'summary.risks' for calling of print.summary.risks()
  class(mysummary) <- c("summary.risks", myclass)
  return(mysummary)
}

#' Print "risks" model summary
#'
#' Print summaries for "risks" models. The printout is the same as
#' for regular summaries of generalized linear models fit via
#' \code{stats::glm()}, except that the type of "risks" model
#' is printed first (e.g., "Poisson model with robust covariance")
#' and confidence intervals for model parameters are printed at the end.
#' By default, normality-based confidence intervals are calculated.
#' By setting \code{default = FALSE}, profile likelihood-based
#' confidence intervals can be calculated for binomial and Poisson models.
#'
#' @param x Model
#' @param conf.int Add confidence intervals to printout?
#' @param default Normal confidence intervals via confint.default()?
#' @param ... Passed on
#'
#' @export
print.summary.risks <- function(
  x,
  conf.int = TRUE,
  default  = TRUE,
  ...) {

  # If estimate_risk(approach = "all") was called:
  if(!is.null(purrr::pluck(x, "all_models"))) {
    cat("\nAll fitted models:\n")
    toprint <- purrr::pluck(x, "all_models") %>%
      purrr::map_dfr(.f = ~tibble(
        Model       = paste0(class(.x)[2], purrr::pluck(.x, "risks_start")),
        Converged   = purrr::pluck(.x, "converged"),
        `Max.prob.` = purrr::pluck(.x, "maxprob"))) %>%
      as.data.frame()
    print(toprint)
    cat(paste0("Access these models via '", deparse(substitute(x)), "$all_models'.\n"))
  }

  # Print type of final model
  cat(x$modeldescr)

  # copy unexported function
  printsummaryglm <- get("print.summary.glm", envir = asNamespace("stats"), inherits = FALSE)

  # Call regular print.summary.*()
  if("addreg" %in% class(x))
    addreg::print.summary.addreg(x, ...)
  else
    printsummaryglm(x, ...)

  # Print confidence intervals
  if(conf.int == TRUE &   # addreg and logbin use standard CIs
     (default == TRUE | "addreg" %in% class(x$object) | "logbin" %in% class(x$object)) &
     !("margstd" %in% class(x$object))) {
    cat("Confidence intervals for coefficients (normality-based):\n")
    print(confint.default(x$object, ...))
  }
  if(conf.int == TRUE &
     default == FALSE &
     sum(c("margstd", "addreg", "logbin") %in% class(x$object)) == 0) {
    cat("Confidence intervals for coefficients (profiling-based):\n")
    print(confint(x$object, ...))
  }
  if(conf.int == TRUE & "margstd" %in% class(x$object)) {
    cat("Confidence intervals for coefficients (bootstrap-based):\n")
    ci <- x$conf.int %>%
      dplyr::select(.data$conf.low, .data$conf.high) %>%
      as.matrix()
    a <- (1 - x$level)/2
    a <- c(a, 1 - a)
    colnames(ci) <- paste0(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    rownames(ci) <- x$conf.int %>% dplyr::pull("term")
    print(ci)
  }
}
