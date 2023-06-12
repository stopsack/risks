# Postestimation methods for 'risks' models
#
# print(), summary(), summary.print(), and tidy()

#' @import stats
#' @importFrom rlang .data

# Helper function for tidy.risks()
risks_process_lm <- function(ret, x, conf.int = FALSE, conf.level = 0.95,
                             bootreps, bootci,
                             bootverbose,
                             exponentiate = FALSE,
                             default = TRUE,
                             confint_from_summary = NULL,
                             ...) {
  # This is broom:::process_lm(), but defaults to calling confint.default(),
  # i.e., normality-based confidence intervals,
  # instead of profile likelihood-based confidence intervals from confint.glm(),
  # where profiling fails in difficult cases for the binomial model.
  # In addition, for the margstd model, use bootstrapped or delta method CIs.
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
    # Use cluster-robust CIs for case duplication model
    if(inherits(x = x, what = "duplicate"))
      CI <- suppressMessages(confint.duplicate(x, level = conf.level, ...))
    # Use robust CIs for the robpoisson model
    if(inherits(x = x, what = "robpoisson"))
      CI <- suppressMessages(confint.robpoisson(x, level = conf.level, ...))
    # Use delta method CIs for margstd_delta
    if(inherits(x = x, what = "margstd_delta"))
      CI <- suppressMessages(confint.margstd_delta(x, level = conf.level, ...))
    # Non-BCa CIs do not have estimates of jackknife standard errors
    bca_jacksd <- tibble::tibble(jacksd.low = NA, jacksd.high = NA)
    if(inherits(x = x, what = "margstd_boot")) {
      if(is.null(confint_from_summary)) {
        # Used only with tidy() calls after approach = "all"
        CI <- suppressMessages(confint.margstd_boot(x, level = conf.level,
                                                    bootrepeats = bootreps,
                                                    bootci = bootci,
                                                    jacksd = TRUE,
                                                    ...))
        if("jacksd.low" %in% dimnames(CI)[[2]]) {
          bca_jacksd <- CI[, c("jacksd.low", "jacksd.high")] %>%
            tibble::as_tibble()
        }
        CI <- CI[, 1:2]
      } else {
        # Usually, take existing bootstrap CIs from summary.margstd_boot()
        if("jacksd.low" %in% names(confint_from_summary)) {
          bca_jacksd <- confint_from_summary[, c("jacksd.low", "jacksd.high")]
        }
        CI <- confint_from_summary[, c("conf.low", "conf.high")]
      }
    }
    # Use normality-based confidence intervals in general:
    if(default == TRUE &
       !inherits(
         x = x,
         what = c(
           "margstd_boot",
           "margstd_delta",
           "robpoisson",
           "duplicate")))
      CI <- suppressMessages(stats::confint.default(x, level = conf.level))
    # profile-likelihood based confidence intervals; this may fail:
    if(default == FALSE &
       !inherits(
         x = x,
         what = c(
           "margstd_boot",
           "margstd_delta",
           "robpoisson",
           "duplicate")))
      CI <- suppressMessages(stats::confint(x, level = conf.level))
    p <- x$rank
    if(!is.null(p) && !is.null(x$qr) &
       !inherits(
         x = x,
         what = c(
           "margstd_boot",
           "margstd_delta",
           "robpoisson",
           "duplicate"))) {
      piv <- x$qr$pivot[seq_len(p)]
      CI <- CI[piv, , drop = FALSE]
    }
    if(inherits(x = CI, what = "numeric", which = TRUE) == 1)
      CI <- as.data.frame(t(CI))
    colnames(CI) <- c("conf.low", "conf.high")
    rownames(CI) <- NULL
    ret <- cbind(ret, trans(CI))
  }
  ret$estimate <- trans(ret$estimate)
  if(bootverbose == TRUE) {
    if(!inherits(x = x, what = "margstd_boot")) {
      bootreps <- NA_real_
      bootci <- NA_character_
    }
    ret <- dplyr::mutate(ret,
                         bootrepeats = bootreps,
                         bootci = bootci[1],
                         jacksd.low = bca_jacksd$jacksd.low,
                         jacksd.high = bca_jacksd$jacksd.high)
  }
  tibble::as_tibble(ret)
}

#' Tidy model summaries for risks models
#'
#' Obtain a tibble (data frame) with parameters,
#' coefficients, standard errors, confidence limits,
#' and p-values. A column with the type of model fitted is added.
#'
#' If multiple types of models are fitted, \code{tidy()} can be used
#' to parameters for all models at once, in one tibble. The last
#' column of the tibble includes the name of the model. See examples.
#'
#' @import broom
#'
#' @param x Model
#' @param conf.int Show confidence intervals?
#' @param conf.level Optional. Confidence level. Defaults to \code{0.95}.
#' @param bootrepeats Optional. Number of bootstrap repeats.
#'   Applicable to models fitted via marginal standardization and bootstrapping
#'   (\code{approach = "margstd_boot"}). Defaults to 1000. Strongly recommended
#'   to increase repeats to >>1000.
#' @param bootci Optional and applicable for \code{approach = "margstd_boot"}
#'   only. Type of bootstrap confidence interval:
#'
#'   * \code{"bca"} Default. Parametric BCa (bias-corrected accelerated)
#'     confidence intervals.
#'   * \code{"normal"} Parametric normality-based confidence intervals,
#'     which require lower repeat numbers but are less accurate and
#'     may result in invalid results for ratios.
#'   * \code{"nonpar"} Non-parametric BCa confidence intervals,
#'     which should be used with caution because of the risk
#'     of sparse-data bias with non-parametric bootstrapping.
#' @param bootverbose Optional. Add values of \code{bootrepeats} and
#'   \code{bootci} parameters and the jackknife-based Monte-Carlo error for the
#'   confidence limits (only for \code{type = "bca"}) to the returned tibble?
#'   Defaults to \code{FALSE}.
#' @param exponentiate Optional. Exponentiate coefficients and confidence
#'   limits? Defaults to FALSE. Setting \code{exponentiate = TRUE} is useful for
#'   relative risk models (log links).
#' @param default Use default, normality-based confidence intervals?
#'   Defaults to TRUE. With \code{default = FALSE}, for binomial models only,
#'   profile likelihood-based confidence intervals can be calculated.

#' @param ... Passed on
#'
#' @return tibble
#' @export
#'
#' @examples
#' # Define example data
#' library(broom)  # provides tidy() function
#' dat <- tibble::tibble(
#'   death    = c(rep(1, 54), rep(0, 138)),
#'   stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
#'                rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
#'   receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
#'                rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
#'                rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))
#' # Fit and tidy the model
#' fit_rr <- riskratio(formula = death ~ stage + receptor, data = dat)
#' tidy(fit_rr)
#'
#' # Marginal standardization,
#' # increase number of bootstrap repeats:
#' \donttest{
#' fit_rr <- riskratio(
#'   formula = death ~ stage + receptor, data = dat,
#'   approach = "margstd_boot")
#' tidy(fit_rr, bootrepeats = 2000)
#' }
#'
#' # Multiple types of models fitted:
#' \donttest{
#' fit_rr <- riskratio(formula = death ~ stage + receptor, data = dat,
#'                     approach = "all")
#' tidy(fit_rr)
#' }
tidy.risks <- function(
  x,
  conf.int     = TRUE,
  conf.level   = 0.95,
  bootrepeats  = 1000,
  bootci = c("bca", "normal", "nonpar"),
  bootverbose  = FALSE,
  exponentiate = FALSE,
  default      = TRUE,
  ...) {
  bootci <- match.arg(bootci)
  getmodel <- function(x, ...) {
    # new as of broom 0.7.0 with the removal of broom:::tidy.summary.lm():
    model_summary <- summary(x, ...)
    ret <- tibble::as_tibble(model_summary$coefficients, rownames = "term")
    colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
    coefs <- tibble::enframe(stats::coef(x), name = "term", value = "estimate")
    list(coefs = dplyr::left_join(coefs, ret, by = c("term", "estimate")),
         confint = model_summary$conf.int)
  }

  if(is.null(purrr::pluck(x, "all_models"))) {
    ret <- getmodel(x,
                    level = conf.level,
                    bootrepeats = bootrepeats,
                    bootci = match.arg(bootci))
    risks_process_lm(ret = ret$coefs,
                     x = x,
                     conf.int = conf.int,
                     conf.level = conf.level,
                     bootreps = bootrepeats,
                     bootci = bootci,
                     bootverbose = bootverbose,
                     exponentiate = exponentiate,
                     default = default,
                     confint_from_summary = ret$confint,
                     ...) %>%
      dplyr::mutate(model = paste0(class(x)[2], x$risks_start))
  } else {  # in case estimate_risk(approach = "all") was called
    purrr::map_dfr(
      .x = purrr::pluck(x, "all_models"),
      .f = ~{
        if((purrr::pluck(.x, "converged") == TRUE) &
           (purrr::pluck(.x, "boundary") == FALSE)) {
          tryCatch({
            ret <- getmodel(.x)$coefs
            risks_process_lm(ret, .x,
                             conf.int = conf.int,
                             conf.level = conf.level,
                             bootreps = bootrepeats,
                             bootci = bootci,
                             bootverbose = bootverbose,
                             exponentiate = exponentiate,
                             default = default,
                             confint_from_summary = NULL,
                             ...) %>%
              dplyr::mutate(model = paste0(class(.x)[2], .x$risks_start))
          },
          error   = function(x) { tibble() })
        }
      })
  }
}

#' Print model
#'
#' Print fitted risks model. The only change, compared to \code{print.glm()},
#' is the addition of the main type of model: relative risk or risk difference.
#' If multiple models were fitted via \code{approach = "all"}, then the
#' first converged model will be printed.
#'
#' @param x Fitted model
#' @param ... Passed to print.glm()
#'
#' @return No return value, called for printing
#' @export
print.risks <- function(x, ...) {
  if(x$converged == FALSE & !is.null(x$all_models)) {
    converged <- purrr::map_lgl(.x = x$all_models,
                                .f = ~purrr::pluck(.x, "converged"))
    if(sum(converged) == 0)
      stop("No model converged")
    x <- x$all_models[[min(which(converged == TRUE))]]
  }

  if(!is.null(x$estimand))
    estimand <- x$estimand
  else
    estimand <- ""
  cat(paste("\nRisk",
            dplyr::if_else(x$family$link == "identity" | estimand == "rd",
                           true = "difference",
                           false = "ratio"),
            "model"))
  class(x) <- "glm"
  print(x, ...)
}

#' Generate model summary
#'
#' Determine type of risks model fitted and
#' generate appropriate summary.
#'
#' If multiple models were fitted (\code{approach = "all"}), then
#' the first converged model is displayed. Other models can be accessed
#' via the returned list \code{$all_models}.
#'
#' @param object Fitted model
#' @param conf.int Add confidence intervals to printout? Defaults to TRUE.
#' @param default Normal confidence intervals via confint.default()?
#'   Default to TRUE. By setting \code{default = FALSE}, profiling-based
#'   confidence intervals can be calculated for binomial models.
#' @param ... Passed on
#' @return Model summary (list)
#' @export
summary.risks <- function(object,
                          conf.int = TRUE,
                          default  = TRUE,
                          ...) {
  # Exception: Multiple models were fitted but the Poisson model failed
  # Retrieve the first converged model to make sure summary() does not fail
  if(object$converged == FALSE & !is.null(object$all_models)) {
    all_models <- object$all_models
    converged <- purrr::map_lgl(.x = object$all_models,
                                .f = ~purrr::pluck(.x, "converged"))
    if(sum(converged) == 0)
      stop("No model converged")
    object <- object$all_models[[min(which(converged == TRUE))]]
    object$all_models <- all_models
  }

  # Call regular summary.*()
  mysummary <- switch(
    EXPR = class(object)[2],
    addreg = addreg::summary.addreg(object, ...),
    margstd_boot = summary.margstd_boot(object, ...),
    margstd_delta = summary.margstd_delta(object, ...),
    robpoisson = summary.robpoisson(object, ...),
    duplicate = summary.duplicate(object, ...),
    summary.glm(object, ...))
  myclass <- class(mysummary)

  if(!is.null(object$estimand))
    estimand <- object$estimand
  else
    estimand <- ""
  modeltype <- paste0(class(object)[2], object$risks_start)
  # Type of model that was returned
  modeltypes <- c(
    "robpoisson" = "as Poisson model with robust covariance",
    "duplicate"  = "as logistic model with duplication of cases",
    "glm"        = "as binomial model",
    "glm_startp" = "as binomial model with starting values from Poisson model",
    "glm_startd" = "as binomial model with starting values from logistic model with case duplication",
    "addreg"     = "as binomial model with combinatorial expectation maximization",
    "addreg_startp" = "as binomial model with combinatorial expectation maximization, Poisson starting values",
    "logbin"        = "as binomial model with combinatorial expectation maximization",
    "logbin_startp" = "as binomial model with combinatorial expectation maximization, Poisson starting values",
    "margstd_boot"  = "via marginal standardization of a logistic model with bootstrapping",
    "margstd_delta"  = "via marginal standardization of a logistic model with delta method",
    "logistic"      = "as a logistic model: binomial model with logit link")
  modeldescr <- paste0("\nRisk ",
                       dplyr::if_else(object$family$link == "identity" |
                                        estimand == "rd",
                               true = "difference", false = "ratio"),
                       " model, fitted ",
                       modeltypes[modeltype], " (", modeltype, ").")

  # Transfer more elements from 'object' fit for use in print.summary.risks()
  mysummary <- append(mysummary, list(modeldescr      = modeldescr,
                                      modeltype       = class(object)[2],
                                      print_confint   = conf.int,
                                      confint_default = default,
                                      object          = object))
  if(!is.null(purrr::pluck(object, "all_models")))
    mysummary <- append(mysummary,
                        list(all_models = purrr::pluck(object, "all_models")))

  # Return a class of 'summary.risks' for calling of print.summary.risks()
  class(mysummary) <- c("summary.risks", myclass)
  return(mysummary)
}

#' Print model summary
#'
#' @description
#'
#' Print summaries for "risks" models. The printout is the same as
#' for regular summaries of generalized linear models fit via
#' \code{stats::glm()}, except that the type of "risks" model
#' is printed first (e.g., "Poisson model with robust covariance")
#' and confidence intervals for model parameters are printed at the end.
#'
#' @param x Model
#' @param ... Passed on
#'
#' @return No return value, called for printing
#' @export
print.summary.risks <- function(x, ...) {
  # If estimate_risk(approach = "all") was called:
  if(!is.null(purrr::pluck(x, "all_models"))) {
    cat("\nAll fitted models:\n")
    toprint <- purrr::pluck(x, "all_models") %>%
      purrr::map_dfr(.f = ~tibble::tibble(
        Model       = paste0(class(.x)[2], purrr::pluck(.x, "risks_start")),
        Converged   = purrr::pluck(.x, "converged"),
        `Max.prob.` = purrr::pluck(.x, "maxprob"))) %>%
      as.data.frame()
    print(toprint)
  }

  # Print type of final model
  cat(x$modeldescr)

  # Call regular print.summary.*()
  if(inherits(x = x, what = "addreg")) {
    addreg::print.summary.addreg(x, ...)
  } else {
    x_print <- x
    class(x_print) <- "summary.glm"
    print(x_print, ...)
  }

  # Print confidence intervals
  if(x$print_confint == TRUE &   # addreg and logbin use standard CIs
     (x$confint_default == TRUE |
      inherits(
        x = x$object,
        what = c("addreg", "logbin"))) &
      !inherits(
        x = x$object,
        what = c(
          "margstd_boot",
          "margstd_delta",
          "duplicate",
          "robpoisson"))) {
    cat("Confidence intervals for coefficients: (normality-based)\n")
    print(confint.default(x$object, ...))
  }
  if(x$print_confint == TRUE &
     inherits(x = x$object, what = "robpoisson")) {
    cat("Confidence intervals for coefficients: (robust)\n")
    print(confint(x$object, ...))
  }
  if(x$print_confint == TRUE &
     inherits(x = x$object, what = "duplicate")) {
    cat("Confidence intervals for coefficients: (cluster-robust)\n")
    print(confint(x$object, ...))
  }
  if(x$print_confint == TRUE &
     inherits(x = x$object, what = "margstd_delta")) {
    cat("Confidence intervals for coefficients: (delta method)\n")
    print(confint(x$object, ...))
  }
  if(x$print_confint == TRUE &
     x$confint_default == FALSE &
     !inherits(
       x = x$object,
       what = c(
         "margstd_boot",
         "margstd_delta",
         "addreg",
         "logbin"))) {
    cat("Confidence intervals for coefficients: (profiling-based)\n")
    print(confint(x$object, ...))
  }

  if(x$print_confint == TRUE &
     inherits(x = x$object, what = "margstd_boot")) {
    # retrieve CIs that were generated when bootstrapping SEs for model summary
    cat(paste0("Confidence intervals for coefficients: (",
              x$margstd_boot.bootrepeats, " ",
              c("bca" = "BCa", "normal" = "normal",
                "nonpar" = "nonparametric BCa")[x$margstd_boot.bootci],
              " bootstrap repeats)\n"))
    ci <- x$conf.int %>%
      dplyr::select("conf.low", "conf.high") %>%
      as.matrix()
    a <- (1 - x$level) / 2
    a <- c(a, 1 - a)
    ci <- array(NA, dim = c(max(length(x$object$margstd_levels), 1), 2L),
                dimnames = list(paste0(x$object$margstd_predictor,
                                       x$object$margstd_levels),
                                paste0(format(100 * a, trim = TRUE,
                                              scientific = FALSE, digits = 3),
                                       "%")))
    ci[] <- x$conf.int %>%
      dplyr::select("conf.low", "conf.high") %>%
      as.matrix()
    print(ci)
  }
}
