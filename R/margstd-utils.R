# Helper functions for marginal standardization after
# fitting a logistic regression model

#' @import stats tidyverse boot
#' @importFrom magrittr %>%
#' @importFrom rlang .data


# Fit logistic model, obtained marginal predictions, generate contrasts
eststd <- function(x, data, predictor, levels, estimate) {
  mdl <- glm(formula = x$formula, family = binomial(link = "logit"), data = data)
  results <- tibble::tibble(term = levels) %>%
    dplyr::mutate(data = purrr::map(.x = term,
                                    .f = ~dplyr::mutate(.data = data,
                                                        !!!predictor := .x)),
                  response = purrr::map(.x = data,
                                        .f = ~stats::predict(object = mdl,
                                                             newdata = .x,
                                                             type = "response")),
                  means = purrr::map_dbl(.x = response, .f = mean))
  if(estimate == "rr")
    res <- log(results$means / results$means[1])
  if(estimate == "rd")
    res <- results$means - results$means[1]
  names(res) <- paste0(predictor, results$term)
  return(res)
}

# Main function for marginal standardization
estimate_margstd <- function(
  formula,
  data,
  estimate   = c("rr", "rd"),
  variable   = NULL,    # where to standardize;
  # default: 1st binary var/categorical var/numeric var w/2 levels
  at         = NULL) {  # level of variable to standardize at)
  fit <- stats::glm(formula = formula, family = binomial(link = "logit"), data = data)

  # find variable to standardize over
  if(!is.null(variable)) {
    if(!(variable %in% names(fit$model)[2:length(names(fit$model))]))
      stop(paste0("Variable '", variable, "' is not part of the model."))
    if(!(class(fit$model[[variable]]) %in% c("character", "factor", "logical")))
      if(length(unique(fit$model[[variable]])) > 2 & is.null(at))
        stop(paste0("Variable '", variable, "' is not a factor, logical, character, ",
                    "or numeric with 2 levels, ",
                    "and no values to standardize at are given via 'at ='."))
    predictor <- variable
  } else {
    model_vars <- tibble::tibble(vars = names(fit$model)) %>%
      dplyr::mutate(type    = purrr::map_chr(.x = .data$vars,
                                             .f = ~class(fit$model %>% dplyr::pull(.x))),
                    nlevels = purrr::map_int(.x = .data$vars,
                                             .f = ~length(unique(fit$model %>% dplyr::pull(.x))))) %>%
      dplyr::slice(-1) %>%
      dplyr::filter(type %in% c("character", "factor", "logical") |
               (type == "numeric" & nlevels == 2)) %>%
      dplyr::slice(1)

    if(nrow(model_vars) > 0)
      predictor <- model_vars$vars
    else
      stop(paste("No exposure variable identified that is a factor, logical, character,",
                 "or numeric with 2 levels."))
  }

  # find levels to standardize at
  if(!is.null(at)) {
    if(length(at) < 2)
      stop("Because 'at' has less than 2 levels, contrasts cannot be estimated.")
    if(class(fit$model %>% dplyr::pull(predictor)) %in% c("character", "factor", "logical") &
       sum(at %in% unique(fit$model %>% dplyr::pull(predictor))) != length(at))
      stop(paste0("Some of the levels, specificied via 'at =', ",
                  "of the non-numeric variable '",
                  predictor,
                  "' were not found in the model data."))
    if(class(fit$model %>% dplyr::pull(predictor)) == "numeric")
      if(min(fit$model %>% dplyr::pull(predictor)) > min(at) |
         max(fit$model %>% dplyr::pull(predictor)) < max(at))
        warning(paste0("Numeric levels provided via 'at =' will lead to out-of-range predictions ",
                       "for the variable '", predictor, "'."))
    levels <- at
  } else {
    if(!is.null(fit$xlevels))
      if(predictor %in% names(fit$xlevels))  # retain level ordering as in model
        levels <- fit$xlevels[[predictor]]
      else
        levels <- unique(fit$model %>% dplyr::pull(predictor))
      else  # if "hidden" categorical, use level orderings as in data
        levels <- unique(fit$model %>% dplyr::pull(predictor))
  }

  eststd <- eststd(x = fit, data = data, predictor = predictor, levels = levels,
                   estimate = estimate[1])

  newfit <- list(coefficients      = eststd,
                 estimate          = estimate[1],
                 margstd_predictor = predictor,
                 margstd_levels    = levels,
                 rank              = 1)
  newfit <- append(newfit, fit[c("residuals", "fitted.values",
                                 "family", "deviance", "aic", "null.deviance",
                                 "iter", "df.residual", "df.null", "y", "converged",
                                 "boundary", "model", "call", "formula", "terms",
                                 "data", "offset", "control", "method", "xlevels", "qr")])
  class(newfit) <- c("margstd", "glm", "lm")
  newfit <- estimate_maxprob(newfit)
  return(newfit)
}

# Bootstrap model fitting
boot_eststd <- function(object, bootrepeats) {
  bootfn <- function(data, index, fit, ...) {
    eststd(x = fit, data = data[index, ], ...)
  }

  boot::boot(data = object$data,
             statistic = bootfn,
             R = bootrepeats,
             fit = glm(formula = object$formula, family = binomial(link = "logit"),
                       data = object$data),
             predictor = object$margstd_predictor,
             levels = object$margstd_levels,
             estimate = object$estimate[1])
}

# Helper function for BCa bootstrap confidence intervals
bcaci <- function(boot.out, conf, parameters) {
  getbcaci <- function(boot.out, index, conf) {
    mybca <- boot::boot.ci(boot.out = boot.out, index = index, type = "bca", conf = conf)$bca
    res <- as.numeric(mybca[1, 4:5])
    names(res) <- c("conf.low", "conf.high")
    return(res)
  }

  dplyr::bind_rows(
    c(conf.low = NA, conf.high = NA),
    purrr::map_dfr(.x = 2:parameters,
                   .f = getbcaci, boot.out = boot.out, conf = conf))
}

#' Bias-corrected/accelerated bootstrap confidence intervals
#'
#' For models fit using marginal standardization
#'
#' @param object Model fitted through marginal standardization
#' @param parm Not used, for compatibility
#' @param level Confidence level, defaults to 0.95.
#' @param bootrepeats Bootstrap repeats. Defaults to 200. Strongly recommend >1000.
#' @param ... Not used
#'
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @export
confint.margstd <- function(object, parm = NULL,
                            level = 0.95,
                            bootrepeats = 200, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste0(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  ci <- array(NA, dim = c(length(pnames), 2L), dimnames = list(pnames, pct))

  myboot <- boot_eststd(object = object, bootrepeats = bootrepeats)

  ci[] <- bcaci(boot.out = myboot, conf = level, parameters = length(pnames)) %>%
    as.matrix()

  return(ci)
}

# Bootstrapped standard errors, required by summary.margstd()
margstd_stderror <- function(object, level = 0.95, bootreps, ...) {
  myboot <- boot_eststd(object = object, bootrepeats = bootreps)

  tibble::tibble(estimate = coef(object),
                 std.error = base::apply(myboot$t, MARGIN = 2, FUN = sd)) %>%
    dplyr::bind_cols(bcaci(boot.out = myboot, conf = level, parameters = length(coef(object))))
}

#' Summary for models using marginal standardization
#'
#' @param object Model
#' @param dispersion Not used
#' @param correlation Not used
#' @param symbolic.cor Not used
#' @param level Confidence level, defaults to 95%
#' @param bootrepeats Bootstrap repeats for standard errors. Defaults to 200. Strongly recommend >1000.
#' @param ... Not used
#'
#' @return Model summary (list)
#' @return
#' @export
summary.margstd <- function(object, dispersion = NULL,
                            correlation = FALSE, symbolic.cor = FALSE,
                            level = 0.95, bootrepeats = 200, ...) {
  est.disp <- FALSE
  df.r <- object$df.residual
  if (is.null(dispersion))
    dispersion <- if (object$family$family %in% c("poisson", "binomial"))
      1
  else if (df.r > 0) {
    est.disp <- TRUE
    if (any(object$weights == 0))
      warning("observations with zero weight not used for calculating dispersion")
    sum((object$weights * object$residuals^2)[object$weights > 0])/df.r
  }
  else {
    est.disp <- TRUE
    NaN
  }
  aliased <- is.na(coef(object))
  p <- object$rank
  if (p > 0) {
    p1 <- 1L:p
    Qr <- object$qr
    if(is.null(Qr))
      stop("lm object does not have a proper 'qr' component.\n Rank zero or should not have used lm(.., qr=FALSE).")
    #coef.p <- object$coefficients[Qr$pivot[p1]]
    coef.p <- object$coefficients
    stderror <- margstd_stderror(object = object, level = level,
                                 bootreps = bootrepeats, ...)
    s.err <- stderror$std.error
    tvalue <- coef.p/s.err
    dn <- c("Estimate", "Std. Error")
    if (!est.disp) {
      pvalue <- 2 * pnorm(-abs(tvalue))
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "z value", "Pr(>|z|)"))
    }
    else if (df.r > 0) {
      pvalue <- 2 * pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    }
    else {
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(names(coef.p), c(dn, "t value", "Pr(>|t|)"))
    }
    df.f <- NCOL(Qr$qr)
  }
  else {
    coef.table <- matrix(, 0L, 4L)
    dimnames(coef.table) <- list(NULL, c("Estimate", "Std. Error",
                                         "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix(, 0L, 0L)
    df.f <- length(aliased)
  }
  keep <- match(c("call", "terms", "family", "deviance", "aic",
                  "contrasts", "df.residual", "null.deviance", "df.null",
                  "iter", "na.action"), names(object), 0L)
  ans <- c(object[keep],
           list(#deviance.resid = residuals(object, type = "deviance"),
             coefficients = coef.table, aliased = aliased,
             dispersion = dispersion, df = c(object$rank, df.r, df.f),
             conf.int = stderror, level = level))
  class(ans) <- "summary.glm"
  return(ans)
}
