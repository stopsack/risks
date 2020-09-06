# Helper functions for marginal standardization after
# fitting a logistic regression model

#' @import stats tidyverse rsample
#' @importFrom magrittr %>%
#' @importFrom rlang .data


# Helper function to obtain predicted values and their contrasts
eststd <- function(x, data, predictor, levels) {
  tibble::tibble(term = levels) %>%
    dplyr::mutate(data = purrr::map(.x = term,
                             .f = ~dplyr::mutate(.data = data,
                                                 !!!predictor := .x)),
           response = purrr::map(.x = data,
                          .f = ~stats::predict(object = x, newdata = .x,
                                        type = "response"))) %>%
    dplyr::transmute(
      term  = paste0(predictor, term),
      means = purrr::map_dbl(.x = response, .f = mean),
      rd    = means - means[1],
      rr    = means / means[1])
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

  eststd <- eststd(x = fit, data = data, predictor = predictor, levels = levels)

  if(estimate[1] == "rr")
    coefs <- log(eststd$rr)
  else
    coefs <- eststd$rd
  names(coefs) <- eststd$term

  newfit <- list(coefficients = coefs,
                 estimate          = estimate[1],
                 margstd_predictor = predictor,
                 margstd_levels    = levels,
                 rank = 1)
  newfit <- append(newfit, fit[c("residuals", "fitted.values",
                                 "family", "deviance", "aic", "null.deviance",
                                 "iter", "df.residual", "df.null", "y", "converged",
                                 "boundary", "model", "call", "formula", "terms",
                                 "data", "offset", "control", "method", "xlevels", "qr")])
  class(newfit) <- c("margstd", "glm", "lm")
  newfit <- estimate_maxprob(newfit)
  return(newfit)
}

# Bootstrap the standardization procedure
eststd_boot <- function(x, reps, failsafe = TRUE) {
  set.seed(123)
  if(failsafe == TRUE) {
    res <- bootstraps(data = x$data, times = reps) %>%
      dplyr::mutate(res = purrr::map(.x = .data$splits,
                       .f = ~{
                         tryCatch({stats::glm(formula = x$formula,
                                       family  = binomial(link = "logit"),
                                       data    = analysis(.x)) %>%
                             eststd(data      = analysis(.x),
                                    predictor = x$margstd_predictor,
                                    levels    = x$margstd_levels)},
                             error = function(x) { tibble() })
                       }))
  } else {
    res <- bootstraps(data = x$data, times = reps) %>%
      dplyr::mutate(res = purrr::map(.x = .data$splits,
                       .f = ~{
                         stats::glm(formula = x$formula,
                             family  = binomial(link = "logit"),
                             data    = analysis(.x)) %>%
                           eststd(data      = analysis(.x),
                                  predictor = x$margstd_predictor,
                                  levels    = x$margstd_levels)
                       }))
  }
  res %>%
    dplyr::select(-.data$splits) %>%
    tidyr::unnest(col = res) %>%
    dplyr::group_by(.data$term)
}

# Bootstrapped CIs
confint.margstd <- function(object, parm = NULL, level = 0.95,
                            bootrepeats = 100, na.rm = TRUE, ...) {
  # modified after stats:::confint.default()
  cf <- coef(object)
  pnames <- names(cf)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste0(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  ci <- array(NA, dim = c(length(pnames), 2L), dimnames = list(pnames, pct))

  boots <- eststd_boot(x = object, reps = bootrepeats, failsafe = na.rm)

  res <- if(object$estimate == "rr") {
    boots %>%
      dplyr::summarize(log(quantile(.data$rr, probs = a[1], na.rm = na.rm)),
                       log(quantile(.data$rr, probs = a[2], na.rm = na.rm)))
  } else {
    boots %>%
      dplyr::summarize(quantile(.data$rd, probs = a[1]),
                       quantile(.data$rd, probs = a[2]))
  }
  ci[] <- res %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$term) %>%
    as.matrix()

  return(ci)
}

# Bootstrapped standard errors, required by summary.margstd()
margstd_stderror <- function(object, level = 0.95, bootreps, ...) {
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  boots <- eststd_boot(x = object, reps = bootreps)

  if(object$estimate == "rr") {
    boots %>%
      dplyr::summarize(estimate  = mean(log(.data$rr)),
                       conf.low  = log(quantile(.data$rr, probs = a[1])),
                       conf.high = log(quantile(.data$rr, probs = a[2])),
                       std.error = sqrt(var(log(.data$rr))))
  } else {
    boots %>%
      dplyr::summarize(estimate  = mean(.data$rd),
                       conf.low  = quantile(.data$rd, probs = a[1]),
                       conf.high = quantile(.data$rd, probs = a[2]),
                       std.error = sqrt(var(.data$rd)))
  }
}

summary.margstd <- function(object, dispersion = NULL,
                            correlation = FALSE, symbolic.cor = FALSE,
                            level = 0.95, bootrepeats = 100, ...) {
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
