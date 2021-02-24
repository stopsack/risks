# Helper functions for marginal standardization after
# fitting a logistic regression model

# Fit logistic model, obtained marginal predictions, generate contrasts
# For better performance than with `::`, import a few functions for eststd()
#' @importFrom stats predict glm binomial
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom purrr %>% map map_dbl
#' @importFrom rlang .data :=
eststd <- function(x, data, predictor, levels, estimate) {
  mdl <- glm(formula = x$formula, family = binomial(link = "logit"),
             data = data)
  results <- tibble(term = levels) %>%
    mutate(data = map(.x = .data$term,
                      .f = ~mutate(.data = data,
                                   !!!predictor := .x)),
           response = map(.x = data,
                          .f = ~predict(object = mdl,
                                        newdata = .x,
                                        type = "response")),
           means = map_dbl(.x = .data$response, .f = mean))
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
  fit <- stats::glm(formula = formula, family = binomial(link = "logit"),
                    data = data)

  # find variable to standardize over
  if(!is.null(variable)) {
    if(!(variable %in% names(fit$model)[2:length(names(fit$model))]))
      stop(paste0("Variable '", variable, "' is not part of the model."))
    if(!(class(fit$model[[variable]])[1] %in%
         c("character", "factor", "ordered", "logical")))
      if(length(unique(fit$model[[variable]])) > 2 & is.null(at))
        stop(paste0("Variable '", variable, "' is not a factor, ordered ",
                    " factor, logical, character, or a numeric with 2 levels, ",
                    "and no values to standardize at are given via 'at ='."))
    predictor <- variable
  } else {
    model_vars <- tibble::tibble(vars = names(fit$model)) %>%
      dplyr::mutate(
        type    = purrr::map_chr(.x = .data$vars,
                                 .f = ~class(fit$model %>%
                                               dplyr::pull(.x))[1]),
        nlevels = purrr::map_int(.x = .data$vars,
                                 .f = ~length(unique(fit$model %>%
                                                       dplyr::pull(.x))))) %>%
      dplyr::slice(-1) %>%
      dplyr::filter(.data$type %in% c("character", "factor",
                                      "logical", "ordered") |
                      (.data$type %in% c("numeric", "integer") &
                         nlevels == 2)) %>%
      dplyr::slice(1)

    if(nrow(model_vars) > 0)
      predictor <- model_vars$vars
    else
      stop(paste("No exposure variable identified that is a factor, ordered",
                 "factor, logical, character, or numeric with 2 levels."))
  }

  # find levels to standardize at
  if(!is.null(at)) {
    if(length(at) < 2)
      stop("Because 'at' has less than 2 levels, contrasts cannot be estimated.")
    if(class(fit$model %>% dplyr::pull(predictor)) %in%
       c("character", "factor", "ordered", "logical") &
       sum(at %in% unique(fit$model %>% dplyr::pull(predictor))) != length(at))
      stop(paste0("Some of the levels, specificied via 'at =', ",
                  "of the non-numeric variable '",
                  predictor,
                  "' were not found in the model data."))
    if(class(fit$model %>% dplyr::pull(predictor)) == "numeric")
      if(min(fit$model %>% dplyr::pull(predictor)) > min(at) |
         max(fit$model %>% dplyr::pull(predictor)) < max(at))
        warning(paste0("Numeric levels provided via 'at = ", at,
                       "' will lead to out-of-range predictions ",
                       "for the variable '", predictor, "'."))
    levels <- at
  } else {
    if(!is.null(fit$xlevels) & predictor %in% names(fit$xlevels))
      # retain level ordering as in model
      levels <- fit$xlevels[[predictor]]
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
  newfit <- append(newfit, fit[c("residuals", "fitted.values", "weights",
                                 "prior.weights", "family", "deviance", "aic",
                                 "null.deviance", "iter", "df.residual",
                                 "df.null", "y", "converged", "boundary",
                                 "model", "call", "formula", "terms", "data",
                                 "offset", "control", "method", "xlevels",
                                 "qr")])
  class(newfit) <- c("margstd", "glm", "lm")
  newfit <- estimate_maxprob(fit = newfit, formula = formula,
                             data = data, link = "logit")
  return(newfit)
}

# Nonparametric bootstrapping
boot_eststd_nonpar <- function(object, bootrepeats) {
  bootfn <- function(data, index, fit, ...) {
    eststd(x = fit, data = data[index, ], ...)
  }

  boot::boot(data = object$data,
             statistic = bootfn,
             R = bootrepeats,
             fit = stats::glm(formula = object$formula,
                              family = binomial(link = "logit"),
                              data = object$data),
             predictor = object$margstd_predictor,
             levels = object$margstd_levels,
             estimate = object$estimate[1])
}

# BCa bootstrap confidence intervals after nonparametric bootstrapping
bootci_nonpar <- function(boot.out, conf, parameters) {
  getbcaci_nonpar <- function(boot.out, index, conf) {
    mybca <- boot::boot.ci(boot.out = boot.out, index = index,
                           type = "bca", conf = conf)$bca
    res <- as.numeric(mybca[1, 4:5])
    names(res) <- c("conf.low", "conf.high")
    return(res)
  }

  dplyr::bind_rows(
    c(conf.low = NA, conf.high = NA),
    purrr::map_dfr(.x = 2:parameters,
                   .f = getbcaci_nonpar, boot.out = boot.out, conf = conf))
}

#' @importFrom rlang :=
# Parametric bootstrapping using boot::boot, use with normal CIs
boot_eststd_norm <- function(object, bootrepeats) {
  mainfit <- stats::glm(formula = object$formula,
                        family = binomial(link = "logit"),
                        data = object$data)
  yvar <- all.vars(object$formula)[1]

  bootfn <- function(data, fitformula, ...) {
    eststd(x = fitformula, data = data, ...)
  }

  # Parametric resampling function
  rangen <- function(data, mle) {
    data %>% dplyr::mutate(!!yvar := rbinom(n = dplyr::n(),
                                            size = 1,
                                            prob = mle))
  }

  boot::boot(data = object$data,
             statistic = bootfn,
             R = bootrepeats,
             sim = "parametric",
             ran.gen = rangen,
             mle = predict(mainfit, type = "response"),
             fitformula = list(formula = object$formula),
             predictor = object$margstd_predictor,
             levels = object$margstd_levels,
             estimate = object$estimate[1])
}

# Normal bootstrap confidence intervals after parametric bootstrapping
bootci_norm <- function(boot.out, conf, parameters) {
  getbootci_norm <- function(boot.out, index, conf) {
    mybootci <- boot::boot.ci(boot.out = boot.out, index = index,
                              type = "norm", conf = conf)$normal
    res <- as.numeric(mybootci[1, 2:3])
    names(res) <- c("conf.low", "conf.high")
    return(res)
  }

  dplyr::bind_rows(
    c(conf.low = NA, conf.high = NA),
    purrr::map_dfr(.x = 2:parameters,
                   .f = getbootci_norm, boot.out = boot.out, conf = conf))
}

# Parametric bootstrapping for use with bcaboot::bcapar()
boot_eststd_bcapar <- function(object, bootrepeats, vars) {
  glm_model <- stats::glm(formula = object$formula,
                          family = binomial(link = "logit"),
                          data = object$model)  # $data includes NA
  yvar <- all.vars(object$formula)[1]

  pi_hat <- glm_model$fitted.values
  n <- length(pi_hat)
  y_star <- sapply(seq_len(bootrepeats),
                   function(i) ifelse(runif(n) <= pi_hat, 1, 0))
  beta_star <- apply(y_star, 2, function(y) {
    boot_data <- glm_model$data
    boot_data[, yvar] <- y
    eststd(x = object, data = boot_data,
           predictor = object$margstd_predictor,
           levels = object$margstd_levels,
           estimate = object$estimate[1])
  })
  list(theta = eststd(x = object, data = object$model,  # $data includes NA
                      predictor = object$margstd_predictor,
                      levels = object$margstd_levels,
                      estimate = object$estimate[1])[vars],
       theta_star = beta_star[vars, ],
       suff_stat = t(y_star) %*% model.matrix(glm_model))
}

# Parametric BCa bootstrap CI from bcaboot::bcapar()
bootci_bcapar <- function(boot_out, level, parameters) {
  getbootci_bcapar <- function(theta, theta_star, suff_stat, alpha) {
    mybca <- bcaboot::bcapar(t0 = theta, tt = theta_star, bb = suff_stat,
                             alpha = alpha)
    res <- c(mybca$lims[1, "bca"], mybca$lims[3, "bca"])
    names(res) <- c("conf.low", "conf.high")
    return(res)
  }

  dplyr::bind_rows(
    c(conf.low = NA, conf.high = NA),
    purrr::map_dfr(.x = 2:parameters,
                   .f = ~getbootci_bcapar(theta      = boot_out$theta[.x],
                                          theta_star = boot_out$theta_star[.x, ],
                                          suff_stat  = boot_out$suff_stat,
                                          alpha      = (1 - level) / 2)))
}

#' Bootstrap confidence intervals
#'
#' Confidence intervals for models fit using marginal standardization
#' based on parametric bootstrapping.
#'
#' @param object Model fitted through marginal standardization
#' @param parm Not used, for compatibility
#' @param level Confidence level, defaults to 0.95.
#' @param bootrepeats Bootstrap repeats. Defaults to 200.
#'   Strongly recommend >1000.
#' @param bootci Type of bootstrap confidence interval:
#'
#'   * \code{"bca"} Default. Parametric BCa (bias-corrected accelerated)
#'     confidence intervals.
#'   * \code{"normal"} Parametric normality-based confidence intervals,
#'     which require lower repeat numbers but are less accurate and
#'     may result in invalid results for ratios.
#'   * \code{"nonpar"} Non-parametric BCa confidence intervals,
#'     which should be used with caution because of the risk
#'     of sparse-data bias with non-parametric bootstrapping.
#'
#' @param ... Not used
#'
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @export
confint.margstd <- function(object, parm = NULL,
                            level = 0.95,
                            bootrepeats = 200,
                            bootci = c("bca", "normal", "nonpar"),
                            ...) {
  cf <- coef(object)
  pnames <- names(cf)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste0(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  ci <- array(NA, dim = c(length(pnames), 2L), dimnames = list(pnames, pct))

  switch(EXPR = match.arg(bootci),
         normal = {
           myboot <- boot_eststd_norm(object = object,
                                      bootrepeats = bootrepeats)

           ci[] <- bootci_norm(boot.out = myboot, conf = level,
                               parameters = length(pnames)) %>%
             as.matrix()
         },
         nonpar = {
           myboot <- boot_eststd_nonpar(object = object,
                                        bootrepeats = bootrepeats)

           ci[] <- bootci_nonpar(boot.out = myboot, conf = level,
                                 parameters = length(pnames)) %>%
             as.matrix()
         },
         bca = {
           boot_out <- boot_eststd_bcapar(object = object,
                                          bootrepeats = bootrepeats,
                                          vars = pnames)
           ci[] <- bootci_bcapar(boot_out = boot_out, level = level,
                                 parameters = length(pnames))  %>%
             as.matrix()
         })
  return(ci)
}

# Bootstrapped standard errors, required by summary.margstd()
margstd_stderror <- function(object, level = 0.95, bootreps, bootci, ...) {
  switch(
    EXPR = bootci,
    normal = {
      myboot <- boot_eststd_norm(object = object, bootrepeats = bootreps)

      tibble::tibble(estimate = coef(object),
                     std.error = base::apply(myboot$t,
                                             MARGIN = 2, FUN = sd)) %>%
        dplyr::bind_cols(bootci_norm(boot.out = myboot, conf = level,
                                     parameters = length(coef(object))))
    },
    nonpar = {
      myboot <- boot_eststd_nonpar(object = object, bootrepeats = bootreps)

      tibble::tibble(estimate = coef(object),
                     std.error = base::apply(myboot$t, MARGIN = 2, FUN = sd)) %>%
        dplyr::bind_cols(bootci_nonpar(boot.out = myboot, conf = level,
                                       parameters = length(coef(object))))
    },
    bca = {
      boot_out <- boot_eststd_bcapar(object = object,
                                     bootrepeats = bootreps,
                                     vars = names(coef(object)))
      tibble::tibble(estimate = coef(object),
                     std.error = base::apply(boot_out$theta_star,
                                             MARGIN = 1, FUN = sd)) %>%
        dplyr::bind_cols(bootci_bcapar(boot_out = boot_out, level = level,
                                       parameters = length(coef(object))))
    })
}

#' Summary for models using marginal standardization
#'
#' @param object Model
#' @param dispersion Not used
#' @param correlation Not used
#' @param symbolic.cor Not used
#' @param level Confidence level, defaults to \code{0.95}.
#' @param bootrepeats Bootstrap repeats for standard errors. Defaults to 200.
#'          Strongly recommend >1000.
#' @param bootci Type of bootstrap confidence interval:
#'
#'   * \code{"bca"} Default. Parametric BCa (bias-corrected accelerated)
#'     confidence intervals.
#'   * \code{"normal"} Parametric normality-based confidence intervals,
#'     which require lower repeat numbers but are less accurate and
#'     may result in invalid results for ratios.
#'   * \code{"nonpar"} Non-parametric BCa confidence intervals,
#'     which should be used with caution because of the risk
#'     of sparse-data bias with non-parametric bootstrapping.
#'
#' @param ... Not used
#'
#' @return Model summary (list)
#' @return
#' @export
summary.margstd <- function(object, dispersion = NULL,
                            correlation = FALSE, symbolic.cor = FALSE,
                            level = 0.95, bootrepeats = 200,
                            bootci = c("bca", "normal",  "nonpar"), ...) {
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
      stop(paste("lm object does not have a proper 'qr' component.\n",
                 "Rank zero or should not have used lm(.., qr=FALSE)."))
    #coef.p <- object$coefficients[Qr$pivot[p1]]
    coef.p <- object$coefficients
    stderror <- margstd_stderror(object = object, level = level,
                                 bootreps = bootrepeats,
                                 bootci = match.arg(bootci), ...)
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
    dimnames(coef.table) <- list(NULL, c("Estimate", "Std. Error", "t value",
                                         "Pr(>|t|)"))
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
             conf.int = stderror, level = level,
             margstd.bootrepeats = bootrepeats,
             margstd.bootci = match.arg(bootci)))
  class(ans) <- "summary.glm"
  return(ans)
}
