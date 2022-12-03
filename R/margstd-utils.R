# Helper functions for marginal standardization with bootstrapping after
# fitting a logistic regression model
#' @importFrom dplyr %>%
#' @importFrom rlang .data
# Fit logistic model, obtained marginal predictions, generate contrasts
fit_and_predict <- function(
    data,
    predictor,
    margstd_levels,
    estimand,
    formula) {
  if(!is.null(margstd_levels)) {  # categorical predictors
    fit <- stats::glm(
      formula = formula,
      data = data,
      family = binomial(link = "logit"))
    data_rep <- data[rep(
      x = seq_len(nrow(data)),
      times = length(margstd_levels)), ]
    data_rep[, predictor] <- rep(
      x = margstd_levels,
      each = nrow(data))
    res <- tapply(
      X = predict(
        object = fit,
        newdata = data_rep,
        type = "response"),
      INDEX = data_rep[, predictor],
      FUN = mean)
    res <- res[as.character(margstd_levels)]
    if(estimand == "rd")
      res <- res - res[1]
    else
      res <- log(res / res[1])
    names(res) <- paste0(predictor, names(res))
  } else {  # continuous predictors: average marginal effect
    fit <- glm(
      formula = formula,
      data = data,
      family = binomial(link = "logit"))
    delta <- sd(data[, predictor]) / 1000
    newdata <- data
    newdata[, predictor] <- newdata[, predictor] + delta
    pred1 <- predict(
      object = fit,
      type = "response")
    pred2 <- predict(
      object = fit,
      newdata = newdata,
      type = "response")
    if(estimand == "rd")
      res <- mean((pred2 - pred1) / delta)
    else
      res <- mean(log(pred2 / pred1) / delta)
    names(res) <- predictor
  }
  return(res)
}


# Main function for marginal standardization
estimate_margstd_boot <- function(
    formula,
    data,
    estimand   = c("rr", "rd"),
    variable   = NULL,    # where to standardize; default: 1st variable
    at         = NULL) {  # levels of variable to standardize at
  fit <- stats::glm(
    formula = formula,
    family = binomial(link = "logit"),
    data = data)
  exposure <- find_margstd_exposure(
    fit = fit,
    variable = variable,
    at = at)

  eststd <- fit_and_predict(
    data = as.data.frame(data),
    predictor = exposure$predictor,
    margstd_levels = exposure$margstd_levels,
    formula = fit$formula,
    estimand = estimand[1])
  newfit <- list(
    coefficients      = eststd,
    estimand          = estimand[1],
    margstd_predictor = exposure$predictor,
    margstd_levels    = exposure$margstd_levels,
    rank              = 1)
  newfit <- append(
    newfit,
    fit[c("residuals", "fitted.values", "weights",
          "prior.weights", "family", "deviance", "aic",
          "null.deviance", "iter", "df.residual",
          "df.null", "y", "converged", "boundary",
          "model", "call", "formula", "terms", "data",
          "offset", "control", "method", "xlevels",
          "qr")])
  class(newfit) <- c("margstd_boot", "glm", "lm")
  newfit <- estimate_maxprob(
    fit = newfit,
    formula = formula,
    data = data,
    link = "logit")
  return(newfit)
}

# Nonparametric bootstrapping
boot_eststd_nonpar <- function(
    object,
    bootrepeats) {
  bootfn <- function(data, index, fit, ...) {
    fit_and_predict(
      data = data[index, ],
      formula = fit$formula, ...)
  }

  boot::boot(
    data = as.data.frame(object$data),
    statistic = bootfn,
    R = bootrepeats,
    fit = stats::glm(
      formula = object$formula,
      family = binomial(link = "logit"),
      data = object$data),
    predictor = object$margstd_predictor,
    margstd_levels = object$margstd_levels,
    estimand = object$estimand[1])
}

# BCa bootstrap confidence intervals after nonparametric bootstrapping
bootci_nonpar <- function(
    boot.out,
    conf,
    parameters) {
  getbcaci_nonpar <- function(boot.out, index, conf) {
    mybca <- boot::boot.ci(
      boot.out = boot.out,
      index = index,
      type = "bca",
      conf = conf)$bca
    res <- as.numeric(mybca[1, 4:5])
    names(res) <- c("conf.low", "conf.high")
    return(res)
  }

  if(parameters > 1) {
    dplyr::bind_rows(
      c(conf.low = NA, conf.high = NA),
      purrr::map_dfr(
        .x = 2:parameters,
        .f = getbcaci_nonpar,
        boot.out = boot.out,
        conf = conf))
  } else {
    tibble::as_tibble(t(getbcaci_nonpar(
      boot.out = boot.out,
      conf = conf,
      index = 1)))
  }
}

#' @importFrom rlang :=
# Parametric bootstrapping using boot::boot, use with normal CIs
boot_eststd_norm <- function(
    object,
    bootrepeats) {
  mainfit <- stats::glm(
    formula = object$formula,
    family = binomial(link = "logit"),
    data = as.data.frame(object$data))
  yvar <- all.vars(object$formula)[1]

  bootfn <- function(data, fitformula, ...) {
    fit_and_predict(
      data = data,
      formula = fitformula$formula, ...)
  }

  # Parametric resampling function
  rangen <- function(data, mle) {
    data %>% dplyr::mutate(
      !!yvar := rbinom(
        n = dplyr::n(),
        size = 1,
        prob = mle))
  }

  boot::boot(
    data = as.data.frame(object$data),
    statistic = bootfn,
    R = bootrepeats,
    sim = "parametric",
    ran.gen = rangen,
    mle = predict(mainfit, type = "response"),
    fitformula = list(formula = object$formula),
    predictor = object$margstd_predictor,
    margstd_levels = object$margstd_levels,
    estimand = object$estimand[1])
}

# Normal bootstrap confidence intervals after parametric bootstrapping
bootci_norm <- function(
    boot.out,
    conf,
    parameters) {
  getbootci_norm <- function(boot.out, index, conf) {
    mybootci <- boot::boot.ci(
      boot.out = boot.out,
      index = index,
      type = "norm",
      conf = conf)$normal
    res <- as.numeric(mybootci[1, 2:3])
    names(res) <- c("conf.low", "conf.high")
    return(res)
  }

  if(parameters > 1) {
  dplyr::bind_rows(
    c(conf.low = NA, conf.high = NA),
    purrr::map_dfr(
      .x = 2:parameters,
      .f = getbootci_norm,
      boot.out = boot.out,
      conf = conf))
  } else {
    tibble::as_tibble(t(getbootci_norm(
      boot.out = boot.out,
      conf = conf,
      index = 1)))
  }
}

# Parametric bootstrapping for use with bcaboot::bcapar()
boot_eststd_bcapar <- function(
    object,
    bootrepeats,
    vars) {
  glm_model <- stats::glm(
    formula = object$formula,
    family = binomial(link = "logit"),
    data = object$model)  # $data includes NA
  yvar <- all.vars(object$formula)[1]

  pi_hat <- glm_model$fitted.values
  n <- length(pi_hat)
  y_star <- sapply(
    X = seq_len(bootrepeats),
    FUN = function(i) ifelse(runif(n) <= pi_hat, 1, 0))
  beta_star <- apply(
    X = y_star,
    MARGIN = 2,
    FUN = function(y) {
      boot_data <- glm_model$data
      boot_data[, yvar] <- y
      fit_and_predict(
        data = boot_data,
        predictor = object$margstd_predictor,
        margstd_levels = object$margstd_levels,
        estimand = object$estimand[1],
        formula = object$formula)
    })

  # fit_and_predict() gives a numeric vector, not an array, for continuous vars
  if(inherits(x = beta_star, what = "numeric", which = TRUE) == 1) {
    beta_star <- array(
      data = beta_star,
      dim = c(1, length(beta_star)),
      dimnames = list(object$margstd_predictor))
    theta_star <- beta_star
  } else {
    theta_star <- beta_star[vars, ]
  }
  list(
    theta = fit_and_predict(
      data = object$model,  # $data includes NA
      predictor = object$margstd_predictor,
      margstd_levels = object$margstd_levels,
      estimand = object$estimand[1],
      formula = object$formula)[vars],
    theta_star = theta_star,
    suff_stat = t(y_star) %*% model.matrix(glm_model))
}

# Parametric BCa bootstrap CI from bcaboot::bcapar()
bootci_bcapar <- function(
    boot_out,
    level,
    parameters) {
  getbootci_bcapar <- function(theta, theta_star, suff_stat, alpha) {
    mybca <- bcaboot::bcapar(
      t0 = theta,
      tt = theta_star,
      bb = suff_stat,
      alpha = alpha)
    res <- c(
      mybca$lims[1, "bca"],
      mybca$lims[3, "bca"],
      mybca$lims[1, "jacksd"],
      mybca$lims[3, "jacksd"])
    names(res) <- c("conf.low", "conf.high", "jacksd.low", "jacksd.high")
    return(res)
  }
  if(parameters > 1) {  # categorical variables with reference level
    dplyr::bind_rows(
      c(conf.low = NA, conf.high = NA),
      purrr::map_dfr(
        .x = 2:parameters,
        .f = ~getbootci_bcapar(
          theta      = boot_out$theta[.x],
          theta_star = boot_out$theta_star[.x, ],
          suff_stat  = boot_out$suff_stat,
          alpha      = (1 - level) / 2)))
  } else {  # continuous variables with a single parameter, no reference
    tibble::as_tibble(t((
      getbootci_bcapar(
        theta      = boot_out$theta[1],
        theta_star = as.numeric(boot_out$theta_star),
        suff_stat  = boot_out$suff_stat,
        alpha      = (1 - level) / 2))))
  }
}

#' Bootstrap confidence intervals
#'
#' Confidence intervals for models fit using marginal standardization
#' based on parametric bootstrapping.
#'
#' @param object Model fitted through marginal standardization
#' @param parm Not used, for compatibility
#' @param level Confidence level, defaults to 0.95.
#' @param bootrepeats Bootstrap repeats. Defaults to 1000. Consider increasing.
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
#' @param jacksd Return jackknife Monte-Carlo error for the confidence limits?
#'   Only functional with BCa confidence intervals. Defaults to \code{FALSE}.
#'
#' @param ... Not used
#'
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @export
confint.margstd_boot <- function(
    object,
    parm = NULL,
    level = 0.95,
    bootrepeats = 1000,
    bootci = c("bca", "normal", "nonpar"),
    jacksd = FALSE,
    ...) {
  cf <- coef(object)
  pnames <- names(cf)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3),
               "%")
  ci <- array(NA, dim = c(length(pnames), 2L), dimnames = list(pnames, pct))

  switch(
    EXPR = match.arg(bootci),
    normal = {
      myboot <- boot_eststd_norm(
        object = object,
        bootrepeats = bootrepeats)
      ci[] <- bootci_norm(
        boot.out = myboot,
        conf = level,
        parameters = length(pnames)) %>%
        as.matrix()
    },
    nonpar = {
      myboot <- boot_eststd_nonpar(
        object = object,
        bootrepeats = bootrepeats)
      ci[] <- bootci_nonpar(
        boot.out = myboot,
        conf = level,
        parameters = length(pnames)) %>%
        as.matrix()
    },
    bca = {
      boot_out <- boot_eststd_bcapar(
        object = object,
        bootrepeats = bootrepeats,
        vars = pnames)
      ci_all <- bootci_bcapar(
        boot_out = boot_out,
        level = level,
        parameters = length(pnames))
      if(jacksd == FALSE) {
        ci[] <- ci_all[, c("conf.low", "conf.high")] %>%
          as.matrix()
      } else {
        ci <- array(
          data = NA,
          dim = c(length(pnames), 4L),
          dimnames = list(pnames,
                          c(pct, "jacksd.low", "jacksd.high")))
        ci[] <- ci_all %>%
          as.matrix()
      }
    })
  return(ci)
}

# Bootstrapped standard errors, required by summary.margstd_boot()
margstd_boot_stderror <- function(
    object,
    level = 0.95,
    bootreps,
    bootci,
    ...) {
  switch(
    EXPR = bootci,
    normal = {
      myboot <- boot_eststd_norm(
        object = object,
        bootrepeats = bootreps)

      tibble::tibble(
        estimate = coef(object),
        std.error = base::apply(
          X = myboot$t,
          MARGIN = 2,
          FUN = sd)) %>%
        dplyr::bind_cols(
          bootci_norm(
            boot.out = myboot,
            conf = level,
            parameters = length(coef(object))))
    },
    nonpar = {
      myboot <- boot_eststd_nonpar(
        object = object,
        bootrepeats = bootreps)
      tibble::tibble(
        estimate = coef(object),
        std.error = base::apply(
          X = myboot$t,
          MARGIN = 2,
          FUN = sd)) %>%
        dplyr::bind_cols(
          bootci_nonpar(
            boot.out = myboot,
            conf = level,
            parameters = length(coef(object))))
    },
    bca = {
      boot_out <- boot_eststd_bcapar(
        object = object,
        bootrepeats = bootreps,
        vars = names(coef(object)))
      tibble::tibble(
        estimate = coef(object),
        std.error = base::apply(
          X = boot_out$theta_star,
          MARGIN = 1,
          FUN = sd)) %>%
        dplyr::bind_cols(
          bootci_bcapar(
            boot_out = boot_out,
            level = level,
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
#' @param bootrepeats Bootstrap repeats for standard errors. Defaults to 1000.
#'  Consider increasing.
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
#' @export
summary.margstd_boot <- function(
    object,
    dispersion = NULL,
    correlation = FALSE,
    symbolic.cor = FALSE,
    level = 0.95,
    bootrepeats = 1000,
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
    coef.p <- object$coefficients
    stderror <- margstd_boot_stderror(
      object = object,
      level = level,
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
  keep <- match(
    x = c(
      "call", "terms", "family", "deviance", "aic",
      "contrasts", "df.residual", "null.deviance", "df.null",
      "iter", "na.action"),
    table = names(object),
    nomatch = 0L)
  ans <- c(
    object[keep],
    list(#deviance.resid = residuals(object, type = "deviance"),
      coefficients = coef.table, aliased = aliased,
      dispersion = dispersion, df = c(object$rank, df.r, df.f),
      conf.int = stderror, level = level,
      margstd_boot.bootrepeats = bootrepeats,
      margstd_boot.bootci = match.arg(bootci)))
  class(ans) <- "summary.glm"
  return(ans)
}
