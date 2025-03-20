#' Marginal standardization with delta method standard errors
#'
#' Delta method implementation modified after \code{probratio} from the
#' \code{epitools} package, also allowing for categorical and continuous
#' exposures.
#'
#' @param formula A formula
#' @param data Data
#' @param estimand Estimand
#' @param variable Variable
#' @param at At
#' @param interaction_warning Warning for interaction
#'
#' @return A model fit, passed by estimate_maxprob()
#' @noRd
estimate_margstd_delta <- function(
    formula,
    data,
    estimand   = c("rr", "rd"),
    variable = NULL,
    at = NULL,
    interaction_warning = TRUE) {
  estimand <- match.arg(estimand)
  fit <- stats::glm(
    formula = formula,
    family = binomial(link = "logit"),
    data = data)
  exposure <- find_margstd_exposure(
    fit = fit,
    variable = variable,
    at = at)
  all_levels <- paste0(exposure$predictor, exposure$all_levels)[-1]

  # Check and, if need be, reorder categorical exposure
  # if different level ordering has been requested
  if(exposure$categorical == TRUE) {
    if(exposure$margstd_levels[1] != exposure$all_levels[1]) {
      data <- data %>%
        dplyr::rename(expos = dplyr::one_of(exposure$predictor)) %>%
        dplyr::mutate(
          expos = relevel(factor(.data$expos),
                          ref = exposure$margstd_levels[1])) %>%
        dplyr::rename(!!exposure$predictor := "expos")
      fit <- stats::glm(
        formula = formula,
        family = binomial(link = "logit"),
        data = data)
      exposure <- find_margstd_exposure(
        fit = fit,
        variable = variable,
        at = at)
      all_levels <- paste0(exposure$predictor, levels(exposure$all_levels))[-1]
    }
  } else {
    if(!is.null(exposure$margstd_levels))
      stop(paste(
        "Levels for marginal standardization for a continuous",
        "exposure are not supported with approach = 'margstd_delta'.",
        "Use approach = 'margstd_boot'."))
  }

  if(exposure$interaction & interaction_warning)
    warning(paste0(
      "The formula appears to contain an interaction term involving the ",
      "exposure variable '", exposure$predictor, "'. ",
      'Such terms may not have a marginal interpretation with approach = ',
      '"margstd_delta". Consider using: approach = "margstd_boot".'))

  n <- nrow(model.matrix(fit))
  Nvec <- matrix(
    data = rep(c(1 / n, 0, 0, 1 / n), each = n),
    nrow = n * 2,
    ncol = 2)

  delta_fun <- function(model_matrix, fit, estimand, Nvec, term) {
    allpreds <- family(fit)$linkinv(model_matrix %*% coef(fit))
    avgpreds <- t(Nvec) %*% allpreds

    if(estimand[1] == "rr") {
      mainfun <- function(x) log(x[2]) - log(x[1])
      derivation <- deriv(~log(y) - log(x), c("x", "y"))
    } else {
      mainfun <- function(x) x[2] - x[1]
      derivation <- deriv(~y - x, c("x", "y"))
    }
    estimate <- mainfun(avgpreds)
    V <- sweep(chol(vcov(fit)) %*% t(model_matrix),
               allpreds * (1 - allpreds), "*", MARGIN = 2) %*% Nvec
    V <- t(V) %*% V
    dxdy <- matrix(attr(eval(derivation, list(x = avgpreds[1],
                                              y = avgpreds[2])),
                        "gradient"))
    tibble::tibble(
      term = term,
      estimate = estimate,
      std.error = as.numeric(sqrt(t(dxdy) %*% V %*% dxdy)),
      statistic = .data$estimate / .data$std.error)
  }

  if(exposure$categorical == TRUE) {
    res <- dplyr::bind_rows(
      tibble::tibble(
        term = paste0(exposure$predictor,
                      exposure$margstd_levels)[1],
        estimate = 0,
        std.error = 0,
        statistic = NA_real_),
      purrr::map_dfr(
        .x = paste0(exposure$predictor, exposure$margstd_levels)[-1],
        .f = ~{
          model_matrix <- model.matrix(fit)
          model_matrix[, all_levels] <- 0
          model_matrix0 <- model_matrix1 <- model_matrix
          model_matrix0[, .x] <- 0
          model_matrix1[, .x] <- 1
          model_matrix <- rbind(model_matrix0, model_matrix1)
          delta_fun(
            model_matrix = model_matrix,
            fit = fit,
            estimand = estimand,
            Nvec = Nvec,
            term = .x)
        }))
  } else {
    model_matrix0 <- model_matrix1 <- model.matrix(fit)
    delta <- sd(model_matrix0[, exposure$predictor]) / 1000
    model_matrix1[, exposure$predictor] <- model_matrix1[, exposure$predictor] +
      delta
    res <- delta_fun(
      model_matrix = rbind(model_matrix0, model_matrix1),
      fit = fit,
      estimand = estimand,
      Nvec = Nvec,
      term = exposure$predictor) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c("estimate", "std.error"),
          .fns = ~. / delta))
  }

  coefs <- res$estimate
  names(coefs) <- res$term
  newfit <- list(
    coefficients      = coefs,
    estimand          = estimand[1],
    margstd_predictor = exposure$predictor,
    margstd_levels    = exposure$margstd_levels,
    margstd_delta_res = res,
    margstd_delta_interaction = exposure$interaction,
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
  class(newfit) <- c("margstd_delta", "glm", "lm")
  newfit <- estimate_maxprob(
    fit = newfit,
    formula = formula,
    data = data,
    link = "logit")
  return(newfit)
}


#' Delta method confidence intervals
#'
#' Confidence intervals for models fit using marginal standardization
#' based on the melta method.
#'
#' @param object Model fitted through marginal standardization, delta method
#' @param parm Not used, for compatibility
#' @param level Confidence level, defaults to \code{0.95}.
#' @param ... Not used
#'
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @export
#' @noRd
confint.margstd_delta <- function(
    object,
    parm = NULL,
    level = 0.95,
    ...) {
  cf <- coef(object)
  pnames <- names(cf)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste(
    format(
      100 * a,
      trim = TRUE,
      scientific = FALSE,
      digits = 3),
    "%")
  ci <- array(
    data = NA,
    dim = c(length(pnames), 2L),
    dimnames = list(pnames, pct))

  ci[] <- object$margstd_delta_res %>%
    dplyr::transmute(
      conf.low  = .data$estimate + qnorm(a[1]) * .data$std.error,
      conf.high = .data$estimate + qnorm(a[2]) * .data$std.error) %>%
    as.matrix()
  return(ci)
}


#' Summary for models using marginal standardization with delta method SEs
#'
#' @param object Model
#' @param dispersion Not used
#' @param correlation Not used
#' @param symbolic.cor Not used
#' @param level Confidence level, defaults to \code{0.95}.
#' @param ... Not used
#'
#' @return Model summary (list)
#' @export
summary.margstd_delta <- function(
    object,
    dispersion = NULL,
    correlation = FALSE,
    symbolic.cor = FALSE,
    level = 0.95,
    ...) {
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
    s.err <- object$margstd_delta_res$std.error
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
           list(
             #deviance.resid = residuals(object, type = "deviance"),
             coefficients = coef.table, aliased = aliased,
             dispersion = dispersion, df = c(object$rank, df.r, df.f),
             conf.int = object$margstd_delta_res$std.error,
             level = level))
  class(ans) <- "summary.glm"
  return(ans)
}
