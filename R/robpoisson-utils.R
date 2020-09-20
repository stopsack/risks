# Helper functions for estimate_risk(approach = "robpoisson") and
# the use of Poisson models in approach = "auto" or approach = "all"

#' @import stats

# estimate_poisson: internal, fitting the Poisson model
estimate_poisson <- function(formula, data, link, weights = NULL, ...) {
  fit <- eval(substitute(stats::glm(formula = formula, family = poisson(link = link),
                                    data = data, weights = weights)))
  class(fit) <- c("robpoisson", class(fit))
  fit <- estimate_maxprob(fit = fit, formula = formula, data = data, link = link)
  return(fit)
}

#' Robust confidence intervals for Poisson model
#'
#' Estimate confidence intervals for the Poisson model
#' with robust/sandwich/empirical covariance structure.
#'
#' @param object Fitted model
#' @param parm Not used
#' @param level Confidence level, defaults to 0.95
#' @param ... Additional arguments, not used
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @export
confint.robpoisson <- function(object, parm = NULL, level = 0.95, ...) {
  # modified after stats:::confint.default()
  cf <- coef(object)
  pnames <- names(cf)
  #if (missing(parm))
  parm <- pnames
  #else if (is.numeric(parm))
  #  parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste0(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  # Robust covariance, but not the HC3.
  # Poisson with HC0 is probably already too conservative compared to binomial model.
  ses <- sqrt(diag(sandwich::vcovHC(object, type = "HC0")))
  ci[] <- cf[parm] + ses %o% fac
  return(ci)
}

risks_meat <- function (x, type = "HC0", omega = NULL, ...) {
  # a modification of sandwich::meatHC(), restricted to HC0 covariance
  # needed because just modifying summary.glm() as below led to recursive calls
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  X <- model.matrix(x)
  if (any(alias <- is.na(coef(x))))
    X <- X[, !alias, drop = FALSE]
  attr(X, "assign") <- NULL
  n <- NROW(X)
  diaghat <- try(hatvalues(x), silent = TRUE)
  df <- n - NCOL(X)
  ef <- sandwich::estfun(x, ...)
  res <- rowMeans(ef/X, na.rm = TRUE)
  res[apply(abs(ef) < .Machine$double.eps, 1L, all)] <- 0
  if (is.null(omega)) {
    omega <- function(residuals, diaghat, df) residuals^2
  }
  if (is.function(omega))
    omega <- omega(res, diaghat, df)
  rval <- sqrt(omega) * X
  rval <- crossprod(rval)/n
  return(rval)
}

#' Summary for Poisson model with robust covariance
#'
#' Summarize results from fitting a Poisson model with
#' robust/empirical/sandwich covariance.
#' The output is the same as for a regular \code{summary(glm(...))},
#' except for using robust standard errors.
#'
#' @param object Model
#' @param dispersion Not used
#' @param correlation Not used
#' @param symbolic.cor Not used
#' @param ... Other arguments, not used
#' @export
summary.robpoisson <- function (object, dispersion = NULL, correlation = FALSE,
                                symbolic.cor = FALSE, ...) {
  # a modification of summary.glm(), calling risks_meat() to estimate covariance
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
    coef.p <- object$coefficients[Qr$pivot[p1]]
    ###### changes here #######
    # needed to avoid recursive calls:
    covmat.unscaled <- risks_meat(object)  # sandwich::vcovHC -> meat.(type = "HC0")
    #chol2inv(Qr$qr[p1, p1, drop = FALSE])
    #dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    ###### end changes ########
    covmat <- dispersion * covmat.unscaled
    var.cf <- diag(covmat)
    s.err <- sqrt(var.cf)
    tvalue <- coef.p/s.err
    dn <- c("Estimate", "Std. Error")
    if (!est.disp) {
      pvalue <- 2 * pnorm(-abs(tvalue))
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn,
                                                    "z value", "Pr(>|z|)"))
    }
    else if (df.r > 0) {
      pvalue <- 2 * pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(names(coef.p), c(dn,
                                                    "t value", "Pr(>|t|)"))
    }
    else {
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(names(coef.p), c(dn,
                                                    "t value", "Pr(>|t|)"))
    }
    df.f <- NCOL(Qr$qr)
  }
  else {
    coef.table <- matrix(, 0L, 4L)
    dimnames(coef.table) <- list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix(, 0L, 0L)
    df.f <- length(aliased)
  }
  keep <- match(c("call", "terms", "family", "deviance", "aic",
                  "contrasts", "df.residual", "null.deviance", "df.null",
                  "iter", "na.action"), names(object), 0L)
  ans <- c(object[keep], list(deviance.resid = residuals(object, type = "deviance"),
                              coefficients = coef.table, aliased = aliased,
                              dispersion = dispersion, df = c(object$rank, df.r, df.f),
                              cov.unscaled = covmat.unscaled, cov.scaled = covmat))
  if (correlation && p > 0) {
    dd <- sqrt(diag(covmat.unscaled))
    ans$correlation <- covmat.unscaled/outer(dd, dd)
    ans$symbolic.cor <- symbolic.cor
  }
  class(ans) <- "summary.glm"
  return(ans)
}
