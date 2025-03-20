# Helper functions for estimate_risk(approach = "duplicate") and
# the use of case duplication in approach = "auto" or approach = "all"

#' @import stats

# estimate_duplicate: internal, fitting a logistic model with case duplication
# (Miettinen/Schouten approach to directly estimating relative risks)
estimate_duplicate <- function(formula, data, ...) {
  yvar <- as.character(all.vars(formula)[1])
  data <- data %>%
    dplyr::mutate(.clusterid = dplyr::row_number())
  data <- dplyr::bind_rows(data,
                           data %>%
                             dplyr::rename(outc = dplyr::one_of(!!yvar)) %>%
                             dplyr::filter(.data$outc == 1) %>%
                             dplyr::mutate(outc = 0) %>%
                             dplyr::rename(!!yvar := "outc"))
  fit <- eval(substitute(stats::glm(formula = formula,
                                    family = binomial(link = "logit"),
                                    data = data)))
  class(fit) <- c("duplicate", class(fit))
  fit <- estimate_maxprob(fit = fit, formula = formula, data = data,
                          link = "logit")
  return(fit)
}


#' Clustering-corrected confidence intervals for case duplication model
#'
#' Estimate confidence intervals for the case duplication model
#' with robust/sandwich/empirical covariance structure.
#'
#' @param object Fitted model
#' @param parm Not used
#' @param level Confidence level, defaults to 0.95
#' @param ... Additional arguments, not used
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @export
#' @noRd
confint.duplicate <- function(object, parm = NULL, level = 0.95, ...) {
  # modified after stats:::confint.default()
  cf <- coef(object)
  pnames <- names(cf)
  #if (missing(parm))
  parm <- pnames
  #else if (is.numeric(parm))
  #  parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3),
               "%")
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  # Robust covariance accounting for clustering
  obj_sandwich <- object
  class(obj_sandwich) <- "glm"
  ses <- sqrt(diag(sandwich::sandwich(
    x = obj_sandwich,
    bread. = sandwich::bread(obj_sandwich),
    meat. = sandwich::meatCL(obj_sandwich,
                             type = "HC0",
                             cluster = object$data$.clusterid))))
  ci[] <- cf[parm] + ses %o% fac
  return(ci)
}


#' Summary for logistic model with case duplication and cluster-robust covariance
#'
#' Summarize results from fitting a logistic model with case duplication and
#' cluster-robust covariance.
#' The output is the same as for a regular \code{summary(glm(...))},
#' except for using cluster-robust standard errors.
#'
#' @param object Model
#' @param dispersion Not used
#' @param correlation Not used
#' @param symbolic.cor Not used
#' @param ... Other arguments, not used
#'
#' @return Model summary (list)
#' @export
summary.duplicate <- function(object, dispersion = NULL, correlation = FALSE,
                              symbolic.cor = FALSE, ...) {
  # a modification of summary.glm()
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
    # covmat.unscaled <- sandwich::vcovCL(object, type = "HC0",
    #                                     cluster = object$data$.clusterid)
    obj_sandwich <- object
    class(obj_sandwich) <- "glm"
    covmat.unscaled <- sandwich::sandwich(
      x = obj_sandwich,
      bread. = sandwich::bread(x = obj_sandwich),
      meat. = sandwich::meatCL(x = obj_sandwich,
                               type = "HC0",
                               cluster = object$data$.clusterid))
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
