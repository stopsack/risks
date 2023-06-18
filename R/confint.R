#' Confidence Intervals for Model Parameters
#'
#' @description
#' Estimates confidence intervals for the fitted risks model. For binomial
#' models, large-sample confidence intervals, not profile likelihood, are used.
#'
#' @param object A fitted risks model
#' @param parm Not used, for compatibility
#' @param level Optional. Confidence level. Defaults to \code{0.95}.
#' @param bootrepeats Optional and only applicable to
#'   \code{approach = "margstd_boot"}: bootstrap repeats. Defaults to
#'   \code{1000}. Consider increasing.
#' @param bootci Optional and only applicable to
#'   \code{approach = "margstd_boot"}: type of bootstrap confidence interval.
#'   Available methods:
#'
#'   * \code{"bca"} Default. Parametric BCa (bias-corrected accelerated)
#'     bootstrap confidence intervals.
#'   * \code{"normal"} Parametric normality-based confidence intervals,
#'     which require lower repeat numbers but are less accurate and
#'     may result in invalid results for ratios.
#'   * \code{"nonpar"} Non-parametric BCa confidence intervals,
#'     which should be used with caution because of the risk
#'     of sparse-data bias with non-parametric bootstrapping.
#' @param jacksd Optional and only applicable to
#'   \code{approach = "margstd_boot"}: Also return jackknife estimate of
#'   Monte-Carlo error for the confidence limits? Only functional with BCa
#'   confidence intervals. Defaults to \code{FALSE}.
#' @param ... Passed on.
#'
#' @return Matrix: First column, lower bound; second column, upper bound.
#' @method confint risks
#' @export
#'
#' @examples
#' confint(
#'   riskratio(
#'     formula = death ~ stage + receptor,
#'     data = breastcancer))
confint.risks <- function(
    object,
    parm,
    level = 0.95,
    bootrepeats = 1000,
    bootci = "bca",
    jacksd = FALSE,
    ...) {
  if(inherits(
    x = object,
    what = c("robpoisson", "duplicate", "margstd_boot", "margstd_delta"))) {
    NextMethod()
  } else {
    stats::confint.default(object = object, level = level, ...)
  }
}
