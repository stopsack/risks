#' risks: Estimate risk ratios and risk differences using regression
#'
#' The risks package allows for fitting risk ratio and risk difference
#' models using regression.
#'
#' @section risks functions:
#'
#' \code{\link[risks]{riskratio}}: Fit risk ratio models.
#'
#' \code{\link[risks]{riskdiff}}: Fit risk difference models.
#'
#' \code{\link[risks]{summary.risks}}: Summarize fitted model.
#'
#' \code{\link[risks]{tidy.risks}}: Tibble (data frame) of parameters,
#'   coefficients, standard errors, confidence intervals.
#'
#' \code{\link[risks]{confint.robpoisson}},
#' \code{\link[risks]{confint.duplicate}},
#' \code{\link[risks]{confint.margstd_boot}},
#' \code{\link[risks]{confint.margstd_delta}}: Confidence intervals. (Standard
#'   confidence intervals for generalized linear models are used for other
#'   models.)
#'
#' @docType package
#' @name risks
#' @seealso \url{https://github.com/stopsack/risks}
NULL
#> NULL
