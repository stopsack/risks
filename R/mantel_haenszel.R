#' Risk Ratios and Risk Differences from Mantel-Haenszel Estimators
#'
#' This function implements the Mantel-Haenszel estimators for risk ratio and
#' risk differences for a binary or categorical exposure and one or more
#' categorical confounder(s). Compare to estimates from regression models.
#'
#' @param data Data set.
#' @param exposure Exposure variable. Must be binary or categorical. The first
#'   level is treated as unexposed.
#' @param outcome Outcome variable. Must be binary.
#' @param confounders Optional. Binary or categorical variable(s) to perform
#'   stratification over. Supply more than one variable using
#'   \code{confounders = c(var1, var2)}.
#' @param estimand Optional. \code{"rr"} for risk ratio; \code{"rd"} for risk
#'   difference. Defaults to \code{"rr"}.
#' @param conf.level Optional. Confidence level. Defaults to \code{0.95}.
#'
#' @references
#' Greenland S, Rothman KJ. Introduction to Stratified Analysis. In: Rothman KJ,
#' Greenland S, Lash TL. Modern Epidemiology. 3rd edn. Lippincott Williams &
#' Wilkins: Philadelphia, PA 2008. Page 275. Risk ratios: formulae 15-18, -20,
#' -22. Risk differences: formulae 15-18, -19, -21.
#'
#' @return
#' Tibble in \code{\link[broom]{tidy}} format with
#' * \code{term} the (non-reference) exposure levels
#' * \code{estimate} Risk ratio (on log scale) or risk difference
#' * \code{std.error}, \code{conf.low}, and \code{conf.high}  Square-root of M-H
#'   variance estimate, and the corresponding confidence limits (on log scale
#'   for RR)
#' * \code{model} always \code{"mh"}
#' * \code{estimand} \code{"rr"} or \code{"rd"}
#' @export
#'
#' @examples
#' # Newman SC. Biostatistical methods in epidemiology. New York, NY:
#' # Wiley, 2001, table 5.3
#'
#' library(tibble)  # used to set up example data
#' dat <- tibble(
#'   death    = c(rep(1, 54), rep(0, 138)),
#'   stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
#'                rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
#'   receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
#'                rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
#'                rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))
#'
#' # Risk difference
#' rr_rd_mantel_haenszel(
#'   data = dat,
#'   exposure = stage,
#'   outcome = death,
#'   confounders = receptor,
#'   estimand = "rd")
#'
#' # Risk ratio, log scale:
#' result <- rr_rd_mantel_haenszel(
#'   data = dat,
#'   exposure = stage,
#'   outcome = death,
#'   confounders = receptor,
#'   estimand = "rr")
#' result
#'
#' # Risk ratio, exponentiated:
#' result %>%
#'   dplyr::mutate(dplyr::across(.cols = c(estimate, conf.low, conf.high),
#'                               .fns = exp))
rr_rd_mantel_haenszel <- function(data, exposure, outcome, confounders,
                                  estimand = c("rr", "rd"),
                                  conf.level = 0.95) {
  estimand <- match.arg(estimand)
  zval <- stats::qnorm(1 - (1 - conf.level) / 2)
  expname <- data %>%
    dplyr::select({{ exposure }}) %>%
    names()
  explvls <- levels(x = data %>%
                      dplyr::mutate(exp = factor({{ exposure }})) %>%
                      dplyr::pull(exp))
  confounder_vars <- data %>%
    dplyr::select({{ confounders }}) %>%
    names()

  data <- data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::all_of(confounder_vars),
                                   {{ exposure }})) %>%
    dplyr::summarize(a = sum({{ outcome }}),
                     b = dplyr::n() - .data$a,
                     .groups = "drop") %>%
    dplyr::mutate(exp = as.numeric(factor({{ exposure }})) - 1) %>%
    dplyr::mutate(a0 = dplyr::if_else(.data$exp == 0,
                                      true = .data$a, false = NA_real_),
                  b0 = dplyr::if_else(.data$exp == 0,
                                      true = .data$b, false = NA_real_)) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::all_of(confounder_vars))) %>%
    tidyr::fill(c("a0", "b0"), .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$exp != 0) %>%
    dplyr::select(-"exp") %>%
    dplyr::mutate(n = .data$a + .data$b,
                  n0 = .data$a0 + .data$b0,
                  n_total = .data$n + .data$n0,
                  w = (.data$n * .data$n0) / .data$n_total) %>%
    dplyr::group_by({{ exposure }})
  if(estimand == "rd") {
    data %>%
      dplyr::mutate(num_rd = (.data$a * .data$n0 - .data$a0 * .data$n) /
                      .data$n_total,
                    num_var = .data$w^2 *
                      (.data$a * .data$b /
                         (.data$n^2 * (.data$n - 1)) +
                         .data$a0 * .data$b0 /
                         (.data$n0^2 * (.data$n0 - 1)))) %>%
      dplyr::summarize(
        term = paste0(expname, {{ exposure }}[1]),
        estimate  = sum(.data$num_rd) / sum(.data$w),
        std.error = sqrt(sum(.data$num_var) / sum(.data$w)^2),
        conf.low  = .data$estimate - zval * .data$std.error,
        conf.high = .data$estimate + zval * .data$std.error,
        model = "mh",
        estimand = estimand,
        .groups = "drop") %>%
      dplyr::select(-{{ exposure }})
  } else {
    data %>%
      dplyr::mutate(num_rr = .data$a * .data$n0 / .data$n_total,
                    den_rr = .data$a0 * .data$n / .data$n_total,
                    num_var = (.data$a + .data$a0) * .data$n *
                      .data$n0 / .data$n_total^2 -
                      .data$a * .data$a0 / .data$n_total) %>%
      dplyr::summarize(
        term = paste0(expname, {{ exposure }}[1]),
        estimate  = log(sum(.data$num_rr) / sum(.data$den_rr)),
        std.error = sqrt(sum(.data$num_var) /
                           (sum(.data$num_rr) *
                              sum(.data$den_rr))),
        conf.low  = .data$estimate - zval * .data$std.error,
        conf.high = .data$estimate + zval * .data$std.error,
        model = "mh",
        estimand = estimand,
        .groups = "drop") %>%
      dplyr::select(-{{ exposure }})
  }
}
