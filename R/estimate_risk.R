#' Fit risk ratio and risk difference models
#'
#' @description
#'
#' \code{riskratio} and \code{riskdiff} provide a flexible interface to fitting
#' risk ratio and risk difference models.
#'
#' In cohort studies with a binary outcome, risk ratios and risk differences
#' are typically more appropriate to report than odds ratios from logistic regression,
#' yet such models have historically been difficult to implement in standard software.
#'
#' The risks package selects an efficient way to fit risk ratio or
#' risk difference models successfully, which will converge whenever logistic models converge.
#' Optionally, a specific approach to model fitting can also be requested.
#' Implemented are Poisson models with robust covariance, binomial models,
#' binomial models aided in convergence by starting values obtained
#' through Poisson models, binomial models fitted via
#' combinatorial expectation maximization (optionally also with Poisson starting values),
#' and estimates obtained via marginal standardization after logistic regression.
#'
#' Adjusting for covariates (e.g., confounders) in the model specification (\code{formula =}) is possible.
#'
#' @import stats
#' @import addreg
#' @importFrom logbin logbin
#'
#' @param formula A formula object of the form 'response ~ predictors'.
#' @param data A \code{tibble} or \code{data.frame} object.
#' @param approach Optional: Method for model fitting.
#'   * \code{"auto"} (default) is recommended; it selects the most efficient
#'     approach that converges and ensures that predicted probabilities are
#'     within range (< 1; see Details).
#'   * \code{"all"} will attempt to fit
#'     the model via all implemented approaches to allow for comparisons.
#'
#'   The other options allow for directly selecting a fitting approach,
#'   some of which may not converge or yield out-of-range predicted probabilities.
#'   See full documentation (currently at \url{https://github.com/stopsack/risks}) for details.
#'
#'   * \code{"glm"} Binomial model.
#'   * \code{"glm_start"} Binomial model with starting values from Poisson model.
#'   * \code{"robpoisson"} Poisson model with robust covariance.
#'   * \code{"glm_cem"} Binomial model fitted with combinatorial expectation maximization.
#'   * \code{"glm_cem_start"} As \code{glm_cem}, with Poisson starting values.
#'   * \code{"margstd"} Marginal standardization after logistic model.
#'   * \code{"logistic"} For comparison only: the logistic model. Only available in \code{riskratio()}.
#' @param variable Optional: exposure variable to use for marginal standardization.
#'   If \code{variable} is not provided and marginal standardization is
#'   attempted, then the first binary or categorical variable in the model
#'   is used as the exposure. Levels are determined automatically for
#'   variables types \code{logical}, \code{character}, \code{factor},
#'   and \code{numeric} (only if no more than 2 levels for the latter);
#'   otherwise levels must be supplied via \code{at =}.
#' @param at Optional: Levels of exposure variable \code{variable} for marginal
#'   standardization. \code{at =} determines the levels at which contrasts of the exposure
#'   are to be assessed. The level listed first is used as the reference.
#'   Levels must exist in the data for character, factor or ordered factor variables.
#'   For numeric variables, levels that do not exist in the data
#'   can be interpolations or extrapolations; if levels exceed the
#'   extremes of the data (extrapolation), a warning will be displayed.
#' @param weights Precision weights for individual observations (integers).
#'   See help for \code{\link[stats]{glm}}.
#'   Precision weights are not supported by approaches \code{"glm_cem"} and \code{"glm_cem_start"}.
#'   Not to be confused with frequency or sampling weights;
#'   use \code{\link[survey]{svyglm}} if such weights are needed.
#' @param ... Optional: Further arguments passed to fitting functions (\code{glm},
#'   \code{logbin}, or \code{addreg}).
#'
#' @references Wacholder S. Binomial regression in GLIM: Estimating risk ratios
#'   and risk differences. Am J Epidemiol 1986;123:174-184.
#'   (Binomial regression models; \code{approach = "glm"})
#' @references Spiegelman D, Hertzmark E. Easy SAS Calculations for Risk or
#'   Prevalence Ratios and Differences. Am J Epidemiol 2005;162:199-200.
#'   (Binomial models fitted used starting values from Poisson models;
#'   \code{approach = "glm_start"})
#' @references Zou G. A modified Poisson regression approach to prospective
#'   studies with binary data. Am J Epidemiol 2004;159:702-706.
#'   (Poisson model with robust/sandwich standard errors;
#'   \code{approach = "robpoisson"})
#' @references Donoghoe MW, Marschner IC. logbin: An R Package for
#'   Relative Risk Regression Using the Log-Binomial Model.
#'   J Stat Softw 2018;86(9). (Log-binomial models fitted via combinatorial
#'   expectation maximization; \code{riskratio(approach = "glm_cem")}
#' @references Donoghoe MW, Marschner IC. Stable computational methods
#'   for additive binomial models with application to adjusted risk differences.
#'   Comput Stat Data Anal 2014;80:184-96. (Additive binomial models
#'   fitted via combinatorial expectation maximization;
#'   \code{riskdiff(approach = "glm_cem")})
#' @references Localio AR, Margolis DJ, Berlin JA.
#'   Relative risks and confidence intervals were easily computed
#'   indirectly from multivariable logistic regression.
#'   J Clin Epidemiol 2007;60(9):874-82. (Marginal standardization after fitting a
#'   logistic model; \code{approach = "margstd"})
#'
#' @export
#' @return Fitted model. This object can be passed on to post-processing functions:
#'   * \code{\link[risks]{summary.risks}}: an overview of results (risks-specific S3 methods:
#'     \code{\link[risks]{summary.robpoisson}}, \code{\link[risks]{summary.margstd}}).
#'   * \code{\link[risks]{tidy.risks}}: a tibble of coefficients and confidence intervals.
#'
#'   Standard post-processing functions can also be used:
#'
#'   * \code{\link[stats]{coef}}: a vector of coefficients.
#'   * \code{\link[stats]{confint}}: a matrix of confidence intervals (risks-specific S3 methods:
#'     \code{\link[risks]{confint.robpoisson}}, \code{\link[risks]{confint.margstd}}).
#'   * \code{\link[stats]{predict.glm}(type = "response")}: fitted values (predictions).
#'   * \code{\link[stats]{residuals}}: residuals.
#'
#'   If model fitting using all possible approaches was requested via
#'   \code{approach = "all"}, then their results can be retrieved from the
#'   list \code{all_models} in the returned object (e.g., \code{fit$all_models[[1]]},
#'   \code{fit$all_models[[2]]}, etc.).
#' @describeIn riskratio Fit risk ratio models
#'
#' @examples
#' # Newman SC. Biostatistical methods in epidemiology. New York, NY: Wiley, 2001, table 5.3
#' library(tibble)  # used to set up example data
#' dat <- tibble(
#'   death    = c(rep(1, 54), rep(0, 138)),
#'   stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
#'                rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
#'   receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
#'                rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
#'                rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))
#'
#' # Risk ratio model
#' fit_rr <- riskratio(formula = death ~ stage + receptor, data = dat)
#' fit_rr
#' summary(fit_rr)
#'
#' # Risk difference model
#' fit_rd <- riskdiff(formula = death ~ stage + receptor, data = dat)
#' fit_rd
#' summary(fit_rd)
riskratio <- function(formula, data, approach = c("auto", "all", "robpoisson", "glm", "glm_start",
                                                  "glm_cem", "glm_cem_start", "margstd", "logistic"),
                      variable = NULL, at = NULL, weights = NULL, ...) {
  if(deparse(substitute(weights)) == "NULL")
    estimate_risk(formula = formula, data = data, estimate = "rr",
                  approach = approach, variable = variable, at = at, ...)
  else
    eval(substitute(estimate_risk(formula = formula, data = data, estimate = "rr",
                                  approach = approach, variable = variable, at = at,
                                  weights = weights, ...)))
}

#' @describeIn riskratio Fit risk difference models
#' @export
riskdiff <- function(formula, data, approach = c("auto", "all", "robpoisson", "glm", "glm_start",
                                                 "glm_cem", "glm_cem_start", "margstd", "logistic"),
                     variable = NULL, at = NULL, weights = NULL, ...) {
  if(deparse(substitute(weights)) == "NULL")
    estimate_risk(formula = formula, data = data, estimate = "rd",
                  approach = approach, variable = variable, at = at, ...)
  else
  eval(substitute(estimate_risk(formula = formula, data = data, estimate = "rd",
                approach = approach, variable = variable, at = at, weights = weights, ...)))
}

# Workhorse for riskratio and riskdiff
estimate_risk <- function(formula, data,
                          estimate = c("rr", "rd"),
                          approach = c("auto", "all", "robpoisson", "glm", "glm_start",
                                       "glm_cem", "glm_cem_start", "margstd", "logistic"),
                          variable = NULL,
                          at = NULL,
                          weights = NULL,
                          ...) {
  implausible <- 0.99999
  link <- switch(EXPR = estimate[1], rr = "log", rd = "identity")
  if(is.null(link))
    stop(paste0("Unknown estimate '", estimate,
                "'. Possible are 'rr', relative risks; 'rd', risk differences."))
  if(!(approach[1] %in% c("auto", "all", "robpoisson", "glm", "glm_start",
                          "glm_cem", "glm_cem_start", "margstd", "logistic")))
    stop(paste0("Approach '", approach[1], "' is not implemented."))

  fit <- switch(EXPR = approach[1],
                # Automated model fitting
                auto = {
                  # 1) try regular GLM with Fisher scoring
                  fit_glm <- eval(substitute(possibly_estimate_glm(formula = formula, data = data,
                                                                   link = link, weights = weights, ...)))
                  if(fit_glm$converged == TRUE &
                     fit_glm$maxprob < implausible &
                     fit_glm$boundary == FALSE)
                    return(fit_glm)

                  # 2) try GLM with starting values from Poisson
                  fit_poisson <- eval(substitute(possibly_estimate_poisson(formula = formula,
                                                                           data = data, link = link,
                                                                           weights = weights, ...)))
                  if(fit_poisson$converged == TRUE) {
                    fit_glm_start <- eval(substitute(possibly_estimate_glm(formula = formula, data = data,
                                                                           link = link,
                                                                           start = coef(fit_poisson),
                                                                           weights = weights, ...)))
                    if(fit_glm_start$converged == TRUE &
                       fit_glm_start$maxprob < implausible  &
                       fit_glm_start$boundary == FALSE)
                      return(fit_glm_start)
                  }

                  # 3) try GLM fitted via CEM (only if unweighted; addreg/logbin do not support weights)
                  if(deparse(substitute(weights)) != "NULL") {
                    if(link == "log")
                      fit_glm_cem <- possibly_estimate_logbin(formula = formula, data = data, ...)
                    else
                      fit_glm_cem <- possibly_estimate_addreg(formula = formula, data = data, ...)

                    if(fit_glm_cem$converged == TRUE &
                       fit_glm_cem$maxprob < implausible  &
                       fit_glm_cem$boundary == FALSE)
                      return(fit_glm_cem)
                  }

                  # 4) Try marginal standardization after logistic model
                  fit_margstd <- eval(substitute(possibly_estimate_margstd(formula = formula,
                                                                           data = data,
                                                                           estimate = estimate,
                                                                           weights = weights, ...)))
                  if(fit_margstd$converged == TRUE &
                     fit_margstd$maxprob < implausible &
                     fit_margstd$boundary == FALSE)
                    return(fit_margstd)

                  # 5) If 1-4 do not work, return at least the Poisson model
                  if(fit_poisson$converged == TRUE &
                     fit_poisson$maxprob < implausible &
                     fit_margstd$boundary == FALSE) {
                    warning("Only the Poisson model converged")
                    return(fit_poisson)
                  }

                  stop("No model converged or had within-range predicted probabilities of < 1.")
                },

                # All models requested to fit
                all  = {
                  fit1 <- eval(substitute(possibly_estimate_poisson(formula = formula, data = data,
                                                                    link = link, weights = weights, ...)))

                  fit2 <- eval(substitute(possibly_estimate_glm(formula = formula, data = data,
                                                                link = link, weights = weights, ...)))

                  if(!is.null(coef(fit1)))  # attempt only if Poisson converged
                    fit3 <- eval(substitute(possibly_estimate_glm(formula = formula, data = data,
                                                                  link = link, start = coef(fit1),
                                                                  weights = weights, ...)))
                  else  # make possibly_estimate_glm return a non-converged object
                    fit3 <- possibly_estimate_glm(formula = "nonsense", data = "nodata")

                  if(link == "log") {
                    if(deparse(substitute(weights)) == "NULL")
                      fit4 <- possibly_estimate_logbin(formula = formula, data = data, ...)
                    else  # logbin/addreg do not support weights
                      fit4 <- possibly_estimate_logbin(formula = "nosense", data = "nodata")
                  } else {
                    if(deparse(substitute(weights)) == "NULL")
                      fit4 <- possibly_estimate_addreg(formula = formula, data = data, ...)
                    else
                      fit4 <- possibly_estimate_addreg(formula = "nonsense", data = "nodata")
                  }

                  if(link == "log") {
                    if(deparse(substitute(weights)) == "NULL") {
                      if(!is.null(coef(fit1))) # attempt only if Poisson converged
                        fit5 <- possibly_estimate_logbin(formula = formula, data = data,
                                                         start = coef(fit1), ...)
                      else
                        fit5 <- possibly_estimate_logbin(formula = "nonsense", data = "nodata")
                      if(fit5$converged == FALSE)
                        fit5$risks_start = "_start"
                    } else {
                      fit5 <- possibly_estimate_logbin(formula = "nosense", data = "nodata")
                      fit5$risks_start = "_start"
                    }
                  } else {
                    if(deparse(substitute(weights)) == "NULL") {
                      if(!is.null(coef(fit1))) # attempt only if Poisson converged
                        fit5 <- possibly_estimate_addreg(formula = formula, data = data,
                                                         start = coef(fit1), ...)
                      else
                        fit5 <- possibly_estimate_addreg(formula = "nonsense", data = "nodata")
                      if(fit5$converged == FALSE)
                        fit5$risks_start = "_start"
                    } else {
                      fit5 <- possibly_estimate_addreg(formula = "nonsense", data = "nodata")
                      fit5$risks_start = "_start"
                    }
                  }

                  fit6 <- eval(substitute(possibly_estimate_margstd(formula = formula, data = data,
                                                                    estimate = estimate,
                                                                    variable = variable, at = at,
                                                                    weights = weights, ...)))

                  # If RR requested, add on plain logistic model for comparison
                  if(estimate[1] == "rr") {
                    fit7 <- eval(substitute(possibly_estimate_logistic(formula = formula, data = data,
                                                                       weights = weights, ...)))

                    fit1$all_models = list(
                      model1 = fit1, model2 = fit2, model3 = fit3, model4 = fit4,
                      model5 = fit5, model6 = fit6, model7 = fit7)
                  } else
                    fit1$all_models = list(
                      model1 = fit1, model2 = fit2, model3 = fit3, model4 = fit4,
                      model5 = fit5, model6 = fit6)
                  fit1
                },

                # Specific models that were directly requested
                robpoisson = eval(substitute(estimate_poisson(formula = formula, data = data,
                                                              link = link, weights = weights, ...))),
                glm        = eval(substitute(estimate_glm(formula = formula, data = data,
                                                          link = link, weights = weights, ...))),
                glm_start  = {
                  fit_poisson <- eval(substitute(estimate_poisson(formula = formula, data = data,
                                                                  link = link, weights = weights, ...)))
                  eval(substitute(estimate_glm(formula = formula, data = data, link = link,
                                               start = coef(fit_poisson), weights = weights, ...)))
                },
                glm_cem    = {
                  if(deparse(substitute(weights)) != "NULL")
                    stop(paste("A weighted model was requested, but approach = 'glm_cem' does not",
                               "support weights."))
                  if(link == "log")
                    estimate_logbin(formula = formula, data = data, ...)
                  else
                    estimate_addreg(formula = formula, data = data, ...)
                },
                glm_cem_start = {
                  if(deparse(substitute(weights)) != "NULL")
                    stop(paste("A weighted model was requested, but approach = 'glm_cem' does not",
                               "support weights."))
                  fit_poisson <- estimate_poisson(formula = formula, data = data, link = link, ...)
                  if(link == "log")
                    estimate_logbin(formula = formula, data = data, start = coef(fit_poisson), ...)
                  else
                    estimate_addreg(formula = formula, data = data, start = coef(fit_poisson), ...)
                },
                margstd    = eval(substitute(estimate_margstd(formula = formula, data = data,
                                                              estimate = estimate,
                                                              variable = variable, at = at,
                                                              weights = weights, ...))),
                logistic = {
                  if(estimate[1] == "rd")
                    stop(paste0("Odds difference models are not implemented.\n",
                    "Further reading: Wacholder S. The Case-Control Study ",
                    "as Data Missing by Design: Estimating Risk Differences. ",
                    "Epidemiology 1996;7:144-150."))
                  eval(substitute(estimate_logistic(formula = formula, data = data,
                                                    weights = weights, ...)))
                })
  return(fit)
}
