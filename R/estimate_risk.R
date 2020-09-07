#' Fit risk ratio and risk difference models
#'
#' @description
#'
#' \code{estimate_risk} Fits risk ratio and risk difference models.
#'
#' \code{estimate_risk} provides a flexible interface to fitting
#' risk ratio and risk difference models.
#' Implemented are Poisson models with robust covariance, binomial models,
#' binomial models aided in convergence by starting values obtained
#' through Poisson models, binomial models fitted via
#' combinatorial expectation maximization, and estimates
#' obtained via regression standardization.
#'
#' @import stats tidyverse
#' @importFrom addreg addreg
#' @importFrom logbin logbin
#'
#' @param formula A formula object of the form 'response ~ predictors'
#' @param data A \code{tibble} or \code{data.frame} object
#' @param estimate Optional: the type of estimate to report: risk ratio
#' (\code{"rr"}, default) or risk difference (\code{"rd"})
#' @param approach Optional: Method for model fitting.
#' \code{"auto"} (default) is recommended; it selects the most efficient
#'   approach that converges and ensures that predicted probabilities are
#'   within range (< 1; see Details). \code{"all"} will attempt to fit
#'   the model via all implemented approaches to allow for comparisons.
#'   The other options allow for directly selecting a fitting approach,
#'   which may not converge or yield out-of-range  predicted probabilities.
#' @param variable Optional: variable use for marginal standardization.
#'   If \code{variable} is not provided and marginal standardization is
#'   attempted, then the first binary or categorical variable in the model
#'   is used as the exposure. Levels are determined automatically for
#'   variables types \code{logical}, \code{character}, \code{factor},
#'   and \code{numeric} (only if no more than 2 levels for the latter);
#'   otherwise levels must be supplied via \code{at =}.
#' @param at Optional: Levels of variable \code{variable} for marginal
#'   standardization. \code{at =} determines the levels at which contrasts of the exposure
#'   are to be assessed. The level listed first is used as the reference.
#'   Levels must exist in the data for character or factor variables.
#'   For numeric variables, levels that do not exist in the data
#'   can be interpolations or extrapolations; if levels exceed the
#'   extremes of the data (extrapolation), a warning will be displayed.
#' @param ... further arguments passed to fitting functions (\code{glm},
#'   \code{logbin}, or \code{addreg})
#'
#' @references Wacholder S. Binomial regression in GLIM: Estimating risk ratios
#'   and risk differences. Am J Epidemiol 1986;123:174-184.
#'   (Binomial regression models; approach = "glm")
#' @references Spiegelman D, Hertzmark E. Easy SAS Calculations for Risk or
#'   Prevalence Ratios and Differences. Am J Epidemiol 2005;162:199-200.
#'   (Binomial models fitted used starting values from Poisson models;
#'   approach = "glm_start")
#' @references Zou G. A modified Poisson regression approach to prospective
#'   studies with binary data. Am J Epidemiol 2004;159:702-706.
#'   (Poisson model with robust/sandwich standard errors;
#'   approach = "robpoisson")
#' @references Donoghoe MW, Marschner IC. logbin: An R Package for
#'   Relative Risk Regression Using the Log-Binomial Model.
#'   J Stat Softw 2018;86(9). (Log-binomial models fitted via combinatorial
#'   expectation maximization; estimate = "rr", approach = "glm_cem")
#' @references Donoghoe MW, Marschner IC. Stable computational methods
#'   for additive binomial models with application to adjusted risk differences.
#'   Comput Stat Data Anal 2014;80:184-96. (Additive binomial models
#'   fitted via combinatorial expectation maximization;
#'   estimate = "rd", approach = "glm_cem")
#' @references Localio AR, Margolis DJ, Berlin JA.
#'   Relative risks and confidence intervals were easily computed
#'   indirectly from multivariable logistic regression.
#'   J Clin Epidemiol 2007;60(9):874-82. (Marginal standardization after
#'   logistic model; approach = "margstd)
#'
#' @export
#' @return Fitted model
#'
#' @examples
#' # Newman SC. Biostatistical methods in epidemiology. New York, NY: Wiley, 2001, table 5.3
#' library(tibble)
#' dat <- tibble(
#'   id = 1:192,
#'   death = c(rep(1, 54), rep(0, 138)),
#'   stage = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
#'             rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
#'   receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
#'                rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
#'                rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))
#' fit_rr <- estimate_risk(formula = death ~ stage + receptor, data = dat)
#' fit_rr
#' summary(fit_rr)


estimate_risk <- function(formula, data,
                          estimate = c("rr", "rd"),
                          approach = c("auto", "all", "robpoisson", "glm", "glm_start",
                                       "glm_cem", "glm_cem_start", "margstd", "logistic"),
                          variable = NULL,
                          at = NULL,
                          ...) {
  implausible <- 0.99999
  link <- switch(EXPR = estimate[1], rr = "log", rd = "identity")
  if(is.null(link))
    stop(paste0("Unknown estimate '", estimate,
                "'. Possible are 'rr', relative risks; 'rd', risk differences."))
  if(!(approach[1] %in% c("auto", "all", "robpoisson", "glm", "glm_start",
                          "glm_cem", "glm_cem_start", "margstd", "logistic")))
    stop(paste("Approach '", approach[1], "' is not implemented."))

  fit <- switch(EXPR = approach[1],
                # Automated model fitting
                auto = {
                  # 1) try regular GLM with Fisher scoring
                  fit_glm <- possibly_estimate_glm(formula = formula, data = data, link = link, ...)
                  if(fit_glm$converged == TRUE &
                     fit_glm$maxprob < implausible &
                     fit_glm$boundary == FALSE)
                    return(fit_glm)

                  # 2) try GLM with starting values from Poisson
                  fit_poisson <- possibly_estimate_poisson(formula = formula, data = data, link = link, ...)
                  if(fit_poisson$converged == TRUE) {
                    fit_glm_start <- possibly_estimate_glm(formula = formula, data = data, link = link,
                                                           start = coef(fit_poisson), ...)
                    if(fit_glm_start$converged == TRUE &
                       fit_glm_start$maxprob < implausible  &
                       fit_glm_start$boundary == FALSE)
                      return(fit_glm_start)
                  }

                  # 3) try GLM fitted via CEM
                  if(link == "log")
                    fit_glm_cem <- possibly_estimate_logbin(formula = formula, data = data, ...)
                  else
                    fit_glm_cem <- possibly_estimate_addreg(formula = formula, data = data, ...)

                  if(fit_glm_cem$converged == TRUE &
                     fit_glm_cem$maxprob < implausible  &
                     fit_glm_cem$boundary == FALSE)
                    return(fit_glm_cem)

                  # 4) Try marginal standardization after logistic model
                  fit_margstd <- possibly_estimate_margstd(formula = formula, data = data,
                                                           estimate = estimate, ...)
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
                  fit1 <- possibly_estimate_poisson(formula = formula, data = data, link = link, ...)
                  if(fit1$converged == FALSE)
                    class(fit1) <- c("risks", "robpoisson", "glm", "lm")

                  fit2 <- possibly_estimate_glm(formula = formula, data = data, link = link, ...)
                  if(fit2$converged == FALSE)
                    class(fit2) <- c("risks", "glm", "lm")

                  if(!is.null(coef(fit1)))  # attempt only if Poisson converged
                    fit3 <- possibly_estimate_glm(formula = formula, data = data, link = link,
                                                  start = coef(fit1), ...)
                  else  # make possibly_estimate_glm return a non-converged object
                    fit3 <- possibly_estimate_glm(formula = "nonsense", data = "nodata")
                  if(fit3$converged == FALSE)
                    class(fit3) <- c("risks", "glm_start", "glm", "lm")

                  if(link == "log") {
                    fit4 <- possibly_estimate_logbin(formula = formula, data = data, ...)
                    if(fit4$converged == FALSE)
                      class(fit4) <- c("risks", "logbin", "glm", "lm")
                  } else {
                    fit4 <- possibly_estimate_addreg(formula = formula, data = data, ...)
                    if(fit4$converged == FALSE)
                      class(fit4) <- c("risks", "addreg", "glm", "lm")
                  }

                  if(link == "log") {
                    if(!is.null(coef(fit1))) # attempt only if Poisson converged
                      fit5 <- possibly_estimate_logbin(formula = formula, data = data,
                                                       start = coef(fit1), ...)
                    else
                      fit5 <- possibly_estimate_logbin(formula = "nonsense", data = "nodata")
                    if(fit5$converged == FALSE)
                      class(fit5) <- c("risks", "logbin", "glm", "lm")
                  } else {
                    if(!is.null(coef(fit1))) # attempt only if Poisson converged
                      fit5 <- possibly_estimate_addreg(formula = formula, data = data,
                                                       start = coef(fit1), ...)
                    else
                      fit5 <- possibly_estimate_addreg(formula = "nonsense", data = "nodata")
                    if(fit5$converged == FALSE)
                      class(fit5) <- c("risks", "addreg", "glm", "lm")
                  }

                  fit6 <- possibly_estimate_margstd(formula = formula, data = data,
                                                    estimate = estimate,
                                                    variable = variable, at = at, ...)
                  if(fit6$converged == FALSE)
                    class(fit6) <- c("risks", "margstd", "glm", "lm")

                  # If RR requested, add on plain logistic model for comparison
                  if(estimate[1] == "rr") {
                    fit7 <- possibly_estimate_logistic(formula = formula, data = data, ...)
                    if(fit7$converged == FALSE)
                      class(fit7) <- c("risks", "logistic", "glm", "lm")

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
                robpoisson = estimate_poisson(formula = formula, data = data, link = link, ...),
                glm        = estimate_glm(formula = formula, data = data, link = link, ...),
                glm_start  = {
                  fit_poisson <- estimate_poisson(formula = formula, data = data, link = link, ...)
                  estimate_glm(formula = formula, data = data, link = link, start = coef(fit_poisson), ...)
                },
                glm_cem    = {
                  if(link == "log")
                    estimate_logbin(formula = formula, data = data, ...)
                  else
                    estimate_addreg(formula = formula, data = data, ...)
                },
                glm_cem_start = {
                  fit_poisson <- estimate_poisson(formula = formula, data = data, link = link, ...)
                  if(link == "log")
                    estimate_logbin(formula = formula, data = data, start = coef(fit_poisson), ...)
                  else
                    estimate_addreg(formula = formula, data = data, start = coef(fit_poisson), ...)
                },
                margstd    = estimate_margstd(formula = formula, data = data, estimate = estimate,
                                              variable = variable, at = at, ...),
                logistic = {
                  if(estimate[1] == "rd")
                    stop("Odds difference models are not implemented.")
                  estimate_logistic(formula = formula, data = data, ...)
                })
  return(fit)
}
