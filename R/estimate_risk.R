#' Fit risk ratio and risk difference models
#'
#' @description
#'
#' \code{riskratio} and \code{riskdiff} provide a flexible interface to fitting
#' risk ratio and risk difference models.
#'
#' In cohort studies with a binary outcome, risk ratios and risk differences
#' are typically more appropriate to report than odds ratios from logistic
#' regression, yet such models have historically been difficult to implement in
#' standard software.
#'
#' The risks package selects an efficient way to fit risk ratio or
#' risk difference models successfully, which will converge whenever logistic
#' models converge. Optionally, a specific approach to model fitting can also be
#' requested. Implemented are Poisson models with robust covariance, binomial
#' models, logistic models with case duplication, binomial models aided in
#' convergence by starting values obtained through Poisson models or
#' logistic models with case duplication, binomial models fitted via
#' combinatorial expectation maximization (optionally also with Poisson starting
#' values), and estimates obtained via marginal standardization after logistic
#' regression with bootstrapped or delta method for confidence intervals.
#'
#' Adjusting for covariates (e.g., confounders) in the model specification
#' (\code{formula =}) is possible.
#'
#' @import stats
#'
#' @param formula A formula object of the form \code{response ~ predictors}.
#' @param data A \code{tibble} or \code{data.frame} object.
#' @param approach Optional: Method for model fitting.
#'   * \code{"auto"} (default) is recommended; it will return results of
#'     \code{"margstd_delta"} unless interaction terms between exposure and
#'     confounders are included. This these cases, results from
#'     \code{"margstd_boot"} are returned.
#'   * \code{"all"} will attempt to fit
#'     the model via all implemented approaches to allow for comparisons.
#'   * \code{"legacy"} selects the most efficient approach that converges and
#'     ensures that predicted probabilities are within range (< 1).
#'
#'   The other options allow for directly selecting a fitting approach,
#'   some of which may not converge or yield out-of-range predicted
#'   probabilities. See full documentation for details.
#'
#'   * \code{"glm"} Binomial model.
#'   * \code{"glm_startp"} Binomial model with starting values from Poisson
#'      model.
#'   * \code{"glm_startd"} Binomial model with starting values from logistic
#'     model with case duplication.
#'   * \code{"robpoisson"} Poisson model with robust covariance.
#'   * \code{"duplicate"} Logistic model with duplication of cases. Only
#'     available in \code{riskratio()}.
#'   * \code{"glm_cem"} Binomial model fitted with combinatorial expectation
#'     maximization.
#'   * \code{"glm_cem_startp"} As \code{glm_cem}, with Poisson starting values.
#'   * \code{"margstd_boot"} Marginal standardization after logistic model,
#'     bootstrap standard errors/confidence intervals.
#'   * \code{"margstd_delta"} Marginal standardization after logistic model,
#'     delta method standard errors/confidence intervals.
#'   * \code{"logistic"} For comparison only: the logistic model. Only available
#'     in \code{riskratio()}.
#' @param variable Optional: exposure variable to use for marginal
#'   standardization. If \code{variable} is not provided and marginal
#'   standardization is attempted, then the first variable in the model
#'   is used as the exposure. Levels are determined automatically for
#'   variables types \code{logical}, \code{character}, \code{factor} and can
#'   optionally be supplied via \code{at =}.
#' @param at Optional: Levels of exposure variable \code{variable} for marginal
#'   standardization. \code{at =} determines the levels at which contrasts of
#'   the exposure are to be assessed. The level listed first is used as the
#'   reference. Levels must exist in the data for character, factor or ordered
#'   factor variables. For numeric variables, levels that do not exist in the
#'   data can be interpolations or extrapolations; if levels exceed the
#'   extremes of the data (extrapolation), a warning will be displayed.
#' @param ... Optional: Further arguments passed to fitting functions
#'   (\code{glm}, \code{logbin}, or \code{addreg}).
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
#' @references Schouten EG, Dekker JM, Kok FJ, Le Cessie S, Van Houwelingen HC,
#'   Pool J, Vandenbroucke JP. Risk ratio and rate ratio estimation in
#'   case-cohort designs: hypertension and cardiovascular mortality.
#'   Stat Med 1993;12:1733–45; (Logistic model with case duplication and
#'   cluster-robust standard errors, \code{approach = "duplicate"}).
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
#'   J Clin Epidemiol 2007;60(9):874-82. (Marginal standardization after fitting
#'   a logistic model; \code{approach = "margstd_boot"})
#'
#' @export
#' @return Fitted model. This object can be passed on to post-processing
#'   functions:
#'   * \code{\link[risks]{summary.risks}}: an overview of results
#'     (risks-specific S3 methods: \code{\link[risks]{summary.robpoisson}},
#'     \code{\link[risks]{summary.margstd_boot}},
#'     \code{\link[risks]{summary.margstd_delta}}).
#'   * \code{\link[risks]{tidy.risks}}: a tibble of coefficients and confidence
#'     intervals.
#'
#'   Standard post-processing functions can also be used:
#'
#'   * \code{\link[stats]{coef}}: a vector of coefficients.
#'   * \code{\link[risks]{confint.risks}}: a matrix of confidence intervals
#'   * \code{\link[stats]{predict.glm}(type = "response")}: fitted values
#'     (predictions).
#'   * \code{\link[stats]{residuals}}: residuals.
#'
#'   If model fitting using all possible approaches was requested via
#'   \code{approach = "all"}, then their results can be retrieved from the
#'   list \code{all_models} in the returned object (e.g.,
#'   \code{fit$all_models[[1]]}, \code{fit$all_models[[2]]}, etc.).
#' @describeIn riskratio Fit risk ratio models
#'
#' @examples
#' data(breastcancer)  # Cohort study with binary outcome
#'                     # See for details: help(breastcancer)
#'
#' # Risk ratio model
#' fit_rr <- riskratio(formula = death ~ stage + receptor, data = breastcancer)
#' fit_rr
#' summary(fit_rr)
#'
#' # Risk difference model
#' fit_rd <- riskdiff(formula = death ~ stage + receptor, data = breastcancer)
#' fit_rd
#' summary(fit_rd)
riskratio <- function(
    formula,
    data,
    approach = c(
      "auto",
      "all",
      "robpoisson",
      "duplicate",
      "glm",
      "glm_startp",
      "glm_startd",
      "glm_cem",
      "glm_cem_startp",
      "margstd_boot",
      "margstd_delta",
      "logistic",
      "legacy"),
    variable = NULL,
    at = NULL,
    ...) {
  estimate_risk(
    formula = formula,
    data = data,
    estimand = "rr",
    approach = approach,
    variable = variable,
    at = at,
    ...)
}

#' @describeIn riskratio Fit risk difference models
#' @export
riskdiff <- function(
    formula,
    data,
    approach = c(
      "auto",
      "all",
      "robpoisson",
      "glm",
      "glm_startp",
      "glm_cem",
      "glm_cem_startp",
      "margstd_boot",
      "margstd_delta",
      "legacy"),
    variable = NULL,
    at = NULL, ...) {
  estimate_risk(
    formula = formula,
    data = data,
    estimand = "rd",
    approach = approach,
    variable = variable,
    at = at,
    ...)
}

# Workhorse for riskratio and riskdiff
estimate_risk <- function(
    formula,
    data,
    estimand = c("rr", "rd"),
    approach,
    variable = NULL,
    at = NULL,
    ...) {
  implausible <- 0.99999
  estimand <- match.arg(estimand)
  link <- switch(
    EXPR = estimand[1],
    rr = "log",
    rd = "identity")
  if(link == "log")
    possible_approaches <- as.character(as.list(
      args(risks::riskratio))$approach)[-1]
  else
    possible_approaches <- as.character(as.list(
      args(risks::riskdiff))$approach)[-1]
  if(!(approach[1] %in% possible_approaches))
    stop(paste0(
      "Approach '", approach[1], "' is not implemented. ",
      "Available are: ",
      paste(possible_approaches, sep = ", ", collapse = ", "),
      "."))

  fit <- switch(
    EXPR = approach[1],
    # Automated model fitting, new approach, always choosing consistent model
    auto = {
      # 1) check if marginal standardization with delta CIs is feasible
      fit <- possibly_estimate_margstd_delta(
        formula = formula,
        data = data,
        estimand = estimand,
        variable = variable,
        at = at,
        interaction_warning = FALSE,
        ...)
      if(fit$converged == TRUE &
         fit$maxprob < implausible &
         fit$boundary == FALSE &
         fit$margstd_delta_interaction == FALSE)
        return(fit)

      # 2) default to marginal standardization with bootstrap CIs
      fit <- possibly_estimate_margstd_boot(
        formula = formula,
        data = data,
        estimand = estimand,
        variable = variable,
        at = at,
        ...)

      if(fit$converged == TRUE &
         fit$maxprob < implausible &
         fit$boundary == FALSE)
        return(fit)

      # 3) Check if a logistic model can be fitted
      warning(paste(
        "risks: No",
        toupper(estimand),
        "model could be fit. Are the data intact and the",
        "formula correct? Any error messages stem directly from fitting a",
        "logistic model on the data."))
      fit <- stats::glm(
        formula = formula,
        data = data,
        family = stats::binomial(link = "logit"))
      # Typically, execution will stop with a non-converged logistic model.
      # If, surprisingly, only a logistic model converges, return an error.
      stop(paste(
        "No model besides the logistic model converged and had",
        "within-range predicted probabilities of < 1."))
    },
    # Automated model fitting, legacy approach, choosing different models
    legacy = {
      # 1) try regular GLM with Fisher scoring
      fit_glm <- possibly_estimate_glm(
        formula = formula,
        data = data,
        link = link,
        ...)
      if(fit_glm$converged == TRUE &
         fit_glm$maxprob < implausible &
         fit_glm$boundary == FALSE)
        return(fit_glm)

      # 2) try GLM with starting values from Poisson for RRs only
      if(link == "log") {
        fit_poisson <- possibly_estimate_poisson(
          formula = formula,
          data = data,
          link = link,
          ...)
        if(fit_poisson$converged == TRUE) {
          fit_glm_start <- possibly_estimate_glm_startp(
            formula = formula,
            data = data,
            link = link,
            start = coef(fit_poisson),
            start_type = "p",
            ...)
          if(fit_glm_start$converged == TRUE &
             fit_glm_start$maxprob < implausible  &
             fit_glm_start$boundary == FALSE)
            return(fit_glm_start)
        }
      }

      # 3) Try marginal standardization with delta method SEs
      fit_margstd_delta <- possibly_estimate_margstd_delta(
        formula = formula,
        data = data,
        estimand = estimand,
        variable = variable,
        at = at,
        interaction_warning = FALSE,
        ...)
      if(fit_margstd_delta$converged == TRUE &
         fit_margstd_delta$maxprob < implausible &
         fit_margstd_delta$boundary == FALSE &
         fit_margstd_delta$margstd_delta_interaction == FALSE)
        return(fit_margstd_delta)

      # 4) try marginal standardization with bootstrap SEs
      fit <- possibly_estimate_margstd_boot(
        formula = formula,
        data = data,
        estimand = estimand,
        variable = variable,
        at = at,
        ...)

      if(fit$converged == TRUE &
         fit$maxprob < implausible &
         fit$boundary == FALSE)
        return(fit)

      # 4) Check if a logistic model can be fitted
      warning(paste(
        "risks: No",
        toupper(estimand),
        "model could be fit. Are the data intact and the",
        "formula correct? Any error messages stem directly from fitting a",
        "logistic model on the data."))
      res <- stats::glm(
        formula = formula,
        data = data,
        family = stats::binomial(link = "logit"))
      # Typically, execution will stop with a non-converged logistic model.
      # If, surprisingly, only a logistic model converges, return an error.
      stop(paste(
        "No model besides the logistic model converged and had",
        "within-range predicted probabilities of < 1."))
    },

    # All models requested to fit
    all  = {
      fit1 <- possibly_estimate_poisson(
        formula = formula,
        data = data,
        link = link,
        ...)

      fit2 <- possibly_estimate_glm(
        formula = formula,
        data = data,
        link = link,
        ...)

      if(!is.null(coef(fit1)))  # attempt only if Poisson converged
        fit3 <- possibly_estimate_glm_startp(
          formula = formula,
          data = data,
          link = link,
          start = coef(fit1),
          start_type = "p",
          ...)
      else  # make possibly_estimate_glm return a non-converged object
        fit3 <- possibly_estimate_glm_startp(
          formula = "nonsense",
          data = "nodata",
          start_type = "p")

      if(link == "log")
        fit4 <- possibly_estimate_logbin(
          formula = formula,
          data = data,
          ...)
      else
        fit4 <- possibly_estimate_addreg(
          formula = formula,
          data = data,
          ...)

      if(link == "log") {
        if(!is.null(coef(fit1))) # attempt only if Poisson converged
          fit5 <- possibly_estimate_logbin(
            formula = formula,
            data = data,
            start = coef(fit1),
            ...)
        else
          fit5 <- possibly_estimate_logbin(
            formula = "nonsense",
            data = "nodata")
        if(fit5$converged == FALSE)
          fit5$risks_start = "_start"
      } else {
        if(!is.null(coef(fit1))) # attempt only if Poisson converged
          fit5 <- possibly_estimate_addreg(
            formula = formula,
            data = data,
            start = coef(fit1),
            ...)
        else
          fit5 <- possibly_estimate_addreg(
            formula = "nonsense",
            data = "nodata")
        if(fit5$converged == FALSE)
          fit5$risks_start = "_start"
      }

      fit6 <- possibly_estimate_margstd_boot(
        formula = formula,
        data = data,
        estimand = estimand,
        variable = variable,
        at = at,
        ...)

      fit7 <- possibly_estimate_margstd_delta(
        formula = formula,
        data = data,
        estimand = estimand,
        variable = variable,
        at = at,
        ...)

      # If RR requested, add on case-duplication model and, for comparison,
      # the plain logistic model
      if(estimand == "rr") {
        fit8 <- possibly_estimate_logistic(
          formula = formula,
          data = data,
          ...)
        fit9 <- possibly_estimate_duplicate(
          formula = formula,
          data = data,
          ...)

        if(!is.null(coef(fit9)))  # attempt only if 'duplicate' converged
          fit10 <- possibly_estimate_glm_startd(
            formula = formula,
            data = data,
            link = link,
            start = coef(fit9),
            start_type = "d",
            ...)
        else  # make possibly_estimate_glm return a non-converged object
          fit10 <- possibly_estimate_glm_startd(
            formula = "nonsense",
            data = "nodata",
            start_type = "d")

        fit1$all_models = list(
          robpoisson = fit1,
          glm = fit2,
          glm_startp = fit3,
          glm_cem = fit4,
          glm_cem_startp = fit5,
          margstd_boot = fit6,
          margstd_delta = fit7,
          logistic = fit8,
          duplicate = fit9,
          glm_startd = fit10)
      } else
        fit1$all_models = list(
          robpoisson = fit1,
          glm = fit2,
          glm_start = fit3,
          glm_cem = fit4,
          glm_cem_startp = fit5,
          margstd_boot = fit6,
          margstd_delta = fit7)
      fit1
    },

    # Specific models that were directly requested
    robpoisson = estimate_poisson(
      formula = formula,
      data = data,
      link = link,
      ...),
    duplicate = estimate_duplicate(
      formula = formula,
      data = data,
      ...),
    glm = estimate_glm(
      formula = formula,
      data = data,
      link = link,
      ...),
    glm_startp = {
      fit_poisson <- estimate_poisson(
        formula = formula,
        data = data,
        link = link,
        ...)
      estimate_glm(
        formula = formula,
        data = data,
        link = link,
        start = coef(fit_poisson),
        start_type = "p",
        ...)
    },
    glm_startd  = {
      fit_duplicate <- estimate_duplicate(
        formula = formula,
        data = data,
        ...)
      estimate_glm(
        formula = formula,
        data = data,
        link = link,
        start = coef(fit_duplicate),
        start_type = "d",
        ...)
    },
    glm_cem    = {
      if(link == "log") {
        if(!requireNamespace("logbin", quietly = TRUE))
          stop(paste(
            "For this approach, the 'logbin' package must be installed:",
            'install.packages("logbin")'),
            call. = FALSE)
        estimate_logbin(
          formula = formula,
          data = data,
          ...)
      } else {
        if(!requireNamespace("addreg", quietly = TRUE))
          stop(paste(
            "For this approach, the 'addreg' package must be installed:",
            'install.packages("addreg")'),
            call. = FALSE)
        estimate_addreg(
          formula = formula,
          data = data,
          ...)
      }
    },
    glm_cem_startp = {
      fit_poisson <- estimate_poisson(
        formula = formula,
        data = data,
        link = link,
        ...)
      if(link == "log") {
        if(!requireNamespace("logbin", quietly = TRUE))
          stop(paste(
            "For this approach, the 'logbin' package must be installed:",
            'install.packages("logbin")'),
            call. = FALSE)
        estimate_logbin(
          formula = formula,
          data = data,
          start = coef(fit_poisson),
          start_type = "p",
          ...)
      } else {
        if(!requireNamespace("addreg", quietly = TRUE))
          stop(paste(
            "For this approach, the 'addreg' package must be installed:",
            'install.packages("addreg")'),
            call. = FALSE)
        estimate_addreg(
          formula = formula,
          data = data,
          start = coef(fit_poisson),
          start_type = "p",
          ...)
      }
    },
    margstd_boot = estimate_margstd_boot(
      formula = formula,
      data = data,
      estimand = estimand,
      variable = variable,
      at = at,
      ...),
    margstd_delta = estimate_margstd_delta(
      formula = formula,
      data = data,
      estimand = estimand,
      variable = variable,
      at = at,
      ...),
    logistic   = estimate_logistic(
      formula = formula,
      data = data,
      ...)
  )
  return(fit)
}
