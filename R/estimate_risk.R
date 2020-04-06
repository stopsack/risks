# relrisk
# -------  Fit risk ratio and risk difference models using robust Poisson and binomial models
#          1) Spiegelman D, Hertzmark E. Easy SAS Calculations for Risk or Prevalence Ratios and Differences.
#             Am J Epidemiol 2005;162:199-200.  (overview)
#          2) Wacholder S. Binomial regression in GLIM: Estimating risk ratios and risk differences.
#             Am J Epidemiol 1986;123:174-184.  (log-binomial model)
#          3) Zou G. A modified Poisson regression approach to prospective studies with binary data.
#             Am J Epidemiol 2004;159:702-706.  (Poisson with sandwich SE)
#          4) Marschner's addbin package
relrisk <- function(data,                 # dataset
                    formula,              # regular GLM formula
                    estimate     = "rr",  # estimate risk (prevalence) ratio or difference?
                    level        = 0.95,  # confidence interval width
                    pvalue       = FALSE, # add p-value to table of results?
                    sourcecolumn = FALSE, # add column with source of final estimates (binomial vs. Poisson)
                    verbose      = TRUE,  # print all model summaries, or just final coefficients?
                    logistic     = FALSE, # show logistic regression for comparison?
                    addbin       = TRUE,  # attempt addbin() if glm() fits fail to converge
                    ...) {                # passed on to both calls of glm()
  # Find out which link function to use
  link <- switch(EXPR = estimate,
                 rr = "log",
                 rd = "identity")

  # Try to fit Poisson model
  fitpois <- tryCatch(glm(formula, family = poisson(link = link), data = data, ...),
                      error   = function(x) { NA },
                      warning = function(x) { NA })

  # Try to fit binomial model with starting values from Poisson if the latter converged
  if(!is.na(fitpois[1])) {
    fitpois.coe <- coef(fitpois)
    fitbin <- tryCatch(
      glm(formula, family = binomial(link = link), data = data, start = fitpois.coe, ...),
      error   = function(x) { NA },
      warning = function(x) { NA })
    maxprob.pois <- max(predict(fitpois, type = "response"))
  } else {  # still try to fit binomial model
    fitbin <- tryCatch(
      glm(formula, family = binomial(link = link), data = data, ...),
      error   = function(x) { NA },
      warning = function(x) { NA })
  }

  # Did either model converge? If not, try to fit binomial model using addbin() for RD models
  tryaddbin <- FALSE
  if(is.na(fitbin[1])) {
    tryaddbin <- TRUE
  } else {
    if(!is.na(fitpois[1]))
      if(maxprob.pois > 1) {
        print(paste("Poisson model converged, but yielded out-of-range probabilities. Maximum:", maxprob.pois))
        tryaddbin <- TRUE
      }
  }

  if(addbin == FALSE)
    tryaddbin <- FALSE

  if(tryaddbin == TRUE) {
    if(estimate == "rd") {
      if(verbose == TRUE) {
        print("Neither the Poisson nor the binomial model for RD converged with glm().")
        print("Fitting RD binomial model with addbin(method = 'em').")
      }
      require(addreg)
      fitbin <- tryCatch(addreg(formula = formula, data = data, family = binomial, method = "em"),
                         error   = function(x) { NA },
                         warning = function(x) { NA })
      if(is.na(fitbin[1]) & is.na(fitpois[1]))
        stop("Neither the Poisson, binomial, nor addreg() binomial model converged.")
    } else {
      stop("Neither the binomial nor the Poisson model for RR converged (or had out-of-range probabilities only).")
    }
  }

  if(is.na(fitbin[1])) {
    result.bin <- data.frame(NA)
  } else {
    fitbin.ci <- confint.default(fitbin, level = level)
    result.bin <- switch(EXPR = estimate,
                         rr   = exp(data.frame(RR = coef(fitbin),
                                               LL = fitbin.ci[, 1],
                                               UL = fitbin.ci[, 2])),
                         rd   = data.frame(RD     = coef(fitbin),
                                           LL     = fitbin.ci[, 1],
                                           UL     = fitbin.ci[, 2]))
    if(pvalue == TRUE)
      result.bin <- data.frame(result.bin, P = summary(fitbin)$coefficients[, "Pr(>|z|)"])
    maxprob.bin <- max(predict(fitbin, type = "response"))
  }

  # Estimate robust covariance for Poisson if binomial model did not converge or verbose output is requested
  if((is.na(result.bin[1, 1]) | verbose == TRUE) & !is.na(fitpois[1])) {
    require(sandwich)
    fitpois.se <- sqrt(diag(vcovHC(fitpois, type = "HC0")))  # consider HC3 (even more conservative)
    zcritical <- qnorm(1-((1-level)/2))
    result.pois <- switch(EXPR = estimate,
                          rr = exp(data.frame(RR = fitpois.coe,
                                              LL = fitpois.coe - zcritical * fitpois.se,
                                              UL = fitpois.coe + zcritical * fitpois.se)),
                          rd =     data.frame(RD = fitpois.coe,
                                              LL = fitpois.coe - zcritical * fitpois.se,
                                              UL = fitpois.coe + zcritical * fitpois.se))
    if(pvalue == TRUE)
      result.pois <- data.frame(result.pois, P = 2 * pnorm(abs(fitpois.coe/fitpois.se), lower.tail = FALSE))
  }

  if(logistic == TRUE) {
    print("**************************************")
    print("*** FOR COMPARISON: LOGISTIC MODEL ***")
    logistic(data = data, formula = formula)
  }

  if(verbose == TRUE) {
    print("######################################")
    print(paste("### CALL:", as.character(formula), " *** Estimate: ", estimate, "###"))
    print("**************************************")
    print("***        POISSON MODEL           ***")
    print("***  (ignore SEs, p in 1st table)  ***")
    if(!is.na(fitpois[1])) {
      print(summary(fitpois))
      print(result.pois)
    } else {
      print("Poisson model did not converge")
    }
    print("")
    print("**************************************")
    print("***        BINOMIAL MODEL          ***")
    if(is.na(result.bin[1, 1])) {
      print("Binomial model did not converge")
    } else {
      # Check that predicted probabilites are meaningful
      if(maxprob.bin > 1) {
        print(paste("Binomial model converged, but yielded out-of-range predicted probabilities. Maximum:", maxprob.bin))
      } else {
        print(summary(fitbin))
        print(result.bin)
      }
    }
    print("######################################")
  }

  # Return results from binomial model if converged and in-range predicted probabilities
  if(is.na(result.bin[1, 1])) {
    returnpoisson <- TRUE
  } else {
    if(maxprob.bin > 1) {  # cannot add to the if() above becase maxprob.bin may be non-existant
      returnpoisson <- TRUE
    } else {
      returnpoisson <- FALSE
      result <- result.bin
      if(sourcecolumn == TRUE) {
        if(tryaddbin == TRUE)
          result <- data.frame(result, model = "Binomial_addbin")
        else
          result <- data.frame(result, model = "Binomial")
      }
    }
  }

  # Otherwise return results from Poisson model
  if(returnpoisson == TRUE) {
    result <- result.pois
    if(sourcecolumn == TRUE)
      result <- data.frame(result, model = "Poisson")
  }

  # Return table of estimates
  result
}
