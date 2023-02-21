# risks 0.4.0

* **Breaking change:** For consistency, the default option for model fitting 
  (`approach = "auto"`) now always uses marginal standardization after fitting
  a logistic model. The previous approach, which relied on different models 
  fitted, is available as `approach = "legacy"`.
* If requesting `approach = "margstd_delta"` in presence of interaction
  terms involving the exposure variable, a warning is displayed. Model fitting
  with `"auto"` uses the bootstrap (i.e., `"margstd_boot"`) in that case.
* `approach = "margstd_boot"` bug fix: Keep categorical exposures of type factor
  in the correct order.
* Include `breastcancer` dataset in the package.
* Internal changes:
  + {addreg} and {logbin} are now soft dependencies (`Suggests:` instead of
    `Imports:`)
  + Remove {lifecycle} dependency
  + Compatibility with tidyselect 1.2.0 variable selection


# risks 0.3.0

* **Breaking changes:**
  + Rename `approach = "glm_start"` to `"glm_startp"` (for **P**oisson).
  + Rename `approach = "margstd"` to `"margstd_boot"`.
  + For consistency with other approaches, no longer treat numeric variables 
    with only two levels (e.g., `1` and `2`) as categorical in 
    `approach = "margstd_boot"`.
* New estimators:
  + `approach = "margstd_delta"`, marginal standardization after fitting a 
    logistic model with standard errors via the delta method.
  + `approach = "margstd_boot"` now also implements average marginal effects to 
     handle continuous exposures.
  + `approach = "duplicate"`, the case duplication method for risk ratios,
    proposed by Miettinen, with cluster-robust standard errors proposed by 
    Schouten et al.
  + `approach = "glm_startd"`, using the case duplication-based 
    coefficients as starting values for binomial models. 
  + `rr_rd_mantel_haenszel()`: New function for comparison purposes.
* Changes to parameters:
  + `approach = "auto"`, the default, now attempts model fitting in this order 
    of priority: `approach = "glm"`; `approach = "glm_startp"` (for risk ratios
    only); `approach = "margstd_delta"`. If all fail, the user
    is shown the error messages from a plain logistic model.
  + Bootstrap repeats (`bootrepeats`) for `approach = "margstd_boot"` now 
    default to `1000`.
* Bug fixes:
  + `summary.robpoisson()`: Fix sandwich standard errors. `tidy()` output was
    correct.
* Programming changes:
  + Do not attach the logbin package to the namespace; export 
    `logbin::conv.test()` on its behalf. Move MASS package (needed only for 
    testthat) to `Suggests`.
  + Remove usage of unexported functions from `stats`.
  + For `approach = "margstd_boot"`, avoid two rounds of bootstrap for standard
    error and confidence intervals separately. Rewrite internal fitting function
    `fit_and_predict()`, replacing `eststd()`. Overall, bootstrapping is more
    than two times faster now.


# risks 0.2.2

* `tidy(bootverbose = TRUE)`: For BC~a~ bootstrap confidence intervals,
  also return `jacksd.low` and `jacksd.high`, the jackknife-based Monte-Carlo 
  standard errors for the upper and lower confidence limits.
* `riskdiff()`: Remove leftover "logistic" parameter.
* `summary.risks()`, `tidy.risk()`: fix error handling if no model converged.


# risks 0.2.1

* Fix bugs in `bootci = "normal"` and in `summary.risks()`.
* Return name of dataset.
* Expand test coverage.


# risks 0.2.0

* Expand bootstrapping options after marginal standardization:
   + Parametric BCa bootstrap confidence intervals via the [bcaboot package](https://cran.r-project.org/web/packages/bcaboot/) are the new default.
   + Parametric normality-based intervals are a backup.
   + Non-parametric bootstrapping with BCa intervals is retained as an option 
     for completeness.
* Remove precision `weight` option.
* Expand documentation.


# risks 0.1.0

* First release
