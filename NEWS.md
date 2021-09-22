# risks 0.3.0 (not released)

* **Breaking changes:**
  + Rename `approach = "glm_start"` to `"glm_startp"` (for **P**oisson).
* New estimators:
  + `approach = "duplicate"`, the case duplication method for risk ratios,
    proposed by Miettinen, with cluster-robust standard errors proposed by 
    Schouten.
  + `approach = "glm_startd"`, using the case duplication-based 
    coefficients as starting values for binomial models. 
  + `rr_rd_mantel_haenszel()`: New function for comparison purposes.
* Bug fixes:
  + `summary.robpoisson()`: Fix sandwich standard errors. `tidy()` output was
    correct.
* Programming changes:
  + Do not attach the logbin package to the namespace; export 
    `logbin::conv.test()` on its behalf. Move MASS package (needed only for 
    testthat) to `Suggests`.
  + Remove usage of unexported functions from `stats`.
  + Rewrite internal fitting function `fit_and_predict()`, replacing `eststd()`
    and accelerating bootstrapping by factor >2.


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
