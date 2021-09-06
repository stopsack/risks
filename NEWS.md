# risks 0.3.0

* `riskratio()`: Add `approach = "duplicate"`, the case duplication method 
  proposed by Miettinen with cluster-robust standard errors proposed by 
  Schouten.
* `summary.robpoisson()`: Fix sandwich standard errors (CIs were correct).
* `rr_rd_mantel_haenszel()`: New function for comparison purposes.
* Remove usage of unexported functions from `stats`.


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
