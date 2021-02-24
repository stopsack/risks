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
