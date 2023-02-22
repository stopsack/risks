
<!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/stopsack/risks/branch/master/graph/badge.svg)](https://codecov.io/gh/stopsack/risks?branch=master)
<!-- badges: end -->


# risks: Estimating risk ratios and risk differences using regression

## Installation

Once risks is released on CRAN, `install.packages("risks")` will be
available. Currently, the development version of `risks` can be
installed from [GitHub](https://github.com/) using:

``` r
# If the "remotes" package is missing:  install.packages("remotes")
remotes::install_github("stopsack/risks")
```

## Summary

The **risks** package fits regression models for risk ratios (RR) and risk
differences (RD). For brevity, the package refers to “risk,” but
“prevalence” can be substituted throughout.

What is the association between an exposure (smoker/nonsmoker, age in years, or
underweight/lean/overweight/obese) and the risk of a binary outcome
(dead/alive, disease/healthy), perhaps adjusting for confounders
(men/women, years of education)? For such questions, many
studies default to reporting odds ratios, which may exaggerate 
associations when the outcome is common. Odds ratios are often used because they
are easily obtained from logistic regression models. Obtaining risk ratios or
risk differences, especially adjusting for confounders, has typically required
more advanced biostatistics and programming skills, including in R.

The **risks** package makes estimating adjusted risk ratios and risk differences 
as simple as fitting a logistic regression model. No advanced programming or
biostatistics skills are required. Risk ratios or risk differences are returned
whenever the data would allow for fitting a logistic model.


## Further reading

-   [Get started with the risks package](articles/risks.html)
-   [Models and model comparisons](articles/models.html)
-   [Marginal standardization](articles/margstd.html)
