# risks

## Installation

Once risks is released on CRAN, `install.packages("risks")` will be
available. Currently, the development version of `risks` can be
installed from [GitHub](https://github.com/) using:

``` r
# install.packages("remotes")  # The "remotes" package needs to be available
remotes::install_github("stopsack/risks")
```

# Summary

The `risks` package fits regression models for risk ratios (RR) and risk
differences (RD). For brevity, the package refers to “risk,” but
“prevalence” can be substituted throughout.

What is the association between an exposure (men/women, age in years, or
underweight/lean/overweight/obese) and the risk of a binary outcome
(dead/alive, disease/healthy), perhaps adjusting for confounders
(smoker/nonsmoker, years of education)? For such binary outcomes, many
studies default to reporting odds ratios (OR). Yet outside of
case-control studies, odds ratios are unnecessary to report. Risk ratios
and risk differences have more intuitive interpretations.
Additionally, when the outcome is not rare, odds ratios are significant
overestimates for risk ratios. Despite these drawbacks, odds ratios are
often simply reported because they are easily obtained from logistic
regression models. Obtaining risk ratios or risk differences, especially
after adjusting for covariates (*e.g.,* confounders) has been quite
challenging and often required more advanced biostatistics and
programming skills, including in R.

The `risks` package fills this gap by providing a user-friendly approach
that makes obtaining adjusted risk ratios and risk differences as simple
as fitting a logistic regression model. No advanced programming or
biostatistics skills are required. Built-in routines automatically
switch to more computationally demanding model fitting approaches if
simple binomial models have convergence issues, making `risks` return
valid estimates whenever the data would allow for fitting a logistic
model. At the same time, several options for customization of model
fitting and reporting as well as comparisons between different
approaches are provided (see [Get started](articles/risks.html)).

Five different approaches are implemented 
(see [Models and model comparisons](articles/models.html)): 
Poisson models with robust covariance,
binomial models aided in convergence by starting values obtained through
Poisson models, binomial models fitted via combinatorial expectation
maximization instead of Fisher scoring (optionally also with Poisson
starting values), and marginal standardization after fitting logistic
models (see [Marginal standardization](articles/margstd.html) for details).

# Further reading

-   [Get started with the risks package](articles/risks.html)
-   [Models and model comparisons](articles/models.html)
-   [Marginal standardization](articles/margstd.html)
