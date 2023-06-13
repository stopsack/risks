
<!-- README.md is generated from README.Rmd. Please edit that file -->

# risks: Estimating risk ratios and risk differences using regression

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/stopsack/risks/branch/master/graph/badge.svg)](https://codecov.io/gh/stopsack/risks?branch=master)
[![R CMD
CHECK](https://github.com/stopsack/risks/actions/workflows/main.yml/badge.svg)](https://github.com/stopsack/risks/actions/workflows/main.yml)
<!-- badges: end -->

## Installation

The **risks** package can be installed from CRAN:

``` r
install.packages("risks")
```

Development versions can be installed from
[GitHub](https://stopsack.github.io/risks/) using:

``` r
remotes::install_github("stopsack/risks")
```

## Summary

The **risks** package fits regression models for risk ratios (RR) and
risk differences (RD). The package refers to “risk,” but “prevalence”
can be substituted throughout.

What is the association between an exposure (smoker/nonsmoker, age in
years, or underweight/lean/overweight/obese) and the risk of a binary
outcome (dead/alive, disease/healthy), perhaps adjusting for confounders
(men/women, years of education)? For such questions, many studies
default to reporting odds ratios, which may exaggerate associations when
the outcome is common. Odds ratios are often used because they are
easily obtained from logistic regression models. Obtaining risk ratios
or risk differences, especially adjusting for confounders, has typically
required more advanced biostatistics and programming skills, including
in R.

The **risks** package makes estimating adjusted risk ratios and risk
differences as simple as fitting a logistic regression model. No
advanced programming or biostatistics skills are required. Risk ratios
or risk differences are returned whenever the data would allow for
fitting a logistic model.

## Basic example

The example data stem from a [cohort of women with breast
cancer](https://pubmed.ncbi.nlm.nih.gov/15286014). The the categorical
exposure is `stage`, the binary outcome is `death`, and the binary
confounder is `receptor`.

Fit a risk difference model:

``` r
library(risks)  # provides riskratio(), riskdiff(), postestimation functions
fit <- riskdiff(formula = death ~ stage + receptor, data = breastcancer)
```

Fitted objects can be used in the usual commands for generalized linear
models, such as:

``` r
summary(fit)
#> 
#> Risk difference model, fitted via marginal standardization of a logistic model with delta method (margstd_delta).
#> Call:
#> stats::glm(formula = death ~ stage + receptor, family = binomial(link = "logit"), 
#>     data = breastcancer, start = "(no starting values)")
#> 
#> Deviance Residuals: 
#>    Min      1Q  Median      3Q     Max  
#>                                         
#> 
#> Coefficients: (3 not defined because of singularities)
#>                Estimate Std. Error z value Pr(>|z|)    
#> stageStage I    0.00000    0.00000     NaN      NaN    
#> stageStage II   0.16303    0.05964   2.734  0.00626 ** 
#> stageStage III  0.57097    0.09962   5.732 9.95e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 228.15  on 191  degrees of freedom
#> Residual deviance: 185.88  on 188  degrees of freedom
#> AIC: 193.88
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> Confidence intervals for coefficients: (delta method)
#>                     2.5 %    97.5 %
#> stageStage I   0.00000000 0.0000000
#> stageStage II  0.04614515 0.2799187
#> stageStage III 0.37571719 0.7662158
```

`tidy()` from the broom package provides easy access to coefficients:

``` r
broom::tidy(fit)
#> # A tibble: 3 × 8
#>   term           estimate std.error statistic   p.value conf.low conf.high model
#>   <chr>             <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl> <chr>
#> 1 stageStage I      0        0         NaN    NaN         0          0     marg…
#> 2 stageStage II     0.163    0.0596      2.73   6.26e-3   0.0461     0.280 marg…
#> 3 stageStage III    0.571    0.0996      5.73   9.95e-9   0.376      0.766 marg…
```

## Further reading

- [Get started with the **risks**
  package](https://stopsack.github.io/risks/articles/risks.html)
- [Models and model
  comparisons](https://stopsack.github.io/risks/articles/models.html)
- [More on marginal
  standardization](https://stopsack.github.io/risks/articles/margstd.html)
- [Reporting results with the **rifttable**
  package](https://stopsack.github.io/rifttable/)
