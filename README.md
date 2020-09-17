
<!-- README.md is generated from README.Rmd. Please edit that file -->

# risks

## Installation

Currently, the development version of `risks` can be installed from
[GitHub](https://github.com/) using:

``` r
# install.packages("devtools")  # The devtools package needs to be installed
library(devtools)
devtools::install_github("stopsack/risks")
```

Once released on [CRAN](https://CRAN.R-project.org), installation will
be possible with:

``` r
install.packages("risks")
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
and risk differences have much more intuitive interpretations.
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
biostatistics skills are required. Build-in routines automatically
switch to more computationally demanding model fitting approaches if
simple binomial models have convergence issues, making `risks` return
valid estimates whenever the data would allow for fitting a logistic
model. At the same time, several options for customization of model
fitting and reporting as well as comparisons between different
approaches are provided.

Implemented are binomial models, Poisson models with robust covariance,
binomial models aided in convergence by starting values obtained through
Poisson models, binomial models fitted via combinatorial expectation
maximization instead of Fisher scoring (optionally also with Poisson
starting values), and marginal standardization after fitting logistic
models.

# Basic usage

We define a cohort of women with breast cancer, as used by Spiegelman
and Hertzmark ([Am J
Epidemiol 2005](https://pubmed.ncbi.nlm.nih.gov/15987728)) and Greenland
([Am J Epidemiol 2004](https://pubmed.ncbi.nlm.nih.gov/15286014)). The
the categorical exposure is `stage`, the binary outcome is `death`, and
the binary confounder is `receptor`.

``` r
library(risks)      # provides riskratio(), riskdiff(), and postestimation functions as below
library(tidyverse)  # For data handling
library(broom)      # For tidy() model summaries

# Sample data originally from:
# Newman SC. Biostatistical methods in epidemiology. New York, NY: Wiley, 2001, table 5.3
dat <- tibble(
  death    = c(rep(1, 54), rep(0, 138)),
  stage    = c(rep("Stage I", 7),  rep("Stage II", 26), rep("Stage III", 21),
               rep("Stage I", 60), rep("Stage II", 70), rep("Stage III", 8)),
  receptor = c(rep("Low", 2),  rep("High", 5),  rep("Low", 9),  rep("High", 17),
               rep("Low", 12), rep("High", 9),  rep("Low", 10), rep("High", 50),
               rep("Low", 13), rep("High", 57), rep("Low", 2),  rep("High", 6)))

# Display the sample data
dat %>% 
  group_by(receptor, stage) %>% 
  summarize(deaths = sum(death), total = n(), risk = deaths/total)
#> # A tibble: 6 x 5
#> # Groups:   receptor [2]
#>   receptor stage     deaths total   risk
#>   <chr>    <chr>      <dbl> <int>  <dbl>
#> 1 High     Stage I        5    55 0.0909
#> 2 High     Stage II      17    74 0.230 
#> 3 High     Stage III      9    15 0.6   
#> 4 Low      Stage I        2    12 0.167 
#> 5 Low      Stage II       9    22 0.409 
#> 6 Low      Stage III     12    14 0.857
```

  
The risk of death is higher among women with higher-stage and hormone
receptor-low cancers, which also tend to be of higher stage. Using
`risks` models to obtain (possibly multivariable-adjusted) risk ratios
or risk differences is similar to the standard code for logistic models
in R. No options for model `family` or `link` need to be supplied:

``` r
fit_rr <- riskratio(formula = death ~ stage + receptor, data = dat)
summary(fit_rr)
#> 
#> Risk ratio model, fitted as binomial model with starting values from Poisson model (glm_start).
#> Call:
#> stats::glm(formula = death ~ stage + receptor, family = binomial(link = "log"), 
#>     data = data, start = "(from Poisson model)")
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -2.0209  -0.7436  -0.4472   0.5272   2.1689  
#> 
#> Coefficients:
#>                Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)     -2.3521     0.3602  -6.531 6.55e-11 ***
#> stageStage II    0.9314     0.3930   2.370   0.0178 *  
#> stageStage III   1.7695     0.3851   4.595 4.33e-06 ***
#> receptorLow      0.4436     0.1968   2.254   0.0242 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 228.15  on 191  degrees of freedom
#> Residual deviance: 185.85  on 188  degrees of freedom
#> AIC: 193.85
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> Confidence intervals for coefficients (normality-based):
#>                      2.5 %     97.5 %
#> (Intercept)    -3.05805977 -1.6461981
#> stageStage II   0.16107688  1.7018004
#> stageStage III  1.01475475  2.5242885
#> receptorLow     0.05781282  0.8294095
```

  
To obtain risk differences, use `riskdiff`, which has the same syntax:

``` r
fit_rd <- riskdiff(formula = death ~ stage + receptor, data = dat)
summary(fit_rd)
#> 
#> Risk difference model, fitted as binomial model (glm).
#> Call:
#> stats::glm(formula = death ~ stage + receptor, family = binomial(link = "identity"), 
#>     data = data, start = "(no starting values)")
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -1.8441  -0.7284  -0.4184   0.6350   2.2268  
#> 
#> Coefficients:
#>                Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)     0.08381    0.03633   2.307  0.02107 *  
#> stageStage II   0.14921    0.05755   2.593  0.00953 ** 
#> stageStage III  0.57227    0.09472   6.042 1.52e-09 ***
#> receptorLow     0.16131    0.07587   2.126  0.03349 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 228.15  on 191  degrees of freedom
#> Residual deviance: 186.38  on 188  degrees of freedom
#> AIC: 194.38
#> 
#> Number of Fisher Scoring iterations: 5
#> 
#> Confidence intervals for coefficients (normality-based):
#>                     2.5 %    97.5 %
#> (Intercept)    0.01260046 0.1550111
#> stageStage II  0.03640945 0.2620070
#> stageStage III 0.38663167 0.7579168
#> receptorLow    0.01261165 0.3100162
```

For example, the risk of death was 57 percentage points higher in women
with stage III breast cancer compared to stage I (95% confidence
interval, 39 to 76 percentage points), adjusting for hormone receptor
status.

The model summary in `risks` includes to two additions compared to a
regular `glm` model:

  - In the first line of `summary(...)`, the type of risk regression
    model is displayed (in the example, “`Risk ratio model, fitted as
    binomial model...`”).
  - At the end of the output, confidence intervals for the model
    coefficients are printed.

  
`risks` provides an interface to `tidy()`, which returns a data frame of
all coefficients (risk differences in this example), their standard
errors, and confidence intervals.

``` r
tidy(fit_rd)
#> # A tibble: 4 x 8
#>   term         estimate std.error statistic     p.value conf.low conf.high model
#>   <chr>           <dbl>     <dbl>     <dbl>       <dbl>    <dbl>     <dbl> <chr>
#> 1 (Intercept)    0.0838    0.0363      2.31     2.11e-2   0.0126     0.155 glm  
#> 2 stageStage …   0.149     0.0576      2.59     9.53e-3   0.0364     0.262 glm  
#> 3 stageStage …   0.572     0.0947      6.04     1.52e-9   0.387      0.758 glm  
#> 4 receptorLow    0.161     0.0759      2.13     3.35e-2   0.0126     0.310 glm
```

  
In accordance with R standards, coefficients for relative risks are
shown on the logarithmic scale. Exponentiated coefficients (risk ratios)
are easily obtained via `tidy(..., exponentiate = TRUE)`:

``` r
tidy(fit_rr, exponentiate = TRUE)
#> # A tibble: 4 x 8
#>   term         estimate std.error statistic  p.value conf.low conf.high model   
#>   <chr>           <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <chr>   
#> 1 (Intercept)    0.0952     0.360     -6.53 6.55e-11   0.0470     0.193 glm_sta…
#> 2 stageStage …   2.54       0.393      2.37 1.78e- 2   1.17       5.48  glm_sta…
#> 3 stageStage …   5.87       0.385      4.60 4.33e- 6   2.76      12.5   glm_sta…
#> 4 receptorLow    1.56       0.197      2.25 2.42e- 2   1.06       2.29  glm_sta…
```

For example, the risk of death was 5.9 times higher in women with stage
III breast cancer compared to stage I (95% confidence interval, 2.8 to
12 times), adjusting for hormone receptor status.

  
Typical R functions that build on regression models can further process
fitted `risks` models. Examples:

  - `coef(fit)` returns model coefficients (*i.e.*, log(RR) or RDs) as a
    numeric vector
  - `confint(fit, level = 0.9)` returns *90%* confidence intervals.
  - `predict(fit, type = "response")` returns predicted probabilities of
    the binary outcome.

# Advanced usage

# Implemented regression models for risk ratios and risk differences

`risks` implements all major regression models that have been proposed
for relative risks and risk differences. By default (`approach =
"auto"`), `riskratio` and `riskdiff` estimate the most efficient valid
model that converges; in more numerically challenging cases, they
default to computationally less efficient models while ensuring that a
valid result is returned.

The following models are implemented in `risks`:

| \#<sup>1</sup> | `approach =` | RR                | RD         | Model                                                                                                               | Reference                                                                                                                                                                                                                               |
| -------------- | ------------ | ----------------- | ---------- | ------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1              | `glm`        | `riskratio`       | `riskdiff` | Binomial model with a log or identity link                                                                          | Wacholder S. Binomial regression in GLIM: Estimating risk ratios and risk differences. [Am J Epidemiol 1986;123:174-184](https://www.ncbi.nlm.nih.gov/pubmed/3509965).                                                                  |
| 2              | `glm_start`  | `riskratio`       | `riskdiff` | Binomial model with a log or identity link, convergence-assisted by starting values from Poisson model              | Spiegelman D, Hertzmark E. Easy SAS calculations for risk or prevalence ratios and differences. [Am J Epidemiol 2005;162:199-200](https://www.ncbi.nlm.nih.gov/pubmed/15987728).                                                        |
| 3              | `glm_cem`    | `riskratio`       | —          | Binomial model with log-link fitted via combinatorial expectation maximization instead of Fisher scoring            | Donoghoe MW, Marschner IC. logbin: An R Package for Relative Risk Regression Using the Log-Binomial Model. [J Stat Softw 2018;86(9)](http://dx.doi.org/10.18637/jss.v086.i09).                                                          |
| 3              | `glm_cem`    | —                 | `riskdiff` | Additive binomial model (identity link) fitted via combinatorial expectation maximization instead of Fisher scoring | Donoghoe MW, Marschner IC. Stable computational methods for additive binomial models with application to adjusted risk differences. [Comput Stat Data Anal 2014;80:184-96](https://doi.org/10.1016/j.csda.2014.06.019).                 |
| 4              | `margstd`    | `riskratio`       | `riskdiff` | Marginally standardized estimates using binomial model with a logit link (logistic model)                           | Localio AR, Margolis DJ, Berlin JA. Relative risks and confidence intervals were easily computed indirectly from multivariable logistic regression. [J Clin Epidemiol 2007;60(9):874-82](https://www.ncbi.nlm.nih.gov/pubmed/17689803). |
| –              | `robpoisson` | `riskratio`       | `riskdiff` | Log-linear (Poisson) model with robust/sandwich/empirical standard errors                                           | Zou G. A modified Poisson regression approach to prospective studies with binary data. [Am J Epidemiol 2004;159(7):702-6](https://www.ncbi.nlm.nih.gov/pubmed/15033648)                                                                 |
| –              | `logistic`   | (only comparison) | —          | Binomial model with logit link (*i.e.*, the logistic model), returning odds ratios                                  | Included for comparison purposes only.                                                                                                                                                                                                  |

<sup>1</sup> Indicates the priority with which the default modelling
strategy (`approach = "auto"`) attempts model fitting.

  
Which model was fitted is always indicated in the first line of the
output of `summary(...)` and in the `model` column of `tidy(...)`. In
methods sections of manuscripts, the approach can be described in detail
as follows:

> Risk ratios \[or risk differences\] were obtained via \[method listed
> in the first line of `summary(...)`\] using the `risks` R package
> (reference to this package and/or the article listed in the column
> “reference”).

For example:

> Risk ratios were obtained from binomial models with a log link,
> convergence-assisted by Poisson models (ref. Spiegelman and Hertzmark,
> AJE 2005), using the `risks` R package
> (<https://github.com/stopsack/risks>).

## Model choice

By default, automatic model fitting according to the priority listed in
the table above is attempted. Alternatively, any of the options listed
under `approach =` in the table can be requested directly. However,
unlike with `approach = "auto"` (the default), the selected model may
not converge.

Selecting a binomial model with starting values from the Poisson model:

``` r
riskratio(formula = death ~ stage + receptor, data = dat, approach = "glm_start")
#> 
#> Risk ratio model
#> Call:  stats::glm(formula = death ~ stage + receptor, family = binomial(link = "log"), 
#>     data = data, start = "(from Poisson model)")
#> 
#> Coefficients:
#>    (Intercept)   stageStage II  stageStage III     receptorLow  
#>        -2.3521          0.9314          1.7695          0.4436  
#> 
#> Degrees of Freedom: 191 Total (i.e. Null);  188 Residual
#> Null Deviance:       228.1 
#> Residual Deviance: 185.9     AIC: 193.9
```

  
However, the binomial model without starting values does not converge:

``` r
riskratio(formula = death ~ stage + receptor, data = dat, approach = "glm")
#> Error: no valid set of coefficients has been found: please supply starting values
```

## Marginal standardization

Marginal standardization, `approach = "margstd"`, makes use of the good
convergence properties of the logit link, which is also guaranteed to
result in probabilities within (0; 1). After fitting a logistic models,
predicted probabilities for each observation are obtained for the levels
of the exposure variable. Risk ratios and risk differences are
calculated by contrasting the predicted probabilities as ratios or
differences. Standard errors and confidence intervals are obtained via
bootstrapping the entire procedure. Standardization can only be done
over one exposure variable, and thus no cofficients are estimated for
other variables in the model. In addition, in order to derive contrasts,
continuous exposures are evaluated at discrete levels.

  - By default, the first categorical right-hand variable in the model
    formula will be assumed to be the exposure. The variable types
    `logical`, `factor`, and `character` are taken to represent
    categorical variables, as are variables of the type `numeric` with
    only two levels (e.g., `0` and `1`).
  - Using `variable =`, the user can specify a different variable.
  - Using `at =`, levels for contrasts and the order of levels can be
    specified. The first level is used as the reference with a risk
    ratio of 1 or a risk difference of 0. The option `at =` is required
    for continuous variables or `numeric` variables with more than two
    levels. A warning will be shown for continuous values if the
    requested levels exceed the range of data (extrapolation).
  - For models fitted via `approach = "margstd"`, standard
    errors/confidence intervals are obtained via bootstrapping. The
    default are 200 bootstrap repeats to reduce initial computation
    time. For final, precise estimates, the number of repeats should be
    increased to \>1000. Use the option `bootrepeats =` in `summary()`,
    `tidy()`, or `confint()`.

We fit the same risk difference model as in section 2:

``` r
fit_margstd <- riskdiff(formula = death ~ stage + receptor, data = dat, 
                        approach = "margstd")
summary(fit_margstd, bootrepeats = 500)
#> 
#> Risk difference model, fitted via marginal standardization of a logistic model (margstd).
#> Call:
#> stats::glm(formula = death ~ stage + receptor, family = binomial(link = "logit"), 
#>     data = data, start = "(no starting values)")
#> 
#> Deviance Residuals: 
#>    Min      1Q  Median      3Q     Max  
#>                                         
#> 
#> Coefficients: (3 not defined because of singularities)
#>                Estimate Std. Error z value Pr(>|z|)    
#> stageStage I    0.00000    0.00000      NA       NA    
#> stageStage II   0.16303    0.05881   2.772  0.00557 ** 
#> stageStage III  0.57097    0.09631   5.928 3.06e-09 ***
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
#> Confidence intervals for coefficients (bootstrap-based):
#>                      2.5%     97.5%
#> stageStage I           NA        NA
#> stageStage II  0.03161866 0.2670610
#> stageStage III 0.34684786 0.7395444
```

Consistent with earlier results, we observed that women with stage III
breast cancer have a 57 percentage points higher risk of death
(bootstrapped 95% confidence interval, 35 to 76 percentage points),
adjusting for hormone receptor status.

Note that coefficients and standard errors are only estimated for the
exposure variable. Model fit characteristics and predicted values stem
directly from the underlying logistic model.

Requesting a different exposure variable:

``` r
fit_margstd2 <- riskdiff(formula = death ~ stage + receptor, data = dat, 
                         approach = "margstd", variable = "receptor")
summary(fit_margstd2, bootrepeats = 500)
#> 
#> Risk difference model, fitted via marginal standardization of a logistic model (margstd).
#> Call:
#> stats::glm(formula = death ~ stage + receptor, family = binomial(link = "logit"), 
#>     data = data, start = "(no starting values)")
#> 
#> Deviance Residuals: 
#>    Min      1Q  Median      3Q     Max  
#>                                         
#> 
#> Coefficients: (3 not defined because of singularities)
#>              Estimate Std. Error z value Pr(>|z|)  
#> receptorHigh  0.00000    0.00000      NA       NA  
#> receptorLow   0.16163    0.07658   2.111   0.0348 *
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
#> Confidence intervals for coefficients (bootstrap-based):
#>                     2.5%     97.5%
#> receptorHigh          NA        NA
#> receptorLow  0.000612633 0.3046141
```

## Model comparisons

With `approach = "all"`, all model types listed in the tables are
fitted. The fitted object, *e.g.*, `fit`, is one of the converged
models. A summary of the convergence status of all models is displayed
at the beginning of `summary(fit)`:

``` r
fit_all <- riskdiff(formula = death ~ stage + receptor, data = dat, 
                    approach = "all")
#> Loading required package: doParallel
#> Loading required package: foreach
#> 
#> Attaching package: 'foreach'
#> The following objects are masked from 'package:purrr':
#> 
#>     accumulate, when
#> Loading required package: iterators
#> Loading required package: parallel
#> Loading required package: numDeriv
#> Loading required package: quantreg
#> Loading required package: SparseM
#> 
#> Attaching package: 'SparseM'
#> The following object is masked from 'package:base':
#> 
#>     backsolve
#> 
#> Attaching package: 'turboEM'
#> The following objects are masked from 'package:numDeriv':
#> 
#>     grad, hessian
summary(fit_all)
#> 
#> All fitted models:
#>          Model Converged Max.prob.
#> 1   robpoisson      TRUE 0.7907040
#> 2          glm      TRUE 0.8173940
#> 3    glm_start      TRUE 0.8173882
#> 4       addreg      TRUE 0.8173957
#> 5 addreg_start      TRUE 0.8173957
#> 6      margstd      TRUE 0.8158560
#> Access these models via 'x$all_models'.
#> 
#> Risk difference model, fitted as Poisson model with robust covariance (robpoisson).
#> Call:
#> stats::glm(formula = death ~ stage + receptor, family = poisson(link = "identity"), 
#>     data = data, start = "(no starting values)")
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -1.2575  -0.6870  -0.4147   0.2260   1.7546  
#> 
#> Coefficients:
#>                Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)     0.08600    0.03871   2.222 0.026292 *  
#> stageStage II   0.14998    0.06474   2.317 0.020519 *  
#> stageStage III  0.56505    0.16482   3.428 0.000607 ***
#> receptorLow     0.13965    0.09602   1.454 0.145866    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for poisson family taken to be 1)
#> 
#>     Null deviance: 137.00  on 191  degrees of freedom
#> Residual deviance: 110.57  on 188  degrees of freedom
#> AIC: 226.57
#> 
#> Number of Fisher Scoring iterations: 5
#> 
#> Confidence intervals for coefficients (normality-based):
#>                      2.5 %    97.5 %
#> (Intercept)     0.01013792 0.1618699
#> stageStage II   0.02309478 0.2768559
#> stageStage III  0.24200852 0.8880958
#> receptorLow    -0.04855731 0.3278532
```

  
Individual models can be accessed as `fit$all_models[[1]]` through
`fit$all_models[[6]]` (or `[[7]]` if fitting a risk ratio model).
`tidy()` shows coefficients and confidence intervals from all models
that converged:

``` r
tidy(fit_all) %>%
  print(n = 25)
#> # A tibble: 23 x 8
#>    term      estimate std.error statistic    p.value conf.low conf.high model   
#>    <chr>        <dbl>     <dbl>     <dbl>      <dbl>    <dbl>     <dbl> <chr>   
#>  1 (Interce…   0.0860    0.0387      2.22   2.63e- 2  0.0122      0.160 robpois…
#>  2 stageSta…   0.150     0.0647      2.32   2.05e- 2  0.0366      0.263 robpois…
#>  3 stageSta…   0.565     0.165       3.43   6.07e- 4  0.377       0.753 robpois…
#>  4 receptor…   0.140     0.0960      1.45   1.46e- 1 -0.00878     0.288 robpois…
#>  5 (Interce…   0.0838    0.0363      2.31   2.11e- 2  0.0126      0.155 glm     
#>  6 stageSta…   0.149     0.0576      2.59   9.53e- 3  0.0364      0.262 glm     
#>  7 stageSta…   0.572     0.0947      6.04   1.52e- 9  0.387       0.758 glm     
#>  8 receptor…   0.161     0.0759      2.13   3.35e- 2  0.0126      0.310 glm     
#>  9 (Interce…   0.0838    0.0363      2.31   2.11e- 2  0.0126      0.155 glm_sta…
#> 10 stageSta…   0.149     0.0576      2.59   9.52e- 3  0.0364      0.262 glm_sta…
#> 11 stageSta…   0.572     0.0947      6.04   1.52e- 9  0.387       0.758 glm_sta…
#> 12 receptor…   0.161     0.0759      2.13   3.35e- 2  0.0126      0.310 glm_sta…
#> 13 (Interce…   0.0838    0.0363      2.31   2.11e- 2  0.0126      0.155 addreg  
#> 14 stageSta…   0.149     0.0576      2.59   9.52e- 3  0.0364      0.262 addreg  
#> 15 stageSta…   0.572     0.0947      6.04   1.52e- 9  0.387       0.758 addreg  
#> 16 receptor…   0.161     0.0759      2.13   3.35e- 2  0.0126      0.310 addreg  
#> 17 (Interce…   0.0838    0.0363      2.31   2.11e- 2  0.0126      0.155 addreg_…
#> 18 stageSta…   0.149     0.0576      2.59   9.52e- 3  0.0364      0.262 addreg_…
#> 19 stageSta…   0.572     0.0947      6.04   1.52e- 9  0.387       0.758 addreg_…
#> 20 receptor…   0.161     0.0759      2.13   3.35e- 2  0.0126      0.310 addreg_…
#> 21 stageSta…   0         0         NaN    NaN        NA          NA     margstd 
#> 22 stageSta…   0.163     0.0578      2.82   4.81e- 3  0.0629      0.284 margstd 
#> 23 stageSta…   0.571     0.0889      6.42   1.32e-10  0.386       0.770 margstd
```

## Prediction

  - Checking maximum predicted probabilities (may be out-of-range for
    Poisson)

## Weights—TBD

## Clustered observations—TBD
