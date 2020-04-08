
## Steps taken for package setup and dev

Many of the setup steps are drawn from Emil Hvitfeldt’s useful summary
of `usethis` focused package development
[here](https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/).

  - First, update some packages

<!-- end list -->

``` r
devtools::install_github("r-lib/devtools")
devtools::install_github("r-lib/usethis")
devtools::install_github("r-lib/roxygen2")
```

  - Check name availability for the proposed package name

<!-- end list -->

``` r
available::available("risks")
```

  - Create package skeleton

<!-- end list -->

``` r
usethis::create_package(here::here("Desktop", "risks"))
```

  - Open the newly created package .Rproj, then

<!-- end list -->

``` r
usethis::use_git()
```

  - Sync to github remote. Note: I do this manually via the Terminal
    pane, but if you have [GitHub setup properly in
    RStudio](https://usethis.r-lib.org/articles/articles/usethis-setup.html)
    you could also

<!-- end list -->

``` r
usethis::use_github()
```

  - Fun part: check that you can do `Cmd + Shift + b` to build the
    package and restart a session with it loaded.

  - Choose a license type and document it

<!-- end list -->

``` r
usethis::use_mit_license("Konrad Stopsack; Travis Gerke")
```

  - Initialize the primary README

<!-- end list -->

``` r
usethis::use_readme_rmd()
```

  - Set up CI with Travis CI and Appveyor (useful for Windows
    compatibility monitoring). Note that this step requires some
    external interfacing with the Travis and Appveyor sites to get the
    connections right. For the two CI steps below, a potentially more
    robust solution may be to use
    [tic](https://github.com/ropensci/tic).

<!-- end list -->

``` r
usethis::use_travis()
usethis::use_appveyor()
```

  - Add test coverage reports

<!-- end list -->

``` r
usethis::use_coverage("codecov")
```

  - Initialize first function

<!-- end list -->

``` r
usethis::use_r("estimate_risk")
```

  - Initialize `testthat` and write a test on that function

<!-- end list -->

``` r
usethis::use_testthat()
usethis::use_test("estimate_risk")
```

  - Add a couple of package imports. Observe the important note for
    `pkg::function()` useage when writing the current package.

<!-- end list -->

``` r
usethis::use_package("sandwich")
usethis::use_package("addreg")
```

  - After initializing the directory `dev/` in which this README lives
    (and other directories/files which are not related to the package
    build), make sure to add them to `.Rbuildignore`

<!-- end list -->

``` r
usethis::use_build_ignore("dev")
```

  - At this point, if we run `devtools::check()` (or shortcut with `Cmd
    \+ Shift \+ E`), we get some notes like “no visible global function
    definition for `glm`”. There’s a suggestion about adding
    `importFrom()` to the NAMESPACE file (which is what needs to be
    done). Most such warnings appear to refer to functions from the
    `stats` package, so let’s import that namespace as a whole. We do
    that by [roxygenizing](https://roxygen2.r-lib.org/index.html)
    `estimate_risk.R`. Among other documentation that we’ll go ahead and
    add, the key line is `#' @import stats`. After we build the
    documentation and rebuild the package, roxygen2 automatically
    updates the NAMESPACE file appropriately. Don’t forget to also
    `usethis::use_package("stats")` to update the DESCRIPTION file.

  - Configure [`pkgdown`](https://pkgdown.r-lib.org/) to build a
    documentation website.

<!-- end list -->

``` r
# if an update is needed
# devtools::install_github("r-lib/pkgdown")
usethis::use_pkgdown()
pkgdown::build_site()
```

  - Make it possible to add lifecycle information to individual
    functions

<!-- end list -->

``` r
usethis::use_lifecycle()
```

  - A note from this code output tells us the following:

<!-- end list -->

    ✓ Adding 'lifecycle' to Imports field in DESCRIPTION
    ● Copy and paste the following lines into '/Users/gerketa/Desktop/risks/R/risks-package.R':
      ## usethis namespace: start
      #' @importFrom lifecycle deprecate_soft
      ## usethis namespace: end
      NULL

  - We also learn that we can update the badge status with

<!-- end list -->

    ● Add badges in documentation topics by inserting one of:
    - \lifecycle{experimental}
    - \lifecycle{maturing}
    - \lifecycle{stable}
    - \lifecycle{superseded}
    - \lifecycle{questioning}
    - \lifecycle{soft-deprecated}
    - \lifecycle{deprecated}
    - \lifecycle{defunct}
    - \lifecycle{archived}

  - Let’s now add a lifecycle badge for the whole package (currently
    “maturing”).

<!-- end list -->

``` r
usethis::use_lifecycle_badge("maturing")
```
