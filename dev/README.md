
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
