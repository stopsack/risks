
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

  - Using [tic](https://github.com/ropensci/tic), set up CI with Travis
    CI and GitHub Actions, as well as codecov. Note that this step
    requires some external interfacing with the external CI sites to get
    the connections right.

<!-- end list -->

``` r
# update, if necessary
# remotes::install_github("ropensci/tic")
# remotes::install_github("ropenscilabs/travis") #also necessary
# install.packages("sodium") #also necessary
tic::use_tic() #guides through the tic setup wizard
```

  - I set it up so that `tic` will run a linux build through Travis CI,
    then perform GitHub Actions R checks on Windows, Mac, and Ubuntu. By
    default, it also runs in Mac OSX development version, but I found
    this unstable. To turn that off, go to `.github/workflows/main.yml`
    and comment out the appropriate line:

<!-- end list -->

``` r
    strategy:
      fail-fast: false
      matrix:
        config:
          # comment out lines if you do not want to build on certain platforms
          - { os: windows-latest, r: "release" }
          - { os: macOS-latest, r: "release", pkgdown: "true" }
          #- { os: macOS-latest, r: "devel" }
          - { os: ubuntu-latest, r: "release" }
```

  - I renamed the GitHub Actions workflow in
    `.github/workflows/main.yml` for more descriptive badge appearance.
    The badge can be copied into README from the Actions tab in GitHub.

  - I also let GitHub Actions build the
    [`pkgdown`](https://pkgdown.r-lib.org/) site in the `gh-pages`
    branch. Make sure to wire that up in Settings -\> GitHub Pages in
    GitHub.

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
