
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bibliomatrix <img src="man/figures/sticker.png" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/KTH-Library/bibliomatrix.svg?branch=master)](https://travis-ci.org/KTH-Library/bibliomatrix)
<!-- badges: end -->

The goal of bibliomatrix is to provide a common set of functionality for
the Annual Bibliometric Monitoring efforts from the Bibliometrics team
at the KTH Library.

## Contact

If you have queries related to the content, functionality or methods
used, please send an email to the KTH Library at <biblioteket@kth.se>.
For technical queries or feature requests, please [open an
issue](https://github.com/KTH-Library/bibliomatrix/issues) on this
repository.

## Installation

You can install the latest development version of bibliomatrix from
[GitHub](https://KTH-Library.github.com/bibliomatrix) with:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/bibliomatrix")
```

If you don’t already have `devtools`, this package can be installed
first with:

``` r
install.packages("devtools", dependencies = TRUE)
```

### Public data

The package includes data from the KTH Annual Bibliometric Monitoring
(ABM) public part.

``` r
library(bibliomatrix)
library(dplyr)

# Simple example showing some KTH units for which ABM results are public
abm_public_kth$meta %>%
  select(unit_code, unit_long_en) %>%
  head() %>%
  knitr::kable()
```

| unit\_code | unit\_long\_en                                         |
| :--------- | :----------------------------------------------------- |
| KTH        | KTH Royal Institute of Technology                      |
| A          | School of Architecture and the Built Environment (ABE) |
| 5851       | Architecture                                           |
| 5857       | Civil and Architectural Engineering                    |
| 875600     | Philosophy and History                                 |
| 5869       | Real Estate and Construction Management                |

``` r

# KTH is level 0, schools level 1 and departments level 2.
# Some basic information about KTH schools:
abm_public_kth$meta %>%
  filter(org_level == 1) %>%
  select(Diva_org_id, unit_code, unit_short, unit_long_en) %>%
  knitr::kable()
```

| Diva\_org\_id | unit\_code | unit\_short | unit\_long\_en                                                              |
| ------------: | :--------- | :---------- | :-------------------------------------------------------------------------- |
|          5850 | A          | ABE         | School of Architecture and the Built Environment (ABE)                      |
|        879223 | E          | EECS        | School of Electrical Engineering and Computer Science (EECS)                |
|          6091 | S          | SCI         | School of Engineering Sciences (SCI)                                        |
|        879224 | C          | CBH         | School of Engineering Sciences in Chemistry, Biotechnology and Health (CBH) |
|          6023 | I          | ITM         | School of Industrial Engineering and Management (ITM)                       |

``` r

# Get ABM results for KTH
kth_abm_tables <- abm_public_kth$units$KTH

# Show parts of ABM table 1 for KTH
kth_abm_tables[[1]] %>%
  select(Publication_Type_DiVA, P_frac, WoS_coverage) %>%
  filter(P_frac > 50) %>%
  knitr::kable()
```

| Publication\_Type\_DiVA       |     P\_frac | WoS\_coverage |
| :---------------------------- | ----------: | ------------: |
| Article, peer review          | 10330.64910 |     0.8814042 |
| Article, other                |   743.83892 |     0.8160024 |
| Conference paper, peer review |  5631.67421 |     0.5829022 |
| Conference paper, other       |  1758.63001 |     0.0294805 |
| Book                          |   100.54508 |     0.0000000 |
| Anthology (editor)            |    61.76667 |     0.0000000 |
| Chapter in book               |   927.51339 |     0.0460833 |
| Article, book review          |    97.10000 |     0.5880536 |
| Report                        |   643.56521 |     0.0000000 |
| Doctorate thesis              |  1976.00000 |     0.0000000 |
| Licentiate thesis             |   749.00000 |     0.0000000 |

## Development

To further develop or change the package, please refer to instructions
at <http://r-pkgs.org/>, then fork this repo, clone it, make the changes
and whe done, submit a PR with the changes.

If you wish to work from the CLI, you can use this approach:

<https://github.com/KTH-Library/kontarion#contributions>

For a concrete example - to make a change with regards to adding tests,
follow instructions for how to [add a
test](https://r-pkgs.org/tests.html) and add it in `test/testthat/`
making sure that it verifies expected results, then do the
Ctrl+Shift+{D,T,E} steps (if using keyboard shortcuts) and then use git
to commit and push the changes to your fork, then issue a PR.

## Releasing a stable version

Before making a stable release from the master branch, ensure all tests
are successful and that any issues identified from deployments in the
staging/reference environment have been resolved.

Then agree with the team on semantic versioning number for the stable
release. Normally given a format such as “1.0.0”, this would mean to
bump the third (last) number for a patch/bugfix, to bump the second
number for a minor update, such as when adding non-breaking
functionality, and to bump the first number for any major update that
might involve potentially backwards-compatibility breaking major
refactorings or rewrites.

To [release a stable version](http://r-pkgs.had.co.nz/release.html), use
the following steps:

  - Change the [version
    number](http://r-pkgs.had.co.nz/release.html#release-version) for
    the package in the [DESCRIPTION file](DESCRIPTION)
  - Make a local build in order to update the documentation site (this
    documentation is generated by `pkgdown` and does not build on cloud
    servers through Travis. It needs therefore to be built locally since
    the cloud servers do not have required network connectivity to
    database and LDAP sources) by following the steps in [dev.R](dev.R)
  - Provide docs in [NEWS.md](http://r-pkgs.had.co.nz/release.html#news)
    for what is new in the version, preferably by linking to the
    relevant corresponding GitHub milestone issues that have been
    adressed in the release.
  - Commit and push the changes using `git`
  - Tag the release version and push the tag using `git tag -a 0.9 -m
    "0.9"` and `git push origin 0.9` which will cause Travis to build
    and publish the release on the [GitHub Releases tab of the
    repository](/releases).

This versioned release can now be used elsewhere, such as in the
[`kontarion` repo](https://github.com/KTH-Library/kontarion) when making
a versioned release there where the tagged `bibliomatrix` release for
this specific version can be explicitly specified.
