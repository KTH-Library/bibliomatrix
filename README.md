
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bibliomatrix <img src="man/figures/sticker.png" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/KTH-Library/bibliomatrix/workflows/R-CMD-check/badge.svg)](https://github.com/KTH-Library/bibliomatrix/actions)
[![R-CMD-check](https://github.com/KTH-Library/bibliomatrix/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/bibliomatrix/actions/workflows/R-CMD-check.yaml)
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

You can install the latest released version of bibliomatrix from
[GitHub](https://KTH-Library.github.com/bibliomatrix) with:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/bibliomatrix")
```

For the latest development version, use the code below. *Note* that this
version may contain errors and experimental features.

``` r
devtools::install_github("KTH-Library/bibliomatrix@devel")
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
  knitr::kable(format = "pipe")
```

| unit_code | unit_long_en                                           |
|:----------|:-------------------------------------------------------|
| KTH       | KTH Royal Institute of Technology                      |
| A         | School of Architecture and the Built Environment (ABE) |
| 5851      | Architecture                                           |
| 5857      | Civil and Architectural Engineering                    |
| 875600    | Philosophy and History                                 |
| 5869      | Real Estate and Construction Management                |

``` r

# KTH is level 0, schools level 1 and departments level 2.
# Some basic information about KTH schools:
abm_public_kth$meta %>%
  filter(org_level == 1) %>%
  select(Diva_org_id, unit_code, unit_short, unit_long_en) %>%
  knitr::kable(format = "pipe")
```

| Diva_org_id | unit_code | unit_short | unit_long_en                                                                |
|------------:|:----------|:-----------|:----------------------------------------------------------------------------|
|        5850 | A         | ABE        | School of Architecture and the Built Environment (ABE)                      |
|      879223 | E         | EECS       | School of Electrical Engineering and Computer Science (EECS)                |
|        6091 | S         | SCI        | School of Engineering Sciences (SCI)                                        |
|      879224 | C         | CBH        | School of Engineering Sciences in Chemistry, Biotechnology and Health (CBH) |
|        6023 | I         | ITM        | School of Industrial Engineering and Management (ITM)                       |

``` r

# Get ABM results for KTH
kth_abm_tables <- abm_public_kth$units$KTH

# Show parts of ABM table 1 for KTH
kth_abm_tables[[1]] %>%
  select(Publication_Type_DiVA, P_frac, WoS_coverage) %>%
  filter(P_frac > 50) %>%
  knitr::kable(format = "pipe")
```

| Publication_Type_DiVA         |      P_frac | WoS_coverage |
|:------------------------------|------------:|-------------:|
| Article, peer review          | 14872.62876 |    0.8818465 |
| Article, other                |   927.71435 |    0.8156852 |
| Conference paper, peer review |  7436.47547 |    0.6058859 |
| Conference paper, other       |  1922.34506 |    0.0404143 |
| Book                          |   153.38119 |    0.0032599 |
| Anthology (editor)            |    77.65952 |    0.0042922 |
| Chapter in book               |  1477.59711 |    0.0274206 |
| Article, book review          |   124.85000 |    0.5474569 |
| Report                        |   746.33660 |    0.0000000 |
| Doctoral thesis               |  2703.00000 |    0.0000000 |
| Licentiate thesis             |   799.00000 |    0.0000000 |
| Patent                        |    74.40238 |    0.0000000 |

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
  number](http://r-pkgs.had.co.nz/release.html#release-version) for the
  package in the [DESCRIPTION file](DESCRIPTION)
- Make a local build in order to update the documentation site (this
  documentation is generated by `pkgdown` and does not build on cloud
  servers through Travis. It needs therefore to be built locally since
  the cloud servers do not have required network connectivity to
  database and LDAP sources) by following the steps in [dev.R](dev.R)
- Provide docs in [NEWS.md](http://r-pkgs.had.co.nz/release.html#news)
  for what is new in the version, preferably by linking to the relevant
  corresponding GitHub milestone issues that have been adressed in the
  release.
- Commit and push the changes using `git`
- Tag the release version and push the tag using
  `git tag -a 0.9 -m "0.9"` and `git push origin 0.9` which will cause
  Travis to build and publish the release on the [GitHub Releases tab of
  the repository](/releases).

This versioned release can now be used elsewhere, such as in the
[`kontarion` repo](https://github.com/KTH-Library/kontarion) when making
a versioned release there where the tagged `bibliomatrix` release for
this specific version can be explicitly specified.
