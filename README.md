
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bibliomatrix

<!-- badges: start -->

<!-- badges: end -->

The goal of bibliomatrix is to provide a common set of functionality for
the Annual Bibliometric Monitoring efforts from the Bibliometrics team
at the KTH Library.

## Installation

You can install the latest development version of bibliomatrix from
[GitHub](https://KTH-Library.github.com/bibliomatrix) with:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/bibliomatrix")
```

## Example

This is a basic example which shows you how to get some data when
creating Annual Bibliometric Monitoring analytics:

``` r
library(bibliomatrix)
library(dplyr)

# simple example showing how to filter some of the data for Table 1
# in the ABM report for an organizational unit and restrict the
# output to a specific set of columns

abm_tab1() %>% 
filter(unit_long == "KTH Royal Institute of Technology") %>%
select(unit_long, `Publication Type`, `WoS Coverage`) %>%
knitr::kable()
#> Warning: Missing values are always removed in SQL.
#> Use `SUM(x, na.rm = TRUE)` to silence this warning
#> This warning is displayed only once per session.
```

| unit\_long                        | Publication Type              | WoS Coverage |
| :-------------------------------- | :---------------------------- | -----------: |
| KTH Royal Institute of Technology | Article, peer review          |    0.8623932 |
| KTH Royal Institute of Technology | Article, other                |    0.8080809 |
| KTH Royal Institute of Technology | Conference paper, peer review |    0.4840580 |
| KTH Royal Institute of Technology | Conference paper, other       |    0.0255618 |
| KTH Royal Institute of Technology | Book                          |    0.0262478 |
| KTH Royal Institute of Technology | Anthology (editor)            |    0.0000000 |
| KTH Royal Institute of Technology | Chapter in book               |    0.0397090 |
| KTH Royal Institute of Technology | Article, book review          |    0.5369030 |
| KTH Royal Institute of Technology | Proceeding (editor)           |    0.0000000 |
| KTH Royal Institute of Technology | Report                        |    0.0000000 |
| KTH Royal Institute of Technology | Doctorate thesis              |    0.0000000 |
| KTH Royal Institute of Technology | Licentiate thesis             |    0.0000000 |
| KTH Royal Institute of Technology | Patent, approved              |    0.0000000 |
