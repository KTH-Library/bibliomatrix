
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
# only show data given a specific unit id (KTH = 177)
filter(id == 177) %>%
select(`Organizational Unit`, `Publication Type`, `WoS Coverage`) %>%
knitr::kable()
```

| Organizational Unit               | Publication Type              | WoS Coverage |
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

This is a basic example of how to retrieve data for the organizational
units used in the Annual Bibliometric Monitoring:

``` r
# get data for organizational units at 
# level 1 (KTH), level 2 (Schools) and level 3 (Departments / Institutions)
# NB: this table is not yet complete and some entries lack english translations

org <- abm_units()

# inspect some organizational unit ids at "level 2" ie at the school level

org %>%
filter(unit_level == 2) %>%
knitr::kable()
```

| unit\_long\_swe                                    | unit\_id | unit\_level | unit\_abbrev | unit\_pid | unit\_long\_eng                                                             | unit\_sort |
| :------------------------------------------------- | -------: | ----------: | :----------- | --------: | :-------------------------------------------------------------------------- | ---------: |
| Skolan för kemi, bioteknologi och hälsa (CBH)      |   879224 |           2 | C            |       177 | School of Engineering Sciences in Chemistry, Biotechnology and Health (CBH) |          2 |
| Skolan för arkitektur och samhällsbyggnad (ABE)    |     5850 |           2 | A            |       177 | School of Architecture and the Built Environment (ABE)                      |          1 |
| Skolan för elektroteknik och datavetenskap (EECS)  |   879223 |           2 | E            |       177 | School of Electrical Engineering and Computer Science (EECS)                |          3 |
| Skolan för industriell teknik och management (ITM) |     6023 |           2 | I            |       177 | School of Industrial Engineering and Management (ITM)                       |          4 |
| Skolan för teknikvetenskap (SCI)                   |     6091 |           2 | S            |       177 | School of Engineering Sciences (SCI)                                        |          5 |

Some data relating to a specific school using the organizational unit
id:

``` r

abm_tab1() %>%
filter(id == 879223) %>%
select(-id, -pubtype_order, -c(starts_with("20"))) %>%
knitr::kable()
```

| Organizational Unit                                          | Publication Type              | WoS Coverage |       Total |
| :----------------------------------------------------------- | :---------------------------- | -----------: | ----------: |
| School of Electrical Engineering and Computer Science (EECS) | Article, peer review          |    0.8804322 | 1973.472157 |
| School of Electrical Engineering and Computer Science (EECS) | Article, other                |    0.7437743 |   79.817100 |
| School of Electrical Engineering and Computer Science (EECS) | Conference paper, peer review |    0.5764869 | 2876.217229 |
| School of Electrical Engineering and Computer Science (EECS) | Conference paper, other       |    0.0556550 |  338.736767 |
| School of Electrical Engineering and Computer Science (EECS) | Book                          |    0.1589574 |   13.525637 |
| School of Electrical Engineering and Computer Science (EECS) | Anthology (editor)            |    0.0000000 |    5.116667 |
| School of Electrical Engineering and Computer Science (EECS) | Chapter in book               |    0.0674281 |  121.116739 |
| School of Electrical Engineering and Computer Science (EECS) | Article, book review          |    0.2045455 |    2.933333 |
| School of Electrical Engineering and Computer Science (EECS) | Proceeding (editor)           |    0.0000000 |   11.008333 |
| School of Electrical Engineering and Computer Science (EECS) | Report                        |    0.0000000 |   91.609695 |
| School of Electrical Engineering and Computer Science (EECS) | Doctorate thesis              |    0.0000000 |  508.000000 |
| School of Electrical Engineering and Computer Science (EECS) | Licentiate thesis             |    0.0000000 |  167.000000 |
| School of Electrical Engineering and Computer Science (EECS) | Patent, approved              |    0.0000000 |   17.326190 |
