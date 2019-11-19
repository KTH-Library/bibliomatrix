# This file just keeps some of the code used when creating this package
# and it is not part of the package. It is provided for convencience only.

library(devtools)
library(usethis)

use_description()
use_r("data_bibmon.R")
use_namespace()
use_build_ignore("dev.R")

use_readme_rmd()
use_testthat()
use_test("connection")



use_data_raw()

build_vignettes()
file.copy("doc", "inst", recursive = TRUE)

pkgdown::build_site()
use_test("db")
use_package("curl")

pkgdown::build_site()

# Travis CI

remotes::install_github("ropenscilabs/travis")

library(travis)

travis_enable()

use_travis_deploy()