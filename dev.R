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

use_package("igraph")
use_package("")

use_data_raw()


## DO THIS when pushing

# remember to update the bundled data
document()
check()
source("data-raw/public_data.R")
# first revert the .gitignore if it has been changed by below
build_vignettes()
pkgdown::build_site()
file.copy("doc", "inst", recursive = TRUE)



use_test("db")
use_package("curl")

pkgdown::build_site()

# Travis CI

remotes::install_github("ropenscilabs/travis")

library(travis)

travis_enable()

use_travis_deploy()