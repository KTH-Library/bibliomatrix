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
