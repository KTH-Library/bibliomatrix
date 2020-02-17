# This file just keeps some of the code used when creating this package
# and it is not part of the package. It is provided for convencience only.

library(devtools)
library(usethis)
library(here)

## DO THIS when pushing

document()
check()
# remember to update the bundled data
db_sync(overwrite_existing = TRUE)
source(here("data-raw/public_data.R"))

# changes to abm.Rmd may require Ctrl-Shift-B (for new interal pkg data to be installed)!

# first revert the .gitignore if it has been changed by below
build_vignettes()
# git revert the .gitignore to remove the "doc" line in there
# Remember to update news.md before build_site()
build_site()
file.copy("doc", "inst", recursive = TRUE)
