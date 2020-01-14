## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(bibliomatrix)
library(dplyr)

# To show the units included in the ABM, use either abm_public_kth$meta or the unit_info() function
identical(unit_info(), abm_public_kth$meta)
unit_info()

# The unit_info() function can take parameters for unit, level or parent.

# For schools, unit is a single letter, for departments it is equal to the DiVA organization id
unit_info(unit = "I")
unit_info(unit = "5869")

# Unit level is 0 for KTH, 1 for schools and 2 for departments
unit_info(level = 1)

# The parent parameter can be used to show all departments of some particular school
# (note that the parent id is always a Diva organization id).
# The result can of course be further processed as any other dataset
unit_info(parent = 6023) %>% select(unit_code, unit_long_en) %>% arrange(unit_long_en)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(bibliomatrix)
library(dplyr)

# The five tables presented in the ABM is available in abm_public_kth$units for each unit.

# Get public ABM results for KTH
kth_abm_tables <- abm_public_kth$units$KTH

# Show parts of table 1
knitr::kable(kth_abm_tables[[1]] %>% select(Publication_Type_DiVA, P_frac, WoS_coverage))

# Get summary data for KTH (used in the ABM dashboard)
kth_abm_tables$summaries

# Note that numeric unit codes need to be enclosed with ` 
abm_public_kth$units$`5851`$summaries

## ---- warning=FALSE, message=FALSE---------------------------------------
library(bibliomatrix)

# The only use for the pubtype_order part of the public data is to present
# the DiVA publication type numbers in a consistent manner
abm_public_kth$pubtype_order

