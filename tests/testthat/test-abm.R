# Tests to check consistency of results between old ABM and new results
#
# Test for git account.

abm_ref <- function(con = con_bib(), table = "cf_glid_dept_frac_aggr", unit_code) {
  res <- con %>% tbl(table)
  if (!missing(unit_code))
    res <- res %>% filter(unit %in% unit_code)
  
  res
}

ref<- abm_ref(table = "cf_glid_dept_frac_aggr", unit_code="5851")

calc<- abm_table3(unit_code="5851")
cols<- c("unit", "cf_label_year", "w_d_sum", "cf_scxwo_Mean", "top10_scxwo_sum", "top10_scxwo_Mean")
ref<- ref[,cols]



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
