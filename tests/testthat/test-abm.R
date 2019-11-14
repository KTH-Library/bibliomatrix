# Tests to check consistency of results between old ABM and new results
#
# Test for git account.


test_that("cf-test", {
  
  abm_ref <- function(con = con_bib(), table = "cf_glid_dept_frac_aggr", unit_code) {
    res <- con %>% tbl(table)
    if (!missing(unit_code))
      res <- res %>% filter(unit %in% unit_code)
    
    res
  }
  
  testcode<- "5851" # slection of unit to check for manual testing
  cols<- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean") #, "unit")
  ref<- abm_ref(table = "cf_glid_dept_frac_aggr", unit_code=testcode)  %>% select(cols)
  calc<- abm_table3(unit_code=testcode)
  #colnames(calc)<- cols # not needed with all(X == Y) 

  ref_df<- as.data.frame(ref)

  #testresult<- all_equal(ref_df,calc)
  expect_true(all(ref_df == calc))
  #expect_true(testresult)
 # expect_equal(2 * 2, 4)
})
