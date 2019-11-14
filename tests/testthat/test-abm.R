# Tests to check consistency of results between old ABM and new results
#
# Test for git account.

testunit<<- "5851"  # "5851" 13604 5857
testlevel<<- "dept" #"dept"
acc_tolerance<- 0.005

abm_ref <- function(con = con_bib(), table = "cf_glid_dept_frac_aggr", unit_code) {
  res <- con %>% tbl(table)
  if (!missing(unit_code))
    res <- res %>% filter(unit %in% unit_code)
  
  res
}

test_that("cf-test", {
  cols<- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean") #, "unit")
  ref<- abm_ref(table = paste0("cf_glid_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% select(cols)
  calc<- abm_table3(unit_code=testunit)
  #colnames(calc)<- cols # not needed with all(X == Y)

  ref_df<- as.data.frame(ref) # needed for comparison - lazy-loaded SQL doesn't work
  calc_df<- as.data.frame(calc)
  
  calc_df$interval<- as.character(calc_df$interval)
  #calc_df[1,3]<- 0.04

  #testresult<- all_equal(ref_df,calc)
  #testresult<- all.equal(ref_df, calc_df, check.names=FALSE)
  #expect_true(all(ref_df == calc_df))
  #expect_true(testresult)
  expect_true(all.equal(ref_df, calc_df, check.names=FALSE, tolerance=acc_tolerance))
})


test_that("c3-test", {
  
  #testcode<- "5851" # slection of unit to check for manual testing
  cols<- c("publication_year", "p_frac_Sum", "c3_frac_Sum", "c3_frac_mean_Sum")
  ref<- abm_ref(table = paste0("c3_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% select(cols)
  calc<- abm_table2(unit_code=testunit)
  #colnames(calc)<- cols # not needed with all(X == Y) 
  
  ref_df<- as.data.frame(ref)
  calc_df<- as.data.frame(calc)
  
  #testresult<- all_equal(ref_df,calc)
  expect_true(all.equal(ref_df,calc_df, check.names=FALSE, tolerance=acc_tolerance))
  #expect_true(testresult)
  # expect_equal(2 * 2, 4)
})