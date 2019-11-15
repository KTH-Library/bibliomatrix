# Tests to check consistency of results between old ABM and new results
#
# 

testunit<<- "13604"  # "5851" 13604 5857
testlevel<<- "dept" #"dept" school
acc_tolerance<- 0.001

# Function to read reference data exported from previous ABM
#----------------------------
abm_ref <- function(con = con_bib(), table = "cf_glid_dept_frac_aggr", unit_code) {
  res <- con %>% tbl(table)
  if (!missing(unit_code))
    res <- res %>% filter(unit %in% unit_code)
  
  res
}

compare_ref<- function(cols, refname, calctable){
  ref<- abm_ref(table = refname, unit_code=testunit)  %>% select(cols) %>% collect()
  calc<- calctable(unit_code=testunit)
  
  ref_df<- as.data.frame(ref) # needed for comparison - lazy-loaded SQL doesn't work
  calc_df<- as.data.frame(calc)
  
  i<- sapply(calc_df, is.factor)
  calc_df[i]<- lapply(calc_df[i], as.character)
  all.equal(ref_df, calc_df, check.names=FALSE, tolerance=acc_tolerance)
  
}


# Test comparing cf
#-----------------------
test_that("cf-test", {
  cols<- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean")
  refname= paste0("cf_glid_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table3)
  expect_true(comp_result)
  
})


# Test comparing c3-table
#----------------------------------
test_that("c3-test", {
  
  cols<- c("publication_year", "p_frac_Sum", "c3_frac_Sum", "c3_frac_mean_Sum")
  refname= paste0("c3_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table2)
  expect_true(comp_result)
  
})


# Test comparing jcf-table
#----------------------------------
test_that("jcf-test", {
  
  cols<- c("jrv_label_year", "Pfrac_Sum", "jrv_mean_frac_Sum", "top20_sum_frac_Sum", "top20_mean_frac_Sum")
  refname= paste0("jcf_glid_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table4)
  expect_true(comp_result)
  
})

# Test comparing co-publication table
#----------------------------------
test_that("co-pub test", {
  
  cols<- c("sp_label_year", "N", "swe_co_Sum", "swe_co_Mean", "int_Sum", "int_Mean")
  refname= paste0("copub_glid_",testlevel,"_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table5)
  expect_true(comp_result)
  
})

# test_that("cf-test", {
#   cols<- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean") #, "unit")
#   ref<- abm_ref(table = paste0("copub_glid_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% select(cols) %>% collect()
#   calc<- abm_table5(unit_code=testunit)
#   #colnames(calc)<- cols # not needed with all(X == Y)
# 
#   ref_df<- as.data.frame(ref) # simpler for comparison 
#   calc_df<- as.data.frame(calc)
#   
#   i<- sapply(calc_df, is.factor)
#   calc_df[i]<- lapply(calc_df[i], as.character)
#   #calc_df[1,3]<- 0.04
# 
#   #testresult<- all_equal(ref_df,calc)
#   #testresult<- all.equal(ref_df, calc_df, check.names=FALSE)
#   #expect_true(all(ref_df == calc_df))
#   #expect_true(testresult)
#   expect_true(all.equal(ref_df, calc_df, check.names=FALSE, tolerance=acc_tolerance))
# })
  