# Tests to check consistency of results between old ABM and new results
#
# 

testunit<<- "5851"  # "5851" 13604 5857
testlevel<<- "dept" #"dept"
acc_tolerance<- 0.005

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

#   cols<- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean") #, "unit")
#   ref<- abm_ref(table = paste0("cf_glid_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% select(cols) %>% collect()
#   calc<- abm_table3(unit_code=testunit)
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


# Test comparing c3-table
#----------------------------------
test_that("c3-test", {
  
  cols<- c("publication_year", "p_frac_Sum", "c3_frac_Sum", "c3_frac_mean_Sum")
  refname= paste0("c3_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table2)
  expect_true(comp_result)
  
})
  
#   cols<- c("publication_year", "p_frac_Sum", "c3_frac_Sum", "c3_frac_mean_Sum")
#   ref<- abm_ref(table = paste0("c3_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% select(cols) %>% collect()
#   calc<- abm_table2(unit_code=testunit)
#   #colnames(calc)<- cols # not needed with all(X == Y) 
#   
#   ref_df<- as.data.frame(ref)
#   calc_df<- as.data.frame(calc)
#   
#   expect_true(all.equal(ref_df,calc_df, check.names=FALSE, tolerance=acc_tolerance))
# })

# Test comparing jcf-table
#----------------------------------
test_that("jcf-test", {
  
  cols<- c("jrv_label_year", "Pfrac_Sum", "jrv_mean_frac_Sum", "top20_sum_frac_Sum", "top20_mean_frac_Sum")
  refname= paste0("jcf_glid_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table4)
  expect_true(comp_result)
  
})
  
#   cols<- c("jrv_label_year", "Pfrac_Sum", "jrv_mean_frac_Sum", "top20_sum_frac_Sum", "top20_mean_frac_Sum")
#   ref<- abm_ref(table = paste0("jcf_glid_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% select(cols) %>% collect()
#   calc<- abm_table4(unit_code=testunit)
#   
#   ref_df<- as.data.frame(ref)
#   calc_df<- as.data.frame(calc)
#   calc_df$interval<- as.character(calc_df$interval)
#   
#   expect_true(all.equal(ref_df,calc_df, check.names=FALSE, tolerance=acc_tolerance))
# })