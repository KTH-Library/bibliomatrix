# Tests to check consistency of results between old ABM and new results
#
# Currently this is only testing the output from a single testing unit (unit_code), at a specified level (testlevel below)
#

# Set this flag to FALSE when testing locally, leave TRUE to skip these tests on Travis
# since Travis does not have access to closed data sources
skip_db_tests <- TRUE

testunit<<- "13604"  # "5851" 13604 5857
testlevel<<- "dept" #"dept" school
acc_tolerance<- 0.00001

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
  calc_df[i]<- invisible(lapply(calc_df[i], as.character, war)) # invisible to supress warnings for character conversion
  all.equal(ref_df, calc_df, check.names=FALSE, tolerance=acc_tolerance)
  
}


# Test comparing cf
#-----------------------
test_that("cf-test", {
  skip_if(skip_db_tests, "skipping (we may be the test without access to the data source")
  cols<- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean")
  refname= paste0("cf_glid_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table3)
  expect_true(comp_result)
  
})


# Test comparing c3-table
#----------------------------------
test_that("c3-test", {
  
  skip_if(skip_db_tests, "skipping (we may be the test without access to the data source")
  
  cols<- c("publication_year", "p_frac_Sum", "c3_frac_Sum", "c3_frac_mean_Sum")
  refname= paste0("c3_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table2)
  expect_true(comp_result)
  
})


# Test comparing jcf-table
#----------------------------------
test_that("jcf-test", {
  
  skip_if(skip_db_tests, "skipping (we may be the test without access to the data source")
  
  cols<- c("jrv_label_year", "Pfrac_Sum", "jrv_mean_frac_Sum", "top20_sum_frac_Sum", "top20_mean_frac_Sum")
  refname= paste0("jcf_glid_",testlevel,"_frac_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table4)
  expect_true(comp_result)
  
})

# Test comparing co-publication table
#----------------------------------
test_that("co-pub test", {
  
  skip_if(skip_db_tests, "skipping (we may be the test without access to the data source")
  
  cols<- c("sp_label_year", "N", "swe_co_Sum", "swe_co_Mean", "int_Sum", "int_Mean")
  refname= paste0("copub_glid_",testlevel,"_aggr")
  
  comp_result<- compare_ref(cols=cols, refname=refname, calctable=abm_table5)
  expect_true(comp_result)
  
})


# Test comparing Diva table
#
# This is not using the compare_ref() function above, since this table needs some special wrangling before 
# comparisons.
#----------------------------------
test_that("Publ. volume test", {
  
  skip_if(skip_db_tests, "skipping (we may be the test without access to the data source")
  
  #Prepare reference table - must be pivoted and merged due to structure
  ref_raw<- abm_ref(table = paste0("publ_",testlevel,"_frac_aggr"), unit_code=testunit)  %>% collect()
  ref_piv<- ref_raw %>% select(-"_TYPE_", -"_PAGE_", -"_TABLE_", -"wos_coverage_Mean", -"unit") %>% 
      filter(!is.na(Doc_Year)) %>% 
      pivot_wider(names_from = Doc_Year, values_from = w_d_Sum) 
  
  ref_comp<- ref_raw %>% filter(is.na(Doc_Year)) %>% select(-"_TYPE_", -"_PAGE_", -"_TABLE_", -"Doc_Year", -"unit")
  ref_full<- merge(ref_piv,ref_comp, by="Publication_Type_DiVA")
  
  # Prepare calculated table
  calc_test<- abm_table1(unit_code=testunit)
  calc_sort<- calc_test %>% arrange(Publication_Type_DiVA)
  
  comp_result<- all.equal(ref_full, calc_sort, ignore_row_order= TRUE, check.names=FALSE, tolerance=acc_tolerance)
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
  