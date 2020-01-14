# Test "everything": All orgs and individuals and all ABM tables.
library(dplyr)
library(testthat)
library(pool)

# Set this flag to FALSE when testing locally, leave TRUE to skip these tests on Travis
# since Travis does not have access to closed data sources
skip_full_abm_test <- TRUE

# The accepted difference (counts as equal if diff < acc_tolerance)
acc_tolerance <- 0.00001

### Define test functions ###
test_tab1 <- function(con = con_bib(), testlevel, unit_code){
  # Get reference table
  cols_pub <- c("Publication_Type_DiVA", "Doc_Year", "w_d_Sum")
  cols_wos <- c("Publication_Type_DiVA", "w_d_Sum", "wos_coverage_Mean")
  refname <- paste0("publ_",testlevel,"_frac_aggr")
  ref_pub <- con %>% tbl(refname) %>% filter(unit == unit_code && !is.na(Doc_Year)) %>% select(cols_pub) %>% arrange(Doc_Year) %>% as.data.frame()
  ref_wos <- con %>% tbl(refname) %>% filter(unit == unit_code && is.na(Doc_Year)) %>% select(cols_wos) %>% as.data.frame()
  reftable <- ref_pub %>%
    pivot_wider(names_from = Doc_Year, values_from = w_d_Sum) %>%
    inner_join(ref_wos, by = c("Publication_Type_DiVA")) %>%
    arrange(Publication_Type_DiVA) %>% 
    as.data.frame()

  # Get ABM table 
  abmtable <- abm_table1(con, unit_code) %>% arrange(Publication_Type_DiVA) %>%  as.data.frame()

  # Check if tables are equal
  isTRUE(all.equal(reftable, abmtable, check.names = FALSE, tolerance = acc_tolerance))
}

test_tab2 <- function(con = con_bib(), testlevel, unit_code) {
  # Get reference table
  cols <- cols<- c("publication_year", "p_frac_Sum", "c3_frac_Sum", "c3_frac_mean_Sum")
  refname <- paste0("c3_",testlevel,"_frac_aggr")
  reftable <- con %>% tbl(refname) %>% filter(unit == unit_code) %>% select(cols) %>% as.data.frame()
  
  # Get ABM table
  abmtable <- abm_table2(con, unit_code) %>% as.data.frame()
  
  # Check if tables are equal
  isTRUE(all.equal(reftable, abmtable, check.names = FALSE, tolerance = acc_tolerance))
}

test_tab3 <- function(con = con_bib(), testlevel, unit_code) {
  # Get reference table
  cols <- c("cf_label_year", "w_d_Sum", "cf_scxwo_Mean", "top10_scxwo_Sum", "top10_scxwo_Mean")
  refname <- paste0("cf_glid_",testlevel,"_frac_aggr")
  reftable <- con %>% tbl(refname) %>% filter(unit == unit_code) %>% select(cols) %>% as.data.frame()
    
  # Get ABM table
  abmtable <- abm_table3(con, unit_code) %>% as.data.frame()
    
  # Check if tables are equal
  isTRUE(all.equal(reftable, abmtable, check.names = FALSE, tolerance = acc_tolerance))
}

test_tab4 <- function(con = con_bib(), testlevel, unit_code) {
  # Get reference table
  cols<- c("jrv_label_year", "Pfrac_Sum", "jrv_mean_frac_Sum", "top20_sum_frac_Sum", "top20_mean_frac_Sum")
  refname <- paste0("jcf_glid_",testlevel,"_frac_aggr")
  reftable <- con %>% tbl(refname) %>% filter(unit == unit_code) %>% select(cols) %>% as.data.frame()
  
  # Get ABM table
  abmtable <- abm_table4(con, unit_code) %>% as.data.frame()
    
  # Check if tables are equal
  isTRUE(all.equal(reftable, abmtable, check.names = FALSE, tolerance = acc_tolerance))
}

test_tab5 <- function(con = con_bib(), testlevel, unit_code) {
  # Get reference table
  cols<- c("sp_label_year", "N", "swe_co_Sum", "swe_co_Mean", "int_Sum", "int_Mean")
  refname <- paste0("copub_glid_",testlevel,"_aggr")
  reftable <- con %>% tbl(refname) %>% filter(unit == unit_code) %>% select(cols) %>% as.data.frame()
  
  # Get ABM table
  abmtable <- abm_table5(con, unit_code) %>% as.data.frame()
  
  # Check if tables are equal
  isTRUE(all.equal(reftable, abmtable, check.names = FALSE, tolerance = acc_tolerance))
}

### Run tests ###
if(!skip_full_abm_test){
  db <- pool_bib()
  
  # KTH and schools
  schoolunits <- unit_info(db) %>% filter(org_level %in% c(0,1)) %>% pull(unit_code)
  test_that("school_tables", {
    skip_if(skip_full_abm_test, "skipping (we might have no connection to database)")
    expect_true(all(sapply(schoolunits, function(x) test_tab1(con = db, testlevel = "school", unit_code = x))))
    expect_true(all(sapply(schoolunits, function(x) test_tab2(con = db, testlevel = "school", unit_code = x))))
    expect_true(all(sapply(schoolunits, function(x) test_tab3(con = db, testlevel = "school", unit_code = x))))
    expect_true(all(sapply(schoolunits, function(x) test_tab4(con = db, testlevel = "school", unit_code = x))))
    expect_true(all(sapply(schoolunits, function(x) test_tab5(con = db, testlevel = "school", unit_code = x))))
  })

  # Departments
  deptunits <- unit_info(db) %>% filter(org_level == 2) %>% pull(unit_code)
  test_that("dept_tables", {
    skip_if(skip_full_abm_test, "skipping (we might have no connection to database)")
    expect_true(all(sapply(deptunits, function(x) test_tab1(con = db, testlevel = "dept", unit_code = x))))
    expect_true(all(sapply(deptunits, function(x) test_tab2(con = db, testlevel = "dept", unit_code = x))))
    expect_true(all(sapply(deptunits, function(x) test_tab3(con = db, testlevel = "dept", unit_code = x))))
    expect_true(all(sapply(deptunits, function(x) test_tab4(con = db, testlevel = "dept", unit_code = x))))
    expect_true(all(sapply(deptunits, function(x) test_tab5(con = db, testlevel = "dept", unit_code = x))))
  })
  
  # Individual researchers
  resunits <- abm_data(unit_level = 3) %>% pull(Unit_code) %>% unique()
  test_that("res_tables", {
    skip_if(skip_full_abm_test, "skipping (we might have no connection to database)")
    expect_true(all(sapply(resunits, function(x) test_tab1(con = db, testlevel = "res", unit_code = x))))
    expect_true(all(sapply(resunits, function(x) test_tab2(con = db, testlevel = "res", unit_code = x))))
    expect_true(all(sapply(resunits, function(x) test_tab3(con = db, testlevel = "res", unit_code = x))))
    expect_true(all(sapply(resunits, function(x) test_tab4(con = db, testlevel = "res", unit_code = x))))
    expect_true(all(sapply(resunits, function(x) test_tab5(con = db, testlevel = "res", unit_code = x))))
  })

  poolClose(db)
}
