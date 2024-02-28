test_that("copub country tables work for public data", {
  
  unit_code <- "KTH"
  
  df_copub_countries <- 
    pluck(abm_public_kth$units, unit_code, "wos_copub_countries")
  
  res <- abm_ui_datatable_copub_countries(
    df_copub_countries,
    unit_file_label = "Test file label", 
    unit_title = "Test title"
  ) 
  
  res_kable <- abm_ui_kable_copub_countries(df_copub_countries) 
  
  is_valid <- !is.null(res$x$data) && (nchar(res_kable) > 0)
  
  expect_true(is_valid)
  
})

test_that("copub country tables work for public data", {
  
  unit_code <- "KTH"
  
  df_copub_orgs <- 
    df_copub_orgs <- pluck(abm_public_kth$units, unit_code, "wos_copub_orgs")
  
  res <- abm_ui_datatable_copub_orgs(
    df_copub_orgs,
    unit_file_label = "Test file label", 
    unit_title = "Test unit title"
  )
  
  res_kable <- abm_ui_kable_copub_orgs(df_copub_orgs) 
  
  is_valid <- !is.null(res$x$data) && (nchar(res_kable) > 0)
  
  expect_true(is_valid)
  
})

test_that("copub country tables work with db connection", {
  
  skip_on_ci()
  
  uc <- "u1o2ujjd"
  
  con <- con_bib_mssql()
  on.exit(DBI::dbDisconnect(con))
  
  df_copub_countries <- abm_copub_countries(
      con = con, unit_level = 3, unit_code = uc)
  
  res <- abm_ui_datatable_copub_countries(
    df_copub_countries,
    unit_file_label = "Test file label", 
    unit_title = "Test title"
  ) 
  
  res_kable <- abm_ui_kable_copub_countries(df_copub_countries) 
  
  is_valid <- all(grepl("no publications available", 
    c(as.character(res), as.character(res_kable))))
  
  expect_true(is_valid)
  
})

test_that("copub country tables work with db connection", {
  
  skip_on_ci()
  
  uc <- "u1o2ujjd"
  
  con <- con_bib_mssql()
  on.exit(DBI::dbDisconnect(con))
  
  df_copub_orgs <- abm_copub_orgs(
    con = con, unit_level = 3, unit_code = uc)
  
  res <- abm_ui_datatable_copub_orgs(
    df_copub_orgs,
    unit_file_label = "Test file label", 
    unit_title = "Test unit title"
  )
  
  res_kable <- abm_ui_kable_copub_orgs(df_copub_orgs) 
  
  is_valid <- all(grepl("no publications available", 
    c(as.character(res), as.character(res_kable))))
  
  expect_true(is_valid)
  
})

test_that("same result is returned from sqlite db and public data", {
  
  skip_on_ci()
  
  uc <- "KTH"
  
  a1 <- 
    pluck(abm_public_kth$units, uc, "wos_copub_countries") |>
    arrange(country)
  
  a2 <- 
    pluck(abm_public_kth$units, uc, "wos_copub_orgs") |>
    arrange(country, org)
  
  con <- con_bib_sqlite()
  on.exit(DBI::dbDisconnect(con))
  
  b1 <- 
    abm_copub_countries(con = con, unit_level = 0, unit_code = uc) |>
    arrange(country)
  
  b2 <- 
    abm_copub_orgs(con = con, unit_level = 0, unit_code = uc) |>
    arrange(country, org)
  
  is_valid <- all.equal(a1, b1) && all.equal(a2, b2)
  
  expect_true(is_valid)
  
})

