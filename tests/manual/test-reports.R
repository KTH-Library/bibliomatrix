test_that("report generation works for units with no publications", {
  
  p1 <- list(
    unit_code = "u1o2ujjd",
    is_employee = TRUE,
    embed_data = TRUE,
    use_package_data = FALSE
  )
  
  expect_warning(render_report(myparams = p1), regexp = "no non-missing arguments")
})

test_that("report generation works for largest unit", {
  
    
  p2 <- list(
    unit_code = "KTH",
    is_employee = FALSE,
    embed_data = TRUE,
    use_package_data = FALSE
  )
  
  t1 <- render_report(myparams = p2)
  
  expect_true(t1$name == "KTH")
})

test_that("report generation works for several reports", {
  
  myparamz <- tibble(
    unit_code = abm_units$unit_code,
    is_employee = FALSE,
    embed_data = TRUE,
    use_package_data = TRUE
  ) %>% slice(1:2) %>%
    purrr::transpose()

  t1 <- 
    myparamz %>%
    purrr::map_df(function(x) render_report(myparams = x))
  
  expect_true(nrow(t1) == 2)
  
})
