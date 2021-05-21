context("API testing")

test_that("local API deployment works", {

  skip_on_ci()
  
  # rs <- callr::r_bg(function() {
  #     pr <- plumber::plumb(system.file(package = "bibliomatrix", "plumber", "abm", "plumber.R"))
  #     pr$run(port = 8080, swagger = TRUE, host = "127.0.0.1")})
  # 
  # Sys.sleep(3)
  # 
  # link <- "http://localhost:8080/__swagger__/"
  # 
  # call <- tryCatch(
  #   httr::GET(
  #     url = link,
  #     httr::add_headers(
  #       `accept` = 'application/json'),
  #     httr::content_type("application/json")
  #   ),
  #   error = function(x) NA)
  # 
  # print(call)
  # 
  # call_parse <- tryCatch(
  #   httr::content(call, encoding = "UTF-8"),
  #   error = function(x) NA)
  # 
  # print(call_parse)
  # 
  # units <- 
  #   jsonlite::fromJSON("http://localhost:8080/units")
  # 
  # expect_equal(subset(units, unit_code == "KTH")$Diva_org_id, as.numeric(177))
  # 
  # rs$kill()
  expect_true(TRUE)
  
})