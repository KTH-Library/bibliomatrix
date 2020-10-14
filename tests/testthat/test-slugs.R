context("Testing enumeration of slugs and related researcher kthids")

# by default set skip tests to TRUE when running on Travis cloud
skip_kthapi_tests <- TRUE

test_that("Catalog crawl descending into kth_catalog beneath slug 'j/jh/jhs' works", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  lookup <- kth_catalog_crawl("j/jh/jhs")
  is_valid <- nrow(lookup) == 0
  expect_true(is_valid)
})

test_that("Catalog crawl descending into kth_catalog beneath slug 'j/jh' works", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  lookup <- kth_catalog_crawl("j/jh")
  is_valid <- nrow(lookup) > 0
  expect_true(is_valid)
})

test_that("Crawling invalid slug gives an error", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  expect_error(kth_catalog_crawl("blaha"))
})

test_that("Valid institutional slugs for ABM are listed", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  abm_slugs <- abm_slugs_institutions()
  is_valid <- all(abm_slugs %in% unit_info()$slug)
  expect_true(is_valid)
})

test_that("Live enumeration of divisions in ABM works", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  divs <- kth_divisions_crawl()
  is_valid <- nrow(divs) > 100
  expect_true(is_valid)
})

test_that("Retrieval of researchers in division 'j/jh/jhs' works", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  kthids <- kthids_from_slug('j/jh/jhs')
  is_valid <- nrow(kthids) > 5
  expect_true(is_valid)
})

