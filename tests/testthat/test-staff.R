# by default set skip tests to TRUE when running on Travis cloud
skip_kthapi_tests <- TRUE

test_that("fetching staff ids and corresponding pubs work", {
  
  skip_if(skip_kthapi_tests, "skipping in case we're in the cloud")
  
  # fetch the staff associated with the "c/ce" unit
  staff_cce <- unit_staff("j/jh/jhs")
  ids <- staff_cce %>% pull(kthid)
  
  # use their kthids to select publications from "masterfile"
  pubs <- abm_staff_data(kthids = ids)
  
  # if we get more than one thousand pubs, this test succeeds
  expect_gt(nrow(pubs), 350)
})
