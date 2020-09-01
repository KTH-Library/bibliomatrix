test_that("fetching staff ids and corresponding pubs work", {
  
  # fetch the staff associated with the "c/ce" unit
  staff_cce <- unit_staff("c/ce")
  ids <- staff_cce %>% pull(kthid)
  
  # use their kthids to select publications from "masterfile"
  pubs <- abm_staff_data(kthids = ids)
  
  # if we get more than one thousand pubs, this test succeeds
  expect_gt(nrow(pubs), 1000)
})
