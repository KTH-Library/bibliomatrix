# by default set skip tests to TRUE when running on Travis cloud
skip_kthapi_tests <- TRUE

test_that("KTH Directory API query for accountname works", {
  skip_if(skip_kthapi_tests, "skipping KTH Directory API query tests in case we're in the cloud")
  res_ldap <- ad_displayname(kthid = ad_kthid("markussk"))
  res_kthapi <- kthapi::kth_displayname("markussk", "username")
  is_valid <- res_ldap == res_kthapi
  expect_true(res_kthapi == "Markus Skyttner (markussk)" && is_valid)
})

test_that("KTH Profiles API query for looking up 'kthid' from account name works", {
  skip_if(skip_kthapi_tests, "skipping KTH Profiles API test in case we're in the cloud")
  
  r1 <- kthapi::kth_profile(username = "markussk")$content$kthId
  r2 <- ad_kthid("markussk")

  expect_equal(r1, r2)
})

