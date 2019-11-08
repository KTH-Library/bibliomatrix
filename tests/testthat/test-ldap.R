# by default set skip tests to TRUE when running on Travis cloud
skip_ldap_tests <- TRUE

test_that("LDAP query for accoutname works", {
  skip_if(skip_ldap_tests, "skipping LDAP query tests in case we're in the cloud")
  df <- as.data.frame(ad_search("markussk", "accountname"))
  res <- subset(df, key == "kthCloudIdentity")$value
  expect_equal(res, "markussk@kth.se")
})

test_that("LDAP query for kthid and memberOf works", {
  skip_if(skip_ldap_tests, "skipping LDAP query tests in case we're in the cloud")
  df <- as.data.frame(ad_memberof(ad_search("u1o2ujjd")))
  is_consultant <- "system.consultants" %in% df$memberof
  expect_true(is_consultant)
})
