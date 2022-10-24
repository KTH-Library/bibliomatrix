test_that("KTH Directory API query for accountname works", {
  skip_on_ci()
  res_kthapi <- displayname_from_kthid("u1o2ujjd")
  expect_true(res_kthapi == "Markus Skyttner (markussk)")
})

test_that("KTH Profiles API query for looking up 'kthid' from account name works", {
  skip_on_ci()

  r1 <- kthid_from_accountname("markussk")
  r2 <- "u1o2ujjd"
  expect_equal(r1, r2)
})

