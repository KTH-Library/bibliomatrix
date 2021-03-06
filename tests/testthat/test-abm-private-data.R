test_that("private data can be retrieved for five different kthids", {
  
  skip_on_ci()
  
  library(dplyr)
  library(purrr)
  
  kthids <- trimws(readr::read_lines(
"u1kzf1xh
u1g9umtq
u1jr9fll 
u1ygqmuy
u13bp6vd
u18qe64m"))

  # if this is changed to include all entries ie kthids[1:6]
  # the test fails
  res <- map(kthids[1:6], abm_private_data)

  # check that returned results have meta and unit slots with data
  has_meta <- function(x) x %>% pluck("meta") %>% nrow() > 0
  has_unit <- function(x) x %>% pluck("units") %>% pluck(1, 1) %>% nrow() > 0
  has_both <- function(x) all(has_meta(x), has_unit(x))
  
  # if both meta and unit slots exist for all results -> it checks out
  all_has_both <- all(map_lgl(res, has_both))
  
  expect_true(all_has_both)
  
})
