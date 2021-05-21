test_that("abm_public_kth contains sort order table for publication types", {

  # the lookup table exists and has more than 10 rows  
  is_included <- 
    abm_public_kth$pubtype_order %>% nrow() > 10
  
  expect_true(is_included)
  
})

