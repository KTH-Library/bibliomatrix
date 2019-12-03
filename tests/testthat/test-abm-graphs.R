test_that("abm_public_kth contains sort order table for publication types", {

  # the lookup table exists and has more than 10 rows  
  is_included <- 
    abm_public_kth$pt_ordning %>% nrow() > 10
  
  expect_true(is_included)
  
})

test_that("abm_graph_diva() and abm_graph_wos_coverage() does not require db connection", {
  
  # TODO: once this change is merged into a branch which contains the fcns above
  # check that the two functions return graphs, without using a db connection
  
})
