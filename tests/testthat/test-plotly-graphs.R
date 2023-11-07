test_that("plotly charts work for KTH", {
  
  skip_on_ci()

  uc <- "KTH"
  
  df_diva <- pluck(abm_public_kth$units, uc, 1)
  df_cit3y <- pluck(abm_public_kth$units, uc, 2)
  df_cf <- pluck(abm_public_kth$units, uc, 3)
  df_jcf <- pluck(abm_public_kth$units, uc, 4)
  df_copub <- pluck(abm_public_kth$units, uc, 5)
  df_oa <- pluck(abm_public_kth$units, uc, "oa")
  df_woscov <- pluck(abm_public_kth$units, uc, "coverage")
  
  
  has_rows <- df_copub %>% filter(!is.na(P_full)) %>% nrow > 0

  last_year <- ifelse(has_rows, as.numeric(nth(df_copub$Publication_Year, -2)))
  
  waffle_share <- df_copub |>
    filter(Publication_Year %in% (last_year-2):last_year) |> 
    summarise(P = sum(P_full, na.rm = T),
              nonuniv_share = sum(nonuniv_count, na.rm = T) / P,
              int_share = sum(int_count, na.rm = T) / P)
  
  nonuniv_lbl <- sprintf("Swedish non-university: %d%%", round(100 * waffle_share$nonuniv_share))
  int_lbl <- sprintf("International: %d%%", round(100 * waffle_share$int_share))
  
  waffle1 <- abm_waffle_pct(waffle_share$nonuniv_share, label = nonuniv_lbl)
  waffle2 <- abm_waffle_pct(waffle_share$int_share, label = int_lbl)
  
  waffles <- waffle1 / waffle2

  waffles
  
  w1 <-
    abm_waffle_pct_plotly(waffle_share$nonuniv_share, label = nonuniv_lbl)
  
  w2 <-
    abm_waffle_pct_plotly(waffle_share$int_share, label = int_lbl)
  
  plotly::subplot(plotly::subplot(w1), plotly::subplot(w2), nrows = 2)

  abm_graph_diva(df_diva) + theme_kth()
  abm_graph_wos_coverage(df_diva) + theme_kth_osc()
  abm_graph_wos_coverage_plotly(df_diva)
  
  expect_true(TRUE)
  
})



