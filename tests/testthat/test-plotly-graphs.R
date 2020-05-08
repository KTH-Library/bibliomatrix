# df_diva <- pluck(abm_public_kth$units, params$unit_code, 1)
# df_cit3y <- pluck(abm_public_kth$units, params$unit_code, 2)
# df_cf <- pluck(abm_public_kth$units, params$unit_code, 3)
# df_jcf <- pluck(abm_public_kth$units, params$unit_code, 4)
# df_copub <- pluck(abm_public_kth$units, params$unit_code, 5)
# df_oa <- pluck(abm_public_kth$units, params$unit_code, "oa")
# df_woscov <- pluck(abm_public_kth$units, params$unit_code, "coverage")
# 
# 
# has_rows <- df_copub %>% filter(!is.na(P_full)) %>% nrow > 0
# last_interval <- ifelse(has_rows, nth(df_copub$interval, -2), "")
# 
# nonuniv_share <- as.numeric(filter(df_copub, interval == last_interval)$nonuniv_share)
# nonuniv_lbl <- sprintf("Swedish non-university: %d%%", round(100 * nonuniv_share))
# waffle1 <- abm_waffle_pct(nonuniv_share, label = nonuniv_lbl) 
# 
# int_share <- as.numeric(filter(df_copub, interval == last_interval)$int_share)
# int_lbl <- sprintf("International: %d%%", round(100*int_share))
# waffle2 <- abm_waffle_pct(int_share, label = int_lbl)
# 
# waffles <- waffle1 / waffle2


# w1 <- 
#   abm_waffle_pct_plotly(nonuniv_share, label = nonuniv_lbl) 
# 
# w2 <- 
#   abm_waffle_pct_plotly(int_share, label = int_lbl)
# 
# subplot(w1, w2, nrows = 2)
