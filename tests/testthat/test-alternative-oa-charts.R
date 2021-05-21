test_that("alternative oa charts work", {

  skip_on_ci()

  c1 <- con_bib_sqlite()
  on.exit(DBI::dbDisconnect(c1))

  c2 <- con_bib_mssql()
  on.exit(DBI::dbDisconnect(c2))
  
  d1 <- abm_data(con = c1, unit_code = "KTH")
  d2 <-  abm_data(con = c2, unit_code = "KTH")
  
  df1 <- abm_table6(d1)
  df2 <- abm_table6(d2)
  
  p1 <- abm_graph_oadata_linegraphs(df1, "plotly")
  p2 <- abm_graph_oadata_linegraphs(df2, "plotly")
  
  # TODO: use vdiffr to test against existing cases of plots
  #vdiffr::manage_cases()
  #vdiffr::expect_doppelganger("oa-linecharts", p1)
  #vdiffr::expect_doppelganger("oa-linecharts", p2)
  
  abm_graph_oadata_pie(df1)
  abm_graph_oadata_piechart(df1, type = "ggplot")
  abm_graph_oadata_share(df1, type = "ggiraph")
  abm_graph_oadata_share(df1, type = "ggplot")
  abm_graph_oadata_share(df1, type = "plotly")
  abm_graph_oadata_stackedarea(df1)
  abm_graph_oadata_linegraphs(df1, "ggiraph")
  abm_graph_oadata_linegraphs(df1, "ggplot")
  abm_graph_oadata_linegraphs(df1, "plotly")
  
  expect_true(identical(df1, df2))
  
})
