skip_chart_tests <- TRUE

test_that("alternative oa charts work", {

  skip_if(skip_chart_tests, "by default skipping since Travis cannot sync db")  
  df1 <- abm_table6(con = con_bib_sqlite(), unit_code = "KTH")
  df2 <- abm_table6(con = con_bib_mssql(), unit_code = "KTH")
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
  
  expect_true(identical(df1, df2))
  
})
