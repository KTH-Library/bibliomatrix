library(shiny)
library(pool)

pool <- pool_bib("mssql")
onStop(function() poolClose(pool))

ui <- fluidPage(
  tableOutput("t1"),
  tableOutput("t2"),
  tableOutput("t3"),
  tableOutput("t4"),
  tableOutput("t5")
)

server <- function(input, output, session) {
  output$t1 <- renderTable(abm_table1(con = pool, unit_code = "I"))
  output$t2 <- renderTable(abm_table2(con = pool, unit_code = "I"))
  output$t3 <- renderTable(abm_table3(con = pool, unit_code = "I"))
  output$t4 <- renderTable(abm_table4(con = pool, unit_code = "I"))
  output$t5 <- renderTable(abm_table5(con = pool, unit_code = "I"))
}

shinyApp(ui, server)
