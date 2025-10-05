## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(bibliomatrix)
library(dplyr)
library(stringr)
library(readr)
library(knitr)
library(bench)
library(DBI)
library(dbplyr)

if (Sys.info()["sysname"] == "Windows") 
  readRenviron(file.path(Sys.getenv("R_USER"), ".Renviron"))

## ----message=FALSE, fig.align='left'------------------------------------------
# Open a connection to Microsoft SQL Server BIBMON database
bibmon <- con_bib("mssql")

# get data for ABM table 1
kth_data <- abm_data(bibmon, unit_code = "KTH")
t1 <- abm_table1(kth_data)

# display first few results
t1 %>% 
  slice(1:5) %>% 
  select(1:2) %>% 
  kable()

# You can build a sqlite3 databsae for local use (this will take a while the first time)
db_sync()
localdb <- con_bib("sqlite")
kth_data_local <- abm_data(localdb, unit_code = "KTH")
t2 <- abm_table1(kth_data_local)

# are the results the same?
identical(t1, t2)

# what about performance?
bench_time(abm_table1(abm_data(con = bibmon, unit_code = "KTH")))
bench_time(abm_table1(abm_data(con = localdb, unit_code = "KTH")))

dbDisconnect(bibmon)
dbDisconnect(localdb)

