## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE------------------------------------------------
library(bibliomatrix)
library(dplyr)
library(stringr)
library(readr)
library(knitr)
library(bench)
library(DBI)
library(pool)

if (Sys.info()["sysname"] == "Windows") 
  readRenviron(file.path(Sys.getenv("R_USER"), ".Renviron"))

## ---- message=FALSE, fig.align='left'------------------------------------
# Open a connection pool to the default data source (Microsoft SQL Server BIBMON database)
bibmon <- pool_bib()

# get data for ABM table 1
t1 <- abm_table1(con = bibmon, unit_code = "KTH")

# display first few results
t1 %>% 
  slice(1:5) %>% 
  select(1:2) %>% 
  kable()

# You can build a sqlite3 databsae for local use (this will take a while the first time)
db_sync()
localdb <- pool_bib("sqlite")
t2 <- abm_table1(con = localdb, unit_code = "KTH")

# are the results the same?
identical(t1, t2)

# what about performance?
bench_time(abm_table1(con = bibmon, unit_code = "KTH"))
bench_time(abm_table1(con = localdb, unit_code = "KTH"))

poolClose(bibmon)
poolClose(localdb)

## ---- fig.align='left'---------------------------------------------------
# search Active Directory using an account name, return first 10 rows
ad_search("agnel", "accountname") %>% head(10) %>% kable()

# search AD using a KTH identifier, same result, so return first row only
ad_search("u1z88syr", "kthid") %>% head(1) %>% kable()

# default if search_type is not specified is to use KTH id
#ad_search("u1z88syr")

# display memberships for an Active Directory user with a specific KTH id
ad_search("u1z88syr") %>% 
  filter(str_starts(key, "ug.*Affiliation")) %>% 
  kable() 

# some functions have been defined to "translate" between identificators
ad_accountname("u1z88syr")
ad_kthid("agnel")

# several kth identifiers can be processed, for example:
kthids <-
"u1kzf1xh
u1g9umtq
u1jr9fll 
u1ygqmuy
u13bp6vd
u18qe64m" %>% read_lines()

# display memberof entries beginning with "aff" for a given user
ad_search(kthids[4]) %>% 
  ad_memberof() %>% 
  filter(str_starts(memberof, "aff")) %>%
  kable()

