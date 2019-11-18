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

if (Sys.info()["sysname"] == "Windows") 
  readRenviron(file.path(Sys.getenv("R_USER"), ".Renviron"))

## ---- message=FALSE, fig.align='left'------------------------------------

# get data from the default data source (Microsoft SQL Server BIBMON database)
t1 <- abm_tab1()

# display first few results
t1 %>% 
slice(1:5) %>% 
select(1:2, "Total") %>% 
arrange(desc(Total)) %>% 
kable()

# get data but explicitly specify the data source 
src1 <- abm_tab1(con = con_bib("mssql"))

db_sync()  ## sync a local sqlite3db with the remote source
src2 <- abm_tab1(con = con_bib("sqlite"))

# are the results the same?
identical(src1, src2)

# what about performance?
bench_time(abm_tab1(con = con_bib("mssql")))
bench_time(abm_tab1(con = con_bib("sqlite")))


## ---- fig.align='left'---------------------------------------------------
# search Active Directory using an account name, return first 10 rows
ad_search("markussk", "accountname") %>% head(10) %>% kable()

# search AD using a KTH identifier, same result, so return first row only
ad_search("u1o2ujjd", "kthid") %>% head(1) %>% kable()

# default if search_type is not specified is to use KTH id
#ad_search("u1o2ujjd")

# display memberships for an Active Directory user with a specific KTH id
ad_search("u1o2ujjd") %>% 
  filter(str_starts(key, "ug.*Affiliation")) %>% 
  kable() 

# function to return account name for a given KTH id
ad_accountname_from_kthid <- function(x)
  ad_search(x) %>% filter(key == "sAMAccountName") %>% pull(value)

# use the above fcn to get the account name for a given kth id
ad_accountname_from_kthid("u1o2ujjd")

# several kth identifiers can be processed, for example:
kthids <- 
"u1kzf1xh
u1g9umtq
u1jr9fll 
u1ygqmuy
u13bp6vd
u18qe64m" %>% 
read_lines()

# display memberof entries beginning with "ug" for a given identifier
ad_search(kthids[4]) %>% 
ad_memberof() %>% 
filter(str_starts(memberof, "aff")) %>%
kable()


