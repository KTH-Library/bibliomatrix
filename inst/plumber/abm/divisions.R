library(plumber)
library(dplyr)
library(purrr)
library(bibliomatrix)
library(htmlwidgets)
library(readr)

cxn <- con_bib("sqlite")

#* @apiTitle Bibliometrics for KTH Divisions
#* @apiDescription Get division level data for organizational units at KTH.
#* @apiTag ABM Functionality related to retrieving data related to the ABM
#* @apiVersion 0.1

#* Table with organizational units at division level for KTH
#* @get /divisions
#* @tag ABM data
function() {
  abm_divisions()
}

#* Dashboard for a specific division
#* @get /visual/dash/<slug>
#* @response 400 Invalid input.
#* @serializer contentType list(type = "text/html")
#* @tag ABM visual
function(slug = "j/jj/jjn") {
  
  
  #on.exit(RSQLite::dbDisconnect(con))
  RSQLite::dbExecute(cxn, "CREATE TABLE IF NOT EXISTS reports (name TEXT, data BLOB)")

  cached <- cxn %>% tbl("reports") %>% filter(name == slug) %>% collect()
  
  if (nrow(cached) >= 1) {
    d <- cached %>% head(1) %>% pull(data)
    return(as.raw(unlist(d)))
  }

  temp <- tempfile()
  on.exit(unlink(temp))
  
  rmarkdown::render(here::here("inst/extdata/abm_staffbased.Rmd"), output_file = I(temp), 
    params = list(unit_code = slug, is_employee = FALSE, embed_data = FALSE, use_package_data = TRUE))  

  b <- blob::as_blob(I(list(read_file_raw(temp))))
  df <- data.frame(name = slug, data = b)
  
  RSQLite::dbWriteTable(cxn, "reports", df, append = TRUE)
  
  as.raw(unlist(b))
}


#* Table with organizational units at division level for KTH
#* @get /visual
#* @tag ABM visual
#* @serializer htmlwidget
function() {
  abm_graph_divisions()
}

#* Table with organizational units at division level for KTH
#* @get /visual/dash
#* @serializer contentType list(type = "text/html")
#* @tag ABM visual
function() {
  
  temp <- tempfile()
  on.exit(unlink(temp))
  
  rmarkdown::render(here::here("data-raw/abm_divisions.Rmd"), output_file = I(temp))
  read_file_raw(temp)
}


#* An endpoint to be used by monitoring services in the KTH IT Operations infrastructure
#* @get /_monitor
#* @serializer contentType list(type = "text/plain;charset=utf-8")
#* @tag ABM service status
function() {
  
  status_ldap <- ifelse(status_ldap()$status == TRUE, "OK", "ERROR")
  status_db <- ifelse(status_db()$status == TRUE, "OK", "ERROR")
  status_renviron <- ifelse(status_renviron()$status == TRUE, "OK", "ERROR")
  status_kthapi <- ifelse(status_kthapi()$status == TRUE, "OK", "ERROR")
  
  status_flag <- ifelse(all(status_ldap == "OK", status_db == "OK", 
                            status_renviron == "OK", status_kthapi == "OK"), "OK", "ERROR")
  
  status_ver <- installed.packages()[ ,"Version"]["bibliomatrix"]
  sprintf(
    "APPLICATION_STATUS: %s\nVERSION: %s\nLDAP: %s\nDATABASE: %s\nRENVIRON: %s\nKTHAPI: %s",
    status_flag, status_ver, status_ldap, status_db, status_renviron, status_kthapi
  ) 
}


