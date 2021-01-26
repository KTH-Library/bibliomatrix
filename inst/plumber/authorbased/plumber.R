library(plumber)
library(dplyr)
library(purrr)
library(bibliomatrix)
library(htmlwidgets)
library(readr)
library(rmarkdown)
library(blob)

render_report <- function(slug) {
  
  id <- URLdecode(slug)
  temp <- tempfile()
  on.exit(unlink(temp))
  
  rmarkdown::render(
    #here::here("inst/extdata/abm_staffbased.Rmd"), 
    input = system.file(package = "bibliomatrix", "extdata", "abm_staffbased.Rmd"),
    output_file = I(temp), 
    params = list(
      unit_code = id, 
      is_employee = FALSE, 
      embed_data = FALSE, 
      use_package_data = TRUE
    ),
    quiet = TRUE
  )  
  
  b <- blob::as_blob(I(list(read_file_raw(temp))))
  
  data.frame(name = id, data = b)
}

cache_reports <- function() {
  
  slugs <- abm_divisions()$id
  
  pb <- progress::progress_bar$new(total = length(slugs))
  
  render_report_pb <- function(x) {
    pb$tick()
    render_report(x)
    Sys.sleep(0.01)
  }
  
  reports <- map_df(slugs, render_report_pb)
  
  message("Updating cache...")
  con <- con_bib("sqlite")
  on.exit(RSQLite::dbDisconnect(con))
  
  cache_clear()
  RSQLite::dbWriteTable(con, "reports", reports)
  message("Done")
  
}

cache_report <- function(con = con_bib("sqlite"), slug) {
  
  id <- utils::URLdecode(slug)
  
  RSQLite::dbExecute(con, "CREATE TABLE IF NOT EXISTS reports (name TEXT, data BLOB)")
  
  cached <- con %>% tbl("reports") %>% filter(name == id) %>% collect()
  
  if (nrow(cached) >= 1) {
    d <- cached %>% head(1) %>% pull(data)
    return(as.raw(unlist(d)))
  }
  
  df <- render_report(id)
  
  RSQLite::dbWriteTable(con, "reports", df, append = TRUE)
  
  as.raw(unlist(df$data))  
}

cache_clear <- function(con = con_bib("sqlite")) {
  RSQLite::dbRemoveTable(con, "reports")  
}


cxn <- con_bib("sqlite")
#on.exit(RSQLite::dbDisconnect(cxn))

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

#* Flexdashboard embedding network graph with organizational units at division level for KTH
#* @get /ui/divisions
#* @serializer contentType list(type = "text/html")
#* @tag ABM visual
function() {
  
  temp <- tempfile()
  on.exit(unlink(temp))
  
  rmarkdown::render(
    system.file(package = "bibliomatrix", "extdata", "abm_divisions.Rmd"),
    output_file = I(temp)
  )
  
  read_file_raw(temp)
}

#* HTML snippet with interactive graph for organizational units at division level for KTH
#* @get /ui/html/divisions
#* @tag ABM visual
#* @serializer htmlwidget
function() {
  le <- function(x) URLencode(x, reserved = TRUE)
  abm_graph_divisions(base_url = "/ui/division/", link_encoder = le)
}

#* Flexdashboard with author-based ABM report for a specific division
#* @get /ui/division/<slug>
#* @response 400 Invalid input.
#* @serializer contentType list(type = "text/html")
#* @tag ABM visual
render_division <- function(slug = "j/jj/jjn") {
  
  cache_report(slug = slug)
  #id <- URLdecode(slug)

  # RSQLite::dbExecute(cxn, "CREATE TABLE IF NOT EXISTS reports (name TEXT, data BLOB)")
  # 
  # cached <- cxn %>% tbl("reports") %>% filter(name == id) %>% collect()
  # 
  # if (nrow(cached) >= 1) {
  #   d <- cached %>% head(1) %>% pull(data)
  #   return(as.raw(unlist(d)))
  # }
  # 
  # temp <- tempfile()
  # on.exit(unlink(temp))
  # 
  # rmarkdown::render(
  #   input = system.file(package = "bibliomatrix", "extdata", "abm_staffbased.Rmd"), 
  #   output_file = I(temp), 
  #   params = list(
  #     unit_code = id, 
  #     is_employee = FALSE, 
  #     embed_data = FALSE, 
  #     use_package_data = TRUE)
  #   )  
  # 
  # b <- blob::as_blob(I(list(read_file_raw(temp))))
  # df <- data.frame(name = id, data = b)
  # 
  # RSQLite::dbWriteTable(cxn, "reports", df, append = TRUE)
  # 
  # as.raw(unlist(b))
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


