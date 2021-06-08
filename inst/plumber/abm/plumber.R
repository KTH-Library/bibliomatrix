library(plumber)
library(dplyr)
library(purrr)
library(bibliomatrix)

public <- abm_public_kth

#* @apiTitle Annual Bibliometric Monitoring data from KTH
#* @apiDescription Get data from the ABM project through an API.
#* @apiTag ABM Functionality related to retrieving data related to the ABM
#* @apiVersion 0.0.0.900

#* Table with organizational units at KTH
#* @get /units
#* @tag ABM tables
function() {
  abm_public_kth$meta
}


#* All tables with ABM data for a specific organizational unit
#* @response 400 Invalid input.
#* @get /unit/<id:int>/tables
#* @tag ABM tables
function(id, res) {
  
  is_valid <- id %in% abm_public_kth$meta$Diva_org_id
  
  if (is_valid) {
    uc <- abm_public_kth$meta %>% filter(Diva_org_id == id) %>% pull(unit_code)
    public %>% pluck("units", uc)
  } else {
    res$status <- 400
  }
  
}

#* A specific table (1:5) with ABM data for a specific organizational unit
#* @response 400 Invalid input.
#* @get /unit/<id:int>/table/<tab:int>
#* @tag ABM tables
function(id, tab, res) {
  
  is_ok_tab <- tab %in% 1:5
  is_ok_id <- id %in% abm_public_kth$meta$Diva_org_id
  is_valid <- all(is_ok_tab, is_ok_id)
  
  if (is_valid) {
    uc <- abm_public_kth$meta %>% filter(Diva_org_id == id) %>% pull(unit_code)
    public %>% pluck("units", uc) %>% pluck(tab)
  } else {
    res$status <- 400
  }
  
}


#* A flexdashboard reporting on Bibliometric indicators for a unit at KTH
#* @get /unit/<id:int>/flexdashboard
#* @param embeddata:logical flag to indicate if publication data should be embedded
#* @tag ABM reports
#* @response 400 Invalid input.
#* @serializer contentType list(type = "text/html")
function(id, res, embeddata = "false") {
  is_valid <- id %in% abm_public_kth$meta$Diva_org_id
  embed <- ifelse(embeddata == "true", TRUE, FALSE)
  #cat("embed is: ", embed, "\n")
  if (is_valid) {
    cache <- file.path(tempdir(), 
       paste0(id, ifelse(embed, "-embed-", ""), ".rds"))
    if (file.exists(cache)) {
      cat("Serving from cache: ", cache, "\n")
      readBin(cache, "raw", n = file.info(cache)$size)
    } else {
      uc <- abm_public_kth$meta %>% filter(Diva_org_id == id) %>% pull(unit_code)
      report <- system.file("extdata", "abm.Rmd", package = "bibliomatrix")
      f <- rmarkdown::render(report, quiet = TRUE, params = list(
        unit_code = uc, embed_data = embed, is_employee = FALSE, use_package_data = TRUE))
      file.copy(f, cache)
      readBin(f, "raw", n = file.info(f)$size)
    }
  } else {
    res$status <- 400
  }
  
}

#* A flexdashboard reporting on Bibliometric indicators for an employee at KTH
#* @get /employee/<id>/flexdashboard
#* @tag ABM reports
#* @response 400 Invalid input.
#* @serializer contentType list(type = "text/html")
function(id, res) {
  cache <- file.path(tempdir(), paste0(id, ".rds"))
  if (file.exists(cache)) {
    readBin(cache, "raw", n = file.info(cache)$size)
  } else {
    report <- system.file("extdata", "abm.Rmd", package = "bibliomatrix")
    f <- rmarkdown::render(report, params = list(
      unit_code = id, is_employee = TRUE, embed_data = TRUE, use_package_data = FALSE))
    file.copy(f, cache)
    readBin(f, "raw", n = file.info(f)$size)
  }
}

#* An endpoint to be used by monitoring services in the KTH IT Operations infrastructure
#* @get /_monitor
#* @serializer contentType list(type = "text/plain;charset=utf-8")
#* @tag ABM service status
function() {

  status_db <- ifelse(status_db()$status == TRUE, "OK", "ERROR")
  status_renviron <- ifelse(status_renviron()$status == TRUE, "OK", "ERROR")
  status_kthapi <- ifelse(status_kthapi()$status == TRUE, "OK", "ERROR")
  
  status_flag <- 
    ifelse(
      all(status_db == "OK", status_renviron == "OK", status_kthapi == "OK"), 
        "OK", "ERROR")
  
  status_ver <- installed.packages()[ ,"Version"]["bibliomatrix"]
  sprintf(
    "APPLICATION_STATUS: %s\nVERSION: %s\nDATABASE: %s\nRENVIRON: %s\nKTHAPI: %s",
    status_flag, status_ver, status_db, status_renviron, status_kthapi
  ) 
}


