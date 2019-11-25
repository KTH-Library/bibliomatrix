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
#* @tag ABM reports
#* @response 400 Invalid input.
#* @serializer contentType list(type = "text/html")
function(id, res) {
  is_valid <- id %in% abm_public_kth$meta$Diva_org_id
  
  if (is_valid) {
    cache <- file.path(tempdir(), paste0(id, ".rds"))
    if (file.exists(cache)) {
      readBin(cache, "raw", n = file.info(cache)$size)
    } else {
      uc <- abm_public_kth$meta %>% filter(Diva_org_id == id) %>% pull(unit_code)
      report <- system.file("extdata/abm.Rmd", package = "bibliomatrix")
      f <- rmarkdown::render(report, params = list(unit_code = uc))
      file.copy(f, cache)
      readBin(f, "raw", n = file.info(f)$size)
    }
  } else {
    res$status <- 400
  }
  
}

#* An endpoint to be used by monitoring services in the KTH IT Operations infrastructure
#* @get /_monitor
#* @serializer contentType list(type = "text/plain;charset=utf-8")
#* @tag ABM service status
function() {
  # TODO check the status properly
  status_flag <- status_ldap <- status_db <- "OK"  # can be "ERROR" otherwise
  status_ver <- installed.packages()[ ,"Version"]["bibliomatrix"]
  sprintf(
    "APPLICATION_STATUS: %s\nVERSION: %s\nLDAP: %s\nDATABASE: %s\n",
    status_flag, status_ver, status_ldap, status_db
  )
}


