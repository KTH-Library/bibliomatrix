#' Status message related to R environment file availability
#' @return list with two slots for status message and status 
#' @export
status_renviron <- function() {
  
  r_environ_path <- normalizePath("~/.Renviron", mustWork = FALSE)
  
  if (!file.exists(r_environ_path)) {
    msg <- paste0("No R environ file found at ", r_environ_path, " ERROR")
    status <- FALSE
  } else {
    msg <- paste0("Found R environ at ", r_environ_path)
    status <- TRUE
  }
  
  list(msg = msg, status = status)
}

#' Status message related to db availability
#' @return list with two slots for status message and status 
#' @export
status_db <- function() {
  
  r_environ_path <- normalizePath("~/.Renviron", mustWork = FALSE)
  envvars <- c("DBHOST", "DBNAME", "DBUSER", "DBPASS")
  
  if (any(Sys.getenv(envvars) == "")) {
    msg <- paste("Please use an .Renviron at", r_environ_path, 
      "with all of these envvars set", paste(envvars))
    return (list (msg = msg, status = FALSE))
  }
  
  if (nrow(get_pubtype_order(pool_bib())) > 0)
    return (list (msg = "OK", status = TRUE))
  
  list(msg = "Unknown issue with db", status = FALSE)
  
}

#' Status message related to db availability
#' @return list with two slots for status message and status 
#' @export
status_ldap <- function() {

  r_environ_path <- normalizePath("~/.Renviron", mustWork = FALSE)
  envvars <- c("LDAP_USER", "LDAP_PASS", "LDAP_HOST", "LDAP_BASE")
  
  if (any(Sys.getenv(envvars) == "")) {
    msg <- paste("Please use an .Renviron at", r_environ_path, 
                 "with all of these envvars set", paste(envvars))
    return (list (msg = msg, status = FALSE))
  }
    
  if (nrow(ad_search(Sys.getenv("LDAP_USER"), search_type = "accountname")) > 0)
    return (list (msg = "OK", status = TRUE))

  list(msg = "Unknown issue with LDAP service account", status = FALSE)
  
}
