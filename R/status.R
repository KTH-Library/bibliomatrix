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
  
  db <- pool_bib(source_type = "mssql")
  res <- try(get_pubtype_order(db), silent = TRUE)
  pool::poolClose(db)

  if (inherits(res, 'try-error')) {
    return (list (msg = geterrmessage(), status = FALSE))
  }
  
  if (nrow(res) > 0)
    return (list (msg = "OK", status = TRUE))
  
  list(msg = "Unknown issue with db", status = FALSE)
  
}

#' Status message related to KTH API availability
#' @return list with two slots for status message and status 
#' @importFrom kthapi status_kthapi
#' @export
status_kthapi <- function() {
  kthapi::status_kthapi()
}