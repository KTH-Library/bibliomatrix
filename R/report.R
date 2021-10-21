#' @importFrom blob as_blob
render_report <- function(rmd, myparams) {
  
  temp <- tempfile()
  on.exit(unlink(temp))
  
  if (missing(rmd))
    rmd <- system.file(package = "bibliomatrix", 
      "extdata", "abm.Rmd")
  
  # Workaround for shiny-server usage due to permission denied when rendering reports
  # see https://github.com/hadley/mastering-shiny/blob/master/rmarkdown-report/app.R#L3-L6  
  # intermediate files may get permission denied for shiny user, indicated by:
  # Error in file(con, "w") : cannot open the connection
  # In file(con, "w") : cannot open file 'abm.knit.md': Permission denied
  # NB: also if error "Could not fetch https://KTH-Library.github.io/abm/About_ABM.html" appears...
  # this may be due to VPN being active
  
  copy_files <- file.path(dirname(rmd), list.files(dirname(rmd)))
  report_dir <- tempdir(check = TRUE)
  file.copy(copy_files, report_dir, recursive = TRUE, overwrite = TRUE)
  report_path <- file.path(report_dir, basename(rmd))
  
  rmarkdown::render(
    input = report_path,
    output_file = I(temp), 
    params = myparams,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )  
  
  b <- blob::as_blob(I(list(readr::read_file_raw(temp))))
  
  is_visible <- FALSE
  if (isTRUE(myparams$embed_data))
    is_visible <- TRUE
  
  tibble::tibble(
    name = myparams$unit_code, data = b, 
    visibility = is_visible, ts = Sys.time(), report = rmd)
}


report_params <- function(ids = abm_public_kth$meta$unit_code,
                          is_employee = FALSE,
                          embed_data = TRUE,
                          use_package_data = TRUE) {

  tibble(
    unit_code = ids,
    is_employee,
    embed_data,
    use_package_data
  ) %>% 
  purrr::transpose()
  
}

render_reports <- function(myparamz = report_params()) {
  
  pb <- progress::progress_bar$new(
    total = length(myparamz), 
    format = " rendering :what [:bar] :percent eta: :eta")
  
  render_report_pb <- function(x) {
    
    pb$tick(tokens = list(what = x$unit_code))

    rr <- purrr::possibly(function(z) render_report(myparams = z), 
                          otherwise = FALSE, quiet = FALSE)
    res <- rr(x)
    
    if (isFALSE(res)) {
      message("Failed rendering for ", x)
      res <- NULL
    }
    
    Sys.sleep(0.01)
    return(res)
  }
  
  myparamz %>%
    purrr::map_df(function(y) render_report_pb(x = y))
  
}

cache_reports <- function() {
  
  message("Rendering reports for private app")
  priv_reports <- render_reports()
  
  message("Rendering reports for public app")
  pub_reports <- render_reports(report_params(embed_data = FALSE))
  
  message("Combining reports before writing to cache...")
  reports <- bind_rows(priv_reports, pub_reports)
  
  message("Updating cache...")
  con <- con_cache()
  on.exit(RSQLite::dbDisconnect(con))

  message("Clearing any existing cached data")
  cache_clear()
  
  message("Writing new data to cache")
  RSQLite::dbWriteTable(con, "reports", reports)
  message("Done")

}

con_cache <- function(dbpath, verbose = FALSE) {
  
  if (missing(dbpath))
    dbpath <- file.path(rappdirs::app_dir("bibmon")$config(), "reports.db")
  
  if (verbose)
    message("Location for reports cache/db is: ", normalizePath(dbpath))
  
  if (!file.exists(dbpath)) {
    con <- con_bib_sqlite(create = TRUE, db_path = dbpath)
  }
  
  con_bib_sqlite(db_path = dbpath)
  
}

cache_report <- function(con, id, params) {
  
  if (missing(con)) {
    con <- con_cache(verbose = TRUE)
    on.exit(RSQLite::dbDisconnect(con))
  }
  
  if (!RSQLite::dbExistsTable(con, "reports")) {
    
    reports_ddl <- tibble::tibble(
      name = character(0), data = blob::as_blob(character(0)), #data = blob::as_blob(I(list(raw(0)))),
      visibility = character(0), ts = as.Date.POSIXct(integer(0)), report = character(0))
    
    RSQLite::dbWriteTable(con, "reports", reports_ddl)
  }
  
  #RSQLite::dbExecute(con, 
  #  "CREATE TABLE IF NOT EXISTS reports (name TEXT, data BLOB, 
  # visibility TEXT, ts TEXT, report TEXT)")
  
  cached <- con %>% tbl("reports") %>% filter(.data$name == id) %>% collect()
  
  if (nrow(cached) >= 1) {
    d <- cached %>% head(1) %>% pull(.data$data)
    return(as.raw(unlist(d)))
  }
  

  df <- render_report(myparams = params)
  
  if (!RSQLite::dbExistsTable(con, "reports")) {
    RSQLite::dbWriteTable(con, "reports", df)
  } else {
    RSQLite::dbWriteTable(con, "reports", df, append = TRUE)
  }
  
  as.raw(unlist(df$data))  
}

cache_clear <- function(con) {
  if (missing(con)) {
    con <- con_cache()
    on.exit(RSQLite::dbDisconnect(con))
  }
  if (RSQLite::dbExistsTable(con, "reports"))
    RSQLite::dbRemoveTable(con, "reports")  
}

cache_clear_report <- function(con, id) {
  
  if (missing(con)) {
    con <- con_cache()
    on.exit(RSQLite::dbDisconnect(con))
  }
  
  cached <- con %>% tbl("reports") %>% filter(.data$name == id) %>% collect()
  
  res <- 0
  
  if (nrow(cached) >= 1) {
    sql <- sprintf("delete from reports where name = '%s'", id)
    res <- DBI::dbExecute(con, sql)
  }
  
  return(list(rows_affected = res))
  
}

view_reports <- function(con) {
  
  if (missing(con)) {
    con <- con_cache()
    on.exit(RSQLite::dbDisconnect(con))
  }
  
  con %>% 
    tbl("reports") %>% 
    collect() %>%
    mutate(ts = as.POSIXct(.data$ts, origin = "1970-01-01")) 
  
}

#' @importFrom utils browseURL
view_report <- function(blob) {
  tf <- tempfile(fileext = "html")
  readr::write_file(unlist(blob), tf)
  message("Please delete ", tf, " when done:")
  message(sprintf("unlink('%s')", tf))
  if (interactive()) utils::browseURL(tf)
}

#view_reports() %>% filter(name == "A") %>% pull(data) %>% view_report()

#' A report used in ABM
#' 
#' @param id the identifier for the report, either unit_code or kthid
#' @param is_private logical indicating whether the report is publicly visible or not
#' @return a raw object with a BLOB containing a HTML report rendered using Rmarkdown.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  r1 <- abm_report(id = "177", is_private = FALSE)
#'  view_report(list(r1))
#'  r2 <- abm_report("u1o2ujjd", is_private = TRUE)
#'  }
#' }
#' @seealso 
#'  \code{\link[kthapi]{kth_profile}}
#' @export 
#' @importFrom kthapi kth_profile
#' @importFrom rlang .data
abm_report <- function(id, is_private) {

  apk <- abm_public_kth
  is_valid_org <- id %in% as.character(apk$meta$Diva_org_id)
  
  if (is_valid_org) {
    
    uc <- abm_public_kth$meta %>% filter(id == as.character(Diva_org_id)) %>% pull(unit_code)
    
    report <- tryCatch(
      view_reports() %>% 
      filter(.data$name == uc, .data$visibility == as.integer(is_private)) %>%
      arrange(desc(.data$ts)) %>%
      slice(1) %>%
      collect %>%
      pull(.data$data) %>%
      unlist(),
      error = function(e) FALSE)
    
    if (!is.null(report) && report != FALSE)
      return (report)
    
    myparams <- 
      report_params(ids = uc, is_employee = FALSE, use_package_data = TRUE, embed_data = is_private)[[1]]
    
    cache_report(id = id, params = myparams)
    
  } else {
    
    is_valid_kthid <- tryCatch(
      !is.null(kthapi::kth_profile(kthid = id)), 
      error = function(e) FALSE)
    
    if (!is_valid_kthid) {
      message("The id ", id, " doesn't seem to be a valid kthid.")
      return (NULL)
    }
    
    myparams <- 
      report_params(ids = id, is_employee = TRUE, use_package_data = FALSE)[[1]]
    
    cache_report(id = id, params = myparams)
  }
    
}

#cache_reports()
# view_reports()