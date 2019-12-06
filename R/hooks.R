#' Running this function launches a Shiny web application presenting
#' the datasets bundled in this package
#' @param example the name of the bundled Shiny application example
#' @export
#' @importFrom shiny runApp
run_app <- function(example) {
  
  PKG <- "bibliomatrix"
  APPS <- "shiny-apps"
  
  validExamples <- list.files(system.file(APPS, package = PKG))
  
  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")
  
  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }
  
  appDir <- system.file(APPS, example, package = PKG)
  if (appDir == "") {
    msg <- paste0("Could not find example directory. Try re-installing `",
                  PKG, "`.")
    stop(msg, call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

#' Running this function launches a Plumber API presenting
#' resources bundled in this package
#' @param teardown logical indicating if the API should be shut down if running
#' @export
run_api <- function(teardown = FALSE) {
  # if (teardown) get0("rs")$kill
  # rs <<- callr::r_bg(function() {
  #   pr <- plumber::plumb(system.file(package = "bibliomatrix", "plumber", "abm", "plumber.R"))
  #   pr$run(port = 8080, swagger = TRUE, host = "127.0.0.1")})
  msg <- sprintf("Please do this manually for now; open a terminal and issue: \ncd %s \nmake up", 
     normalizePath(dirname(system.file(package = "bibliomatrix", "plumber", "abm", "plumber.R"))))
  message(msg)
  if (Sys.info()["sysname"] == "Windows")
    message("On Windows, please use 'RScript.exe run_api.R' instead of 'make up'")
}

#' Prerender dashboards for public data
#' @param refresh logical to indicate whether the cache should be refreshed
#' @export
#' @importFrom purrr map
#' @importFrom rmarkdown render
#' @importFrom rappdirs app_dir
#' @import dplyr
prerender <- function(refresh = FALSE) {
  
  abm_public_kth <- bibliomatrix::abm_public_kth
  dest <- prerender_cache_location()
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  
  orgid <- abm_public_kth$meta$Diva_org_id
  
  f_from_orgid <- function(uc) paste0(uc, ".html")
  uc_from_orgid <- function(orgid) 
    abm_public_kth$meta %>% filter(Diva_org_id == orgid) %>% pull(unit_code)
  f <- file.path(dest, f_from_orgid(abm_public_kth$meta$Diva_org_id))
  f_exists <- f[which(file.exists(f))]
  
  if (refresh & length(f_exists) > 0) {
    message("Deleting cached pre-rendered dashboards: ", f_exists)
    unlink(f_exists)
  }
  
  if (!refresh && identical(f, f_exists)) {
    message("pre-render cache at ", dest, " is up to date.")
    return (invisible(TRUE))
  }
    
  render_with_progress <- function(orgid){
    pb$tick()$print()
    filename <- file.path(dest, f_from_orgid(orgid))
    unit_code <- uc_from_orgid(orgid)
    dash <- system.file("extdata", "abm.Rmd", package = "bibliomatrix")
    f <- rmarkdown::render(dash, output_file = filename, quiet = TRUE,
       params = list(
         unit_code = unit_code, 
         is_employee = FALSE, 
         use_package_data = TRUE, 
         embed_data = FALSE))
  }
  

  pb <- progress_estimated(length(orgid))
  
  res <- map(orgid, render_with_progress)
  
  loc_www <- system.file("shiny-apps", "abm", package = "bibliomatrix")
  message("Updating shiny app cache at ", loc_www)
  file.copy(dest, loc_www, recursive = TRUE, overwrite = TRUE)
  
  return (res)
}

#' Location for prerender cache with dashboard HTML content
#' @export
#' @importFrom rappdirs app_dir
prerender_cache_location <- function() {
  file.path(rappdirs::app_dir("bibmon")$config(), "www")
}