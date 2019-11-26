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
}
