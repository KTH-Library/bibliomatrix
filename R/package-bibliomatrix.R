#' bibliomatrix
#'
#' The goal of bibliomatrix is to provide a common set of functionality for 
#' the Annual Bibliometric Monitoring efforts from the Bibliometrics team at 
#' the KTH Library.
#'
#' @name bibliomatrix
#' @docType package
#' @keywords package
#' @aliases package-bibliomatrix
#' 
# needed for use of . in magrittr pipelines
utils::globalVariables(c("."))
NULL

#' Public data with Annual Bibliometric Monitoring for KTH assembed in 2019
#'
#' A nested named list with two slots - a) "meta" with a tibble 
#' containing organizational unit information from KTH, including unit codes 
#' and b) "units" - a named list with a set of five tibbles per units 
#'
#' The data comes from a function call to \code{\link{abm_public_data}}
#' which requires database credentials being present in the .Renviron file. 
#' For information regarding how to access the data for various units at KTH,
#' please look at the examples for the \code{\link{abm_public_data}} function.
"abm_public_kth"