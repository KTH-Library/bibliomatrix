#' Generate Web of Science disclaimer
#'
#' @param year the year to use for copyright note, current year if not given
#' @return a string with WoS disclaimer for given year
#' @import glue
#' @export
wos_disclaimer <- function(year){
  if(missing(year))
    year <- format(Sys.Date(), "%Y")
  glue("Certain data included herein is derived from the Science Citation Index Expended (SCIE), Social Sciences Citation Index (SSCI), ",
       "Arts & Humanities Citation Index (AHCI), Conference Proceedings Citation Index - Sciences (CPCI-S) and ",
       "Conference Proceedings Citation Index - Social Sciences & Humanities (CPCI -SSH), prepared by Clarivate Analytics, Philadelphia, Pennsylvania, USA: ",
       "© Copyright Clarivate Analytics. {year}. All rights reserved. ")
}
