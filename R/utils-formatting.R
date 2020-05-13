#' String formatting utility to avoid wrapping of hyphens inside compound words
#' while retaining search in HTML content using Ctrl-F.
#' 
#' For example, if "co-publications" should be possible to find on a HTML page
#' using Ctrl-F, a non-breaking hyphen (longer dash) HTML entity might not be ideal.
#' 
#' Wrapping the string "co-publication" in a span with the CSS style 'white-space:nowrap;'
#' might be more convenient for the end user.
#' @param x the string to modify
#' @importFrom stringr str_replace str_c
#' @importFrom purrr map_chr
#' @examples 
#' nowrap_html("Swedish non-university co-publications")
#' @export
nowrap_html <- function(x) 
  str_split(x) %>%
  map_chr(function(y) stringr::str_replace(y, "(.*\\s)?(.+-.+)(\\s(.*?))*$", "\\1<span style='white-space: nowrap;'>\\2</span>\\3")) %>%
  str_c(collapse = " ")

#' String formatting utility to substitute soft hyphens inside compound words
#' with non-breaking hard "long" hyphens using HTML character entity encoding
#' 
#' For example, "co-publications" becomes "co&#8209;publications"
#' 
#' @param x the string to modify
#' @importFrom stringr str_replace_all str_c
#' @importFrom purrr map_chr
#' @examples 
#' nowrap_hyphen_sub("Swedish non-university co-publications")
#' @export
nowrap_hyphen_sub <- function(x)
  str_split(x) %>%
  map_chr(function(y) str_replace_all(y, "-", "\U2011")) %>%
  str_c(collapse = " ")


