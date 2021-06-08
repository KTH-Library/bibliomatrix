#' Decode JWT string
#' 
#' Decode a Java Web Token string in the format used in Azure (KTH OIDC)
#' @param x the jwt in Azure KTH OIDC format
#' @return list with relevant slots populated
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   abm_decode_jwt(Sys.getenv("SHINYPROXY_OIDC_ACCESS_TOKEN"))
#'  }
#' }
#' @importFrom AzureAuth decode_jwt
#' @export
abm_decode_jwt <- function(x) {
  
  stopifnot(nchar(x) > 0)
  
  slots <- c(
    "kthid", "username", "email", 
    "unique_name", "affiliation", "memberOf"
    )
  
  AzureAuth::decode_jwt(x)$payload[slots]
}

# TODO: fcn to parse $payloady$memberof, extract"pa.anstallda.x.xxxx", last segment