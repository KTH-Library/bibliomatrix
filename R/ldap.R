ldap_config <- function() {
  
  user <- Sys.getenv("LDAP_USER")
  pass <- Sys.getenv("LDAP_PASS")
  host <- Sys.getenv("LDAP_HOST")
  base <- Sys.getenv("LDAP_BASE")
  
  if (any(c(user, pass, host, base) == ""))
    stop("please set all of LDAP_USER, LDAP_PASS, LDAP_HOST and LDAP_BASE in your .Renviron")
  
  list(
    ldap_host = sprintf("ldaps://%s", host),
    ldap_base = sprintf("%s", base),
    ldap_user = sprintf("%s@ug.kth.se", user),
    ldap_pass = sprintf("%s", pass)
  )
  
}

ldap_cmd_search <- function(cfg = ldap_config(), ldap_query) {

  if (Sys.info()["sysname"] == "Windows"){
    ldapsearch <- file.path("C:", "OpenLDAP", "bin", "ldapsearch.exe")
    sprintf(
      "%s -H \"%s\" -x -D \"%s\" -w \"%s\" -b \"%s\" \"%s\" -LLL",
      ldapsearch, cfg$ldap_host, cfg$ldap_user, cfg$ldap_pass, cfg$ldap_base, ldap_query)
    
  } else { 
    sprintf(
      "ldapsearch -H \"%s\" -x -D \"%s\" -w \"%s\" -b \"%s\" \"%s\" -LLL",
      cfg$ldap_host, cfg$ldap_user, cfg$ldap_pass, cfg$ldap_base, ldap_query)
  }
  
}

ldap_cmd_whoami <- function(cfg = ldap_config()) {
  
  sprintf(
    "ldapwhoami -H \"%s\" -x -D \"%s\" -w \"%s\"",
    cfg$ldap_host, cfg$ldap_user, cfg$ldap_pass)
  
}

ldap_search <- function(
  ldap_query,
  ldap_config
) 
{
  cmd <- ldap_cmd_search(ldap_config, ldap_query)
  res <- system(cmd, intern = TRUE)
  
  parse_ldif <- function(ldif) {
    re <- "^(.*?):\\s{1}(.*?)$"
    out <- grep(re, ldif, value = TRUE, perl = TRUE)
    key <- stringr::str_match(out, re)[ ,2]
    value <- stringr::str_match(out, re)[ ,3]
    tibble::tibble(key, value)
  }
  
  parse_ldif(res)
  
}

ldap_whoami <- function(ldap_config) 
{
  cmd <- ldap_cmd_whoami(ldap_config)
  system(cmd, intern = TRUE)
}

ad_search_kthid <- function(kthid, cfg) {
  query <- sprintf("(ugKthid=%s)", kthid)
  ldap_search(query, cfg)
}

ad_search_accountname <- function(accountname, cfg) {
  query <- sprintf("(sAMAccountName=%s)", accountname)
  ldap_search(query, cfg)
}

#' Search Active Directory at KTH using ldapsearch
#'
#' This function uses the ldapsearch utility to query the KTH Active Directory
#' 
#' Besides the openldap utils used for LDAP queries it also requires environment variables to be set in .Renviron (LDAP_USER, LDAP_PASS, LDAP_HOST and LDAP_BASE for the service account used for the queries)
#'
#' @param search_term the KTH id or account name to use in the search query
#' @param search_type one of "kthid" or "accountname" for the type of search to make
#' @return tibble with key value pairs
#' @details this function requires that the system library openldap is available ("sudo apt install openldap-utils")
#' @examples ad_search("markussk", "accountname")
#' @export
ad_search <- function(search_term, 
  search_type = c("kthid", "accountname")) 
{
  # load the LDAP service account credentials
  cfg <- ldap_config()
  
  type <- match.arg(search_type)
  switch(type,
   kthid = ad_search_kthid(search_term, cfg),
   accountname = ad_search_accountname(search_term, cfg))
}

#' List member of properties from LDAP search
#' 
#' @param search_results a tibble with search results from the `ad_search()` fcn
#' @return tibble with memberof LDIF values
#' @import dplyr stringr
#' @export
ad_memberof <- function(search_results) {
  search_results %>%
  filter(key == "memberOf") %>%
  mutate(cn = stringr::str_match(value, "^CN=(.*?),(.*?)$")[, 2]) %>%
  select(memberof = cn)
}