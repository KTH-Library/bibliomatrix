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

#' Search Active Directory at KTH using kthid or username
#'
#' This function uses curl to query the KTH Active Directory. It requires 
#' environment variables to be set in .Renviron, specifically
#' LDAP_USER, LDAP_PASS, LDAP_HOST and LDAP_BASE for the service 
#' account used for the queries.
#' 
#' @details
#' 
#' Under the hood, this function issues a curl request similar to this one:
#' curl --ntlm -user $CREDS \\
#' ldaps://ldap.foo.com/DC=ads,DC=foo,DC=com?memberOf?sub?(&(sAMAccountName=$USER)(memberOf=CN=$GROUP,OU=Distribution,OU=Groups,DC=ads,DC=foo,DC=com))"
#'
#' @param search_term the KTH id or account name to use in the search query
#' @param search_type one of "kthid" or "accountname" for the type of search to make
#' @return tibble with key value pairs
#' @examples ad_search("markussk", "accountname")
#' @export
ad_search <- function(search_term, 
  search_type = c("kthid", "accountname")) 
{
  # load the LDAP service account credentials
  cfg <- ldap_config()
  type <- match.arg(search_type)
  ldap_search_curl(search_term, type, cfg)
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

ldap_cmd_curl <- function(ldap_filter, ldap_host, ldap_base,
  ldap_attributes = "", ldap_scope = "sub") {
  # return url pattern like ...
  # ldap[s]://hostname:port/base_dn?attributes?scope?filter
  sprintf("%s/%s?%s?%s?%s", ldap_host, ldap_base, ldap_attributes, ldap_scope, ldap_filter)
}

#' @import curl
#' @noRd
ldap_search_curl <- function(term,
  ldap_search_type = c("accountname", "kthid"),
  cfg = ldap_config()) {
  
  # determine LDAP filter to use based on search type
  type <- match.arg(ldap_search_type)
  filter <- switch(type,
    accountname = sprintf("(sAMAccountName=%s)", term),
    kthid = sprintf("(ugKthid=%s)", term),
    stop("ldap search type needs to be one of kthid or accountname")
  )
  
  # make curl request
  query <- ldap_cmd_curl(filter, cfg$ldap_host, cfg$ldap_base)
  userpwd <- paste0(cfg$ldap_user, ":", cfg$ldap_pass)
  handle <- curl::new_handle()
  curl::handle_setopt(handle, userpwd = userpwd)
  res <- curl_fetch_memory(query, handle)
  raw <- readBin(res$content, "raw", length(res$content))
  mytext <- iconv(readBin(raw, character()), from = "UTF-8", to = "UTF-8")

  # parse the response  
  ldif <- 
    mytext %>%
    str_replace_all("\n{2}", "\n") %>% 
    str_replace_all("\t", "") %>% 
    trimws() %>% 
    strsplit(fixed = TRUE, split = "\n") %>%
    unlist()
  
  parse_ldif <- function(ldif) {
    re <- "^(.*?):\\s{1}(.*?)$"
    out <- grep(re, ldif, value = TRUE, perl = TRUE)
    key <- stringr::str_match(out, re)[ ,2]
    value <- stringr::str_match(out, re)[ ,3]
    tibble::tibble(key, value)
  }
  
  # return a tibble with results
  parse_ldif(ldif)
}


