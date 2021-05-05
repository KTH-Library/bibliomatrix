#' KTH user account identifier (kthid) from account name
#' @param accountname the LDAP accountname
#' @export
#' @importFrom kthapi kth_profile
ad_kthid2 <- function(accountname) {
  kth_profile(username = accountname)$content$kthId  
}

#' KTH displayname from user account identifier
#' @param kthid the employee identifier
#' @export
#' @importFrom kthapi kth_displayname
ad_displayname2 <- function(kthid) {
  kth_displayname(kthid, "kthid")
}
