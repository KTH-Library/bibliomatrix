#' KTH user account identifier (kthid) from account name
#' @param accountname the LDAP accountname
#' @export
#' @importFrom kthapi kth_profile
kthid_from_accountname <- function(accountname) {
  kth_profile(username = accountname)$content$kthId  
}

#' KTH displayname from user account identifier
#' @param kthid the employee identifier
#' @export
#' @importFrom kthapi kth_displayname
displayname_from_kthid <- function(kthid) {
  kth_displayname(kthid, "kthid")
}
