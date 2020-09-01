# Initial tests to get an author/staff-based view of publications connected to a unit at KTH, based on staff info through the KTH AD-API
# So: KTH unit --> current researchers from API --> kthids --> publications (currently from "Masterfile")
#
# Example of use currently:
#   unit_members<- unit_staff(unit_slug="j/jh/jhs")   # note that this is calling both the kth-catalog and also retrieving 
#                                                     # kthids for the members of the previous call. Could be more efficient.
#   unit_publications<- abm_staff_data(kthids=unit_members$kthid)
#
# This set of publications could then be used as input for an author-based analysis of units
#
# ToDo:
#   * Calculating author fractions, by author count from org divided by total number of authors(?)
#   * placing the currently selected unit name as 'Unit_code' and 'Unit_name'. Now this is only the de-duplicated values of individual authors. What to add (kthid, Diva-code etc)?


#' Get members of unit based on slug
#' 
#' @param unit_slug string representing KTH unit
#' @return tibble of unit numbers, including kthid
#' @export

unit_staff <- function(unit_slug = NULL){
  org_users<- kth_catalog(slug = unit_slug)$users
  org_users$kthid<- ""
  nr_users<- dim(org_users)[1]  
  
  for(i in 1:nr_users){
    id<- kth_profile(username = org_users$username[i])$content$kthId
    org_users$kthid[i]<- id
  }
  org_users
}

#' Get members of unit based on slug
#' 
#' @param con A database connection
#' @param kthids a list of KTH-ids to retrieve publications for
#' @return tibble with all staff-based ABM data for selected organizational unit
#' @export
abm_staff_data <- function(con = con_bib(), kthids) {
    res <- con %>%
      tbl("masterfile") %>%
      filter(Unit_code %in% kthids)  %>% 
      collect() #%>% 
    
    auth_count<- res %>% group_by(PID) %>% tally() 
    
    res<- res %>% inner_join(auth_count, by="PID") %>% distinct(PID, WebofScience_ID, .keep_all = TRUE)
    return(res)
    
}

#' Dummy to test git
#' 
#' Future function