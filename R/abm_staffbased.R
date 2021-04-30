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
#' @param unit_slug string representing KTH unit, Default: NULL
#' @importFrom kthapi kth_catalog
#' @return tibble of unit numbers, including kthid
#' @examples 
#' \dontrun{
#' if (interactive()) {
#'  unit_staff(unit_slug = "j/jh/jhs")
#'  unit_staff(unit_slug = "j/jj/jjn")
#'  }
#' }
#' @rdname unit_staff
#' @export 

unit_staff <- function(unit_slug = NULL) {
  
  org_users <- kth_catalog(slug = unit_slug)$users
  org_users$kthid <- ""
  nr_users <- dim(org_users)[1]
  
  for (i in 1:nr_users) {
    id <- suppressMessages(kth_profile(username = org_users$username[i])$content$kthId)
    org_users$kthid[i] <- id
  }
  
  org_users
}

#' Get publications from members of unit via kthids
#' 
#' @param con A database connection
#' @param kthids a list of KTH-ids to retrieve publications for
#' @param analysis_start first publication year of analysis
#' @param analysis_stop last publication year of analysis
#' @return tibble with all staff-based ABM data for selected organizational unit
#' @export
abm_staff_data <- function(con, kthids,
                           analysis_start = abm_config()$start_year, 
                           analysis_stop = abm_config()$stop_year) {

  res <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code %in% kthids &
             level == 3 &
             is_kth == 1 &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    rename(Unit_Fraction_raw = Unit_Fraction, Unit_Fraction_adj_raw = Unit_Fraction_adj) %>%
    collect()
  
  auth_count<- res %>% group_by(PID) %>% tally() 
  colnames(auth_count)<- c("PID", "auth_count")
  
  unit_frac<- res %>% group_by(PID) %>% 
    summarise(Unit_Fraction = sum(Unit_Fraction_raw, na.rm = TRUE), Unit_Fraction_adj = sum(Unit_Fraction_adj_raw, na.rm = TRUE))
  
  res %>% 
    inner_join(auth_count, by="PID") %>% 
    inner_join(unit_frac, by="PID") %>% 
    arrange(PID) %>% # to make sure that Diva_org = 177 is preferred over blanks in deduplication below
    distinct(PID, WebofScience_ID, .keep_all = TRUE) %>%
    select(-Unit_code, -Unit_Name, -analysis_id)  # removing redundant fields, that can be misleading after deduplication
}

#' Get staff list from slug
#' 
#' @param con A database connection
#' @param unit_slug a string representing the KTH unit
#' @param analysis_start first publication year of analysis
#' @param analysis_stop last publication year of analysis
#' @return tibble with staff list for selected organizational unit
#' @export
abm_staff_list <- function(con, unit_slug,
                           analysis_start = abm_config()$start_year, 
                           analysis_stop = abm_config()$stop_year) {
  
  researchers <-
    con %>%
    tbl("researchers") %>% 
    collect() %>%
    filter(stringr::str_starts(slug, unit_slug)) %>%
    distinct() %>%
    select(kthid, firstName, lastName, Title = `title.en`) %>%
    collect()
  
  kthids <- researchers$kthid
  pubs <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code %in% kthids &
             level == 3 &
             is_kth == 1 &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    group_by(Unit_code) %>%
    summarise(pubs = n(), fracs = sum(Unit_Fraction, na.rm = TRUE)) %>%
    collect() %>% 
    rename(kthid = Unit_code)
  
  researchers %>%
    left_join(pubs, by = "kthid") %>% 
    select(-kthid)
}
  
#' Retrieve publication list for staffbased ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the kthids belonging to the analyzed unit
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with publication list data for selected unit
#' @import DBI dplyr tidyr purrr
#' @export
abm_publications_staffbased <- function(con, unit_code, 
                                        analysis_start = abm_config()$start_year, 
                                        analysis_stop = abm_config()$stop_year) {
  ### Probably obsolete ###
  abm_staff_data(kthids = abm_researchers(unit_code), 
                 analysis_start = analysis_start, analysis_stop = analysis_stop, con = con) %>% 
    arrange(Publication_Year, Publication_Type_DiVA, WoS_Journal, PID)
  
}

