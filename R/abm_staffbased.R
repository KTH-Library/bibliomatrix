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
#' @return tibble with all staff-based ABM data for selected organizational unit
#' @export
abm_staff_data <- function(con = con_bib(), kthids) {
  
    res <- con %>%
      tbl("masterfile") %>%
      filter(Unit_code %in% kthids)  %>% 
      collect() #%>% 
    
    auth_count<- res %>% group_by(PID) %>% tally() 
    
    res %>% 
      inner_join(auth_count, by="PID") %>% 
      distinct(PID, WebofScience_ID, .keep_all = TRUE)
}


# Alt versions of abm_table functions
#---------------------------------------
#' Retrieve Table 1 (Publications in DiVA) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return data frame with publications by type and year
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table1_alt <- function(con = con_bib(), data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {
  
  #attach(data)
  # Get publication level data for selected unit
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    mutate(wos_bin = ifelse(!is.na(Doc_id),1,0))
  
  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year, Publication_Type_DiVA) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE)) %>%
    collect() %>%
    arrange(Publication_Year) %>%
    pivot_wider(names_from = Publication_Year, values_from = P_frac) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    group_by(Publication_Type_DiVA) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              WoS_coverage = sum(wos_bin * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>% 
    collect()
  
  pubtype_order <- get_pubtype_order(con)
  
  table1 %>%
    inner_join(table2, by = "Publication_Type_DiVA") %>%
    inner_join(pubtype_order, by = c("Publication_Type_DiVA" = "diva_publication_type")) %>%
    arrange(pt_ordning) %>%
    select(-pt_ordning)
}
