#' Get publications from members of unit via kthids
#' 
#' @param con A database connection
#' @param kthids a list of KTH-ids to retrieve publications for
#' @param analysis_start first publication year of analysis
#' @param analysis_stop last publication year of analysis
#' @param analysisId id for analysis
#' @return tibble with all staff-based ABM data for selected organizational unit
#' @export
abm_staff_data <- function(con, kthids,
                           analysis_start = abm_config()$start_year, 
                           analysis_stop = abm_config()$stop_year,
                           analysisId = abm_config()$analysis_id) {

  res <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code %in% kthids &
             level == 3 &
             is_kth == 1 &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             analysis_id == analysisId) %>%
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

