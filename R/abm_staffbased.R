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
#' @param analysisId id for analysis
#' @return tibble with staff list for selected organizational unit
#' @export
abm_staff_list <- function(con, unit_slug,
                           analysis_start = abm_config()$start_year, 
                           analysis_stop = abm_config()$stop_year,
                           analysisId = abm_config()$analysis_id) {
  
  researchers <-
    con %>%
    tbl("researchers") %>% 
    collect() %>%
    filter(stringr::str_starts(slug, unit_slug)) %>%
    distinct() %>%
    select(kthid, firstName, lastName, Title = `title.en`)
  
  kthids <- researchers$kthid
  pubs <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code %in% kthids &
             level == 3 &
             is_kth == 1 &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             analysis_id == analysisId) %>%
    group_by(Unit_code) %>%
    summarise(pubs = n(), fracs = sum(Unit_Fraction, na.rm = TRUE)) %>%
    collect() %>% 
    rename(kthid = Unit_code)
  
  researchers %>%
    left_join(pubs, by = "kthid") %>% 
    select(-kthid)
}

#' Create staffbased table over co-publication countries for ABM unit
#' 
#' @param con a database connection to BIBMON
#' @param publist a publication list including at least WebofScience_ID, Unit_fraction and n_authors
#' @param exclude_swe wether to exclude Sweden as co-publication country, default TRUE
#' @param limit if set, limit the result to the first limit rows, default NULL
#' @return a tibble
#' @import dplyr
#' @importFrom utils head
#' @export
abm_copub_countries_staffbased <- function(con,
                                           publist,
                                           exclude_swe = TRUE,
                                           limit = NULL){
  
  
  uts <- publist %>%
    filter(!is.na(WebofScience_ID)) %>%
    select(WebofScience_ID, Unit_Fraction, n_authors) %>%
    rename(UT = WebofScience_ID)
  
  uts_countries <- con %>%
    tbl("Bestresaddr_KTH") %>%
    filter(UT %in% !!uts$UT) %>%
    select(UT, Country_name) %>%
    collect() %>%
    rename(country = Country_name) %>% 
    unique()
  
  countries <- uts %>%
    inner_join(uts_countries, by = "UT") %>%
    mutate( p_10 = ifelse(n_authors <= 10,  1, 0),
            p_50 = ifelse(n_authors %in% 11:50, 1, 0),
            p_200 = ifelse(n_authors %in% 51:200, 1, 0),
            p_over200 = ifelse(n_authors > 200, 1, 0)) %>%
    group_by(country) %>%
    summarise( p = n(),
               p_10 = sum(p_10, na.rm = TRUE),
               p_50 = sum(p_50, na.rm = TRUE),
               p_200 = sum(p_200, na.rm = TRUE),
               p_over200 = sum(p_over200, na.rm = TRUE),
               kth_frac = sum(Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>% 
    arrange(-kth_frac)
  
  if(exclude_swe == TRUE)
    countries <- countries %>% filter(country != "Sweden")
  
  if(!is.null(limit))
    countries <- head(countries, limit)
  
  countries
}

#' Create staffbased table over co-publication organizations for ABM unit
#' 
#' @param con a database connection to BIBMON
#' @param publist a publication list including at least WebofScience_ID, Unit_fraction and n_authors
#' @param exclude_swe wether to exclude Sweden as co-publication country, default TRUE
#' @param limit if set, limit the result to the first limit rows, default NULL
#' @return a tibble
#' @import dplyr
#' @importFrom utils head
#' @export
abm_copub_orgs_staffbased <- function(con,
                                      publist,
                                      exclude_swe = FALSE,
                                      limit = 1000){
  
  uts <- publist %>%
    filter(!is.na(WebofScience_ID)) %>%
    select(WebofScience_ID, Unit_Fraction, n_authors) %>%
    rename(UT = WebofScience_ID)
  
  orgtype <- con %>% 
    tbl("Organization_type") %>% 
    collect()

  uts_orgs <- con %>%
    tbl("Bestresaddr_KTH") %>%
    filter(UT %in% !!uts$UT,
           coalesce(Unified_org_id, 0) != 8) %>%
    select(UT, Name_eng, Org_type_code, Unified_org_id, Country_name) %>%
    collect() %>%
    left_join(orgtype, by = "Org_type_code") %>% 
    rename(org = Name_eng, org_type = Org_type_eng, unified_org_id = Unified_org_id, country = Country_name) %>%
    unique()
  
  orgs <- uts %>%
    inner_join(uts_orgs, by = "UT") %>%
    mutate( p_10 = ifelse(n_authors <= 10,  1, 0),
            p_50 = ifelse(n_authors %in% 11:50, 1, 0),
            p_200 = ifelse(n_authors %in% 51:200, 1, 0),
            p_over200 = ifelse(n_authors > 200, 1, 0)) %>%
    group_by(org, org_type, unified_org_id, country) %>%
    summarise(p = n(),
              p_10 = sum(p_10, na.rm = TRUE),
              p_50 = sum(p_50, na.rm = TRUE),
              p_200 = sum(p_200, na.rm = TRUE),
              p_over200 = sum(p_over200, na.rm = TRUE),
              kth_frac = sum(Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>% 
    collect() %>% 
    arrange(-kth_frac)
  
  if(exclude_swe == TRUE)
    orgs <- orgs %>% filter(country != "Sweden")
  
  if(!is.null(limit))
    orgs <- head(orgs, limit)
  
  orgs
}
