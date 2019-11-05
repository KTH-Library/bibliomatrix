#' Retrieve data for ABM tables and graphs from master table
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code for filtering on one or more unit code(s), which can be KTH, a one letter school code, an integer department code or a KTH-id (optional)
#' @param pub_year for filtering on publication years, for example 2012, 2012:2018 or c(2012, 2014, 2016) (optional)
#' @param unit_level for filtering on organizational level, 0 = KTH, 1 = school, 2 = deparment, 3 = researcher.
#' @return tibble with all ABM data for selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export

abm_data <- function(con = con_bib(), unit_code, pub_year, unit_level) {
  res <- con %>% tbl("masterfile")
  if (!missing(unit_code))
    res <- res %>% filter(Unit_code %in% unit_code)
  if (!missing(pub_year))
    res <- res %>% filter(Publication_Year %in% pub_year)
  if (!missing(unit_level))
    res <- res %>% filter(level %in% unit_level)
  
  return(res)
}

#' Retrieve Table 1 (Publications in DiVA) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return table over publications by type and year for display in ABM
#' @import DBI dplyr tidyr purrr
#' @export

abm_table1 <- function(con = con_bib(), unit_code, pub_year){
  # ToDo:
  # - Giving con to abm_data gives error message (external pointer is not valid) - fix that!
  # - Fetch publication type sort order from somewhere (should probably just keep a small table in DB)
  # - collect() when?
  # - Return reasonable field names
  # - Return reasonable formats (for example WoS_coverage as percentage)

  # Get publication level data for selected unit (and filter on pub_year if given)
  orgdata <- abm_data(unit_code = unit_code) %>%
    collect()
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)
  
  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year, Publication_Type_DiVA) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = T)) %>%
    pivot_wider(names_from = Publication_Year, values_from = P_frac) %>%
    ungroup()

  # Summary part of table
  table2 <-
    orgdata %>%
    group_by(Publication_Type_DiVA) %>%
    summarise(P_frac = sum(Unit_Fraction),
              WoS_coverage = sum(Unit_Fraction * !is.na(WebofScience_ID)) / sum(Unit_Fraction) ) %>%
    ungroup()
  
  dbDisconnect(con)
  
  table1 %>% merge(table2)
}
