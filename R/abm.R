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
  
  res
}

#' Retrieve Table 1 (Publications in DiVA) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return data frame with publications by type and year
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
    summarise(P_frac = sum(Unit_Fraction)) %>%
    pivot_wider(names_from = Publication_Year, values_from = P_frac) %>%
    ungroup()

  # Summary part of table
  table2 <-
    orgdata %>%
    group_by(Publication_Type_DiVA) %>%
    summarise(P_frac = sum(Unit_Fraction),
              WoS_coverage = weighted.mean(!is.na(WebofScience_ID), Unit_Fraction, na.rm = T)) %>%
    ungroup()

  dbDisconnect(con)
  
  table1 %>% inner_join(table2) %>% as_tibble()
}

#' Retrieve Table 2 (Citations 3-year window) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with citations statistics by year and total
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table2 <- function(con = con_bib(), unit_code, pub_year){
  # Todo:
  # - See abm_table1 above

  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(unit_code = unit_code) %>%
    filter(Publication_Type_WoS %in% c("Article", "Proceedings paper", "Review", "Letter", "Editorial") &
           Publication_Year < max(Publication_Year) - 1) %>%
    mutate(Publication_Year = as.character(Publication_Year)) %>%
    collect()
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)

  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year) %>%
    summarise(P_frac = sum(Unit_Fraction),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = T),
              C3 = weighted.mean(Citations_3yr, Unit_Fraction, na.rm = T)) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = T),
              C3 = weighted.mean(Citations_3yr, Unit_Fraction, na.rm = T)) %>%
    mutate(Publication_Year = "Total")

  dbDisconnect(con)

  bind_rows(table1, table2)
}

#' Create integer intervals useful for e.g. sliding means
#'
#' @param first the smallest integer in the range to use
#' @param last the largest integer in the range to use
#' @param width the desired width of intervals
#' @return data frame with label for each interval and one row for each year

sliding_intervals <- function(first, last, width){
  
  starts <- seq(first, last - width + 1)
  interval <- paste0(starts, "-", starts + width - 1)
  
  data.frame(interval = rep(interval, each = width), x = rep(starts, each = width) + rep(seq(0, width - 1), length(starts)))
}

#' Retrieve Table 3 (Field normalized citations) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @export

abm_table3 <- function(con = con_bib(), unit_code, pub_year){
  # Todo:
  # - See abm_table1 above
  
  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(unit_code = unit_code) %>%
    filter(Publication_Type_WoS %in% c("Article", "Review") &
             Publication_Year < max(Publication_Year)) %>%
    collect()
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <- merge(x = orgdata,
                        y = sliding_intervals(min(orgdata$Publication_Year), max(orgdata$Publication_Year), 3),
                        by.x = "Publication_Year",
                        by.y = "x")

  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_frac = sum(Unit_Fraction),
              cf = weighted.mean(cf, Unit_Fraction, na.rm = T),
              top10_count = sum(Ptop10*Unit_Fraction, na.rm = T),
              top10_share = weighted.mean(Ptop10, Unit_Fraction, na.rm = T)) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction),
              cf = weighted.mean(cf, Unit_Fraction, na.rm = T),
              top10_count = sum(Ptop10*Unit_Fraction, na.rm = T),
              top10_share = weighted.mean(Ptop10, Unit_Fraction, na.rm = T)) %>%
    mutate(interval = "Total")

  dbDisconnect(con)
  
  rbind(table1, table2)
}

#' Retrieve Table 4 (Journal impact) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @export

abm_table4 <- function(con = con_bib(), unit_code, pub_year){
  # Todo:
  # - See abm_table1 above
  
  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(unit_code = unit_code) %>%
    filter(Publication_Type_WoS %in% c("Article", "Review")) %>%
    collect()
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <- merge(x = orgdata,
                        y = sliding_intervals(min(orgdata$Publication_Year), max(orgdata$Publication_Year), 3),
                        by.x = "Publication_Year",
                        by.y = "x")
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_frac = sum(Unit_Fraction),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = T),
              top10_count = sum(Jtop20*Unit_Fraction, na.rm = T),
              top10_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = T)) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = T),
              top10_count = sum(Jtop20*Unit_Fraction, na.rm = T),
              top10_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = T)) %>%
    mutate(interval = "Total")
  
  dbDisconnect(con)
  
  rbind(table1, table2)
}

#' Retrieve Table 5 (Co-publishing) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table5 <- function(con = con_bib(), unit_code, pub_year){
  # Todo:
  # - See abm_table1 above
  
  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(unit_code = unit_code) %>%
    filter(Publication_Type_WoS %in% c("Article", "Review")) %>%
    collect()
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)

  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <- merge(x = orgdata,
                        y = sliding_intervals(min(orgdata$Publication_Year), max(orgdata$Publication_Year), 3),
                        by.x = "Publication_Year",
                        by.y = "x")
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_full = n(),
              nonuniv_count = sum(swe_nuniv, na.rm = T),
              nonuniv_share = mean(swe_nuniv, na.rm = T),
              int_count = sum(int, na.rm = T),
              int_share = mean(int, na.rm = T)) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_full = n(),
              nonuniv_count = sum(swe_nuniv, na.rm = T),
              nonuniv_share = mean(swe_nuniv, na.rm = T),
              int_count = sum(int, na.rm = T),
              int_share = mean(int, na.rm = T)) %>%
    mutate(interval = "Total")
  
  dbDisconnect(con)
  
  rbind(table1, table2)
}

