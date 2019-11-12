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

#' Get order of publication type for ABM table 1
#' 
#' @param con connection to db, default is to use mssql connection
#' @return tibble with pt_ordning and diva_publiation_type
#' @import DBI dplyr tidyr purrr
#' @export
get_pt_ordning <- function(con = con_bib()){
  con %>% tbl("Diva_publication_types") %>% collect()
}

#' Retrieve Table 1 (Publications in DiVA) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return data frame with publications by type and year
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table1 <- function(con = con_bib(), unit_code, pub_year){

  # Get publication level data for selected unit (and filter on pub_year if given)
  orgdata <- abm_data(con = con, unit_code = unit_code) %>%
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
  
  table1 %>%
    merge(table2) %>%
    merge(get_pt_ordning(), by.x = "Publication_Type_DiVA", by.y = "diva_publication_type") %>%
    arrange(pt_ordning) %>%
    select(-pt_ordning)
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

  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(con = con, unit_code = unit_code) %>%
    filter(Publication_Type_WoS %in% c("Article", "Proceedings paper", "Review", "Letter", "Editorial") &
           Publication_Year < max(Publication_Year) - 1) %>%
    mutate(Publication_Year_ch = as.character(Publication_Year)) %>%
    collect()
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)

  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year_ch) %>%
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
    mutate(Publication_Year_ch = "Total")

  rbind(table1, table2)
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
#' @importFrom stats weighted.mean
#' @export

abm_table3 <- function(con = con_bib(), unit_code, pub_year){

  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(con = con, unit_code = unit_code) %>%
    filter(Publication_Type_WoS %in% c("Article", "Review") &
             Publication_Year < max(Publication_Year) & 
             !is.na(cf)) %>%
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
    summarise(P_frac = sum(Unit_Fraction_adj),
              cf = weighted.mean(cf, Unit_Fraction_adj, na.rm = T),
              top10_count = sum(Ptop10*Unit_Fraction_adj, na.rm = T),
              top10_share = weighted.mean(Ptop10, Unit_Fraction_adj, na.rm = T)) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction_adj),
              cf = weighted.mean(cf, Unit_Fraction_adj, na.rm = T),
              top10_count = sum(Ptop10*Unit_Fraction_adj, na.rm = T),
              top10_share = weighted.mean(Ptop10, Unit_Fraction_adj, na.rm = T)) %>%
    mutate(interval = "Total")

  rbind(table1, table2)
}

#' Retrieve Table 4 (Journal impact) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table4 <- function(con = con_bib(), unit_code, pub_year){

  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(con = con, unit_code = unit_code) %>%
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
              top20_count = sum(Jtop20*Unit_Fraction, na.rm = T),
              top20_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = T)) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = T),
              top20_count = sum(Jtop20*Unit_Fraction, na.rm = T),
              top20_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = T)) %>%
    mutate(interval = "Total")
  
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

  # Get publication level data for selected unit (and filter on pub_year if given), relevant WoS doctypes only
  orgdata <- abm_data(con = con, unit_code = unit_code) %>%
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
  
  rbind(table1, table2)
}

#' Retrieve dashboard indicators for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @return list with indicator values for dashboard startpage
#' @import DBI dplyr tidyr purrr
#' @export
abm_dash_indices <- function(con = con_bib(), unit_code){
  
  # Fetch table 1 for total number of publications and lastyear
  t1 <- abm_table1(con = con, unit_code = unit_code)
  lastyear <- max(as.integer(names(t1)[grep("[0-9]{4}", names(t1))]))

  # Fetch table 3 for cf and top10
  t3 <- abm_table3(con = con, unit_code = unit_code) %>%
    filter(interval == paste(lastyear - 3, lastyear - 1, sep = "-"))
  
  # Fetch table 4 for jcf and top20
  t4 <- abm_table4(con = con, unit_code = unit_code) %>%
    filter(interval == paste(lastyear - 2, lastyear, sep = "-"))
  
  # Fetch table 5 for non-univ and international copublications
  t5 <- abm_table5(con = con, unit_code = unit_code) %>%
    filter(interval == paste(lastyear - 2, lastyear, sep = "-"))
  
  dbDisconnect(con)
  
  list(tot_pubs_frac = sum(t1[, as.character(lastyear)], na.rm = TRUE),
       cf = t3$cf,
       top10_share = t3$top10_share,
       jcf = t4$jcf,
       top20_share = t4$top20_share,
       copub_nonuniv = t5$nonuniv_share,
       copub_internat = t5$int_share)
}

#' Retrieve information about ABM units (level 0-2)
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit unit code(s) for selection
#' @param level organizational level(s) for selection (0 = KTH, 1 = School, 2 = Department)
#' @param parent selects organizations with specific parent(s)
#' @return tibble with information about selected units
#' @import DBI dplyr tidyr purrr
#' @export

unit_info <- function(con = con_bib(), unit, level, parent){
  # ToDo: Make possible to select on Diva_org_id (and possibly some catch-all search)

  res <- con %>% tbl("abm_org_info") %>% collect()
  if(!missing(level))
    res <- res %>% filter(org_level %in% level)
  if(!missing(unit))
    res <- res %>% filter(unit_code %in% unit)
  if(!missing(parent))
    res <- res %>% filter(parent_org_id %in% parent)
  
  dbDisconnect(con)
  
  res
}

#' Display Table 1 (Publications in DiVA) for web use
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return data frame with publications by type and year
#' @import DBI dplyr tidyr purrr
#' @export
abm_table1_display <- function(con = con_bib(), unit_code, pub_year){
  if(missing(pub_year)) table1 <- abm_table1(con, unit_code) else table1 <- abm_table1(con, unit_code, pub_year)
  
  dbDisconnect(con)
  
  table1
}

#' Display Table 2 (Citations 3-year window) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with citations statistics by year and total
#' @import DBI dplyr tidyr purrr
#' @export
abm_table2_display <- function(con = con_bib(), unit_code, pub_year){
  if(missing(pub_year)) table2 <- abm_table2(con, unit_code) else table2 <- abm_table2(con, unit_code, pub_year)
  
  dbDisconnect(con)
  
  table2
}

#' Display Table 3 (Field normalized citations) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @export
abm_table3_display <- function(con = con_bib(), unit_code, pub_year){
  if(missing(pub_year)) table3 <- abm_table3(con, unit_code) else table3 <- abm_table1(con, unit_code, pub_year)
  
  dbDisconnect(con)
  
  table3
}

#' Display Table 4 (Journal impact) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @export
abm_table4_display <- function(con = con_bib(), unit_code, pub_year){
  if(missing(pub_year)) table4 <- abm_table4(con, unit_code) else table4 <- abm_table1(con, unit_code, pub_year)
  
  dbDisconnect(con)
  
  table4
}

#' Display Table 5 (Co-publishing) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export
abm_table5_display <- function(con = con_bib(), unit_code, pub_year){
  if(missing(pub_year)) table5 <- abm_table5(con, unit_code) else table5 <- abm_table5(con, unit_code, pub_year)

  dbDisconnect(con)
  
  table5
}
    
#' Retrieve publication list for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param pub_year publication year(s) to analyze (optional, assuming master table holds only relevant years)
#' @return tibble with publication list data for selected unit
#' @import DBI dplyr tidyr purrr
#' @export
abm_publications <- function(con = con_bib(), unit_code, pub_year){

  # Get publication level data for selected unit (and filter on pub_year if given)
  orgdata <- abm_data(unit_code = unit_code)
  if(!missing(pub_year))
    orgdata <- filter(orgdata, Publication_Year %in% pub_year)
  
  ret <- orgdata %>% select(-c("w_subj", "Unit_Fraction_adj", "level")) %>% collect()
  
  dbDisconnect(con)
  
  ret
}
  
  
  
