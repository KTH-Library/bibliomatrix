#' Provide defaults for ABM analysis
#'
#'@return list of default values used in the current abm presentation
#'@export

abm_config <- function() {
  
  # this can later be expanded with more relevant defaults
  y_start <- 2013
  y_stop <- 2019
  
  if (Sys.getenv("ABM_START_YEAR") != "")
    y_start <- Sys.getenv("ABM_START_YEAR")
  
  if (Sys.getenv("ABM_STOP_YEAR") != "")
    y_stop <- Sys.getenv("ABM_STOP_YEAR")
  
  list(
    start_year = y_start, 
    stop_year = y_stop,
    default_unit = "KTH",
    analysis_id = 1
  )
}

#' Retrieve data for ABM tables and graphs from master table
#' 
#' @param con connection to db, default is sqlite connection
#' @param unit_code for filtering on one or more unit code(s), which can be KTH, a one letter school code, an integer department code or a KTH-id (optional)
#' @param pub_year for filtering on publication years, for example 2012, 2012:2018 or c(2012, 2014, 2016) (optional)
#' @param unit_level for filtering on organizational level, 0 = KTH, 1 = school, 2 = deparment, 3 = researcher.
#' @param analysis_id for filtering on specific analysis, for example 1 = KTH ABM 2020
#' @return tibble with all ABM data for selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export

abm_data <- function(con = con_bib(), unit_code, pub_year, unit_level, analysis_id) {
  res <- con %>% tbl("masterfile")
  if (!missing(analysis_id))
    res <- res %>% filter(analysis_id == analysis_id)
  if (!missing(unit_code))
    res <- res %>% filter(Unit_code %in% unit_code)
  if (!missing(pub_year))
    res <- res %>% filter(Publication_Year %in% pub_year)
  if (!missing(unit_level))
    res <- res %>% filter(level %in% unit_level)
  
  res %>% collect()
}

#' Get order of publication type for ABM table 1
#' 
#' @param con connection to db - if no connection is given, use abm_public_kth data
#' @return tibble with pt_ordning and diva_publiation_type
#' @import DBI dplyr tidyr purrr
#' @export
get_pubtype_order <- function(con){
  if(!missing(con)){
    con %>% tbl("Diva_publication_types") %>% collect()
  } else {
    abm_public_kth$pubtype_order
  }
}

#' Get displayname and descriptions for indicators
#' 
#' @param con connection to db - if no connection is given, use abm_public_kth data
#' @return tibble with names and descriptions
#' @import DBI dplyr tidyr purrr
#' @export
get_indic_descriptions <- function(con){
  if(!missing(con)){
    con %>% tbl("indicator_descriptions") %>% collect()
  } else {
    abm_public_kth$indicator_descriptions
  }
}

#' Retrieve Table 1 (Publications in DiVA) for ABM, fractional counts
#' 
#' @param data dataset with publications as tibble
#' @param con connection to db
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return data frame with publications by type and year
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table1 <- function(data, con, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {
  
  if(!missing(con)){
    pubtype_order <- get_pubtype_order(con)
  } else {
    pubtype_order <- get_pubtype_order()
  }
  
  # Get publication level data for selected unit
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    mutate(wos_bin = ifelse(!is.na(Doc_id), 1, 0),
           scop_bin = ifelse(!is.na(ScopusID), 1, 0))

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
              WoS_coverage = sum(wos_bin * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              Scopus_coverage = sum(scop_bin * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>% 
    collect()
  
  
  
  table1 %>%
    inner_join(table2, by = "Publication_Type_DiVA") %>%
    inner_join(pubtype_order, by = c("Publication_Type_DiVA" = "diva_publication_type")) %>%
    arrange(pt_ordning) %>%
    select(-pt_ordning)
}

#' Retrieve Table 1 (Publications in DiVA) for ABM, full counts
#' 
#' @param data dataset with publications as tibble
#' @param con connection to db
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return data frame with publications by type and year
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table1_full <- function(data, con, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {

  if(!missing(con)){
    pubtype_order <- get_pubtype_order(con)
  } else {
    pubtype_order <- get_pubtype_order()
  }
  
  # Get publication level data for selected unit
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    mutate(wos_bin = ifelse(!is.na(Doc_id), 1, 0),
           scop_bin = ifelse(!is.na(ScopusID), 1, 0))
  
  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year, Publication_Type_DiVA) %>%
    summarise(P_full = n()) %>%
    arrange(Publication_Year) %>%
    pivot_wider(names_from = Publication_Year, values_from = P_full) %>%
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata %>%
    group_by(Publication_Type_DiVA) %>%
    summarise(P_full = n(),
              WoS_coverage = mean(wos_bin, na.rm = TRUE),
              Scopus_coverage = mean(scop_bin, na.rm = TRUE)) %>%
    ungroup()
  
  table1 %>%
    inner_join(table2, by = "Publication_Type_DiVA") %>%
    inner_join(pubtype_order, by = c("Publication_Type_DiVA" = "diva_publication_type")) %>%
    arrange(pt_ordning) %>%
    select(-pt_ordning)
}

#' Retrieve Table 2 (Citations 3-year window) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with citations statistics by year and total
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table2 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>% 
    filter(Publication_Year >= analysis_start &
           Publication_Year <= analysis_stop - 2 &
           Publication_Type_WoS %in% c("Article", "Proceedings Paper", "Review", "Letter", "Editorial")) %>%
    mutate(uncited = ifelse(Citations_3yr == 0, 1, 0))
  
  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              C3 = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              P_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Publication_Year_ch = as.character(Publication_Year)) %>%
    arrange(Publication_Year_ch) %>% 
    select(Publication_Year_ch, P_frac, C3, C3_frac, P_uncited, Share_uncited)
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              C3 = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              P_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    mutate(Publication_Year_ch = "Total")
  
  bind_rows(table1, table2)
}

#' Create integer intervals useful for e.g. sliding means
#'
#' @param first the smallest integer in the range to use
#' @param last the largest integer in the range to use
#' @param width the desired width of intervals
#' @return data frame with label for each interval and one row for each year
#' @export
sliding_intervals <- function(first, last, width){
  
  starts <- seq(first, last - width + 1)
  interval <- paste0(starts, "-", starts + width - 1)
  
  data.frame(interval = rep(interval, each = width), x = rep(starts, each = width) + rep(seq(0, width - 1), length(starts)), stringsAsFactors = FALSE)
}

#' Retrieve Table 3 (Field normalized citations) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table3 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 1 &
             Publication_Type_WoS %in% c("Article", "Review") & 
             !is.na(cf))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    inner_join(sliding_intervals(analysis_start, analysis_stop - 1, 3),
               by = c("Publication_Year" = "x"))
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_frac = sum(Unit_Fraction_adj, na.rm = TRUE),
              cf = weighted.mean(cf, Unit_Fraction_adj, na.rm = TRUE),
              top10_count = sum(Ptop10*Unit_Fraction_adj, na.rm = TRUE),
              top10_share = weighted.mean(Ptop10, Unit_Fraction_adj, na.rm = TRUE)) %>%
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction_adj, na.rm = TRUE),
              cf = sum(cf * Unit_Fraction_adj, na.rm = TRUE) / sum(Unit_Fraction_adj, na.rm = TRUE),
              top10_count = sum(Ptop10 * Unit_Fraction_adj, na.rm = TRUE),
              top10_share = sum(Ptop10 * Unit_Fraction_adj, na.rm = TRUE) / sum(Unit_Fraction_adj, na.rm = TRUE)) %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}

#' Retrieve Table 4 (Journal impact) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table4 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_WoS %in% c("Article", "Review") &
             !is.na(jcf))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    inner_join(sliding_intervals(analysis_start, analysis_stop, 3),
               by = c("Publication_Year" = "x"))
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = TRUE)) %>%
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = TRUE)) %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}

#' Retrieve Table 5 (Co-publishing) for ABM
#'
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table5 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_WoS %in% c("Article", "Review") &
             !is.na(int))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    inner_join(sliding_intervals(analysis_start, analysis_stop, 3),
               by = c("Publication_Year" = "x"))
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_full = n(),
              nonuniv_count = sum(swe_nuniv, na.rm = TRUE),
              nonuniv_share = mean(swe_nuniv, na.rm = TRUE),
              int_count = sum(int, na.rm = TRUE),
              int_share = mean(int, na.rm = TRUE)) %>%
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_full = n(),
              nonuniv_count = sum(swe_nuniv, na.rm = TRUE),
              nonuniv_share = mean(swe_nuniv, na.rm = TRUE),
              int_count = sum(int, na.rm = TRUE),
              int_share = mean(int, na.rm = TRUE)) %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}


#' Retrieve OA-data for ABM
#' 
#' @param con connection to db, default is sqlite connection
#' @param unit_code for filtering on one or more unit code(s), which can be KTH, a one letter school code, an integer department code or a KTH-id (optional)
#' @return tibble with OA-status of all publications from selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export
abm_oa_data <- function(con = con_bib(), unit_code) {
  
  # NB: we avoid a right_join which is not supported in SQLite3 and use a left join
  # and switch the order of the joined tables
  
  # IS THIS FUNCTION USED (previously used in abm_table6) ?
  
  abm_data(con = con, unit_code = unit_code) %>%
    select("PID", "oa_status", "is_oa", 
           "Publication_Type_DiVA", "Publication_Year")
  
}


#' Retrieve Table 6 (OA data) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with OA-status of all publications from incoming data
#' @import DBI dplyr tidyr purrr
#' @export
abm_table6 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {

  # Year-dependent part of table
  table1 <-
    data %>%
    filter((is_oa=="TRUE" | is_oa=="FALSE") &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    group_by(Publication_Year) %>%
    summarise(P_tot=n(),
              oa_count=sum(as.logical(is_oa), na.rm=TRUE),
              gold_count=sum(as.logical(oa_status=="gold"), na.rm=TRUE),
              hybrid_count=sum(as.logical(oa_status=="hybrid"), na.rm=TRUE),
              green_count=sum(as.logical(oa_status=="green"), na.rm=TRUE),
              bronze_count=sum(as.logical(oa_status=="bronze"), na.rm=TRUE),
              closed_count=sum(as.logical(oa_status=="closed"), na.rm=TRUE),
              oa_share=mean(as.logical(is_oa), na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Publication_Year_ch = as.character(Publication_Year)) %>%
    arrange(Publication_Year_ch) %>%
    select(Publication_Year_ch, P_tot, oa_count, gold_count, hybrid_count, green_count, bronze_count, closed_count, oa_share)
  
  # Insert blank years
  years_to_add <- table1[!(as.character(as.numeric(table1$Publication_Year_ch)+1) %in% table1$Publication_Year_ch)
                         & !is.na(lead(table1$Publication_Year_ch)),"Publication_Year_ch"] %>%
    mapply(function (x) as.numeric(x)+1, .)
  
  for (year in years_to_add){
    if (length(year) > 0){
      plusyear = 1
      while (!(as.character(year+plusyear) %in% table1$Publication_Year_ch)){
        years_to_add <- append(years_to_add, year+plusyear)
        plusyear <- plusyear + 1
      }
    }
  }
  
  for (year in years_to_add){
    if (length(year) > 0){
      table1 <- table1 %>% rbind(data.frame(Publication_Year_ch = as.character(year),
                                            P_tot = integer(1),
                                            oa_count = integer(1),
                                            gold_count = integer(1),
                                            hybrid_count = integer(1),
                                            green_count = integer(1),
                                            bronze_count = integer(1),
                                            closed_count = integer(1),
                                            oa_share = 0))
    }
  } 
  
  table1 <- table1 %>% arrange(Publication_Year_ch)
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <- table1 %>%
    summarise(Publication_Year_ch="Total",
              P_tot=sum(P_tot),
              oa_count=sum(oa_count),
              gold_count=sum(gold_count),
              hybrid_count=sum(hybrid_count),
              green_count=sum(green_count),
              bronze_count=sum(bronze_count),
              closed_count=sum(closed_count),
              oa_share=sum(oa_count)/sum(P_tot))
  
  bind_rows(table1, table2)
}

#' Retrieve citations table (Scopus) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with citations statistics by year and total
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table_scop_cit <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data %>% 
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Conference Paper", "Review", "Letter", "Editorial") &
             !is.na(scop_cscxo)) %>%
    mutate(uncited = ifelse(scop_cscxo > 0, 0, 1))
  
  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              C_sum = sum(Unit_Fraction * scop_cscxo, na.rm = TRUE),
              C_avg = weighted.mean(scop_cscxo, Unit_Fraction, na.rm = TRUE),
              P_uncited_scop = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited_scop = weighted.mean(uncited, Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Publication_Year_ch = as.character(Publication_Year)) %>%
    arrange(Publication_Year_ch) %>% 
    select(Publication_Year_ch, P_frac, C_sum, C_avg, P_uncited_scop, Share_uncited_scop)
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              C_sum = sum(Unit_Fraction * scop_cscxo, na.rm = TRUE),
              C_avg = weighted.mean(scop_cscxo, Unit_Fraction, na.rm = TRUE),
              P_uncited_scop = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited_scop = weighted.mean(uncited, Unit_Fraction, na.rm = TRUE)) %>%
    mutate(Publication_Year_ch = "Total") %>% 
    select(Publication_Year_ch, P_frac, C_sum, C_avg, P_uncited_scop, Share_uncited_scop)
  
  bind_rows(table1, table2)
}

#' Retrieve field normalized citations table (Scopus) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table_scop_normcit <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 1 &
             scop_doctype %in% c("Article", "Review", "Conference Paper") & 
             !is.na(scop_fwci_x))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    inner_join(sliding_intervals(analysis_start, analysis_stop - 1, 3),
               by = c("Publication_Year" = "x"))
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              fwci_x = weighted.mean(scop_fwci_x, Unit_Fraction, na.rm = TRUE),
              top10_count = sum(scop_Ptop10 * Unit_Fraction, na.rm = TRUE),
              top10_share = weighted.mean(scop_Ptop10, Unit_Fraction, na.rm = TRUE)) %>%
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              fwci_x = weighted.mean(scop_fwci_x, Unit_Fraction, na.rm = TRUE),
              top10_count = sum(scop_Ptop10 * Unit_Fraction, na.rm = TRUE),
              top10_share = weighted.mean(scop_Ptop10, Unit_Fraction, na.rm = TRUE)) %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}

#' Retrieve Journal impact table (Scopus) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table_scop_snip <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Review", "Conference Paper") &
             !is.na(scop_snip))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    inner_join(sliding_intervals(analysis_start, analysis_stop, 3),
               by = c("Publication_Year" = "x"))
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              avg_snip = weighted.mean(scop_snip, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(scop_Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(scop_Jtop20, Unit_Fraction, na.rm = TRUE)) %>%
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              avg_snip = weighted.mean(scop_snip, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(scop_Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(scop_Jtop20, Unit_Fraction, na.rm = TRUE)) %>% 
    mutate(interval = "Total")
    
  bind_rows(table1, table2)
}

#' Retrieve Co-publishing table (Scopus) for ABM
#'
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table_scop_copub <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Review", "Conference Paper") &
             !is.na(scop_int))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    inner_join(sliding_intervals(analysis_start, analysis_stop, 3),
               by = c("Publication_Year" = "x"))
  
  # Year dependent part of table
  table1 <-
    orgdata3year %>%
    group_by(interval) %>%
    summarise(P_full = n(),
              corp_count = sum(scop_corp, na.rm = TRUE),
              corp_share = mean(scop_corp, na.rm = TRUE),
              int_count = sum(scop_int, na.rm = TRUE),
              int_share = mean(scop_int, na.rm = TRUE)) %>%
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_full = n(),
              corp_count = sum(scop_corp, na.rm = TRUE),
              corp_share = mean(scop_corp, na.rm = TRUE),
              int_count = sum(scop_int, na.rm = TRUE),
              int_share = mean(scop_int, na.rm = TRUE)) %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}

#' Retrieve dashboard indicators for ABM
#' 
#' @param data dataset with publications as tibble
#' @return list with indicator values for dashboard startpage
#' @import DBI dplyr tidyr purrr
#' @export
abm_dash_indices <- function(data){
  
  # Fetch table 1 for total number of publications and lastyear
  t1 <- abm_table1(data)
  lastyear <- max(as.integer(names(t1)[grep("[0-9]{4}", names(t1))]))
  
  # Fetch table 3 for cf and top10
  t3 <- abm_table3(data) %>%
    filter(interval == paste(lastyear - 3, lastyear - 1, sep = "-"))
  
  # Fetch table 4 for jcf and top20
  t4 <- abm_table4(data) %>%
    filter(interval == paste(lastyear - 2, lastyear, sep = "-"))
  
  # Fetch table 5 for non-univ and international copublications
  t5 <- abm_table5(data) %>%
    filter(interval == paste(lastyear - 2, lastyear, sep = "-"))
  
  list(tot_pubs_frac = sum(t1[, as.character(lastyear)], na.rm = TRUE),
       cf = t3$cf,
       top10_share = t3$top10_share,
       jcf = t4$jcf,
       top20_share = t4$top20_share,
       copub_nonuniv = t5$nonuniv_share,
       copub_internat = t5$int_share)
}

#' Retrieve WoS and Scopus coverage for peer reviewed DiVA publication types
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, if not given abm_config() is used
#' @param analysis_stop last publication year of analysis, if not given abm_config() is used
#' @return tibble with fractionalized and full counted WoS coverage by year and publication type
#' @import dplyr
#' @export
abm_coverage <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {
  
  # Get publication level data for selected unit (and filter on pub_year if given)
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_DiVA %in% c("Article, peer review", "Conference paper, peer review")) %>%
    mutate(wos_bin = ifelse(!is.na(Doc_id), 1, 0),
           scop_bin = ifelse(!is.na(ScopusID), 1, 0)) %>%
    select(Publication_Year, Publication_Type_DiVA, Unit_Fraction, wos_bin, scop_bin) %>%
    group_by(Publication_Year, Publication_Type_DiVA) %>%
    summarise(p_frac = sum(Unit_Fraction, na.rm = TRUE),
              p_full = n(),
              sumwos_frac = sum(Unit_Fraction * wos_bin, na.rm = TRUE),
              sumwos_full = sum(wos_bin, na.rm = TRUE),
              woscov_frac = sum(Unit_Fraction * wos_bin, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              woscov_full = sum(wos_bin, na.rm = TRUE) / n(),
              sumscop_frac = sum(Unit_Fraction * scop_bin, na.rm = TRUE),
              sumscop_full = sum(scop_bin, na.rm = TRUE),
              scopcov_frac = sum(Unit_Fraction * scop_bin, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              scopcov_full = sum(scop_bin, na.rm = TRUE) / n()) %>%
    ungroup() %>%
    collect()
  
  peerreviewed <- orgdata %>%
    group_by(Publication_Year) %>%
    summarise(p_frac = sum(p_frac),
              p_full = sum(p_full),
              sumwos_frac = sum(sumwos_frac),
              sumwos_full = sum(sumwos_full),
              sumscop_frac = sum(sumscop_frac),
              sumscop_full = sum(sumscop_full)) %>%
    mutate(woscov_frac = sumwos_frac / p_frac,
           woscov_full = sumwos_full / p_full,
           scopcov_frac = sumscop_frac / p_frac,
           scopcov_full = sumscop_full / p_frac,
           Publication_Type = "Peer reviewed")
  
  orgdata %>%
    rename(Publication_Type = Publication_Type_DiVA) %>%
    bind_rows(peerreviewed) %>%
    arrange(Publication_Year, Publication_Type)
}

#' Sorting a hierarchical structure
#' 
#' This function takes a data frame with a tree-like structure,
#' sorts entries from each sub-level directly after it's parent
#' and returns the id and the resulting sorting order
#' 
#' @param df the data frame to be sorted
#' @param idfield the column name for the id field
#' @param levelfield the column name for the hierarchical level
#' @param parentfield the column name for the parent's id
#' @param sortfield the column name for sorting within level
#' 
#' @return tibble with id and sorting order
#' @import dplyr
#' @noRd
hiersort <- function(df, idfield, levelfield, parentfield, sortfield) {
  
  workdf <- df[, c(idfield, levelfield, parentfield, sortfield)]
  names(workdf) <- c("id", "level", "parent", "sortfield")
  
  levels <- unique(workdf$level) %>% sort()
  
  # Full sortname for first level is just sortfield
  res <- workdf %>% filter(level == levels[1])
  res$fullsort <- res$sortfield
  
  # For each subsequent level, fetch fullsort from parent and add sortfield to get unit's fullsort, then add to res
  for(lvl in levels[-1]){
    lvlres <- workdf %>%
      filter(level == lvl) %>%
      select(id, parent, sortfield) %>%
      inner_join(res %>% select(id, fullsort), by = c("parent" = "id")) %>%
      mutate(fullsort = paste0(fullsort, sortfield))
    res <- bind_rows(res, lvlres)
  }
  
  res <- select(res, id, fullsort)
  
  names(res)[1] <- idfield
  
  res %>%
    arrange(fullsort) %>%
    mutate(sort_order = as.integer(rownames(.))) %>%
    select(-fullsort)
}

#' Retrieve information about ABM units (level 0-2) from database or from package data
#' 
#' If a database connection is given, abm_org_info is read from database,
#' otherwise abm_public_kth$meta is returned
#' 
#' @param con connection to db
#'
#' @return tibble with information about ABM units
#' @import DBI dplyr
#' @importFrom stringr str_pad
#' @export

unit_info <- function(con){
  
  if(missing(con)){
    abm_public_kth$meta 
  } else {
    abm_units <- con %>% tbl("abm_org_info") %>% collect() %>% select(-"sort_order")
    
    abm_units %>%
      # Get full sort order
      inner_join(hiersort(abm_units, "Diva_org_id", "org_level", "parent_org_id", "unit_long_en"), by = "Diva_org_id") %>%
      # Add indented versions of unit_long_en, one with plain white space and one for usage in html where leading white space gets sanitized
      mutate(unit_long_en_indent1 = str_pad(unit_long_en, side = "left", width = 4*org_level + stringr::str_length(unit_long_en)),
             unit_long_en_indent2 = str_pad(unit_long_en, side = "left", width = 4*org_level + stringr::str_length(unit_long_en), pad = "\U00A0")) %>%
      arrange(-desc(sort_order))
  }
}

#' Retrieve publication list for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return tibble with publication list data for selected unit
#' @import DBI dplyr tidyr purrr
#' @export
abm_publications <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit
  data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    select(-c("w_subj", "Unit_Fraction_adj", "level", "is_oa", "analysis_id")) %>%
    mutate(oa_status = ifelse(is.na(oa_status), "unknown", oa_status)) %>% 
    arrange(Publication_Year, Publication_Type_DiVA, WoS_Journal, PID)
}

#' Public data from the Annual Bibliometric Monitoring project
#' 
#' This returns an object which contains data for the various higher 
#' organizational units at KTH
#' 
#' Data is cached in a local application directory by default and
#' is returned from there unless the parameter overwrite_cache is TRUE. 
#' To get fresh data cached, specify this flag.
#' 
#' @param overwrite_cache logical (by default FALSE) specifying whether 
#'   the cache should be refreshed
#' @return a list with three slots - "meta" for organizational unit metadata info,
#'   "units" with a named list of results (set of 5 different tibbles for each of the units)
#'   and "pt_ordning" for DiVA publication type sort order
#' @importFrom pool poolClose
#' @importFrom readr write_rds
#' @importFrom purrr map
#' @importFrom stats setNames
#' @export
#' @examples 
#' \dontrun{
#' 
#' # get all public data from the ABM
#' public <- abm_public_data()
#' 
#' # get public data specifically for KTH and table 1
#' unit_kth <- public \%>\% pluck("units", "KTH", "diva")
#' 
#' # get summary data for KTH
#' public \%>\% pluck("units", "KTH", "summaries")
#'  
#' # get public data specifically for KTH and table 1
#' unit_kth <- public \%>\% pluck("units", "KTH", "diva")
#' 
#' # get public data for the school "I" and all five tables
#' unit_i <- public \%>\% pluck("units", "I")
#' 
#' # get public data for the architecture institution, table 1
#' uc <- public \%>\% pluck("meta") \%>\% 
#'   filter(unit_long_en == "Architecture") \%>\% pull(unit_code)
#'   
#' public \%>\% pluck("units", uc, 1)
#' 
#' }   
abm_public_data <- function(overwrite_cache = FALSE) {
  
  cache_location <- file.path(
    rappdirs::app_dir("bibmon")$config(),
    "public.rds"
  )
  
  # if cache exists and shouldn't be overwritten, return it
  if (file.exists(cache_location) & !overwrite_cache)
    return (readr::read_rds(cache_location))  
  
  db <- pool_bib()
  
  # retrieve unit codes
  units_table <- 
    unit_info(con = db) %>%
    collect() 
  
  units <- 
    units_table %>%
    select(unit_code) %>% 
    pull(1)
  
  # retrieve sort order for DiVA publication types
  pubtype_order <-
    get_pubtype_order(con = db) %>%
    arrange(pt_ordning)

  indicator_descriptions <-
    get_indic_descriptions(con = db)
  
  # for a unit, retrieve all abm tables
  unit_tables <- function(x) {
    data <- abm_data(con = db, unit_code = x, pub_year = abm_config()$start_year:abm_config()$stop_year, analysis_id = abm_config()$analysis_id)
    tabs <- list(
      diva = abm_table1(data),
      wos_cit3y = abm_table2(data),
      wos_cf = abm_table3(data),
      wos_jcf = abm_table4(data),
      wos_copub = abm_table5(data),
      diva_full = abm_table1_full(data),
      coverage = abm_coverage(data),
      summaries = abm_dash_indices(data),
      oa = abm_table6(data),
      scop_cit = abm_table_scop_cit(data),
      scop_normcit = abm_table_scop_normcit(data),
      scop_snip = abm_table_scop_snip(data),
      scop_copub = abm_table_scop_copub(data)
    )
  }
  
  message("Patience, please. It takes a while to fetch the data into the cache.")
  res <- map(units, unit_tables)
  res <- setNames(res, units)
  
  poolClose(db)
  
  out <- list("meta" = units_table, "units" = res, "pubtype_order" = pubtype_order, "indicator_descriptions" = indicator_descriptions)
  
  message("Updating cached data for public data at: ", cache_location)
  readr::write_rds(out, cache_location) 
  
  return(out)  
}

#' Private data from the Annual Bibliometric Monitoring project
#' 
#' This returns an object which contains data for an individual researcher at KTH
#'  
#' @param unit_code the kthid for the researcher
#' @return a list with two slots - "meta" for organizational unit metadata info 
#'   and "units" with a named list of results (set of 5 different tibbles for 
#'   the tables and also the publication list).
#' @importFrom stats setNames
#' @importFrom pool poolClose
#' @export
#' @examples 
#' \dontrun{
#' 
#' # get all public data from the ABM
#' private <- abm_private_data('u1kzf1xh')
#' 
#' # get table 1 for the kthid
#' private \%>\% pluck('units', 'u1kzf1xh', 1)
#' 
#' # get publications for the kthid
#' private \%>\% pluck('units', 'u1kzf1xh', 'publications')
#' 
#' }   
abm_private_data <- function(unit_code) {
  
  if (missing(unit_code))
    stop("Please provide a kthid to be used as unit_code.")
  
  db <- pool_bib()
  
  # retrieve unit codes
  units_table <- 
    unit_info(con = db) %>%
    collect() %>%
    arrange(-desc(org_level)) 
  
  # for a kthid, retrieve all abm tables
  unit_tables <- function(x) {
    data <- abm_data(con = db, unit_code = x, pub_year = abm_config()$start_year:abm_config()$stop_year)
    tabs <- list(
      diva = abm_table1(data),
      wos_cit3y = abm_table2(data),
      wos_cf = abm_table3(data),
      wos_jcf = abm_table4(data),
      wos_copub = abm_table5(data),
      diva_full = abm_table1_full(data),
      coverage = abm_coverage(data),
      summaries = abm_dash_indices(data),
      oa = abm_table6(data),
      scop_cit = abm_table_scop_cit(data),
      scop_normcit = abm_table_scop_normcit(data),
      scop_snip = abm_table_scop_snip(data),
      scop_copub = abm_table_scop_copub(data),
      publications = abm_publications(data)
    )
  }
  
  res <- list(unit_tables(unit_code))
  res <- setNames(res, unit_code)
  
  poolClose(db)
  
  out <- list("meta" = units_table, "units" = res)
  
  return(out)  
}

#' Create graph over DiVA publication types by year
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom stats reorder
#' @export
abm_graph_diva <- function(df) {
  
  df_diva_long <- df %>%
    select(-"P_frac", -"WoS_coverage", -"Scopus_coverage") %>%
    gather("year", "value", -Publication_Type_DiVA) %>%
    left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  
  colvals <- unname(ktheme::palette_kth(13, type = "qual"))
  
  ggplot(data = df_diva_long,
         aes(x = year)) +
    geom_bar(aes(weight = value, fill = reorder(Publication_Type_DiVA, desc(pt_ordning)))) +
    labs(x = "Publication year",
         y = "Number of publications (fractional)",
         fill = NULL) +
    scale_fill_manual(values = colvals) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position = "right",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}


#' Create graph over WoS coverage by year
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom stats reorder
#' @importFrom scales percent_format
#' @export
abm_graph_wos_coverage <- function(df) {
  
  kth_cols <- palette_kth()
  
  df <- 
    df %>% 
    left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type")) %>% 
    filter(WoS_coverage != 0)
  
  ggplot(data = df,
    aes(x = reorder(Publication_Type_DiVA  %>% 
                     gsub(", ",",\n",.) %>% 
                     gsub(" \\(","\n(",.), WoS_coverage), 
       text = paste('coverage:', sprintf("%.1f", 100 * WoS_coverage), '%')
    )) +
  geom_bar(aes(weight = WoS_coverage), fill = kth_cols["blue"]) +
  xlab(NULL) +
  ylab("WoS coverage") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 5L), 
    breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme_kth_osc(axis_text_size = rel(1.1)) + #, ticks = TRUE) +
  theme(axis.text.y  = element_text(hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
}

#' Create graph over Cf by year, WoS
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @export
abm_graph_cf <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(2, ceiling(max(df$cf)))
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = cf, group=1)) +
    geom_point() + 
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Average Cf") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["lightblue"]) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over Top 10\% publications by year, Wos
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom scales percent
#' @export
abm_graph_top10 <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(0.2, ceiling(max(df$top10_share)*10)/10)
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top10_share, group=1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Share Top 10%") +
    geom_hline(yintercept = 0.1, color = kth_cols["lightblue"]) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over jcf by year, WoS
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @export
abm_graph_jcf <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(2, ceiling(max(df$jcf)))
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = jcf, group=1)) +
    geom_point() + 
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Average Journal Cf") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["lightblue"]) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over Top 20\% journals by year, WoS
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom scales percent
#' @export
abm_graph_top20 <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(0.4, ceiling(max(df$top20_share)*10)/10)
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top20_share, group=1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Share Journal Top 20%") +
    geom_hline(yintercept = 0.2, color = kth_cols["lightblue"]) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over international and Swedish non-university copublications by year, WoS
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @export
abm_graph_copub <- function(df){
  kth_cols <- as.vector(palette_kth(4))
  df_copub_long<- df %>%
    select(interval, nonuniv_share, int_share) %>% 
    rename("Swedish Non-university" = nonuniv_share,
           "International" = int_share) %>% 
    gather("Co-publication:", "value", -interval) %>% 
    filter(!interval == "Total")
  
  ggplot(data = df_copub_long,
         aes(x = interval, y = value, group = `Co-publication:`)) +
    geom_line(aes(color = `Co-publication:`), linetype = "dashed") +
    geom_point(aes(color = `Co-publication:`)) +
    xlab("Publication years") +
    ylab("Share of publications") +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_color_manual(values = kth_cols) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position="bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create waffle chart (5 rows, 20 columns) for any single percentage
#' 
#' @param pct a percentage expressed as a decimal number 0 <= pct <= 1
#' @param col a vector with colors for filling (optional)
#' @param label a title for the chart, displayed above the waffle (optional)
#'
#' @return a ggplot object
#' @import waffle
#' @importFrom ggplot2 theme guides element_blank
#' @import ktheme
#' @export
abm_waffle_pct <- function(pct, 
                           col = as.character(c(palette_kth()["blue"], "gray")), label = NULL){
  if(pct < 0.0 | pct > 1.0)
    stop("Please give a number between 0 and 1")
  yes <- round(100*pct)
  waffle(parts = c(yes, 100-yes),
         rows = 5,
         size = 1,
         colors = col,
         legend_pos = "none",
         title = label) +
    theme_kth_osc() + 
    theme(
      plot.title = element_text(size = 12),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank()) + 
    guides(fill = "none")
}

#' Create bullet graph with reference line
#'
#' @param label a label for the indicator, shown to the left of the gauge
#' @param value the value of the indicator, displayed as a horizontal wide line
#' @param reference a reference value displayed as a vertical thin line
#' @param roundto number of digits after the decimal point (default = 1)
#' @param pct boolean, set to TRUE if given value is a share (default = FALSE)
#' @return a ggplot object
#' @import ggplot2 ktheme
#' @export
abm_bullet <- function(label, value, reference, roundto = 1, pct = FALSE)
{
  if (pct) {
    value <- 100 * value
    reference <- 100 * reference
  }
  
  value <- round(value, roundto)
  
  title <- sprintf(paste0("%s = %.", roundto, "f%s"), 
                   label, value, ifelse(pct, "%", ""))
  
  blue <- tolower(palette_kth()["blue"])
  cerise <- tolower(palette_kth()["cerise"])
  
  ggplot(tibble(measure = label, target = reference, value = value)) +
    labs(title = title) +
    geom_bar(aes(x = measure, y = max(2 * target, ceiling(value))), 
             fill = "lightgray", stat = "identity", width = 0.7, alpha = 1) +
    geom_bar(aes(x = measure, y = value), 
             fill = blue,  stat = "identity", width = 0.4) +
    geom_errorbar(aes(x = measure, y = target, ymin = target, ymax = target), 
                  color = cerise, width = 0.9, size = 1.1) +
    coord_flip() +
    theme_kth_osc() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.05),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    )
}



#' Create pie chart for Open Access data
#' 
#' @param df a data frame at the format produced by abm_table6()
#' @return a pie chart object
#' @import ggplot2 dplyr plotrix
#' @importFrom graphics pie
#' @export
abm_graph_oadata_pie <- function(df){
  #unpaywall_cols <- c("#F9BC01", "#8D4EB4", "#20E168", "#CD7F32", "#BBBBBB")
  #kth_cols <- as.vector(palette_kth(4))
  
  unpaywall_colors <- data.frame("Gold"="#F9BC01",
                                 "Hybrid"="#8D4EB4",
                                 "Green"="#20E168",
                                 "Bronze"="#CD7F32",
                                 "Closed"="#BBBBBB") %>%
    rename("Not OA"="Closed")
  
  df_oa_graphdata <- df %>%
    filter(Publication_Year_ch == "Total") %>%
    select(gold_count, hybrid_count, green_count, bronze_count, closed_count) %>%
    rename("Gold" = gold_count,
           "Hybrid" = hybrid_count,
           "Green" = green_count,
           "Bronze" = bronze_count,
           "Not OA" = closed_count)
  
  percentages <- df %>%
    filter(Publication_Year_ch == "Total") %>%
    mutate(gold_count = 100*gold_count/P_tot,
           hybrid_count = 100*hybrid_count/P_tot,
           green_count = 100*green_count/P_tot,
           bronze_count = 100*bronze_count/P_tot,
           closed_count = 100*closed_count/P_tot) %>%
    select(gold_count, hybrid_count, green_count, bronze_count, closed_count) %>%
    t() %>%
    format(digits=2) %>%
    t()
  
  #Remove empty categories
  df_oa_graphdata <- df_oa_graphdata[,t(df_oa_graphdata)[,1]!=0]
  percentages <- percentages[,t(percentages)[,1] %>% as.numeric() != 0]
  unpaywall_colors <- unpaywall_colors %>%
    names(.) %in% names(df_oa_graphdata) %>%
    unpaywall_colors[.] %>%
    t()
  
  labls <- paste(names(df_oa_graphdata), "\n", percentages, " %", separator="")
  pie(t(df_oa_graphdata),  labels = c("","","","","",""), col = unpaywall_colors, cex = 0.8, radius = 0.8)
  pieangles <- floating.pie(x=t(df_oa_graphdata), col = unpaywall_colors)
  pie.labels(labels = labls, radius = 1.1, angles = pieangles, cex = 0.8)
}


#' Create stacked area graph for Open Access data
#' 
#' @param df a data frame at the format produced by abm_table6()
#' @return a ggplot object
#' @import ggplot2 dplyr reshape2 ktheme
#' @export
abm_graph_oadata_stackedarea <- function(df){
  unpaywall_cols <- c("#F9BC01", "#8D4EB4", "#20E168", "#CD7F32", "#BBBBBB")
  df_oa_graphdata <- df %>%
    filter(Publication_Year_ch != "Total") %>%
    select(Publication_Year_ch, gold_count, hybrid_count, green_count, bronze_count, closed_count) %>%
    rename("Gold" = gold_count, "Hybrid" = hybrid_count, "Green" = green_count, "Bronze" = bronze_count, "Not OA" = closed_count)
  
  xymelt <- melt(df_oa_graphdata, id.vars = "Publication_Year_ch") %>%
    rename("OA type:"=variable)
  
  ggplot(xymelt, aes(x = Publication_Year_ch, y = value, fill = `OA type:`, group = `OA type:`)) +
    scale_fill_manual(values=unpaywall_cols) + 
    geom_area() +
    #TODO: geom_line() +  ?
    xlab("Publication year") +
    ylab("Number of publications") +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position="bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over Scopus FWCI by years
#' 
#' @param df a data frame at the format produced by abm_table_scop_normcit()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @export
abm_graph_scop_normcit <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(2, ceiling(max(df$cf)))
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = fwci_x, group=1)) +
    geom_point() + 
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Average FWCI") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["lightblue"]) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over Top 10\% publications by year, Scopus
#' 
#' @param df a data frame at the format produced by abm_table_scop_normcit()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom scales percent
#' @export
abm_graph_scop_top10 <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(0.2, ceiling(max(df$top10_share)*10)/10)
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top10_share, group=1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Share Top 10%") +
    geom_hline(yintercept = 0.1, color = kth_cols["lightblue"]) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over SNIP by year, Scopus
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @export
abm_graph_scop_snip <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(2, ceiling(max(df$avg_snip)))
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = avg_snip, group=1)) +
    geom_point() + 
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Average SNIP") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["lightblue"]) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over Top 20\% journals by year, Scopus
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom scales percent
#' @export
abm_graph_scop_top20 <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(0.4, ceiling(max(df$top20_share)*10)/10)
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top20_share, group=1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab("Publication years") +
    ylab("Share Journal Top 20%") +
    geom_hline(yintercept = 0.2, color = kth_cols["lightblue"]) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create graph over international and corporate copublications by year, Scopus
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @export
abm_graph_scop_copub <- function(df){
  kth_cols <- as.vector(palette_kth(4))
  df_copub_long<- df %>%
    select(interval, corp_share, int_share) %>% 
    rename("Corporate" = corp_share,
           "International" = int_share) %>% 
    gather("Co-publication:", "value", -interval) %>% 
    filter(!interval == "Total")
  
  ggplot(data = df_copub_long,
         aes(x = interval, y = value, group = `Co-publication:`)) +
    geom_line(aes(color = `Co-publication:`), linetype = "dashed") +
    geom_point(aes(color = `Co-publication:`)) +
    xlab("Publication years") +
    ylab("Share of publications") +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_color_manual(values = kth_cols) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position="bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}
