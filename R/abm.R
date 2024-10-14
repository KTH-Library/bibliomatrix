#' Provide defaults for ABM analysis
#'
#'@return list of default values used in the current abm presentation
#'@export

abm_config <- function() {
  
  # this can later be expanded with more relevant defaults
  y_start <- 2014
  y_stop <- 2023
  analysisId <- 10
  
  if (Sys.getenv("ABM_START_YEAR") != "")
    y_start <- Sys.getenv("ABM_START_YEAR")
  
  if (Sys.getenv("ABM_STOP_YEAR") != "")
    y_stop <- Sys.getenv("ABM_STOP_YEAR")
  
  if (Sys.getenv("ABM_ANALYSIS_ID") != "")
    analysisId <- Sys.getenv("ABM_ANALYSIS_ID")
  
  list(
    start_year = y_start, 
    stop_year = y_stop,
    default_unit = "KTH",
    analysis_id = analysisId
  )
}

#' Retrieve data for ABM tables and graphs from master table
#' 
#' @param con connection to db, default is sqlite connection
#' @param unit_code for filtering on one or more unit code(s), which can be KTH, a one letter school code, an integer department code or a KTH-id (optional)
#' @param pub_year for filtering on publication years, for example 2012, 2012:2018 or c(2012, 2014, 2016) (optional)
#' @param unit_level for filtering on organizational level, 0 = KTH, 1 = school, 2 = deparment, 3 = researcher.
#' @param analysisId for filtering on specific analysis, for example 1 = KTH ABM 2020
#' @return tibble with all ABM data for selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export
abm_data <- function(con = con_bib(), unit_code, pub_year, unit_level, analysisId) {
  res <- con |> tbl("masterfile")
  if (!missing(analysisId))
    res <- res |> filter(analysis_id == analysisId)
  if (!missing(unit_code))
    res <- res |> filter(Unit_code %in% unit_code)
  if (!missing(pub_year))
    res <- res |> filter(Publication_Year %in% pub_year)
  if (!missing(unit_level))
    res <- res |> filter(level %in% unit_level)
  
  res |> collect()
}

#' Get order of publication type for ABM table 1
#' 
#' @param con connection to db - if no connection is given, use abm_public_kth data
#' @return tibble with pt_ordning and diva_publiation_type
#' @import DBI dplyr tidyr purrr
#' @export
get_pubtype_order <- function(con){
  if(!missing(con)){
    con |> tbl("Diva_publication_types") |> collect()
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
    con |> tbl("indicator_descriptions") |> collect()
  } else {
    abm_public_kth$indicator_descriptions
  }
}

#' Get date of indicator extraction for ABM
#' 
#' @param con connection to db - if no connection is given, use abm_public_kth data
#' @return date
#' @import dplyr
#' @export
get_analysis_date <- function(con) {
  if(!missing(con)){
    con |>
      tbl("analysis_info") |>
      collect() |>
      filter(analysis_id == abm_config()$analysis_id) |>
      pull(date_of_extraction) |>
      as.POSIXct() |>
      as.Date()
  } else {
    abm_public_kth$analysis_date
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
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) |>
    mutate(wos_bin = ifelse(!is.na(Doc_id), 1, 0),
           scop_bin = ifelse(!is.na(ScopusID), 1, 0))

  # Year dependent part of table
  table1 <-
    orgdata |>
    group_by(Publication_Year, Publication_Type_DiVA) |>
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE)) |>
    collect() |>
    arrange(Publication_Year) |>
    pivot_wider(names_from = Publication_Year, values_from = P_frac) |>
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata |>
    group_by(Publication_Type_DiVA) |>
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              WoS_coverage = sum(wos_bin * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              Scopus_coverage = sum(scop_bin * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) |>
    ungroup() |> 
    collect()
  
  
  
  table1 |>
    inner_join(table2, by = "Publication_Type_DiVA") |>
    inner_join(pubtype_order, by = c("Publication_Type_DiVA" = "diva_publication_type")) |>
    arrange(pt_ordning) |>
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
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) |>
    mutate(wos_bin = ifelse(!is.na(Doc_id), 1, 0),
           scop_bin = ifelse(!is.na(ScopusID), 1, 0))
  
  # Year dependent part of table
  table1 <-
    orgdata |>
    group_by(Publication_Year, Publication_Type_DiVA) |>
    summarise(P_full = n()) |>
    arrange(Publication_Year) |>
    pivot_wider(names_from = Publication_Year, values_from = P_full) |>
    ungroup()
  
  # Summary part of table
  table2 <-
    orgdata |>
    group_by(Publication_Type_DiVA) |>
    summarise(P_full = n(),
              WoS_coverage = mean(wos_bin, na.rm = TRUE),
              Scopus_coverage = mean(scop_bin, na.rm = TRUE)) |>
    ungroup()
  
  table1 |>
    inner_join(table2, by = "Publication_Type_DiVA") |>
    inner_join(pubtype_order, by = c("Publication_Type_DiVA" = "diva_publication_type")) |>
    arrange(pt_ordning) |>
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
  orgdata <- data |> 
    filter(Publication_Year >= analysis_start &
           Publication_Year <= analysis_stop - 2 &
           Publication_Type_WoS %in% c("Article", "Proceedings Paper", "Review", "Letter", "Editorial")) |>
    mutate(uncited = ifelse(Citations_3yr == 0, 1, 0)) |> 
    select(Publication_Year, Unit_Fraction, Citations_3yr, uncited, WebofScience_ID) |>
    unique()
    
  
  # Year dependent part of table
  table1 <-
    orgdata |>
    group_by(Publication_Year) |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              C3 = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              P_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) |>
    ungroup() |>
    mutate(Publication_Year = as.character(Publication_Year)) |>
    arrange(Publication_Year)
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              C3 = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              P_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited = sum(Unit_Fraction * uncited, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")
  
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
#' @param intervals set to TRUE to use 3 year intervals, default FALSE
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table3 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year, intervals = FALSE){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 1 &
             Publication_Type_WoS %in% c("Article", "Review") & 
             !is.na(cf)) |> 
    select(Publication_Year, Unit_Fraction_adj, cf, Ptop10, WebofScience_ID) |>
    mutate(Publication_Year = as.character(Publication_Year)) |> 
    unique()
  
  # Summary part of table
  table2 <-
    orgdata |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction_adj, na.rm = TRUE),
              cf = sum(cf * Unit_Fraction_adj, na.rm = TRUE) / sum(Unit_Fraction_adj, na.rm = TRUE),
              top10_count = sum(Ptop10 * Unit_Fraction_adj, na.rm = TRUE),
              top10_share = sum(Ptop10 * Unit_Fraction_adj, na.rm = TRUE) / sum(Unit_Fraction_adj, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")
  
  if(intervals) {
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
    orgdata <- orgdata |>
      inner_join(sliding_intervals(analysis_start, analysis_stop - 1, 3) |> mutate(x = as.character(x)),
                 by = c("Publication_Year" = "x")) |> 
      mutate(Publication_Year = interval)
    }
  
  # Year dependent part of table
  table1 <-
    orgdata |>
    group_by(Publication_Year) |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction_adj, na.rm = TRUE),
              cf = weighted.mean(cf, Unit_Fraction_adj, na.rm = TRUE),
              top10_count = sum(Ptop10*Unit_Fraction_adj, na.rm = TRUE),
              top10_share = weighted.mean(Ptop10, Unit_Fraction_adj, na.rm = TRUE)) |>
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  bind_rows(table1, table2)
}

#' Retrieve Table 4 (Journal impact) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @param intervals set to TRUE to use 3 year intervals, default FALSE
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table4 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year, intervals = FALSE){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_WoS %in% c("Article", "Review") &
             !is.na(jcf)) |> 
    select(Publication_Year, Unit_Fraction, jcf, Jtop20, WebofScience_ID) |>
    mutate(Publication_Year = as.character(Publication_Year)) |> 
    unique()
  
  # Summary part of table
  table2 <-
    orgdata |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")
  
  if(intervals) {
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
    orgdata <- orgdata |> 
      inner_join(sliding_intervals(analysis_start, analysis_stop, 3) |> mutate(x = as.character(x)),
                 by = c("Publication_Year" = "x")) |> 
      mutate(Publication_Year = interval)
  }
    
  # Year dependent part of table
  table1 <- orgdata |>
    group_by(Publication_Year) |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              jcf = weighted.mean(jcf, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(Jtop20, Unit_Fraction, na.rm = TRUE)) |>
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  bind_rows(table1, table2)
}

#' Retrieve Table 5 (Co-publishing) for ABM
#'
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @param intervals set to TRUE to use 3 year intervals, default FALSE
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table5 <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year, intervals = FALSE){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_WoS %in% c("Article", "Review") &
             !is.na(int)) |> 
    select(Publication_Year, Unit_Fraction, swe_nuniv, int, WebofScience_ID) |>
    mutate(Publication_Year = as.character(Publication_Year)) |> 
    unique()
  
  
  # Summary part of table
  table2 <- orgdata |>
    summarise(P_full = n(),
              nonuniv_count = sum(swe_nuniv, na.rm = TRUE),
              nonuniv_share = mean(swe_nuniv, na.rm = TRUE),
              int_count = sum(int, na.rm = TRUE),
              int_share = mean(int, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")

  if(intervals) {
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
    orgdata <- orgdata |>
      inner_join(sliding_intervals(analysis_start, analysis_stop, 3) |> mutate(x = as.character(x)),
                 by = c("Publication_Year" = "x")) |> 
      mutate(Publication_Year = interval)
  }
  
  # Year dependent part of table
  table1 <- orgdata |>
    group_by(Publication_Year) |>
    summarise(P_full = n(),
              nonuniv_count = sum(swe_nuniv, na.rm = TRUE),
              nonuniv_share = mean(swe_nuniv, na.rm = TRUE),
              int_count = sum(int, na.rm = TRUE),
              int_share = mean(int, na.rm = TRUE)) |>
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  bind_rows(table1, table2)
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
  table1 <- data |>
    filter((is_oa=="TRUE" | is_oa=="FALSE") &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) |>
    group_by(Publication_Year) |>
    summarise(P_tot=n(),
              oa_count=sum(as.logical(is_oa), na.rm=TRUE),
              diamond_count=sum(as.logical(oa_status=="diamond"), na.rm=TRUE),
              gold_count=sum(as.logical(oa_status=="gold"), na.rm=TRUE),
              hybrid_count=sum(as.logical(oa_status=="hybrid"), na.rm=TRUE),
              green_count=sum(as.logical(oa_status=="green"), na.rm=TRUE),
              closed_count=sum(as.logical(oa_status=="closed"), na.rm=TRUE),
              oa_share=mean(as.logical(is_oa), na.rm=TRUE)) |>
    ungroup() |>
    mutate(Publication_Year = as.character(Publication_Year)) |>
    select(Publication_Year, P_tot, oa_count, diamond_count, gold_count, hybrid_count, green_count, closed_count, oa_share)

  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  # Insert blank years
  table1 <- tibble(Publication_Year = as.character(analysis_start:analysis_stop)) |> 
    left_join(table1, by = "Publication_Year") |> 
    arrange(Publication_Year)
    
  # Summary part of table
  table2 <- table1 |>
    summarise(Publication_Year = "Total",
              P_tot = sum(P_tot, na.rm = TRUE),
              oa_count = sum(oa_count, na.rm = TRUE),
              diamond_count = sum(diamond_count, na.rm = TRUE),
              gold_count = sum(gold_count, na.rm = TRUE),
              hybrid_count = sum(hybrid_count, na.rm = TRUE),
              green_count = sum(green_count, na.rm = TRUE),
              closed_count = sum(closed_count, na.rm = TRUE),
              oa_share = oa_count/P_tot)
  
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
  orgdata <- data |> 
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Conference Paper", "Review", "Letter", "Editorial") &
             !is.na(scop_cscxo)) |>
    mutate(uncited = ifelse(scop_cscxo > 0, 0, 1)) |>
    select(Publication_Year, Unit_Fraction, scop_cscxo, uncited, ScopusID) |>
    unique()
  
  # Year dependent part of table
  table1 <-
    orgdata |>
    group_by(Publication_Year) |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              C_sum = sum(Unit_Fraction * scop_cscxo, na.rm = TRUE),
              C_avg = weighted.mean(scop_cscxo, Unit_Fraction, na.rm = TRUE),
              P_uncited_scop = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited_scop = weighted.mean(uncited, Unit_Fraction, na.rm = TRUE)) |>
    ungroup() |>
    mutate(Publication_Year = as.character(Publication_Year)) |>
    arrange(Publication_Year)
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)
  
  # Summary part of table
  table2 <-
    orgdata |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              C_sum = sum(Unit_Fraction * scop_cscxo, na.rm = TRUE),
              C_avg = weighted.mean(scop_cscxo, Unit_Fraction, na.rm = TRUE),
              P_uncited_scop = sum(Unit_Fraction * uncited, na.rm = TRUE),
              Share_uncited_scop = weighted.mean(uncited, Unit_Fraction, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")
  
  bind_rows(table1, table2)
}

#' Retrieve field normalized citations table (Scopus) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @param intervals set to TRUE to use 3 year intervals, default FALSE
#' @return tibble with field normalized citations and number/share of top10 publications by year or 3 year interval
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table_scop_normcit <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year, intervals = FALSE){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 1 &
             scop_doctype %in% c("Article", "Review", "Conference Paper") & 
             !is.na(scop_fwci_x)) |>
    select(Publication_Year, Unit_Fraction, scop_fwci_x, scop_Ptop10, ScopusID) |>
    mutate(Publication_Year = as.character(Publication_Year)) |> 
    unique()
  
  # Summary part of table
  table2 <-
    orgdata |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              fwci_x = weighted.mean(scop_fwci_x, Unit_Fraction, na.rm = TRUE),
              top10_count = sum(scop_Ptop10 * Unit_Fraction, na.rm = TRUE),
              top10_share = weighted.mean(scop_Ptop10, Unit_Fraction, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")
  
  if(intervals) {
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
    orgdata <- orgdata |>
      inner_join(sliding_intervals(analysis_start, analysis_stop - 1, 3) |> mutate(x = as.character(x)),
                 by = c("Publication_Year" = "x")) |> 
      mutate(Publication_Year = interval)
  }
  
  # Year dependent part of table
  table1 <- orgdata |>
    group_by(Publication_Year) |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              fwci_x = weighted.mean(scop_fwci_x, Unit_Fraction, na.rm = TRUE),
              top10_count = sum(scop_Ptop10 * Unit_Fraction, na.rm = TRUE),
              top10_share = weighted.mean(scop_Ptop10, Unit_Fraction, na.rm = TRUE)) |>
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  bind_rows(table1, table2)
}

#' Retrieve Journal impact table (Scopus) for ABM
#' 
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @param intervals set to TRUE to use 3 year intervals, default FALSE
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table_scop_snip <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year, intervals = FALSE){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Review", "Conference Paper") &
             !is.na(scop_snip)) |>
    select(Publication_Year, Unit_Fraction, scop_snip, scop_Jtop20, ScopusID) |>
    mutate(Publication_Year = as.character(Publication_Year)) |> 
    unique()
  
  
  # Summary part of table
  table2 <- orgdata |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              avg_snip = weighted.mean(scop_snip, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(scop_Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(scop_Jtop20, Unit_Fraction, na.rm = TRUE)) |> 
    mutate(Publication_Year = "Total")
  
  if(intervals) {
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
    orgdata <- orgdata |>
      inner_join(sliding_intervals(analysis_start, analysis_stop, 3) |> mutate(x = as.character(x)),
                 by = c("Publication_Year" = "x")) |> 
      mutate(Publication_Year = interval)
  }
  
  # Year dependent part of table
  table1 <- orgdata |>
    group_by(Publication_Year) |>
    summarise(pubs_full = n(),
              pubs_frac = sum(Unit_Fraction, na.rm = TRUE),
              avg_snip = weighted.mean(scop_snip, Unit_Fraction, na.rm = TRUE),
              top20_count = sum(scop_Jtop20*Unit_Fraction, na.rm = TRUE),
              top20_share = weighted.mean(scop_Jtop20, Unit_Fraction, na.rm = TRUE)) |>
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  bind_rows(table1, table2)
}

#' Retrieve Co-publishing table (Scopus) for ABM
#'
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @param intervals set to TRUE to use 3 year intervals, default FALSE
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table_scop_copub <- function(data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year, intervals = FALSE){
  
  # Get publication level data for selected unit, relevant Scopus doctypes only
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Review", "Conference Paper") &
             !is.na(scop_int)) |>
    select(Publication_Year, Unit_Fraction, scop_corp, scop_int, ScopusID) |>
    mutate(Publication_Year = as.character(Publication_Year)) |> 
    unique()
  
  # Summary part of table
  table2 <- orgdata |>
    summarise(P_full = n(),
              corp_count = sum(scop_corp, na.rm = TRUE),
              corp_share = mean(scop_corp, na.rm = TRUE),
              int_count = sum(scop_int, na.rm = TRUE),
              int_share = mean(scop_int, na.rm = TRUE)) |>
    mutate(Publication_Year = "Total")
  
  if(intervals) {
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
    orgdata <- orgdata |>
      inner_join(sliding_intervals(analysis_start, analysis_stop, 3) |> mutate(x = as.character(x)),
                 by = c("Publication_Year" = "x")) |> 
      mutate(Publication_Year = interval)
  }
  
  # Year dependent part of table
  table1 <- orgdata |>
    group_by(Publication_Year) |>
    summarise(P_full = n(),
              corp_count = sum(scop_corp, na.rm = TRUE),
              corp_share = mean(scop_corp, na.rm = TRUE),
              int_count = sum(scop_int, na.rm = TRUE),
              int_share = mean(scop_int, na.rm = TRUE)) |>
    ungroup()
  
  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  bind_rows(table1, table2)
}

#' Retrieve dashboard indicators for ABM
#' 
#' @param data dataset with publications as tibble
#' @return list with indicator values for dashboard startpage
#' @import DBI dplyr tidyr purrr
#' @export
abm_dash_indices <- function(data){
  
  if(nrow(data) > 0) {
    # Fetch table 1 for total number of publications and lastyear
    t1 <- abm_table1(data)
    lastyear <- max(as.integer(names(t1)[grep("[0-9]{4}", names(t1))]))
  
    # Fetch table 3 for cf and top10
    t3 <- abm_table3(data) |>
      filter(Publication_Year %in% (lastyear - 3):(lastyear - 1)) |>
      summarise(cf = weighted.mean(cf, pubs_frac, na.rm = TRUE),
                top10_share = weighted.mean(top10_share, pubs_frac, na.rm = TRUE))
  
    # Fetch table 4 for jcf and top20
    t4 <- abm_table4(data) |>
      filter(Publication_Year %in% (lastyear - 2):(lastyear)) |>
      summarise(jcf = weighted.mean(jcf, pubs_frac, na.rm = TRUE),
                top20_share = weighted.mean(top20_share, pubs_frac, na.rm = TRUE)) 
  
    # Fetch table 5 for non-univ and international copublications
    t5 <- abm_table5(data) |>
      filter(Publication_Year %in% (lastyear - 2):(lastyear)) |>
      summarise(nonuniv_share = weighted.mean(nonuniv_share, P_full, na.rm = TRUE),
                int_share = weighted.mean(int_share, P_full, na.rm = TRUE)) 
                  
  
    list(tot_pubs_frac = sum(t1[, as.character(lastyear)], na.rm = TRUE),
         cf = t3$cf,
         top10_share = t3$top10_share,
         jcf = t4$jcf,
         top20_share = t4$top20_share,
         copub_nonuniv = t5$nonuniv_share,
         copub_internat = t5$int_share)
  } else {
    list(tot_pubs_frac = numeric(0),
         cf = numeric(0),
         top10_share = numeric(0),
         jcf = numeric(0),
         top20_share = numeric(0),
         copub_nonuniv = numeric(0),
         copub_internat = numeric(0))
  }
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
  orgdata <- data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_DiVA %in% c("Article, peer review", "Conference paper, peer review")) |>
    mutate(wos_bin = ifelse(!is.na(Doc_id), 1, 0),
           scop_bin = ifelse(!is.na(ScopusID), 1, 0)) |> 
    select(Publication_Year, Publication_Type_DiVA, Unit_Fraction, wos_bin, scop_bin) |>
    group_by(Publication_Year, Publication_Type_DiVA) |>
    summarise(p_frac = sum(Unit_Fraction, na.rm = TRUE),
              p_full = n(),
              sumwos_frac = sum(Unit_Fraction * wos_bin, na.rm = TRUE),
              sumwos_full = sum(wos_bin, na.rm = TRUE),
              woscov_frac = sum(Unit_Fraction * wos_bin, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              woscov_full = sum(wos_bin, na.rm = TRUE) / n(),
              sumscop_frac = sum(Unit_Fraction * scop_bin, na.rm = TRUE),
              sumscop_full = sum(scop_bin, na.rm = TRUE),
              scopcov_frac = sum(Unit_Fraction * scop_bin, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              scopcov_full = sum(scop_bin, na.rm = TRUE) / n()) |>
    ungroup() |>
    collect()
  
  peerreviewed <- orgdata |>
    group_by(Publication_Year) |>
    summarise(p_frac = sum(p_frac),
              p_full = sum(p_full),
              sumwos_frac = sum(sumwos_frac),
              sumwos_full = sum(sumwos_full),
              sumscop_frac = sum(sumscop_frac),
              sumscop_full = sum(sumscop_full)) |>
    mutate(woscov_frac = sumwos_frac / p_frac,
           woscov_full = sumwos_full / p_full,
           scopcov_frac = sumscop_frac / p_frac,
           scopcov_full = sumscop_full / p_full,
           Publication_Type = "Peer reviewed")
  
  orgdata |>
    rename(Publication_Type = Publication_Type_DiVA) |>
    bind_rows(peerreviewed) |>
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
  
  levels <- unique(workdf$level) |> sort()
  
  # Full sortname for first level is just sortfield
  res <- workdf |> filter(level == levels[1])
  res$fullsort <- res$sortfield
  
  # For each subsequent level, fetch fullsort from parent and add sortfield to get unit's fullsort, then add to res
  for(lvl in levels[-1]){
    lvlres <- workdf |>
      filter(level == lvl) |>
      select(id, parent, sortfield) |>
      inner_join(res |> select(id, fullsort), by = c("parent" = "id")) |>
      mutate(fullsort = paste0(fullsort, sortfield))
    res <- bind_rows(res, lvlres)
  }
  
  res <- select(res, id, fullsort)
  
  names(res)[1] <- idfield
  
  res |>
    arrange(fullsort) |>
    select(-fullsort) |>
    mutate(sort_order = row_number())
}

#' Retrieve information about ABM units (level 0-2) from database or from package data
#' 
#' If a database connection is given, abm_org_info is read from database,
#' otherwise abm_public_kth$meta is returned
#' 
#' @param con connection to db
#' @param analysisId id for analysis of interest, default from abm_config()
#'
#' @return tibble with information about ABM units
#' @import DBI dplyr
#' @importFrom stringr str_pad
#' @export

unit_info <- function(con, analysisId = abm_config()$analysis_id){
  
  if(missing(con)){
    abm_public_kth$meta 
  } else {
    abm_units <- con |> tbl("abm_org_info") |> collect() |> filter(analysis_id == analysisId) |> select(-"sort_order")
    
    abm_units |>
      # Get full sort order
      inner_join(hiersort(abm_units, "Diva_org_id", "org_level", "parent_org_id", "unit_long_en"), by = "Diva_org_id") |>
      # Add indented versions of unit_long_en, one with plain white space and one for usage in html where leading white space gets sanitized
      mutate(unit_long_en_indent1 = str_pad(unit_long_en, side = "left", width = 4*org_level + stringr::str_length(unit_long_en)),
             unit_long_en_indent2 = str_pad(unit_long_en, side = "left", width = 4*org_level + stringr::str_length(unit_long_en), pad = "\U00A0")) |>
      arrange(sort_order)
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
  data |>
    select(-any_of(c("analysis_id",
                     "is_kth",
                     "Doc_id",
                     "w_subj",
                     "level"))) |>
    relocate(c(DOI, Publication_Year, Title, Bibliographic_Information, n_authors, Publication_Type_DiVA), .after = PID) |>
    relocate(Unit_Fraction, .before = Unit_Fraction_adj) |> 
    relocate(Ptop5, .after = Ptop1) |>
    relocate(Cf_log, .after = cf) |> 
    filter(Publication_Year %in% analysis_start:analysis_stop) |>
    mutate(oa_status = ifelse(is.na(oa_status), "unknown", oa_status)) |> 
    arrange(Publication_Year, Publication_Type_DiVA, WoS_Journal, PID)
}

#' Public data from the Annual Bibliometric Monitoring project
#' 
#' This returns an object which contains data for organizational units at KTH
#' and some metadata used in the ABM
#' 
#' Data is cached in a local application directory by default and
#' is returned from there unless the parameter overwrite_cache is TRUE. 
#' To get fresh data cached, specify this flag.
#' 
#' @param overwrite_cache logical (by default FALSE) specifying whether 
#'   the cache should be refreshed
#' @return a list with four slots - "meta" for organizational unit metadata info,
#'   "units" with a named list of results (set of tibbles for each of the units),
#'   "pt_ordning" for DiVA publication type sort order
#'   and analysis_date for the date of data extraction
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
#' unit_kth <- public |> pluck("units", "KTH", "diva")
#'  
#' # get public data specifically for KTH and table 1
#' unit_kth <- public |> pluck("units", "KTH", "diva")
#' 
#' # get public data for the school "I" and all five tables
#' unit_i <- public |> pluck("units", "I")
#' 
#' # get public data for the architecture institution, table 1
#' uc <- public |> pluck("meta") |> 
#'   filter(unit_long_en == "Architecture") |> pull(unit_code)
#'   
#' public |> pluck("units", uc, 1)
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
    unit_info(con = db) |>
    collect() 
  
  units <- 
    units_table |>
    select(unit_code) |> 
    pull(1)
  
  # retrieve sort order for DiVA publication types
  pubtype_order <-
    get_pubtype_order(con = db) |>
    arrange(pt_ordning)

  indicator_descriptions <-
    get_indic_descriptions(con = db)
  
  analysis_date <-
    get_analysis_date(con = db)
  
  # for a unit, retrieve all abm tables
  unit_tables <- function(x) {
    
    data <- abm_data(con = db, unit_code = x, 
                     pub_year = abm_config()$start_year:abm_config()$stop_year, 
                     analysisId = abm_config()$analysis_id)
    
    unit_level <- units_table |> filter(unit_code == x) |> pull(org_level)
    
    tabs <- list(
      diva = abm_table1(data, db),
      wos_cit3y = abm_table2(data),
      wos_cf = abm_table3(data),
      wos_jcf = abm_table4(data),
      wos_copub = abm_table5(data),
      wos_copub_countries = abm_copub_countries(con = db,
                                                unit_level = unit_level,
                                                unit_code = x,
                                                analysisId = abm_config()$analysis_id,
                                                analysis_start = abm_config()$start_year,
                                                analysis_stop = abm_config()$stop_year),
      wos_copub_orgs = abm_copub_orgs(con = db,
                                      unit_level = unit_level,
                                      unit_code = x,
                                      analysisId = abm_config()$analysis_id,
                                      analysis_start = abm_config()$start_year,
                                      analysis_stop = abm_config()$stop_year),
      diva_full = abm_table1_full(data, db),
      coverage = abm_coverage(data),
      summaries = abm_dash_indices(data),
      oa = abm_table6(data),
      scop_cit = abm_table_scop_cit(data),
      scop_normcit = abm_table_scop_normcit(data),
      scop_snip = abm_table_scop_snip(data),
      scop_copub = abm_table_scop_copub(data),
      scop_sdg_year = abm_sdg_year(data, db),
      scop_sdg_table = abm_sdg_table(data, db)
    )
  }
  
  message("Patience, please. It takes a while to fetch the data into the cache.")
  res <- map(units, unit_tables)
  res <- setNames(res, units)
  
  poolClose(db)
  
  out <- list("meta" = units_table,
              "units" = res,
              "pubtype_order" = pubtype_order,
              "indicator_descriptions" = indicator_descriptions,
              "analysis_date" = analysis_date)
  
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
#' private |> pluck('units', 'u1kzf1xh', 1)
#' 
#' # get publications for the kthid
#' private |> pluck('units', 'u1kzf1xh', 'publications')
#' 
#' }   
abm_private_data <- function(unit_code) {
  
  if (missing(unit_code))
    stop("Please provide a kthid to be used as unit_code.")
  
  db <- pool_bib()
  
  # retrieve unit codes
  units_table <- 
    unit_info() |>
    collect() |>
    arrange(-desc(org_level)) 
  
  # for a kthid, retrieve all abm tables
  unit_tables <- function(x) {
    data <- abm_data(con = db,
                     unit_code = x,
                     pub_year = abm_config()$start_year:abm_config()$stop_year,
                     analysisId = abm_config()$analysis_id)
    tabs <- list(
      diva = abm_table1(data, db),
      wos_cit3y = abm_table2(data),
      wos_cf = abm_table3(data),
      wos_jcf = abm_table4(data),
      wos_copub = abm_table5(data),
      diva_full = abm_table1_full(data, db),
      coverage = abm_coverage(data),
      summaries = abm_dash_indices(data),
      oa = abm_table6(data),
      scop_cit = abm_table_scop_cit(data),
      scop_normcit = abm_table_scop_normcit(data),
      scop_snip = abm_table_scop_snip(data),
      scop_copub = abm_table_scop_copub(data),
      scop_sdg_year = abm_sdg_year(data, db),
      scop_sdg_table = abm_sdg_table(data, db),
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
  
  df_diva_long <- df |>
    select(-"P_frac", -"WoS_coverage", -"Scopus_coverage") |>
    gather("year", "value", -Publication_Type_DiVA) |>
    left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  
  colvals <- unname(palette_kth_neo(13, type = "qual"))
  names(colvals) <- abm_public_kth$pubtype_order |> filter(pt_ordning <= 13) |> pull(diva_publication_type)

  ggplot(data = df_diva_long,
         aes(x = year)) +
    geom_bar(aes(weight = value, fill = reorder(Publication_Type_DiVA, desc(pt_ordning)))) +
    labs(x = "Publication year",
         y = "Number of publications (fractional)",
         fill = NULL) +
    scale_fill_manual(values = colvals) +
    theme_kth_neo() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position = "right",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}


#' Create graph over WoS coverage by DiVA publication type
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom stats reorder
#' @importFrom scales percent_format
#' @export
abm_graph_wos_coverage <- function(df) {
  
  kth_cols <- palette_kth_neo()
  
  df <- df |> 
    left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type")) |> 
    filter(WoS_coverage != 0) |> 
    mutate(Publication_Type_DiVA = gsub(" \\(", "\n(", gsub(", ", ",\n", Publication_Type_DiVA)))
  
  ggplot(data = df,
    aes(x = reorder(Publication_Type_DiVA, WoS_coverage), 
       text = paste('coverage:', sprintf("%.1f", 100 * WoS_coverage), '%')
    )) +
  geom_bar(aes(weight = WoS_coverage), fill = kth_cols["blue"]) +
  xlab(NULL) +
  ylab("WoS coverage") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 5L), 
    breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme_kth_neo(axis_text_size = rel(1.1)) + #, ticks = TRUE) +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(2, ceiling(max(df$cf)))
  
  df<- df |> filter(Publication_Year != "Total") |> mutate(ma3 = rollmean(cf, k = 3, na.pad =TRUE))
  
  ggplot(data = df,
         aes(x = Publication_Year, y = cf, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    #geom_line(color = kth_cols["blue2"], size = .8) +
    #geom_ma(ma_fun = SMA, n = 3, size = 3, color = kth_cols["blue2"]) +
    geom_line(aes(y=ma3), color = kth_cols["blue2"], size = 1) +
    xlab("Publication year") +
    ylab("Average Cf") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["blue3"], size = .8) +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(0.2, ceiling(max(df$top10_share)*10)/10)
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = top10_share, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication year") +
    ylab("Share Top 10%") +
    geom_hline(yintercept = 0.1, color = kth_cols["blue3"], size = .8) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(2, ceiling(max(df$jcf)))
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = jcf, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication year") +
    ylab("Average Journal Cf") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["blue3"], size = .8) +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(0.4, ceiling(max(df$top20_share)*10)/10)
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = top20_share, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication year") +
    ylab("Share Journal Top 20%") +
    geom_hline(yintercept = 0.2, color = kth_cols["blue3"], size = .8) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_neo() +
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
  kth_cols <- as.vector(palette_kth_neo(n = 5, type = "seq"))
  df_copub_long<- df |>
    select(Publication_Year, nonuniv_share, int_share) |> 
    rename("Swedish Non-university" = nonuniv_share,
           "International" = int_share) |> 
    gather("Co-publication:", "value", -Publication_Year) |> 
    filter(!Publication_Year == "Total")
  
  ggplot(data = df_copub_long,
         aes(x = Publication_Year, y = value, group = `Co-publication:`)) +
    geom_line(aes(color = `Co-publication:`), size = .8) +
    geom_point(aes(color = `Co-publication:`), size = 3) +
    xlab("Publication year") +
    ylab("Share of publications") +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_color_manual(values = kth_cols[c(1,3)]) +
    theme_kth_neo() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position="bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Create line graph over share of publications by OA type and year
#' 
#' @param df a data frame at the format produced by abm_table6()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme 
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace
#' @importFrom scales percent_format
#' @export
abm_graph_oa_lines <- function(df){
  
  colors_df <- unpaywall_colors() |>
    bind_rows(data.frame(oa_status = "Open Access, all",
                         oa_color = kth_colors("blue")))
  unpaywall <- colors_df$oa_color
  names(unpaywall) <- colors_df$oa_status 

  graph_df <- df |>
    filter(Publication_Year != 'Total') |>
    mutate(Year = as.integer(Publication_Year)) |> 
    mutate(across(ends_with("count"), function(x) x/P_tot)) |> 
    rename_with(.cols = ends_with("count"), ~stringr::str_replace(., "_count", "")) |> 
    pivot_longer(cols = c("oa", "diamond", "gold", "hybrid", "green"),
                 values_to = "share") |> 
    mutate(oa_type = case_match(name,
                                "oa" ~ "Open Access, all",
                                "diamond" ~ "Diamond",
                                "gold" ~ "Gold",
                                "green" ~ "Green",
                                "hybrid" ~ "Hybrid",
                                .default = NA),
           lw = if_else(name == 'oa', 1.5, 0.9),
           sz = if_else(name == 'oa', 3, 2),
           a = if_else(name == 'oa', 1, .7))
  
  xbreaks <- graph_df |> pull(Year) |> unique() |> sort()
  ybreaks <- seq(0, ceiling(10*max(graph_df$share, na.rm = TRUE))/10, .1)
  
  ggplot(data = graph_df,
         aes(x = Year, y = share, color = oa_type, linewidth = lw, size = sz, alpha = a)) +
    geom_line() +
    geom_point() +
    scale_linewidth_identity() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_manual(
      name = 'OA type',
      values = unpaywall) +
    scale_x_continuous(breaks = xbreaks) +
    scale_y_continuous(breaks = ybreaks,
                       labels = percent_format()) +
    theme_kth_neo() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position="bottom") +
    ylab("Share of publications") + 
    xlab("Publication year")
}

#' Create waffle chart (5 rows, 20 columns) for any single percentage
#' 
#' @param pct a percentage expressed as a decimal number 0 <= pct <= 1
#' @param label a title for the chart, displayed above the waffle (optional)
#' @param col a vector with colors for filling (optional)
#'
#' @return a ggplot object
#' @import waffle
#' @importFrom ggplot2 theme guides element_blank
#' @import ktheme
#' @export
abm_waffle_pct <- function(pct, label, col) {
  
  if(missing(col)){
    col <- palette_kth_neo(n = 7, type = "div")[c(2,4)] |> unname()
  }
    
  if(pct < 0.0 | pct > 1.0)
    stop("Please give a number between 0 and 1")
  yes <- round(100*pct)
  waffle(parts = c(yes, 100-yes),
         rows = 5,
         size = 1,
         colors = col,
         legend_pos = "none",
         title = label) +
    theme_kth_neo() + 
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
  
  cols <- palette_kth_neo(n = 7, type = "div")

  ggplot(tibble(measure = label, target = reference, value = value)) +
    labs(title = title) +
    geom_bar(aes(x = measure, y = max(2 * target, ceiling(value))), 
             fill = cols["M"], stat = "identity", width = 0.7, alpha = 1) +
    geom_bar(aes(x = measure, y = value), 
             fill = cols["H2"],  stat = "identity", width = 0.4) +
    geom_errorbar(aes(x = measure, y = target, ymin = target, ymax = target), 
                  color = cols["L2"], width = 0.9, size = 1.1) +
    coord_flip() +
    theme_kth_neo() +
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
#' @importFrom ktheme unpaywall_colors
#' @export
abm_graph_oadata_pie <- function(df){
  
  df_oa_graphdata <- df |>
    filter(Publication_Year == "Total") |>
    select(diamond_count, gold_count, hybrid_count, green_count, closed_count) |>
    rename("Diamond" = diamond_count,
           "Gold" = gold_count,
           "Hybrid" = hybrid_count,
           "Green" = green_count,
           "Not OA" = closed_count)
  
  percentages <- df |>
    filter(Publication_Year == "Total") |>
    mutate(diamond_count = 100*diamond_count/P_tot,
           gold_count = 100*gold_count/P_tot,
           hybrid_count = 100*hybrid_count/P_tot,
           green_count = 100*green_count/P_tot,
           closed_count = 100*closed_count/P_tot) |>
    select(diamond_count, gold_count, hybrid_count, green_count, closed_count) |>
    t() |>
    format(digits=2) |>
    t()
  
  #Remove empty categories
  df_oa_graphdata <- df_oa_graphdata[,t(df_oa_graphdata)[,1]!=0]
  percentages <- percentages[,t(percentages)[,1] |> as.numeric() != 0]
  unpaywall_cols <- unpaywall_colors() |>
    filter(oa_status %in% names(df_oa_graphdata)) |>
    pull(oa_color)
  
  labls <- paste(names(df_oa_graphdata), "\n", percentages, " %", separator="")
  pie(t(df_oa_graphdata),  labels = c("","","","","",""), col = unpaywall_cols, cex = 0.8, radius = 0.8)
  pieangles <- floating.pie(x=t(df_oa_graphdata), col = unpaywall_cols)
  pie.labels(labels = labls, radius = 1.1, angles = pieangles, cex = 0.8)
}


#' Create stacked area graph for Open Access data
#' 
#' @param df a data frame at the format produced by abm_table6()
#' @return a ggplot object
#' @import ggplot2 dplyr reshape2 ktheme
#' @export
abm_graph_oadata_stackedarea <- function(df){
 
  unpaywall_cols <- unpaywall_colors() |> pull(oa_color)

  df_oa_graphdata <- df |>
    filter(Publication_Year != "Total") |>
    select(Publication_Year, diamond_count, gold_count, hybrid_count, green_count, closed_count) |>
    rename("Diamond" = diamond_count, "Gold" = gold_count, "Hybrid" = hybrid_count, "Green" = green_count, "Not OA" = closed_count)
  
  xymelt <- melt(df_oa_graphdata, id.vars = "Publication_Year") |>
    rename("OA type:"=variable)
  
  ggplot(xymelt, aes(x = Publication_Year, y = value, fill = `OA type:`, group = `OA type:`)) +
    scale_fill_manual(values = unpaywall_cols) + 
    geom_area() + 
    #TODO: geom_line() +  ?
    xlab("Publication year") +
    ylab("Number of publications") +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(2, ceiling(max(df$fwci_x)))
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = fwci_x, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication years") +
    ylab("Average FWCI") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["blue3"], size = .8) +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(0.2, ceiling(max(df$top10_share)*10)/10)
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = top10_share, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication years") +
    ylab("Share Top 10%") +
    geom_hline(yintercept = 0.1, color = kth_cols["blue3"], size = .8) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(2, ceiling(max(df$avg_snip)))
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = avg_snip, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication years") +
    ylab("Average SNIP") +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["blue3"], size = .8) +
    theme_kth_neo() +
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
  kth_cols <- palette_kth_neo(n = 5, type = "seq")
  ymax <- max(0.4, ceiling(max(df$top20_share)*10)/10)
  
  ggplot(data = df |> filter(!Publication_Year == "Total"),
         aes(x = Publication_Year, y = top20_share, group=1)) +
    geom_point(color = kth_cols["blue1"], size = 3) + 
    geom_line(color = kth_cols["blue2"], size = .8) +
    xlab("Publication years") +
    ylab("Share Journal Top 20%") +
    geom_hline(yintercept = 0.2, color = kth_cols["blue3"], size = .8) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax)) +
    theme_kth_neo() +
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
  kth_cols <- as.vector(palette_kth_neo(n = 5, type = "seq"))
  df_copub_long<- df |>
    select(Publication_Year, corp_share, int_share) |> 
    rename("Corporate" = corp_share,
           "International" = int_share) |> 
    gather("Co-publication:", "value", -Publication_Year) |> 
    filter(!Publication_Year == "Total")
  
  ggplot(data = df_copub_long,
         aes(x = Publication_Year, y = value, group = `Co-publication:`)) +
    geom_line(aes(color = `Co-publication:`), size = .8) +
    geom_point(aes(color = `Co-publication:`), size = 3) +
    xlab("Publication year") +
    ylab("Share of publications") +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_color_manual(values = kth_cols[c(1,3)]) +
    theme_kth_neo() +
    theme(axis.title.y = element_text(vjust = 2.5),
          legend.position="bottom",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

#' Retrieve co-publishing organizations for ABM tables
#' 
#' This function retrieves all co-publishing organizations for the selected ABM-unit, for all
#' publications that has a UT-number (WebofScience_id). The returned tibble has one row per organization 
#' and publication. 
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code for filtering on one or more unit code(s), which can be KTH, a one letter school code, an integer department code or a KTH-id (optional)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with co-publishing organizations associated with each publication for the selected ABM-organization
#' @import DBI dplyr tidyr purrr
#' @export
abm_copub_data <- function(con = con_bib(), unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {
  oa_data <- abm_data(con = con, unit_code = unit_code) |> 
    rename("UT" = "WebofScience_ID") |>
    left_join(con |> tbl("Bestresaddr_KTH"), by = "UT") |>  #by = c("WebofScience_ID" = "UT")
    filter(!is.na(UT)) |>
    select("UT","Name_eng","Country_name","Org_type_code", "Unified_org_id")
}

#' Create table over co-publication countries for ABM unit
#' 
#' @param con a database connection to BIBMON
#' @param analysisId id for the analysis, default from abm_config()
#' @param unit_level organization level
#' @param unit_code code for the analyzed unit
#' @param exclude_swe wether to exclude Sweden as co-publication country, default TRUE
#' @param limit if set, limit the result to the first limit rows, default NULL
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return a tibble
#' @import dplyr
#' @importFrom utils head
#' @export
abm_copub_countries <- function(con,
                                analysisId = abm_config()$analysis_id,
                                unit_level,
                                unit_code,
                                exclude_swe = TRUE,
                                limit = NULL,
                                analysis_start = abm_config()$start_year,
                                analysis_stop = abm_config()$stop_year){
  
  countries <- con |>
    tbl("abm_copub_entities") |> 
    filter(analysis_id == analysisId &
             level == unit_level &
             Unit_code == unit_code &
             entity == "Country" &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) |> 
    group_by(country) |>
    summarise(p = sum(p, na.rm = TRUE),
              p_10 = sum(p_10, na.rm = TRUE),
              p_50 = sum(p_50, na.rm = TRUE),
              p_200 = sum(p_200, na.rm = TRUE),
              p_over200 = sum(p_over200, na.rm = TRUE),
              kth_frac = sum(kth_frac, na.rm = TRUE)) |>
    ungroup() |> 
    collect() |> 
    arrange(-kth_frac)
  
  if(exclude_swe == TRUE)
    countries <- countries |> filter(country != "Sweden")
  
  if(!is.null(limit))
    countries <- head(countries, limit)
  
  countries
}

#' Create table over co-publication countries for ABM unit
#' 
#' @param con a database connection to BIBMON
#' @param analysisId id for the analysis, default from abm_config()
#' @param unit_level organization level
#' @param unit_code code for the analyzed unit
#' @param exclude_swe wether to exclude Swedish co-publication orgs, default FALSE
#' @param limit if set, limit the result (for level 0 and 1) to the first limit rows, default 1000
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return a tibble
#' @import dplyr
#' @importFrom utils head
#' @export
abm_copub_orgs <- function(con,
                           analysisId = abm_config()$analysis_id,
                           unit_level,
                           unit_code,
                           exclude_swe = FALSE,
                           limit = 1000,
                           analysis_start = abm_config()$start_year,
                           analysis_stop = abm_config()$stop_year){
  
  orgs <- con |>
    tbl("abm_copub_entities") |> 
    filter(analysis_id == analysisId &
             level == unit_level &
             Unit_code == unit_code &
             entity == "Organization" &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) |> 
    group_by(org, org_type, unified_org_id, country) |>
    summarise(p = sum(p, na.rm = TRUE),
              p_10 = sum(p_10, na.rm = TRUE),
              p_50 = sum(p_50, na.rm = TRUE),
              p_200 = sum(p_200, na.rm = TRUE),
              p_over200 = sum(p_over200, na.rm = TRUE),
              kth_frac = sum(kth_frac, na.rm = TRUE)) |>
    ungroup() |> 
    collect() |> 
    arrange(-kth_frac)
  
  if(exclude_swe == TRUE)
    orgs <- orgs |> filter(country != "Sweden")
  
  if(!is.null(limit) & unit_level <= 1)
    orgs <- head(orgs, limit)
  
  orgs
}


#' Calculate average citation indicators across years
#' 
#' This function calculates average indicator values (jcf, cf etc) across a set of years per department, using fractional counting.
#' Data is based on masterfile, using a specific analysis_id (i.e. data version nr.)
#' This three-year average of jcf is used as an performance indicator at KTH. 
#' 
#' @param con connection to db
#' @param starty first publication year of analysis
#' @param stopy last publication year of analysis
#' @param analysis_level organization analysis level. Default is 2 (department).
#' @param analysis_version_id the analysis_id id to be used from masterfile
#' @return tibble average citation indicators along with dept name, school name, along with full and fractional publ. counts.
#' @import DBI dplyr
#' @export
mean_indicator_units <- function(con,starty,stopy, analysis_level=2, analysis_version_id){
  
  Cf_log <- Ptop5 <- Ptop25 <- NULL

  dept_wos<- con |> tbl("masterfile") |> 
            filter(analysis_id == analysis_version_id, level == analysis_level, between(Publication_Year,starty,stopy), !is.na(Doc_id)) |>
            collect()
  
  dept_wos_unique<- dept_wos |> distinct(Unit_code, Doc_id, .keep_all=TRUE) 
  
  jcf_av<- dept_wos_unique |> filter(!is.na(jcf)) |> 
              group_by(Unit_Name, Unit_code) |> 
              summarise(jcf_frac = sum(Unit_Fraction * jcf, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                Jtop20_frac = sum(Unit_Fraction * Jtop20, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                P_frac_jcf = sum(Unit_Fraction, na.rm = TRUE),
                P_full_jcf = n()) |> 
              ungroup() |> arrange(desc(jcf_frac))
  
  cf_av<- dept_wos_unique |> filter(!is.na(cf)) |> 
              group_by(Unit_Name, Unit_code) |> 
              summarise(cf_frac = sum(Unit_Fraction * cf, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                  log_cf_frac = sum(Unit_Fraction * Cf_log, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                  cf_full = mean(cf,na.rm = TRUE),
                  top5_frac = sum(Unit_Fraction * Ptop5, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                  top10_frac = sum(Unit_Fraction * Ptop10, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                  top25_frac = sum(Unit_Fraction * Ptop25, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
                  P_frac_cf = sum(Unit_Fraction, na.rm = TRUE),
                  P_full_cf = n()) |> 
              ungroup() 
  
  oa_av<- dept_wos_unique |> filter(!is.na(is_oa)) |> 
              group_by(Unit_Name, Unit_code) |> 
              summarise(oa_share=mean(as.logical(is_oa), na.rm=TRUE),
              P_full_OA = n()) |> 
          ungroup()
  
  indicator_final<- jcf_av |> left_join(unit_info(con = con, analysisId = analysis_version_id) |> select(unit_code, parent_org_id), by=c("Unit_code" = "unit_code")) |> 
    left_join(cf_av |> select(-Unit_Name), by=c("Unit_code" = "Unit_code")) |>
    left_join(oa_av |> select(-Unit_Name), by=c("Unit_code" = "Unit_code")) |>
    left_join(unit_info(con = con, analysisId = analysis_version_id) |> select(Diva_org_id, unit_long_en), by=c("parent_org_id" = "Diva_org_id")) |> #to join in school name
    select(-parent_org_id) |> relocate(unit_long_en) |> 
    rename("Parent name" = unit_long_en)
  
  indicator_final$starty<- starty
  indicator_final$stopy<- stopy
  
  indicator_final
}


#' Create table over SDGs for the selected unit
#'
#' @param data dataset with publications as tibble
#' @param con a database connection to BIBMON
#' @param analysisId id for the analysis, default from abm_config()
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return a tibble
#' @import dplyr
#' @importFrom stringr str_pad
#' @export
abm_sdg_table <- function(data,
                          con,
                          analysisId = abm_config()$analysis_id,
                          analysis_start = abm_config()$start_year,
                          analysis_stop = abm_config()$stop_year) {

  SDG <- SDG_Name <- SDG_Displayname <- NULL
  
  sdg <- con |>
    tbl("abm_sdg") |> 
    filter(analysis_id == analysisId) |> 
    collect()
  
  data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Conference Paper", "Review")) |>
    select(ScopusID, Unit_Fraction) |>
    left_join(sdg, by = "ScopusID") |>
    mutate(SDG_Displayname = ifelse (!is.na(SDG), paste0("SDG ", str_pad(SDG, 2, "left", "0"), " - ", SDG_Name), "None")) |>
    group_by(SDG_Displayname) |> 
    summarise(p = n(), p_frac = sum(Unit_Fraction))
}


#' Create table over any SDG by year for the selected unit
#'
#' @param data dataset with publications as tibble
#' @param con a database connection to BIBMON
#' @param analysisId id for the analysis, default from abm_config()
#' @param analysis_start first publication year of analysis, default from abm_config()
#' @param analysis_stop last publication year of analysis, default from abm_config()
#' @return a tibble
#' @import dplyr
#' @export
abm_sdg_year <- function(data,
                         con,
                         analysisId = abm_config()$analysis_id,
                         analysis_start = abm_config()$start_year,
                         analysis_stop = abm_config()$stop_year) {
  
  any_sdg <- p_sdg <- p_sdg_frac <- NULL
  
  sdg <- con |>
    tbl("abm_sdg") |>
    filter(analysis_id == analysisId) |> 
    collect()
  
  data |>
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             scop_doctype %in% c("Article", "Conference Paper", "Review")) |>
    select(ScopusID, Publication_Year, Unit_Fraction) |>
    mutate(any_sdg = ifelse(ScopusID %in% sdg$ScopusID, 1, 0)) |> 
    group_by(Publication_Year) |> 
    summarise(p = n(),
              p_frac = sum(Unit_Fraction),
              p_sdg = sum(any_sdg),
              p_sdg_frac = sum(any_sdg * Unit_Fraction),
              share_sdg = p_sdg / p,
              share_sdg_frac = p_sdg_frac / p_frac)
}


#' Create graph over SDGs
#' 
#' @param df a data frame at the format produced by abm_sdg_table()
#' @return a ggplot object
#' @import ggplot2 dplyr ktheme
#' @importFrom scales label_number
#' @importFrom stringr str_pad
#' @export
abm_graph_sdg <- function(df) {
  
  SDG_Displayname <- color <- goal <- goal_nr <- NULL

  if(nrow(df) > 0){
    colors <- sdg_colors() |>
      mutate(goal_nr = str_pad(goal, 2, "left", "0")) |> 
      select(goal_nr, color)
    sdgs <- df |>
      filter(SDG_Displayname != 'None') |> 
      mutate(goal_nr = substr(SDG_Displayname, 5, 6)) |>
      inner_join(colors, by = "goal_nr")
  
  pmax <- max(sdgs$p_frac, na.rm = TRUE)

  if (pmax > 200){
    ymax <- trunc(1+pmax/100, 2)*100
    ybreaks <- seq(0, ymax, 100)
  } else {
    ymax <- trunc(1+pmax/10, 1)*10
    ybreaks <- seq(0, ymax, 10)
  }
  
  ggplot(data = sdgs,
         aes(x = SDG_Displayname)) +
    geom_bar(aes(weight = p_frac), fill = sdgs$color) +
    xlab(NULL) +
    ylab("P (frac)") +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(as.factor(sdgs$SDG_Displayname)))) +
    scale_y_continuous(breaks = ybreaks,
                       minor_breaks = NULL,
                       limits = c(0, ymax),
                       expand = c(0, 10)) +
    theme_kth_neo() + 
    theme(axis.text.y  = element_text(hjust = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  }
}


#' Create Excel workbook with ABM publication data
#' 
#' @param data a data frame
#' @param attribution (optional) attribution text
#' @return A workbook object
#' @import openxlsx dplyr stringr
#' @export
abm_data_workbook <- function(data, attribution = NULL) {
  
  oldmaxWidth <- getOption("openxlsx.maxWidth")
  options("openxlsx.maxWidth" = 120)
  on.exit(options("openxlsx.maxWidth" = oldmaxWidth))
  
  dec3Style <- createStyle(numFmt = "0.000")
  dec3 <- which(names(data) %in% c("jcf", "Jtop20", "cf", "Cf_log",
                                   "Ptop1", "Ptop5", "Ptop10", "Ptop25",
                                   "Unit_Fraction", "Unit_Fraction_adj",
                                   "scop_fwci_x", "scop_snip"))
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Data")
  writeDataTable(wb, "Data", data)
  setColWidths(wb, "Data", cols = 1:ncol(data), widths = "auto")
  addStyle(wb, "Data", dec3Style, cols = dec3, rows = 2:nrow(data), gridExpand = T)
  
  if(!is.null(attribution)) {
    attrib <- data.frame(x = str_wrap(attribution, 90) |> str_split_1("\n"))
    attribStyle <- createStyle(fontSize = 12,
                               fgFill = "#EEEEEE",
                               textDecoration = "bold")
    addWorksheet(wb, "Attribution")
    writeData(wb, "Attribution", x = attrib,
              startCol = 2, startRow = 2,
              colNames = FALSE)
    setColWidths(wb, "Attribution", cols = 2, widths = "auto")
    addStyle(wb, "Attribution", attribStyle, cols = 2, rows = 2:(1+nrow(attrib)))
  }  
  
  wb
}
