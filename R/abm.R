#' Provide defaults for ABM analysis
#'
#'@return list of default values used in the current abm presentation
#'@export

abm_config<- function(){
  # This can later be expanded with more relevant defaults
  list(start_year = 2012, 
       stop_year = 2018,
       default_unit = "KTH")
}
  
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

#' Retrieve Table 1 (Publications in DiVA) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return data frame with publications by type and year
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table1 <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit
  orgdata <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
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
              WoS_coverage = sum(wos_bin * Unit_Fraction, na.rm = TRUE) / sum(Unit_fraction, na.rm = TRUE)) %>%
    ungroup() %>% 
    collect()
  
  pubtype_order <- get_pubtype_order(con)
  
  table1 %>%
    inner_join(table2, by = "Publication_Type_DiVA") %>%
    inner_join(pubtype_order, by = c("Publication_Type_DiVA" = "diva_publication_type")) %>%
    arrange(pt_ordning) %>%
    select(-pt_ordning)
}

#' Retrieve Table 2 (Citations 3-year window) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with citations statistics by year and total
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table2 <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 2 &
             Publication_Type_WoS %in% c("Article", "Proceedings paper", "Review", "Letter", "Editorial"))

  # Year dependent part of table
  table1 <-
    orgdata %>%
    group_by(Publication_Year) %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
              C3 = sum(Citations_3yr * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    ungroup() %>%
    collect() %>%
    mutate(Publication_Year_ch = as.character(Publication_Year)) %>%
    arrange(Publication_Year_ch) %>% 
    select(Publication_Year_ch, P_frac, C3_frac, C3)

  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  # Summary part of table
  table2 <-
    orgdata %>%
    summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
              C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
              C3 = sum(Citations_3yr * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    mutate(Publication_Year_ch = "Total") %>%
    collect()

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
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table3 <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 1 &
             Publication_Type_WoS %in% c("Article", "Review") & 
             !is.na(cf))
  
    # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    collect() %>%
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
    collect() %>% 
    mutate(interval = "Total")

  bind_rows(table1, table2)
}

#' Retrieve Table 4 (Journal impact) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table4 <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_WoS %in% c("Article", "Review"))

  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    collect() %>% 
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
              jcf = sum(jcf * Unit_Fraction, na.rm = TRUE) / sum(Unit_fraction, na.rm = TRUE),
              top20_count = sum(Jtop20 * Unit_Fraction, na.rm = TRUE),
              top20_share = sum(Jtop20 * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    collect() %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}

#' Retrieve Table 5 (Co-publishing) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table5 <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_WoS %in% c("Article", "Review") &
             !is.na(int))

  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>%
    collect() %>% 
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
    collect() %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}


#' Retrieve OA-data for ABM tables
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code for filtering on one or more unit code(s), which can be KTH, a one letter school code, an integer department code or a KTH-id (optional)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with OA-status of all publications from selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export
abm_oa_data <- function(con = con_bib(), unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {
  oa_data <- con %>% tbl("OA_status") %>% 
    right_join(abm_data(con = con, unit_code = unit_code), by = "PID") %>% 
    select("PID","oa_status","is_oa","Publication_Type_DiVA", "Publication_Year", "DOI")
  
  # Year dependent part of table
  table1 <-
      oa_data %>%
      group_by(Publication_Year) %>%
      #filter(is_oa=="TRUE" | is_oa=="FALSE") %>%
      collect() %>%
      summarise(P_tot=n(),
                P_known=sum(as.logical(is_oa=="TRUE" | is_oa=="FALSE"), na.rm=TRUE),
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
      select(Publication_Year_ch, P_tot, P_known, oa_count, gold_count, hybrid_count, green_count, bronze_count, closed_count, oa_share)

  # No summary row if no data
  if(nrow(table1) == 0)
    return(table1)

  # Summary part of table
  table2 <- table1 %>%
      summarise(Publication_Year_ch="Total",
                P_tot=sum(P_tot),
                P_known=sum(P_known),
                oa_count=sum(oa_count),
                gold_count=sum(gold_count),
                hybrid_count=sum(hybrid_count),
                green_count=sum(green_count),
                bronze_count=sum(bronze_count),
                closed_count=sum(closed_count),
                oa_share=sum(oa_count)/sum(P_known))

  bind_rows(table1, table2)
}


#' Retrieve Table 6 (OA-status) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with OA stats per year
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export
abm_table6 <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- abm_oa_data(con=con, unit_code = unit_code) %>%
    filter(  Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 2 &
             Publication_Type_DiVA %in% c("Article, peer review", "Conference paper, peer review"))
  
  # Year dependent part of table
  table1 <-
    orgdata %>% filter(!is.na(oa_status)) %>%
    group_by(Publication_Year, oa_status) %>%
    summarise(n = count()) 
  #%>%
  #  mutate(freq = n / sum(n))
  
  table1
  
  # table1 <-
  #   orgdata %>%
  #   group_by(Publication_Year, oa_status) %>%
  #   summarise(counts = count(oa_status, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   collect() %>%
  #   mutate(Publication_Year_ch = as.character(Publication_Year)) %>%
  #   arrange(Publication_Year_ch) %>% 
  #   select(Publication_Year_ch, P_frac, C3_frac, C3)
  # 
  # No summary row if no data
  # if(nrow(table1) == 0)
  #   return(table1)
  
  # Summary part of table
  # table2 <-
  #   orgdata %>%
  #   summarise(P_frac = sum(Unit_Fraction, na.rm = TRUE),
  #             C3_frac = sum(Unit_Fraction * Citations_3yr, na.rm = TRUE),
  #             C3 = sum(Citations_3yr * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
  #   mutate(Publication_Year_ch = "Total") %>%
  #   collect()
  # 
  # bind_rows(table1, table2)

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
  
  if (!"Pool" %in% class(con)) dbDisconnect(con)

  list(tot_pubs_frac = sum(t1[, as.character(lastyear)], na.rm = TRUE),
       cf = t3$cf,
       top10_share = t3$top10_share,
       jcf = t4$jcf,
       top20_share = t4$top20_share,
       copub_nonuniv = t5$nonuniv_share,
       copub_internat = t5$int_share)
}

#' Retrieve WoS coverage for peer reviewed DiVA publication types
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, if not given abm_config() is used
#' @param analysis_stop last publication year of analysis, if not given abm_config() is used
#' @return tibble with fractionalized and full counted WoS coverage by year and publication type
#' @import pool dplyr
#' @export
abm_woscoverage <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {

  # Get publication level data for selected unit (and filter on pub_year if given)
  orgdata <- abm_data(con = con) %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_DiVA %in% c("Article, peer review", "Conference paper, peer review")) %>%
    mutate(wos_bin = ifelse(!is.na(Doc_id),1,0)) %>%
    select(Publication_Year, Publication_Type_DiVA, Unit_Fraction, wos_bin) %>%
    group_by(Publication_Year, Publication_Type_DiVA) %>%
    summarise(p_frac = sum(Unit_fraction, na.rm = TRUE),
              p_full = n(),
              sumcov_frac = sum(Unit_fraction * wos_bin, na.rm = TRUE),
              sumcov_full = sum(wos_bin, na.rm = TRUE),
              woscov_frac = sum(Unit_fraction * wos_bin, na.rm = TRUE) / sum(Unit_fraction, na.rm = TRUE),
              woscov_full = sum(wos_bin, na.rm = TRUE) / n()) %>%
    ungroup() %>%
    collect()
  
  peerreviewed <- orgdata %>%
    group_by(Publication_Year) %>%
    summarise(p_frac = sum(p_frac),
              p_full = sum(p_full),
              sumcov_frac = sum(sumcov_frac),
              sumcov_full = sum(sumcov_full)) %>%
    mutate(woscov_frac = sumcov_frac / p_frac,
           woscov_full = sumcov_full / p_full,
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
    abm_units <- con %>% tbl("abm_org_info") %>% collect()
    
    abm_units %>%
      # Get full sort order
      inner_join(hiersort(abm_units, "Diva_org_id", "org_level", "parent_org_id", "unit_long_en"), by = "Diva_org_id") %>%
      # Add indented versions of unit_long_en, one with plain white space and one for usage in html where leading white space gets sanitized
      mutate(unit_long_en_indent1 = str_pad(unit_long_en, side = "left", width = 4*org_level + stringr::str_length(unit_long_en)),
             unit_long_en_indent2 = str_pad(unit_long_en, side = "left", width = 4*org_level + stringr::str_length(unit_long_en), pad = "\U00A0")) %>%
      arrange(sort_order)
  }
}

#' Retrieve publication list for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param unit_code the code for the analyzed unit (KTH, a one letter school code, an integer department code or a KTH-id)
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with publication list data for selected unit
#' @import DBI dplyr tidyr purrr
#' @export
abm_publications <- function(con, unit_code, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){

  # Get publication level data for selected unit
  orgdata <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code == unit_code &
             Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop) %>%
    select(-c("w_subj", "Unit_Fraction_adj", "level")) %>%
    collect()
  
  orgdata %>% arrange(Publication_Year, Publication_Type_DiVA, WoS_Journal, PID)
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
#' unit_kth <- public \%>\% pluck("units", "KTH", 1)
#' 
#' # get summary data for KTH
#' public \%>\% pluck("units", "KTH", "summaries")
#'  
#' # get public data specifically for KTH and table 1
#' unit_kth <- public \%>\% pluck("units", "KTH", 1)
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
    get_pubtype_order(con = pool_bib()) %>%
    arrange(pt_ordning)
  
  # for a unit, retrieve all abm tables
  unit_tables <- function(x) {
    tabs <- list(
      abm_table1(con = db, unit_code = x),
      abm_table2(con = db, unit_code = x),
      abm_table3(con = db, unit_code = x),
      abm_table4(con = db, unit_code = x),
      abm_table5(con = db, unit_code = x),
      coverage = abm_woscoverage(con = db, unit_code = x),
      summaries = abm_dash_indices(con = db, unit_code = x),
      oa = abm_oa_data(con = db, unit_code = x)
    )
  }
  
  message("Patience, please. It takes a while to fetch the data into the cache.")
  res <- map(units, unit_tables)
  res <- setNames(res, units)
  
  poolClose(db)
  
  out <- list("meta" = units_table, "units" = res, "pubtype_order" = pubtype_order)
  
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
    tabs <- list(
      abm_table1(con = db, unit_code = x),
      abm_table2(con = db, unit_code = x),
      abm_table3(con = db, unit_code = x),
      abm_table4(con = db, unit_code = x),
      abm_table5(con = db, unit_code = x),
      coverage = abm_woscoverage(con = db, unit_code = x),
      publications = abm_publications(con = db, unit_code = x),
      oa = abm_oa_data(con = db, unit_code = x)
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
#' @import ggplot2 dplyr
#' @importFrom stats reorder
#' @importFrom RColorBrewer brewer.pal
#' @export
abm_graph_diva <- function(df){
  df_diva_long <- df %>%
    select(-"P_frac", -"WoS_coverage") %>%
    gather("year", "value", -Publication_Type_DiVA) %>%
    left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  
  colvals <- c(brewer.pal(12, "Set3"), "#8080B0")
  
  ggplot(data = df_diva_long,
         aes(x = year)) +
    geom_bar(aes(weight = value, fill = reorder(Publication_Type_DiVA, pt_ordning))) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_manual(values = colvals) +
    kth_theme()
}

#' Create graph over WoS coverage by year
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @importFrom stats reorder
#' @importFrom scales percent
#' @export
abm_graph_wos_coverage <- function(df){
  kth_cols <- palette_kth()
  df <- df %>% left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  ggplot(data = df,
         aes(x = reorder(Publication_Type_DiVA, -pt_ordning))) +
    geom_bar(aes(weight = WoS_coverage), fill = kth_cols["blue"]) +
    xlab(NULL) +
    ylab(NULL) +
    coord_flip() +
    scale_y_continuous(labels=percent, breaks = seq(0,1,0.1), limits = c(0, 1)) +
    kth_theme()
}

#' Create graph over Cf by year
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_cf <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(2, ceiling(max(df$cf)))

  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = cf, group=1)) +
    geom_point() + 
    geom_line(color = kth_cols["blue"]) +
    xlab(NULL) +
    ylab(NULL) +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["lightblue"]) +
    kth_theme()
}

#' Create graph over Top 10\% publications by year
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @importFrom scales percent
#' @export
abm_graph_top10 <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(0.2, ceiling(max(df$top10_share)*10)/10)
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top10_share, group=1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"]) +
  xlab(NULL) +
  ylab(NULL) +
  geom_hline(yintercept = 0.1, color = kth_cols["lightblue"]) +
  scale_y_continuous(labels = percent, limits = c(0, ymax)) +
    kth_theme()
}

#' Create graph over jcf by year
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_jcf <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(2, ceiling(max(df$jcf)))

  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = jcf, group=1)) +
    geom_point() + 
    geom_line(color = kth_cols["blue"]) +
    xlab(NULL) +
    ylab(NULL) +
    ylim(0, ymax) +
    geom_hline(yintercept = 1.0, color = kth_cols["lightblue"]) +
    kth_theme()
}

#' Create graph over Top 20\% journals by year
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @importFrom scales percent
#' @export
abm_graph_top20 <- function(df){
  kth_cols <- palette_kth(4)
  ymax <- max(0.4, ceiling(max(df$top20_share)*10)/10)
  
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top20_share, group=1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"]) +
    xlab(NULL) +
    ylab(NULL) +
    geom_hline(yintercept = 0.2, color = kth_cols["lightblue"]) +
    scale_y_continuous(labels = percent, limits = c(0, ymax)) +
    kth_theme()
}

#' Create graph over international and Swedish non-university copublications by year
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_copub <- function(df){
  kth_cols <- as.vector(palette_kth(4))
  df_copub_long<- df %>%
    select(interval, nonuniv_share, int_share) %>% 
    rename("Swedish Non-university" = nonuniv_share, "International" = int_share) %>% 
    gather("Co-publication", "value", -interval) %>% 
    filter(!interval == "Total")
  
  ggplot(data = df_copub_long,
         aes(x = interval, y = value, group = `Co-publication`)) +
    geom_line(aes(color = `Co-publication`)) +
    geom_point(aes(color = `Co-publication`)) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_color_manual(values = kth_cols) +
    kth_theme()
}

#' Create waffle chart (5 rows, 20 columns) for any single percentage
#' 
#' @param pct a percentage expressed as a decimal number 0 <= pct <= 1
#' @param col a vector with colors for filling (optional)
#' @param label a title for the chart, displayed above the waffle (optional)
#'
#' @return a ggplot object
#' @import waffle
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
    theme(plot.title=element_text(size = 12))
}

#' Create bullet graph with reference line
#'
#' @param label a label for the indicator, shown to the left of the gauge
#' @param value the value of the indicator, displayed as a horizontal wide line
#' @param reference a reference value displayed as a vertical thin line
#' @param roundto number of digits after the decimal point (default = 1)
#' @param pct boolean, set to TRUE if given value is a share (default = FALSE)
#' @return a ggplot object
#' @import ggplot2
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
    geom_errorbar(aes(x = measure, ymin = target, ymax = target), 
      color = cerise, width = 0.9, size = 1.1) +
    coord_flip() +
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

#' Make ABM table have last rows bold with gray background, other rows with white background
#'
#' @param t the table to be formatted
#' @export
abm_format_rows <- function(t){
  formatStyle(table = t,
              columns = 1,
              target = "row",
              fontWeight = styleEqual("Total", "bold"),
              backgroundColor = styleEqual("Total", "#DDDDDD", "#FFFFFF"))
}


#' Create graph for Open Access data
#' 
#' @param df a data frame at the format produced by abm_oa_data()
#' @return a pie chart
#' @import ggplot2 dplyr
#' @export
abm_graph_oadata <- function(df){
  unpaywall_cols <- c("#000000", "#F9BC01", "#8D4EB4", "#20E168", "#CD7F32", "#BBBBBB")
  #kth_cols <- as.vector(palette_kth(4))
  df_oa_graphdata <- df %>%
    filter(Publication_Year_ch == "Total") %>%
    select(P_tot, gold_count, hybrid_count, green_count, bronze_count, closed_count) %>%
    mutate(P_tot = P_tot - (gold_count + hybrid_count + green_count + bronze_count + closed_count)) %>%
    rename("Unknown status" = P_tot, "Gold" = gold_count, "Hybrid" = hybrid_count, "Green" = green_count, "Bronze" = bronze_count, "Not OA" = closed_count)
  


  pie(t(df_oa_graphdata), labels = names(df_oa_graphdata), main = "Overview of publications by OA type", col = unpaywall_cols)


 # ggplot(data = df_copub_long,
 #        aes(x = interval, y = value, group = `Co-publication`)) +
 #   geom_line(aes(color = `Co-publication`)) +
 #   geom_point(aes(color = `Co-publication`)) +
 #   xlab(NULL) +
 #   ylab(NULL) +
 #   scale_y_continuous(labels = percent, limits = c(0, 1)) +
 #   scale_color_manual(values = kth_cols) +
 #   kth_theme()
}
