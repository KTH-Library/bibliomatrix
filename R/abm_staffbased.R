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
abm_staff_data <- function(con = con_bib(), kthids,
                           analysis_start = abm_config()$start_year, 
                           analysis_stop = abm_config()$stop_year) {
  
  res <- con %>%
    tbl("masterfile") %>%
    filter(Unit_code %in% kthids, level == 3, is_kth == 1) %>%
    rename(Unit_Fraction_raw = Unit_Fraction, Unit_Fraction_adj_raw = Unit_Fraction_adj) %>%
    collect() #%>% 
  
  auth_count<- res %>% group_by(PID) %>% tally() 
  colnames(auth_count)<- c("PID", "auth_count")
  
  unit_frac<- res %>% group_by(PID) %>% 
    summarise(Unit_Fraction = sum(Unit_Fraction_raw, na.rm = TRUE), Unit_Fraction_adj = sum(Unit_Fraction_adj_raw, na.rm = TRUE))
  
  res %>% 
    inner_join(auth_count, by="PID") %>% 
    inner_join(unit_frac, by="PID") %>% 
    arrange(PID) %>% # to make sure that Diva_org = 177 is preferred over blanks in deduplication below
    distinct(PID, WebofScience_ID, .keep_all = TRUE) %>%
    select(-Unit_code, -Unit_Name)  # removing redundant fields, that can be misleading after deduplication
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
  
  abm_staff_data(kthids = abm_researchers(unit_code), 
                 analysis_start = analysis_start, analysis_stop = analysis_stop, con = con) %>% 
    arrange(Publication_Year, Publication_Type_DiVA, WoS_Journal, PID)
  
}

# Alt versions of abm_table functions
#---------------------------------------
#' Retrieve Table 1 (Publications in DiVA) for ABM
#' 
#' @param con connection to db, default is to use sqlite connection
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
    mutate(wos_bin = ifelse(!is.na(Doc_id),1,0), scop_bin = ifelse(!is.na(ScopusID),1,0))
  
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

#' Retrieve Table 2 (Citations 3-year window) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with citations statistics by year and total
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table2_alt <- function(con = con_bib(), data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(  Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop - 2 &
             Publication_Type_WoS %in% c("Article", "Proceedings Paper", "Review", "Letter", "Editorial")) %>%
    mutate(uncited = ifelse(Citations_3yr > 0, 0, 1))
  
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
    collect() %>%
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
    mutate(Publication_Year_ch = "Total") %>%
    collect()
  
  bind_rows(table1, table2)
  
}


#' Retrieve Table 3 (Field normalized citations) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with field normalized citations and number/share of top10 publications by 3 year interval
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table3_alt <- function(con = con_bib(), data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
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
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with field normalized journal citation score and number/share of publications in top20 journals
#' @import DBI dplyr tidyr purrr
#' @importFrom stats weighted.mean
#' @export

abm_table4_alt <- function(con = con_bib(), data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
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
              jcf = sum(jcf * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
              top20_count = sum(Jtop20 * Unit_Fraction, na.rm = TRUE),
              top20_share = sum(Jtop20 * Unit_Fraction, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE)) %>%
    collect() %>%
    mutate(interval = "Total")
  
  bind_rows(table1, table2)
}


#' Retrieve Table 5 (Co-publishing) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, default 2012
#' @param analysis_stop last publication year of analysis, default 2018
#' @return tibble with number/share of international copublications and copublications with Swedish non-university organizations
#' @import DBI dplyr tidyr purrr
#' @export

abm_table5_alt <- function(con = con_bib(), data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year){
  
  # Get publication level data for selected unit, relevant WoS doctypes only
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
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

#' Retrieve OA-data for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble, incl PID for joining
#' @return tibble with OA-status of all publications from selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export
abm_oa_data_alt <- function(con = con_bib(), data) {
  
  data %>% 
    select("PID", "oa_status", "is_oa", 
           "Publication_Type_DiVA", "Publication_Year")
  
}

#' Retrieve Table 6 (OA data) for ABM
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble, incl PID for joining
#' @return tibble with OA-status of all publications from selected organizational units
#' @import DBI dplyr tidyr purrr
#' @export
abm_table6_alt <- function(con = con_bib(), data) {
  
  oa_data <- abm_oa_data_alt(con =con, data = data)
  
  # Year-dependent part of table
  table1 <-
    oa_data %>%
    group_by(Publication_Year) %>%
    filter(is_oa=="TRUE" | is_oa=="FALSE") %>%
    collect() %>%
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


#' Retrieve WoS coverage for peer reviewed DiVA publication types
#' 
#' @param con connection to db, default is to use mssql connection
#' @param data dataset with publications as tibble
#' @param analysis_start first publication year of analysis, if not given abm_config() is used
#' @param analysis_stop last publication year of analysis, if not given abm_config() is used
#' @return tibble with fractionalized and full counted WoS coverage by year and publication type
#' @import pool dplyr
#' @export
abm_woscoverage_alt <- function(con, data, analysis_start = abm_config()$start_year, analysis_stop = abm_config()$stop_year) {
  
  # Get publication level data for selected unit (and filter on pub_year if given)
  orgdata <- data %>%
    filter(Publication_Year >= analysis_start &
             Publication_Year <= analysis_stop &
             Publication_Type_DiVA %in% c("Article, peer review", "Conference paper, peer review")) %>%
    mutate(wos_bin = ifelse(!is.na(Doc_id),1,0)) %>%
    select(Publication_Year, Publication_Type_DiVA, Unit_Fraction, wos_bin) %>%
    group_by(Publication_Year, Publication_Type_DiVA) %>%
    summarise(p_frac = sum(Unit_Fraction, na.rm = TRUE),
              p_full = n(),
              sumcov_frac = sum(Unit_Fraction * wos_bin, na.rm = TRUE),
              sumcov_full = sum(wos_bin, na.rm = TRUE),
              woscov_frac = sum(Unit_Fraction * wos_bin, na.rm = TRUE) / sum(Unit_Fraction, na.rm = TRUE),
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

