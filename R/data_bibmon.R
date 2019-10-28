#' Connection to Bibliometrics data source for KTH
#' 
#' This connection relies on an .Renviron file with environment variables.
#' 
#' Make sure one exists and that variables are set for:
#' DBHOST, DBNAME, DBUSER, DBPASS
#' 
#' @return database connection
#' @import DBI odbc
#' @export
con_bib <- function() {
  con <- dbConnect(
    odbc::odbc(), driver = "ODBC Driver 17 for SQL Server", 
    server = Sys.getenv("DBHOST"), database = Sys.getenv("DBNAME"), Port = 1433, 
    UID = Sys.getenv("DBUSER"), PWD = Sys.getenv("DBPASS")
  )
}
  
#' Retrieve data for the first ABM table
#' 
#' @return tibble with data for organizational units
#' @export
#' @import DBI dplyr tidyr purrr
#' 
abm_tab1 <- function(con = con_bib()) 
{
  
  df_school <- 
    tbl(con, "t_diva") %>%
    # discard years outside of the 2012-2018 interval
    filter(Doc_Year > 2011, Doc_Year < 2019) %>%
    # group by organizational unit, publication type and years
    group_by(unit_long, Publication_Type_DiVA, Doc_Year, pt_ordning) %>%
    # calculate total and weigths
    summarise(
      w_d_Sum = sum(w_unit),
      wos_coverage = sum(wos_coverage * w_unit)
    ) %>%
    # arrange by organizational unit, year and publication sort order
    arrange(unit_long, Doc_Year, pt_ordning) %>%
    # fetch the data from the connection
    collect()
  
  # NB: The root level KTH is considered to be at "school" level
  df_school2 <- bind_rows(
    df_school,
    df_school %>%
      group_by(unit_long, Publication_Type_DiVA, pt_ordning) %>%
      summarise(
        w_d_Sum = sum(w_d_Sum),
        wos_coverage = sum(wos_coverage) / sum(w_d_Sum)
      ) %>%
      mutate(Doc_Year = NA)
    )  %>%
    mutate(unit_abbrev = if_else(
      unit_long == "KTH Royal Institute of Technology",
      "KTH",
      extract_re(unit_long)) 
    ) %>%
    left_join(abm_units(), by = "unit_abbrev")
  
  
  df_dep <- 
    tbl(con, "PUBL_DEPT_FRAC") %>% 
    filter(Doc_Year > 2011, Doc_Year < 2019) %>%
    collect() %>%
    select(-starts_with("_")) %>%
    rename(unit_id = unit, wos_coverage = wos_coverage_Mean) %>%
    mutate(unit_id = as.numeric(unit_id)) %>%
    left_join(abm_units()) #, by = "unit_id")
  
  df <-  
    # combine the two previous results
    df_school2 %>% bind_rows(df_dep) %>%
    # pivot to get yearly summary values horizontally as columns
    pivot_wider(names_from = Doc_Year, values_from = c(wos_coverage, w_d_Sum)) %>%
    ungroup()

  lookup_pt <- 
    df %>% 
    filter(!is.na(pt_ordning)) %>%
    distinct(pt_ordning, Publication_Type_DiVA)
  
  res <-
    # add publication type sort order
    df %>% left_join(lookup_pt, by = c("Publication_Type_DiVA")) %>%
    # clarify that this is sort order for publications
    rename(pubtype_order = pt_ordning.y) %>%
    # harmonize field names with existing abm report format
    rename(
      `Publication Type` = Publication_Type_DiVA,
      `WoS Coverage` = wos_coverage_NA,
      Total = w_d_Sum_NA
    ) %>%
    select(-c(starts_with("wos_coverage"), pt_ordning.x)) 
  
  res <- 
    set_names(res, replace_if_re(names(res))) %>%
    mutate(`Organizational Unit` = if_else(
      is.na(unit_long_eng), 
      unit_long_swe,
      unit_long_eng)
    ) %>%
    select(`Organizational Unit`, everything()) %>%
    rename("id" = "unit_id") %>%
    select(-c(starts_with("unit_")))
  
  
  dbDisconnect(con)
  
  return (res)
}


#' Extract regex capture groups
#' @param x vector of strings
#' @param re regexp where the first capture group gets extracted
#' @return vector with string contents of first capture group
#' @importFrom dplyr nth
#' @importFrom stringr str_match_all
#' @importFrom purrr map_chr
extract_re <- function(x, re) 
{
  if (missing(re))
    re <- "\\(([[:upper:]]{1})([[:upper:]]{1,4})\\)"
  map_chr(str_match_all(x, re), function(x) nth(x, 2))
}

replace_if_re <- function(x, 
  re_if = "^w_d_Sum", 
  re_replace = "_(\\d{4})$") 
{
  sar <- function(x)
    if_else(grepl(re_if, x), extract_re(x, re_replace), x)
  
  map_chr(x, sar)
}


#' Organizational units at KTH
#' 
#' @return tibble
#' @export
#' @import DBI dplyr tidyr	

abm_units <- function(con = con_bib()) {
  
  diva <- 
    con %>% tbl("DIVA_School_dept") %>%
    distinct(
      skola_namn, 
      Diva_org_id, 
      Diva_school_id, 
      Diva_dep_id, 
      dep_name) %>%
    collect()
  
  lookup <- bind_rows(
    
    # root level 1: KTH (hardcoding for now bec cannot find in BIBMON)
    tibble(
      unit_long_swe = "KTH Royal Institute of Technology",
      unit_id = 177,
      unit_level = 1,
      unit_abbrev = "KTH"
    ),

    # level 2: schools
    diva %>% 
    filter(!is.na(skola_namn)) %>% 
    distinct(Diva_school_id, skola_namn) %>%
    mutate(unit_level = 2) %>%
    rename(unit_long_swe = skola_namn, unit_id = Diva_school_id) %>%
    mutate(
      unit_id = as.numeric(unit_id), 
      unit_pid = 177,
      unit_abbrev = extract_re(unit_long_swe)
    ),

    # level 3: departments
    diva %>%
    filter(!is.na(dep_name)) %>%
    rename(unit_long_swe = dep_name, unit_id = Diva_dep_id) %>% 
    mutate(
      unit_level = 3,
      unit_id = as.numeric(unit_id),
      unit_pid = as.numeric(Diva_school_id)
    ) %>%
    collect()
  )
  
  lookup_abbrev <- 
    con %>% 
    tbl("t_diva") %>% 
    distinct(unit, unit_long, unit_sort) %>%
    rename(unit_abbrev = unit, unit_long_eng = unit_long) %>%
    collect()

  res <- 
    lookup %>%
    left_join(lookup_abbrev, by = "unit_abbrev") %>% 
    select(starts_with("unit"))
  
  dbDisconnect(con)
  
  return (res)
  
}
