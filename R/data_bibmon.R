#' Retrieve data for the first ABM table
#' 
#' @return tibble with data for organizational units
#' @export
abm_tab1 <- function() 
{
  
  library(DBI)
  library(dplyr)
  library(tidyr)
  
  con_bibmon <- dbConnect(
    odbc::odbc(), driver = "ODBC Driver 17 for SQL Server", 
    server = Sys.getenv("DBHOST"), database = Sys.getenv("DBNAME"), Port = 1433, 
    UID = Sys.getenv("DBUSER"), PWD = Sys.getenv("DBPASS")
  )
  
  df_school <- 
    tbl(con_bibmon, "t_diva") %>%
    filter(Doc_Year > 2011, Doc_Year < 2019) %>%
    group_by(unit_long, Publication_Type_DiVA, Doc_Year, pt_ordning) %>%
    summarise(
      w_d_Sum = sum(w_unit),
      wos_coverage = sum(wos_coverage * w_unit)
    ) %>%
    arrange(unit_long, Doc_Year, pt_ordning) %>%
    collect()
  
  df_school2 <- bind_rows(
    df_school,
    df_school %>%
      group_by(unit_long, Publication_Type_DiVA, pt_ordning) %>%
      summarise(
        w_d_Sum = sum(w_d_Sum),
        wos_coverage = sum(wos_coverage) / sum(w_d_Sum)
      ) %>%
      mutate(Doc_Year = NA)
  )
  
  df_dep <- 
    tbl(con_bibmon, "PUBL_DEPT_FRAC") %>% 
    filter(Doc_Year > 2011, Doc_Year < 2019) %>%
    collect() %>%
    select(-starts_with("_")) %>%
    rename(unit_id = unit, wos_coverage = wos_coverage_Mean)
  
  df <- 
    df_school2 %>% bind_rows(df_dep) %>%
    pivot_wider(names_from = Doc_Year, values_from = c(wos_coverage, w_d_Sum)) %>%
    ungroup()

  lookup_pt <- 
    df %>% 
    filter(!is.na(pt_ordning)) %>%
    distinct(pt_ordning, Publication_Type_DiVA)
  
  res <-
    df %>% left_join(lookup_pt, by = c("Publication_Type_DiVA")) %>%
    # 
    rename(sort_order = pt_ordning.y) %>%
    # harmonize field names with existing abm report format
    rename(
      `Publication Type` = Publication_Type_DiVA,
      `WoS Coverage` = wos_coverage_NA,
      Total = w_d_Sum_NA
    ) %>%
    select(-c(starts_with("wos_coverage"), pt_ordning.x))
  
  # TODO:
  # rename på alla w_d_Sum_\\(d{4}) -> $1
  
  # TODO: koppling mellan unit_id och unit_long
  # unit_long för institution
  # unit_id för KTH + skolor 
    
  dbDisconnect(con_bibmon)
  
  return (res)
}

# TODO: fcn to return unit_id, unit_long

# library(readr)
# 
# library(purrr)
# read_rds("~/repos/KTH-Library.github.io/abm/pub.rda") %>%
#   pluck(1, 1)