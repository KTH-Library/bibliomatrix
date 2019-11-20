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
    mutate(wos_bin = ifelse(!is.na(Doc_id),1,0)) %>%  # Wos coverage as calculated in old ÅBU
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
              WoS_coverage = weighted.mean(wos_bin, Unit_Fraction, na.rm = T)) %>%
    ungroup()

  if (!"Pool" %in% class(con)) dbDisconnect(con)

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
           Publication_Year < max(Publication_Year, na.rm = TRUE) - 1) %>%
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

  if (!"Pool" %in% class(con)) dbDisconnect(con)

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
  
  data.frame(interval = rep(interval, each = width), x = rep(starts, each = width) + rep(seq(0, width - 1), length(starts)), stringsAsFactors = FALSE)
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
             Publication_Year < max(Publication_Year, na.rm = TRUE) & 
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

  if (!"Pool" %in% class(con)) dbDisconnect(con)
  
  bind_rows(table1, table2)
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
  
  if (!"Pool" %in% class(con)) dbDisconnect(con)
  
  bind_rows(table1, table2)
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
    filter(Publication_Type_WoS %in% c("Article", "Review")
           & !is.na(int)) %>%
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
  
  if (!"Pool" %in% class(con)) dbDisconnect(con)
  
  bind_rows(table1, table2)
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
  
  if (!"Pool" %in% class(con)) dbDisconnect(con)
  
  res
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
  
  if (!"Pool" %in% class(con)) dbDisconnect(con)

  ret
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
#' @return a list with two slots - "meta" for organizational unit metadata info 
#'   and "units" with a named list of results (set of 5 different tibbles for each of the units).
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
  
  # retrieve unit codes
  units_table <- 
    unit_info() %>%
    collect() %>%
    arrange(-desc(org_level)) 
  
  units <- 
    units_table %>% 
    select(unit_code) %>% 
    pull(1)
  
  # for a unit, retrieve all abm tables
  unit_tables <- function(x) {
    tabs <- list(
      abm_table1(unit_code = x),
      abm_table2(unit_code = x),
      abm_table3(unit_code = x),
      abm_table4(unit_code = x),
      abm_table5(unit_code = x),
      summaries = abm_dash_indices(con = pool_bib(), unit_code = x)
    )
  }
  
  message("Patience, please. It takes a while to fetch the data into the cache.")
  res <- map(units, unit_tables)
  res <- setNames(res, units)
  
  out <- list("meta" = units_table, "units" = res)
  
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
  
  # retrieve unit codes
  units_table <- 
    unit_info() %>%
    collect() %>%
    arrange(-desc(org_level)) 
  
  # for a kthid, retrieve all abm tables
  unit_tables <- function(x) {
    tabs <- list(
      abm_table1(unit_code = x),
      abm_table2(unit_code = x),
      abm_table3(unit_code = x),
      abm_table4(unit_code = x),
      abm_table5(unit_code = x),
      publications = abm_publications(unit_code = x)
    )
  }
  
  res <- list(unit_tables(unit_code))
  res <- setNames(res, unit_code)
  
  out <- list("meta" = units_table, "units" = res)
  
  return(out)  
}

#' Create graph over DiVA publication types by year
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @importFrom stats reorder
#' @export
abm_graph_diva <- function(df){
  df_diva_long <- df %>%
    select(-"P_frac", -"WoS_coverage") %>%
    gather("year", "value", -Publication_Type_DiVA) %>%
    left_join(get_pt_ordning(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  
  ggplot(data = df_diva_long,
         aes(x = year)) +
    geom_bar(aes(weight = value, fill = reorder(Publication_Type_DiVA, pt_ordning))) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_brewer(palette = "Set3")
}

#' Create graph over WoS coverage by year
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @importFrom stats reorder
#' @export
abm_graph_wos_coverage <- function(df){
  df <- df %>% left_join(get_pt_ordning(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  ggplot(data = df,
         aes(x = reorder(Publication_Type_DiVA, -pt_ordning))) +
    geom_bar(aes(weight = WoS_coverage)) +
    ylab("Web of Science coverage") +
    xlab(NULL) +
    ylab(NULL) +
    coord_flip() +
    scale_y_continuous(labels=scales::percent, breaks = seq(0,1,0.1), limits = c(0, 1))
  
}

#' Create graph over Cf by year
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_cf <- function(df){
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = cf, group=1)) +
    geom_point() + 
    geom_line()
}

#' Create graph over Top 10\% publications by year
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_top10 <- function(df){
  ggplot(data = df %>% filter(!interval == "Total"),
         aes(x = interval, y = top10_share, group=1)) +
    geom_point() +
    geom_line()
}

#' Create graph over jcf by year
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_jcf <- function(df){
  ggplot(data = df_jcf %>% filter(!interval == "Total"),
         aes(x = interval, y = jcf, group=1)) +
    geom_point() + 
    geom_line()
}

#' Create graph over Top 20\% journals by year
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_top20 <- function(df){
  ggplot(data = df_jcf %>% filter(!interval == "Total"),
         aes(x = interval, y = top20_share, group=1)) +
    geom_point() + 
    geom_line()
}

#' Create graph over international and Swedish non-university copublications by year
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a ggplot object
#' @import ggplot2 dplyr
#' @export
abm_graph_copub <- function(df){
  df_copub_long<- df_copub %>% gather("indicator", "value", -interval) %>% 
    filter(!interval == "Total") %>% 
    filter(indicator %in% c("int_share", "nonuniv_share"))
  
  ggplot(data = df_copub_long,
         aes(x = interval, y = value, group=indicator)) +
    geom_line(aes(color=indicator)) + 
    geom_point(aes(color=indicator))
}

#' Create waffle share over share of international copublications
#' 
#' @param df a data frame at the format produced by abm_table5()
#' @param col a vector with colors for filling (optional)
#' @return a ggplot object
#' @import ggplot2 waffle dplyr
#' @export
abm_waffle_copub_int <- function(df, col = c(as.character(palette_kth(1)), "#F6F6F6")){
  intshare <- df %>%
    filter(interval == "Total") %>%
    select(int_share) %>%
    mutate(int_share = round(100*int_share)) %>%
    as.integer()

  waffle(c(intshare, 100-intshare),
         rows = 5,
         size = 1,
         colors = col,
         legend_pos = "none")
}

#' Create waffle share over share of Swedish non-university copublications
#' 
#' @param df a data frame at the format produced by abm_table5()
#' @param col a vector with colors for filling (optional)
#' @return a ggplot object
#' @import ggplot2 waffle dplyr
#' @export
abm_waffle_copub_nonuniv <- function(df, col = c(as.character(palette_kth(1)), "#F6F6F6")){
  nonuniv <- df %>%
    filter(interval == "Total") %>%
    select(nonuniv_share) %>%
    mutate(nonuniv_share = round(100*nonuniv_share)) %>%
    as.integer()
  
  waffle(c(nonuniv, 100-nonuniv),
         rows = 5,
         size = 1,
         colors = col,
         legend_pos = "none")
}
