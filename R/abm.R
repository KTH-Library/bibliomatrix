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
#' @param con connection to db - if no connection is given, attempt to use abm_public_kth data
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
  
  pubtype_order <- get_pubtype_order(con)

  if (!"Pool" %in% class(con)) dbDisconnect(con)

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
  
  if(nrow(orgdata) == 0)
    return(tibble(interval = "Total", P_frac = NA, cf = NA, top10_count = NA, top10_share = NA))

  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>% 
    inner_join(sliding_intervals(min(orgdata$Publication_Year), max(orgdata$Publication_Year), 3),
                        by = c("Publication_Year" = "x"))

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
  
  if(nrow(orgdata) == 0)
    return(tibble(interval = "Total", P_frac = NA, jcf = NA, top20_count = NA, top20_share = NA))
  
  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>% 
    inner_join(sliding_intervals(min(orgdata$Publication_Year), max(orgdata$Publication_Year), 3),
               by = c("Publication_Year" = "x"))
  
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
  
  if(nrow(orgdata) == 0)
    return(tibble(interval = "Total", P_full = NA, nonuniv_count = NA, nonuniv_share = NA, int_count = NA, int_share = NA))

  # Duplicate rows so that publications are connected to all intervals they should belong to according to publication year
  orgdata3year <-
    orgdata %>% 
    inner_join(sliding_intervals(min(orgdata$Publication_Year), max(orgdata$Publication_Year), 3),
               by = c("Publication_Year" = "x"))
  
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
  
  # TODO: should this function provide filter params?
  # discuss: filtering could happen from outside
  
  res <- unit_info_internal(con, unit, level, parent)

  if (!missing(level))
    res <- res %>% filter(org_level %in% level)
  
  if (!missing(unit))
    res <- res %>% filter(unit_code %in% unit)
  
  if (!missing(parent))
    res <- res %>% filter(parent_org_id %in% parent)
  
  res
  
}

#' Depth-first search ordered organizational units DAG (tree)'
#' 
#' This function checks data integrity for KTH organizational units data
#' ensuring it is a tree (directed acyclic graph) and makes a dfs to
#' order the tree and then converts it into a tibble adds two fields for
#' display purposes - one space padded indented field and one which is
#' space padded with a Unicode space character that doesn't get santized
#' when used in HTML contexts (such as in a shinyInput filter)
#' @return tibble with dfs ordered tree
#' @importFrom igraph graph_from_data_frame V is_dag
#' @importFrom stringr str_pad
#' @noRd

unit_info_internal <- function(con = con_bib(), unit, level, parent) {

  res <- con %>% tbl("abm_org_info") %>% collect()
    
  treee <- 
    res %>% 
    select(Diva_org_id, parent_org_id, unit_long_en, org_level, everything()) %>%
    arrange(-desc(org_level)) %>%
    mutate(n_pad = org_level * 4) %>%
    mutate(unit_long_en_indent1 = sprintf("%*s%s", n_pad, "", unit_long_en)) %>%
    mutate(unit_long_en_indent2 = sprintf("%s%s", 
      # for usage in html where leading white space gets sanitized
      stringr::str_pad("", side = "left", width = n_pad, pad = "\U00A0"), 
      unit_long_en)) %>%
    select(-n_pad)
  
  v <- 
    treee %>% 
    select(from = 2, to = 1, everything()) %>%
    # convention in igraph for root node id
    mutate(from = recode(from, .missing = as.integer(1)))
  
  g <- igraph::graph_from_data_frame(v, directed = TRUE)
  
  if (!igraph::is_dag(g)) 
    stop("Data integrity failed for abm_org_info tbl - it is not a tree structure!")
    
  #sc <- igraph::subcomponent(g, igraph::V(g)["177"], "out")
  #plot(g, layout = layout.reingold.tilford(g, root=1))
  #layout_as_tree(g, root = 1)
  
  root_id <- "177"  # hard coded for KTH
  
  out <- 
    igraph::dfs(g, igraph::V(g)[root_id], "out")$order %>%
    names() %>%
    as.integer() %>%
    tibble(Diva_org_id = .) %>% 
    inner_join(treee, by = "Diva_org_id")
  
  out
  
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
#' @return a list with three slots - "meta" for organizational unit metadata info,
#'   "units" with a named list of results (set of 5 different tibbles for each of the units)
#'   and "pt_ordning" for DiVA publication type sort order
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
    left_join(get_pubtype_order(), by = c("Publication_Type_DiVA" = "diva_publication_type"))
  
  ggplot(data = df_diva_long,
         aes(x = year)) +
    geom_bar(aes(weight = value, fill = reorder(Publication_Type_DiVA, pt_ordning))) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_fill_brewer(palette = "Set3") +
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
    gather("Copublication", "value", -interval) %>% 
    filter(!interval == "Total")
  
  ggplot(data = df_copub_long,
         aes(x = interval, y = value, group = Copublication)) +
    geom_line(aes(color = Copublication)) +
    geom_point(aes(color = Copublication)) +
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
abm_bullet <- function(label, value, reference, roundto = 1, pct = FALSE){
  if(pct){
    value <- 100*value
    reference <- 100*reference
  }
  value <- round(value, roundto)

  title <- sprintf(paste0("%s = %.", roundto, "f%s"), label, value, ifelse(pct, "%", ""))

  bg.data <- data.frame(measure = label, target = reference, value = value)
  kth_cols <- palette_kth()

  ggplot(bg.data) +
    labs(title = title) +
    geom_bar(aes(x = measure, y = max(2*target, ceiling(value))), fill="gray", stat="identity", width=0.7, alpha=1) +
    geom_bar(aes(x = measure, y = value), fill = kth_cols["blue"],  stat = "identity", width = 0.4) +
    geom_errorbar(aes(x = measure, ymin = target, ymax = target), color=kth_cols["cerise"], width = 0.9, size = 1.1) +
    coord_flip() +
    theme(plot.title=element_text(size = 12),
          axis.text.x=element_text(size=8),
          axis.title.x=element_blank(),
          axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          aspect.ratio = 0.1)
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
