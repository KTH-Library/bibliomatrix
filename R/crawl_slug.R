#' Given a organizational unit slug, such as "j/jj/jjn", retrieves 
#' associated KTH researcher ids
#' @param slug character slug for an organizational unit
#' @return tibble with kthids, usernames, titles and a timestamp
#' @details this fcn belongs in the kthapi package
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  kthids_from_slug("j/jj/jjn")
#'  kthids_from_slug("j/jh/jhs")
#'  }
#' }
#' @seealso 
#'  \code{\link[kthapi]{kth_catalog}},\code{\link[kthapi]{kth_profile}}
#'  \code{\link[progress]{progress_bar}}
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}}
#'  \code{\link[lubridate]{now}}
#' @rdname kthids_from_slug
#' @export 
#' @import dplyr
#' @importFrom kthapi kth_catalog kth_profile
#' @importFrom progress progress_bar
#' @importFrom purrr possibly map_chr
#' @importFrom lubridate now
#' 
kthids_from_slug <- function(slug) {
  
  users <- kthapi::kth_catalog(slug = slug)$users
  
  pb <- progress::progress_bar$new(
    format = "resolving kthids [:bar] :percent eta: :eta",
    total = length(users$username))
  
  kthid_from_username <- function(x) {
    pb$tick()
    kthapi::kth_profile(username = x)$content$kthId
  } 
  
  kfu <- purrr::possibly(kthid_from_username, otherwise = NA_character_)
  
  ids <- purrr::map_chr(users$username, kfu)
  
  users %>% 
    mutate(slug = slug, kthid = ids) %>%
    select(slug, kthid, username, title.en) %>%
    mutate(crawl_ts = lubridate::now())
  
}

#' Slugs or identifiers for departments at KTH used in ABM
#' @return character vector of slugs
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  slugs <- abm_slugs_departments()
#'  }
#' }
#' @rdname abm_slugs_departments
#' @export 
abm_slugs_departments <- function() {
  unit_info() %>% 
    filter(org_level == 2) %>% 
    pull(slug)
}

#' Crawl kthapi for divisions
#' 
#' This function crawls the KTH Directory API for divisions, by default those used in ABM
#' @param include character vector of slugs to include, Default: abm_slugs_institutions()
#' @param exclude character vector of slugs to exclude, Default: NULL
#' @param quiet logical to indicate logging, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  kth_divisions_crawl()
#'  }
#' }
#' @seealso 
#'  \code{\link[progress]{progress_bar}}
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}}
#' @rdname kth_divisions_crawl
#' @export 
#' @importFrom progress progress_bar
#' @importFrom purrr possibly map_df
kth_divisions_crawl <- function(include = abm_slugs_departments(), 
  exclude = NULL, quiet = FALSE) {
  
  if (!missing(exclude) && any(! exclude %in% abm_slugs_departments()))
    stop("Please exclude only valid slugs, use abm_slugs_departments().")
  
  if (!quiet)
    message("Please use this fcn sparingly and cache results, offloading the API.\n")
  
  slugs <- setdiff(include, exclude)

  pb <- progress::progress_bar$new(
    total = length(slugs),
    format = "  processing [:what] [:bar] :percent eta: :eta"
  )

  crawl <- function(slug) {
    if (!quiet) pb$tick(tokens = list(what = sprintf("%10s", slug)))
    kth_catalog_crawl(slug)
  }
  
  crawly <- purrr::possibly(crawl, otherwise = NULL, quiet = FALSE)
  
  purrr::map_df(slugs, crawly)
  
}

#' Given an organizational "slug", a depth first traversal is made
#' enumerating organizational units (descendants)
#' @param slug a string with the slug, for example "j/jj"
#' @return a tibble encoding descendants as a hierarchy (using parent_id, child_id tuples)
#' @details this fcn should be moved to the kthapi package... please avoid using this fcn too often as it descends recursively and
#' generates several API calls; instead please cache the results and try to avoid hammering the API
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  kth_catalog_crawl("j/jj")
#'  }
#' }
#' @seealso 
#'  \code{\link[kthapi]{kth_catalog}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{bind}}
#' @rdname kth_catalog_crawl
#' @export 
#' @importFrom kthapi kth_catalog
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
kth_catalog_crawl <- function(slug) {
  
  if (missing(slug) || length(slug) != 1 || nzchar(slug) < 1)
    stop("Please provide one single valid slug, for example 'j/jj'")

  # immediate children for a given slug
  slug_children <- function(y) {
    lookup <- kthapi::kth_catalog(slug = y)
    children <- lookup$catalogs
    res <- NULL
    if (nrow(children) != 0)
      res <- tibble(
        pid = y,
        id = children$slug, 
        desc_parent = lookup$info$`description.en`,
        desc = children$`description.en`
        )
    res
  } 
  
  # depth first traversal from a given slug
  dfs <- function(x) {
    descendants <- slug_children(x)
    crawl <- purrr::map_df(descendants$id, dfs)
    dplyr::bind_rows(descendants, crawl)
  }
  
  dfs(slug)
}

#' ABM divisions as force directed network
#' 
#' After crawling organizational data, this is used for displaying an 
#' interactive graph with a force directed network of the units.
#' 
#' @param base_url pattern to prefigate links from nodes with in JS click action
#' @return a force directed network object from NetworkD3
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  abm_graph_divisions()
#'  }
#' }
#' @seealso 
#'  \code{\link[networkD3]{forceNetwork}},\code{\link[networkD3]{JS}}
#' @rdname abm_graph_divisions
#' @export 
#' @importFrom stringr str_split_fixed str_replace str_count
#' @importFrom jsonlite toJSON
#' @importFrom networkD3 forceNetwork JS
abm_graph_divisions <- function(base_url = "dash/") {
  
  # assemble org tree only with divisions used in ABM
  
  root <- tibble(from = "ABM", to = NA_character_, name = "root")
  
  # consider only these schools
  schools <- 
    abm_slugs_departments() %>% 
    stringr::str_split_fixed(pattern = "/", n = 2) %>% 
    .[,1]
  
  schools_name <- 
    tibble(slug = schools) %>% left_join(unit_info(), by = c("slug")) %>%
    select(slug, unit_short) %>% pull(unit_short)
  
  # "backfill" the tree with larger org units before adding divisions/subdivisions
  l0 <- tibble(from = schools, to = "ABM", name = schools_name)
  l1 <- tibble(from = abm_slugs_departments(), to = schools, name = schools_name)
  d <- abm_divisions() %>% select(from = id, to = pid, name = desc)
  tree <- bind_rows(l0, l1, d)
  #eert <- tree %>% select(to, from, name)
  
  # make a graph colored by organizational unit level
  
  tree2 <- 
    tree %>% left_join(bind_rows(
      tibble(id = "ABM", name = "ABM"), 
      tree %>% select(id = from, name = name)), by = c(to = "id")) %>%
    rename(to_name = `name.y`, from_name = `name.x`)
  
  src <- paste(tree2$from, ": ", tree2$from_name)
  target <- paste(tree2$to, ": ", tree2$to_name)
  
  # data prepared for NetworkD3  
  networkData <- data.frame(src, target, stringsAsFactors = FALSE)
  
  nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
  nodes$id <- 0:(nrow(nodes) - 1)
  
  edges <- networkData %>%
    left_join(nodes, by = c("src" = "name")) %>%
    select(-src) %>%
    rename(source = id) %>%
    left_join(nodes, by = c("target" = "name")) %>%
    select(-target) %>%
    rename(target = id)
  
  edges$width <- 1
  
  # make a grouping variable that will match to colours
  
  nodes$groupid <- 
    stringr::str_replace(nodes$name, "(.*?)\\s+:\\s+(.*?)$", "\\1")
  
  nodes$group <- 
    nodes$groupid %>%
    stringr::str_count("/") + 1
  
  nodes$group[which(nodes$groupid == "ABM")] <- 0
  labels <- c("ABM", "School", "Institution", "Division", "Subdivision")
  nodes$fgroup <- ordered(as.character(nodes$group), labels = labels)
  groups <- as.character(sort(unique(nodes$fgroup)))
  
  domain <- jsonlite::toJSON(groups)
  range <- jsonlite::toJSON(palette_kth_digital(length(groups)))
  ColourScale <- sprintf("d3.scaleOrdinal().domain(%s).range(%s);", domain, range)
  
  fn <- networkD3::forceNetwork(
    Links = edges, Nodes = nodes, 
    Source = "source",
    Target = "target",
    NodeID ="name",
    Group = "fgroup",
    Value = "width",
    opacity = 0.9,
    zoom = TRUE, legend = TRUE,
    #  opacityNoHover = TRUE,
    colourScale = networkD3::JS(ColourScale)
  )
  
  # fn$x$nodes$hyperlink <- 
  #   sprintf('https://www.kth.se/directory/%s', nodes$groupid)
  # fn$x$options$clickAction = 'window.open(d.hyperlink)'

  links <- purrr::map_chr(nodes$groupid, function(x) URLencode(x, reserved = TRUE))

  fn$x$nodes$hyperlink <- sprintf('%s%s', base_url, links)

  fn$x$options$clickAction = 'window.open(d.hyperlink)'
  
    
  fn
  
}

#' Crawl and persist organizational data from KTH API 
#' 
#' This function can be used to trigger a crawl for organizational data provided
#' by the KTH APIs. Data will be persisted using the provided database connection
#' @param con connection to database, Default: con_bib()
#' @param crawl logical flag confirming intention to perform crawl, Default: FALSE
#' @return invisible TRUE on success
#' @details The parameter crawl must be set to TRUE to run this function
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  db_upload_crawl()
#'  }
#' }
#' @rdname db_upload_crawl
#' @export 
#' @importFrom purrr walk2
db_upload_crawl <- function(con = con_bib(), crawl = FALSE) {
  
  if (!crawl) {
    message("Please call this function with crawl = TRUE to trigger a full crawl")
    return(invisible(FALSE))
  } else {
    message("Crawling, pls use this crawl sparingly, to avoid live API lookups...")
  }
  
  divisions <- 
    kth_divisions_crawl()
  
  researchers <- 
    divisions$id %>% map_df(kthids_from_slug)
  
  on.exit(dbDisconnect(con))
  
  data <- list(
    researchers = researchers,
    divisions = divisions
    #unit_stats = unit_stats
  )
  
  purrr::walk2(names(data), data, function(x, y) db_upsert_table(x, y, con))
  return(invisible(TRUE))
}

#' Create or overwrite table at connection source
#' 
#' Utility that can be used to sync data against a connection.
#' 
#' @param tbl table name
#' @param df data frame or tibble with the data
#' @param con database connection, Default: con_bib()
#' @return invisibly TRUE on success
#' @details not all backend drivers support overwrite and append, therefore
#' param overwrite for dbWriteTable (see docs) is not used
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  db_upsert_table("divisions", abm_divisions(), con = con_bib(type = "mssql"))
#'  }
#' }
#' @seealso 
#'  \code{\link[DBI]{dbExistsTable}},\code{\link[DBI]{dbRemoveTable}},\code{\link[DBI]{dbWriteTable}}
#' @rdname db_upsert_table
#' @export 
#' @importFrom DBI dbExistsTable dbRemoveTable dbWriteTable
db_upsert_table <- function(tbl, df, con = con_bib()) {
  if (DBI::dbExistsTable(con, tbl)) DBI::dbRemoveTable(con, tbl)
  con %>% DBI::dbWriteTable(tbl, df)
}

#' Researchers used in ABM
#' @param unit_slug character identifier for a unit
#' @param con database connection, Default: con_bib()
#' @return tibble with researchers
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  abm_researchers()
#'  }
#' }
#' @rdname abm_researchers
#' @import dplyr
#' @importFrom DBI dbDisconnect
#' @export 
abm_researchers <- function(unit_slug, con) {
  
  if (missing(con)) {
    con <- con_bib()
    on.exit(DBI::dbDisconnect(con))
  }
  
  t1 <- "researchers"
  
  if (t1 %in% (con %>% dbListTables())) {
    con %>% tbl(t1) %>% 
      filter(slug == unit_slug) %>% pull(kthid)
  } else {
    message("Please run db_sync() or use db_upload_crawl()")
  }
  
}

#' Divisions used in ABM
#' @param con database connection, Default: con_bib()
#' @return tibble with divisions
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  abm_divisions()
#'  }
#' }
#' @rdname abm_divisions
#' @importFrom DBI dbListTables
#' @import dplyr
#' @export 
abm_divisions <- function(con =  con_bib()) {
  
  on.exit(dbDisconnect(con))
  t1 <- "divisions"
  
  if (t1 %in% (con %>% DBI::dbListTables())) {
    con %>% tbl(t1) %>% collect()
  } else {
    message("Please run db_sync() or use db_upload_crawl()")
  }
  
}

#' Publications for ABM researchers belonging to a unit
#' 
#' Publications corresponding to researchers associated with a unit slug are 
#' returned.
#' @param unit_slug identifier for organizational unit
#' @return tibble with publications
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  abm_unit_pubs("j/jj/jjn")
#'  }
#' }
#' @rdname abm_unit_pubs
#' @export 
abm_unit_pubs <- function(unit_slug) {
  ids <- abm_researchers(unit_slug)
  abm_staff_data(kthids = ids) 
}

#' Summary for a list of publications
#' 
#' A list of publications belonging to for example an organizational unit
#' is summarized.
#' @param unit_slug identifier for the organizational unit
#' @return tibble with summary
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  abm_pubs_summary("j/jj/jjn")
#'  }
#' }
#' @rdname abm_pubs_summary
#' @import dplyr
#' @export 
abm_pubs_summary <- function(unit_slug) {
  
  ids <- abm_researchers(unit_slug)
  n_staff <- length(ids)
  
  pubs <- abm_unit_pubs(unit_slug)
  
  nd_researchers <- 
    con_bib() %>% tbl("masterfile_researchers") %>% filter(Unit_code %in% ids) %>%
    select(Unit_code) %>% distinct(Unit_code) %>% 
    collect() %>% nrow()
  
  n_pubs <- nrow(pubs)
  
  n_pubs_wos <- pubs %>% filter(Unit_Fraction_adj > 1e-4) %>% nrow()
  
  tibble(slug = unit_slug, n_staff, nd_researchers, n_pubs, n_pubs_wos)
}

#' Publication summary stats for ABM divisions
#' @param slugs a vector of unit slug identifiers, Default: abm_divisions()$id
#' @return tibble with summary data
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  stats <- abm_division_stats() 
#'  # upload results to database
#'  db_upsert_table("division_stats", stats)
#'  }
#' }
#' @rdname abm_division_stats
#' @importFrom purrr map_df
#' @import dplyr
#' @export 
abm_division_stats <- function(slugs = abm_divisions()$id) {
  stats <- purrr::map_df(slugs, abm_pubs_summary)
  abm_divisions() %>% left_join(
    stats %>% arrange(n_pubs, nd_researchers, desc(n_staff)) %>% rename(id = slug))
}

# statz <- abm_division_stats()
# db_upsert_table("division_stats", statz)
# pa_slugs <- c("j/jj/jjn", "j/jh/jhs")
# con_bib() %>% tbl("division_stats") %>%
#   mutate(cov = as.double(n_pubs_wos) / as.double(n_pubs)) %>%
#   mutate(ppr = as.double(n_pubs / as.double(nd_researchers))) %>%
#   mutate(pps = as.double(n_pubs / as.double(n_staff))) %>%
#   arrange(desc(pps), desc(ppr), cov, n_staff, n_pubs, nd_researchers) %>%
#   filter(cov > 0) %>%
#   collect()
# # filter(n_pubs == 0 | nd_researchers == 0) %>%
# #  pull(slug)
#   filter(id %in% pa_slugs)
