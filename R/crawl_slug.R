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

#' Divisions used in ABM
#' @param include character vector of slugs to include, Default: abm_slugs_institutions()
#' @param exclude character vector of slugs to exclude, Default: NULL
#' @param quiet logical to indicate logging, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  abm_divisions()
#'  }
#' }
#' @seealso 
#'  \code{\link[progress]{progress_bar}}
#'  \code{\link[purrr]{safely}},\code{\link[purrr]{map}}
#' @rdname abm_divisions
#' @export 
#' @importFrom progress progress_bar
#' @importFrom purrr possibly map_df
abm_divisions <- function(include = abm_slugs_departments(), 
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
