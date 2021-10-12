#! /usr/bin/env Rscript

library(bibliomatrix)
library(dplyr)

static_site_script <- function(base_url = "http://localhost:8080") {
  
  # landing page and page for units without detailed reports
  p_home <- sprintf("curl -o index.html %s/ui/divisions", base_url)
  p_404 <- sprintf("mkdir -p division/na && curl -o division/na/index.html %s/ui/division/na", base_url)
  
  snakify <- function(x)
    gsub("/", "_", x, fixed = TRUE)
  
  # what units to generate static site for
  units <- 
    con_bib("sqlite") %>% tbl("unit_stats") %>% 
    mutate(has_link = (n_pubs > 10 & nd_researchers > 4)) %>%
    filter(has_link) %>%
    collect() %>%
    pull(id)
  
  urls <- sprintf("%s/ui/division/%s", base_url, 
   purrr::map_chr(units, function(x) URLencode(x, reserved = TRUE)))
  
  destdir <- snakify(units)
  destfile <- sprintf("%s/index.html", destdir)
  curls <- sprintf("mkdir -p division/%s && curl -o division/%s -L -s %s", destdir, destfile, urls)
  p_details <- curls
  
  #curls <- paste0("curl -o /dev/null -L -s -w \"%{http_code}\" ", urls)
  #cmds <- sprintf("sh -c '[ $(%s) -eq 200 ]'", curls)
  
  rc <- function(cmd) {
    res <- system(cmd)
    if (res != 0) {
      message("Failed running cmd: ", cmd)
      print(res)
    }
  }
  
  script <- c("#!/bin/bash", p_home, p_404, p_details)
  
  script
}

readr::write_lines(static_site_script(), file = "gen_static_site.sh")
Sys.chmod("gen_static_site.sh", "755", use_umask = FALSE)

message("before running the gen_static_site.sh script, first start the API...")
message(" in R, for example:")
message("plumber::plumb(file='inst/plumber/authorbased/plumber.R')$run(port = 8080)")

message("...")
message("Then do:")
message("mkdir poc-static; cp gen_static_site.sh poc-static; cd poc-static")
message("./gen_static_site.sh; cd ..; tar cvfz poc-static.tgz poc-static")
message("and upload the tgz to a webserver, for example using scp")

