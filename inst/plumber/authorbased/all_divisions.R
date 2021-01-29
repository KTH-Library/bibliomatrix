#! /usr/bin/env Rscript

library(bibliomatrix)

system("curl -o index.html http://localhost:8080/ui/divisions")

snakify <- function(x)
  gsub("/", "_", x, fixed = TRUE)

unit_schools <- unique(abm_divisions()$pid)
unit_institutions <- unique(sapply(strsplit(unit_schools, "/", fixed = TRUE), "[[", 1))
units <- c(unit_institutions, unit_schools, unique(abm_divisions()$id))

urls <- sprintf("http://localhost:%s/%s/%s", 8080, "ui/division", 
 purrr::map_chr(units, function(x) URLencode(x, reserved = TRUE)))

destdir <- snakify(units)
destfile <- sprintf("%s/index.html", destdir)
curls <- sprintf("mkdir -p %s && curl -o %s -L -s %s", destdir, destfile, urls)
cmds <- curls

#curls <- paste0("curl -o /dev/null -L -s -w \"%{http_code}\" ", urls)
#cmds <- sprintf("sh -c '[ $(%s) -eq 200 ]'", curls)


rc <- function(cmd) {
  res <- system(cmd)
  if (res != 0) {
    message("Failed running cmd: ", cmd)
    print(res)
  }
}

purrr::walk(cmds, rc)



# NB: before running this script, first start the API:
#plumber::plumb(file='inst/plumber/authorbased/plumber.R')$run(port = 8080)
