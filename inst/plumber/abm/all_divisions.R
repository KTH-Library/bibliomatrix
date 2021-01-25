#! /usr/bin/env Rscript

library(bibliomatrix)

urls <- sprintf("http://localhost:%s/%s/%s", 7777, "ui/division", 
 purrr::map_chr(abm_divisions()$id, function(x) URLencode(x, reserved = TRUE)))

curls <- paste0("curl -o /dev/null -L -s -w \"%{http_code}\" ", urls)

cmds <- sprintf("sh -c '[ $(%s) -eq 200 ]'", curls)

rc <- function(cmd) {
  res <- system(cmd)
  if (res != 0) {
    message("Running cmd: ", cmd)
    print(res)
  }
}

purrr::walk(cmds, rc)


# NB: before running this script, first start the API:
# plumb(file='divisions.R')$run(port = 7777)
