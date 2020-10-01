# persist to local storage

library(duckdb)
library(dplyr)

hr <- file.path(rappdirs::app_dir("abm")$config(), "hr")
if (!dir.exists(hr)) dir.create(hr, recursive = TRUE)
db <- file.path(hr, "hr.duck")

con <- dbConnect(duckdb::duckdb(), dbdir = db, read_only = FALSE)

j_jj_jjn <- kthids_from_slug("j/jj/jjn")
j_jh_jhs <- kthids_from_slug("j/jh/jhs")

data <- bind_rows(j_jj_jjn, j_jh_jhs)

if (dbExistsTable(con, "researchers")) db_drop_table(con, "researchers")
con %>% dbWriteTable(name = "researchers", data)


divisions <- abm_divisions()

if (dbExistsTable(con, "divisions")) db_drop_table(con, "divisions")
con %>% dbWriteTable(name = "divisions", divisions)


# inspect results
con %>% tbl("researchers") %>% collect()
con %>% tbl("divisions") %>% collect()


dbDisconnect(con, shutdown = TRUE)


#tidync:::hyper_array.tidync
#tidync:::hyper_tbl_cube.tidync

unit_info()

kth_catalog()
