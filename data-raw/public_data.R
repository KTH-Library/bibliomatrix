# WRITEBACK

# some data needs to be written back or persisted

# embedded package data
public <- abm_public_data(overwrite_cache = TRUE)
abm_public_kth <- public
usethis::use_data(abm_public_kth, internal = FALSE, overwrite = TRUE)


# data for authorbased variant

# persist crawl data locally (researchers, divisions, unit_stats)
db_upload_crawl(crawl = TRUE)

# also sync this data remotely
db_sync_table("unit_stats", con_src = con_bib_sqlite(), con_dest = con_bib_mssql(), overwrite = TRUE)
db_sync_table("researchers", con_src = con_bib_sqlite(), con_dest = con_bib_mssql(), overwrite = TRUE)
db_sync_table("divisions", con_src = con_bib_sqlite(), con_dest = con_bib_mssql(), overwrite = TRUE)
