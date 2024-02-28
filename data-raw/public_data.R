# WRITEBACK

# some data needs to be written back or persisted

# embedded package data
public <- abm_public_data(overwrite_cache = TRUE)
abm_public_kth <- public
usethis::use_data(abm_public_kth, internal = FALSE, overwrite = TRUE)

# then create data for authorbased variant

# REMEMBER: rebuild to load the updated data in the package
# if there is an update of the unit info, make sure it is done first

### Below is not necessary anymore since we have abandoned the staff based ABM
# persist crawl data locally (researchers, divisions, unit_stats)
#db_upload_crawl(crawl = TRUE)

# then sync this data remotely
#db_sync_table("unit_stats", con_src = con_bib_sqlite(), con_dest = con_bib_mssql(), overwrite = TRUE)
#db_sync_table("researchers", con_src = con_bib_sqlite(), con_dest = con_bib_mssql(), overwrite = TRUE)
#db_sync_table("divisions", con_src = con_bib_sqlite(), con_dest = con_bib_mssql(), overwrite = TRUE)

# check that numbers for new division makes sense
#con <- con_bib()
#con %>% tbl("unit_stats") %>% filter(id == "m/mo/mob")
#con %>% tbl("unit_stats") %>% filter(id == "m/mo")
#dbDisconnect(con)


