# test_that("sync with empty sqlite db works", {
#   skip_on_ci()
#   fn <- db_sqlite_location()
#   unlink(fn)
#   db_sync(tables_included = "Log_Run")
# })
# 
# test_that("sync for all db tables work", {
#   skip_on_ci()
#   fn <- db_sqlite_location()
#   unlink(fn)
#   db_sync()
# })
