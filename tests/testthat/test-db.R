skip_db_tests <- TRUE

test_that("sync with empty sqlite db works", {
  skip_if(skip_db_tests, "by default skipping since deleting db may be a bit drastic")
  fn <- db_sqlite_location()
  unlink(fn)
  db_sync(tables_included = "Log_Run")
})

test_that("sync for all db tables work", {
  skip_if(skip_db_tests, "by default skipping since syncing many MB of data may be a bit drastic")
  fn <- db_sqlite_location()
  unlink(fn)
  db_sync()
})


